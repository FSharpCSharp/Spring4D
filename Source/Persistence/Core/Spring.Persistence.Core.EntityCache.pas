{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Persistence.Core.EntityCache;

interface

uses
  Classes,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes;

type
  TColumnDataList = class
  private
    fList: IList<TColumnData>;
    fPrimaryKeyColumn: TColumnData;
    fPrimaryKeyExists: Boolean;
    function GetCount: Integer;
    function GetItem(index: Integer): TColumnData;
    procedure SetItem(index: Integer; const value: TColumnData);
  protected
    procedure SetPrimaryKeyColumn(const Value: TColumnData); virtual;
  public
    constructor Create; virtual;

    function GetEnumerator: IEnumerator<TColumnData>;

    function Add(const columnData: TColumnData): Integer;
//    function IsEmpty: Boolean;
    procedure Delete(index: Integer);

    function TryGetPrimaryKeyColumn(out primaryKeyColumn: TColumnData): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TColumnData read GetItem write SetItem; default;
    property PrimaryKeyColumn: TColumnData read fPrimaryKeyColumn write SetPrimaryKeyColumn;
    property List: IList<TColumnData> read fList;
  end;

  TEntityData = class(TPersistent)
  private
    fColumns: IList<ColumnAttribute>;
    fSelectColumns: IList<ColumnAttribute>;
    fColumnMemberNameIndex: IDictionary<string, ColumnAttribute>;
    fColumnsData: TColumnDataList;
    fForeignKeyColumns: IList<ForeignJoinColumnAttribute>;
    fPrimaryKeyColumn: ColumnAttribute;
    fTable: TableAttribute;
    fOneToManyColumns: IList<OneToManyAttribute>;
    fManyToOneColumns: IList<ManyToOneAttribute>;
    fSequence: SequenceAttribute;
    fHasInstanceField: Boolean;
    fEntityClass: TClass;
    fVersionColumn: VersionAttribute;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetColumnsData; virtual;
    procedure SetEntityData(entityClass: TClass); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsTableEntity: Boolean;
    function ColumnByMemberName(const memberName: string): ColumnAttribute;
    function ColumnByName(const columnName: string): ColumnAttribute;
    function HasInstanceField: Boolean;
    function HasPrimaryKey: Boolean;
    function HasSequence: Boolean;
    function HasManyToOneRelations: Boolean;
    function HasOneToManyRelations: Boolean;
    function HasVersionColumn: Boolean;

   function GetPrimaryKeyValueAsString(const instance: TObject): string;

    property Columns: IList<ColumnAttribute> read fColumns;
    property SelectColumns: IList<ColumnAttribute> read fSelectColumns;
    property ColumnsData: TColumnDataList read fColumnsData write fColumnsData;
    property ForeignColumns: IList<ForeignJoinColumnAttribute> read fForeignKeyColumns;
    property OneToManyColumns: IList<OneToManyAttribute> read fOneToManyColumns;
    property ManyToOneColumns: IList<ManyToOneAttribute> read fManyToOneColumns;
    property PrimaryKeyColumn: ColumnAttribute read fPrimaryKeyColumn;
    property Sequence: SequenceAttribute read fSequence write fSequence;
    property VersionColumn: VersionAttribute read fVersionColumn;
    property EntityClass: TClass read fEntityClass;
    property EntityTable: TableAttribute read fTable;
  end;

  /// <summary>
  ///   Class which holds cached data of annotated entities.
  /// </summary>
  TEntityCache = class
  private
    class var fEntities: IDictionary<TClass,TEntityData>;
    class var fCriticalSection: ICriticalSection;
  public
    class constructor Create;

    class function Get(entityClass: TClass): TEntityData;
    class function TryGet(entityClass: TClass; out entityData: TEntityData): Boolean;

    class function CreateColumnsData(entityClass: TClass): TColumnDataList;
    class function GetColumns(entityClass: TClass): IList<ColumnAttribute>;
    class function GetColumnsData(entityClass: TClass): TColumnDataList;
    class function TryGetColumnByMemberName(entityClass: TClass;
      const memberName: string; out column: ColumnAttribute): Boolean;

    class function IsValidEntity(entityClass: TClass): Boolean;

    class property Entities: IDictionary<TClass, TEntityData> read fEntities;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,
  Rtti,
  Spring.Reflection,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.RttiExplorer;


{$REGION 'TColumnDataList'}

constructor TColumnDataList.Create;
begin
  inherited Create;
  fList := TCollections.CreateList<TColumnData>;
end;

function TColumnDataList.Add(const columnData: TColumnData): Integer;
begin
  Result := fList.Add(columnData);
end;

procedure TColumnDataList.Delete(index: Integer);
begin
  fList.Delete(index);
end;

function TColumnDataList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TColumnDataList.GetEnumerator: IEnumerator<TColumnData>;
begin
  Result := fList.GetEnumerator;
end;

function TColumnDataList.GetItem(index: Integer): TColumnData;
begin
  Result := fList[index];
end;

//function TColumnDataList.IsEmpty: Boolean;
//begin
//  Result := fList.IsEmpty;
//end;

procedure TColumnDataList.SetItem(index: Integer; const value: TColumnData);
begin
  fList[index] := value;
end;

procedure TColumnDataList.SetPrimaryKeyColumn(const Value: TColumnData);
begin
  fPrimaryKeyColumn := Value;
  fPrimaryKeyExists := True;
end;

function TColumnDataList.TryGetPrimaryKeyColumn(out primaryKeyColumn: TColumnData): Boolean;
begin
  Result := fPrimaryKeyExists;
  if Result then
    primaryKeyColumn := fPrimaryKeyColumn;
end;

{$ENDREGION}


{$REGION 'TEntityData'}

procedure TEntityData.AssignTo(Dest: TPersistent);
var
  LDest: TEntityData;
  LPair: TPair<string,ColumnAttribute>;
begin
  if Dest is TEntityData then
  begin
    LDest := TEntityData(Dest);

    LDest.fTable := fTable;
    LDest.fColumns.Clear;
    LDest.fColumns.AddRange(Columns);

    LDest.fSelectColumns.Clear;
    LDest.fSelectColumns.AddRange(SelectColumns);

    LDest.fColumnMemberNameIndex.Clear;
    for LPair in fColumnMemberNameIndex do
      LDest.fColumnMemberNameIndex.Add(LPair.Key, LPair.Value);

    LDest.fColumnsData.fList.Clear;
    LDest.fColumnsData.fList.AddRange(fColumnsData.fList);

    LDest.fForeignKeyColumns.Clear;
    LDest.fForeignKeyColumns.AddRange(fForeignKeyColumns);

    LDest.fPrimaryKeyColumn := fPrimaryKeyColumn;

    LDest.fOneToManyColumns.Clear;
    LDest.fOneToManyColumns.AddRange(fOneToManyColumns);

    LDest.fManyToOneColumns.Clear;
    LDest.fManyToOneColumns.AddRange(ManyToOneColumns);

    LDest.fSequence := fSequence;

    LDest.fVersionColumn := fVersionColumn;

    LDest.fHasInstanceField := fHasInstanceField;
  end;
end;

function TEntityData.ColumnByMemberName(const memberName: string): ColumnAttribute;
begin
  if not fColumnMemberNameIndex.TryGetValue(memberName, Result) then
    Result := nil;
end;

function TEntityData.ColumnByName(const columnName: string): ColumnAttribute;
var
  LAttribute: ColumnAttribute;
begin
  for LAttribute in fColumns do
    if SameText(LAttribute.ColumnName, columnName) then
      Exit(LAttribute);
  Result := nil;
end;

constructor TEntityData.Create;
begin
  inherited Create;
  fColumns := TCollections.CreateList<ColumnAttribute>;
  fSelectColumns := TCollections.CreateList<ColumnAttribute>;
  fColumnsData := TColumnDataList.Create;
  fPrimaryKeyColumn := nil;
  fTable := nil;
  fSequence := nil;
  fVersionColumn := nil;
  fForeignKeyColumns := TCollections.CreateList<ForeignJoinColumnAttribute>;
  fOneToManyColumns := TCollections.CreateList<OneToManyAttribute>;
  fManyToOneColumns := TCollections.CreateList<ManyToOneAttribute>;
  fColumnMemberNameIndex := TCollections.CreateDictionary<string, ColumnAttribute>(
    TStringComparer.OrdinalIgnoreCase);
end;

destructor TEntityData.Destroy;
begin
  fColumnsData.Free;
  inherited Destroy;
end;

function TEntityData.GetPrimaryKeyValueAsString(
  const instance: TObject): string;
var
  LValue: TValue;
begin
  Result := '';
  if Assigned(PrimaryKeyColumn) then
  begin
    LValue := PrimaryKeyColumn.RttiMember.GetValue(instance);
    Result := LValue.ToString;
  end;
end;

function TEntityData.HasInstanceField: Boolean;
begin
  Result := fHasInstanceField;
end;

function TEntityData.HasManyToOneRelations: Boolean;
begin
  Result := fManyToOneColumns.Count > 0;
end;

function TEntityData.HasOneToManyRelations: Boolean;
begin
  Result := fOneToManyColumns.Any;
end;

function TEntityData.HasPrimaryKey: Boolean;
begin
  Result := Assigned(fPrimaryKeyColumn);
end;

function TEntityData.HasSequence: Boolean;
begin
  Result := Assigned(fSequence);
end;

function TEntityData.HasVersionColumn: Boolean;
begin
  Result := Assigned(fVersionColumn);
end;

function TEntityData.IsTableEntity: Boolean;
begin
  Result := Assigned(fTable);
end;

procedure TEntityData.SetColumnsData;
var
  LColData: TColumnData;
  LCol: ColumnAttribute;
begin
  if fColumnsData.Count > 0 then
    Exit;

  for LCol in fColumns do
  begin
    LColData.Properties := LCol.Properties;
    LColData.ColumnName := LCol.ColumnName;
    LColData.TypeInfo := LCol.MemberType;
    LColData.MemberName := LCol.MemberName;
    LColData.ColumnAttr := LCol;

    if LCol.IsPrimaryKey then
      fColumnsData.PrimaryKeyColumn := LColData;

    if LCol.IsVersionColumn then
      fVersionColumn := LCol as VersionAttribute;

    LColData.IsLazy := IsLazyType(LColData.TypeInfo);
    if not LColData.IsLazy then
      fSelectColumns.Add(LCol);

    fColumnsData.Add(LColData);
    fColumnMemberNameIndex.Add(LCol.MemberName, LCol);
  end;
end;

procedure TEntityData.SetEntityData(entityClass: TClass);
begin
  fEntityClass := entityClass;
  fColumns := TRttiExplorer.GetColumns(entityClass);
  SetColumnsData;
  fPrimaryKeyColumn := TRttiExplorer.GetPrimaryKeyColumn(entityClass);
  if Assigned(fPrimaryKeyColumn) then
    fPrimaryKeyColumn.IsIdentity := TRttiExplorer.GetColumnIsIdentity(entityClass, fPrimaryKeyColumn);
  fTable := TRttiExplorer.GetTable(entityClass);
  fForeignKeyColumns := TRttiExplorer.GetClassMembers<ForeignJoinColumnAttribute>(entityClass);
  fOneToManyColumns := TRttiExplorer.GetClassMembers<OneToManyAttribute>(entityClass);
  fManyToOneColumns := TRttiExplorer.GetClassMembers<ManyToOneAttribute>(entityClass);
  fSequence := TRttiExplorer.GetSequence(entityClass);
  fHasInstanceField := TRttiExplorer.HasInstanceField(entityClass);
end;

{$ENDREGION}


{$REGION 'TEntityCache'}

class constructor TEntityCache.Create;
begin
  fCriticalSection := TInterfacedCriticalSection.Create;
  fEntities := TCollections.CreateDictionary<TClass,TEntityData>([doOwnsValues], 100);
end;

class function TEntityCache.CreateColumnsData(entityClass: TClass): TColumnDataList;
var
  LEntityData: TEntityData;
begin
  Result := TColumnDataList.Create;
  LEntityData := Get(entityClass);
  LEntityData.SetColumnsData;
  Result.List.AddRange(LEntityData.ColumnsData.List);
  Result.PrimaryKeyColumn := LEntityData.ColumnsData.PrimaryKeyColumn;
end;

class function TEntityCache.Get(entityClass: TClass): TEntityData;
begin
  if not TryGet(entityClass, Result) then
  begin
    Result := TEntityData.Create;
    Result.SetEntityData(entityClass);
    fCriticalSection.Enter;
    try
      fEntities.AddOrSetValue(entityClass, Result);
    finally
      fCriticalSection.Leave;
    end;
  end;
end;

class function TEntityCache.GetColumns(entityClass: TClass): IList<ColumnAttribute>;
begin
  Result := Get(entityClass).Columns;
end;

class function TEntityCache.GetColumnsData(entityClass: TClass): TColumnDataList;
var
  LEntityData: TEntityData;
begin
  LEntityData := Get(entityClass);
  LEntityData.SetColumnsData;
  Result := LEntityData.ColumnsData;
end;

class function TEntityCache.IsValidEntity(entityClass: TClass): Boolean;
var
  LEntityData: TEntityData;
begin
  LEntityData := TEntityCache.Get(entityClass);

  Result := Assigned(LEntityData)
    and LEntityData.IsTableEntity
    and LEntityData.HasPrimaryKey;
end;

class function TEntityCache.TryGet(entityClass: TClass; out entityData: TEntityData): Boolean;
begin
  Result := fEntities.TryGetValue(entityClass, entityData);
end;

class function TEntityCache.TryGetColumnByMemberName(entityClass: TClass;
  const memberName: string; out column: ColumnAttribute): Boolean;
begin
  column := TEntityCache.Get(entityClass).ColumnByMemberName(memberName);
  Result := Assigned(column);
end;

{$ENDREGION}


end.
