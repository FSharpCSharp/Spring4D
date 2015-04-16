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
    fColumnsData: TColumnDataList;
    fForeignKeyColumns: IList<ForeignJoinColumnAttribute>;
    fPrimaryKeyColumn: ColumnAttribute;
    fTable: TableAttribute;
    fOneToManyColumns: IList<OneToManyAttribute>;
    fManyToOneColumns: IList<ManyToOneAttribute>;
    fSequence: SequenceAttribute;
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
    function ColumnByName(const columnName: string): ColumnAttribute;
    function HasPrimaryKey: Boolean;
    function HasSequence: Boolean;
    function HasManyToOneRelations: Boolean;
    function HasOneToManyRelations: Boolean;
    function HasVersionColumn: Boolean;

    function GetPrimaryKeyValueAsString(const instance: TObject): string;
    function GetForeignKeyColumn(const table: TableAttribute;
      const primaryKeyColumn: ColumnAttribute): ForeignJoinColumnAttribute;

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

    class function IsValidEntity(entityClass: TClass): Boolean;


  end;

implementation

uses
  Generics.Collections,
  SysUtils,
  Rtti,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Reflection;


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

constructor TEntityData.Create;
begin
  inherited Create;
  fColumns := TCollections.CreateList<ColumnAttribute>;
  fSelectColumns := TCollections.CreateList<ColumnAttribute>;
  fColumnsData := TColumnDataList.Create;
  fForeignKeyColumns := TCollections.CreateList<ForeignJoinColumnAttribute>;
  fOneToManyColumns := TCollections.CreateList<OneToManyAttribute>;
  fManyToOneColumns := TCollections.CreateList<ManyToOneAttribute>;
end;

destructor TEntityData.Destroy;
begin
  fColumnsData.Free;
  inherited Destroy;
end;

procedure TEntityData.AssignTo(Dest: TPersistent);
var
  target: TEntityData;
begin
  if Dest is TEntityData then
  begin
    target := TEntityData(Dest);

    target.fTable := fTable;
    target.fColumns.Clear;
    target.fColumns.AddRange(Columns);

    target.fSelectColumns.Clear;
    target.fSelectColumns.AddRange(SelectColumns);

    target.fColumnsData.fList.Clear;
    target.fColumnsData.fList.AddRange(fColumnsData.fList);

    target.fForeignKeyColumns.Clear;
    target.fForeignKeyColumns.AddRange(fForeignKeyColumns);

    target.fPrimaryKeyColumn := fPrimaryKeyColumn;

    target.fOneToManyColumns.Clear;
    target.fOneToManyColumns.AddRange(fOneToManyColumns);

    target.fManyToOneColumns.Clear;
    target.fManyToOneColumns.AddRange(ManyToOneColumns);

    target.fSequence := fSequence;

    target.fVersionColumn := fVersionColumn;
  end;
end;

function TEntityData.ColumnByName(const columnName: string): ColumnAttribute;
var
  column: ColumnAttribute;
begin
  for column in fColumns do
    if SameText(column.ColumnName, columnName) then
      Exit(column);
  Result := nil;
end;

function TEntityData.GetForeignKeyColumn(const table: TableAttribute;
  const primaryKeyColumn: ColumnAttribute): ForeignJoinColumnAttribute;
begin
  Result := ForeignColumns.SingleOrDefault(
    function(const foreignColumn: ForeignJoinColumnAttribute): Boolean
    begin
      Result := SameText(primaryKeyColumn.ColumnName, foreignColumn.ReferencedColumnName)
       and SameText(Table.TableName, foreignColumn.ReferencedTableName);
    end);
end;

function TEntityData.GetPrimaryKeyValueAsString(
  const instance: TObject): string;
begin
  if Assigned(PrimaryKeyColumn) then
    Result := PrimaryKeyColumn.Member.GetValue(instance).ToString
  else
    Result := '';
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
  columnData: TColumnData;
  column: ColumnAttribute;
begin
  if fColumnsData.Count > 0 then
    Exit;

  for column in fColumns do
  begin
    columnData.Properties := column.Properties;
    columnData.ColumnName := column.ColumnName;
    columnData.TypeInfo := column.MemberType;
    columnData.Member :=  column.Member;
    columnData.Column := column;

    if column.IsPrimaryKey then
      fColumnsData.PrimaryKeyColumn := columnData;

    if column.IsVersionColumn then
      fVersionColumn := column as VersionAttribute;

    columnData.IsLazy := IsLazyType(columnData.TypeInfo);
    if not columnData.IsLazy then
      fSelectColumns.Add(column);

    fColumnsData.Add(columnData);
  end;
end;

procedure TEntityData.SetEntityData(entityClass: TClass);
begin
  fEntityClass := entityClass;
  fColumns := TRttiExplorer.GetColumns(fEntityClass);
  SetColumnsData;
  fPrimaryKeyColumn := TRttiExplorer.GetPrimaryKeyColumn(fEntityClass);
  fTable := TRttiExplorer.GetTable(fEntityClass);
  fForeignKeyColumns := TRttiExplorer.GetClassMembers<ForeignJoinColumnAttribute>(fEntityClass);
  fOneToManyColumns := TRttiExplorer.GetClassMembers<OneToManyAttribute>(fEntityClass);
  fManyToOneColumns := TRttiExplorer.GetClassMembers<ManyToOneAttribute>(fEntityClass);
  fSequence := TRttiExplorer.GetSequence(fEntityClass);
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
  entityData: TEntityData;
begin
  Result := TColumnDataList.Create;
  entityData := Get(entityClass);
  entityData.SetColumnsData;
  Result.List.AddRange(entityData.ColumnsData.List);
  Result.PrimaryKeyColumn := entityData.ColumnsData.PrimaryKeyColumn;
end;

class function TEntityCache.Get(entityClass: TClass): TEntityData;
begin
  fCriticalSection.Enter;
  try
    if not fEntities.TryGetValue(entityClass, Result) then
    begin
      Result := TEntityData.Create;
      Result.SetEntityData(entityClass);
      fEntities.Add(entityClass, Result);
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

class function TEntityCache.GetColumns(entityClass: TClass): IList<ColumnAttribute>;
begin
  Result := Get(entityClass).Columns;
end;

class function TEntityCache.GetColumnsData(entityClass: TClass): TColumnDataList;
var
  entityData: TEntityData;
begin
  entityData := Get(entityClass);
  entityData.SetColumnsData;
  Result := entityData.ColumnsData;
end;

class function TEntityCache.IsValidEntity(entityClass: TClass): Boolean;
var
  entityData: TEntityData;
begin
  entityData := TEntityCache.Get(entityClass);
  Result := entityData.IsTableEntity and entityData.HasPrimaryKey;
end;

class function TEntityCache.TryGet(entityClass: TClass; out entityData: TEntityData): Boolean;
begin
  fCriticalSection.Enter;
  try
    Result := fEntities.TryGetValue(entityClass, entityData);
  finally
    fCriticalSection.Leave;
  end;
end;

{$ENDREGION}


end.
