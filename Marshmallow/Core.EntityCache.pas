(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Core.EntityCache;

interface

uses
  Mapping.Attributes
  ,Spring.Collections
  ,Classes
  ;

type
  TColumnDataList = class;

  TColumnDataListEnumerator = class
  private
    FList: TColumnDataList;
    FIndex: Integer;
  protected
    function DoGetCurrent: TColumnData; virtual;
    function DoMoveNext: Boolean; virtual;
  public
    constructor Create(AList: TColumnDataList); virtual;

    property Current: TColumnData read DoGetCurrent;
    function MoveNext: Boolean;
  end;

  TColumnDataList = class
  private
    FList: IList<TColumnData>;
    FPrimaryKeyColumn: TColumnData;
    FPrimaryKeyExists: Boolean;
    function GetItem(Index: Integer): TColumnData;
    procedure SetItem(Index: Integer; const Value: TColumnData);
  protected
    procedure SetPrimaryKeyColumn(const Value: TColumnData); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function GetEnumerator: TColumnDataListEnumerator;

    function Add(const AColumnData: TColumnData): Integer;
    function IsEmpty: Boolean;
    function Count: Integer;
    procedure Delete(AIndex: Integer);

    function TryGetPrimaryKeyColumn(out APrimaryKeyColumn: TColumnData): Boolean;

    property Items[Index: Integer]: TColumnData read GetItem write SetItem; default;
    property PrimaryKeyColumn: TColumnData read FPrimaryKeyColumn write SetPrimaryKeyColumn;
    property List: IList<TColumnData> read FList;
  end;

  TEntityData = class(TPersistent)
  private
    FColumns: IList<ColumnAttribute>;
    FColumnMembernameIndex: IDictionary<string, ColumnAttribute>;
    FColumnsData: TColumnDataList;
    FForeignKeyColumns: IList<ForeignJoinColumnAttribute>;
    FPrimaryKeyColumn: ColumnAttribute;
    FTable: TableAttribute;
    FOneToManyColumns: IList<OneToManyAttribute>;
    FManyToOneColumns: IList<ManyToOneAttribute>;
    FSequence: SequenceAttribute;
    FHasInstanceField: Boolean;
    FEntityClass: TClass;
    FVersionColumn: VersionAttribute;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetEntityData(AClass: TClass); virtual;
    procedure SetColumnsData(); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function IsTableEntity(): Boolean;
    function ColumnByMemberName(const AMemberName: string): ColumnAttribute;
    function ColumnByName(const AColumnName: string): ColumnAttribute;
    function HasInstanceField: Boolean;
    function HasSequence(): Boolean;
    function HasManyToOneRelations(): Boolean;
    function HasOneToManyRelations(): Boolean;
    function HasVersionColumn(): Boolean;

    property Columns: IList<ColumnAttribute> read FColumns;
    property ColumnsData: TColumnDataList read FColumnsData write FColumnsData;
    property ForeignColumns: IList<ForeignJoinColumnAttribute> read FForeignKeyColumns;
    property OneToManyColumns: IList<OneToManyAttribute> read FOneToManyColumns;
    property ManyToOneColumns: IList<ManyToOneAttribute> read FManyToOneColumns;
    property PrimaryKeyColumn: ColumnAttribute read FPrimaryKeyColumn;
    property Sequence: SequenceAttribute read FSequence write FSequence;

    property VersionColumn: VersionAttribute read FVersionColumn;
    property EntityClass: TClass read FEntityClass;
    property EntityTable: TableAttribute read FTable;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Class which holds cached data of annotated entities.
  ///	</summary>
  {$ENDREGION}
  TEntityCache = class
  private
    class var FEntities: IDictionary<TClass,TEntityData>;
  public
    class constructor Create;

    class function Get(AClass: TClass): TEntityData;
    class function TryGet(AClass: TClass; out AEntityData: TEntityData): Boolean;
    class function GetColumns(AClass: TClass): IList<ColumnAttribute>;
    class function GetColumnsData(AClass: TClass): TColumnDataList;
    class function CreateColumnsData(AClass: TClass): TColumnDataList;

    class property Entities: IDictionary<TClass, TEntityData> read FEntities;
  end;


implementation

uses
  Mapping.RttiExplorer
  ,Core.Exceptions
  ,Core.Comparers
  ,Generics.Defaults
  ,Generics.Collections
  ,SysUtils
  ;


{ TEntityData }

procedure TEntityData.AssignTo(Dest: TPersistent);
var
  LDest: TEntityData;
  LPair: TPair<string, ColumnAttribute>;
begin
  if Dest is TEntityData then
  begin
    LDest := TEntityData(Dest);

    LDest.FTable := FTable;
    LDest.FColumns.Clear;
    LDest.FColumns.AddRange(Columns);
    LDest.FColumnMembernameIndex.Clear;
    for LPair in FColumnMembernameIndex do
    begin
      LDest.FColumnMembernameIndex.Add(LPair.Key, LPair.Value);
    end;

    LDest.FColumnsData.FList.Clear;
    LDest.FColumnsData.FList.AddRange(FColumnsData.FList);

    LDest.FForeignKeyColumns.Clear;
    LDest.FForeignKeyColumns.AddRange(FForeignKeyColumns);

    LDest.FPrimaryKeyColumn := FPrimaryKeyColumn;

    LDest.FOneToManyColumns.Clear;
    LDest.FOneToManyColumns.AddRange(FOneToManyColumns);

    LDest.FManyToOneColumns.Clear;
    LDest.FManyToOneColumns.AddRange(ManyToOneColumns);

    LDest.FSequence := FSequence;

    LDest.FHasInstanceField := FHasInstanceField;
  end;
end;

function TEntityData.ColumnByMemberName(const AMemberName: string): ColumnAttribute;
begin
  if not FColumnMembernameIndex.TryGetValue(AMemberName, Result) then
    Result := nil;
end;

function TEntityData.ColumnByName(const AColumnName: string): ColumnAttribute;
begin
  for Result in FColumns do
  begin
    if SameText(Result.Name, AColumnName) then
      Exit;
  end;
  Result := nil;  
end;

constructor TEntityData.Create;
var
  LCaseInsensitiveComparer: IEqualityComparer<string>;
begin
  inherited Create;
  FColumns := TCollections.CreateList<ColumnAttribute>;
  FColumnsData := TColumnDataList.Create;
  FPrimaryKeyColumn := nil;
  FTable := nil;
  FSequence := nil;
  FVersionColumn := nil;
  FForeignKeyColumns := TCollections.CreateList<ForeignJoinColumnAttribute>;
  FOneToManyColumns := TCollections.CreateList<OneToManyAttribute>;
  FManyToOneColumns := TCollections.CreateList<ManyToOneAttribute>;

  LCaseInsensitiveComparer := TStringCaseInsensitiveComparer.Create;
  FColumnMembernameIndex := TCollections.CreateDictionary<string, ColumnAttribute>(LCaseInsensitiveComparer);
end;

destructor TEntityData.Destroy;
begin
  FColumnsData.Free;
  inherited Destroy;
end;


function TEntityData.HasInstanceField: Boolean;
begin
  Result := FHasInstanceField;
end;

function TEntityData.HasManyToOneRelations: Boolean;
begin
  Result := FManyToOneColumns.Count > 0;
end;

function TEntityData.HasOneToManyRelations: Boolean;
begin
  Result := not FOneToManyColumns.IsEmpty;
end;

function TEntityData.HasSequence: Boolean;
begin
  Result := Assigned(FSequence);
end;

function TEntityData.HasVersionColumn: Boolean;
begin
  Result := Assigned(FVersionColumn);
end;

function TEntityData.IsTableEntity: Boolean;
begin
  Result := Assigned(FTable);
end;

procedure TEntityData.SetColumnsData();
var
  LColData: TColumnData;
  LCol: ColumnAttribute;
begin
  if FColumnsData.Count > 0 then
    Exit;

  for LCol in FColumns do
  begin
    LColData.Properties := LCol.Properties;
    LColData.Name := LCol.Name;
    LColData.ColTypeInfo := LCol.GetColumnTypeInfo;
    LColData.ClassMemberName := LCol.ClassMemberName;

    if LCol.IsPrimaryKey then
    begin
      FColumnsData.PrimaryKeyColumn := LColData;
    end;

    if LCol.IsVersionColumn then
    begin
      FVersionColumn := LCol as VersionAttribute;
    end;

    FColumnsData.Add(LColData);
    FColumnMembernameIndex.Add(LCol.ClassMemberName, LCol);
  end;
end;

procedure TEntityData.SetEntityData(AClass: TClass);
begin
  FEntityClass := AClass;
  TRttiExplorer.GetColumns(AClass, FColumns);
  SetColumnsData();
  FPrimaryKeyColumn := TRttiExplorer.GetPrimaryKeyColumn(AClass);
  if Assigned(FPrimaryKeyColumn) then
    FPrimaryKeyColumn.IsIdentity := TRttiExplorer.GetColumnIsIdentity(AClass, FPrimaryKeyColumn);
  FTable := TRttiExplorer.GetTable(AClass);
  TRttiExplorer.GetClassMembers<ForeignJoinColumnAttribute>(AClass, FForeignKeyColumns);
  TRttiExplorer.GetClassMembers<OneToManyAttribute>(AClass, FOneToManyColumns);
  TRttiExplorer.GetClassMembers<ManyToOneAttribute>(AClass, FManyToOneColumns);
  FSequence := TRttiExplorer.GetSequence(AClass);
  FHasInstanceField := TRttiExplorer.HasInstanceField(AClass);
end;

{ TEntityCache }

class constructor TEntityCache.Create;
begin
  FEntities := TCollections.CreateDictionary<TClass,TEntityData>([doOwnsValues], 100);
end;

class function TEntityCache.CreateColumnsData(AClass: TClass): TColumnDataList;
var
  LEntityData: TEntityData;
begin
  Result := TColumnDataList.Create;
  LEntityData := Get(AClass);
  LEntityData.SetColumnsData();
  Result.List.AddRange(LEntityData.ColumnsData.List);
  Result.PrimaryKeyColumn := LEntityData.ColumnsData.PrimaryKeyColumn;
end;


class function TEntityCache.Get(AClass: TClass): TEntityData;
begin
  if not TryGet(AClass, Result) then
  begin
    Result := TEntityData.Create;
    Result.SetEntityData(AClass);
    FEntities.Add(AClass, Result);
  end;
end;

class function TEntityCache.GetColumns(AClass: TClass): IList<ColumnAttribute>;
begin
  Result := Get(AClass).Columns;
end;

class function TEntityCache.GetColumnsData(AClass: TClass): TColumnDataList;
var
  LEntityData: TEntityData;
begin
  LEntityData := Get(AClass);
  LEntityData.SetColumnsData();
  Result := LEntityData.ColumnsData;
end;

class function TEntityCache.TryGet(AClass: TClass; out AEntityData: TEntityData): Boolean;
begin
  Result := FEntities.TryGetValue(AClass, AEntityData);
end;

{ TColumnDataList }

function TColumnDataList.Add(const AColumnData: TColumnData): Integer;
begin
  FList.Add(AColumnData);
  Result := FList.Count - 1;
end;

function TColumnDataList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TColumnDataList.Create;
begin
  inherited Create;
  FPrimaryKeyExists := False;
  FList := Tcollections.CreateList<TColumnData>;
end;

procedure TColumnDataList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

destructor TColumnDataList.Destroy;
begin
  inherited Destroy;
end;

function TColumnDataList.GetEnumerator: TColumnDataListEnumerator;
begin
  Result := TColumnDataListEnumerator.Create(Self);
end;

function TColumnDataList.GetItem(Index: Integer): TColumnData;
begin
  Result := FList[Index];
end;

function TColumnDataList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TColumnDataList.SetItem(Index: Integer; const Value: TColumnData);
begin
  FList[Index] := Value;
end;

procedure TColumnDataList.SetPrimaryKeyColumn(const Value: TColumnData);
begin
  FPrimaryKeyColumn := Value;
  FPrimaryKeyExists := True;
end;

function TColumnDataList.TryGetPrimaryKeyColumn(out APrimaryKeyColumn: TColumnData): Boolean;
begin
  Result := FPrimaryKeyExists;
  if Result then
    APrimaryKeyColumn := FPrimaryKeyColumn;
end;

{ TColumnDataListEnumerator }

constructor TColumnDataListEnumerator.Create(AList: TColumnDataList);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TColumnDataListEnumerator.DoGetCurrent: TColumnData;
begin
  Result := FList.List[FIndex];
end;

function TColumnDataListEnumerator.DoMoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

function TColumnDataListEnumerator.MoveNext: Boolean;
begin
  Result := DoMoveNext;
end;

end.
