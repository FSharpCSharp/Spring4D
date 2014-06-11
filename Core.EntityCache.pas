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
  ,Generics.Collections
  ,Classes
  ;

type
  TColumnDataList = class;

  TColumnDataListEnumerator = class(TEnumerator<TColumnData>)
  private
    FList: TColumnDataList;
    FIndex: Integer;
  protected
    function DoGetCurrent: TColumnData; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(AList: TColumnDataList); virtual;
  end;

  TColumnDataList = class
  private
    FList: TList<TColumnData>;
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
    property List: TList<TColumnData> read FList;
  end;

  TEntityData = class(TPersistent)
  private
    FColumns: TList<ColumnAttribute>;
    FColumnMembernameIndex: TDictionary<string, ColumnAttribute>;
    FColumnsData: TColumnDataList;
    FForeignKeyColumns: TList<ForeignJoinColumnAttribute>;
    FPrimaryKeyColumn: ColumnAttribute;
    FTable: TableAttribute;
    FOneToManyColumns: TList<OneToManyAttribute>;
    FManyToOneColumns: TList<ManyToOneAttribute>;
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
    function HasVersionColumn(): Boolean;

    property Columns: TList<ColumnAttribute> read FColumns;
    property ColumnsData: TColumnDataList read FColumnsData write FColumnsData;
    property ForeignColumns: TList<ForeignJoinColumnAttribute> read FForeignKeyColumns;
    property OneToManyColumns: TList<OneToManyAttribute> read FOneToManyColumns;
    property ManyToOneColumns: TList<ManyToOneAttribute> read FManyToOneColumns;
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
    class var FEntities: TObjectDictionary<string,TEntityData>;
  public
    class constructor Create;
    class destructor Destroy;

    class function Get(AClass: TClass): TEntityData;
    class function TryGet(AClass: TClass; out AEntityData: TEntityData): Boolean;
    class function GetColumns(AClass: TClass): TList<ColumnAttribute>;
    class function GetColumnsData(AClass: TClass): TColumnDataList;
    class function CreateColumnsData(AClass: TClass): TColumnDataList;

    class property Entities: TObjectDictionary<string, TEntityData> read FEntities;
  end;


implementation

uses
  Mapping.RttiExplorer
  ,Core.Exceptions
  ,Core.Comparers
  ,Generics.Defaults
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
  FColumns := TList<ColumnAttribute>.Create;
  FColumnsData := TColumnDataList.Create;
  FPrimaryKeyColumn := nil;
  FTable := nil;
  FSequence := nil;
  FVersionColumn := nil;
  FForeignKeyColumns := TList<ForeignJoinColumnAttribute>.Create;
  FOneToManyColumns := TList<OneToManyAttribute>.Create;
  FManyToOneColumns := TList<ManyToOneAttribute>.Create;

  LCaseInsensitiveComparer := TStringCaseInsensitiveComparer.Create;
  FColumnMembernameIndex := TDictionary<string, ColumnAttribute>.Create(LCaseInsensitiveComparer);
end;

destructor TEntityData.Destroy;
begin
  FColumns.Free;
  FColumnsData.Free;
  FForeignKeyColumns.Free;
  FOneToManyColumns.Free;
  FManyToOneColumns.Free;
  FColumnMembernameIndex.Free;
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
  FEntities := TObjectDictionary<string,TEntityData>.Create([doOwnsValues], 100);
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

class destructor TEntityCache.Destroy;
begin
  FEntities.Free;
end;



class function TEntityCache.Get(AClass: TClass): TEntityData;
begin
  if not TryGet(AClass, Result) then
  begin
    Result := TEntityData.Create;
    Result.SetEntityData(AClass);
    FEntities.Add(AClass.ClassName, Result);
  end;
end;

class function TEntityCache.GetColumns(AClass: TClass): TList<ColumnAttribute>;
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
  Result := FEntities.TryGetValue(AClass.ClassName, AEntityData);
end;

{ TColumnDataList }

function TColumnDataList.Add(const AColumnData: TColumnData): Integer;
begin
  Result := FList.Add(AColumnData);
end;

function TColumnDataList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TColumnDataList.Create;
begin
  inherited Create;
  FPrimaryKeyExists := False;
  FList := TList<TColumnData>.Create;
end;

procedure TColumnDataList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

destructor TColumnDataList.Destroy;
begin
  FList.Free;
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

end.
