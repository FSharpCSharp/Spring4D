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
unit SQL.Commands;

interface

uses
  SQL.Types, Generics.Collections, Mapping.Attributes, Spring.Collections;

type
  TDMLCommandType = (ctSelect, ctInsert, ctUpdate, ctDelete, ctUpdateVersion);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents abstract DML command.
  ///	</summary>
  {$ENDREGION}
  TDMLCommand = class abstract
  private
    FTable: TSQLTable;
    FEntity: TObject;
    FParameterNames: IDictionary<string,Integer>;
  protected
    procedure SetTable(AColumns: IList<ColumnAttribute>); virtual; abstract;
  public
    constructor Create(ATable: TSQLTable); virtual;
    destructor Destroy; override;

    function GetAndIncParameterName(const AFieldname: string): string; virtual;
    function GetExistingParameterName(const AFieldname: string): string; virtual;

    property Entity: TObject read FEntity write FEntity;
    property Table: TSQLTable read FTable;
  end;

  TWhereCommand = class(TDMLCommand)
  private
    FWhereFields: IList<TSQLWhereField>;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    property WhereFields: IList<TSQLWhereField> read FWhereFields;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>select</c> command.
  ///	</summary>
  {$ENDREGION}
  TSelectCommand = class(TWhereCommand)
  private
    FSelectFields: IList<TSQLSelectField>;
    FJoins: IList<TSQLJoin>;
    FGroupByFields: IList<TSQLGroupByField>;
    FOrderByFields: IList<TSQLOrderField>;
    FPrimaryKeyColumn: ColumnAttribute;
    FForeignColumn: ForeignJoinColumnAttribute;
    FTables: IList<TSQLTable>;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    function FindTable(AClass: TClass): TSQLTable;
    function FindCorrespondingTable(ATable: TSQLTable): TSQLTable;

    procedure SetAssociations(AEntityClass: TClass); virtual;
    procedure SetTable(AColumns: IList<ColumnAttribute>); override;
    procedure SetFromPrimaryColumn();
    procedure SetFromForeignColumn(ABaseTableClass, AForeignTableClass: TClass);

    property SelectFields: IList<TSQLSelectField> read FSelectFields;
    property Joins: IList<TSQLJoin> read FJoins;
    property GroupByFields: IList<TSQLGroupByField> read FGroupByFields;
    property OrderByFields: IList<TSQLOrderField> read FOrderByFields;

    property ForeignColumn: ForeignJoinColumnAttribute read FForeignColumn write FForeignColumn;
    property PrimaryKeyColumn: ColumnAttribute read FPrimaryKeyColumn write FPrimaryKeyColumn;
    property Tables: IList<TSQLTable> read FTables;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>insert</c> command.
  ///	</summary>
  {$ENDREGION}
  TInsertCommand = class(TDMLCommand)
  private
    FInsertFields: IList<TSQLField>;
    FSequence: SequenceAttribute;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: IList<ColumnAttribute>); override;

    property InsertFields: IList<TSQLField> read FInsertFields;
    property Sequence: SequenceAttribute read FSequence write FSequence;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>update</c> command.
  ///	</summary>
  {$ENDREGION}
  TUpdateCommand = class(TWhereCommand)
  private
    FUpdateFields: IList<TSQLField>;
    FPrimaryKeyColumn: ColumnAttribute;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: IList<ColumnAttribute>); override;

    property PrimaryKeyColumn: ColumnAttribute read FPrimaryKeyColumn write FPrimaryKeyColumn;
    property UpdateFields: IList<TSQLField> read FUpdateFields;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>delete</c> command.
  ///	</summary>
  {$ENDREGION}
  TDeleteCommand = class(TWhereCommand)
  private
    FPrimaryKeyColumnName: string;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: IList<ColumnAttribute>); override;

    property PrimaryKeyColumnName: string read FPrimaryKeyColumnName write FPrimaryKeyColumnName;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>create table</c> command.
  ///	</summary>
  {$ENDREGION}
  TCreateTableCommand = class(TDMLCommand)
  private
    FColumns: IList<TSQLCreateField>;
    FDbColumns: IList<string>;
    FTableExists: Boolean;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: IList<ColumnAttribute>); override;

    property TableExists: Boolean read FTableExists write FTableExists;
    property DbColumns: IList<string> read FDbColumns;
    property Columns: IList<TSQLCreateField> read FColumns;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>create foreign key</c> command.
  ///	</summary>
  {$ENDREGION}
  TCreateFKCommand = class(TCreateTableCommand)
  private
    FForeigns: IList<TSQLForeignKeyField>;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: IList<ColumnAttribute>); override;

    property ForeignKeys: IList<TSQLForeignKeyField> read FForeigns;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents <c>create sequence</c> command.
  ///	</summary>
  {$ENDREGION}
  TCreateSequenceCommand = class
  private
    FSequence: SequenceAttribute;
    FSequenceExists: Boolean;
  public
    constructor Create(ASequenceAttribute: SequenceAttribute); virtual;

    property SequenceExists: Boolean read FSequenceExists write FSequenceExists;
    property Sequence: SequenceAttribute read FSequence write FSequence;
  end;

implementation

uses
  Mapping.RttiExplorer
  ,Core.EntityCache
  ,SysUtils
  ,StrUtils
  ,Generics.Defaults
  ,Core.Relation.ManyToOne
  ;

{ TSelectCommand }

constructor TSelectCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FSelectFields := TCollections.CreateObjectList<TSQLSelectField>;
  FJoins := TCollections.CreateObjectList<TSQLJoin>;
  FGroupByFields := TCollections.CreateObjectList<TSQLGroupByField>;
  FOrderByFields := TCollections.CreateObjectList<TSQLOrderField>;
  FTables := TCollections.CreateObjectList<TSQLTable>(True);
  FForeignColumn := nil;
end;

destructor TSelectCommand.Destroy;
begin
  inherited Destroy;
end;

function TSelectCommand.FindCorrespondingTable(ATable: TSQLTable): TSQLTable;
var
  LCurrentSQLTable: TSQLTable;
begin
  Result := ATable;

  if (ATable = nil) then
    Exit;

  for LCurrentSQLTable in FTables do
  begin
    if SameText(LCurrentSQLTable.GetNameWithoutSchema, ATable.GetNameWithoutSchema) then
    begin
      Exit(LCurrentSQLTable);
    end;
  end;
end;

function TSelectCommand.FindTable(AClass: TClass): TSQLTable;
var
  LTableName: string;
  LCurrentSQLTable: TSQLTable;
begin
  if AClass = nil then
    Exit(Table);

  LTableName := TEntityCache.Get(AClass).EntityTable.TableName;

  for LCurrentSQLTable in FTables do
  begin
    if SameText( LCurrentSQLTable.GetNameWithoutSchema, LTableName) then
    begin
      Exit(LCurrentSQLTable);
    end;
  end;
  Result := Table;
end;

procedure TSelectCommand.SetAssociations(AEntityClass: TClass);
var
  LEntityData: TEntityData;
  LManyOneCol: ManyToOneAttribute;
  LSelectField: TSQLSelectField;
  LTable: TSQLTable;
  LMappedTableClass: TClass;
  LCol: ColumnAttribute;
  LBuiltFieldname: string;
  LMappedByCol: ColumnAttribute;
  LMappedByColname: string;
  LJoin: TSQLJoin;
  i: Integer;
begin
  LEntityData := TEntityCache.Get(AEntityClass);
  i := 0;

  for LManyOneCol in LEntityData.ManyToOneColumns do
  begin
    LTable := TSQLTable.Create();
    LTable.SetFromAttribute(TRttiExplorer.GetTable(LManyOneCol.GetColumnTypeInfo));
    FTables.Add(LTable);
    LTable.Alias := LTable.Alias + IntToStr(i);

    LMappedByCol := TManyToOneRelation.GetMappedByColumn(LManyOneCol, AEntityClass);
    LMappedByColname := LMappedByCol.Name;
    LMappedTableClass := TRttiExplorer.GetClassFromClassInfo(LManyOneCol.GetColumnTypeInfo);
    for LCol in TEntityCache.Get(LMappedTableClass).Columns do
    begin
      LBuiltFieldname := TManyToOneRelation.BuildColumnName(LTable.GetNameWithoutSchema, LMappedByColname, LCol.Name);
      LSelectField := TSQLSelectField.Create(LCol.Name + ' ' + LBuiltFieldname, LTable);
      FSelectFields.Add(LSelectField);
    end;
    //add join
    LJoin := TSQLJoin.Create(jtLeft);

    LJoin.Segments.Add(TSQLJoinSegment.Create(
      TSQLField.Create(TEntityCache.Get(LMappedTableClass).PrimaryKeyColumn.Name, LTable)
      ,TSQLField.Create(LMappedByColname, Table)
    ));

    FJoins.Add(LJoin);
    Inc(i);
  end;
end;

procedure TSelectCommand.SetFromForeignColumn(ABaseTableClass, AForeignTableClass: TClass);
var
  LPrimaryKeyColumn: ColumnAttribute;
  LWhereField: TSQLWhereField;
begin
  FForeignColumn := nil;
  LPrimaryKeyColumn := TEntityCache.Get(ABaseTableClass).PrimaryKeyColumn;
  if not Assigned(LPrimaryKeyColumn) then
    Exit;

  FForeignColumn := TRttiExplorer.GetForeignKeyColumn(AForeignTableClass, LPrimaryKeyColumn);
  if not Assigned(FForeignColumn) then
    Exit;

  LWhereField := TSQLWhereField.Create(FForeignColumn.Name, FTable);
  LWhereField.ParamName := GetAndIncParameterName(FForeignColumn.Name);
  WhereFields.Add(LWhereField);
end;

procedure TSelectCommand.SetFromPrimaryColumn();
var
  LWhereField: TSQLWhereField;
begin
  FForeignColumn := nil;
  if Assigned(FPrimaryKeyColumn) then
  begin
    LWhereField := TSQLWhereField.Create(FPrimaryKeyColumn.Name, FTable);
    LWhereField.ParamName := GetAndIncParameterName(FPrimaryKeyColumn.Name);
    WhereFields.Add(LWhereField);
  end;
end;

procedure TSelectCommand.SetTable(AColumns: IList<ColumnAttribute>);
var
  LColumn: ColumnAttribute;
  LSelectField: TSQLSelectField;
begin
  Assert(Assigned(AColumns), 'AColumns not assigned');
  FSelectFields.Clear;
  FJoins.Clear;
  WhereFields.Clear;
  FGroupByFields.Clear;
  FOrderByFields.Clear;

  {TODO -oLinas -cGeneral : add all select fields}
  for LColumn in AColumns do
  begin
    LSelectField := TSQLSelectField.Create(LColumn.Name, FTable);
    FSelectFields.Add(LSelectField);
  end;
end;

{ TInsertCommand }

constructor TInsertCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FInsertFields := TCollections.CreateList<TSQLField>;
  FSequence := nil;
end;

destructor TInsertCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TInsertCommand.SetTable(AColumns: IList<ColumnAttribute>);
var
  LField: TSQLField;
  LColumn: ColumnAttribute;
begin
  Assert(Assigned(AColumns), 'AColumns not assigned');
  //add fields
  FInsertFields.Clear;

  for LColumn in AColumns do
  begin
    if (LColumn.CanInsert) then  //fixes #22
    begin
      LField := TSQLField.Create(LColumn.Name, FTable);
      FInsertFields.Add(LField);
    end;
  end;
end;

{ TUpdateCommand }

constructor TUpdateCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FUpdateFields := TCollections.CreateObjectList<TSQLField>;
  FPrimaryKeyColumn := nil;
end;

destructor TUpdateCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TUpdateCommand.SetTable(AColumns: IList<ColumnAttribute>);
var
  LField: TSQLField;
  LWhereField: TSQLWhereField;
  LColumn: ColumnAttribute;
begin
  Assert(Assigned(AColumns), 'AColumns not assigned');
  //add fields
  FUpdateFields.Clear;
  WhereFields.Clear;

  for LColumn in AColumns do
  begin
    if (LColumn.CanUpdate) then
    begin
      LField := TSQLField.Create(LColumn.Name, FTable);
      FUpdateFields.Add(LField);
    end;
  end;

  //add primary key column
  if Assigned(FPrimaryKeyColumn) then
  begin
    LWhereField := TSQLWhereField.Create(FPrimaryKeyColumn.Name, FTable);
    LWhereField.ParamName := GetAndIncParameterName(FPrimaryKeyColumn.Name);
    WhereFields.Add(LWhereField);
  end;

end;

{ TDeleteCommand }

constructor TDeleteCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FPrimaryKeyColumnName := '';
end;

destructor TDeleteCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TDeleteCommand.SetTable(AColumns: IList<ColumnAttribute>);
var
  LWhereField: TSQLWhereField;
begin
  Assert(FPrimaryKeyColumnName <> '', 'Primary key column name is not specified for deletion');
  //add fields
  WhereFields.Clear;

  LWhereField := TSQLWhereField.Create(FPrimaryKeyColumnName, FTable);
  LWhereField.ParamName := GetAndIncParameterName(FPrimaryKeyColumnName);
  WhereFields.Add(LWhereField);
end;

{ TDMLCommand }

constructor TDMLCommand.Create(ATable: TSQLTable);
begin
  inherited Create;
  FTable := ATable;
  FParameterNames := TCollections.CreateDictionary<string,Integer>();
end;

destructor TDMLCommand.Destroy;
begin
  inherited Destroy;
end;

function TDMLCommand.GetAndIncParameterName(const AFieldname: string): string;
var
  LIndex: Integer;
  LUpFieldname: string;
begin
  LUpFieldname := UpperCase(AFieldname);
  if not FParameterNames.TryGetValue(LUpFieldname, LIndex) then
  begin
    LIndex := 1;
  end
  else
  begin
    Inc(LIndex);
  end;
  FParameterNames.AddOrSetValue(LUpFieldname, LIndex);
  Result := Format(':%S%D', [LUpFieldname, LIndex]);
end;

function TDMLCommand.GetExistingParameterName(const AFieldname: string): string;
var
  LIndex: Integer;
  LUpFieldname: string;
begin
  LUpFieldname := UpperCase(AFieldname);
  if not FParameterNames.TryGetValue(LUpFieldname, LIndex) then
  begin
    LIndex := 1;
  end;
  Result := Format(':%S%D', [LUpFieldname, LIndex]);
end;

{ TCreateTableCommand }

constructor TCreateTableCommand.Create(ATable: TSQLTable);
var
  LCaseInsensitiveComparer: IComparer<string>;
begin
  inherited Create(ATable);
  FColumns := TCollections.CreateObjectList<TSQLCreateField>(True);
  LCaseInsensitiveComparer := TComparer<string>.Construct(
    function(const Left, Right: string): Integer
    begin
      Result := CompareText(Left, Right);
    end);

  FDbColumns := TCollections.CreateList<string>(LCaseInsensitiveComparer);
end;

destructor TCreateTableCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TCreateTableCommand.SetTable(AColumns: IList<ColumnAttribute>);
var
  LCol: ColumnAttribute;
  LField: TSQLCreateField;
begin
  FColumns.Clear;
  FDbColumns.Clear;

  for LCol in AColumns do
  begin
    LField := TSQLCreateField.Create(LCol.Name, FTable);
    LField.SetFromAttribute(LCol);
    FColumns.Add(LField);
  end;
end;

{ TCreateFKCommand }

constructor TCreateFKCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FForeigns := TCollections.CreateObjectList<TSQLForeignKeyField>(True);
end;

destructor TCreateFKCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TCreateFKCommand.SetTable(AColumns: IList<ColumnAttribute>);
var
  LCol: ColumnAttribute;
  LForeignKeyColumn: ForeignJoinColumnAttribute;
  LFKField: TSQLForeignKeyField;
begin
  inherited SetTable(AColumns);
  FForeigns.Clear;
  for LCol in AColumns do
  begin
    if TRttiExplorer.TryGetColumnAsForeignKey(LCol, LForeignKeyColumn) then
    begin
      LFKField := TSQLForeignKeyField.Create(LForeignKeyColumn.Name, Table);
      LFKField.Constraints := LForeignKeyColumn.ForeignStrategies;
      LFKField.ReferencedColumnName := LForeignKeyColumn.ReferencedColumnName;
      LFKField.ReferencedTableName := LForeignKeyColumn.ReferencedTableName;
      FForeigns.Add(LFKField);
    end;
  end;
end;

{ TCreateSequenceCommand }

constructor TCreateSequenceCommand.Create(ASequenceAttribute: SequenceAttribute);
begin
  inherited Create;
  FSequence := ASequenceAttribute;
end;

{ TWhereCommand }

constructor TWhereCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FWhereFields := TCollections.CreateObjectList<TSQLWhereField>();
end;

destructor TWhereCommand.Destroy;
begin
  inherited Destroy;
end;

end.
