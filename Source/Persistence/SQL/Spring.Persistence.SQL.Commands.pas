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

unit Spring.Persistence.SQL.Commands;

interface

uses
  Spring.Collections,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Types;

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
    FOwnedTable: Boolean;
  public
    constructor Create(ATable: TSQLTable); overload; override;
    constructor Create(AEntityClass: TClass); reintroduce; overload;
    destructor Destroy; override;

    function FindTable(AClass: TClass): TSQLTable;
    function FindCorrespondingTable(ATable: TSQLTable): TSQLTable;

    procedure SetAssociations(AEntityClass: TClass); virtual;
    procedure SetTable(AColumns: IList<ColumnAttribute>); override;
    procedure SetFromPrimaryColumn;
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
  Generics.Defaults,
  SysUtils,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Relation.ManyToOne,
  Spring.Persistence.Mapping.RttiExplorer;

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

constructor TSelectCommand.Create(AEntityClass: TClass);
var
  LEntityData: TEntityData;
begin
  FOwnedTable := True;
  LEntityData := TEntityCache.Get(AEntityClass);
  FTable := TSQLTable.CreateFromClass(AEntityClass);
  Create(FTable);
  SetTable(LEntityData.Columns);
  PrimaryKeyColumn := LEntityData.PrimaryKeyColumn;
  SetAssociations(AEntityClass);
end;

destructor TSelectCommand.Destroy;
begin
  if FOwnedTable then
    FTable.Free;
  inherited Destroy;
end;

function TSelectCommand.FindCorrespondingTable(ATable: TSQLTable): TSQLTable;
var
  LCurrentSQLTable: TSQLTable;
begin
  Result := ATable;

  if ATable = nil then
    Exit;

  for LCurrentSQLTable in FTables do
    if SameText(LCurrentSQLTable.GetNameWithoutSchema, ATable.GetNameWithoutSchema) then
      Exit(LCurrentSQLTable);
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
    if SameText( LCurrentSQLTable.GetNameWithoutSchema, LTableName) then
      Exit(LCurrentSQLTable);
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
    LTable := TSQLTable.Create;
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

procedure TSelectCommand.SetFromPrimaryColumn;
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
  FInsertFields := TCollections.CreateObjectList<TSQLField>;
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
  FParameterNames := TCollections.CreateDictionary<string,Integer>;
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
  FWhereFields := TCollections.CreateObjectList<TSQLWhereField>;
end;

destructor TWhereCommand.Destroy;
begin
  inherited Destroy;
end;

end.
