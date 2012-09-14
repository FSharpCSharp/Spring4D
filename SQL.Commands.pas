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
  SQL.Types, Generics.Collections, Mapping.Attributes;

type
  TDMLCommandType = (ctSelect, ctInsert, ctUpdate, ctDelete);

  TDMLCommand = class abstract
  private
    FTable: TSQLTable;
  protected
    procedure SetTable(AColumns: TList<ColumnAttribute>); virtual; abstract;
  public
    constructor Create(ATable: TSQLTable); virtual;

    property Table: TSQLTable read FTable;
  end;

  TSelectCommand = class(TDMLCommand)
  private
    FSelectFields: TObjectList<TSQLSelectField>;
    FJoins: TObjectList<TSQLJoin>;
    FWhereFields: TObjectList<TSQLWhereField>;
    FGroupByFields: TObjectList<TSQLGroupByField>;
    FOrderByFields: TObjectList<TSQLOrderField>;
    FPrimaryKeyColumn: ColumnAttribute;
    FForeignColumn: ForeignJoinColumnAttribute;
    FTables: TObjectList<TSQLTable>;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetAssociations(AEntityClass: TClass); virtual;
    procedure SetTable(AColumns: TList<ColumnAttribute>); override;
    procedure SetFromPrimaryColumn();
    procedure SetFromForeignColumn(ABaseTableClass, AForeignTableClass: TClass);

    property SelectFields: TObjectList<TSQLSelectField> read FSelectFields;
    property Joins: TObjectList<TSQLJoin> read FJoins;
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields;
    property GroupByFields: TObjectList<TSQLGroupByField> read FGroupByFields;
    property OrderByFields: TObjectList<TSQLOrderField> read FOrderByFields;

    property ForeignColumn: ForeignJoinColumnAttribute read FForeignColumn write FForeignColumn;
    property PrimaryKeyColumn: ColumnAttribute read FPrimaryKeyColumn write FPrimaryKeyColumn;
  end;

  TInsertCommand = class(TDMLCommand)
  private
    FInsertFields: TObjectList<TSQLField>;
    FSequence: SequenceAttribute;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<ColumnAttribute>); override;

    property InsertFields: TObjectList<TSQLField> read FInsertFields;
    property Sequence: SequenceAttribute read FSequence write FSequence;
  end;

  TUpdateCommand = class(TDMLCommand)
  private
    FUpdateFields: TObjectList<TSQLField>;
    FWhereFields: TObjectList<TSQLWhereField>;
    FPrimaryKeyColumn: ColumnAttribute;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<ColumnAttribute>); override;

    property PrimaryKeyColumn: ColumnAttribute read FPrimaryKeyColumn write FPrimaryKeyColumn;
    property UpdateFields: TObjectList<TSQLField> read FUpdateFields;
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields;
  end;

  TDeleteCommand = class(TDMLCommand)
  private
    FWhereFields: TObjectList<TSQLWhereField>;
    FPrimaryKeyColumnName: string;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<ColumnAttribute>); override;

    property PrimaryKeyColumnName: string read FPrimaryKeyColumnName write FPrimaryKeyColumnName;
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields;
  end;

  TCreateTableCommand = class(TDMLCommand)
  private
    FColumns: TObjectList<TSQLCreateField>;
    FDbColumns: TList<string>;
    FTableExists: Boolean;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<ColumnAttribute>); override;

    property TableExists: Boolean read FTableExists write FTableExists;
    property DbColumns: TList<string> read FDbColumns;
    property Columns: TObjectList<TSQLCreateField> read FColumns;
  end;

  TCreateFKCommand = class(TCreateTableCommand)
  private
    FForeigns: TObjectList<TSQLForeignKeyField>;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<ColumnAttribute>); override;


    property ForeignKeys: TObjectList<TSQLForeignKeyField> read FForeigns;
  end;

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
  FSelectFields := TObjectList<TSQLSelectField>.Create;
  FJoins := TObjectList<TSQLJoin>.Create;
  FWhereFields := TObjectList<TSQLWhereField>.Create;
  FGroupByFields := TObjectList<TSQLGroupByField>.Create;
  FOrderByFields := TObjectList<TSQLOrderField>.Create;
  FTables := TObjectList<TSQLTable>.Create(True);
  FForeignColumn := nil;
end;

destructor TSelectCommand.Destroy;
begin
  FSelectFields.Free;
  FJoins.Free;
  FWhereFields.Free;
  FGroupByFields.Free;
  FOrderByFields.Free;
  FTables.Free;
  inherited Destroy;
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
begin
  LEntityData := TEntityCache.Get(AEntityClass);

  for LManyOneCol in LEntityData.ManyToOneColumns do
  begin
    LTable := TSQLTable.Create();
    LTable.SetFromAttribute(TRttiExplorer.GetTable(LManyOneCol.GetColumnTypeInfo));
    FTables.Add(LTable);

    LMappedByCol := TManyToOneRelation.GetMappedByColumn(LManyOneCol, AEntityClass);
    LMappedByColname := LMappedByCol.Name;
    LMappedTableClass := TRttiExplorer.GetClassFromClassInfo(LManyOneCol.GetColumnTypeInfo);
    for LCol in TEntityCache.Get(LMappedTableClass).Columns do
    begin
      LBuiltFieldname := TManyToOneRelation.BuildColumnName(LTable.Name, LMappedByColname, LCol.Name);
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
  FWhereFields.Add(LWhereField);
end;

procedure TSelectCommand.SetFromPrimaryColumn();
var
  LWhereField: TSQLWhereField;
begin
  FForeignColumn := nil;
  if Assigned(FPrimaryKeyColumn) then
  begin
    LWhereField := TSQLWhereField.Create(FPrimaryKeyColumn.Name, FTable);
    FWhereFields.Add(LWhereField);
  end;
end;

procedure TSelectCommand.SetTable(AColumns: TList<ColumnAttribute>);
var
  LColumn: ColumnAttribute;
  LSelectField: TSQLSelectField;
begin
  Assert(Assigned(AColumns), 'AColumns not assigned');
  FSelectFields.Clear;
  FJoins.Clear;
  FWhereFields.Clear;
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
  FInsertFields := TObjectList<TSQLField>.Create;
  FSequence := nil;
end;

destructor TInsertCommand.Destroy;
begin
  FInsertFields.Free;
  inherited Destroy;
end;

procedure TInsertCommand.SetTable(AColumns: TList<ColumnAttribute>);
var
  LField: TSQLField;
  LColumn: ColumnAttribute;
begin
  Assert(Assigned(AColumns), 'AColumns not assigned');
  //add fields
  FInsertFields.Clear;

  for LColumn in AColumns do
  begin
    if not (cpDontInsert in LColumn.Properties) then
    begin
      LField := TSQLField.Create(LColumn.Name, FTable);
      FInsertFields.Add(LField);
    end
  end;
end;

{ TUpdateCommand }

constructor TUpdateCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FUpdateFields := TObjectList<TSQLField>.Create;
  FWhereFields := TObjectList<TSQLWhereField>.Create;
  FPrimaryKeyColumn := nil;
end;

destructor TUpdateCommand.Destroy;
begin
  FUpdateFields.Free;
  FWhereFields.Free;
  inherited Destroy;
end;

procedure TUpdateCommand.SetTable(AColumns: TList<ColumnAttribute>);
var
  LField: TSQLField;
  LWhereField: TSQLWhereField;
  LColumn: ColumnAttribute;
begin
  Assert(Assigned(AColumns), 'AColumns not assigned');
  //add fields
  FUpdateFields.Clear;
  FWhereFields.Clear;

  for LColumn in AColumns do
  begin
    if not (cpDontUpdate in LColumn.Properties) then
    begin
      LField := TSQLField.Create(LColumn.Name, FTable);
      FUpdateFields.Add(LField);
    end;
  end;

  //add primary key column
  if Assigned(FPrimaryKeyColumn) then
  begin
    LWhereField := TSQLWhereField.Create(FPrimaryKeyColumn.Name, FTable);
    FWhereFields.Add(LWhereField);
  end;

end;

{ TDeleteCommand }

constructor TDeleteCommand.Create(ATable: TSQLTable);
begin
  inherited Create(ATable);
  FPrimaryKeyColumnName := '';
  FWhereFields := TObjectList<TSQLWhereField>.Create;
end;

destructor TDeleteCommand.Destroy;
begin
  FWhereFields.Free;
  inherited Destroy;
end;

procedure TDeleteCommand.SetTable(AColumns: TList<ColumnAttribute>);
var
  LWhereField: TSQLWhereField;
begin
  Assert(FPrimaryKeyColumnName <> '', 'Primary key column name is not specified for deletion');
  //add fields
  FWhereFields.Clear;

  LWhereField := TSQLWhereField.Create(FPrimaryKeyColumnName, FTable);
  FWhereFields.Add(LWhereField);
end;

{ TDMLCommand }

constructor TDMLCommand.Create(ATable: TSQLTable);
begin
  inherited Create;
  FTable := ATable;
end;

{ TCreateTableCommand }

constructor TCreateTableCommand.Create(ATable: TSQLTable);
var
  LCaseInsensitiveComparer: IComparer<string>;
begin
  inherited Create(ATable);
  FColumns := TObjectList<TSQLCreateField>.Create(True);
  LCaseInsensitiveComparer := TComparer<string>.Construct(
    function(const Left, Right: string): Integer
    begin
      Result := CompareText(Left, Right);
    end);

  FDbColumns := TList<string>.Create(LCaseInsensitiveComparer);
end;

destructor TCreateTableCommand.Destroy;
begin
  FColumns.Free;
  FDBColumns.Free;
  inherited Destroy;
end;

procedure TCreateTableCommand.SetTable(AColumns: TList<ColumnAttribute>);
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
  FForeigns := TObjectList<TSQLForeignKeyField>.Create(True);
end;

destructor TCreateFKCommand.Destroy;
begin
  FForeigns.Free;
  inherited Destroy;
end;

procedure TCreateFKCommand.SetTable(AColumns: TList<ColumnAttribute>);
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

end.
