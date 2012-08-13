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
    procedure SetTable(AColumns: TList<Column>); virtual; abstract;
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
    FPrimaryKeyColumn: Column;
    FForeignColumn: ForeignJoinColumnAttribute;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<Column>); override;
    procedure SetFromPrimaryColumn();
    procedure SetFromForeignColumn(ABaseTableClass, AForeignTableClass: TClass);

    property SelectFields: TObjectList<TSQLSelectField> read FSelectFields;
    property Joins: TObjectList<TSQLJoin> read FJoins;
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields;
    property GroupByFields: TObjectList<TSQLGroupByField> read FGroupByFields;
    property OrderByFields: TObjectList<TSQLOrderField> read FOrderByFields;

    property ForeignColumn: ForeignJoinColumnAttribute read FForeignColumn write FForeignColumn;
    property PrimaryKeyColumn: Column read FPrimaryKeyColumn write FPrimaryKeyColumn;
  end;

  TInsertCommand = class(TDMLCommand)
  private
    FInsertFields: TObjectList<TSQLField>;
    FSequence: SequenceAttribute;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<Column>); override;

    property InsertFields: TObjectList<TSQLField> read FInsertFields;
    property Sequence: SequenceAttribute read FSequence write FSequence;
  end;

  TUpdateCommand = class(TDMLCommand)
  private
    FUpdateFields: TObjectList<TSQLField>;
    FWhereFields: TObjectList<TSQLWhereField>;
    FPrimaryKeyColumn: Column;
  public
    constructor Create(ATable: TSQLTable); override;
    destructor Destroy; override;

    procedure SetTable(AColumns: TList<Column>); override;

    property PrimaryKeyColumn: Column read FPrimaryKeyColumn write FPrimaryKeyColumn;
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

    procedure SetTable(AColumns: TList<Column>); override;

    property PrimaryKeyColumnName: string read FPrimaryKeyColumnName write FPrimaryKeyColumnName;
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields;
  end;

implementation

uses
  Mapping.RttiExplorer
  ,Core.EntityCache
  ,SysUtils
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
  FForeignColumn := nil;
end;

destructor TSelectCommand.Destroy;
begin
  FSelectFields.Free;
  FJoins.Free;
  FWhereFields.Free;
  FGroupByFields.Free;
  FOrderByFields.Free;
  inherited Destroy;
end;

procedure TSelectCommand.SetFromForeignColumn(ABaseTableClass, AForeignTableClass: TClass);
var
  LPrimaryKeyColumn: Column;
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

procedure TSelectCommand.SetTable(AColumns: TList<Column>);
var
  LColumn: Column;
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

procedure TInsertCommand.SetTable(AColumns: TList<Column>);
var
  LField: TSQLField;
  LColumn: Column;
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

procedure TUpdateCommand.SetTable(AColumns: TList<Column>);
var
  LField: TSQLField;
  LWhereField: TSQLWhereField;
  LColumn: Column;
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

procedure TDeleteCommand.SetTable(AColumns: TList<Column>);
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

end.
