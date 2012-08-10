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
unit SQL.Commands.Select;

{$I sv.inc}

interface

uses
  SQL.AbstractCommandExecutor, Rtti, SQL.Commands, SQL.Types, Mapping.Attributes, Core.Interfaces
  {$IFDEF USE_SPRING}
  ,Spring.Collections
  {$ENDIF}
  , Generics.Collections
  ;

type
  TSelectType = (stOne, stList, stObjectList);

  TSelectExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TSelectCommand;
    FColumns: TList<Column>;
    FSelectType: TSelectType;
    FID: TValue;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Execute(AEntity: TObject); override;
    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    function Select(AEntity: TObject): IDBResultset;

    procedure SelectList(AList: TObject) ;
    procedure SelectObjectList(AList: TObject; AEnumMethod: TRttiMethod);

    property ID: TValue read FID write FID;
    property SelectType: TSelectType read FSelectType write FSelectType;
  end;



implementation

uses
  Core.Exceptions
  ,Mapping.RttiExplorer
  ,Core.EntityCache
  ,SQL.Params
  ;

{ TSelectCommand }

procedure TSelectExecutor.Build(AClass: TClass);
var
  LAtrTable: Table;
  LCache: TEntityData;
begin
  EntityClass := AClass;
  LCache := TEntityCache.Get(EntityClass);
  LAtrTable := LCache.EntityTable;

  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.Create('Table not specified');

  FTable.SetFromAttribute(LAtrTable);
  FColumns.Clear;
  FColumns.AddRange(LCache.Columns);

  FCommand.PrimaryKeyColumn := LCache.PrimaryKeyColumn;
  FCommand.SetTable(FColumns);
end;

procedure TSelectExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
begin
  inherited BuildParams(AEntity);

  if Assigned(FCommand.ForeignColumn) then
  begin
    LParam := CreateParam(AEntity, FCommand.ForeignColumn);
    SQLParameters.Add(LParam);
  end
  else if Assigned(FCommand.PrimaryKeyColumn) then
  begin
    LParam := CreateParam(AEntity, FCommand.PrimaryKeyColumn);
    SQLParameters.Add(LParam);
  end;
end;

constructor TSelectExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create();
  FColumns := TList<Column>.Create;
  FCommand := TSelectCommand.Create(FTable);
end;

destructor TSelectExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TSelectExecutor.Execute(AEntity: TObject);
begin
  //add where fields if needed
  FCommand.WhereFields.Clear;
  {DONE -oLinas -cGeneral : Must know for what column to set where condition field and value. Setting always for primary key is not good for foreign key tables}
   //AEntity - base table class type
   //EntityClass - can be foreign table class type
  if EntityClass = AEntity.ClassType then
  begin
    FCommand.SetFromPrimaryColumn();  //select from the same as base table
  end
  else
  begin
    //get foreign column name and compare it to the base table primary key column name
    FCommand.SetFromForeignColumn(AEntity.ClassType, EntityClass);
  end;

  SQL := Generator.GenerateSelect(FCommand);
  inherited Execute(AEntity);
end;

procedure TSelectExecutor.SelectObjectList(AList: TObject; AEnumMethod: TRttiMethod);
begin
  FSelectType := stObjectList;
  {DONE -oLinas -cGeneral : fetch from FResultset}
end;

function TSelectExecutor.Select(AEntity: TObject): IDBResultset;
var
  LStmt: IDBStatement;
begin
  Execute(AEntity);
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  if SQLParameters.Count > 0 then
    LStmt.SetParams(SQLParameters);

  Result := LStmt.ExecuteQuery();
end;

procedure TSelectExecutor.SelectList(AList: TObject);
begin
  FSelectType := stList;
   {DONE -oLinas -cGeneral : fetch from FResultset}
end;

end.
