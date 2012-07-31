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
unit SQL.Commands.Update;

interface

uses
  SQL.AbstractCommandExecutor, SQL.Types, SQL.Commands, SQL.Params, Generics.Collections
  , Mapping.Attributes;

type
  TUpdateExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TUpdateCommand;
    FSQL: string;
    FColumns: TList<Column>;
    FPrimaryKeyColumnName: string;
  protected
    function BuildParams(AEntity: TObject): TObjectList<TDBParam>; virtual;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;

    procedure Update(AEntity: TObject); overload;
    procedure Update(AEntity: TObject; AEntity2: TObject); overload;
  end;

implementation

uses
  Core.Exceptions
  ,Core.Interfaces
  ,Mapping.RttiExplorer
  ,SysUtils
  ,Rtti
  ;

{ TUpdateCommand }

procedure TUpdateExecutor.Update(AEntity: TObject);
var
  LTran: IDBTransaction;
  LStmt: IDBStatement;
  LParams: TObjectList<TDBParam>;
begin
  Assert(Assigned(AEntity));

  LTran := Connection.BeginTransaction;
  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(FSQL);
  {TODO -oLinas -cGeneral : assign parameters}
  LParams := BuildParams(AEntity);
  try
    LStmt.SetParams(LParams);

    LStmt.Execute();

    LTran.Commit;
  finally
    LTran := nil;
    LStmt := nil;
    LParams.Free;
  end;
end;

procedure TUpdateExecutor.Build(AClass: TClass);
var
  LAtrTable: Table;
begin
  EntityClass := AClass;
  LAtrTable := TRttiExplorer.GetTable(EntityClass);
  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.Create('Table not specified');

  FTable.SetFromAttribute(LAtrTable);

  if Assigned(FColumns) then
    FreeAndNil(FColumns);
  FColumns := TRttiExplorer.GetColumns(EntityClass);
  FPrimaryKeyColumnName := TRttiExplorer.GetPrimaryKeyColumnName(EntityClass);
   //add fields to tsqltable
  FCommand.PrimaryKeyColumnName := FPrimaryKeyColumnName;
  FCommand.SetTable(FColumns);

  FSQL := Generator.GenerateUpdate(FCommand);
end;

function TUpdateExecutor.BuildParams(AEntity: TObject): TObjectList<TDBParam>;
var
  LParam: TDBParam;
  LColumn: Column;
  LVal: TValue;
begin
  Result := TObjectList<TDBParam>.Create(True);

  for LColumn in FColumns do
  begin
    LParam := TDBParam.Create;
    LParam.Name := ':' + LColumn.Name;
    LVal := TRttiExplorer.GetMemberValue(AEntity, LColumn.ClassMemberName);
    LParam.Value := LVal.AsVariant;
    LParam.ParamType := FromTValueTypeToFieldType(LVal);
    Result.Add(LParam);
  end;
end;

constructor TUpdateExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FCommand := TUpdateCommand.Create(FTable);
  FColumns := nil;
  FPrimaryKeyColumnName := '';
end;

destructor TUpdateExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  if Assigned(FColumns) then
    FColumns.Free;
  inherited Destroy;
end;

procedure TUpdateExecutor.Update(AEntity, AEntity2: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

end.
