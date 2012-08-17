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
  , Mapping.Attributes, Core.EntityMap, Classes, Core.Interfaces;

type
  TUpdateExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TUpdateCommand;
    FColumns: TList<ColumnAttribute>;
    FEntityMap: TEntityMap;
    FMapped: Boolean;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    procedure Execute(AEntity: TObject); override;
    procedure Update(AEntity: TObject; AEntity2: TObject); overload;

    property EntityMap: TEntityMap read FEntityMap write FEntityMap;
  end;

implementation

uses
  Core.Exceptions
  ,Core.EntityCache
  ,Core.Utils
  ,Mapping.RttiExplorer
  ,SysUtils
  ,Rtti
  ,Core.Reflection
  ;

{ TUpdateCommand }

procedure TUpdateExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
  LDirtyObject: TObject;
  iRes: NativeUInt;
begin
  Assert(Assigned(AEntity));

  LStmt := Connection.CreateStatement;

  FMapped := FEntityMap.IsMapped(AEntity);
  if FMapped then
  begin
    LDirtyObject := FEntityMap.Get(AEntity);
    FColumns.Clear;
    TRttiExplorer.GetChangedMembers(AEntity, LDirtyObject, FColumns);
  end;

  FCommand.SetTable(FColumns);

  SQL := Generator.GenerateUpdate(FCommand);

  if (SQL = '') then
    Exit;

  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);

    inherited Execute(AEntity);

    iRes := LStmt.Execute();
    if (iRes < 1) then
    begin
      raise EORMUpdateNotSuccessfulException.Create('Update was not succesful. No records were affected.');
    end;
  finally
    LStmt := nil;
  end;
end;

procedure TUpdateExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
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
   //add fields to tsqltable
//  FCommand.SetTable(FColumns);

 // SQL := Generator.GenerateUpdate(FCommand);
end;

procedure TUpdateExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
  LColumn: ColumnAttribute;
begin
  inherited BuildParams(AEntity);

  for LColumn in FColumns do
  begin
    LParam := CreateParam(AEntity, LColumn);
    SQLParameters.Add(LParam);
  end;

  if Assigned(FCommand.PrimaryKeyColumn) then
  begin
    LParam := CreateParam(AEntity, FCommand.PrimaryKeyColumn);
    SQLParameters.Add(LParam);
  end;
end;

constructor TUpdateExecutor.Create();
begin
  inherited Create();
  FTable := TSQLTable.Create;
  FColumns := TList<ColumnAttribute>.Create;
  FCommand := TUpdateCommand.Create(FTable);
end;

destructor TUpdateExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TUpdateExecutor.Update(AEntity, AEntity2: TObject);
begin
  raise EORMMethodNotImplemented.Create('Method not implemented');
end;

end.
