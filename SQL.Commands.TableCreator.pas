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
unit SQL.Commands.TableCreator;

interface

uses
  SQL.AbstractCommandExecutor, SQL.Commands, SQL.Types;

type
  TTableCreateExecutor = class(TAbstractCommandExecutor)
  private
    FCommand: TCreateTableCommand;
    FTable: TSQLTable;
  public
    constructor Create(); override;
    destructor Destroy; override;

    function TableExists(): Boolean;

    procedure Build(AClass: TClass); override;

    procedure Execute(AEntity: TObject); override;

    procedure CreateTables(AEntity: TClass);
  end;

implementation

uses
  Core.Exceptions
  ,Core.EntityCache
  ,Core.Interfaces
  ,Mapping.Attributes
  ;

{ TTableCreateCommand }

procedure TTableCreateExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
  LEntityData: TEntityData;
begin
  EntityClass := AClass;
  LEntityData := TEntityCache.Get(AClass);
  LAtrTable := LEntityData.EntityTable;
  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.Create('Table not specified');

  FTable.SetFromAttribute(LAtrTable);
  FCommand.SetTable(LEntityData.Columns);
  FCommand.TableExists := TableExists;

  SQL := Generator.GenerateCreateTable(FCommand);
end;

constructor TTableCreateExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FCommand := TCreateTableCommand.Create(FTable);
end;

procedure TTableCreateExecutor.CreateTables(AEntity: TClass);
begin
  Execute(nil);
end;

destructor TTableCreateExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

procedure TTableCreateExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  if (SQL = '') then
    Exit;

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);
  //inherited only when SQL's are constructed
  inherited Execute(AEntity);

  LStmt.Execute();
end;

function TTableCreateExecutor.TableExists: Boolean;
var
  LSqlTableCount: string;
  LStmt: IDBStatement;
  LResults: IDBResultset;
begin
  Result := False;
  LSqlTableCount := Generator.GetSQLTableCount(FTable.Name);
  if (LSqlTableCount <> '') then
  begin
    try
      LStmt := Connection.CreateStatement;
      LStmt.SetSQLCommand(LSqlTableCount);
      LResults := LStmt.ExecuteQuery;
      Result := not LResults.IsEmpty;
    except
      Result := False;
    end;
  end;
end;

end.
