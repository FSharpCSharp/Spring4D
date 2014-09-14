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
unit Spring.Persistence.SQL.Commands.TableCreator;

interface

uses
  Spring.Persistence.SQL.AbstractCommandExecutor, Spring.Persistence.SQL.Commands
  , Spring.Persistence.SQL.Types, Spring.Collections;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing statements which create tables
  ///	  in the database.
  ///	</summary>
  {$ENDREGION}
  TTableCreateExecutor = class(TAbstractCommandExecutor)
  private
    FCommand: TCreateTableCommand;
    FTable: TSQLTable;
    FSQLs: IList<string>;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;

    procedure Execute(AEntity: TObject); override;

    procedure CreateTables(AEntity: TClass);

    property Table: TSQLTable read FTable;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,Spring.Persistence.Core.EntityCache
  ,Spring.Persistence.Core.Interfaces
  ,Spring.Persistence.Mapping.Attributes
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
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);
  FCommand.SetTable(LEntityData.Columns);
  FCommand.TableExists := TableExists(FTable.Name);
  if FCommand.TableExists then
  begin
    //get current columns from db table
    FillDbTableColumns(FTable.Name, FCommand.DbColumns);
  end;

  FSQLs := Generator.GenerateCreateTable(FCommand);
end;

constructor TTableCreateExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FCommand := TCreateTableCommand.Create(FTable);
  FSQLs := nil;
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
  LSql: string;
begin
  for LSql in FSQLs do
  begin
    SQL := LSql;
    if (SQL = '') then
      Continue;

    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(SQL);
    //inherited only when SQL's are constructed
    inherited Execute(AEntity);

    LStmt.Execute();
  end;
end;

function TTableCreateExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

end.
