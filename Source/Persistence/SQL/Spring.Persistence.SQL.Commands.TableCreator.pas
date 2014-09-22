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

unit Spring.Persistence.SQL.Commands.TableCreator;

interface

uses
  Spring.Collections,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

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
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;

    procedure Execute(AEntity: TObject); override;

    procedure CreateTables(AEntity: TClass);

    property Table: TSQLTable read FTable;
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes;

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

    LStmt.Execute;
  end;
end;

function TTableCreateExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

end.
