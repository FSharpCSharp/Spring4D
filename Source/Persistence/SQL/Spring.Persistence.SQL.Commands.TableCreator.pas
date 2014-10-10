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

{$I Spring.inc}

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
    fCommand: TCreateTableCommand;
    fTable: TSQLTable;
    fSQLs: IList<string>;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure Execute(const entity: TObject); override;
    procedure CreateTables(entityClass: TClass);

    property Table: TSQLTable read fTable;
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes;


{$REGION 'TTableCreateCommand'}

constructor TTableCreateExecutor.Create;
begin
  inherited Create;
  fTable := TSQLTable.Create;
  fCommand := TCreateTableCommand.Create(fTable);
end;

destructor TTableCreateExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TTableCreateExecutor.Build(entityClass: TClass);
var
  LAtrTable: TableAttribute;
  LEntityData: TEntityData;
begin
  inherited EntityClass := entityClass;
  LEntityData := TEntityCache.Get(entityClass);
  LAtrTable := LEntityData.EntityTable;
  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%s"', [entityClass.ClassName]);

  fTable.SetFromAttribute(LAtrTable);
  fCommand.SetCommandFieldsFromColumns(LEntityData.Columns);
  fCommand.TableExists := TableExists(fTable.Name);
  if fCommand.TableExists then
    FillDbTableColumns(fTable.Name, fCommand.DbColumns);

  fSQLs := Generator.GenerateCreateTable(fCommand);
end;

procedure TTableCreateExecutor.CreateTables(entityClass: TClass);
begin
  Execute(nil);
end;

procedure TTableCreateExecutor.Execute(const entity: TObject);
var
  LStmt: IDBStatement;
  LSql: string;
begin
  for LSql in fSQLs do
  begin
    SQL := LSql;
    if SQL = '' then
      Continue;

    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(SQL);
    LStmt.Execute;
  end;
end;

function TTableCreateExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

{$ENDREGION}


end.
