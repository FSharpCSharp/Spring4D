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

unit Spring.Persistence.SQL.Commands.FKCreator;

interface

uses
  Spring.Collections,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing statements to create foreign
  ///	  keys.
  ///	</summary>
  {$ENDREGION}
  TForeignKeyCreateExecutor = class(TAbstractCommandExecutor)
  private
    FCommand: TCreateFKCommand;
    FTable: TSQLTable;
    FSQLs: IList<string>;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;

    procedure Execute(AEntity: TObject); override;

    procedure CreateForeignKeys(AEntity: TClass);
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes;

{ TForeignKeyCreateCommand }

procedure TForeignKeyCreateExecutor.Build(AClass: TClass);
begin
  inherited Build(AClass);
  if not EntityData.IsTableEntity then
    raise ETableNotSpecified.Create('Table not specified for class: ' + AClass.ClassName);

  FTable.SetFromAttribute(EntityData.EntityTable);
  FCommand.SetCommandFieldsFromColumns(EntityData.Columns);
  FCommand.TableExists := TableExists(FTable.Name);
  if FCommand.TableExists then
  begin
    //get current columns from db table
    FillDbTableColumns(FTable.Name, FCommand.DbColumns);
  end;
  FSQLs := Generator.GenerateCreateFK(FCommand);
end;

constructor TForeignKeyCreateExecutor.Create;
begin
  inherited Create;
  FTable := TSQLTable.Create;
  FCommand := TCreateFKCommand.Create(FTable);
  FSQLs := nil;
end;

procedure TForeignKeyCreateExecutor.CreateForeignKeys(AEntity: TClass);
begin
  Execute(nil);
end;

destructor TForeignKeyCreateExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

procedure TForeignKeyCreateExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
  LSql: string;
begin
  for LSql in FSQLs do
  begin
    SQL := LSql;
    if (SQL = '') then
      Exit;

    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(SQL);
    LStmt.Execute;
  end;
end;

function TForeignKeyCreateExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

end.
