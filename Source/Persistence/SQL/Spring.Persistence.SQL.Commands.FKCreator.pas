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

unit Spring.Persistence.SQL.Commands.FKCreator;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Responsible for building and executing statements to create foreign
  ///   keys.
  /// </summary>
  TForeignKeyCreateExecutor = class(TAbstractCommandExecutor)
  private
    fCommand: TCreateFKCommand;
    fTable: TSQLTable;
    fSQLs: IList<string>;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create(const connection: IDBConnection); override;
    destructor Destroy; override;

    procedure Build(entityClass: TClass); override;
    procedure Execute(const entity: TObject);
    procedure CreateForeignKeys(const entity: TClass);
  end;

implementation

uses
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.Attributes;


{$REGION 'TForeignKeyCreateCommand'}

constructor TForeignKeyCreateExecutor.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fTable := TSQLTable.Create;
  fCommand := TCreateFKCommand.Create(fTable);
end;

destructor TForeignKeyCreateExecutor.Destroy;
begin
  fCommand.Free;
  fTable.Free;
  inherited Destroy;
end;

procedure TForeignKeyCreateExecutor.Build(entityClass: TClass);
begin
  inherited Build(entityClass);
  if not EntityData.IsTableEntity then
    raise ETableNotSpecified.Create('Table not specified for class: ' + entityClass.ClassName);

  fTable.SetFromAttribute(EntityData.EntityTable);
  fCommand.SetCommandFieldsFromColumns(EntityData.Columns);
  fCommand.TableExists := TableExists(fTable.Name);
  if fCommand.TableExists then
    FillDbTableColumns(fTable.Name, fCommand.DbColumns);
  fSQLs := Generator.GenerateCreateFK(fCommand);
end;

procedure TForeignKeyCreateExecutor.CreateForeignKeys(const entity: TClass);
begin
  Execute(nil);
end;

procedure TForeignKeyCreateExecutor.Execute(const entity: TObject);
var
  LStmt: IDBStatement;
  LSql: string;
begin
  for LSql in fSQLs do
  begin
    SQL := LSql;
    if SQL = '' then
      Exit;

    LStmt := Connection.CreateStatement;
    LStmt.SetSQLCommand(SQL);
    LStmt.Execute;
  end;
end;

function TForeignKeyCreateExecutor.GetCommand: TDMLCommand;
begin
  Result := fCommand;
end;

{$ENDREGION}


end.
