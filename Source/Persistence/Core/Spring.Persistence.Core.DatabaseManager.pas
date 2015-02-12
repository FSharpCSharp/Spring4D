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

unit Spring.Persistence.Core.DatabaseManager;

interface

uses
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.AbstractManager,
  Spring.Persistence.Core.Interfaces;

type
  /// <summary>
  ///   Responsible for building database structure from annotated entities.
  /// </summary>
  TDatabaseManager = class(TAbstractManager)
  private
    fEntities: IList<TClass>;
  protected
    function GetCreateForeignKeyExecutor(entityClass: TClass;
      const connection: IDBConnection): IDDLCommand;
    function GetCreateSequenceExecutor(entityClass: TClass;
      const connection: IDBConnection): IDDLCommand;
    function GetCreateTableExecutor(entityClass: TClass;
      const connection: IDBConnection): IDDLCommand;
    procedure BuildTables(const entities: IList<TClass>); virtual;
    procedure BuildForeignKeys(const entities: IList<TClass>); virtual;
    procedure BuildSequences(const entities: IList<TClass>); virtual;
  public
    constructor Create(const connection: IDBConnection); override;

    procedure BuildDatabase;

    procedure RegisterEntity(entityClass: TClass);
    procedure ClearEntities;

    function EntityExists(entityClass: TClass): Boolean;
  end;

implementation

uses
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Commands.CreateForeignKey,
  Spring.Persistence.SQL.Commands.CreateSequence,
  Spring.Persistence.SQL.Commands.CreateTable;


{$REGION 'TDatabaseManager'}

constructor TDatabaseManager.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fEntities := TRttiExplorer.GetEntities;
end;

procedure TDatabaseManager.BuildDatabase;
var
  transaction: IDBTransaction;
begin
  if not fEntities.Any then
    Exit;

  transaction := Connection.BeginTransaction;

  BuildTables(fEntities);
  BuildForeignKeys(fEntities);
  BuildSequences(fEntities);

  transaction.Commit;
end;

procedure TDatabaseManager.BuildForeignKeys(const entities: IList<TClass>);
var
  entityClass: TClass;
  createForeignKey: IDDLCommand;
begin
  for entityClass in entities do
  begin
    createForeignKey := GetCreateForeignKeyExecutor(entityClass, Connection);
    createForeignKey.Execute;
  end;
end;

procedure TDatabaseManager.BuildSequences(const entities: IList<TClass>);
var
  entityClass: TClass;
  createSequence: IDDLCommand;
begin
  for entityClass in entities do
  begin
    createSequence := GetCreateSequenceExecutor(entityClass, Connection);
    createSequence.Execute;
  end;
end;

procedure TDatabaseManager.BuildTables(const entities: IList<TClass>);
var
  entityClass: TClass;
  createTable: IDDLCommand;
begin
  for entityClass in entities do
  begin
    createTable := GetCreateTableExecutor(entityClass, Connection);
    createTable.Execute;
  end;
end;

procedure TDatabaseManager.ClearEntities;
begin
  fEntities.Clear;
end;

function TDatabaseManager.EntityExists(entityClass: TClass): Boolean;
var
  createTable: IDDLCommand;
begin
  createTable := GetCreateTableExecutor(entityClass, Connection);
  Result := createTable.TableExists;
end;

function TDatabaseManager.GetCreateTableExecutor(entityClass: TClass;
  const connection: IDBConnection): IDDLCommand;
begin
  Result := TCreateTableExecutor.Create(connection);
  Result.Build(entityClass);
end;

function TDatabaseManager.GetCreateForeignKeyExecutor(entityClass: TClass;
  const connection: IDBConnection): IDDLCommand;
begin
  Result := TCreateForeignKeyExecutor.Create(connection);
  Result.Build(entityClass);
end;

function TDatabaseManager.GetCreateSequenceExecutor(entityClass: TClass;
  const connection: IDBConnection): IDDLCommand;
begin
  Result := TCreateSequenceExecutor.Create(connection);
  Result.Build(entityClass);
end;

procedure TDatabaseManager.RegisterEntity(entityClass: TClass);
begin
  fEntities.Add(entityClass);
end;

{$ENDREGION}


end.
