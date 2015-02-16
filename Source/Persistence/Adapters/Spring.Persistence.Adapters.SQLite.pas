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

unit Spring.Persistence.Adapters.SQLite;

interface

uses
  SQLiteTable3,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params;

type
  ESQLiteStatementAdapterException = Exception;

  /// <summary>
  ///   Represents SQLite3 resultset.
  /// </summary>
  TSQLiteResultSetAdapter = class(TDriverResultSetAdapter<ISQLiteTable>)
  public
    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  /// <summary>
  ///   Represents SQLite3 statement.
  /// </summary>
  TSQLiteStatementAdapter = class(TDriverStatementAdapter<ISQLitePreparedStatement>)
  public
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParams(const params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents SQLite3 connection.
  /// </summary>
  TSQLiteConnectionAdapter = class(TDriverConnectionAdapter<TSQLiteDatabase>)
  public
    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  /// <summary>
  ///   Represents SQLite3 transaction.
  /// </summary>
  TSQLiteTransactionAdapter = class(TDriverTransactionAdapter<TSQLiteDatabase>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;

{ TSQLiteResultSetAdapter }

function TSQLiteResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := Dataset.Fields[index].Value;
end;

function TSQLiteResultSetAdapter.FieldNameExists(const fieldName: string): Boolean;
begin
  Result := (Dataset.FindField(fieldName) <> nil);
end;

function TSQLiteResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TSQLiteResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := Dataset.Fields[index].Name;
end;

function TSQLiteResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  Result := Dataset.FieldByName[fieldName].Value;
end;

function TSQLiteResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.EOF;
end;

function TSQLiteResultSetAdapter.Next: Boolean;
begin
  Result := Dataset.Next;
end;

{ TSQLiteStatementAdapter }

function TSQLiteStatementAdapter.Execute: NativeUInt;
var
  affectedRows: Integer;
begin
  inherited;
  if Statement.ExecSQL(affectedRows) then
    Result := affectedRows
  else
    Result := 0;
end;

function TSQLiteStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  LDataset: ISQLiteTable;
begin
  inherited;
  LDataset := Statement.ExecQueryIntf;
  Result := TSQLiteResultSetAdapter.Create(LDataset);
end;

function GetParamByName(AStatement: ISQLitePreparedStatement; const AName: string): TSQliteParam;
var
  i: Integer;
begin
  for i := 0 to AStatement.ParamCount - 1 do
  begin
    if AStatement.Params[i].name = AName then
    begin
      Exit(AStatement.Params[i]);
    end;
  end;
  Result := nil;
end;

procedure TSQLiteStatementAdapter.SetParams(const params: IList<TDBParam>);
var
  param: TDBParam;
begin
  inherited;
  for param in params do
  begin
    Statement.SetParamVariant(param.Name, param.Value);
  end;
end;

procedure TSQLiteStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.PrepareStatement(commandText);
end;

{ TSQLiteConnectionAdapter }

function TSQLiteConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  inherited;

  Connection.ExecSQL(SQL_BEGIN_SAVEPOINT + GetTransactionName);

  Result := TSQLiteTransactionAdapter.Create(Connection);
  Result.TransactionName := GetTransactionName;
end;

procedure TSQLiteConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TSQLiteConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TSQLitePreparedStatement;
  LAdapter: TSQLiteStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TSQLitePreparedStatement.Create(Connection);
  LAdapter := TSQLiteStatementAdapter.Create(Statement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TSQLiteConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TSQLiteConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

function TSQLiteConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_SQLITE;
end;

{ TSQLiteTransactionAdapter }

procedure TSQLiteTransactionAdapter.Commit;
begin
  if Transaction = nil then
    Exit;

  Transaction.ExecSQL('RELEASE SAVEPOINT ' + TransactionName);
end;

function TSQLiteTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.IsTransactionOpen;
end;

procedure TSQLiteTransactionAdapter.Rollback;
begin
  if Transaction = nil then
    Exit;

  Transaction.ExecSQL('ROLLBACK TRANSACTION TO SAVEPOINT ' + TransactionName);
end;

initialization
  TConnectionFactory.RegisterConnection<TSQLiteConnectionAdapter>(dtSQLite);

end.
