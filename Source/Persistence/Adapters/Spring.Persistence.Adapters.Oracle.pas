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

unit Spring.Persistence.Adapters.Oracle;

interface

uses
  SysUtils,
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  EOracleStatementAdapterException = Exception;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle resultset.
  ///	</summary>
  {$ENDREGION}
  TOracleResultsetAdapter = class(TADOResultSetAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle statement.
  ///	</summary>
  {$ENDREGION}
  TOracleStatementAdapter = class(TADOStatementAdapter)
  public
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle connection.
  ///	</summary>
  {$ENDREGION}
  TOracleConnectionAdapter = class(TADOConnectionAdapter)
  public
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
    function CreateStatement: IDBStatement; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Oracle transaction.
  ///	</summary>
  {$ENDREGION}
  TOracleTransactionAdapter = class(TADOTransactionAdapter)
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  ADODB,
  {$ENDIF}
  StrUtils,
  Variants,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;

{ TOracleConnectionAdapter }

function TOracleConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

  GenerateNewID;

  Connection.Execute(SQL_BEGIN_SAVEPOINT + GetTransactionName);

  Result := TOracleTransactionAdapter.Create(Connection);
  Result.TransactionName := GetTransactionName;
end;

function TOracleConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TADOQuery;
  LAdapter: TOracleStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TADOQuery.Create(nil);
  LStatement.Connection := Connection;

  LAdapter := TOracleStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

function TOracleConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_ORACLE;
end;

{ TOracleStatementAdapter }

function TOracleStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
begin
  Result := inherited ExecuteQuery(serverSideCursor);
end;

{ TOracleTransactionAdapter }

procedure TOracleTransactionAdapter.Commit;
begin
  if Transaction = nil then
    Exit;

  Transaction.Execute('COMMIT');
end;

procedure TOracleTransactionAdapter.Rollback;
begin
  if Transaction = nil then
    Exit;

  Transaction.Execute(SQL_ROLLBACK_SAVEPOINT + TransactionName);
end;

initialization
  TConnectionFactory.RegisterConnection<TOracleConnectionAdapter>(dtOracle);

end.
