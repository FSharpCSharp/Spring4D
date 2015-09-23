{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
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
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  EOracleStatementAdapterException = class(EORMAdapterException);

  /// <summary>
  ///   Represents Oracle resultset.
  /// </summary>
  TOracleResultsetAdapter = class(TADOResultSetAdapter);

  /// <summary>
  ///   Represents Oracle statement.
  /// </summary>
  TOracleStatementAdapter = class(TADOStatementAdapter)
  public
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents Oracle connection.
  /// </summary>
  TOracleConnectionAdapter = class(TADOConnectionAdapter)
  public
    procedure AfterConstruction; override;
    function BeginTransaction: IDBTransaction; override;
    function CreateStatement: IDBStatement; override;
  end;

  /// <summary>
  ///   Represents Oracle transaction.
  /// </summary>
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
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.SQL.Generators.Oracle,
  Spring.Persistence.SQL.Interfaces;


{$REGION 'TOracleConnectionAdapter'}

procedure TOracleConnectionAdapter.AfterConstruction;
begin
  inherited;
  QueryLanguage := qlOracle;
end;

function TOracleConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Assigned(Connection) then
  begin
    Connection.Connected := True;
    GenerateNewID;
    Connection.Execute(SQL_BEGIN_SAVEPOINT + GetTransactionName);

    Result := TOracleTransactionAdapter.Create(Connection, ExceptionHandler);
    Result.TransactionName := GetTransactionName;
  end
  else
    Result := nil;
end;

function TOracleConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TADOQuery;
  adapter: TOracleStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TADOQuery.Create(nil);
    statement.Connection := Connection;

    adapter := TOracleStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    Result := adapter;
  end;
end;

{$ENDREGION}


{$REGION 'TOracleStatementAdapter'}

function TOracleStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
begin
  Result := inherited ExecuteQuery(serverSideCursor);
end;

{$ENDREGION}


{$REGION 'TOracleTransactionAdapter'}

procedure TOracleTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
    Transaction.Execute('COMMIT');
end;

procedure TOracleTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
    Transaction.Execute(SQL_ROLLBACK_SAVEPOINT + TransactionName);
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TOracleConnectionAdapter>(dtOracle);

end.
