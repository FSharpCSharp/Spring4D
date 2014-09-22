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

unit Spring.Persistence.Adapters.ASA;

{$I Spring.inc}

interface

uses
  Spring.Persistence.Adapters.ADO, SysUtils, Spring.Persistence.Core.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA resultset.
  ///	</summary>
  {$ENDREGION}
  TASAResultsetAdapter = class(TADOResultSetAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA statement.
  ///	</summary>
  {$ENDREGION}
  TASAStatementAdapter = class(TADOStatementAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA connection.
  ///	</summary>
  {$ENDREGION}
  TASAConnectionAdapter = class(TADOConnectionAdapter)
  public
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent Sybase ASA transaction.
  ///	</summary>
  {$ENDREGION}
  TASATransactionAdapter = class(TADOTransactionAdapter)
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

  ESybaseASAStatementAdapterException = Exception;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory
  ,Spring.Persistence.Core.Consts
  ;

{ TASAConnectionAdapter }

function TASAConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if (Connection = nil) then
    Exit(nil);

  Connection.Connected := True;

  GenerateNewID;

  Connection.Execute(SQL_BEGIN_TRAN + GetTransactionName);

  Result := TASATransactionAdapter.Create(Connection);
  Result.TransactionName := GetTransactionName;
end;

function TASAConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_SYBASE_ASA;
end;

{ TASATransactionAdapter }

procedure TASATransactionAdapter.Commit;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.Execute(SQL_COMMIT_TRAN + TransactionName);
end;

procedure TASATransactionAdapter.Rollback;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.Execute(SQL_ROLLBACK_TRAN + TransactionName);
end;

initialization
  TConnectionFactory.RegisterConnection<TASAConnectionAdapter>(dtASA);
end.
