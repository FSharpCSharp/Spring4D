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

unit Spring.Persistence.Adapters.MSSQL;

interface

{$IFDEF MSWINDOWS}
uses
  SysUtils,
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.Core.Interfaces;

{
  Must use OLEDB povider because ODBC providers are buggy with SQL SERVER
  see: http://stackoverflow.com/questions/4877576/how-to-make-ado-parameter-to-update-sql-server-datetime-column

  Connection string example: 'Provider=SQLOLEDB.1;Password=master;Persist Security Info=True;'+
    'User ID=VIKARINA;Initial Catalog=ViktorDemo;Data Source=FILE_SERVER';
}

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Miscrosoft SQL Server resultset.
  ///	</summary>
  {$ENDREGION}
  TMSSQLResultsetAdapter = class(TADOResultSetAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Miscrosoft SQL Server statement.
  ///	</summary>
  {$ENDREGION}
  TMSSQLStatementAdapter = class(TADOStatementAdapter);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Miscrosoft SQL Server connection.
  ///	</summary>
  {$ENDREGION}
  TMSSQLConnectionAdapter = class(TADOConnectionAdapter)
  public
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Miscrosoft SQL Server transaction.
  ///	</summary>
  {$ENDREGION}
  TMSSQLTransactionAdapter = class(TADOTransactionAdapter)
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

  EMSSQLStatementAdapterException = Exception;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;

{ TMSSQLConnectionAdapter }

function TMSSQLConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

  GenerateNewID;

  Connection.Execute(SQL_BEGIN_TRAN + GetTransactionName);

  Result := TMSSQLTransactionAdapter.Create(Connection);
  Result.TransactionName := GetTransactionName;
end;

function TMSSQLConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_MSSQL;
end;

{ TMSSQLTransactionAdapter }

procedure TMSSQLTransactionAdapter.Commit;
begin
  if Transaction = nil then
    Exit;

  Transaction.Execute(SQL_COMMIT_TRAN + TransactionName);
end;

procedure TMSSQLTransactionAdapter.Rollback;
begin
  if Transaction = nil then
    Exit;

  Transaction.Execute(SQL_ROLLBACK_TRAN + TransactionName);
end;

initialization
  TConnectionFactory.RegisterConnection<TMSSQLConnectionAdapter>(dtMSSQL);

{$ENDIF}

end.
