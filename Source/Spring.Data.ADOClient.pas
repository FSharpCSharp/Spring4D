{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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

unit Spring.Data.ADOClient experimental;

{$I Spring.inc}

interface

uses
  DB, ADODB,
  Spring.System, Spring.Collections, Spring.Data;

type
  TADOConnectionAdapter = class(TInterfacedObject, IDBConnection, IDBTransaction)
  private
    fConnection: TADOConnection;
    fIsolationLevel: TIsolationLevel;
    fTransactionLevel: Integer;
    function GetConnected: Boolean;
    function GetConnectionTimeout: Integer;
    function GetConnectionString: string;
    function GetDatabase: string;
    function GetState: TConnectionState;
    function GetInTransaction: Boolean;
    procedure SetConnectionString(const value: string);
    procedure SetConnected(value: Boolean);
  protected
    function GetADOConnection: TADOConnection;
    { Implements IDBTransaction }
    procedure Commit;
    procedure Rollback;
    function GetConnection: IDBConnection;
    function GetIsolationLevel: TIsolationLevel;
    property Connection: IDBConnection read GetConnection;
    property IsolationLevel: TIsolationLevel read GetIsolationLevel;
  public
    constructor Create(connection: TADOConnection);
    procedure Open;
    procedure Close;
    function BeginTransaction: IDBTransaction; overload;
    function BeginTransaction(isolationLevel: TIsolationLevel): IDBTransaction; overload;
    property Connected: Boolean read GetConnected write SetConnected;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property ConnectionTimeout: Integer read GetConnectionTimeout;
    property Database: string read GetDatabase;
    property State: TConnectionState read GetState;
    property InTransaction: Boolean read GetInTransaction;
  end;

implementation

{ TADOConnectionAdapter }

constructor TADOConnectionAdapter.Create(connection: TADOConnection);
begin
  inherited Create;
  fConnection := connection;
  fTransactionLevel := 0;
end;

function TADOConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  Result := BeginTransaction(Spring.Data.ilReadCommitted);
end;

function TADOConnectionAdapter.BeginTransaction(
  isolationLevel: TIsolationLevel): IDBTransaction;
const
  IsolationLevels: array[Spring.Data.TIsolationLevel] of ADODB.TIsolationLevel = (
    ADODB.ilUnspecified,
    ADODB.ilChaos,
    ADODB.ilReadUncommitted,
    ADODB.ilReadCommitted,
    ADODB.ilRepeatableRead,
    ADODB.ilSerializable,
    ADODB.ilUnspecified     // TEMP
  );
begin
  if fTransactionLevel < 0 then
    fTransactionLevel := 0;
  Inc(fTransactionLevel);
  if fTransactionLevel = 1 then
    fConnection.BeginTrans;
  fIsolationLevel := isolationLevel;
  fConnection.IsolationLevel := IsolationLevels[fIsolationLevel];
  Result := Self;
end;

procedure TADOConnectionAdapter.Open;
begin
  fConnection.Open;
end;

procedure TADOConnectionAdapter.Close;
begin
  fConnection.Close;
end;

procedure TADOConnectionAdapter.Commit;
begin
  Dec(fTransactionLevel);
  if fTransactionLevel = 0 then
  begin
    GetADOConnection.CommitTrans;
  end;
end;

procedure TADOConnectionAdapter.Rollback;
begin
  Dec(fTransactionLevel);
  if fTransactionLevel = 0 then
  begin
    GetADOConnection.RollbackTrans;
  end;
end;

function TADOConnectionAdapter.GetConnection: IDBConnection;
begin
  Result := Self;
end;

function TADOConnectionAdapter.GetADOConnection: TADOConnection;
begin
  Result := fConnection;
end;

function TADOConnectionAdapter.GetConnected: Boolean;
begin
  Result := fConnection.Connected;
end;

function TADOConnectionAdapter.GetConnectionString: string;
begin
  Result := fConnection.ConnectionString;
end;

function TADOConnectionAdapter.GetConnectionTimeout: Integer;
begin
  Result := fConnection.ConnectionTimeout;
end;

function TADOConnectionAdapter.GetDatabase: string;
begin
  { TODO: GetDatabase }
end;

function TADOConnectionAdapter.GetInTransaction: Boolean;
begin
  Result := fConnection.InTransaction;
end;

function TADOConnectionAdapter.GetIsolationLevel: TIsolationLevel;
begin
  Result := fIsolationLevel;
end;

function TADOConnectionAdapter.GetState: TConnectionState;
begin
  if stClosed in fConnection.State then
  begin
    Result := csClosed;
  end
  else if stOpen in fConnection.State then
  begin
    Result := csOpen;
  end
  else if stConnecting in fConnection.State then
  begin
    Result := csConnecting;
  end
  else if stExecuting in fConnection.State then
  begin
    Result := csExecuting;
  end
  else if stFetching in fConnection.State then
  begin
    Result := csFetching;
  end
  else
  begin
    raise EInvalidOperation.Create('Illegal Connection State');
  end
end;

procedure TADOConnectionAdapter.SetConnected(value: Boolean);
begin
  fConnection.Connected := value;
end;

procedure TADOConnectionAdapter.SetConnectionString(const value: string);
begin
  fConnection.ConnectionString := value;
end;

end.
