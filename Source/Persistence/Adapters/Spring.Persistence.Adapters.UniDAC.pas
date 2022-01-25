{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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

unit Spring.Persistence.Adapters.UniDAC;

interface

uses
  SysUtils,
  Uni,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  EUniDACAdapterException = class(EORMAdapterException);

  TUniDACResultSetAdapter = class(TDriverResultSetAdapter<TUniQuery>);

  TUniDACStatementAdapter = class(TDriverStatementAdapter<TUniQuery>)
  public
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IEnumerable<TDBParam>); override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  TUniDACConnectionAdapter = class(TDriverConnectionAdapter<TUniConnection>)
  protected
    constructor Create(const connection: TUniConnection;
      const exceptionHandler: IORMExceptionHandler); override;
  public
    constructor Create(const connection: TUniConnection); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
  end;

  TUniDACTransactionAdapter = class(TDriverTransactionAdapter<TUniTransaction>)
  private
    fOwnsObject: Boolean;
  protected
    function InTransaction: Boolean; override;
  public
    constructor Create(const transaction: TUniTransaction;
      const exceptionHandler: IORMExceptionHandler;
      ownsObject: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TUniDACExceptionHandler = class(TORMExceptionHandler)
  protected
    function GetAdapterException(const exc: Exception;
      const defaultMsg: string): Exception; override;
  end;

implementation

uses
  DB,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.ResourceStrings;


{$REGION 'TUniDACStatementAdapter'}

destructor TUniDACStatementAdapter.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  Statement.Free;
{$ELSE}
  Statement.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TUniDACStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  try
    Statement.ExecSQL;
    Result := Statement.RowsAffected;
  except
    raise HandleException;
  end
end;

function TUniDACStatementAdapter.ExecuteQuery(
  serverSideCursor: Boolean): IDBResultSet;
var
  query: TUniQuery;
begin
  inherited;
  query := TUniQuery.Create(nil);
  query.Connection := Statement.Connection;
  query.SQL.Text := Statement.SQL.Text;
  query.Params.AssignValues(Statement.Params);
  query.DisableControls;

  try
    query.Open;
    Result := TUniDACResultSetAdapter.Create(query, ExceptionHandler);
  except
    on E:Exception do
    begin
      query.Free;
      raise HandleException(Format(SCannotOpenQuery, [E.Message]));
    end;
  end;
end;

procedure TUniDACStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
  parameter: TUniParam;
begin
  paramName := param.GetNormalizedParamName;
  parameter := Statement.ParamByName(paramName);
  parameter.DataType := param.ParamType;
  parameter.Value := param.ToVariant;
end;

procedure TUniDACStatementAdapter.SetParams(const params: IEnumerable<TDBParam>);
begin
  inherited;
  params.ForEach(SetParam);
end;

procedure TUniDACStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TUniDACConnectionAdapter'}

constructor TUniDACConnectionAdapter.Create(const connection: TUniConnection);
begin
  Create(connection, TUniDACExceptionHandler.Create);
end;

constructor TUniDACConnectionAdapter.Create(const connection: TUniConnection;
  const exceptionHandler: IORMExceptionHandler);
begin
  inherited Create(connection, exceptionHandler);
  Connection.LoginPrompt := False;
end;

function TUniDACConnectionAdapter.BeginTransaction: IDBTransaction;
var
  transaction: TUniTransaction;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
    if not Connection.InTransaction then
    begin
      transaction := TUniTransaction.Create(Connection);
      transaction.AddConnection(Connection);
      transaction.StartTransaction;
      Result := TUniDACTransactionAdapter.Create(transaction, ExceptionHandler, True);
    end
    else
      raise EUniDACAdapterException.Create('Transaction already started');
  except
    raise HandleException;
  end
  else
    Result := nil;
end;

procedure TUniDACConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := True;
  except
    raise HandleException;
  end;
end;

function TUniDACConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TUniQuery;
  adapter: TUniDACStatementAdapter;
begin
  if Assigned(Connection) then
  begin
    statement := TUniQuery.Create(nil);
    statement.Connection := Connection;

    adapter := TUniDACStatementAdapter.Create(statement, ExceptionHandler);
    adapter.ExecutionListeners := ExecutionListeners;
    adapter.AllowServerSideCursor := AllowServerSideCursor;
    Result := adapter;
  end
  else
    Result := nil;
end;

destructor TUniDACConnectionAdapter.Destroy;
begin
{$IFDEF AUTOREFCOUNT}
  if AutoFreeConnection then
    Connection.DisposeOf;
{$ENDIF}
  inherited;
end;

procedure TUniDACConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
  try
    Connection.Connected := False;
  except
    raise HandleException;
  end;
end;

function TUniDACConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TUniDACTransactionAdapter'}

constructor TUniDACTransactionAdapter.Create(const transaction: TUniTransaction;
  const exceptionHandler: IORMExceptionHandler; ownsObject: Boolean);
begin
  inherited Create(transaction, exceptionHandler);
  fOwnsObject := ownsObject
end;

destructor TUniDACTransactionAdapter.Destroy;
begin
  try
    inherited Destroy;
  finally
    if fOwnsObject then
    begin
{$IFDEF NEXTGEN}
      fTransaction.DisposeOf;
{$ENDIF}
      fTransaction.Free;
    end;
  end;
end;

procedure TUniDACTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
  try
    Transaction.Commit;
  except
    raise HandleException;
  end;
end;

function TUniDACTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(Transaction) and Transaction.Active;
end;

procedure TUniDACTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
  try
    Transaction.Rollback;
  except
    raise HandleException;
  end;
end;

{$ENDREGION}


{$REGION 'TUniDACExceptionHandler'}

function TUniDACExceptionHandler.GetAdapterException(const exc: Exception;
  const defaultMsg: string): Exception;
begin
  if exc is EUniError then
    Result := EUniDACAdapterException.Create(defaultMsg, EUniError(exc).ErrorCode)
  else if exc is EDatabaseError then
    Result := EUniDACAdapterException.Create(defaultMsg)
  else if defaultMsg <> '' then
    Result := EUniDACAdapterException.Create(DefaultMsg)
  else
    Result := nil;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TUniDACConnectionAdapter>(dtUniDAC);

end.
