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

unit Spring.Persistence.Adapters.DBX;

{$I Spring.inc}

interface

uses
  DB,
  DBXCommon,
  SqlExpr,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params;

type
  EDBXStatementAdapterException = Exception;

  /// <summary>
  ///   Represents DBX resultset.
  /// </summary>
  TDBXResultSetAdapter = class(TDriverResultSetAdapter<TSQLQuery>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const dataSet: TSQLQuery); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldname: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  /// <summary>
  ///   Represents DBX statement.
  /// </summary>
  TDBXStatementAdapter = class(TDriverStatementAdapter<TSQLQuery>)
  public
    constructor Create(const statement: TSQLQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParams(const params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represents DBX connection.
  /// </summary>
  TDBXConnectionAdapter = class(TDriverConnectionAdapter<TSQLConnection>)
  public
    constructor Create(const connection: TSQLConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  /// <summary>
  ///   Represents DBX transaction.
  /// </summary>
  TDBXTransactionAdapter = class(TDriverTransactionAdapter<TDBXTransaction>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  StrUtils,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.SQL.Register;

type
  EDBXAdapterException = class(Exception);

{ TDBXResultSetAdapter }

constructor TDBXResultSetAdapter.Create(const dataSet: TSQLQuery);
begin
  inherited Create(dataSet);
  Dataset.DisableControls;
 // Dataset.CursorLocation := clUseServer;
 // Dataset.CursorType := ctOpenForwardOnly;
  FFieldCache := TFieldCache.Create(dataSet);
end;

destructor TDBXResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited Destroy;
end;

function TDBXResultSetAdapter.FieldNameExists(const fieldName: string): Boolean;
begin
  Result := FFieldCache.FieldNameExists(fieldName);
end;

function TDBXResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TDBXResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := Dataset.Fields[index].FieldName;
end;

function TDBXResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := Dataset.Fields[index].Value;
end;

function TDBXResultSetAdapter.GetFieldValue(const fieldname: string): Variant;
begin
  Result := FFieldCache.GetFieldValue(fieldname);
end;

function TDBXResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.Eof;
end;

function TDBXResultSetAdapter.Next: Boolean;
begin
  Dataset.Next;
  Result := not Dataset.Eof;
end;

{ TDBXStatementAdapter }

constructor TDBXStatementAdapter.Create(const statement: TSQLQuery);
begin
  inherited Create(statement);
end;

destructor TDBXStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TDBXStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Result := Statement.ExecSQL;
end;

function TDBXStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  LStmt: TSQLQuery;
begin
  inherited;
  LStmt := TSQLQuery.Create(nil);
  LStmt.SQLConnection := Statement.SQLConnection;
  LStmt.SQL.Text := Statement.SQL.Text;
  LStmt.Params.AssignValues(Statement.Params);
  LStmt.DisableControls;
  try
    LStmt.Open;
    Result := TDBXResultSetAdapter.Create(LStmt);
  except
    on E:Exception do
    begin
      Result := TDBXResultSetAdapter.Create(LStmt);
      raise EDBXAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;

procedure TDBXStatementAdapter.SetParams(const params: IList<TDBParam>);
var
  LParam: TDBParam;
  sParamName: string;
begin
  inherited;
  for LParam in params do
  begin
    sParamName := LParam.Name;
    {TODO -oLinas -cGeneral : dont know if DBX has the same param issue as ADO}
    //strip leading : in param name because DBX does not like them
    if (LParam.Name <> '') and StartsStr(':', LParam.Name) then
      sParamName := Copy(LParam.Name, 2, Length(LParam.Name));
    Statement.Params.ParamValues[sParamName] := LParam.Value;
  end;
end;

procedure TDBXStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{ TDBXConnectionAdapter }

function TDBXConnectionAdapter.BeginTransaction: IDBTransaction;
var
  LTransaction: TDBXTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;
 // LTran := nil;
 // if not Connection.InTransaction then
 // begin
  LTransaction := Connection.BeginTransaction;
//  end;
  Result := TDBXTransactionAdapter.Create(LTransaction);
end;

procedure TDBXConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

constructor TDBXConnectionAdapter.Create(const connection: TSQLConnection);
begin
  inherited Create(connection);
  Connection.LoginPrompt := False;
end;

function TDBXConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TSQLQuery;
  LAdapter: TDBXStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TSQLQuery.Create(nil);
  LStatement.SQLConnection := Connection;

  LAdapter := TDBXStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TDBXConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TDBXConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_DBX;
end;

function TDBXConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

{ TDBXTransactionAdapter }

procedure TDBXTransactionAdapter.Commit;
begin
  if fTransaction = nil then
    Exit;

  Transaction.Connection.CommitFreeAndNil(fTransaction);
end;

function TDBXTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(fTransaction);
end;

procedure TDBXTransactionAdapter.Rollback;
begin
  if Assigned(fTransaction) then
    Transaction.Connection.RollbackFreeAndNil(fTransaction);
end;

initialization
  TConnectionFactory.RegisterConnection<TDBXConnectionAdapter>(dtDBX);

end.
