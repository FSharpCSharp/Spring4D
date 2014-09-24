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
  SqlExpr, DB, Spring.Collections, Spring.Persistence.Core.Interfaces
  , Spring.Persistence.Core.Base, Spring.Persistence.SQL.Params, SysUtils
  , Spring.Persistence.SQL.Generators.Ansi, DBXCommon, Spring.Persistence.Adapters.FieldCache
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents DBX resultset.
  ///	</summary>
  {$ENDREGION}
  TDBXResultSetAdapter = class(TDriverResultSetAdapter<TSQLQuery>)
  private
    FFieldCache: IFieldCache;
  public
    constructor Create(const ADataset: TSQLQuery); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  end;

  EDBXStatementAdapterException = Exception;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents DBX statement.
  ///	</summary>
  {$ENDREGION}
  TDBXStatementAdapter = class(TDriverStatementAdapter<TSQLQuery>)
  public
    constructor Create(const AStatement: TSQLQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents DBX connection.
  ///	</summary>
  {$ENDREGION}
  TDBXConnectionAdapter = class(TDriverConnectionAdapter<TSQLConnection>)
  public
    constructor Create(const AConnection: TSQLConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents DBX transaction.
  ///	</summary>
  {$ENDREGION}
  TDBXTransactionAdapter = class(TDriverTransactionAdapter<TDBXTransaction>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  Spring.Persistence.SQL.Register
  ,StrUtils
  ,Spring.Persistence.Core.ConnectionFactory
  ,Spring.Persistence.Core.Consts
  ;

type
  EDBXAdapterException = class(Exception);


{ TDBXResultSetAdapter }

constructor TDBXResultSetAdapter.Create(const ADataset: TSQLQuery);
begin
  inherited Create(ADataset);
  Dataset.DisableControls;
 // Dataset.CursorLocation := clUseServer;
 // Dataset.CursorType := ctOpenForwardOnly;
  FFieldCache := TFieldCache.Create(ADataset);
end;

destructor TDBXResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited Destroy;
end;

function TDBXResultSetAdapter.FieldNameExists(const AFieldName: string): Boolean;
begin
  Result := FFieldCache.FieldNameExists(AFieldName);
end;

function TDBXResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TDBXResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := Dataset.Fields[AIndex].FieldName;
end;

function TDBXResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TDBXResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FFieldCache.GetFieldValue(AFieldname);
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

constructor TDBXStatementAdapter.Create(const AStatement: TSQLQuery);
begin
  inherited Create(AStatement);
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

function TDBXStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
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

procedure TDBXStatementAdapter.SetParams(Params: IList<TDBParam>);
var
  LParam: TDBParam;
  sParamName: string;
begin
  inherited;
  for LParam in Params do
  begin
    sParamName := LParam.Name;
    {TODO -oLinas -cGeneral : dont know if DBX has the same param issue as ADO}
    //strip leading : in param name because DBX does not like them
    if (LParam.Name <> '') and (StartsStr(':', LParam.Name)) then
    begin
      sParamName := Copy(LParam.Name, 2, Length(LParam.Name));
    end;
    Statement.Params.ParamValues[sParamName] := LParam.Value;
  end;
end;

procedure TDBXStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  Statement.SQL.Text := ACommandText;
end;

{ TDBXConnectionAdapter }

function TDBXConnectionAdapter.BeginTransaction: IDBTransaction;
var
  LTran: TDBXTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;
 // LTran := nil;
 // if not Connection.InTransaction then
 // begin
  LTran := Connection.BeginTransaction;
//  end;
  Result := TDBXTransactionAdapter.Create(LTran);
end;

procedure TDBXConnectionAdapter.Connect;
begin
  if Connection <> nil then
  begin
    Connection.Connected := True;
  end;
end;

constructor TDBXConnectionAdapter.Create(const AConnection: TSQLConnection);
begin
  inherited Create(AConnection);
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
  if (FTransaction = nil) then
    Exit;

  Transaction.Connection.CommitFreeAndNil(FTransaction);
end;

function TDBXTransactionAdapter.InTransaction: Boolean;
begin
  Result := Assigned(FTransaction);
end;

procedure TDBXTransactionAdapter.Rollback;
begin
  if Assigned(FTransaction) then
    Transaction.Connection.RollbackFreeAndNil(FTransaction);
end;

initialization
  TConnectionFactory.RegisterConnection<TDBXConnectionAdapter>(dtDBX);


end.
