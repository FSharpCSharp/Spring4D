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

unit Spring.Persistence.Adapters.ADO;

{$I Spring.inc}

interface

{$IFDEF MSWINDOWS}
uses
  ADODB,
  DB,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.Mapping.Attributes;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent ADO resultset.
  ///	</summary>
  {$ENDREGION}
  TADOResultSetAdapter = class(TDriverResultSetAdapter<TADODataSet>)
  private
    FFieldCache: IFieldCache;
  public
    constructor Create(const ADataset: TADODataSet); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  end;

  EADOStatementAdapterException = Exception;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent ADO statement.
  ///	</summary>
  {$ENDREGION}
  TADOStatementAdapter = class(TDriverStatementAdapter<TADOQuery>)
  public
    constructor Create(const AStatement: TADOQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParam(ADBParam: TDBParam); virtual;
    procedure SetParams(Params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent ADO connection.
  ///	</summary>
  {$ENDREGION}
  TADOConnectionAdapter = class(TDriverConnectionAdapter<TADOConnection>)
  public
    constructor Create(const AConnection: TADOConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent ADO transaction.
  ///	</summary>
  {$ENDREGION}
  TADOTransactionAdapter = class(TDriverTransactionAdapter<TADOConnection>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent <b>ADO</b> SQL generator.
  ///	</summary>
  {$ENDREGION}
  TADOSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string; override;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  ADOConst,
  StrUtils,
  Spring.Persistence.SQL.Register,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;

type
  EADOAdapterException = class(Exception);

{ TADOResultSetAdapter }

constructor TADOResultSetAdapter.Create(const ADataset: TADODataSet);
begin
  inherited Create(ADataset);
  Dataset.DisableControls;
//  Dataset.CursorLocation := clUseServer;
//  Dataset.CursorType := ctOpenForwardOnly;
  FFieldCache := TFieldCache.Create(ADataset);
end;

destructor TADOResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited Destroy;
end;

function TADOResultSetAdapter.FieldNameExists(const AFieldName: string): Boolean;
begin
  Result := FFieldCache.FieldNameExists(AFieldName);
end;

function TADOResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TADOResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := Dataset.Fields[AIndex].FieldName;
end;

function TADOResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TADOResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FFieldCache.GetFieldValue(AFieldname);
end;

function TADOResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.Eof;
end;

function TADOResultSetAdapter.Next: Boolean;
begin
  Dataset.Next;
  Result := not Dataset.Eof;
end;

{ TADOStatementAdapter }

constructor TADOStatementAdapter.Create(const AStatement: TADOQuery);
begin
  inherited Create(AStatement);
end;

destructor TADOStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TADOStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Result := Statement.ExecSQL;
end;

function TADOStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
var
  LStmt: TADODataSet;
begin
  inherited;
  LStmt := TADODataSet.Create(nil);
//  if AServerSideCursor then
//    LStmt.CursorLocation := clUseServer;
  LStmt.CursorType := ctOpenForwardOnly;
  LStmt.CacheSize := 50;
  LStmt.Connection := Statement.Connection;
  LStmt.CommandText := Statement.SQL.Text;
  LStmt.Parameters.AssignValues(Statement.Parameters);
  LStmt.DisableControls;
  try
    LStmt.Open;
    Result := TADOResultSetAdapter.Create(LStmt);
  except
    on E:Exception do
    begin
      //make sure that resultset is always created to avoid memory leak
      Result := TADOResultSetAdapter.Create(LStmt);
      raise EADOAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;

procedure TADOStatementAdapter.SetParam(ADBParam: TDBParam);
var
  sParamName: string;
begin
  sParamName := ADBParam.Name;
  //strip leading : in param name because ADO does not like them
  if (ADBParam.Name <> '') and StartsStr(':', ADBParam.Name) then
    sParamName := Copy(ADBParam.Name, 2, Length(ADBParam.Name));
  Statement.Parameters.ParamValues[sParamName] := ADBParam.Value;
end;

procedure TADOStatementAdapter.SetParams(Params: IList<TDBParam>);
var
  LParam: TDBParam;
begin
  inherited;
  for LParam in Params do
    SetParam(LParam);
end;

procedure TADOStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  Statement.SQL.Text := ACommandText;
end;

{ TADOConnectionAdapter }

function TADOConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

  if not Connection.InTransaction then
    Connection.BeginTrans;

  Result := TADOTransactionAdapter.Create(Connection);
end;

procedure TADOConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

constructor TADOConnectionAdapter.Create(const AConnection: TADOConnection);
begin
  inherited Create(AConnection);
  Connection.LoginPrompt := False;
end;

function TADOConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TADOQuery;
  LAdapter: TADOStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TADOQuery.Create(nil);
  LStatement.Connection := Connection;

  LAdapter := TADOStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TADOConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TADOConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_ADO;
end;

function TADOConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

{ TADOTransactionAdapter }

procedure TADOTransactionAdapter.Commit;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.CommitTrans;
end;

function TADOTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

procedure TADOTransactionAdapter.Rollback;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.RollbackTrans;
end;

{ TADOSQLGenerator }

function TADOSQLGenerator.GenerateGetLastInsertId(AIdentityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

initialization
  TConnectionFactory.RegisterConnection<TADOConnectionAdapter>(dtADO);
{$ENDIF}

end.
