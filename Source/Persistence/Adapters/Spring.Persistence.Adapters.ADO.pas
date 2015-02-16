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

unit Spring.Persistence.Adapters.ADO;

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
  EADOStatementAdapterException = Exception;

  /// <summary>
  ///   Represent ADO resultset.
  /// </summary>
  TADOResultSetAdapter = class(TDriverResultSetAdapter<TADODataSet>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const ADataset: TADODataSet); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  /// <summary>
  ///   Represent ADO statement.
  /// </summary>
  TADOStatementAdapter = class(TDriverStatementAdapter<TADOQuery>)
  public
    constructor Create(const statement: TADOQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  /// <summary>
  ///   Represent ADO connection.
  /// </summary>
  TADOConnectionAdapter = class(TDriverConnectionAdapter<TADOConnection>)
  public
    constructor Create(const connection: TADOConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  /// <summary>
  ///   Represent ADO transaction.
  /// </summary>
  TADOTransactionAdapter = class(TDriverTransactionAdapter<TADOConnection>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

  /// <summary>
  ///   Represent <b>ADO</b> SQL generator.
  /// </summary>
  TADOSQLGenerator = class(TAnsiSQLGenerator)
  public
    function GenerateGetLastInsertId(const identityColumn: ColumnAttribute): string; override;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  ADOConst,
  StrUtils,
  Variants,
  Spring.Persistence.SQL.Register,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts;

type
  EADOAdapterException = class(Exception);


{$REGION 'TADOResultSetAdapter'}

constructor TADOResultSetAdapter.Create(const ADataset: TADODataSet);
begin
  inherited Create(ADataset);
  Dataset.DisableControls;
//  Dataset.CursorLocation := clUseServer;
//  Dataset.CursorType := ctOpenForwardOnly;
  fFieldCache := TFieldCache.Create(ADataset);
end;

destructor TADOResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited Destroy;
end;

function TADOResultSetAdapter.FieldNameExists(const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldNameExists(fieldName);
end;

function TADOResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TADOResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := Dataset.Fields[index].FieldName;
end;

function TADOResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := Dataset.Fields[index].Value;
end;

function TADOResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  Result := fFieldCache.GetFieldValue(fieldName);
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

{$ENDREGION}


{$REGION 'TADOStatementAdapter'}

constructor TADOStatementAdapter.Create(const statement: TADOQuery);
begin
  inherited Create(statement);
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

function TADOStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
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

procedure TADOStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
  parameter: TParameter;
begin
  paramName := param.NormalizeParamName(':', param.Name);
  parameter := Statement.Parameters.ParamByName(paramName);
  parameter.Value := param.Value;
  if VarIsNull(param.Value) or VarIsEmpty(param.Value) then
    parameter.DataType := param.ParamType;
end;

procedure TADOStatementAdapter.SetParams(const params: IList<TDBParam>);
var
  LParam: TDBParam;
begin
  inherited;
  for LParam in params do
    SetParam(LParam);
end;

procedure TADOStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TADOConnectionAdapter'}

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

constructor TADOConnectionAdapter.Create(const connection: TADOConnection);
begin
  inherited Create(connection);
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

{$ENDREGION}


{$REGION 'TADOTransactionAdapter'}

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

{$ENDREGION}


{$REGION 'TADOSQLGenerator'}

function TADOSQLGenerator.GenerateGetLastInsertId(
  const identityColumn: ColumnAttribute): string;
begin
  Result := '';
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TADOConnectionAdapter>(dtADO);
{$ENDIF}

end.
