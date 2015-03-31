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

unit Spring.Persistence.Adapters.FireDAC;

interface

uses
  DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.DApt,
  FireDAC.DApt.Intf,
  FireDAC.DatS,
  FireDAC.Phys,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Async,
  FireDAC.Stan.Def,
  FireDAC.Stan.Error,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Pool,
  FireDAC.UI.Intf,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params;

type
  TFireDACResultSetAdapter = class(TDriverResultSetAdapter<TFDQuery>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const dataSet: TFDQuery); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  TFireDACStatementAdapter = class(TDriverStatementAdapter<TFDQuery>)
  public
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  TFireDACConnectionAdapter = class(TDriverConnectionAdapter<TFDConnection>)
  public
    constructor Create(const connection: TFDConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  TFireDACTransactionAdapter = class(TDriverTransactionAdapter<TFDTransaction>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  StrUtils,
  Variants,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.SQL.Register;

type
  EFireDACAdapterException = class(Exception);


{$REGION 'TFireDACResultSetAdapter'}

constructor TFireDACResultSetAdapter.Create(const dataSet: TFDQuery);
begin
  inherited Create(dataSet);
  dataSet.DisableControls;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TFireDACResultSetAdapter.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  Dataset.Free;
{$ELSE}
  Dataset.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TFireDACResultSetAdapter.FieldNameExists(
  const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldNameExists(fieldName);
end;

function TFireDACResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TFireDACResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := Dataset.Fields[index].FieldName;
end;

function TFireDACResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := Dataset.Fields[index].Value;
end;

function TFireDACResultSetAdapter.GetFieldValue(
  const fieldName: string): Variant;
begin
  Result := fFieldCache.GetFieldValue(fieldName);
end;

function TFireDACResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.Eof;
end;

function TFireDACResultSetAdapter.Next: Boolean;
begin
  Dataset.Next;
  Result := not Dataset.Eof;
end;

{$ENDREGION}


{$REGION 'TFireDACStatementAdapter'}

destructor TFireDACStatementAdapter.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  Statement.Free;
{$ELSE}
  Statement.DisposeOf;
{$ENDIF}
  inherited Destroy;
end;

function TFireDACStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Statement.ExecSQL;
  Result := Statement.RowsAffected;
end;

function TFireDACStatementAdapter.ExecuteQuery(
  serverSideCursor: Boolean): IDBResultSet;
var
  query: TFDQuery;
begin
  inherited;
  query := TFDQuery.Create(nil);
  query.Connection := Statement.Connection;
  query.SQL.Text := Statement.SQL.Text;
  query.Params.AssignValues(Statement.Params);
  query.DisableControls;
  if serverSideCursor then
    query.FetchOptions.CursorKind := ckForwardOnly;
  try
    query.OpenOrExecute;
    Result := TFireDACResultSetAdapter.Create(query);
  except
    on E:Exception do
    begin
      //make sure that resultset is always created to avoid memory leak
      Result := TFireDACResultSetAdapter.Create(query);
      raise EFireDACAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;

procedure TFireDACStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
  parameter: TFDParam;
begin
  paramName := param.NormalizeParamName(':', param.Name);

  parameter := Statement.ParamByName(paramName);
  parameter.Value := param.Value;
  if parameter.IsNull then
    parameter.DataType := param.ParamType;
end;

procedure TFireDACStatementAdapter.SetParams(const params: IList<TDBParam>);
var
  param: TDBParam;
begin
  inherited;
  for param in params do
    SetParam(param);
end;

procedure TFireDACStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{$ENDREGION}


{$REGION 'TFireDACConnectionAdapter'}

constructor TFireDACConnectionAdapter.Create(const connection: TFDConnection);
begin
  inherited Create(connection);
  Connection.LoginPrompt := False;
end;

function TFireDACConnectionAdapter.BeginTransaction: IDBTransaction;
var
  transaction: TFDTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;
  if (not Connection.InTransaction) or (Connection.TxOptions.EnableNested) then
  begin
    transaction := TFDTransaction.Create(Connection);
    transaction.Connection := Connection;
    transaction.StartTransaction;
  end
  else
    raise EFireDACAdapterException.Create('Transaction already started, and EnableNested transaction is false');

  Result := TFireDACTransactionAdapter.Create(transaction);
end;

procedure TFireDACConnectionAdapter.Connect;
begin
  if Assigned(Connection) then
    Connection.Connected := True;
end;

function TFireDACConnectionAdapter.CreateStatement: IDBStatement;
var
  statement: TFDQuery;
  adapter: TFireDACStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  statement := TFDQuery.Create(nil);
  statement.Connection := Connection;

  adapter := TFireDACStatementAdapter.Create(statement);
  adapter.ExecutionListeners := ExecutionListeners;
  Result := adapter;
end;

procedure TFireDACConnectionAdapter.Disconnect;
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TFireDACConnectionAdapter.GetDriverName: string;
begin
  Result := 'FireDAC';
end;

function TFireDACConnectionAdapter.IsConnected: Boolean;
begin
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TFireDACTransactionAdapter'}

procedure TFireDACTransactionAdapter.Commit;
begin
  if Assigned(Transaction) then
    Transaction.Commit;
end;

function TFireDACTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.Active;
end;

procedure TFireDACTransactionAdapter.Rollback;
begin
  if Assigned(Transaction) then
    Transaction.Rollback;
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TFireDACConnectionAdapter>(dtFireDAC);

end.
