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

unit Spring.Persistence.Adapters.Zeos;

{$I Spring.inc}

interface

uses
  DB,
  Generics.Collections,
  SysUtils,
  ZAbstractConnection,
  ZAbstractDataset,
  ZDataset,
  Spring.Persistence.Adapters.FieldCache,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Params;

type
  EZeosStatementAdapterException = Exception;

  TZeosResultSetAdapter = class(TDriverResultSetAdapter<TZAbstractDataset>)
  private
    fFieldCache: IFieldCache;
  public
    constructor Create(const dataSet: TZAbstractDataset); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function FieldNameExists(const fieldName: string): Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldname: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
  end;

  TZeosStatementAdapter = class(TDriverStatementAdapter<TZQuery>)
  public
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetParam(const param: TDBParam); virtual;
    procedure SetParams(const params: TObjectList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
  end;

  TZeosConnectionAdapter = class(TDriverConnectionAdapter<TZAbstractConnection>)
  public
    constructor Create(const AConnection: TZAbstractConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  TZeosTransactionAdapter = class(TDriverTransactionAdapter<TZAbstractConnection>)
  protected
    function InTransaction: Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  StrUtils,
  Spring.Persistence.Core.ConnectionFactory
  Spring.Persistence.Core.Consts;

type
  EZeosAdapterException = class(Exception);


{ TZeosResultSetAdapter }

constructor TZeosResultSetAdapter.Create(const dataSet: TZAbstractDataset);
begin
  inherited Create(dataSet);
  Dataset.DisableControls;
  fFieldCache := TFieldCache.Create(dataSet);
end;

destructor TZeosResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited;
end;

function TZeosResultSetAdapter.FieldNameExists(
  const fieldName: string): Boolean;
begin
  Result := fFieldCache.FieldNameExists(fieldName);
end;

function TZeosResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TZeosResultSetAdapter.GetFieldName(index: Integer): string;
begin
  Result := Dataset.Fields[index].FieldName;
end;

function TZeosResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := Dataset.Fields[index].Value;
end;

function TZeosResultSetAdapter.GetFieldValue(const fieldname: string): Variant;
begin
  Result := fFieldCache.GetFieldValue(fieldname);
end;

function TZeosResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.Eof;
end;

function TZeosResultSetAdapter.Next: Boolean;
begin
  Dataset.Next;
  Result := not Dataset.Eof;
end;

{ TZeosStatementAdapter }

destructor TZeosStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TZeosStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Statement.ExecSQL;
  Result := Statement.RowsAffected;
end;

function TZeosStatementAdapter.ExecuteQuery(
  serverSideCursor: Boolean): IDBResultSet;
var
  LStmt: TZQuery;
begin
  inherited;
  LStmt := TZQuery.Create(nil);
  LStmt.Connection := Statement.Connection;
  LStmt.SQL.Text := Statement.SQL.Text;
  LStmt.Params.AssignValues(Statement.Params);
  LStmt.DisableControls;
  try
    LStmt.Open;
    Result := TZeosResultSetAdapter.Create(LStmt);
  except
    on E:Exception do
    begin
      Result := TZeosResultSetAdapter.Create(LStmt);
      raise EZeosAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;

procedure TZeosStatementAdapter.SetParam(const param: TDBParam);
var
  paramName: string;
begin
  paramName := param.Name;
  if (param.Name <> '') and StartsStr(':', param.Name) then
    paramName := Copy(param.Name, 2, Length(param.Name));
  Statement.Params.ParamValues[paramName] := param.Value;
end;

procedure TZeosStatementAdapter.SetParams(const params: TObjectList<TDBParam>);
var
  param: TDBParam;
begin
  inherited;
  for param in params do
    SetParam(param);
end;

procedure TZeosStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  Statement.SQL.Text := commandText;
end;

{ TZeosConnectionAdapter }

function TZeosConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
  end;

  Result := TZeosTransactionAdapter.Create(Connection);
end;

procedure TZeosConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

constructor TZeosConnectionAdapter.Create(
  const AConnection: TZAbstractConnection);
begin
  inherited Create(AConnection);
  Connection.LoginPrompt := False;
end;

function TZeosConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TZQuery;
  LAdapter: TZeosStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TZQuery.Create(nil);
  LStatement.Connection := Connection;

  LAdapter := TZeosStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TZeosConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TZeosConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_ZEOS;
end;

function TZeosConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

{ TZeosTransactionAdapter }

procedure TZeosTransactionAdapter.Commit;
begin
  if Transaction = nil then
    Exit;

  Transaction.Commit;
end;

function TZeosTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

procedure TZeosTransactionAdapter.Rollback;
begin
  if Transaction = nil then
    Exit;

  Transaction.Rollback;
end;

initialization
  TConnectionFactory.RegisterConnection<TZeosConnectionAdapter>(dtZeos);

end.
