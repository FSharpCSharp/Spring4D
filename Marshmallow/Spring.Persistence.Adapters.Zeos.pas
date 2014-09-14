{*
* Pü, 2014-06-01
* Adapter for ZeosLib http://zeoslib.sourceforge.net/
* (tested under XE6 with zeoslib 7.1.3a-stable and MySQL)
* This is mainly Copy & Paste code from Adapters.ADO
*}
unit Spring.Persistence.Adapters.Zeos;

interface

uses
  Generics.Collections
  , Spring.Persistence.Core.Base
  , ZAbstractDataset
  , ZDataset
  , Data.DB
  , System.SysUtils
  , Spring.Persistence.Core.Interfaces
  , Spring.Persistence.SQL.Params
  , ZAbstractConnection
  , Spring.Persistence.Adapters.FieldCache
  ;

type

  TZeosResultSetAdapter = class(TDriverResultSetAdapter<TZAbstractDataset>)
  private
    FFieldCache: IFieldCache;
  public
    constructor Create(const ADataset: TZAbstractDataset); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  end;

  EZeosStatementAdapterException = Exception;

  TZeosStatementAdapter = class(TDriverStatementAdapter<TZQuery>)
  public
    constructor Create(const AStatement: TZQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParam(ADBParam: TDBParam); virtual;
    procedure SetParams(Params: TObjectList<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
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
    function InTransaction(): Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  Spring.Persistence.Core.Consts
  , Spring.Persistence.Core.ConnectionFactory
  , System.StrUtils;

type

EZeosAdapterException = class(Exception);


{ TZeosResultSetAdapter }

constructor TZeosResultSetAdapter.Create(const ADataset: TZAbstractDataset);
begin
  inherited Create(ADataset);
  Dataset.DisableControls;
  FFieldCache := TFieldCache.Create(ADataset);
end;

destructor TZeosResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited;
end;

function TZeosResultSetAdapter.FieldnameExists(
  const AFieldName: string): Boolean;
begin
  Result := FFieldCache.FieldnameExists(AFieldName);
end;

function TZeosResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TZeosResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := Dataset.Fields[AIndex].FieldName;
end;

function TZeosResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TZeosResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FFieldCache.GetFieldValue(AFieldname);
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

constructor TZeosStatementAdapter.Create(const AStatement: TZQuery);
begin
  inherited Create(AStatement);
end;

destructor TZeosStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TZeosStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Statement.ExecSQL();
  Result := Statement.RowsAffected;
end;

function TZeosStatementAdapter.ExecuteQuery(
  AServerSideCursor: Boolean): IDBResultSet;
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
    LStmt.Open();
    Result := TZeosResultSetAdapter.Create(LStmt);
  except
    on E:Exception do
    begin
      Result := TZeosResultSetAdapter.Create(LStmt);
      raise EZeosAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;

procedure TZeosStatementAdapter.SetParam(ADBParam: TDBParam);
var
  sParamName: string;
begin
  sParamName := ADBParam.Name;
  if (ADBParam.Name <> '') and (StartsStr(':', ADBParam.Name)) then
  begin
    sParamName := Copy(ADBParam.Name, 2, Length(ADBParam.Name));
  end;
  Statement.Params.ParamValues[sParamName] := ADBParam.Value;
end;

procedure TZeosStatementAdapter.SetParams(Params: TObjectList<TDBParam>);
var
  LParam: TDBParam;
begin
  inherited;
  for LParam in Params do
  begin
    SetParam(LParam);
  end;
end;

procedure TZeosStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  Statement.SQL.Text := ACommandText;
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
  begin
    Connection.Connected := True;
  end;
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
  if (Transaction = nil) then
    Exit;

  Transaction.Commit;
end;

function TZeosTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

procedure TZeosTransactionAdapter.Rollback;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.Rollback;
end;


initialization
  TConnectionFactory.RegisterConnection<TZeosConnectionAdapter>(dtZeos);

end.
