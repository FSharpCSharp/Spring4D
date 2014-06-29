unit Adapters.FireDAC;

interface

uses
  Core.Interfaces, Core.Base, SQL.Params, SysUtils
  , SQL.Generator.Ansi, DB, Mapping.Attributes, Adapters.FieldCache
  ,Spring.Collections
  ,FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.VCLUI.Wait
  ;

type

  TFireDACResultSetAdapter = class(TDriverResultSetAdapter<TFDQuery>)
  private
    FFieldCache: IFieldCache;
  public
    constructor Create(const ADataset: TFDQuery); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  end;

  TFireDACStatementAdapter = class(TDriverStatementAdapter<TFDQuery>)
  public
    constructor Create(const AStatement: TFDQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParam(ADBParam: TDBParam); virtual;
    procedure SetParams(Params: IList<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
  end;

  TFireDACConnectionAdapter = class(TDriverConnectionAdapter<TFDConnection>)
  public
    constructor Create(const AConnection: TFDConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  TFireDACTransactionAdapter = class(TDriverTransactionAdapter<TFDTransaction>)
  protected
    function InTransaction(): Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  SQL.Register
  ,StrUtils
  ,Core.ConnectionFactory
  ,Core.Consts
  ,Variants
  ;

type
  EFireDACAdapterException = class(Exception);


{ TFireDACResultSetAdapter }

constructor TFireDACResultSetAdapter.Create(const ADataset: TFDQuery);
begin
  inherited Create(ADataset);
  ADataset.DisableControls;
  FFieldCache := TFieldCache.Create(ADataset);
end;

destructor TFireDACResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited Destroy;
end;

function TFireDACResultSetAdapter.FieldnameExists(
  const AFieldName: string): Boolean;
begin
  Result := FFieldCache.FieldnameExists(AFieldName);
end;

function TFireDACResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TFireDACResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := Dataset.Fields[AIndex].FieldName;
end;

function TFireDACResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TFireDACResultSetAdapter.GetFieldValue(
  const AFieldname: string): Variant;
begin
  Result := FFieldCache.GetFieldValue(AFieldname);
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

{ TFireDACStatementAdapter }

constructor TFireDACStatementAdapter.Create(const AStatement: TFDQuery);
begin
  inherited Create(AStatement);
end;

destructor TFireDACStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TFireDACStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Statement.ExecSQL();
  Result := Statement.RowsAffected;
end;

function TFireDACStatementAdapter.ExecuteQuery(
  AServerSideCursor: Boolean): IDBResultSet;
var
  LStmt: TFDQuery;
begin
  inherited;
  LStmt := TFDQuery.Create(nil);
  LStmt.Connection := Statement.Connection;
  LStmt.SQL.Text := Statement.SQL.Text;
  LStmt.Params.AssignValues(Statement.Params);
  LStmt.DisableControls;
  if AServerSideCursor then
    LStmt.FetchOptions.CursorKind := ckForwardOnly;
  try
    LStmt.Open();
    Result := TFireDACResultSetAdapter.Create(LStmt);
  except
    on E:Exception do
    begin
      //make sure that resultset is always created to avoid memory leak
      Result := TFireDACResultSetAdapter.Create(LStmt);
      raise EFireDACAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;


procedure TFireDACStatementAdapter.SetParam(ADBParam: TDBParam);
var
  sParamName: string;
begin
  sParamName := ADBParam.Name;
  //strip leading : in param name because FireDAC does not like them
  if (ADBParam.Name <> '') and (StartsStr(':', ADBParam.Name)) then
  begin
    sParamName := Copy(ADBParam.Name, 2, Length(ADBParam.Name));
  end;
  Statement.Params.ParamValues[sParamName] := ADBParam.Value;
end;

procedure TFireDACStatementAdapter.SetParams(Params: IList<TDBParam>);
var
  LParam: TDBParam;
begin
  inherited;
  for LParam in Params do
  begin
    SetParam(LParam);
  end;
end;

procedure TFireDACStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  Statement.SQL.Text := ACommandText;
end;

{ TFireDACConnectionAdapter }

function TFireDACConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
  end;

  Result := TFireDACTransactionAdapter.Create(Connection.Transaction as TFDTransaction);
end;

procedure TFireDACConnectionAdapter.Connect;
begin
  if Connection <> nil then
  begin
    Connection.Connected := True;
  end;
end;

constructor TFireDACConnectionAdapter.Create(const AConnection: TFDConnection);
begin
  inherited Create(AConnection);
  Connection.LoginPrompt := False;
end;

function TFireDACConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TFDQuery;
  LAdapter: TFireDACStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TFDQuery.Create(nil);
  LStatement.Connection := Connection;

  LAdapter := TFireDACStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TFireDACConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TFireDACConnectionAdapter.GetDriverName: string;
begin
  Result := 'FireDAC';
end;

function TFireDACConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

{ TFireDACTransactionAdapter }

procedure TFireDACTransactionAdapter.Commit;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.Commit;
end;

function TFireDACTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.Active;
end;

procedure TFireDACTransactionAdapter.Rollback;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.Rollback;
end;

initialization
  TConnectionFactory.RegisterConnection<TFireDACConnectionAdapter>(dtFireDAC);

end.
