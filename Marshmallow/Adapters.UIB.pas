(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit Adapters.UIB;

interface

uses
  DB, Generics.Collections, Core.Interfaces, Core.Base, SQL.Params, SysUtils
  , SQL.Generator.Ansi, UIB, uibdataset, uiblib, Adapters.FieldCache;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Unified Interbase resultset.
  ///	</summary>
  {$ENDREGION}
  TUIBResultSetAdapter = class(TDriverResultSetAdapter<TUIBDataSet>)
  private
    FFieldCache: IFieldCache;
    FIsNewTransaction: Boolean;
  public
    constructor Create(const ADataset: TUIBDataSet); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;

    property IsNewTransaction: Boolean read FIsNewTransaction write FIsNewTransaction;
  end;

  EUIBStatementAdapterException = Exception;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Unified Interbase statement.
  ///	</summary>
  {$ENDREGION}
  TUIBStatementAdapter = class(TDriverStatementAdapter<TUIBStatement>)
  protected
    procedure AssignParams(AFrom: TSQLParams; ATo: TSQLParams); virtual;
  public
    constructor Create(const AStatement: TUIBStatement); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: TObjectList<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Unified Interbase connection.
  ///	</summary>
  {$ENDREGION}
  TUIBConnectionAdapter = class(TDriverConnectionAdapter<TUIBDataBase>)
  public
    constructor Create(const AConnection: TUIBDataBase); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;

  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents Unified Interbase transaction.
  ///	</summary>
  {$ENDREGION}
  TUIBTransactionAdapter = class(TDriverTransactionAdapter<TUIBTransaction>)
  protected
    function InTransaction(): Boolean; override;
  public
    constructor Create(const ATransaction: TUIBTransaction); override;
    destructor Destroy; override;

    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  SQL.Register
  ,StrUtils
  ,Core.ConnectionFactory
  ,Core.Consts
  ;


type
  EUIBAdapterException = class(Exception);

{ TUIBResultSetAdapter }

constructor TUIBResultSetAdapter.Create(const ADataset: TUIBDataSet);
begin
  inherited Create(ADataset);
  Dataset.OnClose := etmStayIn;
  FFieldCache := TFieldCache.Create(ADataset);
end;

destructor TUIBResultSetAdapter.Destroy;
begin
  if FIsNewTransaction then
    Dataset.Transaction.Free;
  Dataset.Free;
  inherited;
end;

function TUIBResultSetAdapter.FieldnameExists(const AFieldName: string): Boolean;
begin
  Result := FFieldCache.FieldnameExists(AFieldName);
end;

function TUIBResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TUIBResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := Dataset.Fields[AIndex].FieldName;
end;

function TUIBResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TUIBResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FFieldCache.GetFieldValue(AFieldname);
end;

function TUIBResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.Eof;
end;

function TUIBResultSetAdapter.Next: Boolean;
begin
  Dataset.Next;
  Result := not Dataset.Eof;
end;

{ TUIBStatementAdapter }

procedure TUIBStatementAdapter.AssignParams(AFrom, ATo: TSQLParams);
var
  i: Integer;
begin
//  ATo.Parse(Statement.SQL.Text);
  for i := 0 to AFrom.ParamCount - 1 do
  begin
    ATo.AsVariant[i] := AFrom.AsVariant[i];
  end;
end;

constructor TUIBStatementAdapter.Create(const AStatement: TUIBStatement);
begin
  inherited Create(AStatement);
end;

destructor TUIBStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TUIBStatementAdapter.Execute: NativeUInt;
begin
  inherited;
  Statement.Prepare();
  Statement.ExecSQL();
  Result := Statement.RowsAffected;
  Statement.Close(etmStayIn);
end;

function TUIBStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
var
  LDataset: TUIBDataSet;
  LTran: TUIBTransaction;
  LIsNewTran: Boolean;
begin
  inherited;
  LDataset := TUIBDataSet.Create(nil);
  LIsNewTran := (Statement.DataBase.TransactionsCount < 1);
  if not LIsNewTran then
  begin
    LTran := Statement.DataBase.Transactions[0];
  end
  else
  begin
    LTran := TUIBTransaction.Create(nil);
    LTran.DefaultAction := etmRollback;
    LTran.DataBase := Statement.DataBase;
  end;
  LTran.DefaultAction := etmRollback;
  LDataset.DisableControls;
  LDataset.Transaction := LTran;
  LDataset.Database := Statement.DataBase;
  LDataset.UniDirectional := True;
  LDataset.SQL.Text := Statement.SQL.Text;
  AssignParams(Statement.Params, LDataset.Params);
  try
    LDataset.Open();
    Result := TUIBResultSetAdapter.Create(LDataset);
    (Result as TUIBResultSetAdapter).IsNewTransaction := LIsNewTran;
  except
    on E:Exception do
    begin
      Result := TUIBResultSetAdapter.Create(LDataset);
      (Result as TUIBResultSetAdapter).IsNewTransaction := LIsNewTran;
      raise EUIBAdapterException.CreateFmt(EXCEPTION_CANNOT_OPEN_QUERY, [E.Message]);
    end;
  end;
end;

procedure TUIBStatementAdapter.SetParams(Params: TObjectList<TDBParam>);
var
  LParam: TDBParam;
  sParamName: string;
begin
  inherited;
  for LParam in Params do
  begin
    sParamName := LParam.Name;
    //strip leading : in param name because UIB does not like them
    if (LParam.Name <> '') and (StartsStr(':', LParam.Name)) then
    begin
      sParamName := Copy(LParam.Name, 2, Length(LParam.Name));
    end;
    Statement.Params.ByNameAsVariant[sParamName] := LParam.Value;
  end;
end;

procedure TUIBStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  Statement.SQL.Text := ACommandText;
end;

{ TUIBConnectionAdapter }

function TUIBConnectionAdapter.BeginTransaction: IDBTransaction;
var
  LTran: TUIBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

 // if Connection.TransactionsCount < 1 then
 // begin
    LTran := TUIBTransaction.Create(nil);
    LTran.DataBase := Connection;
    LTran.DefaultAction := etmRollback;
    LTran.StartTransaction;
 // end
 // else
 //   LTran := Connection.Transactions[0];

  Result := TUIBTransactionAdapter.Create(LTran);
end;

procedure TUIBConnectionAdapter.Connect;
begin
  if Connection <> nil then
  begin
    Connection.Connected := True;
  end;
end;

constructor TUIBConnectionAdapter.Create(const AConnection: TUIBDataBase);
begin
  inherited Create(AConnection);
end;

function TUIBConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TUIBStatement;
  LTran: TUIBTransaction;
  LAdapter: TUIBStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TUIBStatement.Create(nil);
  if Connection.TransactionsCount > 0 then
    LTran := Connection.Transactions[Connection.TransactionsCount - 1]
  else
  begin
    LTran := TUIBTransaction.Create(nil);
    LTran.DefaultAction := etmRollback;
    LTran.DataBase := Connection;
  end;

  LStatement.DataBase := Connection;
  LStatement.Transaction := LTran;

  LAdapter := TUIBStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

destructor TUIBConnectionAdapter.Destroy;
var
  i: Integer;
begin
  for i := 0 to Connection.TransactionsCount - 1 do
  begin
    Connection.Transactions[i].Free;
  end;
  inherited Destroy;
end;

procedure TUIBConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TUIBConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_UIB;
end;

function TUIBConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

{ TUIBTransactionAdapter }

procedure TUIBTransactionAdapter.Commit;
begin
  if (FTransaction = nil) then
    Exit;

  FTransaction.Commit;
end;

constructor TUIBTransactionAdapter.Create(const ATransaction: TUIBTransaction);
begin
  inherited Create(ATransaction);
  FTransaction.DefaultAction := etmRollback;
  if not InTransaction then
    FTransaction.StartTransaction;
end;

destructor TUIBTransactionAdapter.Destroy;
begin
  inherited Destroy;
  FTransaction.Free;
end;

function TUIBTransactionAdapter.InTransaction: Boolean;
begin
  Result := FTransaction.InTransaction;
end;

procedure TUIBTransactionAdapter.Rollback;
begin
  if (FTransaction = nil) then
    Exit;

  FTransaction.RollBack;
end;

initialization
  TConnectionFactory.RegisterConnection<TUIBConnectionAdapter>(dtUIB);

end.
