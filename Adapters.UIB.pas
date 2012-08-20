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
  , SQL.AnsiSQLGenerator, UIB, uibdataset;

type
  TUIBResultSetAdapter = class(TDriverResultSetAdapter<TUIBDataSet>)
  private
    FFieldCache: TDictionary<string,TField>;
    FTransaction: TUIBTransaction;
  protected
    procedure BuildFieldCache();
  public
    constructor Create(const ADataset: TUIBDataSet); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
  end;

  EUIBStatementAdapterException = Exception;

  TUIBStatementAdapter = class(TDriverStatementAdapter<TUIBStatement>)
  public
    constructor Create(const AStatement: TUIBStatement); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: TEnumerable<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(): IDBResultSet; override;
  end;

  TUIBConnectionAdapter = class(TDriverConnectionAdapter<TUIBDataBase>, IDBConnection)
  public
    constructor Create(const AConnection: TUIBDataBase); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  TUIBTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FTransaction: TUIBTransaction;
  public
    constructor Create(ATransaction: TUIBTransaction);
    destructor Destroy; override;

    procedure Commit;
    procedure Rollback;
  end;

implementation

uses
  SQL.Register
  ,StrUtils
  ,Core.ConnectionFactory
  ;

{ TUIBResultSetAdapter }

procedure TUIBResultSetAdapter.BuildFieldCache;
var
  i: Integer;
begin
  if FFieldCache.Count = 0 then
  begin
    for i := 0 to Dataset.FieldCount - 1 do
    begin
      FFieldCache.Add(UpperCase(Dataset.Fields[i].FieldName), Dataset.Fields[i]);
    end;
  end;
end;

constructor TUIBResultSetAdapter.Create(const ADataset: TUIBDataSet);
begin
  inherited Create(ADataset);
  FTransaction := TUIBTransaction.Create(nil);
  FTransaction.DataBase := ADataset.Database;
  Dataset.Transaction := FTransaction;
  FFieldCache := TDictionary<string,TField>.Create(Dataset.FieldCount * 2);
  BuildFieldCache();
end;

destructor TUIBResultSetAdapter.Destroy;
begin
  FFieldCache.Free;
  Dataset.Free;
  FTransaction.Free;
  inherited;
end;

function TUIBResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TUIBResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TUIBResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FFieldCache[UpperCase(AFieldname)].Value;
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
  Statement.ExecSQL();
  Result := Statement.RowsAffected;
end;

function TUIBStatementAdapter.ExecuteQuery: IDBResultSet;
var
  LStmt: TUIBDataSet;
begin
  LStmt := TUIBDataSet.Create(nil);
  LStmt.DisableControls;
  LStmt.Database := Statement.DataBase;
  LStmt.SQL.Text := Statement.SQL.Text;
  {TODO -oLinas -cGeneral : clone params}
  LStmt.Open();
  Result := TUIBResultSetAdapter.Create(LStmt);
end;

procedure TUIBStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
begin
  inherited;
end;

procedure TUIBStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
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

  if Connection.TransactionsCount < 1 then
  begin
    LTran := TUIBTransaction.Create(nil);
    LTran.DataBase := Connection;
    LTran.StartTransaction;
  end
  else
    LTran := Connection.Transactions[0];

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
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TUIBStatement.Create(nil);
  LStatement.DataBase := Connection;

  Result := TUIBStatementAdapter.Create(LStatement);
end;

procedure TUIBConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TUIBConnectionAdapter.GetDriverName: string;
begin
  Result := 'UIB';
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

constructor TUIBTransactionAdapter.Create(ATransaction: TUIBTransaction);
begin
  inherited Create;
  FTransaction := ATransaction;
end;

destructor TUIBTransactionAdapter.Destroy;
begin
  if FTransaction.InTransaction then
    FTransaction.RollBack;
  FTransaction.Free;
  inherited Destroy;
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
