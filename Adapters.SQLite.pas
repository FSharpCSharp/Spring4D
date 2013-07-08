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
unit Adapters.SQLite;

interface

uses
  Generics.Collections, Core.Interfaces, SQLiteTable3, Core.Base, SQL.Params, SysUtils
  , Mapping.Attributes;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents SQLite3 resultset.
  ///	</summary>
  {$ENDREGION}
  TSQLiteResultSetAdapter = class(TDriverResultSetAdapter<ISQLiteTable>)
  public
    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  end;

  ESQLiteStatementAdapterException = Exception;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents SQLite3 statement.
  ///	</summary>
  {$ENDREGION}
  TSQLiteStatementAdapter = class(TDriverStatementAdapter<ISQLitePreparedStatement>)
  public
    constructor Create(const AStatement: ISQLitePreparedStatement); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: TObjectList<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents SQLite3 connection.
  ///	</summary>
  {$ENDREGION}
  TSQLiteConnectionAdapter = class(TDriverConnectionAdapter<TSQLiteDatabase>)
  public
    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents SQLite3 transaction.
  ///	</summary>
  {$ENDREGION}
  TSQLiteTransactionAdapter = class(TDriverTransactionAdapter<TSQLiteDatabase>)
  protected
    function InTransaction(): Boolean; override;
  public
    procedure Commit; override;
    procedure Rollback; override;
  end;

implementation

uses
  Core.ConnectionFactory
  ,Core.Consts
  ;

{ TSQLiteResultSetAdapter }

function TSQLiteResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := Dataset.Fields[AIndex].Value;
end;

function TSQLiteResultSetAdapter.FieldnameExists(const AFieldName: string): Boolean;
begin
  Result := (Dataset.FindField(AFieldName) <> nil);
end;

function TSQLiteResultSetAdapter.GetFieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TSQLiteResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := Dataset.Fields[AIndex].Name;
end;

function TSQLiteResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := Dataset.FieldByName[AFieldname].Value;
end;

function TSQLiteResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.EOF;
end;

function TSQLiteResultSetAdapter.Next: Boolean;
begin
  Result := Dataset.Next;
end;

{ TSQLiteStatementAdapter }

constructor TSQLiteStatementAdapter.Create(const AStatement: ISQLitePreparedStatement);
begin
  inherited Create(AStatement);
end;

destructor TSQLiteStatementAdapter.Destroy;
begin
  inherited Destroy;
end;

function TSQLiteStatementAdapter.Execute: NativeUInt;
var
  LAffected: Integer;
begin
  inherited;
  if Statement.ExecSQL(LAffected) then
    Result := LAffected
  else
    Result := 0;
end;

function TSQLiteStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
var
  LDataset: ISQLiteTable;
begin
  inherited;
  LDataset := Statement.ExecQueryIntf;
  Result := TSQLiteResultSetAdapter.Create(LDataset);
end;

function GetParamByName(AStatement: ISQLitePreparedStatement; const AName: string): TSQliteParam;
var
  i: Integer;
begin
  for i := 0 to AStatement.ParamCount - 1 do
  begin
    if AStatement.Params[i].name = AName then
    begin
      Exit(AStatement.Params[i]);
    end;
  end;
  Result := nil;
end;

procedure TSQLiteStatementAdapter.SetParams(Params: TObjectList<TDBParam>);
var
  P: TDBParam;
begin
  inherited;
  for P in Params do
  begin
   { LParam := GetParamByName(FSQLiteStmt, P.Name);
    if not Assigned(LParam) then
      raise ESQLiteStatementAdapterException.CreateFmt('Cannot find parameter named %S', [P.Name]);}

    Statement.SetParamVariant(P.Name, P.Value);
  end;
end;

procedure TSQLiteStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  Statement.PrepareStatement(ACommandText);
end;

{ TSQLiteConnectionAdapter }

function TSQLiteConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  inherited;

  Connection.ExecSQL(SQL_BEGIN_SAVEPOINT + GetTransactionName);

  Result := TSQLiteTransactionAdapter.Create(Connection);
  Result.TransactionName := GetTransactionName;
end;

procedure TSQLiteConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TSQLiteConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TSQLitePreparedStatement;
  LAdapter: TSQLiteStatementAdapter;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TSQLitePreparedStatement.Create(Connection);
  LAdapter := TSQLiteStatementAdapter.Create(Statement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TSQLiteConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TSQLiteConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := False;
end;

function TSQLiteConnectionAdapter.GetDriverName: string;
begin
  Result := DRIVER_SQLITE;
end;

{ TSQLiteTransactionAdapter }

procedure TSQLiteTransactionAdapter.Commit;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.ExecSQL('RELEASE SAVEPOINT ' + TransactionName);
end;

function TSQLiteTransactionAdapter.InTransaction: Boolean;
begin
  Result := Transaction.IsTransactionOpen;
end;

procedure TSQLiteTransactionAdapter.Rollback;
begin
  if (Transaction = nil) then
    Exit;

  Transaction.ExecSQL('ROLLBACK TRANSACTION TO SAVEPOINT ' + TransactionName);
end;

initialization
  TConnectionFactory.RegisterConnection<TSQLiteConnectionAdapter>(dtSQLite);

end.
