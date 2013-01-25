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
unit Adapters.ADO;

interface

{$IFDEF MSWINDOWS}

uses
  Generics.Collections, Core.Interfaces, ADODB, Core.Base, SQL.Params, SysUtils
  , SQL.Generator.Ansi, DB, Mapping.Attributes;


type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent ADO resultset.
  ///	</summary>
  {$ENDREGION}
  TADOResultSetAdapter = class(TDriverResultSetAdapter<TADODataSet>)
  private
    FFieldCache: TDictionary<string,TField>;
  protected
    procedure BuildFieldCache();
  public
    constructor Create(const ADataset: TADODataSet); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
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
    procedure SetParams(Params: TEnumerable<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
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
    function InTransaction(): Boolean; override;
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
  SQL.Register
  ,StrUtils
  ,Core.ConnectionFactory
  ,ADOConst
  ,Core.Consts
  ;



{ TADOResultSetAdapter }

procedure TADOResultSetAdapter.BuildFieldCache;
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

constructor TADOResultSetAdapter.Create(const ADataset: TADODataSet);
begin
  inherited Create(ADataset);
  Dataset.DisableControls;
//  Dataset.CursorLocation := clUseServer;
//  Dataset.CursorType := ctOpenForwardOnly;
  FFieldCache := TDictionary<string,TField>.Create(Dataset.FieldCount * 2);
  BuildFieldCache();
end;

destructor TADOResultSetAdapter.Destroy;
begin
  FFieldCache.Free;
  Dataset.Free;
  inherited Destroy;
end;

function TADOResultSetAdapter.FieldnameExists(const AFieldName: string): Boolean;
begin
  Result := FFieldCache.ContainsKey(UpperCase(AFieldName));
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
  Result := FFieldCache[UpperCase(AFieldname)].Value
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
  Result := Statement.ExecSQL();
end;

function TADOStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
var
  LStmt: TADODataSet;
begin
  LStmt := TADODataSet.Create(nil);
  if AServerSideCursor then
    LStmt.CursorLocation := clUseServer;
  LStmt.CursorType := ctOpenForwardOnly;
  LStmt.CacheSize := 50;
  LStmt.Connection := Statement.Connection;
  LStmt.CommandText := Statement.SQL.Text;
  LStmt.Parameters.AssignValues(Statement.Parameters);
  LStmt.DisableControls;
  LStmt.Open();

  Result := TADOResultSetAdapter.Create(LStmt);
end;

procedure TADOStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  LParam: TDBParam;
  sParamName: string;
begin
  for LParam in Params do
  begin
    sParamName := LParam.Name;
    //strip leading : in param name because ADO does not like them
    if (LParam.Name <> '') and (StartsStr(':', LParam.Name)) then
    begin
      sParamName := Copy(LParam.Name, 2, Length(LParam.Name));
    end;
    Statement.Parameters.ParamValues[sParamName] := LParam.Value;
  end;
end;

procedure TADOStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  Statement.SQL.Text := ACommandText;
end;

{ TADOConnectionAdapter }

function TADOConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := True;

  if not Connection.InTransaction then
  begin
    Connection.BeginTrans;
  end;

  Result := TADOTransactionAdapter.Create(Connection);
end;

procedure TADOConnectionAdapter.Connect;
begin
  if Connection <> nil then
  begin
    Connection.Connected := True;
  end;
end;

constructor TADOConnectionAdapter.Create(const AConnection: TADOConnection);
begin
  inherited Create(AConnection);
  Connection.LoginPrompt := False;
end;

function TADOConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TADOQuery;
begin
  if Connection = nil then
    Exit(nil);

  LStatement := TADOQuery.Create(nil);
  LStatement.Connection := Connection;

  Result := TADOStatementAdapter.Create(LStatement);
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
