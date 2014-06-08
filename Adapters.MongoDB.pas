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
unit Adapters.MongoDB;

interface

uses
  MongoDB, MongoBson, Core.Interfaces, Core.Base, SQL.Params, SysUtils
  , Mapping.Attributes, Generics.Collections;

type
  TPageInfo = record
    Limit: Integer;
    Offset: Integer;
  end;

  {TODO -oLinas -cGeneral : finish implementing proper mongo db connection adapter}
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB connection.
  ///	</summary>
  {$ENDREGION}
  TMongoDBConnection = class(TMongo)
  private
    FConnected: Boolean;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  public
    function GetCollectionFromFullName(const AFullConnectionName: string): string; virtual;
  public
    property Connected: Boolean read GetConnected write SetConnected;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB query.
  ///	</summary>
  {$ENDREGION}
  TMongoDBQuery = class(TMongoCursor)
  private
    FConnection: TMongoDBConnection;
    function GetConnection: TMongoDBConnection;
  public
    constructor Create(AConnection: TMongoDBConnection); overload;

    property Connection: TMongoDBConnection read GetConnection;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB resultset.
  ///	</summary>
  {$ENDREGION}
  TMongoResultSetAdapter = class(TDriverResultSetAdapter<TMongoDBQuery>)
  private
    FDoc: IBSONDocument;
    FIsInjected: Boolean;
  public
    constructor Create(const ADataset: TMongoDBQuery); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;

    property Document: IBSONDocument read FDoc write FDoc;
    property IsInjected: Boolean read FIsInjected write FIsInjected;
  end;

  EMongoDBStatementAdapterException = Exception;

  TMongoStatementType = (mstInsert, mstUpdate, mstDelete, mstSelect, mstSelectCount, mtPage);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB statement.
  ///	</summary>
  {$ENDREGION}
  TMongoStatementAdapter = class(TDriverStatementAdapter<TMongoDBQuery>)
  private
    FStmtText: string;
    FStmtType: TMongoStatementType;
    FFullCollectionName: string;
  protected
    function GetStatementType(var AStatementText: string): TMongoStatementType; virtual;
    function GetStatementPageInfo(const AStatement: string; out APageInfo: TPageInfo): string; virtual;
    function IsObjectId(const AValue: string): Boolean; virtual;
  public
    constructor Create(const AStatement: TMongoDBQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: TObjectList<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
    function GetQueryText(): string;
    function GetFullCollectionName(): string; virtual;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB connection.
  ///	</summary>
  {$ENDREGION}
  TMongoConnectionAdapter = class(TDriverConnectionAdapter<TMongoDBConnection>, IDBConnection)
  public
    constructor Create(const AConnection: TMongoDBConnection); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;



  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB transaction.
  ///	</summary>
  {$ENDREGION}
  TMongoTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FConnection: TMongoDBConnection;
  protected
    function GetTransactionName: string;
    procedure SetTransactionName(const Value: string);
  public
    constructor Create(AConnection: TMongoDBConnection);
    destructor Destroy; override;

    procedure Commit;
    procedure Rollback;
  end;

implementation

uses
  StrUtils
  ,Core.ConnectionFactory
  ,Variants
  ,TypInfo
  ;
const
  NAME_COLLECTION = 'UnitTests.MongoAdapter';

var
  MONGO_STATEMENT_TYPES: array[TMongoStatementType] of string = ('I', 'U', 'D', 'S', 'count', 'page');

{ TMongoDBConnection }

{
function TMongoDBConnection.CountQuery(const Collection: WideString; const AQuery: IBSONDocument): IBSONDocument;
var
  LDoc: IBSONDocument;
begin
  LDoc := BSON(['count', GetCollectionFromFullName(Collection)]);

  if AQuery.GetFieldCount > 0 then
    LDoc['query'] := AQuery;


  Result:=RunCommand(GetNamespaceFromFullName(Collection), LDoc); //['n'];
end;      }

function TMongoDBConnection.GetCollectionFromFullName(const AFullConnectionName: string): string;
var
  i: Integer;
begin
  Result := AFullConnectionName;
  i := PosEx('.', Result);
  if (i > 0) then
  begin
    Result := Copy(Result, i + 1, Length(Result));
  end;
end;

function TMongoDBConnection.GetConnected: Boolean;
begin
  Result := isConnected();
end;

procedure TMongoDBConnection.SetConnected(const Value: Boolean);
begin
  FConnected := checkConnection();
  if not Value and FConnected then
  begin
    disconnect();
    FConnected := False;
    Exit;
  end
  else if Value and not FConnected then
    FConnected := reconnect();

  FConnected := Value;
end;

{ TMongoResultSetAdapter }

constructor TMongoResultSetAdapter.Create(const ADataset: TMongoDBQuery);
begin
  inherited Create(ADataset);
  FDoc := nil;
end;

destructor TMongoResultSetAdapter.Destroy;
begin
  FDoc := nil;
  Dataset.Free;
  inherited Destroy;
end;

function TMongoResultSetAdapter.FieldnameExists(const AFieldName: string): Boolean;
var
  LIter: TBsonIterator;
begin
  Result := False;
  if Assigned(FDoc) then
  begin
    LIter := FDoc.find(AFieldName);
    if Assigned(LIter) then
    begin
      Result := True;
      LIter.Free;
    end;
  end;
end;

function TMongoResultSetAdapter.GetFieldCount: Integer;
var
  LIter: TBsonIterator;
  i: Integer;
begin
  if Assigned(FDoc) then
  begin
    LIter := FDoc.iterator;
    i := 0;
    while LIter.next do
    begin
      Inc(i);
    end;
    Result := i;
  end
  else
    Result := 0;
end;

function TMongoResultSetAdapter.GetFieldName(AIndex: Integer): string;
var
  LIter: TBsonIterator;
  i: Integer;
begin
  Assert(Assigned(FDoc), 'Document not assigned');
  LIter := FDoc.iterator;
  i := 0;
  while LIter.next do
  begin
    if AIndex = i then
      Exit(LIter.key);
    Inc(i);
  end;
end;

function TMongoResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Assert(Assigned(FDoc), 'Document not assigned');
  Result := FDoc.value(AFieldname);
end;

function TMongoResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Assert(Assigned(FDoc), 'Document not assigned');
  Result := FDoc.value( GetFieldname(AIndex) );
end;

function TMongoResultSetAdapter.IsEmpty: Boolean;
begin
  Result := not IsInjected and not Dataset.next;
  if not Result and not IsInjected then
  begin
    FDoc := Dataset.value;
  end;
end;

function TMongoResultSetAdapter.Next: Boolean;
begin
  Result := True;
end;

{ TMongoStatementAdapter }

constructor TMongoStatementAdapter.Create(const AStatement: TMongoDBQuery);
begin
  inherited Create(AStatement);
  FStmtText := '';
  FStmtType := mstSelect;
end;

destructor TMongoStatementAdapter.Destroy;
begin
  Statement.Free;
  inherited Destroy;
end;

function TMongoStatementAdapter.Execute: NativeUInt;
var
  LDoc: IBSONDocument;
begin
  inherited;
  LDoc := JsonToBson(FStmtText);
  case FStmtType of
    mstInsert: Statement.Connection.insert(GetFullCollectionName(), LDoc);
    mstUpdate: Statement.Connection.Update(GetFullCollectionName(), bsonEmpty, LDoc);
    mstDelete: Statement.Connection.remove(GetFullCollectionName(), LDoc);
    mstSelect: Statement.Connection.findOne(GetFullCollectionName(), LDoc);
    mstSelectCount: Statement.Connection.Count(GetFullCollectionName(), LDoc);
  end;
  Result := 1;
end;

function TMongoStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
var
  LQuery: TMongoDBQuery;
  LResultset: TMongoResultSetAdapter;
  LPageInfo: TPageInfo;
  LCount: Double;
begin
  inherited;
  LQuery := TMongoDBQuery.Create(Statement.Connection);

  if FStmtType = mtPage then
  begin
    FStmtText := GetStatementPageInfo(FStmtText, LPageInfo);
    LQuery.limit := LPageInfo.Limit;
    LQuery.skip := LPageInfo.Offset;
  end;

  LResultset := TMongoResultSetAdapter.Create(LQuery);
  LResultset.Document := JsonToBson(FStmtText);
  case FStmtType of
    mstInsert:
    begin
      Statement.Connection.Insert(GetFullCollectionName(), LResultset.Document);
      LResultset.IsInjected := True;
    end;
    mstSelect, mtPage:
    begin
      LQuery.query := LResultset.Document;
      if Statement.Connection.find(GetFullCollectionName(), LQuery) then
      begin
        LResultset.Document := nil;
      end;
    end;
    mstSelectCount:
    begin
      LCount := Statement.Connection.count(GetFullCollectionName(), LResultset.Document);
      LResultset.Document := BSON(['n', LCount]);
      LResultset.IsInjected := True;
    end;
  end;
  Result := LResultset;
end;

function TMongoStatementAdapter.GetFullCollectionName: string;
begin
  Result := FFullCollectionName;
end;

function TMongoStatementAdapter.GetQueryText: string;
begin
  Result := FStmtText;
end;

function TMongoStatementAdapter.GetStatementPageInfo(const AStatement: string; out APageInfo: TPageInfo): string;
var
  iStart, iEnd: Integer;
begin
  //ex. page1_10_[Collection]{}
  iStart := PosEx('_', AStatement);
  APageInfo.Limit := StrToInt(Copy(AStatement, 5, iStart - 5) );
  iEnd := PosEx('_', AStatement, iStart+1);
  APageInfo.Offset := StrToInt( Copy(AStatement, (iEnd - iStart) + 1 + 5, iEnd - ((iEnd - iStart) + 1 + 5)) );
  iEnd := PosEx(']', AStatement, iEnd);
  Result := Copy(AStatement, iEnd+1, Length(AStatement));
end;

function TMongoStatementAdapter.GetStatementType(var AStatementText: string): TMongoStatementType;
var
  LIdentifier: string;
  LStartIndex, LEndIndex: Integer;
begin
  AStatementText := Trim(AStatementText);
  if Length(AStatementText) = 0 then
    AStatementText := 'S';

  LStartIndex := PosEx('[', AStatementText);
  LEndIndex := PosEx(']', AStatementText);
  FFullCollectionName := Copy(AStatementText, LStartIndex+1, LEndIndex - LStartIndex-1);

  LIdentifier := AStatementText[1];
  if LIdentifier = 'I' then
    Result := mstInsert
  else if LIdentifier = 'U' then
    Result := mstUpdate
  else if LIdentifier = 'D' then
    Result := mstDelete
  else if (StartsStr('count', AStatementText)) then
  begin
    LIdentifier := 'count';
    Result := mstSelectCount;
  end
  else if (StartsStr('page', AStatementText)) then
  begin
    LIdentifier := 'page';
    Result := mtPage;
    Exit;
  end
  else
    Result := mstSelect;

  AStatementText := Copy(AStatementText, Length(FFullCollectionName) + 2 + Length(LIdentifier) + 1, Length(AStatementText));
end;

function TMongoStatementAdapter.IsObjectId(const AValue: string): Boolean;
begin
  Result := StartsText('ObjectID("', AValue);
end;

procedure TMongoStatementAdapter.SetParams(Params: TObjectList<TDBParam>);
var
  LParam: TDBParam;
  LValue: string;
begin
  inherited;
  for LParam in Params do
  begin
    LValue := LParam.Value;
    case VarType(LParam.Value) of
      varString, varUString, varStrArg, varOleStr:
      begin
        if IsObjectId(LValue) then   //ObjectID("sdsd457845")
        begin
          LValue := ReplaceStr(LValue, '"', '\"');
          LValue := Format('"%S"', [LValue]);
        end
        else
          LValue := AnsiQuotedStr(LValue, '"');
      end;
    end;
    FStmtText := StringReplace(FStmtText, '#$', LValue, []);
  end;
end;

procedure TMongoStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;
  if ACommandText = '' then
    Exit;

  FStmtText := ACommandText;
  FStmtType := GetStatementType(FStmtText);
end;

{ TMongoConnectionAdapter }

function TMongoConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

 { Connection.Connected := True;

  if not Connection.InTransaction then
  begin
    Connection.BeginTrans;
  end;  }

  Result := TMongoTransactionAdapter.Create(Connection);
end;

procedure TMongoConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

constructor TMongoConnectionAdapter.Create(const AConnection: TMongoDBConnection);
begin
  inherited Create(AConnection);
end;

function TMongoConnectionAdapter.CreateStatement: IDBStatement;
var
  LStatement: TMongoDBQuery;
  LAdapter: TMongoStatementAdapter;
begin
  LStatement := TMongoDBQuery.Create(Connection);
  LAdapter := TMongoStatementAdapter.Create(LStatement);
  LAdapter.ExecutionListeners := ExecutionListeners;
  Result := LAdapter;
end;

procedure TMongoConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TMongoConnectionAdapter.GetDriverName: string;
begin
  Result := 'MongoDB';
end;

function TMongoConnectionAdapter.IsConnected: Boolean;
begin
  if Connection = nil then
    Exit(False);
  Result := Connection.Connected;
end;

{ TMongoTransactionAdapter }

procedure TMongoTransactionAdapter.Commit;
begin
  //
end;

constructor TMongoTransactionAdapter.Create(AConnection: TMongoDBConnection);
begin
  inherited Create();
  FConnection := AConnection;
end;

destructor TMongoTransactionAdapter.Destroy;
begin

  inherited Destroy;
end;

function TMongoTransactionAdapter.GetTransactionName: string;
begin
  Result := '';
end;

procedure TMongoTransactionAdapter.Rollback;
begin
  //
end;

procedure TMongoTransactionAdapter.SetTransactionName(const Value: string);
begin
  //
end;

{ TMongoDBQuery }

constructor TMongoDBQuery.Create(AConnection: TMongoDBConnection);
begin
  inherited Create();
  FConnection := AConnection;
end;

function TMongoDBQuery.GetConnection: TMongoDBConnection;
begin
  Result := FConnection;
end;

initialization
  TConnectionFactory.RegisterConnection<TMongoConnectionAdapter>(dtMongo);

end.
