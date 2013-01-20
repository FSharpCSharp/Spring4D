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
  mongoWire, Core.Interfaces, Core.Base, SQL.Params, SysUtils
  , Mapping.Attributes, bsonDoc, Generics.Collections;

type
  {TODO -oLinas -cGeneral : finish implementing proper mongo db connection adapter}
  TMongoDBConnection = class(TMongoWire)
  private
    FConnected: Boolean;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  public
    property Connected: Boolean read GetConnected write SetConnected;
  end;

  TMongoDBQuery = class(TMongoWireQuery)
  private
    FConnection: TMongoDBConnection;
    function GetConnection: TMongoDBConnection;
  public
    constructor Create(AConnection: TMongoDBConnection); overload;

    property Connection: TMongoDBConnection read GetConnection;
  end;

  TMongoResultSetAdapter = class(TDriverResultSetAdapter<TMongoDBQuery>)
  private
    FDoc: IBSONDocument;
  public
    constructor Create(const ADataset: TMongoDBQuery); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;

    property Document: IBSONDocument read FDoc write FDoc;
  end;

  EMongoDBStatementAdapterException = Exception;

  TMongoStatementType = (mstInsert, mstUpdate, mstDelete, mstSelect);

  TMongoStatementAdapter = class(TDriverStatementAdapter<TMongoDBQuery>)
  private
    FStmtText: string;
    FStmtType: TMongoStatementType;
  protected
    function GetStatementType(var AStatementText: string): TMongoStatementType; virtual;
  public
    constructor Create(const AStatement: TMongoDBQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: TEnumerable<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
  end;

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

  TMongoTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FConnection: TMongoDBConnection;
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
  ,bsonUtils
  ;
const
  NAME_COLLECTION = 'UnitTests.MongoAdapter';

var
  MONGO_STATEMENT_TYPES: array[TMongoStatementType] of string = ('I', 'U', 'D', 'S');

{ TMongoDBConnection }

function TMongoDBConnection.GetConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TMongoDBConnection.SetConnected(const Value: Boolean);
begin
  if not Value and Connected then
    Close
  else if Value and not Connected then
    Open();

  FConnected := Value;
end;

{ TMongoResultSetAdapter }

constructor TMongoResultSetAdapter.Create(const ADataset: TMongoDBQuery);
begin
  inherited Create(ADataset);
  FDoc := BSON;
end;

destructor TMongoResultSetAdapter.Destroy;
begin
  FDoc := nil;
  Dataset.Free;
  inherited Destroy;
end;

function TMongoResultSetAdapter.GetFieldCount: Integer;
begin
  if Assigned(FDoc) then
    Result := FDoc.GetFieldCount
  else
    Result := 0;
end;

function TMongoResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Assert(Assigned(FDoc), 'Document not assigned');

  Result := FDoc.GetItemKey(AIndex);
end;

function TMongoResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Assert(Assigned(FDoc), 'Document not assigned');
  Result := FDoc[AFieldname];
end;

function TMongoResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Assert(Assigned(FDoc), 'Document not assigned');
  Result := FDoc.GetItem(AIndex);
end;

function TMongoResultSetAdapter.IsEmpty: Boolean;
begin
  Result := not Dataset.Next(FDoc);
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
  LDoc := JsonToBson(FStmtText);
  {TODO -oLinas -cMongoDB : get table name, update selectors, etc.}
  case FStmtType of
    mstInsert: Statement.Owner.Insert(NAME_COLLECTION, LDoc);
    mstUpdate: Statement.Owner.Update(NAME_COLLECTION, LDoc, LDoc);
    mstDelete: Statement.Owner.Delete(NAME_COLLECTION, LDoc);
    mstSelect: Statement.Query(NAME_COLLECTION, LDoc);
  end;
end;

function TMongoStatementAdapter.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
var
  LQuery: TMongoDBQuery;
  LResultset: TMongoResultSetAdapter;
begin
  LQuery := TMongoDBQuery.Create(Statement.Owner);
  LResultset := TMongoResultSetAdapter.Create(LQuery);
  LResultset.Document := JsonToBson(FStmtText);
  LQuery.Query(NAME_COLLECTION, LResultset.Document);
  Result := LResultset;
end;

function TMongoStatementAdapter.GetStatementType(var AStatementText: string): TMongoStatementType;
var
  LIdentifier: string;
begin
  LIdentifier := AStatementText[1];
  if LIdentifier = 'I' then
    Result := mstInsert
  else if LIdentifier = 'U' then
    Result := mstUpdate
  else if LIdentifier = 'D' then
    Result := mstDelete
  else
    Result := mstSelect;

  AStatementText := Copy(AStatementText, 2, Length(AStatementText));
end;

procedure TMongoStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
begin
  inherited;

end;

procedure TMongoStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
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
begin
  LStatement := TMongoDBQuery.Create(Connection);
  Result := TMongoStatementAdapter.Create(LStatement);
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

procedure TMongoTransactionAdapter.Rollback;
begin
  //
end;

{ TMongoDBQuery }

constructor TMongoDBQuery.Create(AConnection: TMongoDBConnection);
begin
  inherited Create(AConnection);
  FConnection := AConnection;
end;

function TMongoDBQuery.GetConnection: TMongoDBConnection;
begin
  Result := FConnection;
end;

end.
