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

  TMongoResultSetAdapter = class(TDriverResultSetAdapter<TMongoWireQuery>)
  private
    FDoc: IBSONDocument;
  public
    constructor Create(const ADataset: TMongoWireQuery); override;
    destructor Destroy; override;

    function IsEmpty(): Boolean; override;
    function Next(): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  end;

  EMongoDBStatementAdapterException = Exception;

  TMongoStatementAdapter = class(TDriverStatementAdapter<TMongoWireQuery>)
  public
    constructor Create(const AStatement: TMongoWireQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetParams(Params: TEnumerable<TDBParam>); overload; override;
    function Execute(): NativeUInt; override;
    function ExecuteQuery(): IDBResultSet; override;
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
  ;

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

constructor TMongoResultSetAdapter.Create(const ADataset: TMongoWireQuery);
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
  Result := FDoc.GetFieldCount;
end;

function TMongoResultSetAdapter.GetFieldName(AIndex: Integer): string;
begin
  Result := FDoc.GetItemKey(AIndex);
end;

function TMongoResultSetAdapter.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FDoc[AFieldname];
end;

function TMongoResultSetAdapter.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := FDoc.GetItem(AIndex);
end;

function TMongoResultSetAdapter.IsEmpty: Boolean;
begin
  Result := Dataset.Next(FDoc);
end;

function TMongoResultSetAdapter.Next: Boolean;
begin
  Result := True;
end;

{ TMongoStatementAdapter }

constructor TMongoStatementAdapter.Create(const AStatement: TMongoWireQuery);
begin
  inherited Create(AStatement);

end;

destructor TMongoStatementAdapter.Destroy;
begin
  //Statement.Free;
  inherited Destroy;
end;

function TMongoStatementAdapter.Execute: NativeUInt;
begin
  {TODO -oLinas -cGeneral : update, delete or insert based on sql statement}
end;

function TMongoStatementAdapter.ExecuteQuery: IDBResultSet;
var
  LQuery: TMongoWireQuery;
begin
  LQuery := TMongoWireQuery.Create(Statement.Owner);
  Result := TMongoResultSetAdapter.Create(LQuery);
end;

procedure TMongoStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
begin
  inherited;

end;

procedure TMongoStatementAdapter.SetSQLCommand(const ACommandText: string);
begin
  inherited;

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
  LStatement: TMongoWireQuery;
begin
  LStatement := TMongoWireQuery.Create(Connection);
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
