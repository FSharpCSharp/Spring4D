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

unit Spring.Persistence.Adapters.MongoDB;

{$I Spring.inc}

interface

uses
  MongoDB, MongoBson, Spring.Persistence.Core.Interfaces, Spring.Persistence.Core.Base
  , Spring.Persistence.SQL.Params, SysUtils, Spring.Persistence.Mapping.Attributes, Spring.Collections;

type
  TPageInfo = record
    Limit: Integer;
    Offset: Integer;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents MongoDB connection.
  ///	</summary>
  {$ENDREGION}
  TMongoDBConnection = class(TMongoReplset)
  protected
    function GetConnected: Boolean; virtual;
    procedure SetConnected(const Value: Boolean); virtual;
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

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
    function FieldNameExists(const AFieldName: string): Boolean; override;

    property Document: IBSONDocument read FDoc write FDoc;
    property IsInjected: Boolean read FIsInjected write FIsInjected;
  end;

  EMongoDBStatementAdapterException = Exception;

  TMongoStatementType = (mstInsert, mstUpdate, mstDelete, mstSelect, mstSelectCount, mstSelectOrder, mtPage);

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
    function GetOrderFromStatementText: string; virtual;
  public
    constructor Create(const AStatement: TMongoDBQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); override;
    procedure SetQuery(const AMetadata: TQueryMetadata; AQuery: Variant); override;
    procedure SetParams(Params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; override;
    function GetQueryText: string;
    function GetFullCollectionName: string; virtual;
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
  ,Spring.Persistence.Core.ConnectionFactory
  ,Spring.Persistence.SQL.Generator.MongoDB
  ,Variants
  ,TypInfo
  ,Spring.Persistence.SQL.Commands
  ;

const
  NAME_COLLECTION = 'UnitTests.MongoAdapter';

var
  MONGO_STATEMENT_TYPES: array[TMongoStatementType] of string = ('I', 'U', 'D', 'S', 'count', 'SO','page');

{ TMongoDBConnection }

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
  Result := checkConnection;
end;

procedure TMongoDBConnection.SetConnected(const Value: Boolean);
var
  LIsConnected: Boolean;
begin
  LIsConnected := GetConnected;
  if Value and not LIsConnected then
  begin
    Connect;
  end
  else if LIsConnected and not Value then
  begin
    disconnect;
  end;
end;

{ TMongoResultSetAdapter }

constructor TMongoResultSetAdapter.Create(const ADataset: TMongoDBQuery);
begin
  inherited Create(ADataset);
  FDoc := nil;
end;

destructor TMongoResultSetAdapter.Destroy;
begin
  //FDoc := nil;
  Dataset.Free;
  inherited Destroy;
end;

function TMongoResultSetAdapter.FieldNameExists(const AFieldName: string): Boolean;
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
  LDoc, LResultDoc: IBSONDocument;
  LIntf: IInterface;
  LOk: Boolean;
  LValue: Variant;
begin
  inherited;
  Result := 0;
  LOk := False;

  if NativeQueryPresent then
  begin
    case QueryMetadata.QueryType of
      ctUpdateVersion:
      begin
        LIntf := Query;
        LDoc := LIntf as IBsonDocument;
        LResultDoc := Statement.Connection.findAndModify(GetFullCollectionName, LDoc
          , bsonEmpty, BSON(['$inc', BSON(['_version', 1])]));
        LValue := LResultDoc['value'];
        if not (VarIsNull(LValue)) then
          Result := 1;
        Exit;
      end;
    end;
  end;

  LDoc := JsonToBson(FStmtText);
  case FStmtType of
    mstInsert: LOk := Statement.Connection.insert(GetFullCollectionName, LDoc);
    mstUpdate: LOk := Statement.Connection.Update(GetFullCollectionName, bsonEmpty, LDoc);
    mstDelete: LOk := Statement.Connection.remove(GetFullCollectionName, LDoc);
    mstSelect: LOk := Statement.Connection.findOne(GetFullCollectionName, LDoc) <> nil;
    mstSelectCount: Exit(Trunc(Statement.Connection.Count(GetFullCollectionName, LDoc)));
  end;
  if LOk then
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

  if FStmtType = mstSelectOrder then
  begin
    LQuery.sort := JsonToBson(GetOrderFromStatementText);
  end;

  LResultset := TMongoResultSetAdapter.Create(LQuery);
  LResultset.Document := JsonToBson(FStmtText);
  case FStmtType of
    mstInsert:
    begin
      Statement.Connection.Insert(GetFullCollectionName, LResultset.Document);
      LResultset.IsInjected := True;
    end;
    mstSelect, mtPage, mstSelectOrder:
    begin
      LQuery.query := LResultset.Document;
      if Statement.Connection.find(GetFullCollectionName, LQuery) then
      begin
        LResultset.Document := nil;
      end;
    end;
    mstSelectCount:
    begin
      LCount := Statement.Connection.count(GetFullCollectionName, LResultset.Document);
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

function TMongoStatementAdapter.GetOrderFromStatementText: string;
var
  iPos, iLength: Integer;
begin
  //read length
  iPos := PosEx('_', FStmtText);
  if iPos < 0 then
    Exit('{}');
  iLength := StrToInt(Copy(FStmtText, 3, iPos - 3));
  Result := Copy(FStmtText, iPos + 1, iLength);
  iPos := PosEx(']', FStmtText);
  FStmtText := Copy(FStmtText, iPos + 1, Length(FStmtText));
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
  APageInfo.Offset := StrToInt( Copy(AStatement, iStart + 1, iEnd - iStart - 1) );
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
  else if StartsStr('SO', AStatementText) then
  begin
    LIdentifier := 'SO';
    Result := mstSelectOrder;
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

procedure TMongoStatementAdapter.SetParams(Params: IList<TDBParam>);
var
  LParam: TDBParam;
  LValue: string;
  LParamIndex: Integer;
begin
  inherited;
  if (FStmtText = '') then
    Exit;

  LParamIndex := 0;
  for LParam in Params do
  begin
    if (VarType(LParam.Value) = varUnknown) then
      Continue;
    LValue := VarToStrDef(LParam.Value, '');
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

    FStmtText := StringReplace(FStmtText, TMongoDBGenerator.GetParamName(LParamIndex), LValue, [rfReplaceAll]);
    Inc(LParamIndex);
  end;
end;

procedure TMongoStatementAdapter.SetQuery(const AMetadata: TQueryMetadata;
  AQuery: Variant);
begin
  inherited;
  FFullCollectionName := AMetadata.TableName;
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
  inherited Create;
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
  inherited Create;
  FConnection := AConnection;
end;

function TMongoDBQuery.GetConnection: TMongoDBConnection;
begin
  Result := FConnection;
end;

initialization
  TConnectionFactory.RegisterConnection<TMongoConnectionAdapter>(dtMongo);

end.
