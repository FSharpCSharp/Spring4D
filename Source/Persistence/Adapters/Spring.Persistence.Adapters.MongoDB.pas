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

{$I Spring.inc}

unit Spring.Persistence.Adapters.MongoDB;

interface

uses
  MongoDB,
  MongoBson,
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params;

type
  TPageInfo = record
    Limit: Integer;
    Offset: Integer;
  end;

  EMongoDBStatementAdapterException = Exception;

  TMongoStatementType = (mstInsert, mstUpdate, mstDelete, mstSelect, mstSelectCount, mstSelectOrder, mtPage);

  /// <summary>
  ///   Represents MongoDB connection.
  /// </summary>
  TMongoDBConnection = class(TMongoReplset)
  protected
    function GetConnected: Boolean; virtual;
    procedure SetConnected(value: Boolean); virtual;
  public
    function GetCollectionFromFullName(const fullConnectionName: string): string; virtual;
  public
    property Connected: Boolean read GetConnected write SetConnected;
  end;

  /// <summary>
  ///   Represents MongoDB query.
  /// </summary>
  TMongoDBQuery = class(TMongoCursor)
  private
    fConnection: TMongoDBConnection;
    function GetConnection: TMongoDBConnection;
  public
    constructor Create(const connection: TMongoDBConnection); overload;

    property Connection: TMongoDBConnection read GetConnection;
  end;

  /// <summary>
  ///   Represents MongoDB resultset.
  /// </summary>
  TMongoResultSetAdapter = class(TDriverResultSetAdapter<TMongoDBQuery>)
  private
    fDoc: IBSONDocument;
    fIsInjected: Boolean;
  public
    destructor Destroy; override;

    function IsEmpty: Boolean; override;
    function Next: Boolean; override;
    function GetFieldValue(index: Integer): Variant; overload; override;
    function GetFieldValue(const fieldName: string): Variant; overload; override;
    function GetFieldCount: Integer; override;
    function GetFieldName(index: Integer): string; override;
    function FieldNameExists(const fieldName: string): Boolean; override;

    property Document: IBSONDocument read fDoc write fDoc;
    property IsInjected: Boolean read fIsInjected write fIsInjected;
  end;

  /// <summary>
  ///   Represents MongoDB statement.
  /// </summary>
  TMongoStatementAdapter = class(TDriverStatementAdapter<TMongoDBQuery>)
  private
    fStatementText: string;
    fStatementType: TMongoStatementType;
    FFullCollectionName: string;
  protected
    function GetStatementType(var statementText: string): TMongoStatementType; virtual;
    function GetStatementPageInfo(const statementText: string; out pageInfo: TPageInfo): string; virtual;
    function IsObjectId(const value: string): Boolean; virtual;
    function GetOrderFromStatementText: string; virtual;
  public
    constructor Create(const statement: TMongoDBQuery); override;
    destructor Destroy; override;
    procedure SetSQLCommand(const commandText: string); override;
    procedure SetQuery(const metadata: TQueryMetadata; const query: Variant); override;
    procedure SetParams(const params: IList<TDBParam>); overload; override;
    function Execute: NativeUInt; override;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; override;
    function GetQueryText: string;
    function GetFullCollectionName: string; virtual;
  end;

  /// <summary>
  ///   Represents MongoDB connection.
  /// </summary>
  TMongoConnectionAdapter = class(TDriverConnectionAdapter<TMongoDBConnection>, IDBConnection)
  public
    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected: Boolean; override;
    function CreateStatement: IDBStatement; override;
    function BeginTransaction: IDBTransaction; override;
    function GetDriverName: string; override;
  end;

  /// <summary>
  ///   Represents MongoDB transaction.
  /// </summary>
  TMongoTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    fConnection: TMongoDBConnection;
  protected
    function GetTransactionName: string;
    procedure SetTransactionName(const value: string);
  public
    constructor Create(const connection: TMongoDBConnection);

    procedure Commit;
    procedure Rollback;
  end;

implementation

uses
  StrUtils,
  TypInfo,
  Variants,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.MongoDB;

const
  NAME_COLLECTION = 'UnitTests.MongoAdapter';

var
  MONGO_STATEMENT_TYPES: array[TMongoStatementType] of string = ('I', 'U', 'D', 'S', 'count', 'SO', 'page');


{$REGION 'TMongoDBConnection'}

function TMongoDBConnection.GetCollectionFromFullName(const fullConnectionName: string): string;
var
  i: Integer;
begin
  Result := fullConnectionName;
  i := PosEx('.', Result);
  if i > 0 then
    Result := Copy(Result, i + 1, Length(Result));
end;

function TMongoDBConnection.GetConnected: Boolean;
begin
  Result := checkConnection;
end;

procedure TMongoDBConnection.SetConnected(value: Boolean);
var
  LIsConnected: Boolean;
begin
  LIsConnected := GetConnected;
  if value and not LIsConnected then
    Connect
  else if LIsConnected and not value then
    disconnect;
end;

{$ENDREGION}


{$REGION 'TMongoDBQuery'}

constructor TMongoDBQuery.Create(const connection: TMongoDBConnection);
begin
  inherited Create;
  fConnection := connection;
end;

function TMongoDBQuery.GetConnection: TMongoDBConnection;
begin
  Result := fConnection;
end;

{$ENDREGION}


{$REGION 'TMongoResultSetAdapter'}

destructor TMongoResultSetAdapter.Destroy;
begin
  Dataset.Free;
  inherited Destroy;
end;

function TMongoResultSetAdapter.FieldNameExists(const fieldName: string): Boolean;
var
  LIter: TBsonIterator;
begin
  Result := False;
  if Assigned(fDoc) then
  begin
    LIter := fDoc.find(fieldName);
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
  if Assigned(fDoc) then
  begin
    LIter := fDoc.iterator;
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

function TMongoResultSetAdapter.GetFieldName(index: Integer): string;
var
  LIter: TBsonIterator;
  i: Integer;
begin
  Assert(Assigned(fDoc), 'Document not assigned');
  LIter := fDoc.iterator;
  i := 0;
  while LIter.next do
  begin
    if index = i then
      Exit(LIter.key);
    Inc(i);
  end;
end;

function TMongoResultSetAdapter.GetFieldValue(const fieldName: string): Variant;
begin
  Assert(Assigned(fDoc), 'Document not assigned');
  Result := fDoc.value(fieldName);
end;

function TMongoResultSetAdapter.GetFieldValue(index: Integer): Variant;
begin
  Assert(Assigned(fDoc), 'Document not assigned');
  Result := fDoc.value( GetFieldname(index) );
end;

function TMongoResultSetAdapter.IsEmpty: Boolean;
begin
  Result := not IsInjected and not Dataset.next;
  if not Result and not IsInjected then
    fDoc := Dataset.value;
end;

function TMongoResultSetAdapter.Next: Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{$REGION 'TMongoStatementAdapter'}

constructor TMongoStatementAdapter.Create(const statement: TMongoDBQuery);
begin
  inherited Create(statement);
  fStatementText := '';
  fStatementType := mstSelect;
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

  LDoc := JsonToBson(fStatementText);
  case fStatementType of
    mstInsert: LOk := Statement.Connection.insert(GetFullCollectionName, LDoc);
    mstUpdate: LOk := Statement.Connection.Update(GetFullCollectionName, bsonEmpty, LDoc);
    mstDelete: LOk := Statement.Connection.remove(GetFullCollectionName, LDoc);
    mstSelect: LOk := Statement.Connection.findOne(GetFullCollectionName, LDoc) <> nil;
    mstSelectCount: Exit(Trunc(Statement.Connection.Count(GetFullCollectionName, LDoc)));
  end;
  if LOk then
    Result := 1;
end;

function TMongoStatementAdapter.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
var
  LQuery: TMongoDBQuery;
  LResultset: TMongoResultSetAdapter;
  LPageInfo: TPageInfo;
  LCount: Double;
begin
  inherited;
  LQuery := TMongoDBQuery.Create(Statement.Connection);

  if fStatementType = mtPage then
  begin
    fStatementText := GetStatementPageInfo(fStatementText, LPageInfo);
    LQuery.limit := LPageInfo.Limit;
    LQuery.skip := LPageInfo.Offset;
  end;

  if fStatementType = mstSelectOrder then
  begin
    LQuery.sort := JsonToBson(GetOrderFromStatementText);
  end;

  LResultset := TMongoResultSetAdapter.Create(LQuery);
  LResultset.Document := JsonToBson(fStatementText);
  case fStatementType of
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
  iPos := PosEx('_', fStatementText);
  if iPos < 0 then
    Exit('{}');
  iLength := StrToInt(Copy(fStatementText, 3, iPos - 3));
  Result := Copy(fStatementText, iPos + 1, iLength);
  iPos := PosEx(']', fStatementText);
  fStatementText := Copy(fStatementText, iPos + 1, Length(fStatementText));
end;

function TMongoStatementAdapter.GetQueryText: string;
begin
  Result := fStatementText;
end;

function TMongoStatementAdapter.GetStatementPageInfo(const statementText: string; out pageInfo: TPageInfo): string;
var
  iStart, iEnd: Integer;
begin
  //ex. page1_10_[Collection]{}
  iStart := PosEx('_', statementText);
  pageInfo.Limit := StrToInt(Copy(statementText, 5, iStart - 5) );
  iEnd := PosEx('_', statementText, iStart+1);
  pageInfo.Offset := StrToInt( Copy(statementText, iStart + 1, iEnd - iStart - 1) );
  iEnd := PosEx(']', statementText, iEnd);
  Result := Copy(statementText, iEnd+1, Length(statementText));
end;

function TMongoStatementAdapter.GetStatementType(var statementText: string): TMongoStatementType;
var
  LIdentifier: string;
  LStartIndex, LEndIndex: Integer;
begin
  statementText := Trim(statementText);
  if Length(statementText) = 0 then
    statementText := 'S';

  LStartIndex := PosEx('[', statementText);
  LEndIndex := PosEx(']', statementText);
  FFullCollectionName := Copy(statementText, LStartIndex+1, LEndIndex - LStartIndex-1);

  LIdentifier := statementText[1];
  if LIdentifier = 'I' then
    Result := mstInsert
  else if LIdentifier = 'U' then
    Result := mstUpdate
  else if LIdentifier = 'D' then
    Result := mstDelete
  else if (StartsStr('count', statementText)) then
  begin
    LIdentifier := 'count';
    Result := mstSelectCount;
  end
  else if (StartsStr('page', statementText)) then
  begin
    LIdentifier := 'page';
    Result := mtPage;
    Exit;
  end
  else if StartsStr('SO', statementText) then
  begin
    LIdentifier := 'SO';
    Result := mstSelectOrder;
    Exit;
  end
  else
    Result := mstSelect;

  statementText := Copy(statementText, Length(FFullCollectionName) + 2 + Length(LIdentifier) + 1, Length(statementText));
end;

function TMongoStatementAdapter.IsObjectId(const value: string): Boolean;
begin
  Result := StartsText('ObjectID("', value);
end;

procedure TMongoStatementAdapter.SetParams(const params: IList<TDBParam>);
var
  LParam: TDBParam;
  LValue: string;
  LParamIndex: Integer;
begin
  inherited;
  if (fStatementText = '') then
    Exit;

  LParamIndex := 0;
  for LParam in params do
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

    fStatementText := StringReplace(fStatementText, TMongoDBGenerator.GetParamName(LParamIndex), LValue, [rfReplaceAll]);
    Inc(LParamIndex);
  end;
end;

procedure TMongoStatementAdapter.SetQuery(const metadata: TQueryMetadata;
  const query: Variant);
begin
  inherited;
  FFullCollectionName := metadata.TableName;
end;

procedure TMongoStatementAdapter.SetSQLCommand(const commandText: string);
begin
  inherited;
  if commandText = '' then
    Exit;

  fStatementText := commandText;
  fStatementType := GetStatementType(fStatementText);
end;

{$ENDREGION}


{$REGION 'TMongoConnectionAdapter'}

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
  Result := Assigned(Connection) and Connection.Connected;
end;

{$ENDREGION}


{$REGION 'TMongoTransactionAdapter'}

constructor TMongoTransactionAdapter.Create(const connection: TMongoDBConnection);
begin
  inherited Create;
  fConnection := connection;
end;

procedure TMongoTransactionAdapter.Commit;
begin
end;

function TMongoTransactionAdapter.GetTransactionName: string;
begin
  Result := '';
end;

procedure TMongoTransactionAdapter.Rollback;
begin
end;

procedure TMongoTransactionAdapter.SetTransactionName(const value: string);
begin
end;

{$ENDREGION}


initialization
  TConnectionFactory.RegisterConnection<TMongoConnectionAdapter>(dtMongo);

end.
