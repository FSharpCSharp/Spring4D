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

unit Spring.Persistence.Core.Base;

interface

uses
  Spring.Collections,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands.Page,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params;

type
  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBResultSet" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverResultSetAdapter<T> = class(TInterfacedObject, IDBResultSet)
  private
    fDataSet: T;
  protected
    function IsEmpty: Boolean; virtual; abstract;
    function Next: Boolean; virtual; abstract;
    function FieldNameExists(const fieldName: string): Boolean; virtual; abstract;
    function GetFieldValue(index: Integer): Variant; overload; virtual; abstract;
    function GetFieldValue(const fieldname: string): Variant; overload; virtual; abstract;
    function GetFieldCount: Integer; virtual; abstract;
    function GetFieldName(index: Integer): string; virtual; abstract;
  public
    constructor Create(const dataSet: T); virtual;

    property Dataset: T read fDataSet;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBConnection" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverConnectionAdapter<T> = class(TInterfacedObject, IDBConnection)
  private
    fConnection: T;
    fListeners: IList<TExecutionListenerProc>;
    fQueryLanguage: TQueryLanguage;
    fAutoFreeConnection: Boolean;
    fTransationId: Integer;

    procedure SetQueryLanguage(queryLanguage: TQueryLanguage);
    function GetExecutionListeners: IList<TExecutionListenerProc>;
    function GetAutoFreeConnection: Boolean;
    procedure SetAutoFreeConnection(value: Boolean);
  protected
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    function IsConnected: Boolean; virtual; abstract;
    function CreateStatement: IDBStatement; virtual; abstract;
    function BeginTransaction: IDBTransaction; virtual;
    function GetDriverName: string; virtual; abstract;
    function GetTransactionName: string; virtual;
    function GenerateNewID: Integer; virtual;
    procedure AddExecutionListener(const listenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners;

    function GetQueryLanguage: TQueryLanguage; virtual;
    function TryResolveQueryLanguage(out queryLanguage: TQueryLanguage): Boolean; virtual;
  public
    constructor Create(const connection: T); virtual;
    destructor Destroy; override;

    property AutoFreeConnection: Boolean read GetAutoFreeConnection write SetAutoFreeConnection;
    property Connection: T read fConnection;
    property ExecutionListeners: IList<TExecutionListenerProc> read GetExecutionListeners;
    property QueryLanguage: TQueryLanguage read GetQueryLanguage write SetQueryLanguage;
    property TransactionId: Integer read fTransationId;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBStatement" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverStatementAdapter<T> = class(TInterfacedObject, IDBStatement)
  private
    fStatement: T;
    fListeners: IList<TExecutionListenerProc>;
    fParams: IList<TDBParam>;
    fSql: string;
    fQuery: Variant;
    fQueryMetadata: TQueryMetadata;
  protected
    procedure NotifyListeners; virtual;
  public
    constructor Create(const statement: T); virtual;
    procedure SetSQLCommand(const commandText: string); virtual;
    procedure SetQuery(const metadata: TQueryMetadata; const query: Variant); virtual;
    procedure SetParams(const params: IList<TDBParam>); overload; virtual;
    procedure SetParams(const params: array of const); overload;
    function Execute: NativeUInt; virtual;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet; virtual;

    function NativeQueryPresent: Boolean; virtual;

    property ExecutionListeners: IList<TExecutionListenerProc> read fListeners write fListeners;
    property Statement: T read fStatement;
    property Query: Variant read fQuery write fQuery;
    property QueryMetadata: TQueryMetadata read fQueryMetadata write fQueryMetadata;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBTransaction" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverTransactionAdapter<T> = class(TInterfacedObject, IDBTransaction)
  private
    fTransaction: T;
    fTransactionName: string;
    function GetTransactionName: string;
    procedure SetTransactionName(const value: string);
  protected
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
  public
    constructor Create(const transaction: T); virtual;
    destructor Destroy; override;

    property Transaction: T read fTransaction;
    property TransactionName: string read GetTransactionName write SetTransactionName;
  end;

  /// <summary>
  ///   Responsible for building paged queries.
  /// </summary>
  TPager = class
  private
    fConnection: IDBConnection;
    fCurrentPage: Integer;
    fPageSize: Integer;
    fTotalItems: Int64;
    fGenerator: ISQLGenerator;
    function GetLimit: Integer;
    function GetOffset: Integer;
  public
    constructor Create(connection: IDBConnection); virtual;

    function BuildSQL(const sql: string): string;

    property Connection: IDBConnection read fConnection;
    property Page: Integer read fCurrentPage write fCurrentPage;
    property ItemsPerPage: Integer read fPageSize write fPageSize;
    property TotalItems: Int64 read fTotalItems write fTotalItems;
    property Limit: Integer read GetLimit;
    property Offset: Integer read GetOffset;
  end;

  /// <summary>
  ///   Base <see cref="Spring.Persistence.Core.Interfaces|IDBPage&lt;T&gt;" />
  ///   adapter which descendents must override.
  /// </summary>
  TDriverPageAdapter<T: class> = class(TInterfacedObject, IDBPage<T>)
  private
    fItems: IList<T>;
    fPager: TPager;
  protected
    function GetCurrentPage: Integer;
    function GetItemsPerPage: Integer;
    function GetTotalPages: Integer;
    function GetTotalItems: Int64;
    function GetItems: IList<T>;

    property Items: IList<T> read GetItems;
  public
    constructor Create(const pager: TPager); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  SyncObjs,
  SysUtils,
  Variants,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Register;


{$REGION 'TDriverResultSetAdapter<T>'}

constructor TDriverResultSetAdapter<T>.Create(const dataSet: T);
begin
  inherited Create;
  fDataSet := dataSet;
end;

{$ENDREGION}


{$REGION 'TDriverConnectionAdapter<T>'}

procedure TDriverConnectionAdapter<T>.AddExecutionListener(
  const listenerProc: TExecutionListenerProc);
begin
  fListeners.Add(listenerProc);
end;

function TDriverConnectionAdapter<T>.BeginTransaction: IDBTransaction;
begin
  GenerateNewID;
end;

procedure TDriverConnectionAdapter<T>.ClearExecutionListeners;
begin
  fListeners.Clear;
end;

constructor TDriverConnectionAdapter<T>.Create(const connection: T);
begin
  inherited Create;
  fConnection := connection;
  fListeners := TCollections.CreateList<TExecutionListenerProc>;
  fQueryLanguage := qlAnsiSQL;
  fTransationId := 0;
  fAutoFreeConnection := False;
  TryResolveQueryLanguage(fQueryLanguage);
end;

destructor TDriverConnectionAdapter<T>.Destroy;
begin
  if AutoFreeConnection then
    TRttiExplorer.DestroyClass<T>(fConnection);
  inherited Destroy;
end;

function TDriverConnectionAdapter<T>.GenerateNewID: Integer;
begin
  TInterlocked.Increment(fTransationId);
  Result := fTransationId;
end;

function TDriverConnectionAdapter<T>.GetAutoFreeConnection: Boolean;
begin
  Result := fAutoFreeConnection;
end;

function TDriverConnectionAdapter<T>.GetExecutionListeners: IList<TExecutionListenerProc>;
begin
  Result := fListeners;
end;

function TDriverConnectionAdapter<T>.GetQueryLanguage: TQueryLanguage;
begin
  Result := fQueryLanguage;
end;

function TDriverConnectionAdapter<T>.GetTransactionName: string;
begin
  Result := 'T' + IntToStr(fTransationId);
end;

procedure TDriverConnectionAdapter<T>.SetAutoFreeConnection(value: Boolean);
begin
  fAutoFreeConnection := value;
end;

procedure TDriverConnectionAdapter<T>.SetQueryLanguage(queryLanguage: TQueryLanguage);
begin
  if fQueryLanguage <> queryLanguage then
    fQueryLanguage := queryLanguage;
end;

function TDriverConnectionAdapter<T>.TryResolveQueryLanguage(
  out queryLanguage: TQueryLanguage): Boolean;
var
  driverName: string;
begin
  Result := True;
  driverName := GetDriverName;

  if ContainsText(driverName, DRIVER_MSSQL) then
    queryLanguage := qlMSSQL
  else if ContainsText(driverName, DRIVER_SQLITE) then
    queryLanguage := qlSQLite
  else if ContainsText(driverName, DRIVER_ORACLE) then
    queryLanguage := qlOracle
  else if ContainsText(driverName, DRIVER_SYBASE_ASA) then
    queryLanguage := qlASA
  else if ContainsText(driverName, DRIVER_MYSQL) then
    queryLanguage := qlMySQL
  else if ContainsText(driverName, DRIVER_UIB) then
    queryLanguage := qlFirebird
  else if ContainsText(driverName, DRIVER_FIREBIRD) then
    queryLanguage := qlFirebird
  else if ContainsText(driverName, DRIVER_POSTGRESQL) then
    queryLanguage := qlPostgreSQL
  else if ContainsText(driverName, DRIVER_MONGODB) then
    queryLanguage := qlMongoDB
  else
  begin
    Result := False;
    queryLanguage := qlAnsiSQL;
  end;
end;

{$ENDREGION}


{$REGION 'TDriverStatementAdapter<T>'}

constructor TDriverStatementAdapter<T>.Create(const statement: T);
begin
  inherited Create;
  fStatement := statement;
  fQuery := Null;
end;

function TDriverStatementAdapter<T>.Execute: NativeUInt;
begin
  NotifyListeners;
end;

function TDriverStatementAdapter<T>.ExecuteQuery(serverSideCursor: Boolean): IDBResultSet;
begin
  NotifyListeners;
end;

function TDriverStatementAdapter<T>.NativeQueryPresent: Boolean;
begin
  Result := not VarIsNull(fQuery);
end;

procedure TDriverStatementAdapter<T>.NotifyListeners;
var
  listener: TExecutionListenerProc;
  params: IList<TDBParam>;
begin
  if Assigned(fListeners) and (fSql <> '') then
  begin
    params := fParams;
    if not Assigned(params) then
      params := TCollections.CreateObjectList<TDBParam>(True);

    for listener in fListeners do
      listener(fSql, params);
  end;
end;

procedure TDriverStatementAdapter<T>.SetParams(const params: IList<TDBParam>);
begin
  fParams := params;
end;

procedure TDriverStatementAdapter<T>.SetParams(const params: array of const);
var
  paramList: IList<TDBParam>;
begin
  if Length(params) > 0 then
  begin
    paramList := TCollections.CreateObjectList<TDBParam>;
    ConvertParams(params, paramList);
    SetParams(paramList);
  end;
end;

procedure TDriverStatementAdapter<T>.SetQuery(const metadata: TQueryMetadata;
  const query: Variant);
begin
  case TQueryMetadata.GetQueryType(query) of
    qtQueryText: SetSQLCommand(query);
    qtQueryEntity:
    begin
      fQuery := query;
      fQueryMetadata := metadata;
    end;
  end;
end;

procedure TDriverStatementAdapter<T>.SetSQLCommand(const commandText: string);
begin
  fSql := commandText;
end;

{$ENDREGION}


{$REGION 'TDriverPageAdapter<T>'}

constructor TDriverPageAdapter<T>.Create(const pager: TPager);
begin
  inherited Create;
  fPager := pager;
  fItems := TCollections.CreateObjectList<T>(True);
end;

destructor TDriverPageAdapter<T>.Destroy;
begin
  fPager.Free;
  inherited Destroy;
end;

function TDriverPageAdapter<T>.GetCurrentPage: Integer;
begin
  Result := fPager.Page;
end;

function TDriverPageAdapter<T>.GetItems: IList<T>;
begin
  Result := fItems;
end;

function TDriverPageAdapter<T>.GetItemsPerPage: Integer;
begin
  Result := fPager.ItemsPerPage;
end;

function TDriverPageAdapter<T>.GetTotalItems: Int64;
begin
  Result := fPager.TotalItems;
end;

function TDriverPageAdapter<T>.GetTotalPages: Integer;
begin
  Result := GetTotalItems div GetItemsPerPage;
  if (GetTotalItems mod GetItemsPerPage) <> 0 then
    Inc(Result);
end;

{$ENDREGION}


{$REGION 'TDriverTransactionAdapter<T>'}

constructor TDriverTransactionAdapter<T>.Create(const transaction: T);
begin
  inherited Create;
  fTransaction := transaction;
end;

destructor TDriverTransactionAdapter<T>.Destroy;
begin
  if InTransaction then
    Rollback;
  inherited Destroy;
end;

function TDriverTransactionAdapter<T>.GetTransactionName: string;
begin
  Result := fTransactionName;
end;

procedure TDriverTransactionAdapter<T>.SetTransactionName(const value: string);
begin
  fTransactionName := value;
end;

{$ENDREGION}


{$REGION 'TPager'}

function TPager.BuildSQL(const sql: string): string;
begin
  Result := fGenerator.GeneratePagedQuery(sql, Limit, Offset);
end;

constructor TPager.Create(connection: IDBConnection);
begin
  inherited Create;
  fConnection := connection;
  fGenerator := TSQLGeneratorRegister.GetGenerator(connection.GetQueryLanguage);
end;

function TPager.GetLimit: Integer;
begin
  Result := ItemsPerPage;
end;

function TPager.GetOffset: Integer;
begin
  if Page <= 1 then
    Result := 0
  else
    Result := (Page * ItemsPerPage) - ItemsPerPage;
end;

{$ENDREGION}


end.
