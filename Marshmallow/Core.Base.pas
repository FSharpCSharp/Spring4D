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
unit Core.Base;

interface

{$I sv.inc}

uses
  Core.Interfaces, SQL.Params, SQL.Interfaces, Spring.Collections
  , SQL.Commands.Page
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base <c>IDBResultset</c> adapter which descendents must override.
  ///	</summary>
  {$ENDREGION}
  TDriverResultSetAdapter<T> = class(TInterfacedObject, IDBResultset)
  private
    FDataset: T;
  protected
    function IsEmpty(): Boolean; virtual; abstract;
    function Next(): Boolean; virtual; abstract;
    function FieldnameExists(const AFieldName: string): Boolean; virtual; abstract;
    function GetFieldValue(AIndex: Integer): Variant; overload; virtual; abstract;
    function GetFieldValue(const AFieldname: string): Variant; overload; virtual; abstract;
    function GetFieldCount(): Integer; virtual; abstract;
    function GetFieldName(AIndex: Integer): string; virtual; abstract;
  public
    constructor Create(const ADataset: T); virtual;
    destructor Destroy; override;

    property Dataset: T read FDataset;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base <c>IDBConnection</c> adapter which descendents must override.
  ///	</summary>
  {$ENDREGION}
  TDriverConnectionAdapter<T> = class(TInterfacedObject, IDBConnection)
  private
    FConnection: T;
    FExecutionListeners: IList<TExecutionListenerProc>;
    FQueryLanguage: TQueryLanguage;
    FAutoFreeConnection: Boolean;
    FTranID: Integer;

    procedure SetQueryLanguage(AQueryLanguage: TQueryLanguage);
    function GetExecutionListeners: IList<TExecutionListenerProc>;
    function GetAutoFreeConnection: Boolean;
    procedure SetAutoFreeConnection(const Value: Boolean);
  protected
    procedure Connect(); virtual; abstract;
    procedure Disconnect(); virtual; abstract;
    function IsConnected(): Boolean; virtual; abstract;
    function CreateStatement(): IDBStatement; virtual; abstract;
    function BeginTransaction(): IDBTransaction; virtual;
    function GetDriverName(): string; virtual; abstract;
    function GetTransactionName(): string; virtual;
    function GenerateNewID(): Integer; virtual;
    procedure AddExecutionListener(const AListenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners();

    function GetQueryLanguage(): TQueryLanguage; virtual;
    function TryResolveQueryLanguage(out AQueryLanguage: TQueryLanguage): Boolean; virtual;
  public
    constructor Create(const AConnection: T); virtual;
    destructor Destroy; override;

    property AutoFreeConnection: Boolean read GetAutoFreeConnection write SetAutoFreeConnection;
    property Connection: T read FConnection;
    property ExecutionListeners: IList<TExecutionListenerProc> read GetExecutionListeners;
    property QueryLanguage: TQueryLanguage read GetQueryLanguage write SetQueryLanguage;
    property TranId: Integer read FTranID;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base <c>IDBStatement</c> adapter which descendents must override.
  ///	</summary>
  {$ENDREGION}
  TDriverStatementAdapter<T> = class(TInterfacedObject, IDBStatement)
  private
    FStmt: T;
    FExecutionListeners: IList<TExecutionListenerProc>;
    FParams: IList<TDBParam>;
    FSQL: string;
    FQuery: Variant;
    FQueryMetadata: TQueryMetadata;
  protected
    procedure NotifyListeners(); virtual;
  public
    constructor Create(const AStatement: T); virtual;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); virtual;
    procedure SetQuery(const AMetadata: TQueryMetadata; AQuery: Variant); virtual;
    procedure SetParams(Params: IList<TDBParam>); overload; virtual;
    procedure SetParams(const AParams: array of const); overload;
    function Execute(): NativeUInt; virtual;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultSet; virtual;

    function NativeQueryPresent(): Boolean; virtual;

    property ExecutionListeners: IList<TExecutionListenerProc> read FExecutionListeners write FExecutionListeners;
    property Statement: T read FStmt;
    property Query: Variant read FQuery write FQuery;
    property QueryMetadata: TQueryMetadata read FQueryMetadata write FQueryMetadata;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base <c>IDBTransaction</c> adapter which descendents must override.
  ///	</summary>
  {$ENDREGION}
  TDriverTransactionAdapter<T> = class(TInterfacedObject, IDBTransaction)
  private
    FTransaction: T;
    FTransactionName: string;
    function GetTransactionName: string;
    procedure SetTransactionName(const Value: string);
  protected
    procedure Commit(); virtual; abstract;
    procedure Rollback(); virtual; abstract;
    function InTransaction(): Boolean; virtual; abstract;
  public
    constructor Create(const ATransaction: T); virtual;
    destructor Destroy; override;

    property Transaction: T read FTransaction;
    property TransactionName: string read GetTransactionName write SetTransactionName;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building paged queries.
  ///	</summary>
  {$ENDREGION}
  TPager = class
  private
    FConnection: IDBConnection;
    FCurrentPage: Integer;
    FPageSize: Integer;
    FTotalItems: Int64;
    FGenerator: ISQLGenerator;
    function GetLimit: Integer;
    function GetOffset: Integer;
  public
    constructor Create(AConnection: IDBConnection); virtual;

    function BuildSQL(const ASql: string): string;

    property Connection: IDBConnection read FConnection;
    property Page: Integer read FCurrentPage write FCurrentPage;
    property ItemsPerPage: Integer read FPageSize write FPageSize;
    property TotalItems: Int64 read FTotalItems write FTotalItems;
    property Limit: Integer read GetLimit;
    property Offset: Integer read GetOffset;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Base <c>IDBPage&lt;T&gt;</c> adapter which descendents must override.
  ///	</summary>
  {$ENDREGION}
  TDriverPageAdapter<T: class> = class(TInterfacedObject, IDBPage<T>)
  private
    FItems: IList<T>;
    FPager: TPager;
  protected
    function GetCurrentPage(): Integer;
    function GetItemsPerPage(): Integer;
    function GetTotalPages(): Integer;
    function GetTotalItems(): Int64;
    function GetItems(): IList<T>;

    property Items: IList<T> read GetItems;
  public
    constructor Create(const APager: TPager); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SQL.Register
  ,Mapping.RttiExplorer
  ,Math
  ,StrUtils
  ,Core.Consts
  ,SyncObjs
  ,SysUtils
  ,Variants
  ;

{ TDriverResultSetAdapter<T> }

constructor TDriverResultSetAdapter<T>.Create(const ADataset: T);
begin
  inherited Create;
  FDataset := ADataset;
end;

destructor TDriverResultSetAdapter<T>.Destroy;
begin
  inherited Destroy;
end;

{ TDriverConnectionAdapter<T> }

procedure TDriverConnectionAdapter<T>.AddExecutionListener(
  const AListenerProc: TExecutionListenerProc);
begin
  FExecutionListeners.Add(AListenerProc);
end;

function TDriverConnectionAdapter<T>.BeginTransaction: IDBTransaction;
begin
  GenerateNewID();
end;

procedure TDriverConnectionAdapter<T>.ClearExecutionListeners;
begin
  FExecutionListeners.Clear;
end;

constructor TDriverConnectionAdapter<T>.Create(const AConnection: T);
begin
  inherited Create;
  FConnection := AConnection;
  FExecutionListeners := TCollections.CreateList<TExecutionListenerProc>;
  FQueryLanguage := qlAnsiSQL;
  FTranID := 0;
  TryResolveQueryLanguage(FQueryLanguage);
end;

destructor TDriverConnectionAdapter<T>.Destroy;
begin
  if AutoFreeConnection then
    TRttiExplorer.DestroyClass<T>(FConnection);
  inherited Destroy;
end;

function TDriverConnectionAdapter<T>.GenerateNewID: Integer;
begin
  TInterlocked.Increment(FTranID);
  Result := FTranID;
end;

function TDriverConnectionAdapter<T>.GetAutoFreeConnection: Boolean;
begin
  Result := FAutoFreeConnection;
end;

function TDriverConnectionAdapter<T>.GetExecutionListeners: IList<TExecutionListenerProc>;
begin
  Result := FExecutionListeners;
end;

function TDriverConnectionAdapter<T>.GetQueryLanguage: TQueryLanguage;
begin
  Result := FQueryLanguage;
end;

function TDriverConnectionAdapter<T>.GetTransactionName: string;
begin
  Result := 'T' + IntToStr(FTranID);
end;

procedure TDriverConnectionAdapter<T>.SetAutoFreeConnection(const Value: Boolean);
begin
  FAutoFreeConnection := Value;
end;

procedure TDriverConnectionAdapter<T>.SetQueryLanguage(AQueryLanguage: TQueryLanguage);
begin
  if FQueryLanguage <> AQueryLanguage then
  begin
    FQueryLanguage := AQueryLanguage;
  end;
end;

function TDriverConnectionAdapter<T>.TryResolveQueryLanguage(
  out AQueryLanguage: TQueryLanguage): Boolean;
var
  sDriverName: string;
begin
  Result := True;
  sDriverName := GetDriverName;

  if ContainsText(sDriverName, DRIVER_MSSQL) then
    AQueryLanguage := qlMSSQL
  else if ContainsText(sDriverName, DRIVER_SQLITE) then
    AQueryLanguage := qlSQLite
  else if ContainsText(sDriverName, DRIVER_ORACLE) then
    AQueryLanguage := qlOracle
  else if ContainsText(sDriverName, DRIVER_SYBASE_ASA) then
    AQueryLanguage := qlASA
  else if ContainsText(sDriverName, DRIVER_MYSQL) then
    AQueryLanguage := qlMySQL
  else if ContainsText(sDriverName, DRIVER_UIB) then
    AQueryLanguage := qlFirebird
  else if ContainsText(sDriverName, DRIVER_FIREBIRD) then
    AQueryLanguage := qlFirebird
  else if ContainsText(sDriverName, DRIVER_POSTGRESQL) then
    AQueryLanguage := qlPostgreSQL
  else if ContainsText(sDriverName, DRIVER_MONGODB) then
    AQueryLanguage := qlMongoDB
  else
  begin
    Result := False;
    AQueryLanguage := qlAnsiSQL;
  end;
end;

{ TDriverStatementAdapter<T> }

constructor TDriverStatementAdapter<T>.Create(const AStatement: T);
begin
  inherited Create;
  FStmt := AStatement;
  FParams := nil;
  FExecutionListeners := nil;
  FQuery := Null;
end;

destructor TDriverStatementAdapter<T>.Destroy;
begin
  inherited Destroy;
end;

function TDriverStatementAdapter<T>.Execute: NativeUInt;
begin
  NotifyListeners();
end;

function TDriverStatementAdapter<T>.ExecuteQuery(AServerSideCursor: Boolean): IDBResultSet;
begin
  NotifyListeners();
end;

function TDriverStatementAdapter<T>.NativeQueryPresent: Boolean;
begin
  Result := not VarIsNull(FQuery);
end;

procedure TDriverStatementAdapter<T>.NotifyListeners;
var
  LListener: TExecutionListenerProc;
  LParams: IList<TDBParam>;
begin
  if Assigned(FExecutionListeners) and (FSQL <> '') then
  begin
    LParams := FParams;
    if not Assigned(LParams) then
    begin
      LParams := TCollections.CreateObjectList<TDBParam>(True);
    end;

    for LListener in FExecutionListeners do
    begin
      LListener(FSQL, LParams);
    end;
  end;
end;

procedure TDriverStatementAdapter<T>.SetParams(Params: IList<TDBParam>);
begin
  FParams := Params;
end;

procedure TDriverStatementAdapter<T>.SetParams(const AParams: array of const);
var
  LParams: IList<TDBParam>;
begin
  if Length(AParams) > 0 then
  begin
    LParams := TCollections.CreateObjectList<TDBParam>;
    ConvertParams(AParams, LParams);
    SetParams(LParams);
  end;
end;

procedure TDriverStatementAdapter<T>.SetQuery(const AMetadata: TQueryMetadata;
  AQuery: Variant);
begin
  FQuery := AQuery;
  FQueryMetadata := AMetadata;
  case VarType(AQuery) of
    varUString, varString: FSQL := AQuery;
  end;
end;

procedure TDriverStatementAdapter<T>.SetSQLCommand(const ACommandText: string);
begin
  FSQL := ACommandText;
end;

{ TDriverPageAdapter<T> }

constructor TDriverPageAdapter<T>.Create(const APager: TPager);
begin
  inherited Create;
  FPager := APager;
  FItems := TCollections.CreateObjectList<T>(True);
end;

destructor TDriverPageAdapter<T>.Destroy;
begin
  FPager.Free;
  inherited Destroy;
end;

function TDriverPageAdapter<T>.GetCurrentPage: Integer;
begin
  Result := FPager.Page;
end;

function TDriverPageAdapter<T>.GetItems: IList<T>;
begin
  Result := FItems;
end;

function TDriverPageAdapter<T>.GetItemsPerPage: Integer;
begin
  Result := FPager.ItemsPerPage;
end;

function TDriverPageAdapter<T>.GetTotalItems: Int64;
begin
  Result := FPager.TotalItems;
end;

function TDriverPageAdapter<T>.GetTotalPages: Integer;
begin
  Result := GetTotalItems div GetItemsPerPage;
  if (GetTotalItems mod GetItemsPerPage) <> 0 then
    Inc(Result);
end;

{ TPager }

function TPager.BuildSQL(const ASql: string): string;
begin
  Result := FGenerator.GeneratePagedQuery(ASql, Limit, Offset);
end;

constructor TPager.Create(AConnection: IDBConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FGenerator := TSQLGeneratorRegister.GetGenerator(AConnection.GetQueryLanguage);
end;

function TPager.GetLimit: Integer;
begin
  Result := ItemsPerPage;
end;

function TPager.GetOffset: Integer;
begin
  if (Page <= 1) then
    Result := 0
  else
    Result := (Page * ItemsPerPage) - ItemsPerPage;
end;

{ TDriverTransactionAdapter<T> }

constructor TDriverTransactionAdapter<T>.Create(const ATransaction: T);
begin
  inherited Create;
  FTransaction := ATransaction;
end;

destructor TDriverTransactionAdapter<T>.Destroy;
begin
  if InTransaction then
    Rollback();
  inherited Destroy;
end;

function TDriverTransactionAdapter<T>.GetTransactionName: string;
begin
  Result := FTransactionName;
end;

procedure TDriverTransactionAdapter<T>.SetTransactionName(const Value: string);
begin
  FTransactionName := Value;
end;

end.
