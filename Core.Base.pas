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
  Core.Interfaces, SQL.Params, SQL.Interfaces
  {$IFDEF USE_SPRING},Spring.Collections{$ENDIF}
  , Generics.Collections
  , SQL.Commands.Page
  ;

type
  TDriverResultSetAdapter<T> = class(TInterfacedObject, IDBResultset)
  private
    FDataset: T;
  protected
    function IsEmpty(): Boolean; virtual; abstract;
    function Next(): Boolean; virtual; abstract;
    function GetFieldValue(AIndex: Integer): Variant; overload; virtual; abstract;
    function GetFieldValue(const AFieldname: string): Variant; overload; virtual; abstract;
    function GetFieldCount(): Integer; virtual; abstract;
  public
    constructor Create(const ADataset: T); virtual;
    destructor Destroy; override;

    property Dataset: T read FDataset;
  end;

  TDriverConnectionAdapter<T> = class(TInterfacedObject, IDBConnection)
  private
    FConnection: T;
    FExecutionListeners: TList<TExecutionListenerProc>;
    function GetExecutionListeners: TList<TExecutionListenerProc>;
  protected
    procedure Connect(); virtual; abstract;
    procedure Disconnect(); virtual; abstract;
    function IsConnected(): Boolean; virtual; abstract;
    function CreateStatement(): IDBStatement; virtual; abstract;
    function BeginTransaction(): IDBTransaction; virtual; abstract;
    function GetDriverName(): string; virtual; abstract;
    procedure AddExecutionListener(const AListenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners();
    procedure NotifyExecutionListeners(const ACommand: string; const AParams: TObjectList<TDBParam>);
  public
    constructor Create(const AConnection: T); virtual;
    destructor Destroy; override;

    property Connection: T read FConnection;
    property ExecutionListeners: TList<TExecutionListenerProc> read GetExecutionListeners;
  end;

  TDriverStatementAdapter<T> = class(TInterfacedObject, IDBStatement)
  private
    FStmt: T;
  public
    constructor Create(const AStatement: T); virtual;
    destructor Destroy; override;
    procedure SetSQLCommand(const ACommandText: string); virtual; abstract;
    procedure SetParams(Params: TEnumerable<TDBParam>); overload; virtual; abstract;
    procedure SetParams(const AParams: array of const); overload;
    function Execute(): NativeUInt; virtual; abstract;
    function ExecuteQuery(): IDBResultSet; virtual; abstract;

    property Statement: T read FStmt;
  end;

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

  TDriverPageAdapter<T: class> = class(TInterfacedObject, IDBPage<T>)
  private
    FItems: {$IFDEF USE_SPRING} Spring.Collections.IList<T> {$ELSE}TObjectList<T> {$ENDIF};
    FPager: TPager;
  protected
    function GetCurrentPage(): Integer;
    function GetItemsPerPage(): Integer;
    function GetTotalPages(): Integer;
    function GetTotalItems(): Int64;
    function GetItems(): {$IFDEF USE_SPRING} Spring.Collections.IList<T> {$ELSE}TObjectList<T> {$ENDIF};

    property Items: {$IFDEF USE_SPRING} Spring.Collections.IList<T> {$ELSE}TObjectList<T> {$ENDIF} read GetItems;
  public
    constructor Create(const APager: TPager); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SQL.Register
  ,Math
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

procedure TDriverConnectionAdapter<T>.ClearExecutionListeners;
begin
  FExecutionListeners.Clear;
end;

constructor TDriverConnectionAdapter<T>.Create(const AConnection: T);
begin
  inherited Create;
  FConnection := AConnection;
  FExecutionListeners := TList<TExecutionListenerProc>.Create;
end;

destructor TDriverConnectionAdapter<T>.Destroy;
begin
  FExecutionListeners.Free;
  inherited Destroy;
end;

function TDriverConnectionAdapter<T>.GetExecutionListeners: TList<TExecutionListenerProc>;
begin
  Result := FExecutionListeners;
end;

procedure TDriverConnectionAdapter<T>.NotifyExecutionListeners(const ACommand: string; const AParams: TObjectList<TDBParam>);
var
  LExecProc: TExecutionListenerProc;
begin
  for LExecProc in FExecutionListeners do
  begin
    LExecProc(ACommand, AParams);
  end;
end;

{ TDriverStatementAdapter<T> }

constructor TDriverStatementAdapter<T>.Create(const AStatement: T);
begin
  inherited Create;
  FStmt := AStatement;
end;

destructor TDriverStatementAdapter<T>.Destroy;
begin
  inherited Destroy;
end;

procedure TDriverStatementAdapter<T>.SetParams(const AParams: array of const);
var
  LParams: TObjectList<TDBParam>;
begin
  if Length(AParams) > 0 then
  begin
    LParams := TObjectList<TDBParam>.Create;
    try
      ConvertParams(AParams, LParams);
      SetParams(LParams);
    finally
      LParams.Free;
    end;
  end;
end;

{ TDriverPageAdapter<T> }

constructor TDriverPageAdapter<T>.Create(const APager: TPager);
begin
  inherited Create;
  FPager := APager;
  {$IFDEF USE_SPRING}
  FItems := TCollections.CreateObjectList<T>(True);
  {$ELSE}
  FItems := TObjectList<T>.Create(True);
  {$ENDIF}
end;

destructor TDriverPageAdapter<T>.Destroy;
begin
  {$IFNDEF USE_SPRING}
  FItems.Free;
  {$ELSE}
  FItems := nil;
  {$ENDIF}
  FPager.Free;
  inherited Destroy;
end;

function TDriverPageAdapter<T>.GetCurrentPage: Integer;
begin
  Result := FPager.Page;
end;

function TDriverPageAdapter<T>.GetItems: {$IFDEF USE_SPRING} Spring.Collections.IList<T> {$ELSE}TObjectList<T> {$ENDIF};
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
  FGenerator := TSQLGeneratorRegister.GetGenerator(AConnection.GetDriverName);
end;

function TPager.GetLimit: Integer;
begin
  Result := ItemsPerPage;
end;

function TPager.GetOffset: Integer;
begin
  Result := Min( (Page * ItemsPerPage) - ItemsPerPage + 1, 1);
end;

end.
