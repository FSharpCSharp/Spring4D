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

unit Spring.Persistence.Core.DatabaseManager;

{$I Spring.inc}

interface

uses
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.AbstractManager,
  Spring.Persistence.Core.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building database structure from annotated entities.
  ///	</summary>
  {$ENDREGION}
  TDatabaseManager = class(TAbstractManager)
  private
    FEntities: IList<TClass>;
  protected
    procedure BuildTables(AEntities: IList<TClass>); virtual;
    procedure BuildForeignKeys(AEntities: IList<TClass>); virtual;
    procedure BuildSequences(AEntities: IList<TClass>); virtual;
  public
    constructor Create(const AConnection: IDBConnection); override;

    procedure BuildDatabase;

    procedure RegisterEntity(AEntityClass: TClass);
    procedure ClearEntities;

    function EntityExists(AEntityClass: TClass): Boolean;
  end;

  EODBCException = class(Exception);

  TBaseODBC = class(TInterfacedObject, IODBC)
  private
    FHandle: THandle;
    SQLAllocEnv: function(var phenv: Pointer): SmallInt; stdcall;
    SQLAllocConnect: function(henv: Pointer; var phdbc: Pointer): Smallint; stdcall;
    SQLDataSourcesW: function(henv: Pointer; direction:word; szDSN: PWideChar; cbDSN: Word; var pbDSN: Word;
      szDescr: PWideChar; cbDescr: Word; var pbDescr: Word): Smallint; stdcall;
  protected
    function GetDatasources: TArray<string>; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Commands.Factory,
  Spring.Persistence.SQL.Commands.FKCreator,
  Spring.Persistence.SQL.Commands.SeqCreator,
  Spring.Persistence.SQL.Commands.TableCreator;

const
  DLL_ODBC_32 = 'ODBC32.DLL';
  DLL_ODBC_64 = 'ODBC32.DLL';

  SQL_ERROR = -1;
  SQL_SUCCESS = 0;
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

function GetTableCreateExecutor(AClass: TClass; AConnection: IDBConnection): TTableCreateExecutor;
begin
  Result := CommandFactory.GetCommand<TTableCreateExecutor>(AClass, AConnection);
end;

function GetFKCreateExecutor(AClass: TClass; AConnection: IDBConnection): TForeignKeyCreateExecutor;
begin
  Result := CommandFactory.GetCommand<TForeignKeyCreateExecutor>(AClass, AConnection);
end;

function GetSequenceCreateExecutor(AClass: TClass; AConnection: IDBConnection): TSequenceCreateExecutor;
begin
  Result := CommandFactory.GetCommand<TSequenceCreateExecutor>(AClass, AConnection);
end;

{ TDatabaseManager }

constructor TDatabaseManager.Create(const AConnection: IDBConnection);
begin
  inherited Create(AConnection);
  FEntities := TRttiExplorer.GetEntities;
end;

procedure TDatabaseManager.BuildDatabase;
var
  LTran: IDBTransaction;
begin
  if (FEntities.Count < 1) then
    Exit;

  LTran := Connection.BeginTransaction;

  BuildTables(FEntities);

  BuildForeignKeys(FEntities);

  BuildSequences(FEntities);

  LTran.Commit;
end;

procedure TDatabaseManager.BuildForeignKeys(AEntities: IList<TClass>);
var
  LFkCreator: TForeignKeyCreateExecutor;
  LEntityClass: TClass;
begin
  for LEntityClass in AEntities do
  begin
    LFkCreator := GetFKCreateExecutor(LEntityClass, Connection);
    try
      LFkCreator.CreateForeignKeys(LEntityClass);
    finally
      LFkCreator.Free;
    end;
  end;
end;

procedure TDatabaseManager.BuildSequences(AEntities: IList<TClass>);
var
  LSequenceCreator: TSequenceCreateExecutor;
  LEntityClass: TClass;
begin
  for LEntityClass in AEntities do
  begin
    LSequenceCreator := GetSequenceCreateExecutor(LEntityClass, Connection);
    try
      LSequenceCreator.CreateSequence(LEntityClass);
    finally
      LSequenceCreator.Free;
    end;
  end;
end;

procedure TDatabaseManager.BuildTables(AEntities: IList<TClass>);
var
  LTableCreator: TTableCreateExecutor;
  LEntityClass: TClass;
begin
  for LEntityClass in AEntities do
  begin
    LTableCreator := GetTableCreateExecutor(LEntityClass, Connection);
    try
      LTableCreator.CreateTables(LEntityClass);
    finally
      LTableCreator.Free;
    end;
  end;
end;

procedure TDatabaseManager.ClearEntities;
begin
  FEntities.Clear;
end;

function TDatabaseManager.EntityExists(AEntityClass: TClass): Boolean;
var
  LTableCreator: TTableCreateExecutor;
begin
  LTableCreator := GetTableCreateExecutor(AEntityClass, Connection);
  try
    Result := LTableCreator.TableExists(LTableCreator.Table.Name);
  finally
    LTableCreator.Free;
  end;
end;

procedure TDatabaseManager.RegisterEntity(AEntityClass: TClass);
begin
  FEntities.Add(AEntityClass);
end;

{ TBaseODBC }

constructor TBaseODBC.Create;
begin
  inherited Create;
  FHandle := 0;
  SQLAllocEnv := nil;
  SQLAllocConnect := nil;
  SQLDataSourcesW := nil;
  {$IFDEF MSWINDOWS}
  FHandle := LoadLibrary(PChar(DLL_ODBC_32));
  if FHandle <> 0 then
  begin
    SQLAllocEnv := GetProcAddress(FHandle, 'SQLAllocEnv');
    SQLAllocConnect := GetProcAddress(FHandle, 'SQLAllocConnect');
    SQLDataSourcesW := GetProcAddress(FHandle, 'SQLDataSourcesW');
  end;
  {$ENDIF}
end;

destructor TBaseODBC.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if FHandle <> 0 then
    FreeLibrary(FHandle);
  {$ENDIF}
  inherited Destroy;
end;

function TBaseODBC.GetDatasources: TArray<string>;
{$IFDEF MSWINDOWS}
var
  LHandle: Pointer;
  LConnection: Pointer;
  LDSN, LDescr: array[0..255] of WideChar;
  LcbDsn, LcbDescr: Word;
  LList: TStrings;
{$ENDIF}
begin
  SetLength(Result, 0);

  LList := TStringList.Create;
  try
    {$IFDEF MSWINDOWS}
    if not Assigned(SQLDataSourcesW) then
      Exit;

    if (SQLAllocEnv(LHandle) <> SQL_SUCCESS) then
      raise EODBCException.Create('Cannot allocate ODBC handle');

    if (SQLAllocConnect(LHandle,LConnection) <> SQL_SUCCESS) then
      raise EODBCException.Create('Cannot allocate ODBC connection');

    if SQLDataSourcesW(LHandle, SQL_FETCH_FIRST, LDSN, SizeOf(LDSN),
        LcbDsn, LDescr, SizeOf(LDescr), LcbDescr) = SQL_SUCCESS then
      LList.Add(StrPas(LDSN))
    else
      Exit;

    while SQLDataSourcesW(LHandle, SQL_FETCH_NEXT, LDSN, SizeOf(LDSN),
        LcbDsn, LDescr, SizeOf(LDescr), LcbDescr) = SQL_SUCCESS do
    begin
      LList.Add(StrPas(LDSN));
    end;

    Result := LList.ToStringArray;

    {$ENDIF}
  finally
    LList.Free;
  end;
end;

end.

