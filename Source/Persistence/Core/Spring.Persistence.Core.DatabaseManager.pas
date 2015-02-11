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

unit Spring.Persistence.Core.DatabaseManager;

interface

uses
  SysUtils,
  Spring.Collections,
  Spring.Persistence.Core.AbstractManager,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.SQL.Commands.Abstract,
  Spring.Persistence.SQL.Commands.FKCreator,
  Spring.Persistence.SQL.Commands.SeqCreator,
  Spring.Persistence.SQL.Commands.TableCreator;

type
  /// <summary>
  ///   Responsible for building database structure from annotated entities.
  /// </summary>
  TDatabaseManager = class(TAbstractManager)
  private
    fEntities: IList<TClass>;
  protected
    function GetFKCreateExecutor(entityClass: TClass;
      const connection: IDBConnection): TForeignKeyCreateExecutor;
    function GetSequenceCreateExecutor(entityClass: TClass;
      const connection: IDBConnection): TSequenceCreateExecutor;
    function GetTableCreateExecutor(entityClass: TClass;
      const connection: IDBConnection): TTableCreateExecutor;
    procedure BuildTables(const entities: IList<TClass>); virtual;
    procedure BuildForeignKeys(const entities: IList<TClass>); virtual;
    procedure BuildSequences(const entities: IList<TClass>); virtual;
  public
    constructor Create(const connection: IDBConnection); override;

    procedure BuildDatabase;

    procedure RegisterEntity(entityClass: TClass);
    procedure ClearEntities;

    function EntityExists(entityClass: TClass): Boolean;
  end;

  EODBCException = class(Exception);

  TBaseODBC = class(TInterfacedObject, IODBC)
  private
    fHandle: THandle;
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
  Spring.Persistence.Mapping.RttiExplorer;

const
  DLL_ODBC_32 = 'ODBC32.DLL';
  DLL_ODBC_64 = 'ODBC32.DLL';

  SQL_ERROR = -1;
  SQL_SUCCESS = 0;
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;


{$REGION 'TDatabaseManager'}

constructor TDatabaseManager.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fEntities := TRttiExplorer.GetEntities;
end;

procedure TDatabaseManager.BuildDatabase;
var
  LTran: IDBTransaction;
begin
  if not fEntities.Any then
    Exit;

  LTran := Connection.BeginTransaction;

  BuildTables(fEntities);
  BuildForeignKeys(fEntities);
  BuildSequences(fEntities);

  LTran.Commit;
end;

procedure TDatabaseManager.BuildForeignKeys(const entities: IList<TClass>);
var
  LFkCreator: TForeignKeyCreateExecutor;
  LEntityClass: TClass;
begin
  for LEntityClass in entities do
  begin
    LFkCreator := GetFKCreateExecutor(LEntityClass, Connection);
    try
      LFkCreator.CreateForeignKeys(LEntityClass);
    finally
      LFkCreator.Free;
    end;
  end;
end;

procedure TDatabaseManager.BuildSequences(const entities: IList<TClass>);
var
  LSequenceCreator: TSequenceCreateExecutor;
  LEntityClass: TClass;
begin
  for LEntityClass in entities do
  begin
    LSequenceCreator := GetSequenceCreateExecutor(LEntityClass, Connection);
    try
      LSequenceCreator.CreateSequence(LEntityClass);
    finally
      LSequenceCreator.Free;
    end;
  end;
end;

procedure TDatabaseManager.BuildTables(const entities: IList<TClass>);
var
  LTableCreator: TTableCreateExecutor;
  LEntityClass: TClass;
begin
  for LEntityClass in entities do
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
  fEntities.Clear;
end;

function TDatabaseManager.EntityExists(entityClass: TClass): Boolean;
var
  LTableCreator: TTableCreateExecutor;
begin
  LTableCreator := GetTableCreateExecutor(entityClass, Connection);
  try
    Result := LTableCreator.TableExists(LTableCreator.Table.Name);
  finally
    LTableCreator.Free;
  end;
end;

function TDatabaseManager.GetTableCreateExecutor(entityClass: TClass;
  const connection: IDBConnection): TTableCreateExecutor;
begin
  Result := TTableCreateExecutor.Create(connection);
  Result.Build(entityClass);
end;

function TDatabaseManager.GetFKCreateExecutor(entityClass: TClass;
  const connection: IDBConnection): TForeignKeyCreateExecutor;
begin
  Result := TForeignKeyCreateExecutor.Create(connection);
  Result.Build(entityClass);
end;

function TDatabaseManager.GetSequenceCreateExecutor(entityClass: TClass;
  const connection: IDBConnection): TSequenceCreateExecutor;
begin
  Result := TSequenceCreateExecutor.Create(connection);
  Result.Build(entityClass);
end;

procedure TDatabaseManager.RegisterEntity(entityClass: TClass);
begin
  fEntities.Add(entityClass);
end;

{$ENDREGION}


{$REGION 'TBaseODBC'}

constructor TBaseODBC.Create;
begin
  inherited Create;
  fHandle := 0;
  SQLAllocEnv := nil;
  SQLAllocConnect := nil;
  SQLDataSourcesW := nil;
  {$IFDEF MSWINDOWS}
  fHandle := LoadLibrary(PChar(DLL_ODBC_32));
  if fHandle <> 0 then
  begin
    SQLAllocEnv := GetProcAddress(fHandle, 'SQLAllocEnv');
    SQLAllocConnect := GetProcAddress(fHandle, 'SQLAllocConnect');
    SQLDataSourcesW := GetProcAddress(fHandle, 'SQLDataSourcesW');
  end;
  {$ENDIF}
end;

destructor TBaseODBC.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if fHandle <> 0 then
    FreeLibrary(fHandle);
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

{$ENDREGION}


end.
