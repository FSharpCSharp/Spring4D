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

unit Spring.Persistence.Core.Session;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.AbstractSession,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.Core.EntityMap,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Params;

const
  IID_GETIMPLEMENTOR: TGUID = '{4C12C697-6FE2-4263-A2D8-85034F0D0E01}';

type
  /// <summary>
  ///   The main runtime class between an application and ORM. This is the
  ///   central API class abstracting the notion of a persistence service. The
  ///   main function of the Session is to offer create, read and delete
  ///   operations for instances of mapped entity classes. <see cref="Spring.Persistence.Core.Session|TSession.Insert(TObject)">
  ///   Insert</see> results in an SQL <c>insert</c>, <see cref="Spring.Persistence.Core.Session|TSession.Delete(TObject)">
  ///   Delete</see> in an SQL <c>delete</c> and <see cref="Spring.Persistence.Core.Session|TSession.Update(TObject)">
  ///   Update</see> in an SQL <c>update</c>. Changes to persistent instances
  ///   are detected at flush time and also result in an SQL <c>update</c>. <see cref="Spring.Persistence.Core.Session|TSession.Save(TObject)">
  ///   Save</see> results in either an <c>insert</c> or an <c>update</c>. It
  ///   is not intended that implementers be thread-safe. Instead each
  ///   thread/transaction should obtain its own instance from a
  ///   SessionFactory.
  /// </summary>
  TSession = class(TAbstractSession)
  private
    fOldStateEntities: TEntityMap;
    fStartedTransaction: IDBTransaction;
  protected
    function GetPager(page, itemsPerPage: Integer): TObject;
    function GetQueryCountSql(const sql: string): string;
    function GetQueryCount(const sql: string; const params: array of const): Int64; overload;
    function GetQueryCount(const sql: string; const params: IList<TDBParam>): Int64; overload;

    procedure AttachEntity(const entity: TObject); override;
    procedure DetachEntity(const entity: TObject); override;
  public
    constructor Create(const connection: IDBConnection); override;
    destructor Destroy; override;

    /// <summary>
    ///   Starts a new List Session. ListSession monitors changes in the
    ///   specified list and can commit or rollback these changes to the
    ///   database
    /// </summary>
    /// <remarks>
    ///   Can return newly started list transaction interface which controls
    ///   how changes will be reflected in the database.
    /// </remarks>
    function BeginListSession<T: class, constructor>(
      const list: IList<T>): IListSession<T>;

    /// <summary>
    ///   Starts a new transaction.
    /// </summary>
    /// <remarks>
    ///   Can optionally return newly started transaction interface.
    /// </remarks>
    function BeginTransaction: IDBTransaction;

    /// <summary>
    ///   Commits currently active transaction.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     In order for this to work, transaction at first must be started
    ///     by calling BeginTransaction and ReleaseCurrentTransaction must
    ///     not be called after this.
    ///   </para>
    ///   <para>
    ///     After CommitTransaction call there is no need to
    ///     ReleaseCurrentTransaction because it is done automatically. 
    ///   </para>
    /// </remarks>
    procedure CommitTransaction;

    /// <summary>
    ///   Rollbacks currently active transaction.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     After the rollback is performed, all the changes are not
    ///     reflected in session entity classes. They need to be reloaded
    ///     manually if this is required.
    ///   </para>
    ///   <para>
    ///     After RollbackTransaction call there is no need to
    ///     ReleaseCurrentTransaction because it is done automatically.
    ///   </para>
    /// </remarks>
    procedure RollbackTransaction;
    procedure ReleaseCurrentTransaction;

    /// <summary>
    ///   Create a new ICriteria&lt;T&gt; instance, for the given entity class,
    ///   or a superclass of an entity class.
    /// </summary>
    function CreateCriteria<T: class, constructor>: ICriteria<T>;

    /// <summary>
    ///   Executes sql statement which does not return resultset.
    /// </summary>
    function Execute(const sql: string; const params: array of const): NativeUInt;

    /// <summary>
    ///   <para>
    ///     Executes given sql statement and returns first column value. SQL
    ///     statement should be like this:
    ///   </para>
    ///   <code lang="Delphi">SELECT COUNT(*) FROM TABLE;</code>
    /// </summary>
    function ExecuteScalar<T>(const sql: string; const params: array of const): T;

    /// <summary>
    ///   Tries to retrieves first and only model from the sql statement. If
    ///   not succeeds, returns false.
    /// </summary>
    function TryFirst<T: class, constructor>(const sql: string;
      const params: array of const; out value: T): Boolean;

    /// <summary>
    ///   Retrieves first and only model from the sql statement.  Raises an <c>
    ///   exception</c> if model does not exist.
    /// </summary>
    function First<T: class, constructor>(const sql: string;
      const params: array of const): T;

    /// <summary>
    ///   Retrieves first and only model or the default value if model does not
    ///   exist.
    /// </summary>
    function FirstOrDefault<T: class, constructor>(const sql: string;
      const params: array of const): T;

    /// <summary>
    ///   Retrieves only one entity model from the database. Raises an <c>
    ///   exception</c> if model does not exist.
    /// </summary>
    function Single<T: class, constructor>(const sql: string;
      const params: array of const): T; overload;

    /// <summary>
    ///   Retrieves only one entity model from the database. Returns default
    ///   value if model does not exist.
    /// </summary>
    function SingleOrDefault<T: class, constructor>(const sql: string;
      const params: array of const): T;

    /// <summary>
    ///   Retrieves multiple models from the sql statement.
    /// </summary>
    function GetList<T: class, constructor>(const sql: string;
      const params: array of const): IList<T>; overload;

    function GetList<T: class, constructor>(const sql: string;
      const params: IList<TDBParam>): IList<T>; overload;

    /// <summary>
    ///   Retrieves single model from the database based on its primary key
    ///   value. If record not found, nil is returned.
    /// </summary>
    function FindOne<T: class, constructor>(const id: TValue): T;

    /// <summary>
    ///   Retrieves all models from PODO database table.
    /// </summary>
    function FindAll<T: class, constructor>: IList<T>;

    /// <summary>
    ///   Inserts model to the database .
    /// </summary>
    procedure Insert(const entity: TObject);

    /// <summary>
    ///   Inserts models to the database.
    /// </summary>
    procedure InsertList<T: class, constructor>(const entities: ICollection<T>);

    /// <summary>
    ///   Checks if given entity is newly created (does not exist in the
    ///   database yet).
    /// </summary>
    function IsNew(const entity: TObject): Boolean;

    /// <summary>
    ///   Updates model in a database.
    /// </summary>
    procedure Update(const entity: TObject);

    /// <summary>
    ///   Updates multiple models in a database.
    /// </summary>
    procedure UpdateList<T: class, constructor>(const entities: ICollection<T>);

    /// <summary>
    ///   Removes model from the database.
    /// </summary>
    procedure Delete(const entity: TObject);

    /// <summary>
    ///   Removes models from the database.
    /// </summary>
    procedure DeleteList<T: class, constructor>(const entities: ICollection<T>);

    /// <summary>
    ///   Fetches data in pages. Pages are 1-indexed.
    /// </summary>
    function Page<T: class, constructor>(
      page, itemsPerPage: Integer): IDBPage<T>; overload;

    /// <summary>
    ///   Fetches data in pages. You do not need to write custom sql for this,
    ///   just use ordinary sql. All the work will be done for you.  Pages are
    ///   1-indexed.
    /// </summary>
    function Page<T: class, constructor>(page, itemsPerPage: Integer;
      const sql: string; const params: array of const): IDBPage<T>; overload;

    /// <summary>
    ///   Fetches data in pages. You do not need to write custom sql for this,
    ///   just use ordinary sql. All the work will be done for you. Pages are
    ///   1-indexed.
    /// </summary>
    function Page<T: class, constructor>(page, itemsPerPage: Integer;
      const sql: string; const params: IList<TDBParam>): IDBPage<T>; overload;

    /// <summary>
    ///   Saves the entity to the database. It will do update or the insert
    ///   based on the entity state.
    /// </summary>
    procedure Save(const entity: TObject);

    /// <summary>
    ///   Saves the entity and all entities it contains to the database. It
    ///   will do update or the insert based on the entity state.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     Use with caution when inserting new entities containing identity
    ///     primary keys. If both base (main) and sub entities are newly
    ///     created then framework won't be able to resolve their
    ///     relationships because their primary keys aren't known at save
    ///     time.
    ///   </para>
    ///   <para>
    ///     Works best when entities are updated.
    ///   </para>
    /// </remarks>
    procedure SaveAll(const entity: TObject);

    /// <summary>
    ///   Saves entities to the database. It will do update or the insert based
    ///   on the entity state.
    /// </summary>
    procedure SaveList<T: class, constructor>(const entities: ICollection<T>);

    function GetLazyValueAsObject<T:class, constructor>(const id: TValue;
      const entity: TObject; const column: ColumnAttribute): T;
    procedure SetLazyValue<T>(var value: T; const id: TValue;
      const entity: TObject; const column: ColumnAttribute);

    property OldStateEntities: TEntityMap read fOldStateEntities;
  end;

implementation

uses
  TypInfo,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Consts,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.ListSession,
  Spring.Persistence.Core.Reflection,
  Spring.Persistence.Core.Utils,
  Spring.Persistence.Criteria,
  Spring.Persistence.Mapping.RttiExplorer,
  Spring.Persistence.SQL.Commands.Delete,
  Spring.Persistence.SQL.Commands.Insert,
  Spring.Persistence.SQL.Commands.Page,
  Spring.Persistence.SQL.Commands.Select,
  Spring.Persistence.SQL.Commands.Update,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Register;

{ TSession }

constructor TSession.Create(const connection: IDBConnection);
begin
  inherited Create(connection);
  fOldStateEntities := TEntityMap.Create(True);
end;

destructor TSession.Destroy;
begin
  fOldStateEntities.Free;
  inherited Destroy;
end;

procedure TSession.AttachEntity(const entity: TObject);
begin
  fOldStateEntities.AddOrReplace(TRttiExplorer.Clone(entity));
end;

function TSession.BeginListSession<T>(const list: IList<T>): IListSession<T>;
begin
  Result := TListSession<T>.Create(Self, list);
end;

function TSession.BeginTransaction: IDBTransaction;
begin
  Result := Connection.BeginTransaction;
  fStartedTransaction := Result;
end;

procedure TSession.CommitTransaction;
begin
  if not Assigned(fStartedTransaction) then
    raise EORMTransactionNotStarted.Create(EXCEPTION_CANNOT_COMMIT);

  fStartedTransaction.Commit;
  ReleaseCurrentTransaction;
end;

function TSession.CreateCriteria<T>: ICriteria<T>;
begin
  Result := TCriteria<T>.Create(Self);
end;

procedure TSession.DetachEntity(const entity: TObject);
begin
  fOldStateEntities.Remove(entity);
end;

procedure TSession.Delete(const entity: TObject);
var
  LDeleter: TDeleteExecutor;
begin
  LDeleter := GetDeleteCommandExecutor(entity.ClassType);
  try
    DoDelete(entity, LDeleter);
  finally
    LDeleter.Free;
  end;
end;

procedure TSession.DeleteList<T>(const entities: ICollection<T>);
var
  LEntity: T;
  LDeleter: TDeleteExecutor;
begin
  LDeleter := GetDeleteCommandExecutor(T);
  try
    for LEntity in entities do
    begin
      DoDelete(LEntity, LDeleter);
    end;
  finally
    LDeleter.Free;
  end;
end;

function TSession.Execute(const sql: string; const params: array of const): NativeUInt;
var
  LStatement: IDBStatement;
begin
  LStatement := Connection.CreateStatement;
  LStatement.SetSQLCommand(sql);
  if Length(params) > 0 then
    LStatement.SetParams(params);

  Result := LStatement.Execute;
end;

function TSession.ExecuteScalar<T>(const sql: string; const params: array of const): T;
var
  LResults: IDBResultSet;
  LVal: Variant;
  LValue, LConvertedValue: TValue;
  LMustFree: Boolean;
begin
  Result := System.Default(T);
  LResults := GetResultset(sql, params);
  if not LResults.IsEmpty then
  begin
    LVal := LResults.GetFieldValue(0);

    LValue := TUtils.FromVariant(LVal);
    if not LValue.TryConvert(TypeInfo(T), LConvertedValue, LMustFree) then
      raise EORMCannotConvertValue.CreateFmt(EXCEPTION_CANNOT_CONVERT_TYPE
        , [LValue.TypeInfo.Name, PTypeInfo(TypeInfo(T)).Name]);
    Result := LConvertedValue.AsType<T>;
  end;
end;

function TSession.GetList<T>(const sql: string; const params: array of const): IList<T>;
begin
  Result := TCollections.CreateObjectList<T>(True);
  FetchFromQueryText(sql, params, Result as IObjectList, TClass(T));
end;

function TSession.GetLazyValueAsObject<T>(const id: TValue;
  const entity: TObject; const column: ColumnAttribute): T;
var
  LResults: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit(System.Default(T));

  LResults := DoGetLazy(id, entity, column, TypeInfo(T));

  if TUtils.IsEnumerable(TypeInfo(T)) then
    Result := GetObjectList<T>(LResults)
  else
    Result := T(MapEntityFromResultsetRow(LResults, TClass(T), entity));  {TODO -oOwner -cGeneral : get one with arg entity is needed only for lazy loading}
end;

function TSession.GetList<T>(const sql: string;
  const params: IList<TDBParam>): IList<T>;
begin
  Result := TCollections.CreateObjectList<T>(True);
  FetchFromQueryText(sql, params, Result as IObjectList, TClass(T));
end;

function TSession.FindAll<T>: IList<T>;
var
  LEntityClass: TClass;
  LSelecter: TSelectExecutor;
  LResults: IDBResultSet;
begin
  LEntityClass := TRttiExplorer.GetEntityClass(TypeInfo(T));
  LSelecter := GetSelectCommandExecutor(LEntityClass);
  try
    LResults := LSelecter.SelectAll(LEntityClass);
    Result := TCollections.CreateObjectList<T>(True);
    MapFromResultsetToCollection(LResults, Result as IObjectList, T);
  finally
    LSelecter.Free;
  end;
end;

function TSession.FindOne<T>(const id: TValue): T;
var
  LSelecter: TSelectExecutor;
  LEntityClass: TClass;
  LResults: IDBResultSet;
begin
  Result := System.Default(T);
  LEntityClass := TRttiExplorer.GetEntityClass(TypeInfo(T));
  LSelecter := GetSelectByIdCommandExecutor(LEntityClass, id);
  try
    LResults := LSelecter.Select;
    if not LResults.IsEmpty then
    begin
      Result := T.Create;
      DoMapEntity(TObject(Result), LResults, nil);
    end;
  finally
    LSelecter.Free;
  end;
end;

function TSession.First<T>(const sql: string; const params: array of const): T;
begin
  if not TryFirst<T>(sql, params, Result) then
    raise EORMRecordNotFoundException.Create(EXCEPTION_QUERY_NO_RECORDS);
end;

function TSession.FirstOrDefault<T>(const sql: string; const params: array of const): T;
begin
  if not TryFirst<T>(sql, params, Result) then
    Result := System.Default(T);
end;

function TSession.GetPager(page, itemsPerPage: Integer): TObject;
var
  LPager: TPager;
begin
  Result := TPager.Create(Connection);
  LPager := TPager(Result);
  LPager.Page := page;
  LPager.ItemsPerPage := itemsPerPage;
end;

function TSession.GetQueryCount(const sql: string; const params: array of const): Int64;
var
  LSQL: string;
  LResults: IDBResultSet;
begin
  Result := 0;
  LSQL := GetQueryCountSql(sql);
  LResults := GetResultset(LSQL, params);
  if not LResults.IsEmpty then
  begin
    Result := LResults.GetFieldValue(0);
  end;
end;

function TSession.GetQueryCount(const sql: string; const params: IList<TDBParam>): Int64;
var
  LSQL: string;
  LResults: IDBResultSet;
begin
  Result := 0;
  LSQL := GetQueryCountSql(sql);
  LResults := GetResultset(LSQL, params);
  if not LResults.IsEmpty then
  begin
    Result := LResults.GetFieldValue(0);
  end;
end;

function TSession.GetQueryCountSql(const sql: string): string;
var
  LGenerator: ISQLGenerator;
begin
  LGenerator := TSQLGeneratorRegister.GetGenerator(Connection.GetQueryLanguage);
  Result := LGenerator.GenerateGetQueryCount(sql);
end;

procedure TSession.Insert(const entity: TObject);
var
  LInserter: TInsertExecutor;
begin
  LInserter := GetInsertCommandExecutor(entity.ClassType);
  try
    DoInsert(entity, LInserter);
  finally
    LInserter.Free;
  end;
end;

procedure TSession.InsertList<T>(const entities: ICollection<T>);
var
  LEntity: T;
  LInserter: TInsertExecutor;
begin
  LInserter := GetInsertCommandExecutor(T);
  try
    for LEntity in entities do
    begin
      DoInsert(LEntity, LInserter);
    end;
  finally
    LInserter.Free;
  end;
end;

function TSession.IsNew(const entity: TObject): Boolean;
begin
  Result := not fOldStateEntities.IsMapped(entity);
end;

function TSession.Page<T>(page, itemsPerPage: Integer): IDBPage<T>;
begin
  Result := CreateCriteria<T>.Page(page, itemsPerPage);
end;

function TSession.Page<T>(page, itemsPerPage: Integer; const sql: string;
  const params: IList<TDBParam>): IDBPage<T>;
var
  LPager: TPager;
  LSQL: string;
  LResultset: IDBResultSet;
begin
  LPager := GetPager(page, itemsPerPage) as TPager;
  Result := TDriverPageAdapter<T>.Create(LPager);
  LPager.TotalItems := GetQueryCount(sql, params);
  LSQL := LPager.BuildSQL(sql);

  LResultset := GetResultset(LSQL, params);
  MapFromResultsetToCollection(LResultset, Result.Items as IObjectList, T);
end;

function TSession.Page<T>(page, itemsPerPage: Integer; const sql: string;
  const params: array of const): IDBPage<T>;
var
  LPager: TPager;
  LSQL: string;
begin
  LPager := GetPager(page, itemsPerPage) as TPager;
  Result := TDriverPageAdapter<T>.Create(LPager);
  LPager.TotalItems := GetQueryCount(sql, params);
  LSQL := LPager.BuildSQL(sql);
  FetchFromQueryText(LSQL, params, Result.Items as IObjectList, TClass(T));
end;

procedure TSession.ReleaseCurrentTransaction;
begin
  fStartedTransaction := nil;
end;

procedure TSession.RollbackTransaction;
begin
  if not Assigned(fStartedTransaction) then
    raise EORMTransactionNotStarted.Create(EXCEPTION_CANNOT_ROLLBACK);

  fStartedTransaction.Rollback;
  ReleaseCurrentTransaction;
end;

procedure TSession.Save(const entity: TObject);
begin
  if IsNew(entity) then
    Insert(entity)
  else
    Update(entity);
end;

procedure TSession.SaveAll(const entity: TObject);
var
  LRelations: IList<TObject>;
  i: Integer;
begin
  LRelations := TRttiExplorer.GetRelationsOf(entity);
  for i := 0 to LRelations.Count - 1 do
  begin
    SaveAll(LRelations[i]);
  end;
  Save(entity);
end;

procedure TSession.SaveList<T>(const entities: ICollection<T>);
var
  LEntity: T;
  LInserts, LUpdates: IList<T>;
begin
  LInserts := TCollections.CreateList<T>;
  LUpdates := TCollections.CreateList<T>;
  for LEntity in entities do
  begin
    if IsNew(LEntity) then
      LInserts.Add(LEntity)
    else
      LUpdates.Add(LEntity);
  end;
  if not LInserts.IsEmpty then
    InsertList<T>(LInserts);
  if not LUpdates.IsEmpty then
    UpdateList<T>(LUpdates);
end;

procedure TSession.SetLazyValue<T>(var value: T; const id: TValue;
  const entity: TObject; const column: ColumnAttribute);
var
  LResults: IDBResultSet;
begin
  if not Assigned(entity) or id.IsEmpty then
    Exit;

  case PTypeInfo(TypeInfo(T)).Kind of
    tkClass, tkClassRef, tkPointer, tkRecord, tkUnknown:
      raise EORMUnsupportedType.CreateFmt(EXCEPTION_UNSUPPORTED_LAZY_TYPE, [
        PTypeInfo(TypeInfo(T)).TypeName]);
  end;

  LResults := DoGetLazy(id, entity, column, TypeInfo(T));

  if TUtils.IsEnumerable(TypeInfo(T)) then
    SetInterfaceList<T>(value, LResults)
  else
    SetOne<T>(value, LResults, entity);
end;

function TSession.Single<T>(const sql: string; const params: array of const): T;
begin
  Result := First<T>(sql, params);
end;

function TSession.SingleOrDefault<T>(const sql: string; const params: array of const): T;
begin
  Result := FirstOrDefault<T>(sql, params);
end;

function TSession.TryFirst<T>(const sql: string; const params: array of const; out value: T): Boolean;
var
  LResults: IDBResultSet;
begin
  LResults := GetResultset(sql, params);
  Result := not LResults.IsEmpty;
  if Result then
    value := T(MapEntityFromResultsetRow(LResults, TClass(T)));
end;

procedure TSession.Update(const entity: TObject);
var
  LUpdater: TUpdateExecutor;
begin
  LUpdater := GetUpdateCommandExecutor(entity.ClassType);
  try
    LUpdater.EntityMap := fOldStateEntities;
    DoUpdate(entity, LUpdater);
  finally
    LUpdater.Free;
  end;
end;

procedure TSession.UpdateList<T>(const entities: ICollection<T>);
var
  LEntity: T;
  LUpdater: TUpdateExecutor;
begin
  LUpdater := GetUpdateCommandExecutor(T);
  try
    LUpdater.EntityMap := fOldStateEntities;
    for LEntity in entities do
    begin
      DoUpdate(LEntity, LUpdater);
    end;
  finally
    LUpdater.Free;
  end;
end;

end.
