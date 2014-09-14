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
unit Core.Session;

{$I sv.inc}
interface

uses
  Core.AbstractSession, Core.EntityMap, Core.Interfaces, Rtti, TypInfo
  ,Core.EntityCache
  ,Spring.Collections
  ,SQL.Params
  ,Mapping.Attributes;

const
  IID_GETIMPLEMENTOR: TGUID = '{4C12C697-6FE2-4263-A2D8-85034F0D0E01}';

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  The main runtime class between an application and ORM. This is the
  ///	  central API class abstracting the notion of a persistence service. The
  ///	  main function of the Session is to offer create, read and delete
  ///	  operations for instances of mapped entity classes. <c>Insert()</c>
  ///	  results in an <c>SQL INSERT</c>, <c>delete()</c> in an <c>SQL DELETE</c>
  ///	   and <c>update() </c>in an <c>SQL UPDATE</c>. Changes to persistent
  ///	  instances are detected at flush time and also result in an
  ///	  <c>SQL UPDATE</c>. <c>save()</c> results in either an <c>INSERT</c> or
  ///	  an <c>UPDATE</c>. It is not intended that implementors be threadsafe.
  ///	  Instead each thread/transaction should obtain its own instance from a
  ///	  SessionFactory.
  ///	</summary>
  {$ENDREGION}
  TSession = class(TAbstractSession)
  private
    FOldStateEntities: TEntityMap;
    FStartedTransaction: IDBTransaction;
  protected
    function GetPager(APage, AItemsInPage: Integer): TObject;
    function GetQueryCountSql(const ASql: string): string;
    function GetQueryCount(const ASql: string; const AParams: array of const): Int64; overload;
    function GetQueryCount(const ASql: string; AParams:IList<TDBParam>): Int64; overload;

    procedure AttachEntity(AEntity: TObject); override;
    procedure DetachEntity(AEntity: TObject); override;
  public
    constructor Create(AConnection: IDBConnection); override;
    destructor Destroy; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Starts a new List Session. ListSession monitors changes in the specified list and can commit or rollback these changes to the database
    ///	</summary>
    ///	<remarks>
    ///	  Can return newly started list transaction interface which controls how changes will be reflected in the database.
    ///	</remarks>
    {$ENDREGION}
    function BeginListSession<T: class, constructor>(AList: IList<T>): IListSession<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Starts a new transaction.
    ///	</summary>
    ///	<remarks>
    ///	  Can optionally return newly started transaction interface.
    ///	</remarks>
    {$ENDREGION}
    function BeginTransaction(): IDBTransaction;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Commits currently active transaction.
    ///	</summary>
    ///	<remarks>
    ///	  <para>
    ///	    In order for this to work, transaction at first must be started by
    ///	    calling BeginTransaction() and ReleaseCurrentTransaction() must not
    ///	    be called after this.
    ///	  </para>
    ///	  <para>
    ///	    After CommitTransaction() call there is no need to
    ///	    ReleaseCurrentTransaction because it is done automatically. 
    ///	  </para>
    ///	</remarks>
    {$ENDREGION}
    procedure CommitTransaction();

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Rollbacks currently active transaction.
    ///	</summary>
    ///	<remarks>
    ///	  <para>
    ///	    After the rollback is performed, all the changes are not reflected
    ///	    in session entity classes. They need to be reloaded manually if
    ///	    this is required.
    ///	  </para>
    ///	  <para>
    ///	    After RollbackTransaction() call there is no need to
    ///	    ReleaseCurrentTransaction because it is done automatically.
    ///	  </para>
    ///	</remarks>
    {$ENDREGION}
    procedure RollbackTransaction();
    procedure ReleaseCurrentTransaction();

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Create a new ICriteria&lt;T&gt; instance, for the given entity class,
    ///	  or a superclass of an entity class.
    ///	</summary>
    {$ENDREGION}
    function CreateCriteria<T: class, constructor>(): ICriteria<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Executes sql statement which does not return resultset.
    ///	</summary>
    {$ENDREGION}
    function Execute(const ASql: string; const AParams: array of const): NativeUInt;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  <para>
    ///	    Executes given sql statement and returns first column value. SQL
    ///	    statement should be like this:
    ///	  </para>
    ///	  <code lang="Delphi">
    ///	SELECT COUNT(*) FROM TABLE;</code>
    ///	</summary>
    {$ENDREGION}
    function ExecuteScalar<T>(const ASql: string; const AParams: array of const): T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Tries to retrieves first and only model from the sql statement. If
    ///	  not succeeds, returns false.
    ///	</summary>
    {$ENDREGION}
    function TryFirst<T: class, constructor>(const ASql: string; const AParams: array of const; out AValue: T): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves first and only model from the sql statement.  Raises an
    ///	  <c>exception</c> if model does not exist.
    ///	</summary>
    {$ENDREGION}
    function First<T: class, constructor>(const ASql: string; const AParams: array of const): T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves first and only model or the default value if model does not
    ///	  exist.
    ///	</summary>
    {$ENDREGION}
    function FirstOrDefault<T: class, constructor>(const ASql: string; const AParams: array of const): T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves only one entity model from the database. Raises an
    ///	  <c>exception</c> if model does not exist.
    ///	</summary>
    {$ENDREGION}
    function Single<T: class, constructor>(const ASql: string; const AParams: array of const): T; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves only one entity model from the database. Returns default
    ///	  value if model does not exist.
    ///	</summary>
    {$ENDREGION}
    function SingleOrDefault<T: class, constructor>(const ASql: string; const AParams: array of const): T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves multiple models from the sql statement.
    ///	</summary>
    {$ENDREGION}
    function GetList<T: class, constructor>(const ASql: string;
      const AParams: array of const): IList<T>; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves single model from the database based on its primary key
    ///	  value. If record not found, nil is returned.
    ///	</summary>
    {$ENDREGION}
    function FindOne<T: class, constructor>(const AID: TValue): T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves all models from PODO database table.
    ///	</summary>
    {$ENDREGION}
    function FindAll<T: class, constructor>(): IList<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Inserts model to the database .
    ///	</summary>
    {$ENDREGION}
    procedure Insert(AEntity: TObject);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Inserts models to the database.
    ///	</summary>
    {$ENDREGION}
    procedure InsertList<T: class, constructor>(ACollection: ICollection<T>);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Checks if given entity is newly created (does not exist in the
    ///	  database yet).
    ///	</summary>
    {$ENDREGION}
    function IsNew(AEntity: TObject): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Updates model in a database.
    ///	</summary>
    {$ENDREGION}
    procedure Update(AEntity: TObject);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Updates multiple models in a database.
    ///	</summary>
    {$ENDREGION}
    procedure UpdateList<T: class, constructor>(ACollection: ICollection<T>);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Removes model from the database.
    ///	</summary>
    {$ENDREGION}
    procedure Delete(AEntity: TObject);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Removes models from the database.
    ///	</summary>
    {$ENDREGION}
    procedure DeleteList<T: class, constructor>(ACollection: ICollection<T>);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Fetches data in pages. Pages are 1-indexed.
    ///	</summary>
    {$ENDREGION}
    function Page<T: class, constructor>(APage: Integer; AItemsPerPage: Integer): IDBPage<T>; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Fetches data in pages. You do not need to write custom sql for this,
    ///	  just use ordinary sql. All the work will be done for you.  Pages are
    ///	  1-indexed.
    ///	</summary>
    {$ENDREGION}
    function Page<T: class, constructor>(APage: Integer; AItemsPerPage: Integer;
      const ASql: string; const AParams: array of const): IDBPage<T>; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Fetches data in pages. You do not need to write custom sql for this,
    ///	  just use ordinary sql. All the work will be done for you. Pages are
    ///	  1-indexed.
    ///	</summary>
    {$ENDREGION}
    function Page<T: class, constructor>(APage: Integer; AItemsPerPage: Integer;
      const ASql: string; AParams: IList<TDBParam>): IDBPage<T>; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Saves the entity to the database. It will do update or the insert
    ///	  based on the entity state.
    ///	</summary>
    {$ENDREGION}
    procedure Save(AEntity: TObject);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Saves the entity and all entities it contains to the database. It
    ///	  will do update or the insert based on the entity state.
    ///	</summary>
    ///	<remarks>
    ///	  <para>
    ///	    Use with caution when inserting new entities containing identity
    ///	    primary keys. If both base (main) and sub entities are newly
    ///	    created then framework won't be able to resolve their relationships
    ///	    because their primary keys aren't known at save time.
    ///	  </para>
    ///	  <para>
    ///	    Works best when entities are updated.
    ///	  </para>
    ///	</remarks>
    {$ENDREGION}
    procedure SaveAll(AEntity: TObject);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Saves entities to the database. It will do update or the insert based
    ///	  on the entity state.
    ///	</summary>
    {$ENDREGION}
    procedure SaveList<T: class, constructor>(ACollection: ICollection<T>);

    property OldStateEntities: TEntityMap read FOldStateEntities;
  end;

implementation

{$REGION 'Implementation uses'}
  uses
    SQL.Commands.Insert
    ,SQL.Commands.Select
    ,SQL.Commands.Update
    ,SQL.Commands.Delete
    ,SQL.Commands.Page
    ,SQL.Register
    ,SQL.Interfaces
    ,Core.Exceptions
    ,SQL.Commands.Factory
    ,Mapping.RttiExplorer
    ,Core.Reflection
    ,Core.Utils
    ,Core.Base
    ,SysUtils
    ,Core.Consts
    ,Core.Criteria
    ,Core.ListSession
    ;
{$ENDREGION}

{ TSession }

procedure TSession.AttachEntity(AEntity: TObject);
begin
  FOldStateEntities.AddOrReplace(TRttiExplorer.Clone(AEntity));
end;

function TSession.BeginListSession<T>(AList: IList<T>): IListSession<T>;
begin
  Result := TListSession<T>.Create(Self, AList);
end;

function TSession.BeginTransaction: IDBTransaction;
begin
  Result := Connection.BeginTransaction;
  FStartedTransaction := Result;
end;

procedure TSession.CommitTransaction;
begin
  if not Assigned(FStartedTransaction) then
    raise EORMTransactionNotStarted.Create(EXCEPTION_CANNOT_COMMIT);

  FStartedTransaction.Commit;
  ReleaseCurrentTransaction();
end;

constructor TSession.Create(AConnection: IDBConnection);
begin
  inherited Create(AConnection);
  FOldStateEntities := TEntityMap.Create(True);
end;

function TSession.CreateCriteria<T>: ICriteria<T>;
begin
  Result := TCriteria<T>.Create(Self);
end;

procedure TSession.DetachEntity(AEntity: TObject);
begin
  FOldStateEntities.Remove(AEntity);
end;

procedure TSession.Delete(AEntity: TObject);
var
  LDeleter: TDeleteExecutor;
begin
  LDeleter := CommandFactory.GetCommand<TDeleteExecutor>(AEntity.ClassType, Connection);
  try
    LDeleter.Execute(AEntity);
  finally
    LDeleter.Free;
  end;

  DetachEntity(AEntity);
end;

procedure TSession.DeleteList<T>(ACollection: ICollection<T>);
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Delete(LEntity);
  end;
end;

destructor TSession.Destroy;
begin
  FOldStateEntities.Free;
  inherited Destroy;
end;

function TSession.Execute(const ASql: string; const AParams: array of const): NativeUInt;
var
  LStatement: IDBStatement;
begin
  LStatement := Connection.CreateStatement;
  LStatement.SetSQLCommand(ASql);
  if Length(AParams) > 0 then
    LStatement.SetParams(AParams);

  Result := LStatement.Execute;
end;

function TSession.ExecuteScalar<T>(const ASql: string; const AParams: array of const): T;
var
  LResults: IDBResultset;
  LVal: Variant;
  LValue, LConvertedValue: TValue;
  LMustFree: Boolean;
begin
  Result := System.Default(T);
  LResults := GetResultset(ASql, AParams);
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

function TSession.GetList<T>(const ASql: string; const AParams: array of const): IList<T>;
begin
  Result := TCollections.CreateList<T>(True);
  Fetch<T>(ASql, AParams, Result);
end;

function TSession.FindAll<T>: IList<T>;
var
  LEntityClass: TClass;
  LSelecter: TSelectExecutor;
  LResults: IDBResultset;
begin
  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
  begin
    //we are fetching from the same table - AEntity
    LEntityClass := T;
  end;

  LSelecter := GetSelector(LEntityClass) as TSelectExecutor;
  try
    LResults := LSelecter.SelectAll(nil, LEntityClass);
    Result := GetList<T>(LResults);
  finally
    LSelecter.Free;
  end;
end;

function TSession.FindOne<T>(const AID: TValue): T;
var
  LSelecter: TSelectExecutor;
  LEntityClass: TClass;
  LResults: IDBResultset;
begin
  Result := System.Default(T);
  if not TRttiExplorer.TryGetEntityClass(TypeInfo(T), LEntityClass) then
  begin
    //we are fetching from the same table - AEntity
    LEntityClass := T;
  end;

  LSelecter := GetSelector(LEntityClass) as TSelectExecutor;
  try
    LSelecter.ID := AID;
    LResults := LSelecter.Select(nil, LEntityClass);
    if not LResults.IsEmpty then
    begin
      Result := GetOne<T>(LResults, nil);
    end;
  finally
    LSelecter.Free;
  end;
end;

function TSession.First<T>(const ASql: string; const AParams: array of const): T;
begin
  if not TryFirst<T>(ASql, AParams, Result) then
    raise EORMRecordNotFoundException.Create(EXCEPTION_QUERY_NO_RECORDS);
end;

function TSession.FirstOrDefault<T>(const ASql: string; const AParams: array of const): T;
begin
  if not TryFirst<T>(ASql, AParams, Result) then
    Result := System.Default(T);
end;

function TSession.GetPager(APage, AItemsInPage: Integer): TObject;
var
  LPager: TPager;
begin
  Result := TPager.Create(Connection);
  LPager := TPager(Result);
  LPager.Page := APage;
  LPager.ItemsPerPage := AItemsInPage;
end;

function TSession.GetQueryCount(const ASql: string; const AParams: array of const): Int64;
var
  LSQL: string;
  LResults: IDBResultset;
begin
  Result := 0;
  LSQL := GetQueryCountSql(ASql);
  LResults := GetResultset(LSQL, AParams);
  if not LResults.IsEmpty then
  begin
    Result := LResults.GetFieldValue(0);
  end;
end;

function TSession.GetQueryCount(const ASql: string; AParams:IList<TDBParam>): Int64;
var
  LSQL: string;
  LResults: IDBResultset;
begin
  Result := 0;
  LSQL := GetQueryCountSql(ASql);
  LResults := GetResultset(LSQL, AParams);
  if not LResults.IsEmpty then
  begin
    Result := LResults.GetFieldValue(0);
  end;
end;

function TSession.GetQueryCountSql(const ASql: string): string;
var
  LGenerator: ISQLGenerator;
begin
  LGenerator := TSQLGeneratorRegister.GetGenerator(Connection.GetQueryLanguage);
  Result := LGenerator.GenerateGetQueryCount(ASql);
end;

procedure TSession.Insert(AEntity: TObject);
var
  LInserter: TInsertExecutor;
begin
  LInserter := CommandFactory.GetCommand<TInsertExecutor>(AEntity.ClassType, Connection);
  try
    LInserter.Execute(AEntity);

    SetLazyColumns(AEntity, TEntityCache.Get(AEntity.ClassType));
    AttachEntity(AEntity);
  finally
    LInserter.Free;
  end;
end;

procedure TSession.InsertList<T>(ACollection: ICollection<T>);
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Insert(LEntity);
  end;
end;

function TSession.IsNew(AEntity: TObject): Boolean;
begin
  Result := not FOldStateEntities.IsMapped(AEntity);
end;

function TSession.Page<T>(APage, AItemsPerPage: Integer): IDBPage<T>;
begin
  Result := CreateCriteria<T>.Page(APage, AItemsPerPage);
end;

function TSession.Page<T>(APage, AItemsPerPage: Integer; const ASql: string;
  AParams: IList<TDBParam>): IDBPage<T>;
var
  LPager: TPager;
  LSQL: string;
  LResultset: IDBResultset;
begin
  LPager := GetPager(APage, AItemsPerPage) as TPager;
  Result := TDriverPageAdapter<T>.Create(LPager);
  LPager.TotalItems := GetQueryCount(ASql, AParams);
  LSQL := LPager.BuildSQL(ASql);

  LResultset := GetResultset(LSQL, AParams);
  Fetch<T>(LResultset, Result.Items);
end;

function TSession.Page<T>(APage, AItemsPerPage: Integer; const ASql: string;
  const AParams: array of const): IDBPage<T>;
var
  LPager: TPager;
  LSQL: string;
begin
  LPager := GetPager(APage, AItemsPerPage) as TPager;
  Result := TDriverPageAdapter<T>.Create(LPager);
  LPager.TotalItems := GetQueryCount(ASql, AParams);
  LSQL := LPager.BuildSQL(ASql);

  Fetch<T>(LSQL, AParams, Result.Items);
end;

procedure TSession.ReleaseCurrentTransaction;
begin
  FStartedTransaction := nil;
end;

procedure TSession.RollbackTransaction;
begin
  if not Assigned(FStartedTransaction) then
    raise EORMTransactionNotStarted.Create(EXCEPTION_CANNOT_ROLLBACK);

  FStartedTransaction.Rollback();
  ReleaseCurrentTransaction();
end;

procedure TSession.Save(AEntity: TObject);
begin
  if IsNew(AEntity) then
    Insert(AEntity)
  else
    Update(AEntity);
end;

procedure TSession.SaveAll(AEntity: TObject);
var
  LRelations: IList<TObject>;
  i: Integer;
begin
  LRelations := TRttiExplorer.GetRelationsOf(AEntity);
  for i := 0 to LRelations.Count - 1 do
  begin
    SaveAll(LRelations[i]);
  end;
  Save(AEntity);
end;

procedure TSession.SaveList<T>(ACollection: ICollection<T>);
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Save(LEntity);
  end;
end;

function TSession.Single<T>(const ASql: string; const AParams: array of const): T;
begin
  Result := First<T>(ASql, AParams);
end;

function TSession.SingleOrDefault<T>(const ASql: string; const AParams: array of const): T;
begin
  Result := FirstOrDefault<T>(ASql, AParams);
end;

function TSession.TryFirst<T>(const ASql: string; const AParams: array of const; out AValue: T): Boolean;
var
  LResults: IDBResultset;
begin
  LResults := GetResultset(ASql, AParams);
  Result := not LResults.IsEmpty;
  if Result then
    AValue := GetOne<T>(LResults, nil);
end;

procedure TSession.Update(AEntity: TObject);
var
  LUpdater: TUpdateExecutor;
begin
  LUpdater := CommandFactory.GetCommand<TUpdateExecutor>(AEntity.ClassType, Connection);
  try
    LUpdater.EntityMap := FOldStateEntities;
    LUpdater.Execute(AEntity);

    SetLazyColumns(AEntity, TEntityCache.Get(AEntity.ClassType));
    AttachEntity(AEntity);
  finally
    LUpdater.Free;
  end;
end;

procedure TSession.UpdateList<T>(ACollection: ICollection<T>);
var
  LEntity: T;
begin
  for LEntity in ACollection do
  begin
    Update(LEntity);
  end;
end;

end.
