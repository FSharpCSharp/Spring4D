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
unit Core.Interfaces;

interface

{$I sv.inc}

uses
  SQL.Params
  ,Spring.Collections
  ,SQL.Interfaces
  ,SQL.Types
  ,SQL.Commands
  ,Rtti
  ;

type
  TDBDriverType = (dtSQLite = 0 {$IFDEF MSWINDOWS}, dtADO, dtMSSQL, dtASA, dtOracle{$ENDIF}, dtDBX, dtUIB, dtZeos, dtMongo);

  TExecutionListenerProc = reference to procedure(const ACommand: string; const AParams: IList<TDBParam>);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents paged fetches.
  ///	</summary>
  ///	<remarks>
  ///	  Pages are zero indexed.
  ///	</remarks>
  {$ENDREGION}
  IDBPage<T: class> = interface(IInvokable)
    ['{384357E2-A0B1-4EEE-9A22-2C01479D4148}']
    function GetCurrentPage(): Integer;
    function GetItemsPerPage(): Integer;
    function GetTotalPages(): Integer;
    function GetTotalItems(): Int64;
    function GetItems(): IList<T>;

    property Items: IList<T> read GetItems;
  end;


  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents an order imposed upon a <c>ICriteria&lt;T&gt;</c> result set.
  ///	</summary>
  {$ENDREGION}
  IOrder = interface(IInvokable)
    ['{F0047369-10D6-4A4D-9BB8-FD5699936D5D}']
    function GetPropertyName(): string;
    function GetOrderType(): TOrderType;
    function GetEntityClass(): TClass;
    procedure SetEntityClass(AClass: TClass);
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  An object-oriented representation of a query criterion that may be used
  ///	  as a restriction in a <c>ICriteria&lt;T&gt;</c> query. Built-in
  ///	  criterion types are provided by the <c>TRestrictions</c> factory class.
  ///	  This interface might be implemented by application classes that define
  ///	  custom restriction criteria.
  ///	</summary>
  {$ENDREGION}
  ICriterion = interface(IInvokable)
    ['{E22DFB1C-0E0E-45F4-9740-9469164B4557}']
    function ToSqlString(AParams: IList<TDBParam>; ACommand: TDMLCommand; AGenerator: ISQLGenerator; AAddToCommand: Boolean): string;
    procedure SetEntityClass(const Value: TClass);
    function GetEntityClass: TClass;
    function GetMatchMode(): TMatchMode;
    function GetWhereOperator(): TWhereOperator;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  <c>ICriteria&lt;T&gt;</c> is a simplified API for retrieving entities
  ///	  by composing <c>ICriterion</c> objects. This is a very convenient
  ///	  approach for functionality like "search" screens where there is a
  ///	  variable number of conditions to be placed upon the result set. The
  ///	  <c>TSession</c> is a factory for <c>ICriteria&lt;T&gt;</c>.
  ///	  <c>ICriterion</c> instances are usually obtained via the factory
  ///	  methods on <c>TRestrictions</c>.
  ///	</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	IList&lt;TCat&gt; cats = session.createCriteria&lt;TCat&gt;
  ///	.add( TRestrictions.Like('name', 'Iz%') )
  ///	.add( TRestrictions.Gt( 'weight', MIN_WEIGHT ) )
  ///	.addOrder( TOrder.Asc('age') ) .List();</code>
  ///	</example>
  {$ENDREGION}
  ICriteria<T: class, constructor> = interface(IInvokable)
    ['{09428AF2-3A36-44DB-B0E7-8B7D7620ED1C}']

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Add a
    ///	  <see cref="Core.Criteria.Restrictions|TRestrictions">restriction</see>
    ///	   to constrain the results to be retrieved.
    ///	</summary>
    {$ENDREGION}
    function Add(ACriterion: ICriterion): ICriteria<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Add an <see cref="Core.Criteria.Order|TOrder">ordering</see> to the
    ///	  result set.
    ///	</summary>
    {$ENDREGION}
    function AddOrder(AOrder: IOrder): ICriteria<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Clear current Criteria.
    ///	</summary>
    {$ENDREGION}
    procedure Clear();

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Get count of added <c>ICriterion</c>s.
    ///	</summary>
    {$ENDREGION}
    function Count(): Integer;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Get the results.
    ///	</summary>
    {$ENDREGION}
    function List(): IList<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  fetch the results into the collection.
    ///	</summary>
    {$ENDREGION}
    procedure Fetch(const ACollection: TValue);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Get the results in pages.
    ///	</summary>
    ///	<param name="APage">
    ///	  Page index. Zero (0) indexed
    ///	</param>
    ///	<param name="AItemsPerPage">
    ///	  Items include in on page.
    ///	</param>
    {$ENDREGION}
    function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<T>;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A factory for property-specific criterion and projection instances.
  ///	</summary>
  ///	<remarks>
  ///	  For detailed methods documentation look in
  ///	  <see cref="Core.Criteria.Restrictions|TRestrictions" />.
  ///	</remarks>
  {$ENDREGION}
  IProperty = interface(IInvokable)
    ['{2F58C81C-4817-43E7-BA3F-7570FE2A6823}']
    function Eq(const AValue: TValue): ICriterion;
    function NotEq(const AValue: TValue): ICriterion;
    function GEq(const AValue: TValue): ICriterion;
    function Gt(const AValue: TValue): ICriterion;
    function IsNull(): ICriterion;
    function IsNotNull(): ICriterion;
    function Like(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion;
    function NotLike(const AValue: string; AMatchMode: TMatchMode = mmExact): ICriterion;
    function LEq(const AValue: TValue): ICriterion;
    function Lt(const AValue: TValue): ICriterion;
    function &InStr(const AValues: TArray<string>): ICriterion;
    function NotInStr(const AValues: TArray<string>): ICriterion;
    function &InInt(const AValues: TArray<Integer>): ICriterion;
    function NotInInt(const AValues: TArray<Integer>): ICriterion;
    function Between(const ALow, AHigh: TValue): ICriterion;

    function EqProperty(AOther: IProperty): ICriterion; overload;
    function EqProperty(const AOtherPropertyName: string): ICriterion; overload;
    function NeProperty(AOther: IProperty): ICriterion; overload;
    function NeProperty(const AOtherPropertyName: string): ICriterion; overload;
    function GeProperty(AOther: IProperty): ICriterion; overload;
    function GeProperty(const AOtherPropertyName: string): ICriterion; overload;
    function GtProperty(AOther: IProperty): ICriterion; overload;
    function GtProperty(const AOtherPropertyName: string): ICriterion; overload;
    function LeProperty(AOther: IProperty): ICriterion; overload;
    function LeProperty(const AOtherPropertyName: string): ICriterion; overload;
    function LtProperty(AOther: IProperty): ICriterion; overload;
    function LtProperty(const AOtherPropertyName: string): ICriterion; overload;

    function GetPropertyName(): string;
    function GetEntityClass(): TClass;
    procedure SetEntityClass(AClass: TClass);
    procedure SetPropertyName(const Value: string);

    function Asc(): IOrder;
    function Desc(): IOrder;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents the result set to fetch data from the database.
  ///	</summary>
  {$ENDREGION}
  IDBResultset = interface(IInvokable)
    ['{4FA97CFB-4992-4DAA-BB2A-B5CAF84B6B47}']
    function IsEmpty(): Boolean;
    function Next(): Boolean;
    function FieldnameExists(const AFieldName: string): Boolean;
    function GetFieldValue(AIndex: Integer): Variant; overload;
    function GetFieldValue(const AFieldname: string): Variant; overload;
    function GetFieldCount(): Integer;
    function GetFieldName(AIndex: Integer): string;
  end;

  TQueryMetadata = record
    QueryType: TDMLCommandType;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents the executable database statement.
  ///	</summary>
  {$ENDREGION}
  IDBStatement = interface(IInvokable)
    ['{DA905CAA-0FC2-4570-9788-1DC206600171}']
    procedure SetSQLCommand(const ACommandText: string);
    procedure SetQuery(const AMetadata: TQueryMetadata; AQuery: Variant);
    procedure SetParams(AParams: IList<TDBParam>); overload;
    procedure SetParams(const AParams: array of const); overload;
    function Execute(): NativeUInt;
    function ExecuteQuery(AServerSideCursor: Boolean = True): IDBResultset;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents the database transaction.
  ///	</summary>
  ///	<remarks>
  ///	  If transaction was not committed, rollback will be performed when
  ///	  interface goes out of scope.
  ///	</remarks>
  {$ENDREGION}
  IDBTransaction = interface(IInvokable)
    ['{AA35EE88-7271-4894-B6F0-06080C797BCF}']
    procedure Commit();
    procedure Rollback();
    function GetTransactionName: string;
    procedure SetTransactionName(const Value: string);
    property TransactionName: string read GetTransactionName write SetTransactionName;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents the database connection.
  ///	</summary>
  {$ENDREGION}
  IDBConnection = interface(IInvokable)
    ['{256B8F14-7FF1-4442-A202-358B24756654}']
    procedure Connect();
    procedure Disconnect();
    function IsConnected(): Boolean;
    function CreateStatement(): IDBStatement;
    function BeginTransaction(): IDBTransaction;
    function GetDriverName(): string;
    function GetQueryLanguage(): TQueryLanguage;
    procedure SetQueryLanguage(AQuerLanguage: TQueryLanguage);

    procedure AddExecutionListener(const AListenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners();
    function GetExecutionListeners: IList<TExecutionListenerProc>;
    property ExecutionListeners: IList<TExecutionListenerProc> read GetExecutionListeners;
    function GetAutoFreeConnection: Boolean;
    procedure SetAutoFreeConnection(const Value: Boolean);
    property AutoFreeConnection: Boolean read GetAutoFreeConnection write SetAutoFreeConnection;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents list session which can be used to sync changes in the list
  ///	  with the database table. E.g. we could fetch some entities into the
  ///	  list from the database using <see cref="Core.Session|TSession" />.
  ///	  After List Session is started, we can make changes in the list (add new
  ///	  entities, remove current entities, change entity properties, etc.). If
  ///	  we want that these changes should also change database table we must
  ///	  call <see cref="CommitListSession" />. To clear changes history we must
  ///	  call <see cref="RollbackListSession" />.
  ///	</summary>
  {$ENDREGION}
  IListSession<T: class, constructor> = interface(IInvokable)
    ['{D3F67BC4-1F62-4C1F-8E1F-4CD3A414F79D}']
    procedure CommitListSession();
    procedure RollbackListSession();
  end;

  {$IFDEF USE_SPRING}
  IRepository<T: class, constructor; TID> = interface(IInvokable)
    ['{849C6AB6-04F0-4C0F-B139-A08A3396525D}']
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Executes sql statement which does not return resultset.
    ///	</summary>
    {$ENDREGION}
    function Execute(const AQuery: string; const AParams: array of const): NativeUInt;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves multiple models from the sql statement.
    ///	</summary>
    {$ENDREGION}
    function Query(const AQuery: string;
      const AParams: array of const): IList<T>;
  end;

  ICrudRepository<T: class, constructor; TID> = interface(IRepository<T, TID>)
    ['{E490DD59-5466-4036-8CA5-852D8F7EF527}']
    function Count(): Int64;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves single model from the database based on its primary key
    ///	  value. If record not found, nil is returned.
    ///	</summary>
    {$ENDREGION}
    function FindOne(const AID: TID): T;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves all models from PODO database table.
    ///	</summary>
    {$ENDREGION}
    function FindAll(): IList<T>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Inserts model to the database .
    ///	</summary>
    {$ENDREGION}
    procedure Insert(AEntity: T); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Inserts models to the database.
    ///	</summary>
    {$ENDREGION}
    procedure Insert(AEntities: ICollection<T>); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Saves the entity to the database. It will do update or the insert
    ///	  based on the entity state.
    ///	</summary>
    {$ENDREGION}
    function Save(AEntity: T): T; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Saves entities to the database. It will do update or the insert based
    ///	  on the entity state.
    ///	</summary>
    {$ENDREGION}
    function Save(AEntities: ICollection<T>): ICollection<T>; overload;

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
    procedure SaveCascade(AEntity: T);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Removes model from the database.
    ///	</summary>
    {$ENDREGION}
    procedure Delete(AEntity: T); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Removes entities from the database.
    ///	</summary>
    {$ENDREGION}
    procedure Delete(AEntities: ICollection<T>); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Deletes all entities managed by the repository.
    ///	</summary>
    {$ENDREGION}
    procedure DeleteAll();
  end;

  IPagedRepository<T: class, constructor; TID> = interface(ICrudRepository<T, TID>)
    ['{46A40512-604A-4013-B0F0-693D81CAF5DF}']
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Fetches data in pages. Pages are 1-indexed.
    ///	</summary>
    {$ENDREGION}
    function Page(APage: Integer; AItemsPerPage: Integer): IDBPage<T>;
  end;

  {$ENDIF}

  IEmbeddedEntity = interface(IInvokable)
    ['{49A1BF38-F54D-4888-8189-02CA34B01D04}']
    function IsObject(): Boolean;
    function IsArray(): Boolean;
    function GetValue(const AFieldName: string): Variant;
  end;

  IEntitySerializer = interface
    ['{BF7320F9-2B57-4B8C-997D-2F157D626D0D}']

  end;

  IODBC = interface
    ['{7A235A2E-1ABA-4AD6-A6FD-276A16374596}']
    function GetDatasources: TArray<string>;
  end;

  ICollectionEnumerator<T: class, constructor> = interface(IInvokable)
    ['{B1908786-00B2-454E-9D87-054A0A2CE8B3}']
    function GetCurrent: T;
    function MoveNext(): Boolean;
    property Current: T read GetCurrent;
  end;

  ICollectionAdapter<T: class, constructor> = interface(IInvokable)
    ['{378C9FEF-BAE0-4C47-A21E-7A8A510C6757}']
    procedure Add(AEntity: T);
    procedure Clear();
    function Count: Integer;
    function GetEnumerator(): ICollectionEnumerator<T>;

    function IsAddSupported(): Boolean;
  end;

implementation

uses
  RttiPatch;

end.
