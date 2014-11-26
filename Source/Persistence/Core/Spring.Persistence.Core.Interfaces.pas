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

unit Spring.Persistence.Core.Interfaces;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Types,
  Variants;

type
  TDBDriverType = (dtSQLite = 0, {$IFDEF MSWINDOWS}dtADO, dtMSSQL, dtASA, dtOracle,{$ENDIF} dtDBX, dtUIB, dtZeos, dtMongo, dtFireDAC);

  TExecutionListenerProc = reference to procedure(const ACommand: string; const AParams: IList<TDBParam>);

  /// <summary>
  ///   Represents paged fetches.
  /// </summary>
  /// <remarks>
  ///   Pages are zero indexed.
  /// </remarks>
  IDBPage<T: class> = interface(IInvokable)
    ['{384357E2-A0B1-4EEE-9A22-2C01479D4148}']
    function GetCurrentPage: Integer;
    function GetItemsPerPage: Integer;
    function GetTotalPages: Integer;
    function GetTotalItems: Int64;
    function GetItems: IList<T>;

    property Items: IList<T> read GetItems;
  end;

  /// <summary>
  ///   Represents an ordering imposed upon a <see cref="Spring.Persistence.Core.Interfaces|ICriteria&lt;T&gt;" />
  ///    result set.
  /// </summary>
  IOrderBy = interface(IInvokable)
    ['{F0047369-10D6-4A4D-9BB8-FD5699936D5D}']
    function GetEntityClass: TClass;
    function GetPropertyName: string;
    function GetSortingDirection: TSortingDirection;
    procedure SetEntityClass(value: TClass);
    property EntityClass: TClass read GetEntityClass write SetEntityClass;
    property PropertyName: string read GetPropertyName;
    property SortingDirection: TSortingDirection read GetSortingDirection;
  end;

  /// <summary>
  ///   An object-oriented representation of a query criterion that may be used
  ///   as a restriction in a <see cref="Spring.Persistence.Core.Interfaces|ICriteria&lt;T&gt;" />
  ///    query. Built-in criterion types are provided by the <see cref="Spring.Persistence.Criteria.Restrictions|TRestrictions" />
  ///    factory class. This interface might be implemented by application
  ///   classes that define custom restriction criteria.
  /// </summary>
  ICriterion = interface(IInvokable)
    ['{E22DFB1C-0E0E-45F4-9740-9469164B4557}']
    function GetEntityClass: TClass;
    function GetMatchMode: TMatchMode;
    function GetWhereOperator: TWhereOperator;
    procedure SetEntityClass(value: TClass);
    function ToSqlString(const params: IList<TDBParam>; const command: TDMLCommand;
      const generator: ISQLGenerator; addToCommand: Boolean): string;
    property EntityClass: TClass read GetEntityClass write SetEntityClass;
    property MatchMode: TMatchMode read GetMatchMode;
    property WhereOperator: TWhereOperator read GetWhereOperator;
  end;

  /// <summary>
  ///   <see cref="Spring.Persistence.Core.Interfaces|ICriteria&lt;T&gt;" /> is
  ///   a simplified API for retrieving entities by composing <see cref="Spring.Persistence.Core.Interfaces|ICriterion" />
  ///    objects. This is a very convenient approach for functionality like
  ///   "search" screens where there is a variable number of conditions to be
  ///   placed upon the result set. The <see cref="Spring.Persistence.Core.Session|TSession" />
  ///    is a factory for <see cref="Spring.Persistence.Core.Interfaces|ICriteria&lt;T&gt;" />
  ///    . <see cref="Spring.Persistence.Core.Interfaces|ICriterion" />
  ///   instances are usually obtained via the factory methods on <see cref="Spring.Persistence.Criteria.Restrictions|TRestrictions" />
  ///    .
  /// </summary>
  /// <example>
  ///   <code lang="Delphi">var
  ///   cats: IList&lt;TCat&gt;;
  /// begin
  ///   cats := session.CreateCriteria&lt;TCat&gt;
  ///     .Add(TRestrictions.Like('name', 'Iz%'))
  ///     .Add(TRestrictions.Gt('weight', MIN_WEIGHT))
  ///     .OrderBy(TOrder.Asc('age')).List;</code>
  /// </example>
  ICriteria<T: class, constructor> = interface(IInvokable)
    ['{09428AF2-3A36-44DB-B0E7-8B7D7620ED1C}']

    /// <summary>
    ///   Add a restriction to constrain the results to be retrieved.
    /// </summary>
    function Add(const criterion: ICriterion): ICriteria<T>;

    /// <summary>
    ///   Add an ordering to the result set.
    /// </summary>
    function OrderBy(const orderBy: IOrderBy): ICriteria<T>;

    /// <summary>
    ///   Clear current Criteria.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Returns the count of the added <see cref="Spring.Persistence.Core.Interfaces|ICriterion" />
    ///    instances.
    /// </summary>
    function Count: Integer;

    /// <summary>
    ///   Returns the results.
    /// </summary>
    function ToList: IList<T>;

    /// <summary>
    ///   Returns the results in pages.
    /// </summary>
    /// <param name="page">
    ///   Page index. Zero (0) indexed
    /// </param>
    /// <param name="itemsPerPage">
    ///   Items include in on page.
    /// </param>
    function Page(page, itemsPerPage: Integer): IDBPage<T>;
  end;

  /// <summary>
  ///   A factory for property specific criterion and projection instances.
  /// </summary>
  /// <remarks>
  ///   For detailed methods documentation look in <see cref="Spring.Persistence.Criteria.Restrictions|TRestrictions" />
  ///   .
  /// </remarks>
  IProperty = interface(IInvokable)
    ['{2F58C81C-4817-43E7-BA3F-7570FE2A6823}']
    function Eq(const value: TValue): ICriterion;
    function NotEq(const value: TValue): ICriterion;
    function GEq(const value: TValue): ICriterion;
    function Gt(const value: TValue): ICriterion;
    function IsNull: ICriterion;
    function IsNotNull: ICriterion;
    function Like(const value: string; matchMode: TMatchMode = mmExact): ICriterion;
    function NotLike(const value: string; matchMode: TMatchMode = mmExact): ICriterion;
    function LEq(const value: TValue): ICriterion;
    function Lt(const value: TValue): ICriterion;
    function &InStr(const values: TArray<string>): ICriterion;
    function NotInStr(const values: TArray<string>): ICriterion;
    function &InInt(const values: TArray<Integer>): ICriterion;
    function NotInInt(const values: TArray<Integer>): ICriterion;
    function Between(const low, high: TValue): ICriterion;

    function EqProperty(const other: IProperty): ICriterion; overload;
    function EqProperty(const otherPropertyName: string): ICriterion; overload;
    function NeProperty(const other: IProperty): ICriterion; overload;
    function NeProperty(const otherPropertyName: string): ICriterion; overload;
    function GeProperty(const other: IProperty): ICriterion; overload;
    function GeProperty(const otherPropertyName: string): ICriterion; overload;
    function GtProperty(const other: IProperty): ICriterion; overload;
    function GtProperty(const otherPropertyName: string): ICriterion; overload;
    function LeProperty(const other: IProperty): ICriterion; overload;
    function LeProperty(const otherPropertyName: string): ICriterion; overload;
    function LtProperty(const other: IProperty): ICriterion; overload;
    function LtProperty(const otherPropertyName: string): ICriterion; overload;

    function GetEntityClass: TClass;
    function GetPropertyName: string;
    procedure SetEntityClass(value: TClass);
    procedure SetPropertyName(const value: string);

    function Asc: IOrderBy;
    function Desc: IOrderBy;

    property EntityClass: TClass read GetEntityClass write SetEntityClass;
    property PropertyName: string read GetPropertyName write SetPropertyName;
  end;

  /// <summary>
  ///   Represents the result set to fetch data from the database.
  /// </summary>
  IDBResultSet = interface(IInvokable)
    ['{4FA97CFB-4992-4DAA-BB2A-B5CAF84B6B47}']
    function IsEmpty: Boolean;
    function Next: Boolean;
    function FieldNameExists(const fieldName: string): Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldname: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
  end;

  TQueryType = (qtQueryText, qtQueryEntity);

  TQueryMetadata = record
  public
    QueryOperation: TDMLCommandType;
    TableName: string;
  public
    class function GetQueryType(const query: Variant): TQueryType; inline; static;
  end;

  /// <summary>
  ///   Represents the executable database statement.
  /// </summary>
  IDBStatement = interface(IInvokable)
    ['{DA905CAA-0FC2-4570-9788-1DC206600171}']
    procedure SetSQLCommand(const commandText: string);
    procedure SetQuery(const metadata: TQueryMetadata; const query: Variant);
    procedure SetParams(const params: IList<TDBParam>); overload;
    procedure SetParams(const params: array of const); overload;
    function Execute: NativeUInt;
    function ExecuteQuery(serverSideCursor: Boolean = True): IDBResultSet;
  end;

  /// <summary>
  ///   Represents the database transaction.
  /// </summary>
  /// <remarks>
  ///   If transaction was not committed, rollback will be performed when
  ///   interface goes out of scope.
  /// </remarks>
  IDBTransaction = interface(IInvokable)
    ['{AA35EE88-7271-4894-B6F0-06080C797BCF}']
    procedure Commit;
    procedure Rollback;
    function GetTransactionName: string;
    procedure SetTransactionName(const value: string);
    property TransactionName: string read GetTransactionName write SetTransactionName;
  end;

  /// <summary>
  ///   Represents the database connection.
  /// </summary>
  IDBConnection = interface(IInvokable)
    ['{256B8F14-7FF1-4442-A202-358B24756654}']
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function GetDriverName: string;
    function GetQueryLanguage: TQueryLanguage;
    procedure SetQueryLanguage(queryLanguage: TQueryLanguage);

    procedure AddExecutionListener(const listenerProc: TExecutionListenerProc);
    procedure ClearExecutionListeners;
    function GetExecutionListeners: IList<TExecutionListenerProc>;
    property ExecutionListeners: IList<TExecutionListenerProc> read GetExecutionListeners;
    function GetAutoFreeConnection: Boolean;
    procedure SetAutoFreeConnection(value: Boolean);
    property AutoFreeConnection: Boolean read GetAutoFreeConnection write SetAutoFreeConnection;
  end;

  /// <summary>
  ///   Represents interface for mapping rows of a ResultSet on a per-row basis.
  ///   Implementations of this interface perform the actual work of mapping each row to a result object.
  /// </summary>
  IRowMapper<T: class, constructor> = interface(IInvokable)
    ['{557EE245-3542-40EC-86B5-16B1A3EA902A}']
    /// <summary>
    /// Implementations must implement this method to map each row of data in the ResultSet.
    /// Params. resultSet: the ResultSet to map (pre-initialized for the current row)
    /// </summary>
    function MapRow(const resultSet: IDBResultSet): T;
  end;

  /// <summary>
  ///   Represents list session which can be used to sync changes in the list
  ///   with the database table. E.g. entities from the database can be fetched
  ///   by using a <see cref="Spring.Persistence.Core.Session|TSession" /> .
  ///   After List Session is started, changes can be made in the list (add new
  ///   entities, remove current entities, change entity properties, etc.). To
  ///   persist these changes <see cref="CommitListSession" /> must be called.
  ///   To clear changes history <see cref="RollbackListSession" /> must be
  ///   called.
  /// </summary>
  IListSession<T: class, constructor> = interface(IInvokable)
    ['{D3F67BC4-1F62-4C1F-8E1F-4CD3A414F79D}']
    procedure CommitListSession;
    procedure RollbackListSession;
  end;

  IRepository<T: class, constructor; TID> = interface(IInvokable)
    ['{849C6AB6-04F0-4C0F-B139-A08A3396525D}']
    /// <summary>
    ///   Executes sql statement which does not return resultset.
    /// </summary>
    function Execute(const query: string; const params: array of const): NativeUInt;

    /// <summary>
    ///   Retrieves multiple entities from the sql statement.
    /// </summary>
    function Query(const query: string; const params: array of const): IList<T>;
  end;

  ICrudRepository<T: class, constructor; TID> = interface(IRepository<T, TID>)
    ['{E490DD59-5466-4036-8CA5-852D8F7EF527}']
    function Count: Int64;

    /// <summary>
    ///   Retrieves single model from the database based on its primary key
    ///   value. If record not found, nil is returned.
    /// </summary>
    function FindOne(const id: TID): T;

    /// <summary>
    ///   Retrieves all models from PODO database table.
    /// </summary>
    function FindAll: IList<T>;

    /// <summary>
    ///   Checks if entity exists in the repository.
    /// </summary>
    function Exists(const id: TID): Boolean;

    /// <summary>
    ///   Inserts model to the database .
    /// </summary>
    procedure Insert(const entity: T); overload;

    /// <summary>
    ///   Inserts models to the database.
    /// </summary>
    procedure Insert(const entities: ICollection<T>); overload;

    /// <summary>
    ///   Saves the entity to the database. It will do update or the insert
    ///   based on the entity state.
    /// </summary>
    function Save(const entity: T): T; overload;

    /// <summary>
    ///   Saves entities to the database. It will do update or the insert based
    ///   on the entity state.
    /// </summary>
    function Save(const entities: ICollection<T>): ICollection<T>; overload;

    /// <summary>
    ///   Saves the entity and all entities it contains to the database. It
    ///   will do update or the insert based on the entity state.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     Use with caution when inserting new entities containing identity
    ///     primary keys. If both base (main) and sub entities are newly
    ///     created then framework won't be able to resolve their
    ///     relationships because their primary keys aren't known at save
    ///     time.
    ///   </para>
    ///   <para>
    ///     Works best when entities are updated.
    ///   </para>
    /// </remarks>
    procedure SaveCascade(const entity: T);

    /// <summary>
    ///   Removes model from the database.
    /// </summary>
    procedure Delete(const entity: T); overload;

    /// <summary>
    ///   Removes entities from the database.
    /// </summary>
    procedure Delete(const entities: ICollection<T>); overload;

    /// <summary>
    ///   Deletes all entities managed by the repository.
    /// </summary>
    procedure DeleteAll;
  end;

  IPagedRepository<T: class, constructor; TID> = interface(ICrudRepository<T, TID>)
    ['{46A40512-604A-4013-B0F0-693D81CAF5DF}']
    /// <summary>
    ///   Fetches data in pages. Pages are 1-indexed.
    /// </summary>
    function Page(page, itemsPerPage: Integer): IDBPage<T>;
  end;

  IEmbeddedEntity = interface(IInvokable)
    ['{49A1BF38-F54D-4888-8189-02CA34B01D04}']
    function IsObject: Boolean;
    function IsArray: Boolean;
    function GetValue(const fieldName: string): Variant;
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
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  ICollectionAdapter<T: class, constructor> = interface(IInvokable)
    ['{378C9FEF-BAE0-4C47-A21E-7A8A510C6757}']
    procedure Add(const entity: T);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: ICollectionEnumerator<T>;

    function IsAddSupported: Boolean;
  end;

implementation

{ TQueryMetadata }

class function TQueryMetadata.GetQueryType(const query: Variant): TQueryType;
begin
  case VarType(query) of
    varUString, varString, varStrArg, varOleStr: Result := qtQueryText
    else
      Result := qtQueryEntity;  
  end;
end;

end.
