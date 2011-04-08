{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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

/// <summary>
/// The Spring.Collections namespaces introduce the Collections Framework in
/// spring4d.
/// </summary>
/// <preliminary />
unit Spring.Collections;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  Spring;

type
  { Forward Declarations }
  IEnumerator<T> = interface;
  IEnumerable<T> = interface;
  ICollection<T> = interface;
  IList<T> = interface;
  IDictionary<TKey, TValue> = interface;
  IStack<T> = interface;
  IQueue<T> = interface;

  IEnumerator<T> = interface(IInterface)
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

  (*

  ISet<T>
  THashSet<T>
  TSortedSet<T>
  TSortedDictionary<TKey, TValue>

    Concat

    Any, All
    EqualsTo

    Skip, SkipWhile
    Take, TakeWhile

    Single, SingleOrDefault
    ElementAt, ElementAtOrDefault

    Min, Max

    Union, Intersect, Exclude
    Distinct

    OfType
    Select
    OrderBy, OrderByDescending
  *)

  ///	<summary>
  /// Provides limited LINQ-like enumerable extension methods for
  ///	<c>IEnumerable{T}</c>.
  /// </summary>
  IEnumerable<T> = interface(IInterface)
    /// <summary>
    /// Returns an enumerator that iterates through a collection.
    /// </summary>
    function GetEnumerator: IEnumerator<T>;

    ///	<summary>
    /// Returns the first element of a sequence.
    /// </summary>
    function First: T; overload;

    ///	<summary>
    /// Returns the first element in a sequence that satisfies a
    ///	specified condition.
    /// </summary>
    function First(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    /// Returns the first element of a sequence, or a default value if
    ///	the sequence contains no elements.
    /// </summary>
    function FirstOrDefault: T; overload;

    ///	<summary>
    /// Returns the first element of a sequence, or the specified defaultValue if
    ///	the sequence contains no elements.
    /// </summary>
    function FirstOrDefault(const defaultValue: T): T; overload;

    ///	<summary>Returns the first element of the sequence that satisfies a
    ///	condition or a default value if no such element is found.</summary>
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload;

    ///	<summary>Returns the last element of a sequence.</summary>
    function Last: T; overload;

    ///	<summary>Returns the last element of a sequence that satisfies a
    ///	specified condition.</summary>
    function Last(const predicate: TPredicate<T>): T; overload;

    ///	<summary>Returns the last element of a sequence, or a default value if
    ///	the sequence contains no elements.</summary>
    function LastOrDefault: T; overload;

    ///	<summary>Returns the last element of a sequence, or the specified
    ///	default value if the sequence contains no elements.</summary>
    function LastOrDefault(const defaultValue: T): T; overload;

    ///	<summary>Returns the last element of a sequence that satisfies a
    ///	condition or a default value if no such element is found.</summary>
    function LastOrDefault(const predicate: TPredicate<T>): T; overload;

    ///	<summary>Filters a sequence of values based on a predicate.</summary>
    function Where(const predicate: TPredicate<T>): IEnumerable<T>;

    ///	<summary>Determines whether a sequence contains a specified element by
    ///	using the default equality comparer.</summary>
    function Contains(const item: T): Boolean; overload;

    ///	<summary>Determines whether a sequence contains a specified element by
    ///	using a specified <c>IEqualityComparer{T}.</c></summary>
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    ///	<summary>Determines whether a sequence contains a specified element by
    ///	using the default equality comparer.</summary>
//    function ContainsRange(const collection: IEnumerable<T>): Boolean; overload;

    /// <summary>
    /// Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: TAction<T>); overload;

    /// <summary>
    /// Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: TActionProc<T>); overload;

    /// <summary>
    /// Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: TActionMethod<T>); overload;

    ///	<summary>Creates a new array which is filled with the elements in the
    ///	collection.</summary>
    function ToArray: TArray<T>;

    ///	<summary>
    /// Creates a new list which is filled with the elements in the collection.
    /// </summary>
    function ToList: IList<T>;

    function GetCount: Integer;
    function GetIsEmpty: Boolean;

    ///	<summary>Gets the number of elements in the collection.</summary>
    property Count: Integer read GetCount;

    ///	<summary>
    /// Gets a value which indicates whether this collection is empty or not.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  /// Defines methods to manipulate generic collections.
  /// </summary>
  ICollection<T> = interface(IEnumerable<T>)
  {$REGION 'Property Getters & Setters'}
    function GetIsReadOnly: Boolean;
  {$ENDREGION}
//    function Add(const item: T): Boolean;
    procedure Add(const item: T); overload;
    procedure AddRange(const collection: array of T); overload;
    procedure AddRange(const collection: IEnumerable<T>); overload;
    procedure AddRange(const collection: TEnumerable<T>); overload;

    function Remove(const item: T): Boolean;
    procedure RemoveRange(const collection: array of T); overload;
    procedure RemoveRange(const collection: IEnumerable<T>); overload;
    procedure RemoveRange(const collection: TEnumerable<T>); overload;

    procedure Clear;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  /// <summary>
  /// Represents a collection of objects that can be individually accessed by index.
  /// </summary>
  IList<T> = interface(ICollection<T>)
  {$REGION 'Property Getters & Setters'}
    function GetItem(index: Integer): T;
    procedure SetItem(index: Integer; const item: T);
  {$ENDREGION}
    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const collection: array of T); overload;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload;
    procedure InsertRange(index: Integer; const collection: TEnumerable<T>); overload;

    procedure RemoveAt(index: Integer); deprecated 'Use Delete(index) instead.';
    procedure Delete(index: Integer);
    procedure DeleteRange(startIndex, count: Integer);

//    procedure Exchange(index1, index2: Integer);
//    procedure Move(currentIndex, newIndex: Integer);

//    procedure Reverse;

//    procedure Sort; overload;
//    procedure Sort(const comparer: IComparer<T>); overload;
//    procedure Sort(const comparer: TComparison<T>); overload;
//    function Range(fromIndex, toIndex: Integer): IList<T>;
//    function AsReadOnly: IList<T>;

    function IndexOf(const item: T): Integer;
    function LastIndexOf(const item: T): Integer;
    property Items[index: Integer]: T read GetItem write SetItem; default;
  end;

  /// <summary>
  /// Represents a generic collection of key/value pairs.
  /// </summary>
  IDictionary<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>)
  {$REGION 'Property Getters & Setters'}
    function GetItem(const key: TKey): TValue;
    function GetKeys: ICollection<TKey>;
    function GetValues: ICollection<TValue>;
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    procedure Add(const key: TKey; const value: TValue); overload;
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    procedure Remove(const key: TKey); overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;

//    function AsReadOnly: IDictionary<TKey, TValue>;

    /// <summary>
    /// Gets a read-only collection which contains all keys in the dictionary.
    /// </summary>
    property Keys: ICollection<TKey> read GetKeys;

    /// <summary>
    /// Gets a read-only collection which contains all values in the dictionary.
    /// </summary>
    property Values: ICollection<TValue> read GetValues;
  end;

  IStack<T> = interface(IEnumerable<T>)
    procedure Clear;
    procedure Push(const item: T);
    function Pop: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;
  end;

  IQueue<T> = interface(IEnumerable<T>)
    procedure Clear;
    procedure Enqueue(const item: T);
    function Dequeue: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;
  end;

  ///	<summary>Internal interface. Reserved for future use.</summary>
  ICollectionItemType = interface
    ['{FE986DD7-41D5-4312-A2F9-94F7D9E642EE}']
    function GetItemType: PTypeInfo;
  end;

  ICollectionOwnership = interface
    ['{6D028EAF-3D14-4362-898C-BFAD1110547F}']

    {$REGION 'Property Getters & Setters'}
      function GetOwnsObjects: Boolean;
      procedure SetOwnsObjects(const value: Boolean);
    {$ENDREGION}
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TOwnershipType = (
    otReference,
    otOwned
  );

  TDictionaryOwnerships = Generics.Collections.TDictionaryOwnerships;


  {$REGION 'Documentation'}
  ///	<summary>Provides an abstract implementation for the <see cref=
  ///	"IEnumerator{T}">IEnumerator&lt;T&gt;</see> interface.</summary>
  {$ENDREGION}
  TEnumeratorBase<T> = class abstract(TInterfacedObject, IEnumerator<T>, IInterface)
  protected
    function GetCurrent: T; virtual; abstract;
  public
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// Provides a default implementation for <c>IEnumerable(T)</c> (Extension Methods).
  /// </summary>
  TEnumerableBase<T> = class abstract(TInterfacedObject, IEnumerable<T>, ICollectionItemType)
  protected

  {$REGION 'Implements ICollectionItemType'}
    function GetItemType: PTypeInfo;
  {$ENDREGION}
  protected
    function GetCount: Integer; virtual;
    function GetIsEmpty: Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; virtual;
    function TryGetLast(out value: T): Boolean; virtual;
  public
    function GetEnumerator: IEnumerator<T>; virtual; abstract;
    function First: T; overload; virtual;
    function First(const predicate: TPredicate<T>): T; overload; virtual;
    function FirstOrDefault: T; overload; virtual;
    function FirstOrDefault(const defaultValue: T): T; overload;
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function Last: T; overload; virtual;
    function Last(const predicate: TPredicate<T>): T; overload; virtual;
    function LastOrDefault: T; overload; virtual;
    function LastOrDefault(const defaultValue: T): T; overload;
    function LastOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function Where(const predicate: TPredicate<T>): IEnumerable<T>; virtual;
    function Contains(const item: T): Boolean; overload; virtual;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload; virtual;
    procedure ForEach(const action: TAction<T>); overload;
    procedure ForEach(const action: TActionProc<T>); overload;
    procedure ForEach(const action: TActionMethod<T>); overload;
    function ToArray: TArray<T>; virtual;
    function ToList: IList<T>; virtual;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TCollectionBase<T> = class abstract(TEnumerableBase<T>, ICollection<T>)
  protected
    function GetIsReadOnly: Boolean; virtual;
  public
    procedure Add(const item: T); virtual; abstract;
    procedure AddRange(const collection: array of T); overload;
    procedure AddRange(const collection: IEnumerable<T>); overload;
    procedure AddRange(const collection: TEnumerable<T>); overload;

    function  Remove(const item: T): Boolean; virtual; abstract;
    procedure RemoveRange(const collection: array of T); overload;
    procedure RemoveRange(const collection: IEnumerable<T>); overload;
    procedure RemoveRange(const collection: TEnumerable<T>); overload;

    procedure Clear; virtual; abstract;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TContainedCollectionBase<T> = class(TCollectionBase<T>)
  private
    fController: Pointer;
    function GetController: IInterface;
  protected

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}
  public
    constructor Create(const controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  TList<T> = class(TCollectionBase<T>, IList<T>, ISupportIndexedProperties)
  protected
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TList<T>;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TList<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fItems: array of T;
    fCount: Integer;
    fComparer: IComparer<T>;
    function GetItem(index: Integer): T;
    function GetCapacity: Integer;
    procedure SetItem(index: Integer; const value: T);
    procedure SetCapacity(value: Integer);
  protected
    function EnsureCapacity(value: Integer): Integer;

  {$REGION 'Implements ISupportIndexedProperties'}
    function GetPropertyValue(const propertyName: string; const index: Rtti.TValue): Rtti.TValue;
    procedure SetPropertyValue(const propertyName: string; const index, value: Rtti.TValue);
  {$ENDREGION}
  protected
    procedure DoDelete(index: Integer; notification: TCollectionNotification);
    procedure Notify(const item: T; action: TCollectionNotification); virtual;
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function TryGetFirst(out value: T): Boolean; override;
    function TryGetLast(out value: T): Boolean; override;
    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create; overload;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IComparer<T>); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function Contains(const item: T): Boolean; override;
    function ToArray: TArray<T>; override;
    function ToList: IList<T>; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;
    procedure Clear; override;
    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const collection: array of T); overload;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload;
    procedure InsertRange(index: Integer; const collection: TEnumerable<T>); overload;
    procedure Delete(index: Integer);
    procedure DeleteRange(startIndex, count: Integer);
    procedure RemoveAt(index: Integer);
    function Extract(const item: T): T;
    function IndexOf(const item: T): Integer;
    function LastIndexOf(const item: T): Integer;
    function Remove(const item: T): Boolean; override;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TDictionary<TKey, TValue> = class(TCollectionBase<TPair<TKey, TValue>>, IDictionary<TKey, TValue>)
  protected
    type
      TGenericDictionary = Generics.Collections.TDictionary<TKey, TValue>;
      TGenericObjectDictionary = Generics.Collections.TObjectDictionary<TKey, TValue>;
      TGenericPair = Generics.Collections.TPair<TKey,TValue>;

      ///	<summary>Provides a read-only ICollection{TKey}	implementation</summary>
      TKeyCollection = class(TContainedCollectionBase<TKey>, ICollection<TKey>)
      private
        fDictionary: TGenericDictionary;
      protected
        function GetCount: Integer; override;
        function GetIsEmpty: Boolean; override;
        function GetIsReadOnly: Boolean; override;
      public
        constructor Create(const controller: IInterface; dictionary: TGenericDictionary);
      {$REGION 'IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>; override;
        function Contains(const item: TKey): Boolean; override;
        function ToArray: TArray<TKey>; override;
      {$ENDREGION}

      {$REGION 'ICollection<TKey>'}
        procedure Add(const item: TKey); overload; override;
        procedure Clear; override;
        function Remove(const item: TKey): Boolean; overload; override;
        function Extract(const item: TKey): TKey;
      {$ENDREGION}
      end;

      ///	<summary>Provides a read-only ICollection{TValue} implementation</summary>
      TValueCollection = class(TContainedCollectionBase<TValue>, ICollection<TValue>)
      private
        fDictionary: TGenericDictionary;
      protected
        function GetCount: Integer; override;
        function GetIsEmpty: Boolean; override;
        function GetIsReadOnly: Boolean; override;
      public
        constructor Create(const controller: IInterface; dictionary: TGenericDictionary);
      {$REGION 'IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const item: TValue): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}

      {$REGION 'ICollection<TValue>'}
        procedure Add(const item: TValue); overload; override;
        procedure Clear; override;
        function Remove(const item: TValue): Boolean; overload; override;
        function Extract(const item: TValue): TValue;
        property IsReadOnly: Boolean read GetIsReadOnly;
      {$ENDREGION}
      end;
  private
    fDictionary: TGenericDictionary;
    fOwnership: TOwnershipType;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
  public
    constructor Create(dictionary: TGenericDictionary;
      ownership: TOwnershipType = otReference); overload;
    constructor Create; overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TPair<TKey,TValue>>; override;
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function Contains(const item: TPair<TKey,TValue>): Boolean; override;
    function ToArray: TArray<TPair<TKey,TValue>>; override;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Add(const item: TPair<TKey,TValue>); overload; override;
    procedure Clear; override;
    function Remove(const item: TPair<TKey,TValue>): Boolean; overload; override;
    function Extract(const item: TPair<TKey,TValue>): TPair<TKey,TValue>;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey,TValue>'}
    function GetItem(const key: TKey): TValue;
    function GetKeys: ICollection<TKey>;
    function GetValues: ICollection<TValue>;
    procedure SetItem(const key: TKey; const value: TValue);
    procedure Add(const key: TKey; const value: TValue); reintroduce; overload;
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    procedure Remove(const key: TKey); reintroduce; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    function ExtractPair(const key: TKey): TPair<TKey, TValue>;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
    property Keys: ICollection<TKey> read GetKeys;
    property Values: ICollection<TValue> read GetValues;
  {$ENDREGION}
  end;

  TStack<T> = class(TEnumerableBase<T>, IStack<T>)
  private
    type

      TGenericStack = Generics.Collections.TStack<T>;
//      TGenericObjectStack = Generics.Collections.TObjectStack<T>;

      TStackEnumerator = class(TEnumeratorBase<T>)
      private
        fStack: TGenericStack;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(stack: TGenericStack);
        function MoveNext: Boolean; override;
      end;
  private
    fStack: TGenericStack;
    fOwnership: TOwnershipType;
  protected
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
//    function TryGetFirst(out value: T): Boolean; override;
//    function TryGetLast(out value: T): Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    constructor Create(stack: TGenericStack; ownership: TOwnershipType); overload;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<T>; override;
    procedure Push(const item: T);
    function Pop: T;
    function Peek: T;
    function PeekOrDefault: T; overload;
    function PeekOrDefault(const predicate: TPredicate<T>): T; overload;
    function TryPeek(out item: T): Boolean;
    procedure Clear;
    procedure TrimExcess;
  end;

  TQueue<T> = class(TEnumerableBase<T>, IQueue<T>)
  private
    type
      TGenericQueue = Generics.Collections.TQueue<T>;
  private
    fQueue: TGenericQueue;
    fOwnership: TOwnershipType;
  protected
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    constructor Create(queue: TGenericQueue; ownership: TOwnershipType); overload;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<T>; override;
    procedure Enqueue(const item: T);
    function Dequeue: T;
    function Peek: T;
    function PeekOrDefault: T; overload;
    function PeekOrDefault(const predicate: TPredicate<T>): T; overload;
    function TryPeek(out item: T): Boolean;
    procedure Clear;
    procedure TrimExcess;
  end;

  TObjectList<T: class> = class(TList<T>, ICollectionOwnership)
  private
    fOwnsObjects: Boolean;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  protected
    procedure Notify(const item: T; action: TCollectionNotification); override;
  public
    constructor Create(ownsObjects: Boolean = True); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
    constructor Create(collection: TEnumerable<T>; ownsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///	<summary>The adapter implementation for <c>IEnumerator{T}</c>.</summary>
  TEnumeratorAdapter<T> = class(TEnumeratorBase<T>)
  public
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
      TGenericEnumerator = Generics.Collections.TEnumerator<T>;
  private
    fEnumerator: TGenericEnumerator;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(collection: TGenericEnumerable);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// Internal type.
  /// </summary>
  /// <exclude/>
  TStackAccess<T> = class(TEnumerable<T>)
  public
    fCount: Integer;
    fItems: array of T;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Provides static methods to create an instance of various
  ///	interfaced generic collections such as <c>IList{T}</c>,
  ///	<c>IDictionary{TKey, TValue}</c>.</summary>
  {$ENDREGION}
  TCollections = class
  public
    class function CreateList<T>: IList<T>; overload;
    class function CreateList<T>(const comparer: IComparer<T>): IList<T>; overload;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload;
    class function CreateList<T: class>(ownsObjects: Boolean; const comparer: IComparer<T>): IList<T>; overload;

    class function CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(capacity: Integer): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(dictionary: Generics.Collections.TDictionary<TKey, TValue>; ownership: TOwnershipType): IDictionary<TKey, TValue>; overload;

    class function CreateStack<T>: IStack<T>; overload;
    class function CreateStack<T: class>(ownsObjects: Boolean): IStack<T>; overload;

    class function CreateQueue<T>: IQueue<T>; overload;
    class function CreateQueue<T: class>(ownsObjects: Boolean): IQueue<T>; overload;
  end;

const
  doOwnsKeys = Generics.Collections.doOwnsKeys;
  doOwnsValues = Generics.Collections.doOwnsValues;


  {$REGION 'Some Code'}

  (*
  ICollectionDynamic = interface
    ['{CA1A9672-5241-47B7-A54E-8E69CCA9946D}']
    {$REGION 'Property Getters & Setters'}
      function GetCapacity: Integer;
      procedure SetCapacity(const value: Integer);
    {$ENDREGION}
    function EnsureCapacity(capacity: Integer): Integer;
    procedure Shrink;
//    procedure Grow;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  IDictionaryOwnership = interface
    ['{5D149F58-E8EA-480A-8EB6-985A8342BDBD}']
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    procedure SetOwnsKeys(const value: Boolean);
    procedure SetOwnsValues(const value: Boolean);
//    function ExtractPair(const key: T): T;
    property Ownerships: TDictionaryOwnerships;
    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;
  end;

  TObjectStack<T:class> = class(TStack<T>)
  end;

  TObjectQueue<T:class> = class(TQueue<T>)
  end;

  TObjectDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  end;

  ISet<T> = interface(ICollection<T>)
    function Add(const item: T): Boolean;
    procedure ExceptWith(const collection: IEnumerable<T>);
    procedure IntersectWith(const collection: IEnumerable<T>);
    procedure UnionWith(const collection: IEnumerable<T>);
    function EqualsTo(const collection: IEnumerable<T>): Boolean;
    function Overlaps(const collection: IEnumerable<T>): Boolean;
    function IsSubsetOf(const collection: IEnumerable<T>): Boolean;
    function IsSupersetOf(const collection: IEnumerable<T>): Boolean;
  end;

  IEnumerable = interface(IEnumerable<TValue>)
    ['{30F3400E-1D9A-4C57-A7A5-27665CFEE316}']
  end;

  ICollection = interface(IEnumerable)
    ['{B87ABB57-1E75-4DAD-96CE-C077B565F11A}']
  {$REGION 'Property Getters & Setters'}
    function GetIsReadOnly: Boolean;
  {$ENDREGION}
    procedure Add(const item: TValue); overload;
    procedure Clear;
    function Remove(const item: TValue): Boolean; overload;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  IList = interface(ICollection)
    ['{629166B3-E538-430F-BE5A-D6FE42704965}']
  {$REGION 'Property Getters & Setters'}
    function GetItem(index: Integer): TValue;
    procedure SetItem(index: Integer; const item: TValue);
  {$ENDREGION}
    procedure Insert(index: Integer; const item: TValue);
    procedure RemoveAt(index: Integer);
    function IndexOf(const item: TValue): Integer;
    property Items[index: Integer]: TValue read GetItem write SetItem; default;
  end;

  IDictionary = interface(ICollection)
    ['{BAA9A5D9-BBE1-4512-9AA3-9E1F81908857}']
  {$REGION 'Property Getters & Setters'}
    function GetItem(const key: TValue): TValue;
    function GetKeys: ICollection;
    function GetValues: ICollection;
    procedure SetItem(const key: TValue; const value: TValue);
  {$ENDREGION}
    procedure Add(const key: TValue; const value: TValue); overload;
    procedure Remove(const key: TValue); overload;
    function ContainsKey(const key: TValue): Boolean;
    function TryGetValue(const key: TValue; out value: TValue): Boolean;
    property Items[const key: TValue]: TValue read GetItem write SetItem; default;
    property Keys: ICollection read GetKeys;
    property Values: ICollection read GetValues;
  end;
  //*)

  {$ENDREGION}

implementation

uses
  Spring.ResourceStrings,
  Spring.Collections.Extensions;

{$REGION 'TEnumeratorBase<T>'}

function TEnumeratorBase<T>.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TEnumeratorBase<T>.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

function TEnumerableBase<T>.Contains(const item: T): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
  TArgument.CheckNotNull<T>(item, 'item');
  comparer := TEqualityComparer<T>.Default;
  Result := Contains(item, comparer);
end;

function TEnumerableBase<T>.Contains(const item: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  TArgument.CheckNotNull<T>(item, 'item');
  enumerator := GetEnumerator;
  Result := False;
  while enumerator.MoveNext do
  begin
    if comparer.Equals(enumerator.Current, item) then
    begin
      Exit(True);
    end;
  end;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
  if Result then
  begin
    value := enumerator.Current;
  end
end;

function TEnumerableBase<T>.TryGetLast(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
  hasNext: Boolean;
begin
  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
  hasNext := Result;
  while hasNext do
  begin
    value := enumerator.Current;
    hasNext := enumerator.MoveNext;
  end;
end;

function TEnumerableBase<T>.First: T;
begin
  if not TryGetFirst(Result) then
  begin
    raise EInvalidOperation.Create('First');  // TEMP
  end;
end;

function TEnumerableBase<T>.First(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).First; // TEMP
end;

function TEnumerableBase<T>.FirstOrDefault: T;
begin
  if not TryGetFirst(Result) then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
begin
  if not TryGetFirst(Result) then
  begin
    Result := defaultValue;
  end;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).FirstOrDefault; // TEMP
end;

procedure TEnumerableBase<T>.ForEach(const action: TAction<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(action), 'action');
  for item in Self do
  begin
    action(item);
  end;
end;

procedure TEnumerableBase<T>.ForEach(const action: TActionProc<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(action), 'action');
  for item in Self do
  begin
    action(item);
  end;
end;

procedure TEnumerableBase<T>.ForEach(const action: TActionMethod<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(action), 'action');
  for item in Self do
  begin
    action(item);
  end;
end;

function TEnumerableBase<T>.Last: T;
begin
  if not TryGetLast(Result) then
  begin
    raise EInvalidOperation.Create('Last');  // TEMP
  end;
end;

function TEnumerableBase<T>.Last(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).Last;
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
begin
  if not TryGetLast(Result) then
  begin
    Result := defaultValue;
  end;
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  if not TryGetLast(Result) then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).LastOrDefault;
end;

function TEnumerableBase<T>.Where(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');
  Result := TEnumerableWithPredicate<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
begin
  Result := ToList.ToArray;
end;

function TEnumerableBase<T>.ToList: IList<T>;
var
  enumerator: IEnumerator<T>;
begin
  Result := TCollections.CreateList<T>;
  enumerator := GetEnumerator;
  while enumerator.MoveNext do
  begin
    Result.Add(enumerator.Current);
  end;
end;

function TEnumerableBase<T>.GetCount: Integer;
var
  enumerator: IEnumerator<T>;
begin
  Result := 0;
  enumerator := GetEnumerator;
  while enumerator.MoveNext do
  begin
    Inc(Result);
  end;
end;

function TEnumerableBase<T>.GetIsEmpty: Boolean;
begin
  Result := not GetEnumerator.MoveNext;
end;

function TEnumerableBase<T>.GetItemType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TCollectionBase<T>'}

procedure TCollectionBase<T>.AddRange(const collection: array of T);
var
  item: T;
begin
  for item in collection do
  begin
    Add(item);
  end;
end;

procedure TCollectionBase<T>.AddRange(const collection: IEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Add(item);
  end;
end;

procedure TCollectionBase<T>.AddRange(const collection: TEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Add(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const collection: array of T);
var
  item: T;
begin
  for item in collection do
  begin
    Remove(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const collection: TEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Remove(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const collection: IEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Remove(item);
  end;
end;

function TCollectionBase<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

{$ENDREGION}


{$REGION 'TContainedCollectionBase<T>'}

constructor TContainedCollectionBase<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedCollectionBase<T>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedCollectionBase<T>._AddRef: Integer;
begin
  Result := Controller._AddRef;
end;

function TContainedCollectionBase<T>._Release: Integer;
begin
  Result := Controller._Release;
end;

{$ENDREGION}


{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(collection: TGenericEnumerable);
begin
  inherited Create;
  fEnumerator := collection.GetEnumerator;
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  fEnumerator.Free;
  inherited Destroy;
end;

function TEnumeratorAdapter<T>.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TList<T>'}

constructor TList<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TList<T>.Create(capacity: Integer);
begin
  Create;
  Self.Capacity := capacity;
end;

constructor TList<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
  if fComparer = nil then
    fComparer := TComparer<T>.Default;
end;

constructor TList<T>.Create(const collection: IEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

constructor TList<T>.Create(const collection: TEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

destructor TList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TList<T>.EnsureCapacity(value: Integer): Integer;
var
  newCapacity: Integer;
begin
  newCapacity := Length(FItems);
  if newCapacity = 0 then
    newCapacity := value
  else
    repeat
      newCapacity := newCapacity * 2;
      if newCapacity < 0 then
        OutOfMemoryError;
    until newCapacity >= value;
  Capacity := newCapacity;
  Result := newCapacity;
end;

procedure TList<T>.Add(const item: T);
begin
  EnsureCapacity(Count + 1);
  fItems[fCount] := item;
  Inc(fCount);
  Notify(item, cnAdded);
end;

procedure TList<T>.Insert(index: Integer; const item: T);
begin
  TArgument.CheckRange<T>(fItems, index);
  EnsureCapacity(Count + 1);
  if index <> Count then
  begin
    System.Move(fItems[index], fItems[index + 1], (Count - index) * SizeOf(T));
    FillChar(fItems[index], SizeOf(fItems[index]), 0);
  end;
  fItems[index] := item;
  Inc(fCount);
  Notify(item, cnAdded);
end;

procedure TList<T>.InsertRange(index: Integer;
  const collection: array of T);
var
  item: T;
begin
  TArgument.CheckRange<T>(fItems, index);
  EnsureCapacity(Count + Length(collection));
  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TList<T>.InsertRange(index: Integer;
  const collection: IEnumerable<T>);
var
  item: T;
begin
  TArgument.CheckRange<T>(fItems, index);
  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TList<T>.InsertRange(index: Integer;
  const collection: TEnumerable<T>);
var
  item: T;
begin
  TArgument.CheckRange<T>(fItems, index);
  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

function TList<T>.Remove(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item);
  Result := index > -1;
  if Result then
  begin
    DoDelete(index, cnRemoved);
  end;
end;

procedure TList<T>.RemoveAt(index: Integer);
begin
  Delete(index);
end;

function TList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := fItems[index];
    DoDelete(index, cnExtracted);
  end;
end;

procedure TList<T>.DoDelete(index: Integer;
  notification: TCollectionNotification);
var
  oldItem: T;
begin
  TArgument.CheckRange<T>(fItems, index);
  oldItem := fItems[index];
  fItems[index] := Default(T);
  Dec(fCount);
  if index <> Count then
  begin
    System.Move(fItems[index + 1], fItems[index], (Count - index) * SizeOf(T));
    FillChar(fItems[Count], SizeOf(T), 0);
  end;
  Notify(oldItem, notification);
end;

procedure TList<T>.Delete(index: Integer);
begin
  TArgument.CheckRange<T>(fItems, index);
  DoDelete(index, cnRemoved);
end;

procedure TList<T>.DeleteRange(startIndex, count: Integer);
var
  oldItems: array of T;
  tailCount,
  i: Integer;
begin
  if (startIndex < 0) or
    (count < 0) or
    (startIndex + count > Self.Count) or
    (startIndex + count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRangeException);
  if count = 0 then
    Exit;

  SetLength(oldItems, count);
  System.Move(fItems[startIndex], oldItems[0], count * SizeOf(T));

  tailCount := Self.Count - (startIndex + count);
  if tailCount > 0 then
  begin
    System.Move(fItems[startIndex + count], fItems[startIndex], tailCount * SizeOf(T));
    FillChar(fItems[Self.Count - count], count * SizeOf(T), 0);
  end
  else
  begin
    FillChar(fItems[startIndex], count * SizeOf(T), 0);
  end;
  Dec(fCount, count);

  for i := 0 to Length(oldItems) - 1 do
  begin
    Notify(oldItems[i], cnRemoved);
  end;
end;

procedure TList<T>.Clear;
begin
  if Count > 0 then
  begin
    DeleteRange(0, Count);
  end;
  Capacity := 0;
end;

function TList<T>.Contains(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item);
  Result := index > -1;
end;

function TList<T>.IndexOf(const item: T): Integer;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
  begin
    if fComparer.Compare(fItems[i], item) = 0 then
      Exit(i);
  end;
  Result := -1;
end;

function TList<T>.LastIndexOf(const item: T): Integer;
var
  i: Integer;
begin
  for i := fCount - 1 downto 0 do
  begin
    if fComparer.Compare(fItems[i], item) = 0 then
      Exit(i);
  end;
  Result := -1;
end;

procedure TList<T>.Notify(const item: T; action: TCollectionNotification);
begin

end;

function TList<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := fItems[i];
  end;
end;

function TList<T>.ToList: IList<T>;
begin
  Result := Self;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

function TList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TList<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TList<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[0];
end;

function TList<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[Count - 1];
end;

function TList<T>.GetItem(index: Integer): T;
begin
  TArgument.CheckRange<T>(fItems, index);
  Result := fItems[index];
end;

procedure TList<T>.SetCapacity(value: Integer);
begin
  if value < Count then
  begin
    DeleteRange(Count - value + 1, Count - value);
  end;
  SetLength(fItems, value);
end;

procedure TList<T>.SetItem(index: Integer; const value: T);
var
  oldItem: T;
begin
  TArgument.CheckRange<T>(fItems, index);
    
  oldItem := fItems[index];
  fItems[index] := value;
  
  Notify(oldItem, cnRemoved);
  Notify(value, cnAdded);
end;

function TList<T>.GetPropertyValue(const propertyName: string;
  const index: Rtti.TValue): Rtti.TValue;
var
  indexValue: Integer;
begin
  TArgument.CheckTrue(SameText(propertyName, 'Items'), propertyName);
  TArgument.CheckTrue(index.TryAsType<Integer>(indexValue), 'index');
  Result := Rtti.TValue.From<T>(Self.Items[indexValue]);
end;

procedure TList<T>.SetPropertyValue(const propertyName: string;
  const index, value: Rtti.TValue);
var
  indexValue: Integer;
begin
  TArgument.CheckTrue(SameText(propertyName, 'Items'), propertyName);
  TArgument.CheckTrue(index.TryAsType<Integer>(indexValue), 'index');
  Self.Items[indexValue] := value.AsType<T>;
end;

{$ENDREGION}


{$REGION 'TList<T>.TEnumerator'}

constructor TList<T>.TEnumerator.Create(const list: TList<T>);
begin
  inherited Create;
  fList := list;
  fIndex := -1;
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fList.Count - 1;
  if Result then
    Inc(fIndex);
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fList[fIndex];
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(
  dictionary: TGenericDictionary; ownership: TOwnershipType);
begin
  inherited Create;
  fDictionary := dictionary;
  fOwnership := ownership;
end;

constructor TDictionary<TKey, TValue>.Create;
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create;
  Create(dictionary, otOwned);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  fKeys.Free;
  fValues.Free;
  if fOwnership = otOwned then
  begin
    fDictionary.Free;
  end;
  inherited Destroy;
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TEnumeratorAdapter<TPair<TKey, TValue>>.Create(fDictionary);
end;

procedure TDictionary<TKey, TValue>.Add(const item: TPair<TKey, TValue>);
begin
  fDictionary.Add(item.Key, item.Value);
end;

procedure TDictionary<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
end;

function TDictionary<TKey, TValue>.Contains(
  const item: TPair<TKey, TValue>): Boolean;
var
  value: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.TryGetValue(item.Key, value);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(value, item.Value);
  end;
end;

function TDictionary<TKey, TValue>.Remove(
  const item: TPair<TKey, TValue>): Boolean;
var
  value: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.TryGetValue(item.Key, value);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(value, item.Value);
    if Result then
    begin
      fDictionary.Remove(item.Key);
    end;
  end;
end;

function TDictionary<TKey, TValue>.Extract(
  const item: TPair<TKey, TValue>): TPair<TKey, TValue>;
var
  value: TValue;
  found: Boolean;
  comparer: IEqualityComparer<TValue>;
begin
  found := fDictionary.TryGetValue(item.Key, value);
  if found then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    found := comparer.Equals(value, item.Value);
    if found then
    begin
      Result := fDictionary.ExtractPair(item.Key);
    end;
  end;
  if not found then
  begin
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  pair: TPair<TKey, TValue>;
  index: Integer;
begin
  SetLength(Result, fDictionary.Count);
  index := 0;
  for pair in fDictionary do
  begin
    Result[index].Key := pair.Key;
    Result[index].Value := pair.Value;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

function TDictionary<TKey, TValue>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  fDictionary.Add(key, value);
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TPair<TKey, TValue>;
begin
  Result := fDictionary.ExtractPair(key);
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fDictionary.TryGetValue(key, value);
end;

procedure TDictionary<TKey, TValue>.Remove(const key: TKey);
begin
  fDictionary.Remove(key);
end;

function TDictionary<TKey, TValue>.GetKeys: ICollection<TKey>;
begin
  if fKeys = nil then
  begin
    fKeys := TKeyCollection.Create(Self, fDictionary);
  end;
  Result := fKeys;
end;

function TDictionary<TKey, TValue>.GetValues: ICollection<TValue>;
begin
  if fValues = nil then
  begin
    fValues := TValueCollection.Create(Self, fDictionary);
  end;
  Result := fValues;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := fDictionary[key];
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TKeyCollection'}

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(
  const controller: IInterface; dictionary: TGenericDictionary);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TKeyCollection.Contains(
  const item: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  key: TKey;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for key in fDictionary.Keys do
  begin
    Result[index] := key;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TEnumeratorAdapter<TKey>.Create(fDictionary.Keys);
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

procedure TDictionary<TKey, TValue>.TKeyCollection.Add(const item: TKey);
begin
  raise ENotSupportedException.Create('Add');
end;

procedure TDictionary<TKey, TValue>.TKeyCollection.Clear;
begin
  raise ENotSupportedException.Create('Clear');
end;

function TDictionary<TKey, TValue>.TKeyCollection.Extract(
  const item: TKey): TKey;
begin
  raise ENotSupportedException.Create('Extract');
end;

function TDictionary<TKey, TValue>.TKeyCollection.Remove(
  const item: TKey): Boolean;
begin
  raise ENotSupportedException.Create('Remove');
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TValueCollection'}

constructor TDictionary<TKey, TValue>.TValueCollection.Create(
  const controller: IInterface; dictionary: TGenericDictionary);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TValueCollection.Contains(
  const item: TValue): Boolean;
begin
  Result := fDictionary.ContainsValue(item);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  value: TValue;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for value in fDictionary.Values do
  begin
    Result[index] := value;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumeratorAdapter<TValue>.Create(fDictionary.Values);
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Values.Count;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

procedure TDictionary<TKey, TValue>.TValueCollection.Add(const item: TValue);
begin
  raise ENotSupportedException.Create('Add');
end;

procedure TDictionary<TKey, TValue>.TValueCollection.Clear;
begin
  raise ENotSupportedException.Create('Clear');
end;

function TDictionary<TKey, TValue>.TValueCollection.Extract(
  const item: TValue): TValue;
begin
  raise ENotSupportedException.Create('Extract');
end;

function TDictionary<TKey, TValue>.TValueCollection.Remove(
  const item: TValue): Boolean;
begin
  raise ENotSupportedException.Create('Remove');
end;

{$ENDREGION}


{$REGION 'TStack<T>'}

constructor TStack<T>.Create(stack: TGenericStack;
  ownership: TOwnershipType);
begin
  inherited Create;
  fStack := stack;
  fOwnership := ownership;
end;

constructor TStack<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    push(item);
  end;
end;

constructor TStack<T>.Create(const collection: TEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    push(item);
  end;
end;

constructor TStack<T>.Create;
var
  stack: TGenericStack;
begin
  stack := TGenericStack.Create;
  Create(stack, otOwned);
end;

destructor TStack<T>.Destroy;
begin
  if fOwnership = otOwned then
    fStack.Free;
  inherited Destroy;
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TStackEnumerator.Create(fStack);
end;

function TStack<T>.GetCount: Integer;
begin
  Result := fStack.Count;
end;

function TStack<T>.GetIsEmpty: Boolean;
begin
  Result := fStack.Count = 0;
end;

procedure TStack<T>.Push(const item: T);
begin
  fStack.Push(item);
end;

function TStack<T>.Pop: T;
begin
  Result := fStack.Pop;
end;

procedure TStack<T>.Clear;
begin
  fStack.Clear;
end;

function TStack<T>.Peek: T;
begin
  Result := fStack.Peek;
end;

function TStack<T>.PeekOrDefault(const predicate: TPredicate<T>): T;
var
  item: T;
begin
  Result := Default(T);
  if (fStack.Count = 1) and predicate(fStack.Peek) then
  begin
    Result := fStack.Peek;
  end
  else if fStack.Count > 0 then
  begin
    for item in fStack do
    begin
      if predicate(item) then
      begin
        Result := item;
        Break;
      end;
    end;
  end;
end;

function TStack<T>.PeekOrDefault: T;
begin
  if fStack.Count > 0 then
    Result := fStack.Peek
  else
    Result := Default(T);
end;

procedure TStack<T>.TrimExcess;
begin
  fStack.TrimExcess;
end;

function TStack<T>.TryPeek(out item: T): Boolean;
begin
  Result := fStack.Count > 0;
  if Result then
    item := fStack.Peek
  else
    item := Default(T);
end;

{$ENDREGION}


{$REGION 'TStack<T>.TEnumerator'}

constructor TStack<T>.TStackEnumerator.Create(stack: TGenericStack);
begin
  inherited Create;
  fStack := stack;
  fIndex := fStack.Count;
end;

function TStack<T>.TStackEnumerator.GetCurrent: T;
begin
  Result := TStackAccess<T>(fStack).FItems[fIndex];
end;

function TStack<T>.TStackEnumerator.MoveNext: Boolean;
begin
  Result := fIndex > 0;
  if Result then
    Dec(fIndex);
end;

{$ENDREGION}


{$REGION 'TQueue<T>'}

constructor TQueue<T>.Create(queue: TGenericQueue;
  ownership: TOwnershipType);
begin
  fQueue := queue;
  fOwnership := ownership;
end;

constructor TQueue<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    Enqueue(item);
  end;
end;

constructor TQueue<T>.Create(const collection: TEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
  begin
    Enqueue(item);
  end;
end;

constructor TQueue<T>.Create;
var
  queue: TGenericQueue;
begin
  queue := TGenericQueue.Create;
  Create(queue, otOwned);
end;

destructor TQueue<T>.Destroy;
begin
  if fOwnership = otOwned then
    fQueue.Free;
  inherited Destroy;
end;

function TQueue<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fQueue);
end;

procedure TQueue<T>.Enqueue(const item: T);
begin
  fQueue.Enqueue(item);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := fQueue.Dequeue;
end;

procedure TQueue<T>.Clear;
begin
  fQueue.Clear;
end;

function TQueue<T>.Peek: T;
begin
  Result := fQueue.Peek;
end;

function TQueue<T>.PeekOrDefault: T;
begin
  if fQueue.Count > 0 then
    Result := fQueue.Peek
  else
    Result := Default(T);
end;

function TQueue<T>.PeekOrDefault(const predicate: TPredicate<T>): T;
var
  item: T;
begin
  Result := Default(T);
  if (fQueue.Count = 1) and predicate(fQueue.Peek) then
  begin
    Result := fQueue.Peek;
  end
  else if fQueue.Count > 0 then
  begin
    for item in fQueue do
    begin
      if predicate(item) then
      begin
        Result := item;
        Break;
      end;
    end;
  end;
end;

procedure TQueue<T>.TrimExcess;
begin
  fQueue.TrimExcess;
end;

function TQueue<T>.TryPeek(out item: T): Boolean;
begin
  Result := fQueue.Count > 0;
  if Result then
    item := fQueue.Peek
  else
    item := Default(T);
end;

function TQueue<T>.GetCount: Integer;
begin
  Result := fQueue.Count;
end;

function TQueue<T>.GetIsEmpty: Boolean;
begin
  Result := fQueue.Count = 0;
end;

{$ENDREGION}


{$REGION 'TObjectList<T>'}

constructor TObjectList<T>.Create(ownsObjects: Boolean);
begin
  Create(TComparer<T>.Default, ownsObjects);
end;

constructor TObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(collection: TEnumerable<T>;
  ownsObjects: Boolean);
begin
  Create(TComparer<T>.Default, ownsObjects);
  AddRange(collection);
end;

function TObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

procedure TObjectList<T>.Notify(const item: T; action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (action = cnRemoved) then
    item.Free;
end;

{$ENDREGION}


{$REGION 'TCollections'}

class function TCollections.CreateList<T>: IList<T>;
begin
  Result := TList<T>.Create;
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>): IList<T>;
begin
  Result := TList<T>.Create(comparer);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean): IList<T>;
begin
  Result := TCollections.CreateList<T>(ownsObjects, TComparer<T>.Default);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean;
  const comparer: IComparer<T>): IList<T>;
begin
  Result := TObjectList<T>.Create(comparer, ownsObjects);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  dictionary: Generics.Collections.TDictionary<TKey, TValue>;
  ownership: TOwnershipType): IDictionary<TKey, TValue>;
begin
  TArgument.CheckNotNull(dictionary, 'dictionary');
  Result := TDictionary<TKey, TValue>.Create(dictionary, ownership);
end;

class function TCollections.CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey,TValue>(0, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(capacity, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(0, comparer);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: Generics.Collections.TDictionary<TKey,TValue>;
begin
  TArgument.CheckRange(capacity >= 0, 'capacity');
  dictionary := Generics.Collections.TDictionary<TKey,TValue>.Create(capacity, comparer);
  Result := TDictionary<TKey, TValue>.Create(dictionary, otOwned);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(ownerships, 0, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships;
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(ownerships, capacity, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships; capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: Generics.Collections.TObjectDictionary<TKey,TValue>;
begin
  dictionary := TObjectDictionary<TKey, TValue>.Create(ownerships, capacity, comparer);
  Result := TDictionary<TKey, TValue>.Create(dictionary, otOwned);
end;

class function TCollections.CreateStack<T>: IStack<T>;
var
  stack: Generics.Collections.TStack<T>;
begin
  stack := Generics.Collections.TStack<T>.Create;
  Result := TStack<T>.Create(stack, otOwned);
end;

class function TCollections.CreateStack<T>(ownsObjects: Boolean): IStack<T>;
var
  stack: Generics.Collections.TObjectStack<T>;
begin
  stack := TObjectStack<T>.Create(ownsObjects);
  Result := TStack<T>.Create(stack, otOwned);
end;

class function TCollections.CreateQueue<T>: IQueue<T>;
var
  queue: Generics.Collections.TQueue<T>;
begin
  queue := Generics.Collections.TQueue<T>.Create;
  Result := TQueue<T>.Create(queue, otOwned);
end;

class function TCollections.CreateQueue<T>(ownsObjects: Boolean): IQueue<T>;
var
  queue: Generics.Collections.TObjectQueue<T>;
begin
  queue := Generics.Collections.TObjectQueue<T>.Create(ownsObjects);
  Result := TQueue<T>.Create(queue, otOwned);
end;

{$ENDREGION}

end.
