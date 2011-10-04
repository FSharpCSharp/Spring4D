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
  ISet<T>   = interface;

  ICollectionNotifyDelegate<T> = interface;

  TCollectionNotification = Generics.Collections.TCollectionNotification;

  /// <summary>
  /// Represents an iterator over a generic enumerable collection.
  /// </summary>
  IEnumerator<T> = interface(IInterface)
    /// <summary>
    /// The getter of the <see cref="Current" /> property.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperation">
    /// The enumerator has not been started or ended.
    /// </exception>
    function GetCurrent: T;

    /// <summary>
    /// Advances the enumerator to the next element of the collection.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperation">
    /// The collection was modified after the enumerator was created.
    /// </exception>
    function MoveNext: Boolean;

    /// <exception cref="Spring|EInvalidOperation">
    /// The collection was modified after the enumerator was created.
    /// </exception>
    /// <exception cref="Spring|ENotSupportedException">
    /// The Reset method is not supported.
    /// </exception>
    procedure Reset;

    /// <summary>
    /// Gets the current element in the collection.
    /// </summary>
    property Current: T read GetCurrent;
  end;

  (*

    Distinct, Union, Intersect, Exclude

    Select<T>, SelectMany<T>
    OfType<T>

    ToDictionary<TKey, TValue>

    Aggregate<>

    GroupBy
    OrderBy, OrderByDescending, ThenBy
  *)

  ///	<summary>
  /// Provides limited LINQ-like enumerable extension methods for
  ///	<c>IEnumerable{T}</c>.
  /// </summary>
  /// <seealso href="http://msdn.microsoft.com/en-us/magazine/cc700332.aspx">
  /// The LINQ Enumerable Class
  /// </seealso>
  IEnumerable<T> = interface(IInvokable)
    /// <summary>
    /// Returns an enumerator that iterates through a collection.
    /// </summary>
    function GetEnumerator: IEnumerator<T>;

    /// <summary>
    /// Returns the reference to the instance.
    /// </summary>
    function AsObject: TObject;

    /// <summary>
    /// Try getting the first element.
    /// </summary>
    function TryGetFirst(out value: T): Boolean;

    /// <summary>
    /// Try getting the last element.
    /// </summary>
    function TryGetLast(out value: T): Boolean;

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

    ///	<summary>Returns the only element of a sequence, and throws an
    ///	exception if there is not exactly one element in the
    ///	sequence.</summary>
    function Single: T; overload;

    ///	<summary>Returns the only element of a sequence, and throws an
    ///	exception if there is not exactly one element in the
    ///	sequence.</summary>
    function Single(const predicate: TPredicate<T>): T; overload;

    ///	<summary>Returns the only element of a sequence, or a default value if
    ///	the sequence is empty; this method throws an exception if there is more
    ///	than one element in the sequence.</summary>
    function SingleOrDefault: T; overload;

    ///	<summary>Returns the only element of a sequence that satisfies a
    ///	specified condition or a default value if no such element exists; this
    ///	method throws an exception if more than one element satisfies the
    ///	condition.</summary>
    function SingleOrDefault(const predicate: TPredicate<T>): T; overload;

    /// <summary>
    /// Returns the element at a specified index in a sequence.
    /// </summary>
    function ElementAt(index: Integer): T;

    /// <summary>
    /// Returns the element at a specified index in a sequence or a default value
    /// if the index is out of range.
    /// </summary>
    function ElementAtOrDefault(index: Integer): T;

    ///	<summary>
    /// Determines whether all elements of a sequence satisfy a condition.
    ///	</summary>
    function All(const predicate: TPredicate<T>): Boolean;

    ///	<summary>
    /// Determines whether any element of a sequence satisfies a condition.
    ///	</summary>
    function Any(const predicate: TPredicate<T>): Boolean;

    ///	<summary>
    /// Determines whether a sequence contains a specified element by
    ///	using the default equality comparer.
    /// </summary>
    function Contains(const item: T): Boolean; overload;

    ///	<summary>
    /// Determines whether a sequence contains a specified element by
    ///	using a specified <c>IEqualityComparer{T}.</c>
    /// </summary>
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    /// <summary>
    /// Returns the minimum value in a sequence.
    /// </summary>
    function Min: T;

    /// <summary>
    /// Returns the maximum value in a sequence.
    /// </summary>
    function Max: T;

    ///	<summary>Filters a sequence of values based on a predicate.</summary>
    function Where(const predicate: TPredicate<T>): IEnumerable<T>;

    /// <summary>
    /// Bypasses a specified number of elements in a sequence and then returns the remaining elements.
    /// </summary>
    function Skip(count: Integer): IEnumerable<T>;

    /// <summary>
    /// Bypasses elements in a sequence as long as a specified condition is true and then returns the remaining elements.
    /// </summary>
    function SkipWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;

    /// <summary>
    /// Bypasses elements in a sequence as long as a specified condition is true and then returns the remaining elements. The element's index is used in the logic of the predicate function.
    /// </summary>
    function SkipWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    /// Returns a specified number of contiguous elements from the start of a sequence.
    /// </summary>
    function Take(count: Integer): IEnumerable<T>;

    /// <summary>
    /// Returns elements from a sequence as long as a specified condition is true.
    /// </summary>
    function TakeWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;

    /// <summary>
    /// Returns elements from a sequence as long as a specified condition is true.
    /// The element's index is used in the logic of the predicate function.
    /// </summary>
    function TakeWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    /// Concatenates two sequences.
    /// </summary>
    function Concat(const collection: IEnumerable<T>): IEnumerable<T>;
    
    /// <summary>
    /// Inverts the order of the elements in a sequence.
    /// </summary>
    function Reversed: IEnumerable<T>;

    /// <summary>
    /// Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: TAction<T>); overload;

    /// <summary>
    /// Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: TActionProc<T>); overload;

  {$IFDEF DELPHIXE_UP}
    /// <summary>
    /// Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: TActionMethod<T>); overload;
  {$ENDIF}

    /// <summary>
    /// Determines whether two sequences are equal by comparing the elements
    /// by using the default equality comparer for their type.
    /// </summary>
    function EqualsTo(const collection: IEnumerable<T>): Boolean; overload;

    /// <summary>
    /// Determines whether two sequences are equal by comparing their elements
    /// by using a specified IEqualityComparer<T>.
    /// </summary>
    function EqualsTo(const collection: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    ///	<summary>
    /// Creates a new array which is filled with the elements in the collection.
    ///	</summary>
    function ToArray: TArray<T>;

    ///	<summary>
    /// Creates a new list which is filled with the elements in the collection.
    /// </summary>
    function ToList: IList<T>;

    ///	<summary>
    /// Creates a new list which is filled with the elements in the collection.
    /// </summary>
    function ToSet: ISet<T>;

    /// <summary>
    /// The getter of the <see cref="Count" /> property.
    /// </summary>
    function GetCount: Integer;

    /// <summary>
    /// The getter of the <see cref="IsEmpty" /> property.
    /// </summary>
    function GetIsEmpty: Boolean;

    ///	<summary>
    /// Gets the number of elements in the collection.
    /// </summary>
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
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
  {$ENDREGION}

    procedure Add(const item: T); overload;
    procedure AddRange(const collection: array of T); overload;
    procedure AddRange(const collection: IEnumerable<T>); overload;
    procedure AddRange(const collection: TEnumerable<T>); overload;

    function Remove(const item: T): Boolean;
    procedure RemoveRange(const collection: array of T); overload;
    procedure RemoveRange(const collection: IEnumerable<T>); overload;
    procedure RemoveRange(const collection: TEnumerable<T>); overload;

//    function Extract(const item: T): T; overload;
// Ownerships: TCollectionOwnerships (coOwnsElements, coOwnsKeys, coOwnsValues)

    procedure Clear;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  /// <summary>
  /// Represents a collection of objects that can be individually accessed by index.
  /// </summary>
  IList<T> = interface(ICollection<T>)
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
    function GetOnNotify: ICollectionNotifyDelegate<T>;
    procedure SetItem(index: Integer; const item: T);
  {$ENDREGION}
    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const collection: array of T); overload;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload;
    procedure InsertRange(index: Integer; const collection: TEnumerable<T>); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;

    function IndexOf(const item: T): Integer;
    function LastIndexOf(const item: T): Integer;

    property Items[index: Integer]: T read GetItem write SetItem; default;
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

  /// <summary>
  /// Represents a generic collection of key/value pairs.
  /// </summary>
  IDictionary<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>)
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): TValue;
    function GetKeys: ICollection<TKey>;
    function GetValues: ICollection<TValue>;
    function GetOnKeyNotify: ICollectionNotifyDelegate<TKey>;
    function GetOnValueNotify: ICollectionNotifyDelegate<TValue>;
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
    procedure Add(const key: TKey; const value: TValue); overload;
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    procedure Remove(const key: TKey); overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;

    /// <summary>
    /// Gets a read-only collection which contains all keys in the dictionary.
    /// </summary>
    property Keys: ICollection<TKey> read GetKeys;

    /// <summary>
    /// Gets a read-only collection which contains all values in the dictionary.
    /// </summary>
    property Values: ICollection<TValue> read GetValues;

    property OnKeyNotify: ICollectionNotifyDelegate<TKey> read GetOnKeyNotify;
    property OnValueNotify: ICollectionNotifyDelegate<TValue> read GetOnValueNotify;
  end;

  IStack<T> = interface(IEnumerable<T>)
  {$REGION 'Property Getters'}
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  {$ENDREGION}
    procedure Clear;
    procedure Push(const item: T);
    function Pop: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

  IQueue<T> = interface(IEnumerable<T>)
  {$REGION 'Property Getters'}
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  {$ENDREGION}
    procedure Clear;
    procedure Enqueue(const item: T);
    function Dequeue: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

  ISet<T> = interface(ICollection<T>)
    procedure ExceptWith(const collection: IEnumerable<T>);
    procedure IntersectWith(const collection: IEnumerable<T>);
    procedure UnionWith(const collection: IEnumerable<T>);
    function SetEquals(const collection: IEnumerable<T>): Boolean;
    function Overlaps(const collection: IEnumerable<T>): Boolean;
  end;

  ICollectionNotifyDelegate<T> = interface(IMulticastEvent<TCollectionNotifyEvent<T>>)
  end;

  TCollectionNotifyDelegate<T> = class(TMulticastEvent<TCollectionNotifyEvent<T>>, ICollectionNotifyDelegate<T>)
  end;

  ///	<summary>Internal interface. Reserved for future use.</summary>
  ICountable = interface
    ['{CA225A9C-B6FD-4D6E-B3BD-22119CCE6C87}']
    function GetCount: Integer;
    function GetIsEmpty: Boolean;

    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  ///	<summary>Internal interface. Reserved for future use.</summary>
  IElementType = interface
    ['{FE986DD7-41D5-4312-A2F9-94F7D9E642EE}']
    function GetElementType: PTypeInfo;
  end;

  ICollectionOwnership = interface
    ['{6D028EAF-3D14-4362-898C-BFAD1110547F}']

    {$REGION 'Property Accessors'}
      function GetOwnsObjects: Boolean;
      procedure SetOwnsObjects(const value: Boolean);
    {$ENDREGION}
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  /// <summary>
  /// Defines the ownership style of an instance.
  /// </summary>
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

  TArrayEnumerator<T> = class(TEnumeratorBase<T>)
  private
    fArray: TArray<T>;
    fIndex: Integer;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(const value: TArray<T>);
    function MoveNext: Boolean; override;
  end;

  TArrayReversedEnumerator<T> = class(TEnumeratorBase<T>)
  private
    fArray: TArray<T>;
    fIndex: Integer;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(const value: TArray<T>);
    function MoveNext: Boolean; override;
  end;

  /// <summary>
  /// Provides a default implementation for <c>IEnumerable(T)</c> (Extension Methods).
  /// </summary>
  TEnumerableBase<T> = class abstract(TInterfacedObject, IEnumerable<T>, IElementType)
  protected
  {$REGION 'Implements IElementType'}
    function GetElementType: PTypeInfo;
  {$ENDREGION}
    function GetComparer: IComparer<T>; virtual;
    function GetCount: Integer; virtual;
    function GetIsEmpty: Boolean; virtual;
  public
    function GetEnumerator: IEnumerator<T>; virtual; abstract;
    function AsObject: TObject;
    function TryGetFirst(out value: T): Boolean; virtual;
    function TryGetLast(out value: T): Boolean; virtual;
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
    function Single: T; overload;
    function Single(const predicate: TPredicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const predicate: TPredicate<T>): T; overload;
    function ElementAt(index: Integer): T;
    function ElementAtOrDefault(index: Integer): T;
    function Min: T;
    function Max: T;
    function Contains(const item: T): Boolean; overload; virtual;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function All(const predicate: TPredicate<T>): Boolean;
    function Any(const predicate: TPredicate<T>): Boolean;
    function Where(const predicate: TPredicate<T>): IEnumerable<T>; virtual;
    function Skip(count: Integer): IEnumerable<T>;
    function SkipWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function SkipWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;
    function Take(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;
    function Concat(const collection: IEnumerable<T>): IEnumerable<T>;
    function Reversed: IEnumerable<T>; virtual;
    procedure ForEach(const action: TAction<T>); overload;
    procedure ForEach(const action: TActionProc<T>); overload;
    procedure ForEach(const action: TActionMethod<T>); overload;
    function EqualsTo(const collection: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const collection: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>; virtual;
    function ToList: IList<T>; virtual;
    function ToSet: ISet<T>; virtual;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  /// Provides an abstract class base for <c>IColleciton{T}</c>.
  /// </summary>
  /// <remarks>
  ///   Notes: The Add/Remove/Clear methods are abstract. IsReadOnly returns False by default.
  /// </remarks>
  TCollectionBase<T> = class abstract(TEnumerableBase<T>, ICollection<T>)
  protected
    function GetIsReadOnly: Boolean; virtual;
  public
    procedure Add(const item: T); virtual; abstract;
    procedure AddRange(const collection: array of T); overload; virtual;
    procedure AddRange(const collection: IEnumerable<T>); overload; virtual;
    procedure AddRange(const collection: TEnumerable<T>); overload; virtual;

    function  Remove(const item: T): Boolean; virtual; abstract;
    procedure RemoveRange(const collection: array of T); overload; virtual;
    procedure RemoveRange(const collection: IEnumerable<T>); overload; virtual;
    procedure RemoveRange(const collection: TEnumerable<T>); overload; virtual;

    procedure Clear; virtual; abstract;

    /// <value>Returns false, by default.</value>
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

  TListBase<T> = class abstract(TCollectionBase<T>, IList<T>)
  protected
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TListBase<T>;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TListBase<T>);
        function MoveNext: Boolean; override;
      end;

      TReversedEnumerable = class(TEnumerableBase<T>)
      private
        fList: TListBase<T>;
        fReference: IInterface;
      public
        constructor Create(const list: TListBase<T>);
        function GetEnumerator: IEnumerator<T>; override;
      end;

      TReversedEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TListBase<T>;
        fCount: Integer;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TListBase<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fComparer: IComparer<T>;
    fOnNotify: ICollectionNotifyDelegate<T>;
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  protected
    function GetComparer: IComparer<T>; override;
  protected
    procedure Notify(const item: T; action: TCollectionNotification); virtual;
    procedure DoSort(const comparer: IComparer<T>); virtual;
    procedure DoInsert(index: Integer; const item: T); virtual; abstract;
    procedure DoDelete(index: Integer; notification: TCollectionNotification); virtual; abstract;
    procedure DoDeleteRange(index, count: Integer; notification: TCollectionNotification); virtual; abstract;
    function  GetItem(index: Integer): T; virtual; abstract;
    procedure SetItem(index: Integer; const value: T); virtual; abstract;
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<T>); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function TryGetFirst(out value: T): Boolean; override;
    function TryGetLast(out value: T): Boolean; override;
    function Contains(const item: T): Boolean; override;
    function ToArray: TArray<T>; override;
    function ToList: IList<T>; override;
    function Reversed: IEnumerable<T>; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;
    function  Remove(const item: T): Boolean; override;
    procedure Clear; override;

    procedure Insert(index: Integer; const item: T); virtual;
    procedure InsertRange(index: Integer; const collection: array of T); overload; virtual;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload; virtual;
    procedure InsertRange(index: Integer; const collection: TEnumerable<T>); overload; virtual;
    procedure Delete(index: Integer);
    procedure DeleteRange(startIndex, count: Integer);
    function Extract(const item: T): T;
    function IndexOf(const item: T): Integer;
    function LastIndexOf(const item: T): Integer;
    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer); virtual; abstract;
    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparison: TComparison<T>); overload;
    procedure Reverse;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property IsReadOnly: Boolean read GetIsReadOnly;
  public
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

  /// <summary>
  /// Provides an array-based implementation of IList<T>.
  /// </summary>
  TList<T> = class(TListBase<T>)
  private
    fItems: array of T;
    fCount: Integer;
  protected
    function GetCount: Integer; override;
    function GetItem(index: Integer): T; override;
    procedure SetItem(index: Integer; const value: T); override;
    procedure DoInsert(index: Integer; const item: T); override;
    procedure DoDelete(index: Integer; notification: TCollectionNotification); override;
    procedure DoDeleteRange(startIndex, count: Integer; notification: TCollectionNotification); override;
    procedure DoSort(const comparer: IComparer<T>); override;
  protected
    function  EnsureCapacity(value: Integer): Integer;
    function  GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    procedure Move(currentIndex, newIndex: Integer); override;
    procedure Clear; override;
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
    fOnKeyNotify: ICollectionNotifyDelegate<TKey>;
    fOnValueNotify: ICollectionNotifyDelegate<TValue>;
    procedure DoKeyNotify(Sender: TObject; const Item: TKey; Action: TCollectionNotification);
    procedure DoValueNotify(Sender: TObject; const Item: TValue; Action: TCollectionNotification);
    function GetOnKeyNotify: ICollectionNotifyDelegate<TKey>;
    function GetOnValueNotify: ICollectionNotifyDelegate<TValue>;
  public
    constructor Create(dictionary: TGenericDictionary;
      ownership: TOwnershipType = otReference); overload;
    constructor Create; overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TPair<TKey,TValue>>; override;
    function GetCount: Integer; override;
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
    property OnKeyNotify: ICollectionNotifyDelegate<TKey> read GetOnKeyNotify;
    property OnValueNotify: ICollectionNotifyDelegate<TValue> read GetOnValueNotify;
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
    fOnNotify: ICollectionNotifyDelegate<T>;
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  protected
    function GetCount: Integer; override;

//    function TryGetFirst(out value: T): Boolean; override;
//    function TryGetLast(out value: T): Boolean; override;
    class function GetStackItem(stack: TGenericStack; index: Integer): T;
  public
    constructor Create; overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    constructor Create(stack: TGenericStack; ownership: TOwnershipType); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    {$REGION 'Implements ICollection<T>'}
      procedure Add(const item: T); // override;
      function  Remove(const item: T): Boolean; // override;
      procedure Clear;
    {$ENDREGION}

    procedure Push(const item: T);
    function Pop: T;
    function Peek: T;
    function PeekOrDefault: T; overload;
    function PeekOrDefault(const predicate: TPredicate<T>): T; overload;
    function TryPeek(out item: T): Boolean;
    procedure TrimExcess;
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

  TQueue<T> = class(TEnumerableBase<T>, IQueue<T>)
  private
    type
      TGenericQueue = Generics.Collections.TQueue<T>;
  private
    fQueue: TGenericQueue;
    fOwnership: TOwnershipType;
    fOnNotify: ICollectionNotifyDelegate<T>;
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  protected
    function GetCount: Integer; override;
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
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
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
    class function CreateObjectList<T: class>(ownsObjects: Boolean = True): IList<T>; overload;
    class function CreateObjectList<T: class>(ownsObjects: Boolean; const comparer: IComparer<T>): IList<T>; overload;

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

    class function CreateSet<T>: ISet<T>; overload;

    /// <summary>
    /// Creates a readonly empty collection.
    /// </summary>
    class function Empty<T>: IEnumerable<T>;
  end;

const
  doOwnsKeys = Generics.Collections.doOwnsKeys;
  doOwnsValues = Generics.Collections.doOwnsValues;

implementation

uses
  Spring.ResourceStrings,
  Spring.Collections.Extensions,
  Spring.Collections.Sets;

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

function TEnumerableBase<T>.All(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := True;
  for item in Self do
  begin
    if not predicate(item) then
      Exit(False);
  end;
end;

function TEnumerableBase<T>.Any(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := False;
  for item in Self do
  begin
    if predicate(item) then
      Exit(True);
  end;
end;

function TEnumerableBase<T>.AsObject: TObject;
begin
  Result := Self;
end;

function TEnumerableBase<T>.Concat(
  const collection: IEnumerable<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(collection), 'collection');

  Result := TConcatEnumerable<T>.Create(Self, collection);
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

function TEnumerableBase<T>.ElementAt(index: Integer): T;
var
  enumerator: IEnumerator<T>;
  localIndex: Integer;
begin
  TArgument.CheckRange(index >= 0, 'index');
  
  enumerator := GetEnumerator;
  localIndex := 0;
  while enumerator.MoveNext do
  begin
    if localIndex = index then
    begin
      Exit(enumerator.Current);
    end;
    Inc(localIndex);
  end;
  TArgument.RaiseArgumentOutOfRangeException('index');
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
var
  enumerator: IEnumerator<T>;
  localIndex: Integer;
begin
  TArgument.CheckRange(index >= 0, 'index');

  enumerator := GetEnumerator;
  localIndex := 0;
  while enumerator.MoveNext do
  begin
    if localIndex = index then
    begin
      Exit(enumerator.Current);
    end;
    Inc(localIndex);
  end;
  Result := Default(T);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>): Boolean;
begin
  Result := EqualsTo(collection, TEqualityComparer<T>.Default);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
var
  e1, e2: IEnumerator<T>;
  hasNext: Boolean;
begin
  TArgument.CheckNotNull(Assigned(collection), 'collection');
  TArgument.CheckNotNull(Assigned(comparer), 'comparer');

  e1 := GetEnumerator;
  e2 := collection.GetEnumerator;

  while True do
  begin
    hasNext := e1.MoveNext;
    if hasNext <> e2.MoveNext then
      Exit(False)
    else if not hasNext then
      Exit(True);
    if hasNext and not comparer.Equals(e1.Current, e2.Current) then
    begin
      Exit(False);
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

function TEnumerableBase<T>.Max: T;
var
  comparer: IComparer<T>;
  hasElement: Boolean;
  item: T;
begin
  comparer := GetComparer;
  hasElement := False;
  for item in Self do
  begin
    if hasElement then
    begin
      if comparer.Compare(item, Result) > 0 then
      begin
        Result := item;
      end;
    end
    else 
    begin
      hasElement := True;
      Result := item;
    end;
  end;
  if not hasElement then
  begin
    raise EInvalidOperation.CreateRes(@SSequenceIsEmpty);
  end;
end;

function TEnumerableBase<T>.Min: T;
var
  comparer: IComparer<T>;
  hasElement: Boolean;
  item: T;
begin
  comparer := GetComparer;
  hasElement := False;
  for item in Self do
  begin
    if hasElement then
    begin
      if comparer.Compare(item, Result) < 0 then
      begin
        Result := item;
      end;
    end
    else 
    begin
      hasElement := True;
      Result := item;
    end;
  end;
  if not hasElement then
  begin
    raise EInvalidOperation.CreateRes(@SSequenceIsEmpty);
  end;
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
var
  list: IList<T>;
begin
  list := TList<T>.Create;
  list.AddRange(Self);
  list.Reverse;
  Result := list;
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
  begin
    raise EInvalidOperation.CreateRes(@SSequenceIsEmpty);
  end;
  Result := enumerator.Current;
  if enumerator.MoveNext then
  begin
    raise EInvalidOperation.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TEnumerableBase<T>.Single(const predicate: TPredicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  isSatisfied: Boolean;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');
  
  enumerator := GetEnumerator;
  
  if not enumerator.MoveNext then
  begin
    raise EInvalidOperation.CreateRes(@SSequenceIsEmpty);
  end;
  
  Result := enumerator.Current;
  isSatisfied := predicate(Result);
  
  while enumerator.MoveNext do
  begin
    Result := enumerator.Current;
    if predicate(Result) then
    begin
      if isSatisfied then
      begin
        raise EInvalidOperation.CreateRes(@SMoreThanOneElementSatisfied);
      end;
      isSatisfied := True;
    end;
  end;  
  if not isSatisfied then
  begin
    raise EInvalidOperation.CreateRes(@SNoElementSatisfiesCondition);
  end;
end;

function TEnumerableBase<T>.SingleOrDefault: T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
  begin
    Exit(Default(T));
  end;
  Result := enumerator.Current;
  if enumerator.MoveNext then
  begin
    raise EInvalidOperation.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: TPredicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  isSatisfied: Boolean;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');
  
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
  begin
    Exit(Default(T));
  end;
  
  Result := enumerator.Current;
  isSatisfied := predicate(Result);
  
  while enumerator.MoveNext do
  begin
    Result := enumerator.Current;
    if predicate(Result) then
    begin
      if isSatisfied then
      begin
        raise EInvalidOperation.CreateRes(@SMoreThanOneElementSatisfied);
      end;
      isSatisfied := True;
    end;
  end;  

  if not isSatisfied then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableBase<T>.Where(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');
  
  Result := TWhereEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>;
begin
  Result := TSkipEnumerable<T>.Create(Self, count);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileIndexEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>;
begin
  Result := TTakeEnumerable<T>.Create(Self, count);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileIndexEnumerable<T>.Create(Self, predicate);
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

function TEnumerableBase<T>.ToSet: ISet<T>;
var
  item: T;
begin
  Result := THashSet<T>.Create;
  for item in Self do
  begin
    Result.Add(item);
  end;
end;

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := TComparer<T>.Default;
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
  Result := Count = 0;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
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


{$REGION 'TListBase<T>'}

constructor TListBase<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TListBase<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fComparer := comparer;
  if fComparer = nil then
    fComparer := TComparer<T>.Default;
end;

constructor TListBase<T>.Create(const collection: IEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

constructor TListBase<T>.Create(const collection: TEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

destructor TListBase<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TListBase<T>.Remove(const item: T): Boolean;
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

procedure TListBase<T>.DoSort(const comparer: IComparer<T>);
begin
end;

procedure TListBase<T>.Reverse;
var
  tmp: T;
  b, e: Integer;
begin
  b := 0;
  e := Count - 1;
  while b < e do
  begin
    tmp := Items[b];
    Items[b] := Items[e];
    Items[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

function TListBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TReversedEnumerable.Create(Self);
end;

procedure TListBase<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
  temp := Items[index1];
  Items[index1] := Items[index2];
  Items[index2] := temp;
end;

function TListBase<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := Items[index];
    DoDelete(index, cnExtracted);
  end;
end;

procedure TListBase<T>.Delete(index: Integer);
begin
  TArgument.CheckRange((index >= 0) and (index < Count), 'index');

  DoDelete(index, cnRemoved);
end;

procedure TListBase<T>.DeleteRange(startIndex, count: Integer);
begin
  if (startIndex < 0) or
    (count < 0) or
    (startIndex + count > Self.Count) or
    (startIndex + count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRangeException);

  if count = 0 then
    Exit;

  DoDeleteRange(startIndex, count, cnRemoved);
end;

procedure TListBase<T>.Add(const item: T);
begin
  Insert(Count, item);
end;

procedure TListBase<T>.Clear;
begin
  if Count > 0 then
  begin
    DeleteRange(0, Count);
  end;
end;

function TListBase<T>.Contains(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item);
  Result := index > -1;
end;

function TListBase<T>.IndexOf(const item: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if fComparer.Compare(Items[i], item) = 0 then
      Exit(i);
  end;
  Result := -1;
end;

procedure TListBase<T>.InsertRange(index: Integer; const collection: array of T);
var
  item: T;
begin
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.InsertRange(index: Integer;
  const collection: IEnumerable<T>);
var
  item: T;
begin
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.InsertRange(index: Integer;
  const collection: TEnumerable<T>);
var
  item: T;
begin
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.Insert(index: Integer; const item: T);
begin
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

  DoInsert(index, item);
end;

function TListBase<T>.LastIndexOf(const item: T): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if fComparer.Compare(Items[i], item) = 0 then
      Exit(i);
  end;
  Result := -1;
end;

procedure TListBase<T>.Notify(const item: T; action: TCollectionNotification);
begin
  if (fOnNotify <> nil) and fOnNotify.IsNotEmpty and fOnNotify.Enabled then
  begin
    fOnNotify.Invoke(Self, item, action);
  end;
end;

function TListBase<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := Items[i];
  end;
end;

function TListBase<T>.ToList: IList<T>;
begin
  Result := Self;
end;

function TListBase<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TListBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TListBase<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Items[0];
end;

function TListBase<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Items[Count - 1];
end;

function TListBase<T>.GetOnNotify: ICollectionNotifyDelegate<T>;
begin
  if fOnNotify = nil then
  begin
    fOnNotify := TCollectionNotifyDelegate<T>.Create;
  end;
  Result := fOnNotify;
end;

procedure TListBase<T>.Sort;
begin
  DoSort(fComparer);
end;

procedure TListBase<T>.Sort(const comparer: IComparer<T>);
begin
  DoSort(comparer);
end;

procedure TListBase<T>.Sort(const comparison: TComparison<T>);
var
  comparer: IComparer<T>;
begin
  comparer := TComparer<T>.Construct(comparison);
  DoSort(comparer);
end;

{$ENDREGION}


{$REGION 'TListBase<T>.TEnumerator'}

constructor TListBase<T>.TEnumerator.Create(const list: TListBase<T>);
begin
  inherited Create;
  fList := list;
  fIndex := -1;
end;

function TListBase<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fList.Count - 1;
  if Result then
    Inc(fIndex);
end;

function TListBase<T>.TEnumerator.GetCurrent: T;
begin
  Result := fList[fIndex];
end;

{$ENDREGION}


{$REGION 'TListBase<T>.TReversedEnumerator'}

constructor TListBase<T>.TReversedEnumerator.Create(const list: TListBase<T>);
begin
  inherited Create;
  fList := list;
  fCount := fList.Count;
  fIndex := fCount;
end;

function TListBase<T>.TReversedEnumerator.GetCurrent: T;
begin
  Result := fList[fIndex];
end;

function TListBase<T>.TReversedEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex > 0) and (fIndex <= fCount);
  Dec(fIndex);
end;

{ TListBase<T>.TReversedEnumerable }

constructor TListBase<T>.TReversedEnumerable.Create(const list: TListBase<T>);
begin
  inherited Create;
  fList := list;
  fReference := list;
end;

function TListBase<T>.TReversedEnumerable.GetEnumerator: IEnumerator<T>;
begin
  Result := TReversedEnumerator.Create(fList);
end;

{$ENDREGION}


{$REGION 'TList<T>'}

function TList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TList<T>.GetItem(index: Integer): T;
begin
  TArgument.CheckRange<T>(fItems, index);

  Result := fItems[index];
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

procedure TList<T>.DoInsert(index: Integer; const item: T);
begin
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

procedure TList<T>.DoDelete(index: Integer;
  notification: TCollectionNotification);
var
  oldItem: T;
begin
  Assert((index >= 0) and (index <= Count));

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

procedure TList<T>.DoDeleteRange(startIndex, count: Integer; notification: TCollectionNotification);
var
  oldItems: array of T;
  tailCount,
  i: Integer;
begin
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

procedure TList<T>.DoSort(const comparer: IComparer<T>);
begin
  TArray.Sort<T>(fItems, comparer, 0, Count);
end;

procedure TList<T>.Move(currentIndex, newIndex: Integer);
var
  temp: T;
begin
  temp := fItems[currentIndex];
  fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
    System.Move(fItems[currentIndex + 1], fItems[currentIndex], (newIndex - currentIndex) * SizeOf(T))
  else
    System.Move(fItems[newIndex], fItems[newIndex + 1], (currentIndex - newIndex) * SizeOf(T));

  FillChar(fItems[newIndex], SizeOf(T), 0);
  fItems[newIndex] := temp;
end;

procedure TList<T>.Clear;
begin
  inherited;
  Capacity := 0;
end;

function TList<T>.EnsureCapacity(value: Integer): Integer;
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems);
  if newCapacity >= value then
    Exit(newCapacity);

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

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

procedure TList<T>.SetCapacity(value: Integer);
begin
  if value < Count then
  begin
    DeleteRange(Count - value + 1, Count - value);
  end;
  SetLength(fItems, value);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(
  dictionary: TGenericDictionary; ownership: TOwnershipType);
begin
  inherited Create;
  fDictionary := dictionary;
  fOwnership := ownership;
  fDictionary.OnKeyNotify := DoKeyNotify;
  fDictionary.OnValueNotify := DoValueNotify;
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

procedure TDictionary<TKey, TValue>.DoKeyNotify(Sender: TObject;
  const Item: TKey; Action: TCollectionNotification);
begin
  if fOnKeyNotify <> nil then
  begin
    fOnKeyNotify.Invoke(sender, item, action);
  end;
end;

procedure TDictionary<TKey, TValue>.DoValueNotify(Sender: TObject;
  const Item: TValue; Action: TCollectionNotification);
begin
  if fOnValueNotify <> nil then
  begin
    fOnValueNotify.Invoke(sender, item, action);
  end;
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

function TDictionary<TKey, TValue>.GetOnKeyNotify: ICollectionNotifyDelegate<TKey>;
begin
  if fOnKeyNotify = nil then
  begin
    fOnKeyNotify := TCollectionNotifyDelegate<TKey>.Create;
  end;
  Result := fOnKeyNotify;
end;

function TDictionary<TKey, TValue>.GetOnValueNotify: ICollectionNotifyDelegate<TValue>;
begin
  if fOnValueNotify = nil then
  begin
    fOnValueNotify := TCollectionNotifyDelegate<TValue>.Create;
  end;
  Result := fOnValueNotify;
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

function TStack<T>.GetOnNotify: ICollectionNotifyDelegate<T>;
begin
  if fOnNotify = nil then
  begin
    fOnNotify := TCollectionNotifyDelegate<T>.Create;
  end;
  Result := fOnNotify;
end;

{
  TStack<T> = class(TEnumerable<T>)
  private
    FCount: Integer;
    FItems: array of T;
    //...
  end;
}
class function TStack<T>.GetStackItem(stack: TGenericStack; index: Integer): T;
begin
  Result := TArray<T>(PInteger(NativeInt(stack) + hfFieldSize + SizeOf(Integer))^)[index];
end;

procedure TStack<T>.Push(const item: T);
begin
  fStack.Push(item);
end;

function TStack<T>.Pop: T;
begin
  Result := fStack.Pop;
end;

procedure TStack<T>.Add(const item: T);
begin
  fStack.Push(item);
end;

function TStack<T>.Remove(const item: T): Boolean;
//var
//  stack: TStackAccess<T>;
//  comparer: IComparer<T>;
//  element: T;
begin
  // TODO: TStack<T>.Remove
//  stack := TStackAccess<T>(fStack);
//  comparer := TComparer<T>.Default;
  Result := False;
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
  Result := TStack<T>.GetStackItem(fStack, fIndex);
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

function TQueue<T>.GetOnNotify: ICollectionNotifyDelegate<T>;
begin
  if fOnNotify = nil then
  begin
    fOnNotify := TCollectionNotifyDelegate<T>.Create;
  end;
  Result := fOnNotify;
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

class function TCollections.CreateObjectList<T>(ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(TComparer<T>.Default, ownsObjects);
end;

class function TCollections.CreateObjectList<T>(ownsObjects: Boolean; const comparer: IComparer<T>): IList<T>;
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

class function TCollections.CreateSet<T>: ISet<T>;
begin
  Result := THashSet<T>.Create;
end;

class function TCollections.Empty<T>: IEnumerable<T>;
begin
  Result := TNullEnumerable<T>.Create;
end;

{$ENDREGION}


{$REGION 'TArrayEnumerator<T>'}

constructor TArrayEnumerator<T>.Create(const value: TArray<T>);
begin
  inherited Create;
  fArray := value;
  fIndex := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := fArray[fIndex];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Result := fIndex < Length(fArray) - 1;
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TArrayReversedEnumerator<T>'}

constructor TArrayReversedEnumerator<T>.Create(const value: TArray<T>);
begin
  inherited Create;
  fArray := value;
  fIndex := Length(fArray);
end;

function TArrayReversedEnumerator<T>.GetCurrent: T;
begin
  Result := fArray[fIndex];
end;

function TArrayReversedEnumerator<T>.MoveNext: Boolean;
begin
  Result := fIndex > 0;
  if Result then
    Dec(fIndex);
end;

{$ENDREGION}

end.
