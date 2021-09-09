{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2021 Spring4D Team                           }
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

unit Spring.Collections.Base;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Events,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  {$IFDEF MSWINDOWS}
  IEnumerableInternal = interface
    procedure GetEnumerator(var result);
  end;
  IEnumeratorInternal = interface
    procedure GetCurrent(var result);
  end;

  // see https://quality.embarcadero.com/browse/RSP-31615
  {$IF defined(DELPHIX_SYDNEY_UP)}{$DEFINE RSP31615}{$IFEND}
  {$ENDIF}

  PEnumeratorVtable = ^TEnumeratorVtable;
  TEnumeratorVtable = array[0..4] of Pointer;
  PEnumeratorBlock = ^TEnumeratorBlock;
  TEnumeratorBlock = record
    Vtable: Pointer;
    RefCount: Integer;
    TypeInfo: PTypeInfo;
    Parent: TRefCountedObject;
    function _Release: Integer; stdcall;
    class function Create(enumerator: PPointer; vtable: PEnumeratorVtable;
      typeInfo, getCurrent, moveNext: Pointer): Pointer; static;
  end;

  PComparerVtable = ^TComparerVtable;
  TComparerVtable = array[0..3] of Pointer;
  PPairComparer = ^TPairComparer;
  TPairComparer = record
    Vtable: Pointer;
    RefCount: Integer;
    KeyComparer, ValueComparer: IInterface;
    function _Release: Integer; stdcall;
    class function Create(comparer: PPointer; vtable: PComparerVtable;
      compare: Pointer; keyType, valueType: PTypeInfo): Pointer; static;
  end;

  TPairComparer<TKey, TValue> = record
    Vtable: Pointer;
    RefCount: Integer;
    KeyComparer: IComparer<TKey>;
    ValueComparer: IComparer<TValue>;
    class var Comparer_Vtable: TComparerVtable;
    function Compare(const left, right: TPair<TKey, TValue>): Integer;
    class function Default: IComparer<TPair<TKey, TValue>>; static;
  end;

  TComparerThunks<T> = record
    class function Equals(instance: Pointer; const left, right): Boolean; static;
    class function GetHashCode(instance: Pointer; const value): Integer; static;
  end;

  TEnumerableBase = class abstract(TRefCountedObject)
  protected
    this: Pointer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
    function GetIsEmpty: Boolean;
  {$ENDREGION}
    function IsCountInRange(min, max, limit: Integer): Boolean;
  public
    class function NewInstance: TObject; override;

    function Any: Boolean; overload;

    function AtLeast(count: Integer): Boolean;
    function AtMost(count: Integer): Boolean;
    function Between(min, max: Integer): Boolean;
    function Exactly(count: Integer): Boolean;
  end;

  TEnumerableBase<T> = class abstract(TEnumerableBase)
  {$IFDEF RSP31615}
  private type FuncInternal = reference to procedure(
    {$IFDEF CPUX64}var result;{$ENDIF}
    const arg1, arg2: T
    {$IFDEF CPUX86}; var result{$ENDIF});
  {$ENDIF}
  protected
    fComparer: IComparer<T>;
  {$REGION 'Property Accessors'}
    function GetComparer: IComparer<T>;
    function GetElementType: PTypeInfo; virtual;
  {$ENDREGION}
    function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
    procedure CopyTo(var values: TArray<T>; index: Integer);
    function TryGetElementAt(var value: T; index: Integer): Boolean;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetFirst(var value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
    function TryGetLast(var value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetSingle(var value: T): Boolean; overload;
    function TryGetSingle(var value: T; const predicate: Predicate<T>): Boolean; overload;
    property Comparer: IComparer<T> read fComparer;
  public
    procedure AfterConstruction; override;

    function Aggregate(const func: Func<T, T, T>): T;

    function All(const predicate: Predicate<T>): Boolean;

    function Any(const predicate: Predicate<T>): Boolean; overload;

    function Concat(const second: IEnumerable<T>): IEnumerable<T>;

    function Contains(const value: T): Boolean; overload;
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function Contains(const value: T; const comparer: TEqualityComparison<T>): Boolean; overload;

    function ElementAt(index: Integer): T;
    function ElementAtOrDefault(index: Integer): T; overload;
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    function EqualsTo(const values: array of T): Boolean; overload;
    function EqualsTo(const values: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const values: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    function First: T; overload;
    function First(const predicate: Predicate<T>): T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const defaultValue: T): T; overload;
    function FirstOrDefault(const predicate: Predicate<T>): T; overload;
    function FirstOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    procedure ForEach(const action: Action<T>); overload;

    function Last: T; overload;
    function Last(const predicate: Predicate<T>): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const defaultValue: T): T; overload;
    function LastOrDefault(const predicate: Predicate<T>): T; overload;
    function LastOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    function Max: T; overload;
    function Max(const selector: Func<T, Integer>): Integer; overload;
    function Max(const comparer: IComparer<T>): T; overload;
    function Max(const comparer: TComparison<T>): T; overload;
    function Min: T; overload;
    function Min(const selector: Func<T, Integer>): Integer; overload;
    function Min(const comparer: IComparer<T>): T; overload;
    function Min(const comparer: TComparison<T>): T; overload;

    function Memoize: IEnumerable<T>;

    function Ordered: IEnumerable<T>; overload;
    function Ordered(const comparer: IComparer<T>): IEnumerable<T>; overload;
    function Ordered(const comparer: TComparison<T>): IEnumerable<T>; overload;

    function Reversed: IEnumerable<T>;

    function Shuffled: IEnumerable<T>;

    function Single: T; overload;
    function Single(const predicate: Predicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;
    function SingleOrDefault(const predicate: Predicate<T>): T; overload;
    function SingleOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    function Skip(count: Integer): IEnumerable<T>;
    function SkipWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function SkipWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Sum: T;

    function Take(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Where(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function Where(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function ToArray: TArray<T>;
  end;

  TEnumerableWrapper = class(TRefCountedObject, IInterface, IEnumerable)
  private type
    TGetCurrentFunc = function(const enumerator: IEnumerator; elementType: PTypeInfo): Spring.TValue;
    TEnumerator = class(TRefCountedObject, IEnumerator)
    private
      fSource: IEnumerator;
      fElementType: PTypeInfo;
      fGetCurrent: TGetCurrentFunc;
      function GetCurrent: Spring.TValue;
    public
      constructor Create(const source: IEnumerator; elementType: PTypeInfo; getCurrent: TGetCurrentFunc);
      function MoveNext: Boolean;
    end;
  private
    fSource: IEnumerable;
    fGetCurrent: TGetCurrentFunc;
    function GetCount: Integer;
    function GetCountFast: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
  protected
    function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
  public
    class function Create(const source: IEnumerable; out obj;
      getCurrent: TGetCurrentFunc): HResult; overload; static;
    function AsObject: TObject;
    function GetEnumerator: IEnumerator;
  end;

  TCollectionWrapper = class(TEnumerableWrapper, ICollection)
  private type
    TAddFunc = function(const collection: IInterface; const value: Spring.TValue): Boolean;
  private
    fAdd: TAddFunc;
  public
    class function Create(const source: IEnumerable; out obj;
      getCurrent: TEnumerableWrapper.TGetCurrentFunc;
      add: TAddFunc): HRESULT; overload; static;
    function Add(const item: Spring.TValue): Boolean;
    procedure Clear;
  end;

  TIteratorKind = (
    Partition, &Array,
    Concat, Memoize, Ordered, Reversed, Shuffled,
    SkipWhile, SkipWhileIndex,
    TakeWhile, TakeWhileIndex,
    Where, WhereIndex, Select);

  TIteratorMethods = packed record
    MoveNext: function(self: Pointer): Boolean;
    Finalize: function(self: Pointer): Boolean;
  end;

  PIteratorBlock = ^TIteratorBlock;
  TIteratorBlock = record
    // field layout has to match with TIteratorBlock<T> record below
    Vtable: Pointer;
    RefCount: Integer;

    Parent: TRefCountedObject;

    Methods: TIteratorMethods;
    DoMoveNext: function(self: Pointer): Boolean;
    Enumerator: IEnumerator;

    Source: IEnumerable;
    Predicate: IInterface;
    Items: Pointer;
    Index, Count: Integer;
    Kind: TIteratorKind;

    function MoveNextEmpty: Boolean;

    function GetEnumerator: Boolean;
    function GetEnumeratorAndSkip: Boolean;
    function GetEnumeratorMemoize: Boolean;

    function _Release: Integer; stdcall;
    function MoveNext: Boolean;
  end;

  PIteratorBase = ^TIteratorBase;
  TIteratorBase = record
    // field layout has to match with TIteratorBase<T> class
    fSource: IEnumerable;
    fPredicate: IInterface;
    fItems: Pointer;
    fIndex, fCount: Integer;
    fKind: TIteratorKind;

    function GetCountFast: Integer;
  end;

  TMemoizeIterator<T> = record
    // only used via pointer internally
    // field layout has to match with TIteratorBase
    Source: IEnumerable;
    Enumerator: IEnumerator<T>;
    Items: TArray<T>;
    Index, Count: Integer;
  end;

  TIteratorBase<T> = class abstract(TEnumerableBase<T>)
  private
    function HasLimit: Boolean; inline;
  protected
    // field layout has to match with TIteratorBase record
    fSource: IEnumerable<T>;
    fPredicate: IInterface;
    fItems: TArray<T>;
    fIndex, fCount: Integer;
    fKind: TIteratorKind;

    function GetElementType: PTypeInfo; override;
    function PartitionToArray(var values: TArray<T>): Boolean;
  public
    function GetEnumerator: IEnumerator<T>;
    function Skip(count: Integer): IEnumerable<T>;
    function Take(count: Integer): IEnumerable<T>;
    function ToArray: TArray<T>;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
  end;

  TIteratorBlock<T> = record
    // field layout has to match with TIteratorBlock record above
    Vtable: Pointer;
    RefCount: Integer;

    Parent: TRefCountedObject;

    Methods: TIteratorMethods;
    DoMoveNext: function(self: Pointer): Boolean;
    Enumerator: IEnumerator<T>;

    Source: IEnumerable<T>;
    Predicate: IInterface;
    Items: TArray<T>;
    Index, Count: Integer;
    Kind: TIteratorKind;

    Current: T;

    class var Enumerator_Vtable: array[0..4] of Pointer;
    procedure InitMethods;
    procedure InitVtable;

    class function Create(const iterator: TIteratorBase<T>): IEnumerator<T>; static;
    function Finalize: Boolean;

    function GetCurrent: T;

    function MoveNextConcat: Boolean;
    function MoveNextMemoize: Boolean;
    function MoveNextOrdered: Boolean;
    function MoveNextReversed: Boolean;
    function MoveNextSkipWhile: Boolean;
    function MoveNextSkipWhileIndex: Boolean;
    function MoveNextTakeWhile: Boolean;
    function MoveNextTakeWhileIndex: Boolean;
    function MoveNextWhere: Boolean;
    function MoveNextWhereIndex: Boolean;

    function MoveNextEnumerator: Boolean;
    function MoveNextEnumeratorCounted: Boolean;
    function MoveNextIndexed: Boolean;

    function ToArray: Boolean;

    class function DoGetCurrent(const enumerator: IEnumerator; elementType: PTypeInfo): Spring.TValue; static;
    class function DoAdd(const collection: IInterface; const value: Spring.TValue): Boolean; static;
  end;

  TEnumerableIterator<T> = class sealed(TIteratorBase<T>, IInterface, IEnumerable<T>)
  private
    function GetCountFast: Integer;
  public
    class function Create(const source: IEnumerable<T>; index, count: Integer;
      predicate: Pointer; kind: TIteratorKind): IEnumerable<T>; overload; static;
  end;

  TArrayIterator<T> = class(TIteratorBase<T>, IInterface,
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    class function Create(const values: TArray<T>): IReadOnlyList<T>; overload; static;
    class function Create(const values: array of T): IReadOnlyList<T>; overload; static;

  {$REGION 'Implements IReadOnlyCollection<T>'}
    procedure CopyTo(var values: TArray<T>; index: Integer);
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyList<T>'}
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TFoldedArrayIterator<T> = class(TArrayIterator<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    class function Create(const values: TArray<T>; elementType: PTypeInfo): IReadOnlyList<T>; overload; static;
    class function Create(values: PPointer; count: Integer; elementType: PTypeInfo): IReadOnlyList<T>; overload; static;
  end;

  TIterator<T> = class abstract(TEnumerableBase<T>, IInterface, IEnumerator<T>)
  private
    fCurrent: T;
    fThreadId: TThreadID;
    fState: Integer;
    fTryMoveNext: function (self: TObject; var current: T): Boolean;
    const
      STATE_INITIAL    = -2; // initial state, before GetEnumerator
      STATE_FINISHED   = -1; // end of enumerator
      STATE_ENUMERATOR = 0;  // before calling MoveNext
      STATE_RUNNING    = 1;  // enumeration is running
    function GetCurrent: T;
  protected
    function Clone: TIterator<T>; virtual; abstract;
    procedure Dispose; virtual;
    procedure Start; virtual;
    function TryMoveNext(var current: T): Boolean; virtual; abstract;
  public
    procedure AfterConstruction; override;
    function GetEnumerator: IEnumerator<T>;
    function MoveNext: Boolean;
  end;

  TSourceIterator<T> = class abstract(TIterator<T>, IEnumerable<T>)
  strict private
    fSource: IEnumerable<T>;
  protected
    function GetElementType: PTypeInfo; override;
    property Source: IEnumerable<T> read fSource;
  public
    constructor Create(const source: IEnumerable<T>);
  end;

  /// <summary>
  ///   Provides an abstract implementation for the <see cref="Spring.Collections|ICollection&lt;T&gt;" />
  ///    interface.
  /// </summary>
  /// <remarks>
  ///   The Add/Remove/Extract/Clear methods are abstract. IsReadOnly returns <c>
  ///   False</c> by default.
  /// </remarks>
  TCollectionBase<T> = class abstract(TEnumerableBase<T>)
  private type
    TNotify = procedure(Self: TObject; const item: T; action: TCollectionChangedAction);
  private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fNotify: TNotify;
    procedure EventChanged(Sender: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}
    function QueryInterface(const IID: TGUID; out obj): HResult; stdcall;
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
    procedure DoNotify(const item: T; action: TCollectionChangedAction);
      // there are errors in the XE4 Win64 compiler when this method is inline
      {$IF not (Defined(DELPHIXE4) and Defined(WIN64))}inline;{$IFEND}
    procedure Reset;
    property OnChanged: TCollectionChangedEventImpl<T> read fOnChanged;
    property Notify: TNotify read fNotify;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Add(const item: T): Boolean;
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    function RemoveAll(const match: Predicate<T>): Integer;
    function RemoveRange(const values: array of T): Integer; overload;
    function RemoveRange(const values: IEnumerable<T>): Integer; overload;

    function ExtractAll(const match: Predicate<T>): TArray<T>;
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const values: IEnumerable<T>); overload;

    function MoveTo(const collection: ICollection<T>): Integer; overload;
    function MoveTo(const collection: ICollection<T>;
      const match: Predicate<T>): Integer; overload;
  end;

  THashTableEnumerator = class(TRefCountedObject)
  protected
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fIndex: Integer;
    fVersion: Integer;
    fItem: PByte;
  public
    constructor Create(const source: TRefCountedObject; hashTable: PHashTable); overload;
    procedure BeforeDestruction; override;
    function MoveNext: Boolean;
  end;

  TInnerCollection<T> = class sealed(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PT = ^T;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fSource: TInnerCollection<T>;
      fIndex: Integer;
      fVersion: Integer;
      fCurrent: T;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fElementType: PTypeInfo;
    fComparer: IEqualityComparer<T>;
    fOffset: Integer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
  {$ENDREGION}
  protected
    function GetElementType: PTypeInfo; override;
  public
    class function Create(const source: TRefCountedObject; hashTable: PHashTable;
      const comparer: IEqualityComparer<T>; elementType: PTypeInfo;
      offset: Integer): TInnerCollection<T>; overload; static;

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function Contains(const value: T): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
  {$ENDREGION}
  end;

  TCircularArrayBuffer<T> = class(TEnumerableBase<T>)
  private
  {$REGION 'Nested Types'}
    type
      PEnumerator = ^TEnumerator;
      TEnumerator = record
        Vtable: Pointer;
        RefCount: Integer;
        TypeInfo: PTypeInfo;
        fSource: TCircularArrayBuffer<T>;
        fIndex, fCount: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
        function MoveNext: Boolean;
        class var Enumerator_Vtable: TEnumeratorVtable;
      end;
      ItemType = TTypeInfo<T>;
      PT = ^T;
  {$ENDREGION}
  strict private
    fOnChanged: TCollectionChangedEventImpl<T>;
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
    fHead: Integer;
    fTail: Integer;
    function GetTail: Integer; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetCountFast: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}

    procedure AddToHead(const item: T); inline;
    procedure AddToTail(const item: T); inline;
    procedure DeleteFromHead(action: TCollectionChangedAction); inline;
    procedure DeleteFromTail(action: TCollectionChangedAction); inline;

    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Items: TArray<T> read fItems;
    property Head: Integer read fHead;
    property Tail: Integer read GetTail;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(capacity: Integer = 0; ownsObjects: Boolean = False);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function First: T; overload;
    function FirstOrDefault: T; overload;
    function Single: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
  {$ENDREGION}

    procedure Clear;
    procedure TrimExcess;
  end;

  TMapBase<TKey, TValue> = class abstract(TCollectionBase<TPair<TKey, TValue>>)
  private
    type
      TKeyValuePair = TPair<TKey, TValue>;
      TKeyValuePairComparer = TPairComparer<TKey, TValue>;
  protected
    fOnKeyChanged: TCollectionChangedEventImpl<TKey>;
    fOnValueChanged: TCollectionChangedEventImpl<TValue>;
  {$REGION 'Property Accessors'}
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<TValue>;
    function GetKeyType: PTypeInfo; virtual;
    function GetValueType: PTypeInfo; virtual;
  {$ENDREGION}
    procedure DoNotify(const key: TKey; const value: TValue; action: TCollectionChangedAction); overload;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Add(const item: TKeyValuePair): Boolean; overload;
    procedure Add(const key: TKey; const value: TValue); overload;

    function Remove(const item: TKeyValuePair): Boolean;
    function RemoveRange(const keys: array of TKey): Integer; overload;
    function RemoveRange(const keys: IEnumerable<TKey>): Integer; overload;

    function Extract(const item: TKeyValuePair): TKeyValuePair;

    function Contains(const item: TKeyValuePair): Boolean; overload;
  end;

const
  OwnsObjectsBitIndex = 31;
  OwnsObjectsMask     = 1 shl OwnsObjectsBitIndex;
  CountMask           = not OwnsObjectsMask;

procedure AssignComparer(var comparer; const source: IInterface);
procedure EnsureEventInstance(var event: TEventBase; var result;
  eventClass: TEventBaseClass; eventChanged: TNotifyEvent);
function SkipAndCount(const enumerator: IEnumerator; limit: Integer): Integer;
function SupportsIndexedAccess(const source: IInterface): Boolean;
procedure UpdateNotify(instance: TObject; baseClass: TClass; var notify);

implementation

uses
  Rtti,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  Spring.Collections.Lists,
  Spring.Comparers,
  Spring.ResourceStrings;

procedure AssignComparer(var comparer; const source: IInterface);
begin
  IComparer<Integer>(comparer) := IEnumerable<Integer>(source).Comparer;
end;

type
  TEventImpl = class(TEventBase, IEvent);

procedure EnsureEventInstance(var event: TEventBase; var result;
  eventClass: TEventBaseClass; eventChanged: TNotifyEvent);

  procedure CreateEvent(var event: TEventBase; var result;
    eventClass: TEventBaseClass; eventChanged: TNotifyEvent);
  var
    newEvent: TEventBase;
  begin
    newEvent := eventClass.Create;
    if AtomicCmpExchange(Pointer(event), Pointer(newEvent), nil) <> nil then
      newEvent.Free
    else
    begin
      event.OnChanged := eventChanged;
      eventChanged(event);
    end;
    IEvent(result) := TEventImpl(event);
  end;

begin
  if Assigned(event) then
    IntfAssign(IEvent(TEventImpl(event)), IInterface(result))
  else
    CreateEvent(event, result, eventClass, eventChanged);
end;

function SkipAndCount(const enumerator: IEnumerator; limit: Integer): Integer;
begin
  Result := 0;
  while (Result < limit) and enumerator.MoveNext do
    Inc(Result);
end;

function SupportsIndexedAccess(const source: IInterface): Boolean;
var
  obj: TObject;
  entry: PInterfaceEntry;
begin
  obj := IEnumerable(source).AsObject;
  Result := obj.GetInterfaceEntry(IReadOnlyListOfTGuid) <> nil;
  if not Result then
  begin
    entry := obj.GetInterfaceEntry(IPartitionOfTGuid);
    Result := Assigned(entry) and (IEnumerable(PByte(obj) + entry.IOffset).GetCountFast >= 0);
  end;
end;

procedure UpdateNotify(instance: TObject; baseClass: TClass; var notify);
type
  TNotifyRec = record
    event: TEventBase;
    code: Pointer;
  end;
const
  ChangedVirtualIndex = 1;
var
  baseAddress, actualAddress: Pointer;
begin
  baseAddress := PVTable(baseClass)[ChangedVirtualIndex];
  actualAddress := PPVTable(instance)^[ChangedVirtualIndex];
  with TNotifyRec(notify) do
    if (Assigned(event) and event.CanInvoke) or (actualAddress <> baseAddress) then
      code := actualAddress
    else
      code := nil;
end;


{$REGION 'TEnumeratorBlock'}

class function TEnumeratorBlock.Create(enumerator: PPointer; vtable: PEnumeratorVtable;
  typeInfo, getCurrent, moveNext: Pointer): Pointer;

  function GetEnumeratorBlockSize(typeInfo: Pointer): Integer; inline;
  var
    p: PByte;
  begin
    p := typeInfo;
    Result := PTypeData(@p[p[1]+2]).RecSize;
  end;

begin
  IInterface(enumerator^) := nil;
  Result := AllocMem(GetEnumeratorBlockSize(typeInfo));
  PEnumeratorBlock(Result).Vtable := vtable;
  PEnumeratorBlock(Result).RefCount := 1;
  PEnumeratorBlock(Result).TypeInfo := typeInfo;
  enumerator^ := Result;

  if not Assigned(vtable[0]) then
  begin
    vtable[0] := @NopQueryInterface;
    vtable[1] := @RecAddRef;
    vtable[2] := @TEnumeratorBlock._Release;
    vtable[3] := getCurrent;
    vtable[4] := moveNext;
  end;
end;

function TEnumeratorBlock._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    if Assigned(Parent) then
      Parent._Release;
    FinalizeRecord(@Self, TypeInfo);
    FreeMem(@Self);
  end;
end;

{$ENDREGION}


{$REGION 'TPairComparer'}

class function TPairComparer.Create(comparer: PPointer; vtable: PComparerVtable;
  compare: Pointer; keyType, valueType: PTypeInfo): Pointer;
begin
  vtable[0] := @NopQueryInterface;
  vtable[1] := @RecAddRef;
  vtable[2] := @TPairComparer._Release;
  vtable[3] := compare;

  IInterface(comparer^) := nil;
  Result := AllocMem(SizeOf(TPairComparer));
  PPairComparer(Result).Vtable := vtable;
  PPairComparer(Result).RefCount := 1;
  PPairComparer(Result).KeyComparer := IInterface(_LookupVtableInfo(giComparer, keyType, GetTypeSize(keyType)));
  PPairComparer(Result).ValueComparer := IInterface(_LookupVtableInfo(giComparer, valueType, GetTypeSize(valueType)));
  comparer^ := Result;
end;

function TPairComparer._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    KeyComparer := nil;
    ValueComparer := nil;
    FreeMem(@Self);
  end;
end;

{$ENDREGION}


{$REGION 'TPairComparer<TKey, TValue>' }

function TPairComparer<TKey, TValue>.Compare(const left, right: TPair<TKey, TValue>): Integer;
begin
  Result := KeyComparer.Compare(left.Key, right.Key);
  if Result = 0 then
    Result := ValueComparer.Compare(left.Value, right.Value);
end;

class function TPairComparer<TKey, TValue>.Default: IComparer<TPair<TKey, TValue>>;
begin
  TPairComparer.Create(@Result, @Comparer_Vtable,
    @TPairComparer<TKey, TValue>.Compare, TypeInfo(TKey), TypeInfo(TValue));
end;

{$ENDREGION}


{$REGION 'TComparerThunks<T>'}

class function TComparerThunks<T>.Equals(instance: Pointer; const left, right): Boolean;
begin
  Result := IEqualityComparer<T>(instance).Equals(T(left), T(right));
//  Result := TEqualsMethod<T>(instance^)(T(left), T(right));
end;

class function TComparerThunks<T>.GetHashCode(instance: Pointer; const value): Integer;
begin
  Result := IEqualityComparer<T>(instance).GetHashCode(T(value));
//  Result := TGetHashCodeMethod<T>(instance^)(T(value));
end;

{$ENDREGION}


{$REGION 'TEnumerableBase'}

class function TEnumerableBase.NewInstance: TObject;
begin
  Result := inherited NewInstance;

  // child classes must implement IEnumerable<T>
  TEnumerableBase(Result).this := Pointer(PByte(Result) + GetInterfaceEntry(IEnumerableOfTGuid).IOffset);
end;

function TEnumerableBase.Any: Boolean;
begin
  Result := IsCountInRange(1, MaxInt, 1);
end;

function TEnumerableBase.AtLeast(count: Integer): Boolean;
begin
  if count >= 0 then
    Result := IsCountInRange(count, MaxInt, count)
  else
    Result := RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
end;

function TEnumerableBase.AtMost(count: Integer): Boolean;
begin
  if count >= 0 then
    Result := IsCountInRange(0, count, count + 1)
  else
    Result := RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
end;

function TEnumerableBase.Between(min, max: Integer): Boolean;
begin
  if min >= 0 then
    if max >= min then
      Result := IsCountInRange(min, max, max + 1)
    else
      Result := RaiseHelper.ArgumentOutOfRange(ExceptionArgument.max)
  else
    Result := RaiseHelper.ArgumentOutOfRange(ExceptionArgument.min, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
end;

function TEnumerableBase.Exactly(count: Integer): Boolean;
begin
  if count >= 0 then
    Result := IsCountInRange(count, count, count + 1)
  else
    Result := RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
end;

function TEnumerableBase.GetCount: Integer;

  function GetCountSlow(const this: IEnumerable): Integer;
  var
    enumerator: IEnumerator;
  begin
    Result := 0;
    enumerator := this.GetEnumerator;
    while enumerator.MoveNext do
      Inc(Result);
  end;

begin
  Result := IEnumerable(this).GetCountFast;
  if Result < 0 then
    Result := GetCountSlow(IEnumerable(this));
end;

function TEnumerableBase.GetIsEmpty: Boolean;
begin
  Result := IsCountInRange(0, 0, 1);
end;

function TEnumerableBase.IsCountInRange(min, max, limit: Integer): Boolean;

  function SkipAndCountSlow(const this: IEnumerable; limit: Integer): Integer;
  var
    enumerator: IEnumerator;
  begin
    Result := 0;
    enumerator := this.GetEnumerator;
    while (Result < limit) and enumerator.MoveNext do
      Inc(Result);
  end;

var
  count: Integer;
begin
  count := IEnumerable(this).GetCountFast;
  if count < 0 then
    count := SkipAndCountSlow(IEnumerable(this), limit);
  Result := {$B+}(count >= min) and (count <= max);{$B-}
end;

function TEnumerableBase.GetCountFast: Integer;
begin
  // implementing IReadOnlyCollection is an indicator for having its own count
  if GetInterfaceEntry(IReadOnlyCollectionOfTGuid) <> nil then
    Result := IEnumerable(this).Count
  else
    Result := -1;
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

procedure TEnumerableBase<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  if not Assigned(fComparer) then
    fComparer := IComparer<T>(_LookupVtableInfo(giComparer, GetElementType, SizeOf(T)));
end;

function TEnumerableBase<T>.Aggregate(const func: Func<T, T, T>): T;
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item, res: T;
  {$ENDIF}
begin
  if not Assigned(func) then RaiseHelper.ArgumentNil(ExceptionArgument.func);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      FuncInternal(func)({$IFDEF CPUX64}res, {$ENDIF}Result, item{$IFDEF CPUX86}, res{$ENDIF});
      Result := res;
    end
    else
    {$ENDIF}
    Result := func(Result, enumerator.Current);
end;

function TEnumerableBase<T>.All(const predicate: Predicate<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      if not predicate(item) then
        Exit(False);
    end
    else
    {$ENDIF}
    if not predicate(enumerator.Current) then
      Exit(False);
  Result := True;
end;

function TEnumerableBase<T>.Any(const predicate: Predicate<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      if predicate(item) then
        Exit(True);
    end
    else
    {$ENDIF}
    if predicate(enumerator.Current) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase<T>.Concat(const second: IEnumerable<T>): IEnumerable<T>;
begin
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, Pointer(second), TIteratorKind.Concat);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
var
  comparer: Pointer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T));
  Result := IEnumerable<T>(this).Contains(value, IEqualityComparer<T>(comparer));
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      if comparer.Equals(item, value) then
        Exit(True);
    end
    else
    {$ENDIF}
    if comparer.Equals(enumerator.Current, value) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: TEqualityComparison<T>): Boolean;
begin
  Result := IEnumerable<T>(this).Contains(value, IEqualityComparer<T>(PPointer(@comparer)^));
end;

procedure TEnumerableBase<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  enumerator: IEnumerator<T>;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(values[index])
    else
    {$IFEND}
    values[index] := enumerator.Current;
    Inc(index);
  end;
end;

function TEnumerableBase<T>.ElementAt(index: Integer): T;
begin
  if not IEnumerable<T>(this).TryGetElementAt(Result, index) then
    RaiseHelper.ArgumentOutOfRange_Index;
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
begin
  IEnumerable<T>(this).TryGetElementAt(Result, index);
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer;
  const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetElementAt(Result, index) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.EqualsTo(const values: array of T): Boolean;
label
  ExitFalse;
var
  enumerator: IEnumerator<T>;
  comparer: Pointer;
  i: Integer;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  i := 0;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  comparer := _LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T));
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      if i > High(values) then
        goto ExitFalse;
      IEnumeratorInternal(enumerator).GetCurrent(item);
      if not IEqualityComparer<T>(comparer).Equals(item, values[i]) then
        goto ExitFalse;
    end
    else
    {$ENDIF}
    if (i > High(values)) or not IEqualityComparer<T>(comparer).Equals(enumerator.Current, values[i]) then
      goto ExitFalse;
    Inc(i);
  end;
  Exit(i > High(values));
ExitFalse:
  Result := False;
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>): Boolean;
var
  comparer: Pointer;
begin
  Result := IInterface(this) = values;
  if not Result then
  begin
    comparer := _LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T));
    Result := IEnumerable<T>(this).EqualsTo(values, IEqualityComparer<T>(comparer));
  end;
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
label
  ExitFalse;
var
  e1, e2: IEnumerator<T>;
  {$IFDEF RSP31615}
  item1, item2: T;
  {$ENDIF}
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  e1 := IEnumerable<T>(this).GetEnumerator;
  e2 := values.GetEnumerator;

  while e1.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      if not e2.MoveNext then
        goto ExitFalse;

      IEnumeratorInternal(e1).GetCurrent(item1);
      IEnumeratorInternal(e2).GetCurrent(item2);
      if not comparer.Equals(item1, item2) then
        goto ExitFalse;
    end
    else
    {$ENDIF}
    if not e2.MoveNext or not comparer.Equals(e1.Current, e2.Current) then
      goto ExitFalse;
  if not e2.MoveNext then
    Exit(True);
ExitFalse:
  Result := False;
end;

function TEnumerableBase<T>.First: T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result) then
    RaiseHelper.NoElements;
end;

function TEnumerableBase<T>.First(const predicate: Predicate<T>): T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result, predicate) then
    RaiseHelper.NoMatch;
end;

function TEnumerableBase<T>.FirstOrDefault: T;
begin
  IEnumerable<T>(this).TryGetFirst(Result);
end;

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: Predicate<T>): T;
begin
  IEnumerable<T>(this).TryGetFirst(Result, predicate);
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetFirst(Result, predicate) then
    Result := defaultValue;
end;

procedure TEnumerableBase<T>.ForEach(const action: Action<T>);
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(action) then RaiseHelper.ArgumentNil(ExceptionArgument.action);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      action(item);
    end
    else
    {$ENDIF}
    action(enumerator.Current);
end;

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerableBase<T>.Last: T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result) then
    RaiseHelper.NoElements;
end;

function TEnumerableBase<T>.Last(const predicate: Predicate<T>): T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result, predicate) then
    RaiseHelper.NoMatch;
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  IEnumerable<T>(this).TryGetLast(Result);
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: Predicate<T>): T;
begin
  IEnumerable<T>(this).TryGetLast(Result, predicate);
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
begin
  if not IEnumerable<T>(this).TryGetLast(Result, predicate) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.Max: T;
begin
  Result := Max(fComparer);
end;

function TEnumerableBase<T>.Max(const selector: Func<T, Integer>): Integer;
var
  enumerator: IEnumerator<T>;
  value: Integer;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    Result := selector(item);
  end
  else
  {$ENDIF}
  Result := selector(enumerator.Current);
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      value := selector(item);
    end
    else
    {$ENDIF}
    value := selector(enumerator.Current);
    if value > Result then
      Result := value;
  end;
end;

function TEnumerableBase<T>.Max(const comparer: IComparer<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if comparer.Compare(item, Result) > 0 then
      Result := item;
  end;
end;

function TEnumerableBase<T>.Max(const comparer: TComparison<T>): T;
begin
  Result := Max(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Min: T;
begin
  Result := Min(fComparer);
end;

function TEnumerableBase<T>.Min(const selector: Func<T, Integer>): Integer;
var
  enumerator: IEnumerator<T>;
  value: Integer;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
  begin
    IEnumeratorInternal(enumerator).GetCurrent(item);
    Result := selector(item);
  end
  else
  {$ENDIF}
  Result := selector(enumerator.Current);
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      value := selector(item);
    end
    else
    {$ENDIF}
    value := selector(enumerator.Current);
    if value < Result then
      Result := value;
  end;
end;

function TEnumerableBase<T>.Min(const comparer: IComparer<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if comparer.Compare(item, Result) < 0 then
      Result := item;
  end;
end;

function TEnumerableBase<T>.Min(const comparer: TComparison<T>): T;
begin
  Result := IEnumerable<T>(this).Min(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Memoize: IEnumerable<T>;
begin
  if (GetInterfaceEntry(ICollectionOfTGuid) <> nil)
    or (GetInterfaceEntry(IReadOnlyCollectionOfTGuid) <> nil) then
    Result := IEnumerable<T>(this)
  else
    Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
      0, 0, nil, TIteratorKind.Memoize);
end;

function TEnumerableBase<T>.Ordered: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, Pointer(fComparer), TIteratorKind.Ordered);
end;

function TEnumerableBase<T>.Ordered(const comparer: IComparer<T>): IEnumerable<T>;
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, Pointer(comparer), TIteratorKind.Ordered);
end;

function TEnumerableBase<T>.Ordered(const comparer: TComparison<T>): IEnumerable<T>;
begin
  Result := IEnumerable<T>(this).Ordered(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if IID = IEnumerableGuid then
    Result := TEnumerableWrapper.Create(IEnumerable(this), obj, TIteratorBlock<T>.DoGetCurrent)
  else
    Result := inherited QueryInterface(IID, obj);
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, nil, TIteratorKind.Reversed);
end;

function TEnumerableBase<T>.Shuffled: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, nil, TIteratorKind.Shuffled);
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  if enumerator.MoveNext then
    RaiseHelper.MoreThanOneElement;
end;

function TEnumerableBase<T>.Single(const predicate: Predicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  found: Boolean;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  found := False;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while True do
  begin
    if not enumerator.MoveNext then Break;
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if predicate(item) then
    begin
      found := not found;
      if found then
        Result := item
      else
        RaiseHelper.MoreThanOneMatch;
    end;
  end;
  if not found then
    RaiseHelper.NoMatch;
end;

function TEnumerableBase<T>.SingleOrDefault: T;
var
  defaultValue: T;
begin
  defaultValue := Default(T);
  Result := IEnumerable<T>(this).SingleOrDefault(defaultValue);
end;

function TEnumerableBase<T>.SingleOrDefault(const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    Exit(defaultValue);
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(enumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := enumerator.Current;
  if enumerator.MoveNext then
    RaiseHelper.MoreThanOneElement;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: Predicate<T>): T;
var
  defaultValue: T;
begin
  defaultValue := Default(T);
  Result := IEnumerable<T>(this).SingleOrDefault(predicate, defaultValue);
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: Predicate<T>; const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  found: Boolean;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  found := False;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while True do
  begin
    if not enumerator.MoveNext then Break;
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    if predicate(item) then
    begin
      found := not found;
      if found then
        Result := item
      else
        RaiseHelper.MoreThanOneMatch;
    end;
  end;
  if not found then
    Result := defaultValue;
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>;
var
  maxCount: Integer;
begin
  if count <= 0 then
    Result := IEnumerable<T>(this)
  else
  begin
    maxCount := IEnumerable<T>(this).GetCountFast;
    if maxCount >= 0 then
      maxCount := MaxInt - count;
    Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
      count, maxCount, nil, TIteratorKind.Partition);
  end;
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, PPointer(@predicate)^, TIteratorKind.SkipWhile);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, PPointer(@predicate)^, TIteratorKind.SkipWhileIndex);
end;

function TEnumerableBase<T>.Sum: T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  Result := Default(T);
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    case TType.Kind<T> of
      tkInteger: PInteger(@Result)^ := PInteger(@Result)^ + PInteger(@item)^;
      tkInt64: PInt64(@Result)^ := PInt64(@Result)^ + PInt64(@item)^;
      tkFloat:
      case GetTypeData(TypeInfo(T)).FloatType of
        ftSingle: PSingle(@Result)^ := PSingle(@Result)^ + PSingle(@item)^;
        ftDouble: PDouble(@Result)^ := PDouble(@Result)^ + PDouble(@item)^;
      end;
    end;
  end;
end;

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>;
begin
  if count < 0 then
    count := 0;
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, count, nil, TIteratorKind.Partition);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, PPointer(@predicate)^, TIteratorKind.TakeWhile);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, PPointer(@predicate)^, TIteratorKind.TakeWhileIndex);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
var
  enumerator: IEnumerator<T>;
  count, capacity: Integer;
begin
  count := 0;
  capacity := 0;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    if count >= capacity then
    begin
      capacity := GrowCapacity(capacity);
      SetLength(Result, capacity);
    end;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(enumerator).GetCurrent(Result[count])
    else
    {$IFEND}
    Result[count] := enumerator.Current;
    Inc(count);
  end;
  SetLength(Result, count);
end;

function TEnumerableBase<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  if index >= 0 then
  begin
    enumerator := IEnumerable<T>(this).GetEnumerator;
    while True do
    begin
      if not enumerator.MoveNext then
        Break;
      Dec(index);
      if index >= 0 then
        Continue;

      value := enumerator.Current;
      Exit(True);
    end;
  end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetFirst(var value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  if enumerator.MoveNext then
  begin
    value := enumerator.Current;
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetFirst(var value: T; const predicate: Predicate<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if predicate(item) then
    begin
      value := item;
      Exit(True);
    end;
  end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetLast(var value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  if enumerator.MoveNext then
  begin
    repeat
      value := enumerator.Current;
    until not enumerator.MoveNext;
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetLast(var value: T; const predicate: Predicate<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := False;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if predicate(item) then
    begin
      value := item;
      Result := True;
    end;
  end;
  if not Result then
    value := Default(T);
end;

function TEnumerableBase<T>.TryGetSingle(var value: T): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  if enumerator.MoveNext then
  begin
    item := enumerator.Current;
    if not enumerator.MoveNext then
    begin
      value := item;
      Exit(True);
    end;
  end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetSingle(var value: T;
  const predicate: Predicate<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := False;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;

    if predicate(item) then
    begin
      if Result then
      begin
        value := Default(T);
        Exit(False);
      end;
      value := item;
      Result := True;
    end;
  end;
end;

function TEnumerableBase<T>.Where(const predicate: Predicate<T>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, PPointer(@predicate)^, TIteratorKind.Where);
end;

function TEnumerableBase<T>.Where(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, 0, PPointer(@predicate)^, TIteratorKind.WhereIndex);
end;

{$ENDREGION}


{$REGION 'TEnumerableWrapper'}

class function TEnumerableWrapper.Create(const source: IEnumerable; out obj;
  getCurrent: TGetCurrentFunc): HResult;
var
  wrapper: TEnumerableWrapper;
begin
  wrapper := TEnumerableWrapper.Create;
  wrapper.fSource := source;
  wrapper.fGetCurrent := getCurrent;
  IEnumerable(obj) := wrapper;
  Result := S_OK;
end;

function TEnumerableWrapper.AsObject: TObject;
begin
  Result := fSource.AsObject;
end;

function TEnumerableWrapper.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TEnumerableWrapper.GetElementType: PTypeInfo;
begin
  Result := fSource.ElementType;
end;

function TEnumerableWrapper.GetEnumerator: IEnumerator;
begin
  Result := TEnumerator.Create(fSource.GetEnumerator, fSource.ElementType, fGetCurrent);
end;

function TEnumerableWrapper.GetIsEmpty: Boolean;
begin
  Result := fSource.IsEmpty;
end;

function TEnumerableWrapper.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  Result := inherited QueryInterface(IID, obj);
  if Result <> S_OK then
    Result := fSource.QueryInterface(IID, obj);
end;

function TEnumerableWrapper.GetCountFast: Integer;
begin
  Result := fSource.GetCountFast;
end;

{$ENDREGION}


{$REGION 'TEnumerableWrapper.TEnumerator'}

constructor TEnumerableWrapper.TEnumerator.Create(const source: IEnumerator;
  elementType: PTypeInfo; getCurrent: TGetCurrentFunc);
begin
  fSource := source;
  fElementType := elementType;
  fGetCurrent := getCurrent;
end;

function TEnumerableWrapper.TEnumerator.GetCurrent: Spring.TValue;
begin
  Result := fGetCurrent(fSource, fElementType);
end;

function TEnumerableWrapper.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper'}

class function TCollectionWrapper.Create(const source: IEnumerable; out obj;
  getCurrent: TEnumerableWrapper.TGetCurrentFunc; add: TAddFunc): HRESULT;
var
  wrapper: TCollectionWrapper;
begin
  wrapper := TCollectionWrapper.Create;
  wrapper.fSource := source;
  wrapper.fGetCurrent := getCurrent;
  wrapper.fAdd := add;
  ICollection(obj) := wrapper;
  Result := S_OK;
end;

function TCollectionWrapper.Add(const item: Spring.TValue): Boolean;
begin
  Result := fAdd(fSource, item);
end;

procedure TCollectionWrapper.Clear;
begin
  // Clear does not have any generic parameter so we can hardcast fSource
  // which is an ICollection<T> to access it
  ICollection<Integer>(fSource).Clear;
end;

{$ENDREGION}


{$REGION 'TIterator<T>'}

procedure TIterator<T>.AfterConstruction;
var
  method: function (var current: T): Boolean of object;
begin
  inherited AfterConstruction;
  fState := STATE_INITIAL;
  fThreadId := GetCurrentThreadId;
  method := TryMoveNext;
  fTryMoveNext := TMethod(method).Code;
end;

procedure TIterator<T>.Dispose;
begin
end;

function TIterator<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TIterator<T>.GetEnumerator: IEnumerator<T>;
var
  enumerator: TIterator<T>;
begin
  if (fState = STATE_INITIAL) and (fThreadId = GetCurrentThreadId) then
    enumerator := Self
  else
    enumerator := Clone;
  enumerator.fState := STATE_ENUMERATOR;
  Result := enumerator;
end;

function TIterator<T>.MoveNext: Boolean;
label
  _STATE_RUNNING;
begin
  case fState of
    STATE_RUNNING:
    _STATE_RUNNING:
    begin
      if fTryMoveNext(Self, fCurrent) then
        Exit(True);

      Dispose;
      fCurrent := Default(T);
      fState := STATE_FINISHED;
    end;
    STATE_ENUMERATOR:
    begin
      Start;
      fState := STATE_RUNNING;
      goto _STATE_RUNNING;
    end;
  end;
  Result := False;
end;

procedure TIterator<T>.Start;
begin
end;

{$ENDREGION}


{$REGION 'TSourceIterator<T>'}

constructor TSourceIterator<T>.Create(const source: IEnumerable<T>);
begin
  AssignComparer(fComparer, source);
  fSource := source;
end;

function TSourceIterator<T>.GetElementType: PTypeInfo;
begin
  Result := fSource.ElementType;
end;

{$ENDREGION}


{$REGION 'TCollectionBase<T>'}

procedure TCollectionBase<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
  fOnChanged.OnChanged := EventChanged;
  UpdateNotify(Self, TCollectionBase<T>, fOnChanged);
end;

procedure TCollectionBase<T>.BeforeDestruction;
begin
  fOnChanged.Free;
  inherited BeforeDestruction;
end;

procedure TCollectionBase<T>.EventChanged(Sender: TObject);
begin
  UpdateNotify(Self, TCollectionBase<T>, fOnChanged);
end;

procedure TCollectionBase<T>.DoNotify(const item: T;
  action: TCollectionChangedAction);
begin
  if Assigned(Notify) then
    Notify(Self, item, action);
end;

function TCollectionBase<T>.Add(const item: T): Boolean;
begin
  // only for usage with implementing IList<T>
  IList<T>(this).Add(item);
  Result := True;
end;

procedure TCollectionBase<T>.AddRange(const values: array of T);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    ICollection<T>(this).Add(values[i]);
end;

procedure TCollectionBase<T>.AddRange(const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      ICollection<T>(this).Add(item);
    end
    else
    {$ENDIF}
    ICollection<T>(this).Add(enumerator.Current);
end;

procedure TCollectionBase<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

function TCollectionBase<T>.ExtractAll(const match: Predicate<T>): TArray<T>;
begin
  Result := IEnumerable<T>(this).Where(match).ToArray;
  ExtractRange(Result);
end;

procedure TCollectionBase<T>.ExtractRange(const values: array of T);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    ICollection<T>(this).Extract(values[i]);
end;

procedure TCollectionBase<T>.ExtractRange(const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      ICollection<T>(this).Extract(item);
    end
    else
    {$ENDIF}
    ICollection<T>(this).Extract(enumerator.Current);
end;

function TCollectionBase<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TCollectionBase<T>.MoveTo(const collection: ICollection<T>): Integer;
begin
  Result := ICollection<T>(this).MoveTo(collection, nil);
end;

function TCollectionBase<T>.MoveTo(const collection: ICollection<T>;
  const match: Predicate<T>): Integer;
var
  values: TArray<T>;
  i: Integer;
begin
  if not Assigned(collection) then RaiseHelper.ArgumentNil(ExceptionArgument.collection);

  Result := 0;
  values := IEnumerable<T>(this).ToArray;
  for i := 0 to DynArrayHigh(values) do
    if not Assigned(match) or match(values[i]) then
    begin
      ICollection<T>(this).Extract(values[i]);
      collection.Add(values[i]);
      Inc(Result);
    end;
end;

function TCollectionBase<T>.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if IID = ICollectionGuid then
    Result := TCollectionWrapper.Create(IEnumerable(this), obj, TIteratorBlock<T>.DoGetCurrent, TIteratorBlock<T>.DoAdd)
  else
    Result := inherited QueryInterface(IID, obj);
end;

function TCollectionBase<T>.RemoveAll(const match: Predicate<T>): Integer;
begin
  Result := RemoveRange(IEnumerable<T>(this).Where(match).ToArray);
end;

function TCollectionBase<T>.RemoveRange(const values: array of T): Integer;
var
  this: Pointer;
  i: Integer;
begin
  this := Self.this;
  Result := 0;
  for i := 0 to High(values) do
    Inc(Result, Byte(ICollection<T>(this).Remove(values[i])));
end;

function TCollectionBase<T>.RemoveRange(const values: IEnumerable<T>): Integer;
var
  enumerator: IEnumerator<T>;
  count: Integer;
  {$IFDEF RSP31615}
  item: T;
  {$ENDIF}
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  count := 0;
  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
    {$IFDEF RSP31615}
    if IsManagedType(T) then
    begin
      IEnumeratorInternal(enumerator).GetCurrent(item);
      Inc(Result, Byte(ICollection<T>(this).Remove(item)));
    end
    else
    {$ENDIF}
    Inc(Result, Byte(ICollection<T>(this).Remove(enumerator.Current)));
  Result := count;
end;

procedure TCollectionBase<T>.Reset;
var
  defaultValue: T;
begin
  if Assigned(Notify) then
  begin
    defaultValue := Default(T);
    Notify(Self, defaultValue, caReset);
  end;
end;

{$ENDREGION}


{$REGION 'THashTableEnumerator'}

constructor THashTableEnumerator.Create(const source: TRefCountedObject;
  hashTable: PHashTable);
begin
  fSource := source;
  fSource._AddRef;
  fHashTable := hashTable;
  fVersion := fHashTable.Version;
end;

procedure THashTableEnumerator.BeforeDestruction;
begin
  fSource._Release;
end;

function THashTableEnumerator.MoveNext: Boolean;
begin
  if fVersion = fHashTable.Version then
  begin
    while True do
    begin
      if fIndex >= fHashTable.ItemCount then
        Break;

      fItem := fHashTable.Items + fIndex * fHashTable.ItemSize;
      Inc(fIndex);
      if PInteger(fItem)^ >= 0 then
        Exit(True);
    end;
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TInnerCollection<T>'}

class function TInnerCollection<T>.Create(const source: TRefCountedObject;
  hashTable: PHashTable; const comparer: IEqualityComparer<T>;
  elementType: PTypeInfo; offset: Integer): TInnerCollection<T>;
begin
  Result := TInnerCollection<T>(TInnerCollection<T>.NewInstance);
  Result.fSource := source;
  Result.fHashTable := hashTable;
  Result.fElementType := elementType;
  Result.fComparer := comparer;
  Result.fOffset := KeyOffset + offset;
  Result.AfterConstruction;
end;

function TInnerCollection<T>.Contains(const value: T): Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize, targetIndex, offset: Integer;
  comparer: Pointer;
begin
  if fOffset = KeyOffset then // means this is for the key
  begin
    item := IHashTable<T>(fHashTable).Find(value);
    Result := Assigned(item);
  end
  else
  begin
    offset := fOffset;
    hashTable := fHashTable;
    item := hashTable.Items;
    itemCount := hashTable.ItemCount;
    itemSize := hashTable.ItemSize;
    comparer := Pointer(fComparer);
    while itemCount > 0 do
    begin
      if PInteger(item)^ >= 0 then
        if IEqualityComparer<T>(comparer).Equals(PT(item + offset)^, value) then
          Exit(True);
      Inc(item, itemSize);
      Dec(itemCount);
    end;
    Result := False;
  end;
end;

function TInnerCollection<T>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function TInnerCollection<T>.GetCountFast: Integer;
begin
  Result := fHashTable.Count;
end;

function TInnerCollection<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TInnerCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self.fSource;
    fSource := Self;
    fVersion := Self.fHashTable.Version;
  end;
end;

function TInnerCollection<T>.ToArray: TArray<T>;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize, targetIndex, offset: Integer;
begin
  offset := fOffset;
  hashTable := fHashTable;
  SetLength(Result, hashTable.Count);
  item := hashTable.Items;
  itemCount := hashTable.ItemCount;
  itemSize := hashTable.ItemSize;
  targetIndex := 0;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
    begin
      Result[targetIndex] := PT(item + offset)^;
      Inc(targetIndex);
    end;
    Inc(item, itemSize);
    Dec(itemCount);
  end;
end;

function TInnerCollection<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  if Cardinal(index) < Cardinal(fHashTable.Count) then
  begin
    fHashTable.EnsureCompact;
    value := PT(PByte(fHashTable.Items) + fHashTable.ItemSize * index + fOffset)^;
    Result := True;
  end
  else
    Result := False;
end;

function TInnerCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TInnerCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TInnerCollection<T>.TEnumerator'}

function TInnerCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TInnerCollection<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  offset: Integer;
begin
  offset := fSource.fOffset;
  hashTable := fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if fIndex >= hashTable.ItemCount then
        Break;

      item := @hashTable.Items[fIndex * hashTable.ItemSize];
      Inc(fIndex);
      if PInteger(item)^ >= 0 then
      begin
        fCurrent := PT(item + offset)^;
        Exit(True);
      end;
    until False;
    fCurrent := Default(T);
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TCircularArrayBuffer<T>'}

constructor TCircularArrayBuffer<T>.Create(capacity: Integer; ownsObjects: Boolean);
begin
  SetCapacity(capacity);
  SetOwnsObjects(ownsObjects);
end;

procedure TCircularArrayBuffer<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

procedure TCircularArrayBuffer<T>.BeforeDestruction;
begin
  Clear;
  fOnChanged.Free;
end;

procedure TCircularArrayBuffer<T>.Clear;
var
  i: Integer;
begin
  for i := Count downto 1 do
    DeleteFromHead(caRemoved);
end;

function TCircularArrayBuffer<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TCircularArrayBuffer<T>.GetCount: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TCircularArrayBuffer<T>.GetCountFast: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TCircularArrayBuffer<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fCount := Self.Count;
    fVersion := Self.fVersion;
  end;
end;

function TCircularArrayBuffer<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

function TCircularArrayBuffer<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

function TCircularArrayBuffer<T>.GetTail: Integer;
var
  index: Integer;
begin
  index := fTail;
  if index = 0 then
    index := fCapacity;
  Result := index - 1;
end;

procedure TCircularArrayBuffer<T>.AddToHead(const item: T);
var
  index: Integer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);

  index := fHead;
  if index = 0 then
    index := fCapacity;
  Dec(index);
  fItems[index] := item;
  fHead := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, caAdded);
end;

procedure TCircularArrayBuffer<T>.AddToTail(const item: T);
var
  index: Integer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);

  index := fTail;
  fItems[index] := item;
  Inc(index);
  if index = fCapacity then
    index := 0;
  fTail := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, caAdded);
end;

procedure TCircularArrayBuffer<T>.DeleteFromHead(action: TCollectionChangedAction);
var
  index: Integer;
  item: PT;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  index := fHead;
  item := @fItems[index];
  Inc(index);
  if index = fCapacity then
    index := 0;
  fHead := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item^, action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(item^);
  item^ := Default(T);
end;

procedure TCircularArrayBuffer<T>.DeleteFromTail(action: TCollectionChangedAction);
var
  index: Integer;
  item: PT;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  index := fTail;
  if index = 0 then
    index := fCapacity;
  Dec(index);
  item := @fItems[index];
  fTail := index;

  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item^, action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(item^);
  item^ := Default(T);
end;

function TCircularArrayBuffer<T>.First: T;
begin
  if Count > 0 then
    Result := fItems[fHead]
  else
    RaiseHelper.NoElements;
end;

function TCircularArrayBuffer<T>.FirstOrDefault: T;
begin
  if Count > 0 then
    Result := fItems[fHead]
  else
    Result := Default(T);
end;

function TCircularArrayBuffer<T>.Single: T;
begin
  case Count of
    0: RaiseHelper.NoElements;
    1: Result := fItems[fHead];
  else
    RaiseHelper.MoreThanOneElement;
  end;
end;

function TCircularArrayBuffer<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := fItems[fHead];
  else
    RaiseHelper.MoreThanOneElement;
  end;
end;

procedure TCircularArrayBuffer<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TCircularArrayBuffer<T>.TryGetFirst(var value: T): Boolean;
begin
  if Count > 0 then
  begin
    value := fItems[fHead];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TCircularArrayBuffer<T>.TryGetLast(var value: T): Boolean;
begin
  if Count > 0 then
  begin
    value := fItems[Tail];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

procedure TCircularArrayBuffer<T>.SetCapacity(value: Integer);
var
  itemCount, oldCapacity, offset: Integer;
begin
  itemCount := Count;
  if value < itemCount then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_Capacity);

  if itemCount = 0 then
  begin
    fHead := 0;
    fTail := 0;
    fCapacity := value;
    SetLength(fItems, value);
    Exit;
  end;

  oldCapacity := DynArrayLength(fItems);
  offset := value - oldCapacity;
  if offset = 0 then
    Exit;

  if offset > 0 then
  begin
    fCapacity := value;
    SetLength(fItems, value);
  end;
  if fTail <= fHead then
  begin
    if ItemType.HasWeakRef then
      MoveManaged(@fItems[fHead], @fItems[fHead + offset], TypeInfo(T), oldCapacity - fHead)
    else
      System.Move(fItems[fHead], fItems[fHead + offset], SizeOf(T) * (oldCapacity - fHead));
    if offset > 0 then
    begin
      if ItemType.HasWeakRef then
        System.Finalize(fItems[fHead], offset);
      System.FillChar(fItems[fHead], SizeOf(T) * offset, 0);
    end
    else
    begin
      if ItemType.HasWeakRef then
        System.Finalize(fItems[itemCount], -offset);
      System.FillChar(fItems[itemCount], SizeOf(T) * -offset, 0)
    end;
    Inc(fHead, offset);
  end
  else
  begin
    if fHead + itemCount > value then
    begin
      if ItemType.HasWeakRef then
      begin
        MoveManaged(@fItems[fHead], @fItems[0], TypeInfo(T), itemCount);
        System.Finalize(fItems[itemCount], fHead);
      end
      else
        System.Move(fItems[fHead], fItems[0], SizeOf(T) * itemCount);
      System.FillChar(fItems[itemCount], SizeOf(T) * fHead, 0);
      fHead := 0;
    end;
    fTail := itemCount;
  end;
  if offset < 0 then
  begin
    fCapacity := value;
    SetLength(fItems, value);
  end;
end;

procedure TCircularArrayBuffer<T>.SetOwnsObjects(value: Boolean);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) = tkClass then
  {$ELSE}
  if TType.Kind<T> = tkClass then
  {$ENDIF}
    fCount := (fCount and CountMask) or (Ord(value) shl OwnsObjectsBitIndex);
end;

{$ENDREGION}


{$REGION 'TCircularArrayBuffer<T>.TEnumerator'}

function TCircularArrayBuffer<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TCircularArrayBuffer<T>.TEnumerator.MoveNext: Boolean;
var
  source: TCircularArrayBuffer<T>;
  index: Integer;
begin
  source := fSource;
  if fVersion = source.fVersion then
  begin
    index := fIndex;
    if index < fCount then
    begin
      Inc(index, source.fHead);
      if index >= source.fCapacity then
        Dec(index, source.fCapacity);
      fCurrent := source.fItems[index];
      Inc(fIndex);
      Exit(True);
    end;
    fCurrent := Default(T);
    Exit(False);
  end;
  Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TMapBase<TKey, TValue>'}

procedure TMapBase<TKey, TValue>.AfterConstruction;
begin
  TPairComparer.Create(@fComparer,
    @TKeyValuePairComparer.Comparer_Vtable,
    @TKeyValuePairComparer.Compare,
    GetKeyType, GetValueType);
  inherited AfterConstruction;
  fOnKeyChanged := TCollectionChangedEventImpl<TKey>.Create;
  fOnValueChanged := TCollectionChangedEventImpl<TValue>.Create;
end;

procedure TMapBase<TKey, TValue>.BeforeDestruction;
begin
  fOnValueChanged.Free;
  fOnKeyChanged.Free;
  inherited BeforeDestruction;
end;

procedure TMapBase<TKey, TValue>.DoNotify(const key: TKey; const value: TValue;
  action: TCollectionChangedAction);
var
  pair: TKeyValuePair;
begin
  pair.Key := key;
  pair.Value := value;
  Notify(Self, pair, action);
end;

function TMapBase<TKey, TValue>.Add(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, TValue>(this).TryAdd(item.Key, item.Value);
end;

procedure TMapBase<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  if not IMap<TKey, TValue>(this).TryAdd(key, value) then
    RaiseHelper.DuplicateKey;
end;

function TMapBase<TKey, TValue>.Contains(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, TValue>(this).Contains(item.Key, item.Value);
end;

function TMapBase<TKey, TValue>.Extract(const item: TKeyValuePair): TKeyValuePair;
begin
  Result := IMap<TKey, TValue>(this).Extract(item.Key, item.Value);
end;

function TMapBase<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := TypeInfo(TKey);
end;

function TMapBase<TKey, TValue>.GetOnKeyChanged: ICollectionChangedEvent<TKey>;
begin
  Result := fOnKeyChanged;
end;

function TMapBase<TKey, TValue>.GetOnValueChanged: ICollectionChangedEvent<TValue>;
begin
  Result := fOnValueChanged;
end;

function TMapBase<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := TypeInfo(TValue);
end;

function TMapBase<TKey, TValue>.Remove(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, TValue>(this).Remove(item.Key, item.Value);
end;

function TMapBase<TKey, TValue>.RemoveRange(const keys: array of TKey): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(keys) do
    Inc(Result, Integer(IMap<TKey, TValue>(this).Remove(keys[i])));
end;

function TMapBase<TKey, TValue>.RemoveRange(const keys: IEnumerable<TKey>): Integer;
var
  key: TKey;
begin
  Result := 0;
  for key in keys do
    Inc(Result, Integer(IMap<TKey, TValue>(this).Remove(key)));
end;

{$ENDREGION}


{$REGION 'TIteratorBlock'}

function TIteratorBlock.MoveNext: Boolean;
{$IFDEF CPUX86}
asm
  push eax
  call TIteratorBlock(eax).DoMoveNext
  test al,al
  jz @@ExitFalse
  pop edx
  ret
@@ExitFalse:
  mov eax,[esp]
  mov edx,offset [TIteratorBlock.MoveNextEmpty]
  mov TIteratorBlock(eax).DoMoveNext,edx
  call TIteratorBlock(eax).Methods.Finalize
  pop edx
end;
{$ELSE}
begin
  Result := DoMoveNext(@Self);
  if Result then
    Exit;

  DoMoveNext := @TIteratorBlock.MoveNextEmpty;
  Result := Methods.Finalize(@Self);
end;
{$ENDIF}

function TIteratorBlock._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    Parent._Release;
    Methods.Finalize(@Self);
    FreeMem(@Self);
  end;
end;

function TIteratorBlock.GetEnumerator: Boolean;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}
  DoMoveNext := Methods.MoveNext;
  Result := DoMoveNext(@Self);
end;

function TIteratorBlock.GetEnumeratorAndSkip: Boolean;
begin
{$IFDEF MSWINDOWS}
  IEnumerableInternal(Source).GetEnumerator(Enumerator);
{$ELSE}
  Enumerator := Source.GetEnumerator;
{$ENDIF}
  while (Index > 0) and Enumerator.MoveNext do
    Dec(Index);
  Result := Index = 0;
  if Result then
  begin
    DoMoveNext := Methods.MoveNext;
    Result := DoMoveNext(@Self);
  end;
end;

function TIteratorBlock.GetEnumeratorMemoize: Boolean;
begin
  if PIteratorBase(Predicate).fCount = 0 then
  begin
  {$IFDEF MSWINDOWS}
    IEnumerableInternal(Source).GetEnumerator(IEnumerator(PIteratorBase(Predicate).fPredicate));
  {$ELSE}
    IEnumerator(PIteratorBase(Predicate).fPredicate) := Source.GetEnumerator;
  {$ENDIF}
    PIteratorBase(Predicate).fCount := PIteratorBase(Predicate).fCount or not CountMask;
  end;
  DoMoveNext := Methods.MoveNext;
  Result := DoMoveNext(@Self);
end;

function TIteratorBlock.MoveNextEmpty: Boolean;
begin
  Result := False;
end;

{$ENDREGION}


{$REGION 'TIteratorBlock<T>'}

class function TIteratorBlock<T>.Create(const iterator: TIteratorBase<T>): IEnumerator<T>;
type
  TIteratorBlockT = TIteratorBlock<T>;
  PIteratorBlock = ^TIteratorBlockT;
var
  rec: PIteratorBlock;
begin
  Result := nil;
  rec := AllocMem(SizeOf(TIteratorBlockT));
  Pointer(Result) := rec;

  with rec^ do
  begin
    RefCount := 1;
    Parent := iterator;
    Source := iterator.fSource;
    Predicate := iterator.fPredicate;
    Items := iterator.fItems;
    Index := iterator.fIndex;
    Count := iterator.fCount;
    Kind := iterator.fKind;
    Methods.Finalize := @TIteratorBlock<T>.Finalize;
    if Kind = Memoize then
      Pointer(Predicate) := @iterator.fSource;
  end;
  rec.InitMethods;
  rec.InitVtable;
  iterator._AddRef;
end;

procedure TIteratorBlock<T>.InitVtable;
begin
  if Assigned(Enumerator_Vtable[0]) then
  begin
    Vtable := @Enumerator_Vtable;
    Exit;
  end;

  Enumerator_Vtable[0] := @NopQueryInterface;
  Enumerator_Vtable[1] := @RecAddRef;
  Enumerator_Vtable[2] := @TIteratorBlock._Release;
  Enumerator_Vtable[3] := @TIteratorBlock<T>.GetCurrent;
  Enumerator_Vtable[4] := @TIteratorBlock.MoveNext;
  Vtable := @Enumerator_Vtable;
end;

procedure TIteratorBlock<T>.InitMethods;
begin
  case Kind of
    TIteratorKind.Partition:
      if Assigned(Source) then
        if SupportsIndexedAccess(Source) then
          DoMoveNext := @TIteratorBlock<T>.MoveNextIndexed
        else
        begin
          if Count >= 0 then
            Methods.MoveNext := @TIteratorBlock<T>.MoveNextEnumeratorCounted
          else
            Methods.MoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
          DoMoveNext := @TIteratorBlock.GetEnumeratorAndSkip;
        end
      else
        DoMoveNext := @TIteratorBlock.MoveNextEmpty;
    TIteratorKind.Array:
      DoMoveNext := @TIteratorBlock<T>.MoveNextOrdered;
    TIteratorKind.Concat:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextConcat;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
    TIteratorKind.Memoize:
    begin
      if Count = 0 then
      begin
        Methods.MoveNext := @TIteratorBlock<T>.MoveNextMemoize;
        DoMoveNext := @TIteratorBlock.GetEnumeratorMemoize;
      end
      else
        DoMoveNext := @TIteratorBlock<T>.MoveNextMemoize;
    end;
    TIteratorKind.Ordered:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextOrdered;
      DoMoveNext := @TIteratorBlock<T>.ToArray;
    end;
    TIteratorKind.Reversed:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextReversed;
      DoMoveNext := @TIteratorBlock<T>.ToArray;
    end;
    TIteratorKind.Shuffled:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextOrdered;
      DoMoveNext := @TIteratorBlock<T>.ToArray;
    end;
    TIteratorKind.SkipWhile:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextSkipWhile;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
    TIteratorKind.SkipWhileIndex:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextSkipWhileIndex;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
    TIteratorKind.TakeWhile:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextTakeWhile;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
    TIteratorKind.TakeWhileIndex:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextTakeWhileIndex;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
    TIteratorKind.Where:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextWhere;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
    TIteratorKind.WhereIndex:
    begin
      Methods.MoveNext := @TIteratorBlock<T>.MoveNextWhereIndex;
      DoMoveNext := @TIteratorBlock.GetEnumerator;
    end;
  end;
end;

class function TIteratorBlock<T>.DoGetCurrent(const enumerator: IEnumerator;
  elementType: PTypeInfo): Spring.TValue;
var
  current: T;
begin
  current := IEnumerator<T>(enumerator).Current;
  Spring.TValue.Make(@current, elementType, Result);
end;

function TIteratorBlock<T>.Finalize: Boolean;
begin
  Enumerator := nil;
  Source := nil;
  if Kind <> Memoize then
    Predicate := nil
  else
    Pointer(Predicate) := nil;
  Items := nil;
{$IFDEF DELPHIXE7_UP}
  if IsManagedType(T) then
{$ENDIF}
    Current := Default(T);
  Result := False;
end;

class function TIteratorBlock<T>.DoAdd(const collection: IInterface;
  const value: Spring.TValue): Boolean;
var
  elementType: PTypeInfo;
  item: T;
begin
  elementType := IEnumerable(collection).ElementType;
  if value.TryAsType(elementType, item) then
    Result := ICollection<T>(collection).Add(item)
  else
    Guard.RaiseInvalidTypeCast(value.TypeInfo, elementType);
end;

function TIteratorBlock<T>.GetCurrent: T;
begin
  Result := Current;
end;

function TIteratorBlock<T>.MoveNextConcat: Boolean;
begin
  repeat
    if Enumerator.MoveNext then
    begin
      {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
      if IsManagedType(T) then
        IEnumeratorInternal(Enumerator).GetCurrent(Current)
      else
      {$IFEND}
      Current := Enumerator.Current;
      Exit(True);
    end;

    Result := Assigned(Predicate);
    if not Result then
      Break;

    {$IFDEF MSWINDOWS}
    IEnumerableInternal(Predicate).GetEnumerator(Enumerator);
    {$ELSE}
    Enumerator := IEnumerable<T>(Predicate).GetEnumerator;
    {$ENDIF}
    Predicate := nil;
  until False;
end;

function TIteratorBlock<T>.MoveNextEnumerator: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := True;
  end;
end;

function TIteratorBlock<T>.MoveNextEnumeratorCounted: Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    Result := Enumerator.MoveNext;
    if Result then
    begin
      {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
      if IsManagedType(T) then
        IEnumeratorInternal(Enumerator).GetCurrent(Current)
      else
      {$IFEND}
      Current := Enumerator.Current;
      Dec(Count);
      Result := True;
    end;
  end;
end;

function TIteratorBlock<T>.MoveNextIndexed: Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    Result := Source.TryGetElementAt(Current, Index);
    if Result then
    begin
      Inc(Index);
      Dec(Count);
    end;
  end;
end;

function TIteratorBlock<T>.MoveNextMemoize: Boolean;
var
  iterator: ^TMemoizeIterator<T>;
  count, capacity: NativeInt;
begin
  iterator := Pointer(Predicate);
  count := iterator.Count and CountMask;
  if Index >= count then
  begin
    if iterator.Enumerator = nil then
      Exit(False);

    if not iterator.Enumerator.MoveNext then
    begin
      iterator.Enumerator := nil;
      Exit(False);
    end;

    capacity := DynArrayLength(iterator.Items);
    if count >= capacity then
    begin
      capacity := GrowCapacity(capacity);
      SetLength(iterator.Items, capacity);
    end;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(iterator.Enumerator).GetCurrent(iterator.Items[count])
    else
    {$IFEND}
    iterator.Items[count] := iterator.Enumerator.Current;
    Inc(iterator.Count);
  end;

  Current := iterator.Items[Index];
  Inc(Index);
  Result := True;
end;

function TIteratorBlock<T>.MoveNextOrdered: Boolean;
begin
  if Index < Count then
  begin
    Current := Items[Index];
    Inc(Index);
    Result := True;
  end
  else
    Result := False;
end;

function TIteratorBlock<T>.MoveNextReversed: Boolean;
begin
  if Count > 0 then
  begin
    Dec(Count);
    Current := Items[Count];
    Result := True;
  end
  else
    Result := False;
end;

function TIteratorBlock<T>.MoveNextSkipWhile: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
    if Result then
      Continue;
    DoMoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
    Exit(True);
  until False;
end;

function TIteratorBlock<T>.MoveNextSkipWhileIndex: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Func<T, Integer, Boolean>(Predicate)(Current, Index);
    Inc(Index, Ord(Result));
    if Result then
      Continue;
    DoMoveNext := @TIteratorBlock<T>.MoveNextEnumerator;
    Exit(True);
  until False;
end;

function TIteratorBlock<T>.MoveNextTakeWhile: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
  end;
end;

function TIteratorBlock<T>.MoveNextTakeWhileIndex: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Func<T, Integer, Boolean>(Predicate)(Current, Index);
    Inc(Index);
  end;
end;

function TIteratorBlock<T>.MoveNextWhere: Boolean;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
  until Result;
end;

function TIteratorBlock<T>.MoveNextWhereIndex: Boolean;
var
  i: Integer;
begin
  repeat
    Result := Enumerator.MoveNext;
    if not Result then
      Break;
    {$IF defined(DELPHIXE7_UP) and defined(MSWINDOWS)}
    if IsManagedType(T) then
      IEnumeratorInternal(Enumerator).GetCurrent(Current)
    else
    {$IFEND}
    Current := Enumerator.Current;
    Result := Func<T,Integer,Boolean>(Predicate)(Current, Index);
    Inc(Index);
  until Result;
end;

function TIteratorBlock<T>.ToArray: Boolean;
begin
  Items := Source.ToArray;
  Count := DynArrayLength(Items);
  case Kind of
    TIteratorKind.Ordered:
      TArray.Sort<T>(Items, IComparer<T>(Predicate));
    TIteratorKind.Shuffled:
      TArray.Shuffle<T>(Items, DynArrayHigh(Items));
  end;
  DoMoveNext := Methods.MoveNext;
  Result := DoMoveNext(@Self);
end;

{$ENDREGION}


{$REGION 'TIteratorFields'}

function TIteratorBase.GetCountFast: Integer;
var
  count: Integer;
begin
  case fKind of
    TIteratorKind.Concat:
    begin
      Result := fSource.GetCountFast;
      if Result >= 0 then
      begin
        count := IEnumerable(fPredicate).GetCountFast;
        if count >= 0 then
          {$Q+}
          Inc(Result, count);
          {$IFNDEF OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
      end;
    end;
    TIteratorKind.Ordered, TIteratorKind.Reversed, TIteratorKind.Shuffled:
      Result := fSource.GetCountFast;
    TIteratorKind.Partition:
    begin
      Result := fCount;
      if Result > 0 then
      begin
        Result := fSource.GetCountFast;
        if Result >= 0 then
        begin
          Dec(Result, fIndex);
          if Result < 0 then
            Result := 0;
          if Cardinal(Result) > Cardinal(fCount) then
            Result := fCount;
        end;
      end;
    end;
  else
    Result := -1;
  end;
end;

{$ENDREGION}


{$REGION 'TIteratorBase<T>'}

function TIteratorBase<T>.GetElementType: PTypeInfo;
begin
  if Assigned(fSource) then
    Result := fSource.ElementType
  else
    Result := inherited GetElementType;
end;

function TIteratorBase<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TIteratorBlock<T>.Create(Self);
end;

function TIteratorBase<T>.HasLimit: Boolean;
begin
  Result := fCount <> -1;
end;

function TIteratorBase<T>.Skip(count: Integer): IEnumerable<T>;
var
  minIndex: Integer;
  source: Pointer;
begin
  if fKind = TIteratorKind.Partition then
  begin
    if count < 0 then
      count := 0;
    {$Q-}
    minIndex := fIndex + count;
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    if not HasLimit then
    begin
      if minIndex < 0 then
      begin
        // If we don't know our max count and minIndex can no longer fit in a positive int,
        // then we will need to wrap ourselves in another iterator.
        // This can happen, for example, during e.Skip(MaxInt).Skip(MaxInt).
        source := Pointer(this);
        minIndex := count;
        count := -1;
      end
      else
      begin
        source := Pointer(fSource);
        count := -1;
      end;
    end
    else if count >= fCount then
    begin
      source := nil;
      minIndex := 0;
      count := 0;
    end
    else
    begin
      source := Pointer(fSource);
      count := fCount - count;
    end;

    Result := TEnumerableIterator<T>.Create(IEnumerable<T>(source),
      minIndex, count, nil, TIteratorKind.Partition);
  end
  else
    Result := inherited Skip(count);
end;

function TIteratorBase<T>.Take(count: Integer): IEnumerable<T>;
var
  maxIndex: Integer;
  source: Pointer;
begin
  if fKind = TIteratorKind.Partition then
  begin
    {$Q-}
    maxIndex := fIndex + count - 1;
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    if not HasLimit then
    begin
      if maxIndex < 0 then
      begin
        // If we don't know our max count and maxIndex can no longer fit in a positive int,
        // then we will need to wrap ourselves in another iterator.
        // Note that although maxIndex may be too large, the difference between it and
        // fIterator.Index (which is count - 1) must fit in an int.
        // Example: e.Skip(50).Take(MaxInt).
        source := Pointer(this);
        maxIndex := 0;
      end
      else
      begin
        source := Pointer(fSource);
        maxIndex := fIndex;
      end;
    end
    else if count >= fCount then
    begin
      Result := IEnumerable<T>(this);
      Exit;
    end
    else
    begin
      source := Pointer(fSource);
      maxIndex := fIndex;
    end;

    Result := TEnumerableIterator<T>.Create(IEnumerable<T>(source),
      maxIndex, count, nil, TIteratorKind.Partition);
  end
  else
    Result := inherited Take(count);
end;

function TIteratorBase<T>.PartitionToArray(var values: TArray<T>): Boolean;
var
  count, index, i: Integer;
  source: Pointer;
begin
  Result := SupportsIndexedAccess(fSource);
  if Result then
  begin
    index := fIndex;
    count := fSource.Count - index;
    if count > fCount then
      count := fCount
    else if count < 0 then
      count := 0;
    SetLength(values, count);
    source := Pointer(fSource);
    for i := 0 to count - 1 do
    begin
      IEnumerable<T>(source).TryGetElementAt(values[i], index);
      Inc(index);
    end;
    Result := True;
  end;
end;

function TIteratorBase<T>.ToArray: TArray<T>;
begin
  case fKind of
    TIteratorKind.Partition:
      if PartitionToArray(Result) then Exit;
    TIteratorKind.Array:
    begin
      Result := fItems;
      SetLength(Result, DynArrayLength(Result));
      Exit;
    end;
    TIteratorKind.Ordered:
    begin
      Result := fSource.ToArray;
      TArray.Sort<T>(Result, IComparer<T>(fPredicate));
      Exit;
    end;
    TIteratorKind.Reversed:
    begin
      Result := fSource.ToArray;
      TArray.Reverse<T>(Result);
      Exit;
    end;
    TIteratorKind.Shuffled:
    begin
      Result := fSource.ToArray;
      TArray.Shuffle<T>(Result);
      Exit;
    end;
  end;
  Result := inherited ToArray;
end;

function TIteratorBase<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  if fKind = TIteratorKind.Partition then
    if Cardinal(index) >= Cardinal(fCount) then
    begin
      value := Default(T);
      Result := False;
    end
    else
      Result := fSource.TryGetElementAt(value, fIndex + index)
  else
    Result := inherited TryGetElementAt(value, index);
end;

function TIteratorBase<T>.TryGetFirst(var value: T): Boolean;
begin
  if fKind = TIteratorKind.Partition then
    if fCount = 0 then
    begin
      value := Default(T);
      Result := False;
    end
    else
      Result := fSource.TryGetElementAt(value, fIndex)
  else if fKind = TIteratorKind.Array then
  begin
    if Assigned(fItems) then
    begin
      value := fItems[0];
      Exit(True);
    end;

    value := Default(T);
    Result := False;
  end
  else
    Result := inherited TryGetFirst(value);
end;

function TIteratorBase<T>.TryGetLast(var value: T): Boolean;
var
  intf: IInterface;
  count, lastIndex: Integer;
begin
  if fKind = TIteratorKind.Partition then
  begin
    if fCount <> 0 then
    begin
      if SupportsIndexedAccess(fSource) then
      begin
        count := fSource.Count;
        if fIndex < count then
        begin
          {$Q-}
          lastIndex := fIndex + fCount - 1;
          {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
          if lastIndex > count - 1 then
            lastIndex := count - 1;
          Exit(fSource.TryGetElementAt(value, lastIndex));
        end;
      end
      else
      begin
        intf := fSource.GetEnumerator;
        if (SkipAndCount(IEnumerator(intf), fIndex) = fIndex)
          and IEnumerator(intf).MoveNext then
        begin
          count := fCount;
          repeat
            value := IEnumerator<T>(intf).Current;
            Dec(count);
          until (count = 0) or not IEnumerator(intf).MoveNext;
          Exit(True);
        end;
      end;
    end;

    value := Default(T);
    Result := False;
  end
  else if fKind = TIteratorKind.Array then
  begin
    if Assigned(fItems) then
    begin
      value := fItems[fCount - 1];
      Exit(True);
    end;

    value := Default(T);
    Result := False;
  end
  else
    Result := inherited TryGetLast(value);
end;

{$ENDREGION}


{$REGION 'TEnumerableIterator<T>'}

class function TEnumerableIterator<T>.Create(const source: IEnumerable<T>;
  index, count: Integer; predicate: Pointer; kind: TIteratorKind): IEnumerable<T>;
var
  iterator: TEnumerableIterator<T>;
begin
  iterator := TEnumerableIterator<T>.Create;
  iterator.fSource := source;
  if Assigned(source) then
    AssignComparer(iterator.fComparer, source);
  iterator.fPredicate := IInterface(predicate);
  iterator.fIndex := index;
  iterator.fCount := count;
  iterator.fKind := kind;
  Result := iterator;
end;

function TEnumerableIterator<T>.GetCountFast: Integer;
begin
  Result := PIteratorBase(@fSource).GetCountFast;
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>'}

class function TArrayIterator<T>.Create(const values: TArray<T>): IReadOnlyList<T>;
var
  iterator: TArrayIterator<T>;
begin
  iterator := TArrayIterator<T>.Create;
  iterator.fKind := TIteratorKind.Array;
  iterator.fCount := DynArrayLength(values);
  iterator.fItems := values;
  Result := iterator;
end;

class function TArrayIterator<T>.Create(const values: array of T): IReadOnlyList<T>;
var
  iterator: TArrayIterator<T>;
  count: Integer;
begin
  iterator := TArrayIterator<T>.Create;
  iterator.fKind := TIteratorKind.Array;
  count := Length(values);
  if count > 0 then
  begin
    iterator.fCount := count;
    SetLength(iterator.fItems, count);
    if TType.IsManaged<T> then
      MoveManaged(@values[0], @iterator.fItems[0], TypeInfo(T), count)
    else
      System.Move(values[0], iterator.fItems[0], SizeOf(T) * count);
  end;
  Result := iterator;
end;

procedure TArrayIterator<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  count: Integer;
begin
  count := DynArrayLength(fItems);
  if count > 0 then
    if TType.IsManaged<T> then
      MoveManaged(@fItems[0], @values[index], TypeInfo(T), count)
    else
      System.Move(fItems[0], values[index], SizeOf(T) * count);
end;

function TArrayIterator<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TArrayIterator<T>.GetCountFast: Integer;
begin
  Result := fCount;
end;

function TArrayIterator<T>.GetItem(index: Integer): T;
begin
  CheckIndex(index, fCount);

  Result := fItems[index];
end;

function TArrayIterator<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, fCount);
end;

function TArrayIterator<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, fCount - index);
end;

function TArrayIterator<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  comparer: Pointer;
  i: Integer;
begin
  CheckRange(index, count, fCount);

  comparer := _LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T));
  for i := index to index + count - 1 do
    if IEqualityComparer<T>(comparer).Equals(fItems[i], item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TFoldedArrayIterator<T>'}

class function TFoldedArrayIterator<T>.Create(const values: TArray<T>;
  elementType: PTypeInfo): IReadOnlyList<T>;
var
  iterator: TFoldedArrayIterator<T>;
begin
  iterator := TFoldedArrayIterator<T>.Create;
  iterator.fKind := TIteratorKind.Array;
  iterator.fCount := DynArrayLength(values);
  iterator.fItems := values;
  iterator.fElementType := elementType;
  Result := iterator;
end;

class function TFoldedArrayIterator<T>.Create(values: PPointer; count: Integer;
  elementType: PTypeInfo): IReadOnlyList<T>;
var
  iterator: TFoldedArrayIterator<T>;
begin
  iterator := TFoldedArrayIterator<T>.Create;
  iterator.fKind := TIteratorKind.Array;
  if count > 0 then
  begin
    iterator.fCount := count;
    SetLength(iterator.fItems, count);
    if TType.IsManaged<T> then
      MoveManaged(values, @iterator.fItems[0], TypeInfo(T), count)
    else
      System.Move(values^, iterator.fItems[0], SizeOf(T) * count);
  end;
  Result := iterator;
end;

function TFoldedArrayIterator<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
