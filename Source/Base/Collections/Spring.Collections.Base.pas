{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2020 Spring4D Team                           }
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
  Spring.Collections.HashTable,
  Spring.Events.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  PEnumeratorVtable = ^TEnumeratorVtable;
  TEnumeratorVtable = array[0..4] of Pointer;
  PEnumeratorBlock = ^TEnumeratorBlock;
  TEnumeratorBlock = record
    Vtable: Pointer;
    RefCount: Integer;
    TypeInfo: PTypeInfo;
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
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

  TEnumerableBase = class abstract(TRefCountedObject)
  protected
    this: Pointer;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  public
    class function NewInstance: TObject; override;

    function Any: Boolean; overload;
  end;

  TEnumerableBase<T> = class abstract(TEnumerableBase)
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

    function Where(const predicate: Predicate<T>): IEnumerable<T>;

    function ToArray: TArray<T>;
  end;

  TEnumerableWrapper = class(TRefCountedObject, IInterface, IEnumerable)
  private type
    TGetCurrentFunc = function(const enumerator: IEnumerator; elementType: PTypeInfo): Spring.TValue;
    TEnumerator = class sealed(TRefCountedObject, IEnumerator)
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
    Concat, Ordered, Reversed, Shuffled,
    Skip, SkipWhile, SkipWhileIndex,
    Take, TakeWhile, TakeWhileIndex,
    Where);
  TMoveNextFunc = function(self: Pointer): Boolean;
  TStartFunc = function(self: Pointer): Boolean;
  PIteratorRec = ^TIteratorRec;
  TIteratorRec = record
    MoveNext: TMoveNextFunc;
    Source: IEnumerable;
    Enumerator: IEnumerator;
    TypeInfo: PTypeInfo;
    Start: TStartFunc;

    Predicate: IInterface;
    Items: Pointer;
    Count: Integer;
    Kind: TIteratorKind;

    function Clone: PIteratorRec;
    procedure Finalize;

    function TryGetCount(out count: Integer): Boolean;

    function GetEnumerator: Boolean;
    function GetEnumeratorAndSkip: Boolean;
    procedure GetSecondEnumerator;
  end;

  TIteratorRec<T> = record
    MoveNext: TMoveNextFunc;
    Source: IEnumerable<T>;
    Enumerator: IEnumerator<T>;
    TypeInfo: PTypeInfo;
    Start: TStartFunc;

    Predicate: IInterface;
    Items: TArray<T>;
    Count: Integer;
    Kind: TIteratorKind;
    Current: T;

    function ToArray: Boolean;

    function Concat: Boolean;
    function Ordered: Boolean;
    function Reversed: Boolean;
    function Skip: Boolean;
    function SkipWhile: Boolean;
    function SkipWhileIndex: Boolean;
    function Take: Boolean;
    function TakeWhile: Boolean;
    function TakeWhileIndex: Boolean;
    function Where: Boolean;

    class function GetCurrent(const enumerator: IEnumerator; elementType: PTypeInfo): Spring.TValue; static;
    class function Add(const collection: IInterface; const value: Spring.TValue): Boolean; static;
  end;

  PIterator = ^TIterator;
  TIterator = record
  private type
    TEnumeratorState = (Initial, Started, Finished);
  private
    Vtable: Pointer;
    RefCount: Integer;
    Parent: TRefCountedObject;
    Iterator: PIteratorRec;
    State: TEnumeratorState;
    procedure Start;
  public
    class function Create(enumerator: PPointer; vtable: PEnumeratorVtable;
      parent: TRefCountedObject; iterator: PIteratorRec; getCurrent: Pointer): Pointer; static;
    function _Release: Integer; stdcall;
    function MoveNext: Boolean;
  end;

  TIteratorBase<T> = class abstract(TEnumerableBase<T>)
  private type
    TIteratorRecT = TIteratorRec<T>;
    PIteratorRecT = ^TIteratorRecT;

    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;

      fSource: TRefCountedObject;
      fIterator: PIteratorRecT;
      fState: TIterator.TEnumeratorState;
      function GetCurrent: T;

      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  protected
    fIterator: TIteratorRec<T>;
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const source: IEnumerable<T>;
      count: Integer; predicate: Pointer; kind: TIteratorKind); overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  TEnumerableIterator<T> = class sealed(TIteratorBase<T>, IInterface, IEnumerable<T>)
  private
    function GetCount: Integer;
  end;

  TArrayIterator<T> = class(TIteratorBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    constructor Create(const values: TArray<T>); overload;
    constructor Create(const values: array of T); overload;

  {$REGION 'Implements IEnumerable<T>'}
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyCollection<T>'}
    procedure CopyTo(var values: TArray<T>; index: Integer);
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyList<T>'}
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TObjectArrayIterator = class(TArrayIterator<TObject>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const source: TArray<TObject>; elementType: PTypeInfo);
    constructor CreateFromArray(source: PPointer; count: Integer; elementType: PTypeInfo);
  end;

  TInterfaceArrayIterator = class(TArrayIterator<IInterface>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const source: TArray<IInterface>; elementType: PTypeInfo);
    constructor CreateFromArray(source: PPointer; count: Integer; elementType: PTypeInfo);
  end;

  TIterator<T> = class abstract(TEnumerableBase<T>, IEnumerator<T>)
  private
    fCurrent: T;
    fThreadId: TThreadID;
    fState: Integer;
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
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
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
    TEnumerator = class(THashTableEnumerator, IEnumerator<T>)
    private
      fCurrent: T;
      function GetCurrent: T;
    public
      procedure BeforeDestruction; override;
      function MoveNext: Boolean;
    end;
  {$ENDREGION}
  private
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fElementType: PTypeInfo;
    fComparer: IEqualityComparer<T>;
    fOffset: Integer;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
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
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
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
    function GetIsEmpty: Boolean;
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

    function Extract(const item: TKeyValuePair): TKeyValuePair;

    function Contains(const item: TKeyValuePair): Boolean; overload;
  end;

const
  OwnsObjectsBitIndex = 31;
  OwnsObjectsMask     = 1 shl OwnsObjectsBitIndex;
  CountMask           = not OwnsObjectsMask;

  // use the MSB of the HashCode to note removed items
  RemovedFlag        = Integer($80000000);
  MinCapacity        = 6; // 75% load factor leads to min bucket count of 8
  BucketSentinelFlag = RemovedFlag; // note: the same as RemovedFlag
  EmptyBucket        = -1; // must be negative, note choice of BucketSentinelFlag
  UsedBucket         = -2; // likewise

procedure EnsureEventInstance(var event: TEventBase; var result;
  eventClass: TEventBaseClass; eventChanged: TNotifyEvent);
procedure UpdateNotify(instance: TObject; baseClass: TClass; const event: TEventBase; var code);

implementation

uses
  Math,
  Rtti,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  Spring.Collections.Lists,
  Spring.ResourceStrings;

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
{$IFDEF AUTOREFCOUNT}
      newEvent.__ObjAddRef;
{$ENDIF AUTOREFCOUNT}
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

procedure UpdateNotify(instance: TObject; baseClass: TClass; const event: TEventBase; var code);
const
  ChangedVirtualIndex = 1;
var
  baseAddress, actualAddress: Pointer;
begin
{$POINTERMATH ON}
  baseAddress := PPointer(baseClass)[ChangedVirtualIndex];
  actualAddress := PPointer(instance.ClassType)[ChangedVirtualIndex];
{$POINTERMATH OFF}
  if (Assigned(event) and event.CanInvoke) or (actualAddress <> baseAddress) then
    Pointer(code) := actualAddress
  else
    Pointer(code) := nil;
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


{$REGION 'TEnumerableBase'}

class function TEnumerableBase.NewInstance: TObject;
begin
  Result := inherited NewInstance;

  // child classes must implement IEnumerable<T>
  TEnumerableBase(Result).this := Pointer(PByte(Result) + GetInterfaceEntry(IEnumerableOfTGuid).IOffset);
end;

function TEnumerableBase.Any: Boolean;
begin
  Result := not IEnumerable(this).IsEmpty;
end;

function TEnumerableBase.GetCount: Integer;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := IEnumerable(this).GetEnumerator;
  while enumerator.MoveNext do
    Inc(Result);
end;

function TEnumerableBase.GetIsEmpty: Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := IEnumerable(this).GetEnumerator;
  Result := not enumerator.MoveNext;
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
  item: T;
begin
  if not Assigned(func) then RaiseHelper.ArgumentNil(ExceptionArgument.func);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := enumerator.Current;
  while enumerator.MoveNext do
    Result := func(Result, enumerator.Current);
end;

function TEnumerableBase<T>.All(const predicate: Predicate<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if not predicate(item) then
      Exit(False);
  end;
  Result := True;
end;

function TEnumerableBase<T>.Any(const predicate: Predicate<T>): Boolean;
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
      Exit(True);
  end;
  Result := False;
end;

function TEnumerableBase<T>.Concat(const second: IEnumerable<T>): IEnumerable<T>;
begin
  if not Assigned(second) then RaiseHelper.ArgumentNil(ExceptionArgument.second);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, Pointer(second), TIteratorKind.Concat);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
  comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T)));
  Result := IEnumerable<T>(this).Contains(value, comparer);
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if comparer.Equals(value, item) then
      Exit(True);
  end;
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
var
  enumerator: IEnumerator<T>;
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  i := 0;
  enumerator := IEnumerable<T>(this).GetEnumerator;
  comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T)));
  while enumerator.MoveNext do
  begin
    if (i > High(values)) or not comparer.Equals(enumerator.Current, values[i]) then
      Exit(False);
    Inc(i);
  end;
  Result := i > High(values);
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
  if IInterface(this) = values then
    Result := True
  else
  begin
    comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T)));
    Result := IEnumerable<T>(this).EqualsTo(values, comparer);
  end;
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
var
  e1, e2: IEnumerator<T>;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  e1 := IEnumerable<T>(this).GetEnumerator;
  e2 := values.GetEnumerator;

  while e1.MoveNext do
    if not (e2.MoveNext and comparer.Equals(e1.Current, e2.Current)) then
      Exit(False);
  Result := not e2.MoveNext;
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
  item: T;
begin
  if not Assigned(action) then RaiseHelper.ArgumentNil(ExceptionArgument.action);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    action(item);
  end;
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
  item: Integer;
begin
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := selector(enumerator.Current);
  while enumerator.MoveNext do
  begin
    item := selector(enumerator.Current);
    if item > Result then
      Result := item;
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
  Result := enumerator.Current;
  while enumerator.MoveNext do
  begin
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
  item: Integer;
begin
  if not Assigned(selector) then RaiseHelper.ArgumentNil(ExceptionArgument.selector);

  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
  Result := selector(enumerator.Current);
  while enumerator.MoveNext do
  begin
    item := selector(enumerator.Current);
    if item < Result then
      Result := item;
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
  Result := enumerator.Current;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if comparer.Compare(item, Result) < 0 then
      Result := item;
  end;
end;

function TEnumerableBase<T>.Min(const comparer: TComparison<T>): T;
begin
  Result := IEnumerable<T>(this).Min(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Ordered: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, Pointer(fComparer), TIteratorKind.Ordered);
end;

function TEnumerableBase<T>.Ordered(const comparer: IComparer<T>): IEnumerable<T>;
begin
  if not Assigned(comparer) then RaiseHelper.ArgumentNil(ExceptionArgument.comparer);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, Pointer(comparer), TIteratorKind.Ordered);
end;

function TEnumerableBase<T>.Ordered(const comparer: TComparison<T>): IEnumerable<T>;
begin
  Result := IEnumerable<T>(this).Ordered(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if IID = IEnumerableGuid then
    Result := TEnumerableWrapper.Create(IEnumerable(this), obj, TIteratorRec<T>.GetCurrent)
  else
    Result := inherited QueryInterface(IID, obj);
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, nil, TIteratorKind.Reversed);
end;

function TEnumerableBase<T>.Shuffled: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, nil, TIteratorKind.Shuffled);
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := IEnumerable<T>(this).GetEnumerator;
  if not enumerator.MoveNext then
    RaiseHelper.NoElements;
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
begin
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    count, nil, TIteratorKind.Skip);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, PPointer(@predicate)^, TIteratorKind.SkipWhile);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, PPointer(@predicate)^, TIteratorKind.SkipWhileIndex);
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
  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    count, nil, TIteratorKind.Take);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, PPointer(@predicate)^, TIteratorKind.TakeWhile);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  Result := TEnumerableIterator<T>.Create(IEnumerable<T>(this),
    0, PPointer(@predicate)^, TIteratorKind.TakeWhileIndex);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
var
  enumerator: IEnumerator<T>;
  count, capacity: Integer;
begin
  Result := nil;
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
    0, PPointer(@predicate)^, TIteratorKind.Where);
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
begin
  inherited AfterConstruction;
  fState := STATE_INITIAL;
  fThreadId := GetCurrentThreadId;
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
begin
  case fState of
    STATE_ENUMERATOR,
    STATE_RUNNING:
    begin
      if fState = STATE_ENUMERATOR then
      begin
        Start;
        fState := STATE_RUNNING;
      end;

      if TryMoveNext(fCurrent) then
        Exit(True);

      Dispose;
      fCurrent := Default(T);
      fState := STATE_FINISHED;
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
  fComparer := source.Comparer;
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
  UpdateNotify(Self, TCollectionBase<T>, fOnChanged, fNotify);
end;

procedure TCollectionBase<T>.BeforeDestruction;
begin
  fOnChanged.Free;
end;

procedure TCollectionBase<T>.EventChanged(Sender: TObject);
begin
  UpdateNotify(Self, TCollectionBase<T>, fOnChanged, fNotify);
end;

procedure TCollectionBase<T>.DoNotify(const item: T;
  action: TCollectionChangedAction);
begin
  if Assigned(Notify) then
    Notify(Self, item, action);
end;

procedure TCollectionBase<T>.AddRange(const values: array of T);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    ICollection<T>(this).Add(values[i]);
end;

function TCollectionBase<T>.Add(const item: T): Boolean;
begin
  // only for usage with implementing IList<T>
  IList<T>(this).Add(item);
  Result := True;
end;

procedure TCollectionBase<T>.AddRange(const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    ICollection<T>(this).Add(item);
  end;
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
  item: T;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    ICollection<T>(this).Extract(item);
  end;
end;

function TCollectionBase<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  EnsureEventInstance(TEventBase(fOnChanged), Result, TCollectionChangedEventImpl<T>, EventChanged);
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
    Result := TCollectionWrapper.Create(IEnumerable(this), obj, TIteratorRec<T>.GetCurrent, TIteratorRec<T>.Add)
  else
    Result := inherited QueryInterface(IID, obj);
end;

function TCollectionBase<T>.RemoveAll(const match: Predicate<T>): Integer;
begin
  Result := RemoveRange(IEnumerable<T>(this).Where(match).ToArray);
end;

function TCollectionBase<T>.RemoveRange(const values: array of T): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(values) do
    if ICollection<T>(this).Remove(values[i]) then
      Inc(Result);
end;

function TCollectionBase<T>.RemoveRange(const values: IEnumerable<T>): Integer;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  Result := 0;
  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    if ICollection<T>(this).Remove(item) then
      Inc(Result);
  end;
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
  Result.fOffset := THashTable.KeyOffset + offset;
  Result.AfterConstruction;
end;

function TInnerCollection<T>.Contains(const value: T): Boolean;
var
  entry: THashTableEntry;
begin
  if fOffset = THashTable.KeyOffset then // means this is for the key
  begin
    entry.HashCode := fComparer.GetHashCode(value);
    Result := fHashTable.Find(value, entry);
  end
  else
    Result := inherited Contains(value, fComparer);
end;

function TInnerCollection<T>.GetCount: Integer;
begin
  Result := fHashTable.Count;
end;

function TInnerCollection<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TInnerCollection<T>.GetEnumerator: IEnumerator<T>;
var
  enumerator: TEnumerator;
begin
  _AddRef;
  enumerator := TEnumerator.Create;
  enumerator.fSource := Self;
  enumerator.fHashTable := fHashTable;
  enumerator.fVersion := enumerator.fHashTable.Version;
  Result := enumerator;
end;

function TInnerCollection<T>.GetIsEmpty: Boolean;
begin
  Result := fHashTable.Count = 0;
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

procedure TInnerCollection<T>.TEnumerator.BeforeDestruction;
begin
  TInnerCollection<T>(fSource)._Release;
end;

function TInnerCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TInnerCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  if inherited MoveNext then
  begin
    fCurrent := PT(fItem + TInnerCollection<T>(fSource).fOffset)^;
    Exit(True);
  end;
  Result := False;
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

function TCircularArrayBuffer<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
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

{$ENDREGION}


{$REGION 'TIteratorBase<T>'}

constructor TIteratorBase<T>.Create(const source: IEnumerable<T>;
  count: Integer; predicate: Pointer; kind: TIteratorKind);
var
  MoveNextFuncs: array[TIteratorKind] of Pointer;
  StartFuncs: array[TIteratorKind] of Pointer;
begin
  fIterator.Source := source;
  fComparer := source.Comparer;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<T>);
  fIterator.Count := count;
  fIterator.Predicate := IInterface(predicate);
  fIterator.Kind := kind;

  MoveNextFuncs[TIteratorKind.Concat] := @TIteratorRec<T>.Concat;
  MoveNextFuncs[TIteratorKind.Ordered] := @TIteratorRec<T>.Ordered;
  MoveNextFuncs[TIteratorKind.Reversed] := @TIteratorRec<T>.Reversed;
  MoveNextFuncs[TIteratorKind.Shuffled] := @TIteratorRec<T>.Ordered;

  MoveNextFuncs[TIteratorKind.Skip] := @TIteratorRec<T>.Skip;
  MoveNextFuncs[TIteratorKind.SkipWhile] := @TIteratorRec<T>.SkipWhile;
  MoveNextFuncs[TIteratorKind.SkipWhileIndex] := @TIteratorRec<T>.SkipWhileIndex;

  MoveNextFuncs[TIteratorKind.Take] := @TIteratorRec<T>.Take;
  MoveNextFuncs[TIteratorKind.TakeWhile] := @TIteratorRec<T>.TakeWhile;
  MoveNextFuncs[TIteratorKind.TakeWhileIndex] := @TIteratorRec<T>.TakeWhileIndex;

  MoveNextFuncs[TIteratorKind.Where] := @TIteratorRec<T>.Where;

  StartFuncs[TIteratorKind.Concat] := @TIteratorRec.GetEnumerator;
  StartFuncs[TIteratorKind.Ordered] := @TIteratorRec<T>.ToArray;
  StartFuncs[TIteratorKind.Reversed] := @TIteratorRec<T>.ToArray;
  StartFuncs[TIteratorKind.Shuffled] := @TIteratorRec<T>.ToArray;

  StartFuncs[TIteratorKind.Skip] := @TIteratorRec.GetEnumeratorAndSkip;
  StartFuncs[TIteratorKind.SkipWhile] := @TIteratorRec.GetEnumerator;
  StartFuncs[TIteratorKind.SkipWhileIndex] := @TIteratorRec.GetEnumerator;

  StartFuncs[TIteratorKind.Take] := @TIteratorRec.GetEnumerator;
  StartFuncs[TIteratorKind.TakeWhile] := @TIteratorRec.GetEnumerator;
  StartFuncs[TIteratorKind.TakeWhileIndex] := @TIteratorRec.GetEnumerator;

  StartFuncs[TIteratorKind.Where] := @TIteratorRec.GetEnumerator;

  fIterator.MoveNext := MoveNextFuncs[kind];
  fIterator.Start := StartFuncs[kind];
end;

function TIteratorBase<T>.GetElementType: PTypeInfo;
begin
  if Assigned(fIterator.Source) then
    Result := fIterator.Source.ElementType
  else
    Result := inherited GetElementType;
end;

function TIteratorBase<T>.GetEnumerator: IEnumerator<T>;
begin
  TIterator.Create(@Result, @TEnumerator.Enumerator_Vtable, Self, @fIterator, @TEnumerator.GetCurrent);
end;

function TIteratorBase<T>.ToArray: TArray<T>;
begin
  case fIterator.Kind of
    TIteratorKind.Ordered:
    begin
      Result := fIterator.Source.ToArray;
      TArray.Sort<T>(Result, IComparer<T>(fIterator.Predicate));
    end;
    TIteratorKind.Reversed:
    begin
      Result := fIterator.Source.ToArray;
      TArray.Reverse<T>(Result);
    end;
    TIteratorKind.Shuffled:
    begin
      Result := fIterator.Source.ToArray;
      TArray.Shuffle<T>(Result);
    end;
  else
    Result := inherited ToArray;
  end;
end;

{$ENDREGION}


{$REGION 'TEnumerableIterator<T>'}

function TEnumerableIterator<T>.GetCount: Integer;
begin
  if not PIteratorRec(@fIterator).TryGetCount(Result) then
    Result := inherited GetCount;
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>'}

constructor TArrayIterator<T>.Create(const values: TArray<T>);
begin
  fIterator.MoveNext := @TIteratorRec<T>.Ordered;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<T>);
  fIterator.Items := values;
end;

constructor TArrayIterator<T>.Create(const values: array of T);
var
  count: Integer;
begin
  fIterator.MoveNext := @TIteratorRec<T>.Ordered;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<T>);
  count := Length(values);
  SetLength(fIterator.Items, count);
  if TType.IsManaged<T> then
    MoveManaged(@values[0], @fIterator.Items[0], TypeInfo(T), count)
  else
    System.Move(values[0], fIterator.Items[0], count * SizeOf(T));
end;

procedure TArrayIterator<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  count: Integer;
begin
  count := DynArrayLength(fIterator.Items);
  if count > 0 then
    if TType.IsManaged<T> then
      MoveManaged(@fIterator.Items[0], @values[index], TypeInfo(T), count)
    else
      System.Move(fIterator.Items[0], values[index], SizeOf(T) * count);
end;

function TArrayIterator<T>.GetCount: Integer;
begin
  Result := DynArrayLength(fIterator.Items);
end;

function TArrayIterator<T>.GetIsEmpty: Boolean;
begin
  Result := fIterator.Items = nil;
end;

function TArrayIterator<T>.GetItem(index: Integer): T;
begin
  CheckIndex(index, DynArrayLength(fIterator.Items));

  Result := fIterator.Items[index];
end;

function TArrayIterator<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, DynArrayLength(fIterator.Items));
end;

function TArrayIterator<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, DynArrayLength(fIterator.Items) - index);
end;

function TArrayIterator<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, GetElementType, SizeOf(T)));

  for i := index to index + count - 1 do
    if comparer.Equals(fIterator.Items[i], item) then
      Exit(i);
  Result := -1;
end;

function TArrayIterator<T>.ToArray: TArray<T>;
begin
  Result := fIterator.Items;
  SetLength(Result, DynArrayLength(Result));
end;

{$ENDREGION}


{$REGION 'TIterator'}

class function TIterator.Create(enumerator: PPointer; vtable: PEnumeratorVtable;
  parent: TRefCountedObject; iterator: PIteratorRec; getCurrent: Pointer): Pointer;
begin
  IInterface(enumerator^) := nil;
  parent._AddRef;
  Result := AllocMem(SizeOf(TIterator));
  PIterator(Result).Vtable := vtable;
  PIterator(Result).RefCount := 1;
  PIterator(Result).Parent := parent;
  PIterator(Result).Iterator := iterator.Clone;
  enumerator^ := Result;

  if not Assigned(vtable[0]) then
  begin
    vtable[0] := @NopQueryInterface;
    vtable[1] := @RecAddRef;
    vtable[2] := @TIterator._Release;
    vtable[3] := getCurrent;
    vtable[4] := @TIterator.MoveNext;
  end;
end;

function TIterator._Release: Integer;
begin
  Result := AtomicDecrement(RefCount);
  if Result = 0 then
  begin
    Iterator.Finalize;
    FreeMem(Iterator);
    Parent._Release;
    FreeMem(@Self);
  end;
end;

function TIterator.MoveNext: Boolean;
begin
  repeat
    if State = Started then
    begin
      if Iterator.MoveNext(Iterator) then
        Exit(True);
      State := Finished;
    end;
    if State = Initial then
    begin
      State := Started;
      Start;
    end;
  until State = Finished;

  Iterator.Finalize;
  Result := False;
end;

procedure TIterator.Start;
var
  startProc: TStartFunc;
begin
  startProc := Iterator.Start;
  if Assigned(startProc) then
    if not startProc(Iterator) then
      State := Finished;
end;

{$ENDREGION}


{$REGION 'TIteratorBase<T>.TEnumerator'}

function TIteratorBase<T>.TEnumerator.GetCurrent: T;
begin
  Result := PIteratorRecT(fIterator).Current;
end;

{$ENDREGION}


{$REGION 'TIteratorRec'}

function TIteratorRec.Clone: PIteratorRec;
begin
  Result := AllocMem(TypeInfo.TypeData.RecSize);
  CopyRecord(Result, @Self, TypeInfo);
end;

procedure TIteratorRec.Finalize;
begin
  FinalizeRecord(@Self, TypeInfo);
end;

function TIteratorRec.TryGetCount(out count: Integer): Boolean;
begin
  Result := True;
  case Kind of
    TIteratorKind.Concat:
      count := Source.Count + IEnumerable(Predicate).Count;
    TIteratorKind.Skip:
      count := Math.Max(0, Source.Count - Self.Count);
    TIteratorKind.Take:
      count := Math.Min(Self.Count, Source.Count);
  else
    Result := False;
      end;
end;

function TIteratorRec.GetEnumerator: Boolean;
begin
  Enumerator := Source.GetEnumerator;
  Result := True;
end;

function TIteratorRec.GetEnumeratorAndSkip: Boolean;
begin
  Enumerator := Source.GetEnumerator;
  while (Count > 0) and Enumerator.MoveNext do
    Dec(Count);
  Result := Count <= 0;
end;

procedure TIteratorRec.GetSecondEnumerator;
begin
  Enumerator := IEnumerable(Predicate).GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TIteratorRec<T>'}

class function TIteratorRec<T>.GetCurrent(const enumerator: IEnumerator;
  elementType: PTypeInfo): Spring.TValue;
var
  current: T;
begin
  current := IEnumerator<T>(enumerator).Current;
  Spring.TValue.Make(@current, elementType, Result);
end;

class function TIteratorRec<T>.Add(const collection: IInterface;
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

function TIteratorRec<T>.Concat: Boolean;
begin
  while Assigned(Enumerator) do
  begin
    if Enumerator.MoveNext then
    begin
      Current := Enumerator.Current;
      Exit(True);
    end;

    if Count = 1 then // 1 means already done with the second
      Break;

    Count := 1;
      PIteratorRec(@Self).GetSecondEnumerator;
  end;
    Result := False;
end;

function TIteratorRec<T>.Ordered: Boolean;
begin
  Result := Count < DynArrayLength(Items);
  if Result then
  begin
    Current := Items[Count];
    Inc(Count);
  end;
end;

function TIteratorRec<T>.Reversed: Boolean;
begin
  Result := Count > 0;
  if Result then
  begin
    Dec(Count);
    Current := Items[Count];
  end;
end;

function TIteratorRec<T>.Skip: Boolean;
begin
  if Enumerator.MoveNext then
  begin
    Current := Enumerator.Current;
    Result := True;
  end
  else
    Result := False;
end;

function TIteratorRec<T>.SkipWhile: Boolean;
begin
  while Enumerator.MoveNext do
  begin
    Current := Enumerator.Current;

    if Count > -1 then // -1 means already done skipping
      if not Predicate<T>(Predicate)(Current) then
        Count := -1;

    if Count = -1 then
      Exit(True);
  end;
  Result := False;
end;

function TIteratorRec<T>.SkipWhileIndex: Boolean;
begin
  while Enumerator.MoveNext do
  begin
    Current := Enumerator.Current;

    if Count > -1 then // -1 means already done skipping
      if not Func<T, Integer, Boolean>(Predicate)(Current, Count) then
        Count := -1
      else
        Inc(Count);

    if Count = -1 then
      Exit(True);
  end;
  Result := False;
end;

function TIteratorRec<T>.Take: Boolean;
begin
  if Count > 0 then
    if Enumerator.MoveNext then
    begin
      Current := Enumerator.Current;
      Dec(Count);
      Exit(True);
    end;
  Result := False;
end;

function TIteratorRec<T>.TakeWhile: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    Current := Enumerator.Current;
    Result := Predicate<T>(Predicate)(Current);
  end;
end;

function TIteratorRec<T>.TakeWhileIndex: Boolean;
begin
  Result := Enumerator.MoveNext;
  if Result then
  begin
    Current := Enumerator.Current;
    Result := Func<T, Integer, Boolean>(Predicate)(Current, Count);
    Inc(Count);
  end;
end;

function TIteratorRec<T>.ToArray: Boolean;
begin
  Items := Source.ToArray;
  case Kind of
    TIteratorKind.Ordered:
      TArray.Sort<T>(Items, IComparer<T>(Predicate));
    TIteratorKind.Reversed:
      Count := DynArrayLength(Items);
    TIteratorKind.Shuffled:
      TArray.Shuffle<T>(Items);
  end;
  Result := True;
end;

function TIteratorRec<T>.Where: Boolean;
begin
  while Enumerator.MoveNext do
  begin
    Current := Enumerator.Current;
    if Predicate<T>(Predicate)(Current) then
      Exit(True);
  end;
  Result := False;
end;

{$ENDREGION}


{$REGION 'TObjectArrayIterator'}

constructor TObjectArrayIterator.Create(const source: TArray<TObject>;
  elementType: PTypeInfo);
begin
  inherited Create(source);
  fElementType := elementType;
end;

constructor TObjectArrayIterator.CreateFromArray(source: PPointer;
  count: Integer; elementType: PTypeInfo);
begin
  fElementType := elementType;
  fIterator.MoveNext := @TIteratorRec<TObject>.Ordered;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<TObject>);
  if count > 0 then
  begin
    SetLength(fIterator.Items, count);
{$IFDEF AUTOREFCOUNT}
    MoveManaged(source, @fIterator.Items[0], TypeInfo(TObject), count)
{$ELSE}
    System.Move(source^, fIterator.Items[0], count * SizeOf(TObject));
{$ENDIF}
  end;
end;

function TObjectArrayIterator.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


{$REGION 'TInterfaceArrayIterator'}

constructor TInterfaceArrayIterator.Create(const source: TArray<IInterface>;
  elementType: PTypeInfo);
begin
  inherited Create(source);
  fElementType := elementType;
end;

constructor TInterfaceArrayIterator.CreateFromArray(source: PPointer;
  count: Integer; elementType: PTypeInfo);
begin
  fElementType := elementType;
  fIterator.MoveNext := @TIteratorRec<IInterface>.Ordered;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<IInterface>);
  if count > 0 then
  begin
    SetLength(fIterator.Items, count);
    MoveManaged(source, @fIterator.Items[0], TypeInfo(IInterface), count)
  end;
end;

function TInterfaceArrayIterator.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


end.
