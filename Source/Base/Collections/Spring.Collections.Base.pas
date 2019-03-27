{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2019 Spring4D Team                           }
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
  Generics.Defaults,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Events;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  /// <summary>
  ///   Provides a default implementation for the <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
  ///    interface.
  /// </summary>
  TEnumerableBase<T> = class abstract(TRefCountedObject)
  protected
    fComparer: IComparer<T>;
    this: IEnumerable<T>;
  {$REGION 'Property Accessors'}
    function GetComparer: IComparer<T>;
    function GetCount: Integer;
    function GetElementType: PTypeInfo; virtual;
    function GetIsEmpty: Boolean;
  {$ENDREGION}
    function QueryInterface(const IID: TGUID; out obj): HResult; override;
    function Equals(const left, right: T): Boolean; reintroduce; inline;
    procedure CopyTo(var values: TArray<T>; index: Integer);
    function TryGetElementAt(out value: T; index: Integer): Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; overload;
    function TryGetFirst(out value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetLast(out value: T): Boolean; overload;
    function TryGetLast(out value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetSingle(out value: T): Boolean; overload;
    function TryGetSingle(out value: T; const predicate: Predicate<T>): Boolean; overload;
    function UseComparer(typeKind: TTypeKind): Boolean; inline;
    property Comparer: IComparer<T> read fComparer;
  public
    constructor Create; overload; virtual;
    constructor Create(const comparer: IComparer<T>); overload;
    constructor Create(const comparer: TComparison<T>); overload;
    destructor Destroy; override;

    function Aggregate(const func: Func<T, T, T>): T;

    function All(const predicate: Predicate<T>): Boolean;

    function Any: Boolean; overload;
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

    function Sum: T; overload;

    function Take(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Where(const predicate: Predicate<T>): IEnumerable<T>;

    function ToArray: TArray<T>;
  end;

  TEnumerableWrapper = class sealed(TRefCountedObject, IEnumerable)
  private type
    TGetCurrentFunc = function(const enumerator: IEnumerator): Spring.TValue;
    TEnumerator = class sealed(TRefCountedObject, IEnumerator)
    private
      fSource: IEnumerator;
      fGetCurrent: TGetCurrentFunc;
      function GetCurrent: TValue;
    public
      constructor Create(const source: IEnumerator; getCurrent: TGetCurrentFunc);
      function MoveNext: Boolean;
    end;
  private
    fSource: IEnumerable;
    fGetCurrent: TGetCurrentFunc;
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
  public
    constructor Create(const source: IEnumerable; getCurrent: TGetCurrentFunc);
    function AsObject: TObject;
    function GetEnumerator: IEnumerator;
  end;

  TMoveNextFunc = function(self: Pointer): Boolean;
  TStartProc = procedure(self: Pointer);
  PIteratorRec = ^TIteratorRec;
  TIteratorRec = record
    MoveNext: TMoveNextFunc;
    Source: IEnumerable;
    Enumerator: IEnumerator;
    TypeInfo: PTypeInfo;
    Start: TStartProc;

    Predicate: IInterface;

    function Clone: PIteratorRec;
    procedure Finalize;

    procedure GetEnumerator;
    procedure GetSecondEnumerator;
  const
    Ordered = -1;
    Reversed = -2;
    Shuffled = -3;
  end;

  TIteratorRec<T> = record
    MoveNext: TMoveNextFunc;
    Source: IEnumerable<T>;
    Enumerator: IEnumerator<T>;
    TypeInfo: PTypeInfo;
    Start: TStartProc;

    Predicate: IInterface;
    Items: TArray<T>;
    Count: Integer;
    Current: T;

    procedure ToArray;

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

    class function GetCurrent(const enumerator: IEnumerator): TValue; static;
  end;

  TEnumerableIterator = class abstract(TRefCountedObject)
  private type
    TEnumeratorState = (Initial, Started, Finished);
  private
    fSource: TRefCountedObject;
    fIterator: PIteratorRec;
    fState: TEnumeratorState;
    procedure Start;
  public
    constructor Create(source: TRefCountedObject; iterator: PIteratorRec);
    destructor Destroy; override;
    function MoveNext: Boolean;
  end;

  TEnumerableIteratorBase<T> = class abstract(TEnumerableBase<T>)
  private type
    TEnumerator = class(TEnumerableIterator, IInterface, IEnumerator<T>)
    private
      function GetCurrent: T;
    end;
  private
    fIterator: TIteratorRec<T>;
  public
    constructor Create(const source: IEnumerable<T>;
      count: Integer; predicate: Pointer;
      moveNext: TMoveNextFunc; start: TStartProc); overload;
    function GetEnumerator: IEnumerator<T>;
  end;

  TEnumerableIterator<T> = class sealed(TEnumerableIteratorBase<T>, IInterface, IEnumerable<T>);

  TArrayIterator<T> = class sealed(TEnumerableIteratorBase<T>, IInterface,
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
    function GetItem(index: Integer): T;

    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  public
    constructor Create(const source: TArray<T>); overload;
    constructor Create(const source: array of T); overload;

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

  TIterator<T> = class abstract(TEnumerableBase<T>, IInterface, IEnumerator<T>)
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
    constructor Create; override;
    function GetEnumerator: IEnumerator<T>;
    function MoveNext: Boolean;
  end;

  TSourceIterator<T> = class abstract(TIterator<T>, IEnumerable<T>)
  protected
    fSource: IEnumerable<T>;
    function GetElementType: PTypeInfo; override;
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
    procedure InitializeEvent;
    procedure UpdateNotify(Sender: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
    procedure DoNotify(const item: T; action: TCollectionChangedAction); inline;
    procedure Reset;
    property OnChanged: TCollectionChangedEventImpl<T> read fOnChanged;
    property Notify: TNotify read fNotify;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    function RemoveAll(const match: Predicate<T>): Integer;
    function RemoveRange(const values: array of T): Integer; overload;
    function RemoveRange(const values: IEnumerable<T>): Integer; overload;

    function ExtractAll(const match: Predicate<T>): TArray<T>;
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const values: IEnumerable<T>); overload;

    procedure CopyTo(var values: TArray<T>; index: Integer);
    function MoveTo(const collection: ICollection<T>): Integer; overload;
    function MoveTo(const collection: ICollection<T>;
      const match: Predicate<T>): Integer; overload;
  end;

  TCircularArrayBuffer<T> = class(TEnumerableBase<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TRefCountedObject, IInterface, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TCircularArrayBuffer<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: TCircularArrayBuffer<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
      ItemType = TArrayManager<T>;
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
    function GetTail: PT; inline;
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

    procedure DoNotify(const item: T; action: TCollectionChangedAction); inline;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Items: TArray<T> read fItems;
    property Head: Integer read fHead;
    property Tail: PT read GetTail;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create; override;
    constructor Create(capacity: Integer; ownsObjects: Boolean = False); overload;
    constructor Create(ownsObjects: Boolean); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function First: T; overload;
    function FirstOrDefault: T; overload;
    function Single: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;
    function TryGetFirst(out value: T): Boolean; overload;
    function TryGetLast(out value: T): Boolean; overload;
  {$ENDREGION}

    procedure Clear;
    procedure TrimExcess;
  end;

  TMapBase<TKey, T> = class abstract(TCollectionBase<TPair<TKey, T>>)
  private
    type
      TKeyValuePair = TPair<TKey, T>;
  protected
    fOnKeyChanged: TCollectionChangedEventImpl<TKey>;
    fOnValueChanged: TCollectionChangedEventImpl<T>;
  {$REGION 'Property Accessors'}
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<T>;
    function GetKeyType: PTypeInfo; virtual;
    function GetValueType: PTypeInfo; virtual;
  {$ENDREGION}
    procedure DoNotify(const key: TKey; const value: T; action: TCollectionChangedAction);
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: T; action: TCollectionChangedAction); inline;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Add(const item: TKeyValuePair): Boolean; overload;
    procedure Add(const key: TKey; const value: T); overload;

    function Remove(const item: TKeyValuePair): Boolean;

    function Extract(const item: TKeyValuePair): TKeyValuePair;

    function Contains(const item: TKeyValuePair): Boolean; overload;

    property OnKeyChanged: ICollectionChangedEvent<TKey> read GetOnKeyChanged;
    property OnValueChanged: ICollectionChangedEvent<T> read GetOnValueChanged;
    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  /// <summary>
  ///   Provides an abstract implementation for the <see cref="Spring.Collections|IList&lt;T&gt;" />
  ///    interface.
  /// </summary>
  TListBase<T> = class abstract(TCollectionBase<T>)
  protected
  {$REGION 'Implements IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  {$ENDREGION}
    function CreateList: IList<T>; virtual; //abstract;
  public
    constructor Create; override;

    function Add(const item: T): Boolean;

    function Remove(const item: T): Boolean;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;

    function LastIndexOf(const item: T): Integer; overload;
    function LastIndexOf(const item: T; index: Integer): Integer; overload;

    procedure Reverse; overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;
    procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;
  end;

  Error = record
    class function ArgumentOutOfRange(const s: string): Exception; static;
    class function DuplicateKey: Exception; static;
    class function EnumFailedVersion: Exception; static;
    class function KeyNotFound: Exception; static;
    class function MoreThanOneElement: Exception; static;
    class function MoreThanOneMatch: Exception; static;
    class function NoClassType(t: PTypeInfo): Exception; static;
    class function NoElements: Exception; static;
    class function NoMatch: Exception; static;
    class function NotSupported: Exception; static;
  end;

  TArrayHelper = record
    class function IndexOf1(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function IndexOf2(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function IndexOf4(items: Pointer; const item; index, count: Integer): Integer; static;
    class function IndexOf8(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function IndexOfObj(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function IndexOfStr(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function LastIndexOf1(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function LastIndexOf2(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function LastIndexOf4(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function LastIndexOf8(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function LastIndexOfObj(const items: Pointer; const item; index, count: Integer): Integer; static;
    class function LastIndexOfStr(const items: Pointer; const item; index, count: Integer): Integer; static;
  end;

const
  CountMask = Integer($7FFFFFFF);
  BitMask: array[Boolean] of Integer = (0, not CountMask);

  // use the MSB of the HashCode to note removed items
  RemovedFlag        = Integer($80000000);
  MinCapacity        = 6; // 75% load factor leads to min bucket count of 8
  BucketSentinelFlag = RemovedFlag; // note: the same as RemovedFlag
  EmptyBucket        = -1; // must be negative, note choice of BucketSentinelFlag
  UsedBucket         = -2; // likewise

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  Spring.Collections.Lists,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TArrayHelper'}

class function TArrayHelper.IndexOf1(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index to index + count - 1 do
    if TArray<Byte>(items)[Result] = Byte(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.IndexOf2(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index to index + count - 1 do
    if TArray<Word>(items)[Result] = Word(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.IndexOf4(items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index to index + count - 1 do
    if TArray<Cardinal>(items)[Result] = Cardinal(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.IndexOf8(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index to index + count - 1 do
    if TArray<UInt64>(items)[Result] = UInt64(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.IndexOfObj(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index to index + count - 1 do
    if TArray<TObject>(items)[Result] = nil then
    begin
      if TObject(item) = nil then
        Exit;
    end
    else if TArray<TObject>(items)[Result].Equals(TObject(item)) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.IndexOfStr(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index to index + count - 1 do
    if TArray<string>(items)[Result] = string(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.LastIndexOf1(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index downto index - count do
    if TArray<Byte>(items)[Result] = Byte(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.LastIndexOf2(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index downto index - count do
    if TArray<Word>(items)[Result] = Word(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.LastIndexOf4(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index downto index - count do
    if TArray<Cardinal>(items)[Result] = Cardinal(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.LastIndexOf8(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index downto index - count do
    if TArray<UInt64>(items)[Result] = UInt64(item) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.LastIndexOfObj(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index downto index - count do
    if TArray<TObject>(items)[Result] = nil then
    begin
      if TObject(item) = nil then
        Exit;
    end
    else if TArray<TObject>(items)[Result].Equals(TObject(item)) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.LastIndexOfStr(const items: Pointer; const item;
  index, count: Integer): Integer;
begin
  for Result := index downto index - count do
    if TArray<string>(items)[Result] = string(item) then
      Exit;
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

constructor TEnumerableBase<T>.Create;
begin
//  Assert(Assigned(GetInterfaceEntry(IInterface)), ClassName + ' does not implement IInterface');
  inherited Create;
  if TType.Kind<T> = tkClass then
    fComparer := IComparer<T>(GetInstanceComparer)
  else
    fComparer := IComparer<T>(_LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));

  Pointer(this) := Pointer(PByte(Self) + GetInterfaceEntry(IEnumerable<T>).IOffset);
end;

constructor TEnumerableBase<T>.Create(const comparer: IComparer<T>);
begin
  Create;
  if Assigned(comparer) then
    fComparer := comparer;
end;

constructor TEnumerableBase<T>.Create(const comparer: TComparison<T>);
begin
  Create(IComparer<T>(PPointer(@comparer)^));
end;

destructor TEnumerableBase<T>.Destroy;
begin
  Pointer(this) := nil;
  inherited Destroy;
end;

function TEnumerableBase<T>.UseComparer(typeKind: TTypeKind): Boolean;
const
  FastComparableTypes = [tkUnknown, tkInteger, tkChar, tkEnumeration, tkSet,
                         tkClass, tkMethod, tkWChar, tkInterface, tkInt64,
                         tkUString, tkClassRef, tkPointer, tkProcedure];
begin
  Result := not ((typeKind in FastComparableTypes) and (SizeOf(T) in [1, 2, 4, 8])
    and ((fComparer = nil) or (Pointer(fComparer) = _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)))));
end;

function TEnumerableBase<T>.Aggregate(const func: Func<T, T, T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(func), 'func');
{$ENDIF}

  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    raise Error.NoElements;
  Result := enumerator.Current;
  while enumerator.MoveNext do
    Result := func(Result, enumerator.Current);
end;

function TEnumerableBase<T>.All(const predicate: Predicate<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  for item in this do
    if not predicate(item) then
      Exit(False);
  Result := True;
end;

function TEnumerableBase<T>.Any: Boolean;
begin
  Result := not this.IsEmpty;
end;

function TEnumerableBase<T>.Any(const predicate: Predicate<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  for item in this do
    if predicate(item) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase<T>.Concat(
  const second: IEnumerable<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, 0, Pointer(second),
    @TIteratorRec<T>.Concat, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
  comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));
  Result := IEnumerable<T>(this).Contains(value, comparer);
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull<T>(value, 'value');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  for item in this do
    if comparer.Equals(value, item) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: TEqualityComparison<T>): Boolean;
begin
  Result := this.Contains(value, IEqualityComparer<T>(PPointer(@comparer)^));
end;

procedure TEnumerableBase<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
  while enumerator.MoveNext do
  begin
    values[index] := enumerator.Current;
    Inc(index);
  end;
end;

function TEnumerableBase<T>.ElementAt(index: Integer): T;
begin
  if not TryGetElementAt(Result, index) then
    raise Error.ArgumentOutOfRange('index');
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
begin
  if not TryGetElementAt(Result, index) then
    Result := Default(T);
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer;
  const defaultValue: T): T;
begin
  if not TryGetElementAt(Result, index) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.Equals(const left, right: T): Boolean;
begin
  if TType.Kind<T> = tkClass then
    if PObject(@left)^ = nil then
      Result := PObject(@right)^ = nil
    else
      Result := PObject(@left).Equals(PObject(@right)^)
  else
    if UseComparer(TType.Kind<T>) then
      Result := fComparer.Compare(left, right) = 0
    else
      if TType.Kind<T> = tkUString then
        Result := PString(@left)^ = PString(@right)^
      else
        case SizeOf(T) of
          1: Result := PByte(@left)^ = PByte(@right)^;
          2: Result := PWord(@left)^ = PWord(@right)^;
          4: Result := PCardinal(@left)^ = PCardinal(@right)^;
          8: Result := PUInt64(@left)^ = PUInt64(@right)^;
        end;
end;

function TEnumerableBase<T>.EqualsTo(const values: array of T): Boolean;
var
  e: IEnumerator<T>;
  i: Integer;
begin
  e := this.GetEnumerator;
  i := 0;

  while e.MoveNext do
  begin
    if not ((i < Length(values)) and Equals(e.Current, values[i])) then
      Exit(False);
    Inc(i);
  end;
  Result := i = Length(values);
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
  if this = values then
    Result := True
  else
  begin
    comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));
    Result := IEnumerable<T>(this).EqualsTo(values, comparer);
  end;
end;

function TEnumerableBase<T>.EqualsTo(const values: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
var
  e1, e2: IEnumerator<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  e1 := this.GetEnumerator;
  e2 := values.GetEnumerator;

  while e1.MoveNext do
    if not (e2.MoveNext and comparer.Equals(e1.Current, e2.Current)) then
      Exit(False);
  Result := not e2.MoveNext;
end;

function TEnumerableBase<T>.First: T;
begin
  if not this.TryGetFirst(Result) then
    raise Error.NoElements;
end;

function TEnumerableBase<T>.First(const predicate: Predicate<T>): T;
begin
  if not this.TryGetFirst(Result, predicate) then
    raise Error.NoMatch;
end;

function TEnumerableBase<T>.FirstOrDefault: T;
var
  defaultItem: T;
begin
  if not this.TryGetFirst(Result) then
    Result := Default(T);
end;

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
begin
  if not this.TryGetFirst(Result) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: Predicate<T>): T;
begin
  if not this.TryGetFirst(Result, predicate) then
    Result := Default(T);
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
begin
  if not this.TryGetFirst(Result, predicate) then
    Result := defaultValue;
end;

procedure TEnumerableBase<T>.ForEach(const action: Action<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(action), 'action');
{$ENDIF}

  for item in this do
    action(item);
end;

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TEnumerableBase<T>.GetCount: Integer;
var
  enumerator: IEnumerator<T>;
begin
  Result := 0;
  enumerator := this.GetEnumerator;
  while enumerator.MoveNext do
    Inc(Result);
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerableBase<T>.GetIsEmpty: Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
  Result := not enumerator.MoveNext;
end;

function TEnumerableBase<T>.Last: T;
begin
  if not this.TryGetLast(Result) then
    raise Error.NoElements;
end;

function TEnumerableBase<T>.Last(const predicate: Predicate<T>): T;
begin
  if not this.TryGetLast(Result, predicate) then
    raise Error.NoMatch;
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  if not this.TryGetLast(Result) then
    Result := Default(T);
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
begin
  if not this.TryGetLast(Result) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: Predicate<T>): T;
begin
  if not this.TryGetLast(Result, predicate) then
    Result := Default(T);
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
begin
  if not this.TryGetLast(Result) then
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
  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    raise Error.NoElements;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    raise Error.NoElements;
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
  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    raise Error.NoElements;
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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    raise Error.NoElements;
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
  Result := this.Min(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Ordered: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(this, TIteratorRec.Ordered, Pointer(fComparer),
    @TIteratorRec<T>.Ordered, @TIteratorRec<T>.ToArray);
end;

function TEnumerableBase<T>.Ordered(
  const comparer: IComparer<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, TIteratorRec.Ordered, Pointer(comparer),
    @TIteratorRec<T>.Ordered, @TIteratorRec<T>.ToArray);
end;

function TEnumerableBase<T>.Ordered(
  const comparer: TComparison<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  Result := Ordered(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.QueryInterface(const IID: TGUID; out obj): HResult;
begin
  if IID = IEnumerable then
  begin
    IEnumerable(obj) := TEnumerableWrapper.Create(IEnumerable(this), TIteratorRec<T>.GetCurrent);
    Result := S_OK;
  end
  else
    Result := inherited QueryInterface(IID, obj);
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(this, TIteratorRec.Reversed, nil,
    @TIteratorRec<T>.Reversed, @TIteratorRec<T>.ToArray);
end;

function TEnumerableBase<T>.Shuffled: IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(this, TIteratorRec.Shuffled, nil,
    @TIteratorRec<T>.Ordered, @TIteratorRec<T>.ToArray);
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    raise Error.NoElements;
  Result := enumerator.Current;
  if enumerator.MoveNext then
    raise Error.MoreThanOneElement;
end;

function TEnumerableBase<T>.Single(const predicate: Predicate<T>): T;
var
  enumerator: IEnumerator<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  enumerator := this.GetEnumerator;
  while enumerator.MoveNext do
  begin
    Result := enumerator.Current;
    if predicate(Result) then
    begin
      while enumerator.MoveNext do
        if predicate(enumerator.Current) then
          raise Error.MoreThanOneMatch;
      Exit;
    end;
  end;
  raise Error.NoMatch;
end;

function TEnumerableBase<T>.SingleOrDefault: T;
var
  defaultValue: T;
begin
  defaultValue := Default(T);
  Result := this.SingleOrDefault(defaultValue);
end;

function TEnumerableBase<T>.SingleOrDefault(const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
  if not enumerator.MoveNext then
    Exit(defaultValue);
  Result := enumerator.Current;
  if enumerator.MoveNext then
    raise Error.MoreThanOneElement;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: Predicate<T>): T;
var
  defaultValue: T;
begin
  defaultValue := Default(T);
  Result := this.SingleOrDefault(predicate, defaultValue);
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: Predicate<T>;
  const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  enumerator := this.GetEnumerator;
  while enumerator.MoveNext do
  begin
    Result := enumerator.Current;
    if predicate(Result) then
    begin
      while enumerator.MoveNext do
        if predicate(enumerator.Current) then
          raise Error.MoreThanOneMatch;
      Exit;
    end;
  end;
  Result := defaultValue;
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(this, count, nil,
    @TIteratorRec<T>.Skip, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, 0, PPointer(@predicate)^,
    @TIteratorRec<T>.SkipWhile, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, 0, PPointer(@predicate)^,
    @TIteratorRec<T>.SkipWhileIndex, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.Sum: T;
var
  item: T;
begin
  Result := Default(T);
  for item in this do
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

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>;
begin
  Result := TEnumerableIterator<T>.Create(this, count, nil,
    @TIteratorRec<T>.Take, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, 0, PPointer(@predicate)^,
    @TIteratorRec<T>.TakeWhile, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, 0, PPointer(@predicate)^,
    @TIteratorRec<T>.TakeWhileIndex, @TIteratorRec.GetEnumerator);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
var
  collection: ICollection<T>;
  count: Integer;
  item: T;
begin
  Result := nil;
  if Supports(Self, ICollection<T>, collection) then
  begin
    count := collection.Count;
    if count > 0 then
    begin
      SetLength(Result, count);
      collection.CopyTo(Result, 0);
    end;
  end
  else
  begin
    count := 0;
    for item in this do
    begin
      if Result = nil then
        SetLength(Result, 4)
      else if Length(Result) = count then
        SetLength(Result, count * 2);
      Result[count] := item;
      Inc(count);
    end;
    SetLength(Result, count);
  end;
end;

function TEnumerableBase<T>.TryGetElementAt(out value: T;
  index: Integer): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  if index < 0 then
    Exit(False);
  enumerator := this.GetEnumerator;
  while enumerator.MoveNext do
  begin
    if index = 0 then
    begin
      value := enumerator.Current;
      Exit(True);
    end;
    Dec(index);
  end;
  Result := False;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
  if enumerator.MoveNext then
  begin
    value := enumerator.Current;
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T; const predicate: Predicate<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  for item in this do
    if predicate(item) then
    begin
      value := item;
      Exit(True);
    end;
  value := Default(T);
  Result := False;
end;

function TEnumerableBase<T>.TryGetLast(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
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

function TEnumerableBase<T>.TryGetLast(out value: T; const predicate: Predicate<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := False;
  for item in this do
  begin
    if predicate(item) then
    begin
      value := item;
      Result := True;
    end;
  end;
  if not Result then
    value := Default(T);
end;

function TEnumerableBase<T>.TryGetSingle(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := this.GetEnumerator;
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

function TEnumerableBase<T>.TryGetSingle(out value: T;
  const predicate: Predicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in this do
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

function TEnumerableBase<T>.Where(const predicate: Predicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TEnumerableIterator<T>.Create(this, 0, PPointer(@predicate)^,
    @TIteratorRec<T>.Where, @TIteratorRec.GetEnumerator);
end;

{$ENDREGION}


{$REGION 'TEnumerableWrapper'}

constructor TEnumerableWrapper.Create(const source: IEnumerable;
  getCurrent: TGetCurrentFunc);
begin
  inherited Create;
  fSource := source;
  fGetCurrent := getCurrent;
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
  Result := TEnumerator.Create(fSource.GetEnumerator, fGetCurrent);
end;

function TEnumerableWrapper.GetIsEmpty: Boolean;
begin
  Result := fSource.IsEmpty;
end;

{$ENDREGION}


{$REGION 'TEnumerableWrapper.TEnumerator'}

constructor TEnumerableWrapper.TEnumerator.Create(const source: IEnumerator;
  getCurrent: TGetCurrentFunc);
begin
  inherited Create;
  fSource := source;
  fGetCurrent := getCurrent;
end;

function TEnumerableWrapper.TEnumerator.GetCurrent: TValue;
begin
  Result := fGetCurrent(fSource);
end;

function TEnumerableWrapper.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TIterator<T>'}

constructor TIterator<T>.Create;
begin
  inherited Create;
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

function TSourceIterator<T>.GetElementType: PTypeInfo;
begin
  Result := fSource.ElementType;
end;

{$ENDREGION}


{$REGION 'TCollectionBase<T>'}

constructor TCollectionBase<T>.Create;
begin
  inherited Create;
  Pointer(this) := Pointer(PByte(Self) + GetInterfaceEntry(ICollection<T>).IOffset);
  UpdateNotify(Self);
end;

destructor TCollectionBase<T>.Destroy;
begin
  fOnChanged.Free;
  inherited Destroy;
end;

procedure TCollectionBase<T>.InitializeEvent;
var
  onChanged: TCollectionChangedEventImpl<T>;
begin
  if not Assigned(fOnChanged) then
  begin
    onChanged := TCollectionChangedEventImpl<T>.Create;
    onChanged.OnChanged := UpdateNotify;
    if AtomicCmpExchange(Pointer(fOnChanged), Pointer(onChanged), nil) <> nil then
      onChanged.Free
    else
    begin
{$IFDEF AUTOREFCOUNT}
      onChanged.__ObjAddRef;
{$ENDIF AUTOREFCOUNT}
      UpdateNotify(Self);
    end;
  end;
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
  for i := Low(values) to High(values) do
    ICollection<T>(this).Add(values[i]);
end;

procedure TCollectionBase<T>.AddRange(const values: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  for item in values do
    ICollection<T>(this).Add(item);
end;

procedure TCollectionBase<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if Assigned(fOnChanged) and fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
end;

procedure TCollectionBase<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(Length(values), index, this.Count);
{$ENDIF}

  for item in this do
  begin
    values[index] := item;
    Inc(index);
  end;
end;

function TCollectionBase<T>.ExtractAll(const match: Predicate<T>): TArray<T>;
begin
  Result := this.Where(match).ToArray;
  ExtractRange(Result);
end;

procedure TCollectionBase<T>.ExtractRange(const values: array of T);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    ICollection<T>(this).Extract(values[i]);
end;

procedure TCollectionBase<T>.ExtractRange(const values: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  for item in values do
    ICollection<T>(this).Extract(item);
end;

function TCollectionBase<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

function TCollectionBase<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  InitializeEvent;
  Result := fOnChanged;
end;

function TCollectionBase<T>.MoveTo(const collection: ICollection<T>): Integer;
begin
  Result := MoveTo(collection, nil);
end;

function TCollectionBase<T>.MoveTo(const collection: ICollection<T>;
  const match: Predicate<T>): Integer;
var
  values: TArray<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  Result := 0;
  values := ToArray;
  for i := Low(values) to High(values) do
    if not Assigned(match) or match(values[i]) then
    begin
      ICollection<T>(this).Extract(values[i]);
      collection.Add(values[i]);
      Inc(Result);
    end;
end;

procedure TCollectionBase<T>.UpdateNotify(Sender: TObject);
var
  event: procedure(const item: T; action: TCollectionChangedAction) of object;
begin
  event := Changed;
  if (Assigned(fOnChanged) and fOnChanged.CanInvoke)
    or (TMethod(event).Code <> @TCollectionBase<T>.Changed) then
    fNotify := TMethod(event).Code
  else
    fNotify := nil;
end;

function TCollectionBase<T>.RemoveAll(const match: Predicate<T>): Integer;
begin
  Result := RemoveRange(this.Where(match).ToArray);
end;

function TCollectionBase<T>.RemoveRange(const values: array of T): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(values) to High(values) do
    if ICollection<T>(this).Remove(values[i]) then
      Inc(Result);
end;

function TCollectionBase<T>.RemoveRange(const values: IEnumerable<T>): Integer;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  Result := 0;
  for item in values do
    if ICollection<T>(this).Remove(item) then
      Inc(Result);
end;

procedure TCollectionBase<T>.Reset;
var
  defaultValue: T;
begin
  if Assigned(Notify) then
  begin
    defaultValue := Default(T);
    Notify(Self, defaultValue, caReseted);
  end;
end;

{$ENDREGION}


{$REGION 'TCircularArrayBuffer<T>'}

constructor TCircularArrayBuffer<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TCircularArrayBuffer<T>.Create(capacity: Integer; ownsObjects: Boolean);
begin
  Create;
  SetCapacity(capacity);
  SetOwnsObjects(ownsObjects);
end;

constructor TCircularArrayBuffer<T>.Create(ownsObjects: Boolean);
begin
  Create;
  SetOwnsObjects(ownsObjects);
end;

destructor TCircularArrayBuffer<T>.Destroy;
begin
  Clear;
  fOnChanged.Free;
  inherited Destroy;
end;

procedure TCircularArrayBuffer<T>.Clear;
var
  i: Integer;
begin
  for i := Count downto 1 do
    DeleteFromHead(caRemoved);
end;

procedure TCircularArrayBuffer<T>.DoNotify(const item: T;
  action: TCollectionChangedAction);
begin
  if fOnChanged.CanInvoke then
    fOnChanged.Invoke(Self, item, action);
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
  Result := TEnumerator.Create(Self);
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

function TCircularArrayBuffer<T>.GetTail: PT;
var
  index: Integer;
begin
  index := fTail;
  if index > 0 then
    Result := @fItems[index - 1]
  else
    Result := @fItems[fCapacity - 1];
end;

procedure TCircularArrayBuffer<T>.AddToHead(const item: T);
var
  index: Integer;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);

  index := fHead;
  if index = 0 then
    index := fCapacity;
  Dec(index);
  fItems[index] := item;
  fHead := index;

  DoNotify(item, caAdded);
end;

procedure TCircularArrayBuffer<T>.AddToTail(const item: T);
var
  index: Integer;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);

  index := fTail;
  fItems[index] := item;
  Inc(index);
  if index = fCapacity then
    index := 0;
  fTail := index;
  DoNotify(item, caAdded);
end;

procedure TCircularArrayBuffer<T>.DeleteFromHead(action: TCollectionChangedAction);
var
  index: Integer;
  item: PT;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  index := fHead;
  item := @fItems[index];
  Inc(index);
  if index = fCapacity then
    index := 0;
  fHead := index;

  DoNotify(item^, action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(item^);
  item^ := Default(T);
end;

procedure TCircularArrayBuffer<T>.DeleteFromTail(action: TCollectionChangedAction);
var
  index: Integer;
  item: PT;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);

  index := fTail;
  if index = 0 then
    index := fCapacity;
  Dec(index);
  item := @fItems[index];
  fTail := index;

  DoNotify(item^, action);
  if OwnsObjects and (action = caRemoved) then
    FreeObject(item^);
  item^ := Default(T);
end;

function TCircularArrayBuffer<T>.First: T;
begin
  if Count > 0 then
    Result := fItems[fHead]
  else
    raise Error.NoElements;
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
    0: raise Error.NoElements;
    1: Result := fItems[fHead];
  else
    raise Error.MoreThanOneElement;
  end;
end;

function TCircularArrayBuffer<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := fItems[fHead];
  else
    raise Error.MoreThanOneElement;
  end;
end;

procedure TCircularArrayBuffer<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TCircularArrayBuffer<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[fHead]
  else
    value := Default(T);
end;

function TCircularArrayBuffer<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Tail^
  else
    value := Default(T);
end;

procedure TCircularArrayBuffer<T>.SetCapacity(value: Integer);
var
  offset, oldCapacity, itemCount: Integer;
begin
  Guard.CheckRange(value >= Count, 'capacity');

  offset := value - Length(fItems);
  if offset = 0 then
    Exit;

  itemCount := Count;
  if itemCount = 0 then
  begin
    fHead := 0;
    fTail := 0;
    fCapacity := value;
    SetLength(fItems, value);
    Exit;
  end;

  oldCapacity := Length(fItems);
  if offset > 0 then
  begin
    fCapacity := value;
    SetLength(fItems, value);
  end;
  if fTail <= fHead then
  begin
    ItemType.Move(fItems, fItems, fHead, fHead + offset, oldCapacity - fHead);
    if offset > 0 then
      ItemType.Finalize(fItems, fHead, offset)
    else
      ItemType.Finalize(fItems, itemCount, -offset);
    Inc(fHead, offset);
  end
  else
  begin
    if fHead + itemCount > value then
    begin
      ItemType.Move(fItems, fItems, fHead, 0, itemCount);
      ItemType.Finalize(fItems, itemCount, fHead);
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
  fCount := (fCount and CountMask) or BitMask[value];
end;

{$ENDREGION}


{$REGION 'TCircularArrayBuffer<T>.TEnumerator'}

constructor TCircularArrayBuffer<T>.TEnumerator.Create(
  const source: TCircularArrayBuffer<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TCircularArrayBuffer<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TCircularArrayBuffer<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TCircularArrayBuffer<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  Result := fIndex < fSource.Count;
  if Result then
  begin
    fCurrent := fSource.fItems[(fSource.fHead + fIndex) mod fSource.fCapacity];
    Inc(fIndex);
  end
  else
    fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TMapBase<TKey, T>'}

constructor TMapBase<TKey, T>.Create;
begin
  inherited Create;
  fOnKeyChanged := TCollectionChangedEventImpl<TKey>.Create;
  fOnValueChanged := TCollectionChangedEventImpl<T>.Create;

  Pointer(this) := Pointer(PByte(Self) + GetInterfaceEntry(IMap<TKey, T>).IOffset);
end;

destructor TMapBase<TKey, T>.Destroy;
begin
  fOnValueChanged.Free;
  fOnKeyChanged.Free;
  inherited;
end;

procedure TMapBase<TKey, T>.DoNotify(const key: TKey; const value: T;
  action: TCollectionChangedAction);
var
  pair: TKeyValuePair;
begin
  pair.Key := key;
  pair.Value := value;
  Notify(Self, pair, action);
end;

function TMapBase<TKey, T>.Add(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, T>(this).TryAdd(item.Key, item.Value);
end;

procedure TMapBase<TKey, T>.Add(const key: TKey; const value: T);
begin
  if not IMap<TKey, T>(this).TryAdd(key, value) then
    raise Error.DuplicateKey;
end;

function TMapBase<TKey, T>.Contains(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, T>(this).Contains(item.Key, item.Value);
end;

function TMapBase<TKey, T>.Extract(const item: TKeyValuePair): TKeyValuePair;
begin
  Result := IMap<TKey, T>(this).Extract(item.Key, item.Value);
end;

function TMapBase<TKey, T>.GetKeyType: PTypeInfo;
begin
  Result := TypeInfo(TKey);
end;

function TMapBase<TKey, T>.GetOnKeyChanged: ICollectionChangedEvent<TKey>;
begin
  Result := fOnKeyChanged;
end;

function TMapBase<TKey, T>.GetOnValueChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnValueChanged;
end;

function TMapBase<TKey, T>.GetValueType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

procedure TMapBase<TKey, T>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
end;

function TMapBase<TKey, T>.Remove(const item: TKeyValuePair): Boolean;
begin
  Result := IMap<TKey, T>(this).Remove(item.Key, item.Value);
end;

procedure TMapBase<TKey, T>.ValueChanged(const item: T;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
end;

{$ENDREGION}


{$REGION 'TListBase<T>'}

constructor TListBase<T>.Create;
begin
  inherited Create;

  Pointer(this) := Pointer(PByte(Self) + GetInterfaceEntry(IList<T>).IOffset);
end;

function TListBase<T>.CreateList: IList<T>;
begin
  Result := TList<T>.Create;
end;

function TListBase<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  case TType.Kind<T> of
    tkClass:
      if IID = IObjectList then
        Exit(inherited QueryInterface(IList<TObject>, Obj));
    tkInterface:
      if IID = IInterfaceList then
        Exit(inherited QueryInterface(IList<IInterface>, Obj));
  end;
    Result := inherited QueryInterface(IID, Obj);
end;

function TListBase<T>.Add(const item: T): Boolean;
begin
  IList<T>(this).Add(item);
  Result := True;
end;

function TListBase<T>.IndexOf(const item: T): Integer;
var
  count: Integer;
begin
  count := this.Count;
  if count > 0 then
    Result := IList<T>(this).IndexOf(item, 0, count)
  else
    Result := -1;
end;

function TListBase<T>.IndexOf(const item: T; index: Integer): Integer;
var
  count: Integer;
begin
  count := this.Count;
  if count > 0 then
    Result := IList<T>(this).IndexOf(item, index, count - index)
  else
    Result := -1;
end;

function TListBase<T>.LastIndexOf(const item: T): Integer;
var
  count: Integer;
begin
  count := this.Count;
  if count > 0 then
    Result := IList<T>(this).LastIndexOf(item, count - 1, count)
  else
    Result := -1;
end;

function TListBase<T>.LastIndexOf(const item: T; index: Integer): Integer;
var
  count: Integer;
begin
  count := this.Count;
  if count > 0 then
    Result := IList<T>(this).LastIndexOf(item, index, index + 1)
  else
    Result := -1;
end;

function TListBase<T>.Remove(const item: T): Boolean;
var
  index: Integer;
begin
  index := IList<T>(this).IndexOf(item);
  Result := index >= 0;
  if Result then
    IList<T>(this).Delete(index);
end;

procedure TListBase<T>.Reverse;
begin
  IList<T>(this).Reverse(0, this.Count);
end;

procedure TListBase<T>.Sort;
begin
  IList<T>(this).Sort(fComparer, 0, this.Count);
end;

procedure TListBase<T>.Sort(const comparer: IComparer<T>);
begin
  IList<T>(this).Sort(comparer, 0, this.Count);
end;

procedure TListBase<T>.Sort(const comparer: TComparison<T>);
begin
  IList<T>(this).Sort(IComparer<T>(PPointer(@comparer)^), 0, this.Count);
end;

procedure TListBase<T>.Sort(const comparer: TComparison<T>; index, count: Integer);
begin
  IList<T>(this).Sort(IComparer<T>(PPointer(@comparer)^), index, count);
end;

{$ENDREGION}


{$REGION 'Error'}

class function Error.ArgumentOutOfRange(const s: string): Exception;
begin
  Result := EArgumentOutOfRangeException.CreateResFmt(@SArgument_ParamName, [s]);
end;

class function Error.DuplicateKey: Exception;
begin
  Result := EArgumentException.CreateRes(@SArgument_DuplicateKey);
end;

class function Error.EnumFailedVersion: Exception;
begin
  Result := EInvalidOperationException.CreateRes(@SEnumFailedVersion);
end;

class function Error.KeyNotFound: Exception;
begin
  Result := EKeyNotFoundException.CreateRes(@SArgument_KeyNotFound);
end;

class function Error.MoreThanOneElement: Exception;
begin
  Result := EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
end;

class function Error.MoreThanOneMatch: Exception;
begin
  Result := EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneMatchingElement);
end;

class function Error.NoClassType(t: PTypeInfo): Exception;
begin
  Result := EInvalidCast.CreateResFmt(@SNotClassType, [t.TypeName]);
end;

class function Error.NoElements: Exception;
begin
  Result := EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

class function Error.NoMatch: Exception;
begin
  Result := EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

class function Error.NotSupported: Exception;
begin
  Result := ENotSupportedException.Create('');
end;

{$ENDREGION}


{$REGION 'TEnumerableIterator<T>'}

constructor TEnumerableIteratorBase<T>.Create(const source: IEnumerable<T>;
  count: Integer; predicate: Pointer; moveNext: TMoveNextFunc; start: TStartProc);
begin
  inherited Create(source.Comparer);
  fIterator.MoveNext := moveNext;
  fIterator.Source := source;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<T>);
  fIterator.Start := start;
  fIterator.Count := count;
  fIterator.Predicate := IInterface(predicate);
end;

function TEnumerableIteratorBase<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self, @fIterator);
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>'}

constructor TArrayIterator<T>.Create(const source: TArray<T>);
begin
  inherited Create;
  fIterator.MoveNext := @TIteratorRec<T>.Ordered;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<T>);
  fIterator.Items := source;
end;

constructor TArrayIterator<T>.Create(const source: array of T);
var
  count: Integer;
begin
  inherited Create;
  fIterator.MoveNext := @TIteratorRec<T>.Ordered;
  fIterator.TypeInfo := TypeInfo(TIteratorRec<T>);
  count := Length(source);
  SetLength(fIterator.Items, count);
  if TType.IsManaged<T> then
    System.CopyArray(@fIterator.Items[0], @source[0], TypeInfo(T), count)
  else
    System.Move(source[0], fIterator.Items[0], count * SizeOf(T));
end;

procedure TArrayIterator<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  count: Integer;
begin
  count := Length(fIterator.Items);
  if count > 0 then
    if TType.IsManaged<T> then
      System.CopyArray(@values[index], @fIterator.Items[0], TypeInfo(T), count)
    else
      System.Move(fIterator.Items[0], values[index], SizeOf(T) * count);
end;

function TArrayIterator<T>.GetCount: Integer;
begin
  Result := Length(fIterator.Items);
end;

function TArrayIterator<T>.GetIsEmpty: Boolean;
begin
  Result := fIterator.Items = nil;
end;

function TArrayIterator<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Length(fIterator.Items)), 'index');
{$ENDIF}

  Result := fIterator.Items[index];
end;

function TArrayIterator<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Length(fIterator.Items));
end;

function TArrayIterator<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, Length(fIterator.Items) - index);
end;

function TArrayIterator<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
var
  comparer: IEqualityComparer<T>;
begin
  comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));
  Result := TArray.IndexOf<T>(fIterator.Items, item, index, count, comparer);
end;

function TArrayIterator<T>.ToArray: TArray<T>;
begin
  Result := fIterator.Items;
  SetLength(Result, Length(Result));
end;

{$ENDREGION}


{$REGION 'TEnumerableIterator'}

constructor TEnumerableIterator.Create(source: TRefCountedObject; iterator: PIteratorRec);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fIterator := iterator.Clone;
end;

destructor TEnumerableIterator.Destroy;
begin
  fIterator.Finalize;
  FreeMem(fIterator);
  fSource._Release;
  inherited;
end;

function TEnumerableIterator.MoveNext: Boolean;
begin
  case fState of
    Initial, Started:
    begin
      if fState = Initial then
      begin
        Start;
        fState := Started;
      end;

      Result := fIterator.MoveNext(fIterator);
      if Result then
        Exit;

      fIterator.Finalize;
      fState := Finished;
    end;
  end;
  Result := False;
end;

procedure TEnumerableIterator.Start;
var
  startProc: TStartProc;
begin
  startProc := fIterator.Start;
  if Assigned(startProc) then
    startProc(fIterator);
end;

{$ENDREGION}


{$REGION 'TEnumerableIterator<T>.TEnumerator'}

function TEnumerableIteratorBase<T>.TEnumerator.GetCurrent: T;
type
  TIteratorRec = TIteratorRec<T>;
  PIteratorRec = ^TIteratorRec;
begin
  Result := PIteratorRec(fIterator).Current;
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

procedure TIteratorRec.GetEnumerator;
begin
  Enumerator := Source.GetEnumerator;
end;

procedure TIteratorRec.GetSecondEnumerator;
begin
  Enumerator := IEnumerable(Predicate).GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TIteratorRec<T>'}

class function TIteratorRec<T>.GetCurrent(const enumerator: IEnumerator): TValue;
var
  current: T;
begin
  current := IEnumerator<T>(enumerator).Current;
  TValue.Make(@current, System.TypeInfo(T), Result);
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
  Result := Count < Length(Items);
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
  while (Count > 0) and Enumerator.MoveNext do
    Dec(Count);
  Result := Enumerator.MoveNext;
  if Result then
    Current := Enumerator.Current;
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
  Result := (Count > 0) and Enumerator.MoveNext;
  if Result then
  begin
    Current := Enumerator.Current;
    Dec(Count);
  end;
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

procedure TIteratorRec<T>.ToArray;
begin
  Items := Source.ToArray;
  case Count of // using Count as flag to determine how to treat the array
    TIteratorRec.Ordered:
    begin
      TArray.Sort<T>(Items, IComparer<T>(Predicate));
      Count := 0;
    end;
    TIteratorRec.Reversed:
      Count := Length(Items);
    TIteratorRec.Shuffled:
    begin
      TArray.Shuffle<T>(Items);
      Count := 0;
    end;
  end;
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


end.
