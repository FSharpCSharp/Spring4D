{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Events;

type
  /// <summary>
  ///   Provides an abstract implementation for the <see cref="Spring.Collections|IEnumerable" />
  ///    interface.
  /// </summary>
  TAbstractEnumerable = class abstract(TObject, IInterface)
{$IFNDEF AUTOREFCOUNT}
  private const
    objDestroyingFlag = Integer($80000000);
    function GetRefCount: Integer; inline;
{$ENDIF}
  protected
{$IFNDEF AUTOREFCOUNT}
{$IF Declared(VolatileAttribute)}
    [Volatile]
{$IFEND}
    fRefCount: Integer;
    class procedure __MarkDestroying(const obj); static; inline;
{$ENDIF}
    function QueryInterface(const IID: TGUID; out obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function AsObject: TObject;
  public
{$IFNDEF AUTOREFCOUNT}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read GetRefCount;
{$ENDIF}
  end;

  /// <summary>
  ///   Provides a default implementation for the <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />
  ///    interface.
  /// </summary>
  TEnumerableBase<T> = class abstract(TAbstractEnumerable, IEnumerable)
  private type
    TEnumerator = class(TInterfacedObject, IEnumerator)
    private
      fSource: IEnumerator<T>;
      function GetCurrent: TValue;
    public
      constructor Create(const source: IEnumerator<T>);
      function MoveNext: Boolean;
    end;
  private
    fComparer: IComparer<T>;
  {$REGION 'Implements IEnumerable'}
    function GetCountNonGeneric: Integer;
    function GetEnumeratorNonGeneric: IEnumerator;
    function GetIsEmptyNonGeneric: Boolean;
    function IEnumerable.GetCount = GetCountNonGeneric;
    function IEnumerable.GetEnumerator = GetEnumeratorNonGeneric;
    function IEnumerable.GetIsEmpty = GetIsEmptyNonGeneric;
  {$ENDREGION}
  protected
    this: IEnumerable<T>;
  {$REGION 'Property Accessors'}
    function GetComparer: IComparer<T>;
    function GetCount: Integer;
    function GetElementType: PTypeInfo; virtual;
    function GetIsEmpty: Boolean;
    function GetUseComparer: Boolean; {$IFNDEF DELPHIXE7_UP}inline;{$ENDIF}
  {$ENDREGION}
  {$REGION 'Implements IEqualityComparer<T>'}
    function GetHashCode(const value: T): Integer; reintroduce; inline;
    function Equals(const left, right: T): Boolean; reintroduce; inline;
  {$ENDREGION}
    function TryGetElementAt(out value: T; index: Integer): Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; overload;
    function TryGetFirst(out value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetLast(out value: T): Boolean; overload;
    function TryGetLast(out value: T; const predicate: Predicate<T>): Boolean; overload;
    function TryGetSingle(out value: T): Boolean; overload;
    function TryGetSingle(out value: T; const predicate: Predicate<T>): Boolean; overload;
    property Comparer: IComparer<T> read fComparer;
    property UseComparer: Boolean read GetUseComparer;
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

  TEqualityComparerWrapper<T> = class(TInterfacedObject, IEqualityComparer<T>)
  private
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fEnumerable: TEnumerableBase<T>;
    constructor Create(const enumerable: TEnumerableBase<T>);
  public
    class function Construct(const enumerable: TEnumerableBase<T>): IEqualityComparer<T>; static;
    destructor Destroy; override;
    function Equals(const left, right: T): Boolean; reintroduce;
    function GetHashCode(const value: T): Integer; reintroduce;
  end;

  TIterator<T> = class abstract(TEnumerableBase<T>, IEnumerator, IEnumerator<T>)
  private
    fCurrent: T;
    fInitialThreadId: TThreadID;
    fState: Integer;
    const
      STATE_INITIAL    = -2; // initial state, before GetEnumerator
      STATE_FINISHED   = -1; // end of enumerator
      STATE_ENUMERATOR = 0;  // before calling MoveNext
      STATE_RUNNING    = 1;  // enumeration is running
    function GetCurrentNonGeneric: TValue;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
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

  TSourceIterator<T> = class abstract(TIterator<T>)
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

    procedure RemoveAll(const predicate: Predicate<T>);
    procedure RemoveRange(const values: array of T); overload;
    procedure RemoveRange(const values: IEnumerable<T>); overload;

    function ExtractAll(const predicate: Predicate<T>): IReadOnlyList<T>;
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const values: IEnumerable<T>); overload;

    procedure CopyTo(var values: TArray<T>; index: Integer);
    function MoveTo(const collection: ICollection<T>): Integer; overload;
    function MoveTo(const collection: ICollection<T>;
      const predicate: Predicate<T>): Integer; overload;
  end;

  TContainedCollectionBase<T> = class abstract(TCollectionBase<T>)
  private
    fController: Pointer;
  protected
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  {$ENDREGION}
  public
    constructor Create(const controller: IInterface);
  end;

  TContainedReadOnlyCollection<T> = class abstract(TEnumerableBase<T>)
  private
    fController: Pointer;
  protected
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  {$ENDREGION}
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
  {$ENDREGION}
  public
    constructor Create(const controller: IInterface);
  end;

  TMapBase<TKey, T> = class abstract(TCollectionBase<TPair<TKey, T>>)
  private
    type
      TKeyValuePair = Generics.Collections.TPair<TKey, T>;
  protected
    fOnKeyChanged: TCollectionChangedEventImpl<TKey>;
    fOnValueChanged: TCollectionChangedEventImpl<T>;
  {$REGION 'Property Accessors'}
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<T>;
    function GetKeyType: PTypeInfo; inline;
    function GetValueType: PTypeInfo; inline;
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
    function CreateList: IList<T>; virtual;
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

const
  CountMask = Integer($7FFFFFFF);
  BitMask: array[Boolean] of Integer = (0, not CountMask);

implementation

uses
  Classes,
  Rtti,
  TypInfo,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractEnumerable'}

function TAbstractEnumerable.AsObject: TObject;
begin
  Result := Self;
end;

{$IFNDEF AUTOREFCOUNT}
function TAbstractEnumerable.GetRefCount: Integer;
begin
  Result := fRefCount and not objDestroyingFlag;
end;

class procedure TAbstractEnumerable.__MarkDestroying(const obj);
var
  refCount: Integer;
begin
  repeat
    refCount := TAbstractEnumerable(obj).fRefCount;
  until AtomicCmpExchange(TAbstractEnumerable(obj).fRefCount, refCount or objDestroyingFlag, refCount) = refCount;
end;

procedure TAbstractEnumerable.AfterConstruction;
begin
  AtomicDecrement(fRefCount);
end;

procedure TAbstractEnumerable.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

class function TAbstractEnumerable.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TAbstractEnumerable(Result).fRefCount := 1;
end;
{$ENDIF AUTOREFCOUNT}

function TAbstractEnumerable.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TAbstractEnumerable._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(fRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TAbstractEnumerable._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(fRefCount);
  if Result = 0 then
  begin
    __MarkDestroying(Self);
    Destroy;
  end;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

constructor TEnumerableBase<T>.Create;
begin
  inherited Create;
  if TType.Kind<T> = tkClass then
    fComparer := IComparer<T>(GetInstanceComparer)
  else
    fComparer := TComparer<T>.Default;

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

  Result := TConcatIterator<T>.Create(this, second);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
begin
  Result := this.Contains(value, TEqualityComparerWrapper<T>.Construct(Self));
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
  Result := this.Contains(value, TEqualityComparer<T>.Construct(comparer, nil));
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
    if UseComparer then
      Result := fComparer.Compare(left, right) = 0
{$IFDEF DELPHIXE7_UP}
    else
      case TType.Kind<T> of
        tkUString: Result := PString(@left)^ = PString(@right)^
      else
        case SizeOf(T) of
          1: Result := PByte(@left)^ = PByte(@right)^;
          2: Result := PWord(@left)^ = PWord(@right)^;
          4: Result := PCardinal(@left)^ = PCardinal(@right)^;
          8: Result := PUInt64(@left)^ = PUInt64(@right)^;
        end;
      end;
{$ENDIF}
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
begin
  Result := this.EqualsTo(values, TEqualityComparerWrapper<T>.Construct(Self));
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

function TEnumerableBase<T>.GetCountNonGeneric: Integer;
begin
  Result := this.Count;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerableBase<T>.GetEnumeratorNonGeneric: IEnumerator;
begin
  Result := TEnumerator.Create(this.GetEnumerator);
end;

function TEnumerableBase<T>.GetHashCode(const value: T): Integer;
begin
  raise Error.NotSupported;
end;

function TEnumerableBase<T>.GetIsEmpty: Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := this.GetEnumerator;
  Result := not enumerator.MoveNext;
end;

function TEnumerableBase<T>.GetIsEmptyNonGeneric: Boolean;
begin
  Result := this.IsEmpty;
end;

function TEnumerableBase<T>.GetUseComparer: Boolean;
{$IFDEF DELPHIXE7_UP}
const
  FastComparableTypes = [tkUnknown, tkInteger, tkChar, tkEnumeration, tkSet,
                         tkClass, tkMethod, tkWChar, tkInterface, tkInt64,
                         tkUString, tkClassRef, tkPointer, tkProcedure];
begin
  Result := not ((TType.Kind<T> in FastComparableTypes) and (SizeOf(T) in [1, 2, 4, 8])
    and ((fComparer = nil) or (Pointer(fComparer) = _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)))));
{$ELSE}
begin
  Result := True;
{$ENDIF}
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
begin
  Result := TEnumerable.Max<T>(this, selector);
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
begin
  Result := TEnumerable.Min<T>(this, selector);
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
  Result := TOrderedIterator<T>.Create(this, fComparer);
end;

function TEnumerableBase<T>.Ordered(
  const comparer: IComparer<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  Result := TOrderedIterator<T>.Create(this, comparer);
end;

function TEnumerableBase<T>.Ordered(
  const comparer: TComparison<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  Result := Ordered(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TReversedIterator<T>.Create(this);
end;

function TEnumerableBase<T>.Shuffled: IEnumerable<T>;
var
  items: TArray<T>;
begin
  items := ToArray;
  TArray.Shuffle<T>(items);
  Result := TArrayIterator<T>.Create(items);
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
  Result := TSkipIterator<T>.Create(this, count);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TSkipWhileIterator<T>.Create(this, predicate);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TSkipWhileIndexIterator<T>.Create(this, predicate);
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
  Result := TTakeIterator<T>.Create(this, count);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TTakeWhileIterator<T>.Create(this, predicate);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: Func<T, Integer, Boolean>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TTakeWhileIndexIterator<T>.Create(this, predicate);
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

function TEnumerableBase<T>.Where(
  const predicate: Predicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TWhereIterator<T>.Create(this, predicate);
end;

{$ENDREGION}


{$REGION 'TWrappedEqualityComparer<T>'}

constructor TEqualityComparerWrapper<T>.Create(
  const enumerable: TEnumerableBase<T>);
begin
  inherited Create;
  fEnumerable := enumerable;
  fEnumerable._AddRef;
end;

destructor TEqualityComparerWrapper<T>.Destroy;
begin
  fEnumerable._Release;
  inherited;
end;

function TEqualityComparerWrapper<T>.Equals(const left, right: T): Boolean;
begin
  Result := fEnumerable.Equals(left, right);
end;

function TEqualityComparerWrapper<T>.GetHashCode(const value: T): Integer;
begin
  Result := fEnumerable.GetHashCode(value);
end;

class function TEqualityComparerWrapper<T>.Construct(
  const enumerable: TEnumerableBase<T>): IEqualityComparer<T>;
begin
  Result := TEqualityComparerWrapper<T>.Create(enumerable);
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>.TEnumerator'}

constructor TEnumerableBase<T>.TEnumerator.Create(const source: IEnumerator<T>);
begin
  inherited Create;
  fSource := source;
end;

function TEnumerableBase<T>.TEnumerator.GetCurrent: TValue;
begin
  Result := TValue.From<T>(fSource.Current);
end;

function TEnumerableBase<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TIterator<T>'}

constructor TIterator<T>.Create;
begin
  inherited Create;
  fState := STATE_INITIAL;
  fInitialThreadId := TThread.CurrentThread.ThreadID;
end;

procedure TIterator<T>.Dispose;
begin
end;

function TIterator<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TIterator<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(GetCurrent);
end;

function TIterator<T>.GetEnumerator: IEnumerator<T>;
var
  iterator: TIterator<T>;
begin
  if (fInitialThreadId = TThread.CurrentThread.ThreadID) and (fState = STATE_INITIAL) then
  begin
    fState := STATE_ENUMERATOR;
    Result := Self;
  end
  else
  begin
    iterator := Clone;
    iterator.fState := STATE_ENUMERATOR;
    Result := iterator;
  end;
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

function TCollectionBase<T>.ExtractAll(const predicate: Predicate<T>): IReadOnlyList<T>;
var
  items: TArray<T>;
begin
  items := this.Where(predicate).ToArray;
  ExtractRange(items);
  Result := TArrayIterator<T>.Create(items);
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
  const predicate: Predicate<T>): Integer;
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
    if not Assigned(predicate) or predicate(values[i]) then
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

procedure TCollectionBase<T>.RemoveRange(const values: array of T);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    ICollection<T>(this).Remove(values[i]);
end;

procedure TCollectionBase<T>.RemoveAll(const predicate: Predicate<T>);
begin
  RemoveRange(this.Where(predicate).ToArray);
end;

procedure TCollectionBase<T>.RemoveRange(const values: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  for item in values do
    ICollection<T>(this).Remove(item);
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


{$REGION 'TContainedCollectionBase<T>'}

constructor TContainedCollectionBase<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedCollectionBase<T>._AddRef: Integer;
begin
  Result := IInterface(fController)._AddRef;
end;

function TContainedCollectionBase<T>._Release: Integer;
begin
  Result := IInterface(fController)._Release;
end;

{$ENDREGION}


{$REGION 'TContainedReadOnlyCollection<T>'}

constructor TContainedReadOnlyCollection<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedReadOnlyCollection<T>.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

function TContainedReadOnlyCollection<T>._AddRef: Integer;
begin
  Result := IInterface(fController)._AddRef;
end;

function TContainedReadOnlyCollection<T>._Release: Integer;
begin
  Result := IInterface(fController)._Release;
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
begin
  Result := IList<T>(this).IndexOf(item, 0, this.Count);
end;

function TListBase<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IList<T>(this).IndexOf(item, index, this.Count - index);
end;

function TListBase<T>.LastIndexOf(const item: T): Integer;
var
  count: Integer;
begin
  count := this.Count;
  Result := IList<T>(this).LastIndexOf(item, count - 1, count);
end;

function TListBase<T>.LastIndexOf(const item: T; index: Integer): Integer;
begin
  Result := IList<T>(this).LastIndexOf(item, index, index + 1);
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


end.
