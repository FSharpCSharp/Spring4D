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

unit Spring.Collections.Lists;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  /// <summary>
  ///   Represents a strongly typed list of elements that can be accessed by
  ///   index. Provides methods to search, sort, and manipulate lists.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the list.
  /// </typeparam>
  TAbstractArrayList<T> = class(TListBase<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TAbstractArrayList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: TAbstractArrayList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
      TArrayManager = TArrayManager<T>;
  {$ENDREGION}
  private
    fItems: TArray<T>;
    fCount: Integer;
    fVersion: Integer;
    procedure DeleteInternal(index: Integer; notification: TCollectionChangedAction); inline;
    procedure DeleteRangeInternal(index, count: Integer; doClear: Boolean);
    procedure InsertRangeInternal(index, count: Integer; const values: array of T);
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): T;
    function GetOwnsObjects: Boolean; inline;
    procedure SetCapacity(value: Integer);
    procedure SetCount(value: Integer);
    procedure SetItem(index: Integer; const value: T);
  {$ENDREGION}
    procedure EnsureCapacity(capacity: Integer); inline;
    procedure Grow(capacity: Integer);

    function TryGetElementAt(out value: T; index: Integer): Boolean; override;
    function TryGetFirst(out value: T): Boolean;
    function TryGetLast(out value: T): Boolean;
    function TryGetSingle(out value: T): Boolean;

    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function Contains(const value: T): Boolean; overload;
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    function Single: T;
    function SingleOrDefault(const defaultValue: T): T;

    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    procedure RemoveAll(const predicate: Predicate<T>);

    function Extract(const item: T): T;
    function ExtractAll(const predicate: Predicate<T>): IReadOnlyList<T>;

    procedure Clear;

    procedure CopyTo(var values: TArray<T>; index: Integer);
    function MoveTo(const collection: ICollection<T>): Integer; overload;
    function MoveTo(const collection: ICollection<T>; const predicate: Predicate<T>): Integer; overload;
  {$ENDREGION}

  {$REGION 'Implements IList<T>'}
    function Add(const item: T): Integer; overload;

    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const values: array of T); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable<T>); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    function ExtractAt(index: Integer): T;
    function ExtractRange(index, count: Integer): TArray<T>; overload;

    function GetRange(index, count: Integer): IList<T>;

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse(index, count: Integer); overload;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    function AsReadOnlyList: IReadOnlyList<T>;

    procedure TrimExcess;
  {$ENDREGION}
  end;

  TList<T> = class(TAbstractArrayList<T>, IEnumerable<T>, ICollection<T>, IList<T>,
    IReadOnlyCollection<T>, IReadOnlyList<T>);

  TObjectList<T: class> = class(TList<T>)
  private
  {$REGION 'Property Accessors'}
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
  end;

  TSortedList<T> = class(TAbstractArrayList<T>, IEnumerable<T>, ICollection<T>, IList<T>,
    IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
    procedure SetItem(index: Integer; const value: T);
    function AsReadOnlyList: IReadOnlyList<T>;
  public
  {$REGION 'Implements IEnumerable<T>'}
    function Contains(const value: T): Boolean;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;
  {$ENDREGION}

  {$REGION 'Implements IList<T>'}
    function Add(const item: T): Integer; overload;
    procedure Insert(index: Integer; const item: T);

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse(index, count: Integer); overload;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;
  {$ENDREGION}
  end;

  TSortedObjectList<T: class> = class(TSortedList<T>)
  private
  {$REGION 'Property Accessors'}
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;
  end;

  TCollectionList<T: TCollectionItem> = class(TListBase<T>, IEnumerable<T>,
    ICollection<T>, IList<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TCollectionList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const list: TCollectionList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fCollection: TCollection;
    fVersion: Integer;
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): T;
    function GetOwnsObjects: Boolean;
    procedure SetCapacity(value: Integer);
    procedure SetCount(value: Integer);
    procedure SetItem(index: Integer; const value: T);
  {$ENDREGION}
    procedure DeleteRangeInternal(index, count: Integer; doClear: Boolean);
    function AsReadOnlyList: IReadOnlyList<T>;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const collection: TCollection);

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollections<T>'}
    function Extract(const item: T): T;

    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IList<T>'}
    function Add(const item: T): Integer; overload;

    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const values: array of T); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable<T>); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    function ExtractAt(index: Integer): T;
    function ExtractRange(index, count: Integer): TArray<T>; overload;

    function GetRange(index, count: Integer): IList<T>;

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    procedure Reverse(index, count: Integer); overload;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;

    procedure TrimExcess;
  {$ENDREGION}
  end;

  TAnonymousReadOnlyList<T> = class(TEnumerableBase<T>, IReadOnlyCollection<T>,
    IReadOnlyList<T>)
  private
    fCount: Func<Integer>;
    fItems: Func<Integer, T>;
    fIterator: IEnumerable<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    constructor Create(const count: Func<Integer>; const items: Func<Integer, T>;
      const iterator: IEnumerable<T>{$IFDEF DELPHIXE3_UP} = nil{$ENDIF});

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
  {$ENDREGION}

  {$REGION 'Implements IList<T>'}
    function GetRange(index, count: Integer): IList<T>;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TFoldedObjectList<T{: class}> = class(TObjectList<TObject>)
  protected
    function CreateList: IList<TObject>; override;
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedInterfaceList<T{: IInterface}> = class(TList<IInterface>)
  protected
    function CreateList: IList<IInterface>; override;
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedSortedObjectList<T{: class}> = class(TSortedObjectList<TObject>)
  protected
    function CreateList: IList<TObject>; override;
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedSortedInterfaceList<T{: IInterface}> = class(TSortedList<IInterface>)
  protected
    function CreateList: IList<IInterface>; override;
    function GetElementType: PTypeInfo; override;
  end;

  TFoldedObjectList = class(TObjectList<TObject>)
  private
    fElementType: PTypeInfo;
  protected
    function CreateList: IList<TObject>; override;
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const elementType: PTypeInfo;
      const comparer: IComparer<TObject>; ownsObjects: Boolean = True);
  end;

  TFoldedInterfaceList = class(TList<IInterface>)
  private
    fElementType: PTypeInfo;
  protected
    function CreateList: IList<IInterface>; override;
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(const elementType: PTypeInfo;
      const comparer: IComparer<IInterface>);
  end;

  TObservableList<T: class> = class(TFoldedObjectList<T>, INotifyPropertyChanged)
  private
    fOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(sender: TObject;
      const eventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const propertyName: string);
    procedure Changed(const value: TObject; action: TCollectionChangedAction); override;
  public
    constructor Create; override;

    property OnPropertyChanged: IEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TArrayHelper = record
    class function IndexOf1(const item; const items: Pointer; count: Integer): Integer; static;
    class function IndexOf2(const item; const items: Pointer; count: Integer): Integer; static;
    class function IndexOf4(const item; const items: Pointer; count: Integer): Integer; static;
    class function IndexOf8(const item; const items: Pointer; count: Integer): Integer; static;
    class function IndexOfObj(const item; const items: Pointer; count: Integer): Integer; static;
    class function IndexOfStr(const item; const items: Pointer; count: Integer): Integer; static;
  end;

implementation

uses
{$IFDEF DELPHIXE4}
  Rtti, // suppress hint about inlining
{$ENDIF}
  TypInfo,
  Spring.Collections.Extensions,
  Spring.Events,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TArrayHelper'}

class function TArrayHelper.IndexOf1(const item; const items: Pointer;
  count: Integer): Integer;
var
  p: PByte;
begin
  p := items;
  for Result := 0 to count - 1 do
  begin
    if p^ = Byte(item) then
      Exit;
    Inc(p);
  end;
  Result := -1;
end;

class function TArrayHelper.IndexOf2(const item; const items: Pointer;
  count: Integer): Integer;
var
  p: PWord;
begin
  p := items;
  for Result := 0 to count - 1 do
  begin
    if p^ = Word(item) then
      Exit;
    Inc(p);
  end;
  Result := -1;
end;

class function TArrayHelper.IndexOf4(const item; const items: Pointer;
  count: Integer): Integer;
var
  p: PCardinal;
begin
  p := items;
  for Result := 0 to count - 1 do
  begin
    if p^ = Cardinal(item) then
      Exit;
    Inc(p);
  end;
  Result := -1;
end;

class function TArrayHelper.IndexOf8(const item; const items: Pointer;
  count: Integer): Integer;
var
  p: PUInt64;
begin
  p := items;
  for Result := 0 to count - 1 do
  begin
    if p^ = UInt64(item) then
      Exit;
    Inc(p);
  end;
  Result := -1;
end;

class function TArrayHelper.IndexOfObj(const item; const items: Pointer;
  count: Integer): Integer;
var
  p: PObject;
begin
  p := items;
  for Result := 0 to count - 1 do
  begin
    if p^ = nil then
    begin
      if TObject(item) = nil then
        Exit;
    end
    else if p^.Equals(TObject(item)) then
      Exit;
    Inc(p);
  end;
  Result := -1;
end;

class function TArrayHelper.IndexOfStr(const item; const items: Pointer;
  count: Integer): Integer;
var
  p: PString;
begin
  p := items;
  for Result := 0 to count - 1 do
  begin
    if p^ = string(item) then
      Exit;
    Inc(p);
  end;
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TAbstractArrayList<T>'}

constructor TAbstractArrayList<T>.Create(const values: array of T);
var
  len, i: Integer;
begin
  Create;
  len := Length(values);
  if len > 0 then
  begin
    SetCount(len);
    for i := Low(values) to High(values) do
      fItems[i] := values[i];
  end;
end;

constructor TAbstractArrayList<T>.Create(const values: IEnumerable<T>);
var
  collection: ICollection<T>;
  len: Integer;
begin
  Create;
  if Supports(values, ICollection<T>, collection) then
  begin
    len := collection.Count;
    if len > 0 then
    begin
      SetCount(len);
      collection.CopyTo(fItems, 0);
    end;
  end
  else
    IList<T>(this).AddRange(values);
end;

destructor TAbstractArrayList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TAbstractArrayList<T>.GetCapacity: Integer;
begin
  Result := DynArrayLength(fItems);
end;

function TAbstractArrayList<T>.GetCount: Integer;
begin
  Result := fCount and CountMask;
end;

function TAbstractArrayList<T>.GetOwnsObjects: Boolean;
begin
  Result := fCount < 0;
end;

function TAbstractArrayList<T>.Add(const item: T): Integer;
begin
  Result := Count;
  if Result = DynArrayLength(fItems) then
    Grow(Result + 1);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[Result] := item;
  Inc(fCount);

  DoNotify(item, caAdded);
end;

procedure TAbstractArrayList<T>.AddRange(const values: array of T);
begin
  InsertRange(Count, values);
end;

procedure TAbstractArrayList<T>.AddRange(const values: IEnumerable<T>);
begin
  InsertRange(Count, values);
end;

procedure TAbstractArrayList<T>.EnsureCapacity(capacity: Integer);
begin
  if capacity > DynArrayLength(fItems) then
    Grow(capacity)
  else if capacity < 0 then
    OutOfMemoryError;
end;

function TAbstractArrayList<T>.GetEnumerator: IEnumerator<T>;
begin
  if TType.Kind<T> = tkClass then
    IEnumerator<TObject>(Result) := TList<TObject>.TEnumerator.Create(TList<TObject>(Self))
  else
    Result := TEnumerator.Create(Self);
end;

function TAbstractArrayList<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TAbstractArrayList<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := fItems[index];
end;

function TAbstractArrayList<T>.GetRange(index, count: Integer): IList<T>;
var
  list: TList<T>;
{$IFNDEF DELPHIXE2_UP}
  i: Integer;
{$ENDIF}
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  Result := CreateList;
  list := TList<T>(Result.AsObject);
  list.fCount := (list.fCount and not CountMask) or count;
{$IFDEF DELPHIXE2_UP}
  list.fItems := Copy(fItems, index, count);
{$ELSE}
  // the compiler passes wrong typeinfo for the generated call
  // to _DynArrayCopyRange up to XE
  SetLength(list.fItems, count);
  for i := 0 to count - 1 do
  begin
    list.fItems[i] := fItems[index];
    Inc(index);
  end;
{$ENDIF}
end;

procedure TAbstractArrayList<T>.Grow(capacity: Integer);
var
  newCapacity: Integer;
begin
  newCapacity := DynArrayLength(fItems);
  if newCapacity = 0 then
    newCapacity := capacity
  else
    repeat
      newCapacity := newCapacity * 2;
      if newCapacity < 0 then
        OutOfMemoryError;
    until newCapacity >= capacity;
  SetCapacity(newCapacity);
end;

function TAbstractArrayList<T>.IndexOf(const item: T): Integer;
var
  p: ^T;
begin
  if UseComparer then
  begin
    p := Pointer(fItems);
    for Result := 0 to Count - 1 do
    begin
      if Equals(p^, item) then
        Exit;
      Inc(p);
    end;
    Result := -1;
  end
{$IFDEF DELPHIXE7_UP}
  else
    case TType.Kind<T> of
      tkClass: Result := TArrayHelper.IndexOfObj(item, fItems, Count);
      tkUString: Result := TArrayHelper.IndexOfStr(item, fItems, Count);
    else
      case SizeOf(T) of
        1: Result := TArrayHelper.IndexOf1(item, fItems, Count);
        2: Result := TArrayHelper.IndexOf2(item, fItems, Count);
        4: Result := TArrayHelper.IndexOf4(item, fItems, Count);
        8: Result := TArrayHelper.IndexOf8(item, fItems, Count);
      end;
    end;
{$ENDIF}
end;

function TAbstractArrayList<T>.IndexOf(const item: T; index: Integer): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  for i := index to Count - 1 do
    if Equals(fItems[i], item) then
      Exit(i);
  Result := -1;
end;

function TAbstractArrayList<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  for i := index to index + count - 1 do
    if Equals(fItems[i], item) then
      Exit(i);
  Result := -1;
end;

procedure TAbstractArrayList<T>.SetItem(index: Integer; const value: T);
var
  oldItem: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  oldItem := fItems[index];

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[index] := value;

  if Assigned(Notify) then
  begin
    Notify(Self, oldItem, caRemoved);
    if {$IFDEF DELPHIXE7_UP}(TType.Kind<T> = tkClass) and {$ENDIF}OwnsObjects then
      FreeObject(oldItem);
    Notify(Self, value, caAdded);
  end
  else
    if {$IFDEF DELPHIXE7_UP}(TType.Kind<T> = tkClass) and {$ENDIF}OwnsObjects then
      FreeObject(oldItem);
end;

function TAbstractArrayList<T>.Single: T;
begin
  case Count of
    0: raise Error.NoElements;
    1: Result := fItems[0];
  else
    raise Error.MoreThanOneElement;
  end;
end;

function TAbstractArrayList<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := fItems[0];
  else
    raise Error.MoreThanOneElement;
  end;
end;

procedure TAbstractArrayList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  EnsureCapacity(Count + 1);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if index <> Count then
  begin
    TArrayManager.Move(fItems, index, index + 1, Count - index);
    TArrayManager.Finalize(fItems, index, 1);
  end;
  fItems[index] := item;
  Inc(fCount);

  DoNotify(item, caAdded);
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer; const values: array of T);
var
  count: Integer;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  count := Length(values);
  if count = 0 then
    Exit;

  InsertRangeInternal(index, count, values);
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer;
  const values: IEnumerable<T>);
var
  list: TList<T>;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  TObject(list) := values.AsObject;
  if TObject(list) is TList<T> then
  begin
    if list.Count = 0 then
      Exit;

    InsertRangeInternal(index, list.Count, list.fItems);
  end
  else
  begin
    for item in values do
    begin
      Insert(index, item);
      Inc(index);
    end;
  end;
end;

procedure TAbstractArrayList<T>.InsertRangeInternal(index, count: Integer;
  const values: array of T);
var
  i: Integer;
begin
  EnsureCapacity(Self.Count + count);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if index <> Self.Count then
  begin
    TArrayManager.Move(fItems, index, index + count, Self.Count - index);
    TArrayManager.Finalize(fItems, index, count);
  end;

  if not TType.IsManaged<T>{$IFDEF WEAKREF} and not TType.HasWeakRef<T>{$ENDIF} then
    System.Move(values[0], fItems[index], count * SizeOf(T))
  else
    for i := Low(values) to count - 1 do
      fItems[index + i] := values[i];

  Inc(fCount, count);

  if Assigned(Notify) then
    for i := Low(values) to count - 1 do
      Notify(Self, values[i], caAdded);
end;

function TAbstractArrayList<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
begin
  Result := TArray.LastIndexOf<T>(fItems, item, index, count);
end;

procedure TAbstractArrayList<T>.DeleteInternal(index: Integer;
  notification: TCollectionChangedAction);
var
  oldItem: T;
begin
  oldItem := fItems[index];

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[index] := Default(T);
  Dec(fCount);
  if index <> Count then
  begin
    TArrayManager.Move(fItems, index + 1, index, Count - index);
    TArrayManager.Finalize(fItems, Count, 1);
  end;

  if Assigned(Notify) then
    Notify(Self, oldItem, notification);
  if {$IFDEF DELPHIXE7_UP}(TType.Kind<T> = tkClass) and {$ENDIF}OwnsObjects then
    if (notification = caRemoved) then
      FreeObject(oldItem);
end;

procedure TAbstractArrayList<T>.DeleteRange(index, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  DeleteRangeInternal(index, count, False);
end;

procedure TAbstractArrayList<T>.DeleteRangeInternal(index, count: Integer; doClear: Boolean);
var
  oldItems: TArray<T>;
  tailCount,
  i: Integer;
begin
  SetLength(oldItems, count);
  TArrayManager.Move(fItems, oldItems, index, 0, count);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  tailCount := Self.Count - (index + count);
  if tailCount > 0 then
  begin
    TArrayManager.Move(fItems, index + count, index, tailCount);
    TArrayManager.Finalize(fItems, Self.Count - count, count);
  end
  else
    TArrayManager.Finalize(fItems, index, count);

  Dec(fCount, count);

  if doClear then
    Reset;

  if Assigned(Notify) then
    if {$IFDEF DELPHIXE7_UP}(TType.Kind<T> = tkClass) and {$ENDIF}OwnsObjects then
      for i := Low(oldItems) to DynArrayHigh(oldItems) do
      begin
        Notify(Self, oldItems[i], caRemoved);
        FreeObject(oldItems[i]);
      end
    else
      for i := Low(oldItems) to DynArrayHigh(oldItems) do
        Notify(Self, oldItems[i], caRemoved)
  else
    if {$IFDEF DELPHIXE7_UP}(TType.Kind<T> = tkClass) and {$ENDIF}OwnsObjects then
      for i := Low(oldItems) to DynArrayHigh(oldItems) do
        FreeObject(oldItems[i]);
end;

procedure TAbstractArrayList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  TArray.Sort<T>(fItems, comparer, index, count);

  Reset;
end;

procedure TAbstractArrayList<T>.Move(currentIndex, newIndex: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < Count), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < Count), 'newIndex');
{$ENDIF}

  temp := fItems[currentIndex];

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
    TArrayManager.Move(fItems, currentIndex + 1, currentIndex, newIndex - currentIndex)
  else
    TArrayManager.Move(fItems, newIndex, newIndex + 1, currentIndex - newIndex);

  TArrayManager.Finalize(fItems, newIndex, 1);
  fItems[newIndex] := temp;

  DoNotify(temp, caMoved);
end;

function TAbstractArrayList<T>.MoveTo(const collection: ICollection<T>): Integer;
begin
  Result := MoveTo(collection, nil);
end;

function TAbstractArrayList<T>.MoveTo(const collection: ICollection<T>;
  const predicate: Predicate<T>): Integer;
var
  i: Integer;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  Result := 0;
  i := 0;
  while i < Count do
    if not Assigned(predicate) or predicate(fItems[i]) then
    begin
      item := fItems[i];
      DeleteInternal(i, caExtracted);
      collection.Add(item);
      Inc(Result);
    end
    else
      Inc(i);
end;

function TAbstractArrayList<T>.AsReadOnlyList: IReadOnlyList<T>;
begin
  Result := Self as IReadOnlyList<T>;
end;

procedure TAbstractArrayList<T>.Clear;
begin
  if Count > 0 then
    DeleteRangeInternal(0, Count, True);
  SetCapacity(0);
end;

procedure TAbstractArrayList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < Count), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < Count), 'index2');
{$ENDIF}

  temp := fItems[index1];

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[index1] := fItems[index2];
  fItems[index2] := temp;

  if Assigned(Notify) then
  begin
    Notify(Self, fItems[index2], caMoved);
    Notify(Self, fItems[index1], caMoved);
  end;
end;

procedure TAbstractArrayList<T>.RemoveAll(const predicate: Predicate<T>);
var
  index: Integer;
begin
  index := 0;
  while index < Count do
    if predicate(fItems[index]) then
      DeleteInternal(index, caRemoved)
    else
      Inc(index);
end;

procedure TAbstractArrayList<T>.Reverse(index, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(index >= 0, 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  TArray.Reverse<T>(fItems, index, count);

  Reset;
end;

procedure TAbstractArrayList<T>.SetCapacity(value: Integer);
begin
  if value < Count then
    DeleteRange(value, Count - value);
  SetLength(fItems, value);
end;

procedure TAbstractArrayList<T>.SetCount(value: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(value >= 0, 'count');
{$ENDIF}

  if value > DynArrayLength(fItems) then
    SetCapacity(value);
  if value < Count then
    DeleteRange(value, Count - value);
  fCount := (fCount and not CountMask) or value;
end;

procedure TAbstractArrayList<T>.Delete(index: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  DeleteInternal(index, caRemoved);
end;

function TAbstractArrayList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := fItems[index];
    DeleteInternal(index, caExtracted);
  end;
end;

function TAbstractArrayList<T>.ExtractAt(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := fItems[index];
  DeleteInternal(index, caExtracted);
end;

function TAbstractArrayList<T>.ExtractAll(const predicate: Predicate<T>): IReadOnlyList<T>;
var
  index: Integer;
  list: IList<T>;
begin
  index := 0;
  list := CreateList;
  while index < Count do
    if predicate(fItems[index]) then
    begin
      list.Add(fItems[index]);
      DeleteInternal(index, caExtracted);
    end
    else
      Inc(index);
  list.TrimExcess;
  Result := list.AsReadOnlyList;
end;


function TAbstractArrayList<T>.ExtractRange(index, count: Integer): TArray<T>;
var
  tailCount, i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  SetLength(Result, count);
  TArrayManager.Move(fItems, Result, index, 0, count);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  tailCount := Self.Count - (index + count);
  if tailCount > 0 then
  begin
    TArrayManager.Move(fItems, index + count, index, tailCount);
    TArrayManager.Finalize(fItems, Self.Count - count, count);
  end
  else
    TArrayManager.Finalize(fItems, index, count);

  Dec(fCount, count);

  if Assigned(Notify) then
    for i := Low(Result) to DynArrayHigh(Result) do
      Notify(Self, Result[i], caExtracted);
end;

function TAbstractArrayList<T>.Contains(const value: T): Boolean;
begin
  Result := IndexOf(value) > -1;
end;

function TAbstractArrayList<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if comparer.Equals(value, fItems[i]) then
      Exit(True);
  Result := False;
end;

procedure TAbstractArrayList<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(Length(values), index, Count);
{$ENDIF}

  for i := 0 to Count - 1 do
  begin
    values[index] := fItems[i];
    Inc(index);
  end;
end;

function TAbstractArrayList<T>.ToArray: TArray<T>;
begin
  Result := fItems;
  SetLength(Result, Count);
end;

procedure TAbstractArrayList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TAbstractArrayList<T>.TryGetElementAt(out value: T; index: Integer): Boolean;
begin
  Result := (index >= 0) and (index < Count);
  if Result then
    value := fItems[index];
end;

function TAbstractArrayList<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[0]
  else
    value := Default(T);
end;

function TAbstractArrayList<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[Count - 1]
  else
    value := Default(T);
end;

function TAbstractArrayList<T>.TryGetSingle(out value: T): Boolean;
begin
  Result := Count = 1;
  if Result then
    value := fItems[0]
  else
    value := Default(T);
end;

{$ENDREGION}


{$REGION 'TAbstractArrayList<T>.TEnumerator'}

constructor TAbstractArrayList<T>.TEnumerator.Create(
  const source: TAbstractArrayList<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TAbstractArrayList<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TAbstractArrayList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TAbstractArrayList<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  Result := fIndex < fSource.Count;
  if Result then
  begin
    fCurrent := fSource.fItems[fIndex];
    Inc(fIndex);
  end
  else
    fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectList<T>'}

constructor TObjectList<T>.Create;
begin
  inherited Create;
  SetOwnsObjects(True);
end;

constructor TObjectList<T>.Create(ownsObjects: Boolean);
begin
  Create;
  SetOwnsObjects(ownsObjects);
end;

constructor TObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

procedure TObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fCount := (fCount and CountMask) or BitMask[value];
end;

{$ENDREGION}


{$REGION 'TSortedList<T>'}

function TSortedList<T>.Add(const item: T): Integer;
begin
  Result := Count;
  if Result > 0 then
  begin
    // If the new item is smaller than the last one in the list ...
    if Comparer.Compare(fItems[Result - 1], item) > 0 then
      // ... search for the correct insertion point
      TArray.BinarySearch<T>(fItems, item, Result, Comparer, 0, Count);
  end;
  inherited Insert(Result, item);
end;

procedure TSortedList<T>.AddRange(const values: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  for item in values do
    Add(item);
end;

procedure TSortedList<T>.AddRange(const values: array of T);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    Add(values[i]);
end;

function TSortedList<T>.AsReadOnlyList: IReadOnlyList<T>;
begin
  Result := Self;
end;

function TSortedList<T>.Contains(const value: T): Boolean;
var
  index: Integer;
begin
  Result := TArray.BinarySearch<T>(fItems, value, index, Comparer, 0, Count);
end;

procedure TSortedList<T>.Exchange(index1, index2: Integer);
begin
  raise Error.NotSupported;
end;

function TSortedList<T>.IndexOf(const item: T): Integer;
begin
  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, 0, Count) then
    Result := -1;
end;

function TSortedList<T>.IndexOf(const item: T; index: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, index, Count - index) then
    Result := -1;
end;

function TSortedList<T>.IndexOf(const item: T; index, count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, index, count) then
    Result := -1;
end;

procedure TSortedList<T>.Insert(index: Integer; const item: T);
begin
  raise Error.NotSupported;
end;

function TSortedList<T>.LastIndexOf(const item: T; index,
  count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  if not TArray.BinarySearchUpperBound<T>(
    fItems, item, Result, Comparer, index, count) then
    Result := -1;
end;

procedure TSortedList<T>.Move(currentIndex, newIndex: Integer);
begin
  raise Error.NotSupported;
end;

procedure TSortedList<T>.Reverse(index, count: Integer);
begin
  raise Error.NotSupported;
end;

procedure TSortedList<T>.SetItem(index: Integer; const value: T);
begin
  raise Error.NotSupported;
end;

procedure TSortedList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
end;

{$ENDREGION}


{$REGION 'TSortedObjectList<T>'}

constructor TSortedObjectList<T>.Create;
begin
  Create(True);
end;

constructor TSortedObjectList<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  SetOwnsObjects(ownsObjects);
end;

constructor TSortedObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

procedure TSortedObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fCount := (fCount and CountMask) or BitMask[value];
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>'}

constructor TCollectionList<T>.Create(const collection: TCollection);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(collection, 'collection');
  Guard.CheckInheritsFrom(collection.ItemClass, TClass(T), 'collection.ItemClass');
{$ENDIF}

  inherited Create;
  fCollection := collection;
end;

function TCollectionList<T>.Add(const item: T): Integer;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  item.Collection := fCollection;
  Result := item.Index;

  DoNotify(item, caAdded);
end;

function TCollectionList<T>.AsReadOnlyList: IReadOnlyList<T>;
begin
  Result := Self;
end;

procedure TCollectionList<T>.Clear;
begin
  if fCollection.Count > 0 then
    DeleteRangeInternal(0, fCollection.Count, True);
end;

procedure TCollectionList<T>.Delete(index: Integer);
var
  oldItem: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
{$ENDIF}

  oldItem := T(fCollection.Items[index]);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  oldItem.Collection := nil;

  DoNotify(oldItem, caRemoved);
  oldItem.Free;
end;

procedure TCollectionList<T>.DeleteRange(index, count: Integer);
begin
  DeleteRangeInternal(index, count, False);
end;

procedure TCollectionList<T>.DeleteRangeInternal(index, count: Integer;
  doClear: Boolean);
var
  oldItems: array of T;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCollection.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  SetLength(oldItems, count);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  for i := count downto 1 do
  begin
    oldItems[count - i] := T(fCollection.Items[index]);
    fCollection.Items[index].Collection := nil;
  end;

  if doClear then
    Reset;

  if Assigned(Notify) then
    for i := Low(oldItems) to DynArrayHigh(oldItems) do
    begin
      Notify(Self, oldItems[i], caRemoved);
      oldItems[i].Free;
    end
  else
    for i := Low(oldItems) to DynArrayHigh(oldItems) do
      oldItems[i].Free;
end;

procedure TCollectionList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < fCollection.Count), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < fCollection.Count), 'index2');
{$ENDIF}

  temp := T(fCollection.Items[index1]);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fCollection.Items[index2].Index := index1;
  temp.Index := index2;

  if Assigned(Notify) then
  begin
    Notify(Self, T(fCollection.Items[index2]), caMoved);
    Notify(Self, T(fCollection.Items[index1]), caMoved);
  end;
end;

function TCollectionList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
    Result := ExtractAt(index);
end;

function TCollectionList<T>.ExtractAt(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
{$ENDIF}

  Result := T(fCollection.Items[index]);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Result.Collection := nil;

  DoNotify(Result, caExtracted);
end;

function TCollectionList<T>.ExtractRange(index, count: Integer): TArray<T>;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCollection.Count - index), 'count');
{$ENDIF}

  SetLength(Result, count);
  i := 0;

  while count > 0 do
  begin
    Result[i] := ExtractAt(index);
    Inc(i);
    Dec(count);
  end;
end;

function TCollectionList<T>.GetCapacity: Integer;
begin
  Result := fCollection.Capacity;
end;

function TCollectionList<T>.GetCount: Integer;
begin
  Result := fCollection.Count;
end;

function TCollectionList<T>.GetElementType: PTypeInfo;
begin
  Result := fCollection.ItemClass.ClassInfo;
end;

function TCollectionList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TCollectionList<T>.GetIsEmpty: Boolean;
begin
  Result := fCollection.Count = 0;
end;

function TCollectionList<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
{$ENDIF}

  Result := T(fCollection.Items[index]);
end;

function TCollectionList<T>.GetOwnsObjects: Boolean;
begin
  Result := True;
end;

function TCollectionList<T>.GetRange(index, count: Integer): IList<T>;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCollection.Count - index), 'count');
{$ENDIF}

  Result := CreateList;
  Result.Count := count;
  for i := 0 to count - 1 do
  begin
    Result[i] := T(fCollection.Items[index]);
    Inc(index);
  end;
end;

function TCollectionList<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, fCollection.Count);
end;

function TCollectionList<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, fCollection.Count - index);
end;

function TCollectionList<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCollection.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCollection.Count - index), 'count');
{$ENDIF}

  for i := index to index + count - 1 do
    if Equals(T(fCollection.Items[i]), item) then
      Exit(i);
  Result := -1;
end;

procedure TCollectionList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCollection.Count), 'index');
{$ENDIF}

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  item.Collection := fCollection;
  item.Index := index;

  DoNotify(item, caAdded);
end;

procedure TCollectionList<T>.InsertRange(index: Integer;
  const values: array of T);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCollection.Count), 'index');
{$ENDIF}

  for i := Low(values) to High(values) do
  begin
    Insert(index, values[i]);
    Inc(index);
  end;
end;

procedure TCollectionList<T>.InsertRange(index: Integer;
  const values: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCollection.Count), 'index');
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  for item in values do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

function TCollectionList<T>.LastIndexOf(const item: T; index,
  count: Integer): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  for i := index downto index - count + 1 do
    if Equals(T(fCollection.Items[i]), item) then
      Exit(i);
  Result := -1;
end;

procedure TCollectionList<T>.Move(currentIndex, newIndex: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < fCollection.Count), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < fCollection.Count), 'newIndex');
{$ENDIF}

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_ON}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fCollection.Items[currentIndex].Index := newIndex;

  DoNotify(T(fCollection.Items[newIndex]), caMoved);
end;

procedure TCollectionList<T>.Reverse(index, count: Integer);
begin
  raise Error.NotSupported;
end;

procedure TCollectionList<T>.SetCapacity(value: Integer);
begin
  fCollection.Capacity := value;
end;

procedure TCollectionList<T>.SetCount(value: Integer);
begin
  raise Error.NotSupported;
end;

procedure TCollectionList<T>.SetItem(index: Integer; const value: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCollection.Count), 'index');
{$ENDIF}

  fCollection.Items[index] := value;
end;

procedure TCollectionList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  raise Error.NotSupported;
end;

function TCollectionList<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, fCollection.Count);
  for i := 0 to DynArrayLength(Result) - 1 do
    Result[i] := T(fCollection.Items[i]);
end;

procedure TCollectionList<T>.TrimExcess;
begin
  fCollection.Capacity := fCollection.Count;
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>.TEnumerator'}

constructor TCollectionList<T>.TEnumerator.Create(const list: TCollectionList<T>);
begin
  inherited Create;
  fSource := list;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TCollectionList<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function TCollectionList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TCollectionList<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  Result := fIndex < fSource.fCollection.Count;
  if Result then
  begin
    fCurrent := T(fSource.fCollection.Items[fIndex]);
    Inc(fIndex);
  end
  else
    fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TAnonymousReadOnlyList<T>'}

constructor TAnonymousReadOnlyList<T>.Create(const count: Func<Integer>;
  const items: Func<Integer, T>; const iterator: IEnumerable<T>);
begin
  inherited Create;
  fCount := count;
  fItems := items;
  fIterator := iterator;
  if not Assigned(fIterator) then
    fIterator := TAnonymousIterator<T>.Create(fCount, fItems);
end;

function TAnonymousReadOnlyList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TAnonymousReadOnlyList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fIterator.GetEnumerator;
end;

function TAnonymousReadOnlyList<T>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TAnonymousReadOnlyList<T>.GetItem(index: Integer): T;
begin
  Result := fItems(index);
end;

function TAnonymousReadOnlyList<T>.GetRange(index, count: Integer): IList<T>;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  Result := TCollections.CreateList<T>;
  Result.Count := count;
  for i := 0 to count - 1 do
  begin
    Result[i] := fItems(index);
    Inc(index);
  end;
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, fCount)
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T;
  index: Integer): Integer;
begin
  Result := IndexOf(item, index, fCount - index);
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
var
  i: Integer;
begin
  for i := index to index + count - 1 do
    if Equals(fItems(i), item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TFoldedObjectList<T>'}

function TFoldedObjectList<T>.CreateList: IList<TObject>;
begin
  Result := TFoldedObjectList<T>.Create(False);
end;

function TFoldedObjectList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TFoldedInterfaceList<T>'}

function TFoldedInterfaceList<T>.CreateList: IList<IInterface>;
begin
  Result := TFoldedInterfaceList<T>.Create;
end;

function TFoldedInterfaceList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TFoldedSortedObjectList<T>'}

function TFoldedSortedObjectList<T>.CreateList: IList<TObject>;
begin
  Result := TFoldedObjectList<T>.Create(False);
end;

function TFoldedSortedObjectList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TFoldedSortedInterfaceList<T>'}

function TFoldedSortedInterfaceList<T>.CreateList: IList<IInterface>;
begin
  Result := TFoldedInterfaceList<T>.Create;
end;

function TFoldedSortedInterfaceList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{$ENDREGION}


{$REGION 'TFoldedObjectList'}

constructor TFoldedObjectList.Create(const elementType: PTypeInfo;
  const comparer: IComparer<TObject>; ownsObjects: Boolean);
begin
  inherited Create(comparer, ownsObjects);
  fElementType := elementType;
end;

function TFoldedObjectList.CreateList: IList<TObject>;
begin
  Result := TFoldedObjectList.Create(fElementType, Comparer, False);
end;

function TFoldedObjectList.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


{$REGION 'TFoldedInterfaceList'}

constructor TFoldedInterfaceList.Create(const elementType: PTypeInfo;
  const comparer: IComparer<IInterface>);
begin
  inherited Create(comparer);
  fElementType := elementType;
end;

function TFoldedInterfaceList.CreateList: IList<IInterface>;
begin
  Result := TFoldedInterfaceList.Create(fElementType, Comparer);
end;

function TFoldedInterfaceList.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


{$REGION 'TObservableList<T> }

constructor TObservableList<T>.Create;
begin
  inherited Create;
  fOnPropertyChanged := TPropertyChangedEventImpl.Create;
end;

procedure TObservableList<T>.DoItemPropertyChanged(sender: TObject;
  const eventArgs: IPropertyChangedEventArgs);
begin
  inherited Changed(T(sender), caChanged);
end;

procedure TObservableList<T>.DoPropertyChanged(const propertyName: string);
begin
  if Assigned(fOnPropertyChanged) and fOnPropertyChanged.CanInvoke then
    fOnPropertyChanged.Invoke(Self,
      TPropertyChangedEventArgs.Create(propertyName) as IPropertyChangedEventArgs);
end;

function TObservableList<T>.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := fOnPropertyChanged;
end;

procedure TObservableList<T>.Changed(const value: TObject;
  action: TCollectionChangedAction);
var
  notifyPropertyChanged: INotifyPropertyChanged;
  propertyChanged: IEvent<TPropertyChangedEvent>;
begin
  if Supports(value, INotifyPropertyChanged, notifyPropertyChanged) then
  begin
    propertyChanged := notifyPropertyChanged.OnPropertyChanged;
    case Action of
      caAdded: propertyChanged.Add(DoItemPropertyChanged);
      caRemoved, caExtracted: propertyChanged.Remove(DoItemPropertyChanged);
    end;
  end;

  inherited Changed(value, action);
  DoPropertyChanged('Count');
end;

{$ENDREGION}


end.
