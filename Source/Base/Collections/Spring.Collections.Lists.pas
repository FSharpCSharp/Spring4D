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

unit Spring.Collections.Lists;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

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
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
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
      ItemType = TArrayManager<T>;
      {$POINTERMATH ON}
      PT = ^T;
      {$POINTERMATH OFF}
  {$ENDREGION}
  private
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
    procedure Grow; overload;
    {$IFNDEF DELPHIXE8_UP}{$HINTS OFF}{$ENDIF}
    procedure Grow(capacity: Integer); overload;
    {$IFNDEF DELPHIXE8_UP}{$HINTS ON}{$ENDIF}
    procedure DeleteInternal(index: Integer; action: TCollectionChangedAction); inline;
    function DeleteAllInternal(const match: Predicate<T>;
      action: TCollectionChangedAction; const items: TArray<T>): Integer;
    procedure DeleteRangeInternal(index, count: Integer; doClear: Boolean);
    function InsertInternal(index: Integer; const item: T): Integer;
    procedure InsertRangeInternal(index, count: Integer; const values: array of T);
    procedure SetItemInternal(index: Integer; const value: T);
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
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}

    function TryGetElementAt(out value: T; index: Integer): Boolean;
    function TryGetFirst(out value: T): Boolean; overload;
    function TryGetLast(out value: T): Boolean; overload;
    function TryGetSingle(out value: T): Boolean; overload;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: IEnumerable<T>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;

    function Contains(const value: T): Boolean; overload;
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    function Single: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload;

    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    function RemoveAll(const match: Predicate<T>): Integer;

    function Extract(const item: T): T;
    function ExtractAll(const match: Predicate<T>): TArray<T>;

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

    function IndexOf(const item: T; index, count: Integer): Integer; overload;
    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    procedure TrimExcess;
  {$ENDREGION}
  end;

  TList<T> = class(TAbstractArrayList<T>, IInterface, IEnumerable<T>,
    IReadOnlyCollection<T>, IReadOnlyList<T>, ICollection<T>, IList<T>)
  protected
    function AsReadOnly: IReadOnlyList<T>;
  end;

  TSortedList<T> = class(TAbstractArrayList<T>, IInterface, IEnumerable<T>,
    IReadOnlyCollection<T>, IReadOnlyList<T>, ICollection<T>, IList<T>)
  private
    procedure SetItem(index: Integer; const value: T);
    function AsReadOnly: IReadOnlyList<T>;
  public
  {$REGION 'Implements IEnumerable<T>'}
    function Contains(const value: T): Boolean; overload;
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

  TCollectionList<T: TCollectionItem> = class(TListBase<T>, IInterface,
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>, ICollection<T>, IList<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
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
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}
    procedure DeleteRangeInternal(index, count: Integer; doClear: Boolean);
    function AsReadOnly: IReadOnlyList<T>;
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

  TAnonymousReadOnlyList<T> = class(TEnumerableBase<T>, IInterface,
    IReadOnlyCollection<T>, IReadOnlyList<T>)
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

  {$REGION 'Implements IReadOnlyList<T>'}
    function GetRange(index, count: Integer): IList<T>;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TFoldedList<T> = class(TList<T>)
  private
    fElementType: PTypeInfo;
  protected
    function CreateList: IList<T>; override;
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(elementType: PTypeInfo; const comparer: IComparer<T>;
      ownsObjects: Boolean = False);
  end;

  TFoldedSortedList<T> = class(TSortedList<T>)
  private
    fElementType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
  public
    constructor Create(elementType: PTypeInfo; const comparer: IComparer<T>;
      ownsObjects: Boolean = False);
  end;

  TObservableObjectList = class(TFoldedList<TObject>, INotifyPropertyChanged)
  private
    fOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(sender: TObject;
      const eventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const propertyName: string);
    procedure Changed(const value: TObject; action: TCollectionChangedAction); override;
  public
    constructor Create(elementType: PTypeInfo; ownsObjects: Boolean = True);
    property OnPropertyChanged: IEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TObservableInterfaceList = class(TFoldedList<IInterface>, INotifyPropertyChanged)
  private
    fOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(sender: TObject;
      const eventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const propertyName: string);
    procedure Changed(const value: IInterface; action: TCollectionChangedAction); override;
  public
    constructor Create(elementType: PTypeInfo);
    property OnPropertyChanged: IEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
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


{$REGION 'TAbstractArrayList<T>'}

constructor TAbstractArrayList<T>.Create(const values: array of T);
var
  count, i: Integer;
begin
  Create;
  count := Length(values);
  if count > 0 then
  begin
    SetCount(count);
    for i := Low(values) to High(values) do
      fItems[i] := values[i];
  end;
end;

constructor TAbstractArrayList<T>.Create(const values: IEnumerable<T>);
var
  collection: IReadOnlyCollection<T>;
  count: Integer;
begin
  Create;
  if Supports(values, IReadOnlyCollection<T>, collection) then
  begin
    count := collection.Count;
    if count > 0 then
    begin
      SetCount(count);
      collection.CopyTo(fItems, 0);
    end;
  end
  else
    AddRange(values);
end;

destructor TAbstractArrayList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TAbstractArrayList<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TAbstractArrayList<T>.GetCount: Integer;
begin
  Result := fCount and CountMask;
end;

function TAbstractArrayList<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

function TAbstractArrayList<T>.Add(const item: T): Integer;
begin
  Result := Count;
  if (Result <> fCapacity) and not Assigned(Notify) then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    Inc(fCount);
    fItems[Result] := item;
  end
  else
    Result := InsertInternal(Result, item);
end;

procedure TAbstractArrayList<T>.AddRange(const values: array of T);
begin
  InsertRange(Count, values);
end;

procedure TAbstractArrayList<T>.AddRange(const values: IEnumerable<T>);
begin
  InsertRange(Count, values);
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
  list.fCapacity := count;
  SetLength(list.fItems, count);
  for i := 0 to count - 1 do
  begin
    list.fItems[i] := fItems[index];
    Inc(index);
  end;
{$ENDIF}
end;

procedure TAbstractArrayList<T>.Grow;
begin
  fCapacity := GrowCapacity(fCapacity);
  SetLength(fItems, fCapacity);
end;

procedure TAbstractArrayList<T>.Grow(capacity: Integer);
begin
  fCapacity := GrowCapacity(fCapacity, capacity);
  SetLength(fItems, fCapacity);
end;

function TAbstractArrayList<T>.IndexOf(const item: T; index, count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if TType.Kind<T> = tkClass then
    Result := TArrayHelper.IndexOfObj(fItems, item, index, count)
  else if UseComparer(TType.Kind<T>) then
  begin
    for Result := index to index + count - 1 do
      if Comparer.Compare(fItems[Result], item) = 0 then
        Exit;
    Result := -1;
  end
  else
    if TType.Kind<T> = tkUString then
      Result := TArrayHelper.IndexOfStr(fItems, item, index, count)
    else
      case SizeOf(T) of
        1: Result := TArrayHelper.IndexOf1(fItems, item, index, count);
        2: Result := TArrayHelper.IndexOf2(fItems, item, index, count);
        4: Result := TArrayHelper.IndexOf4(fItems, item, index, count);
        8: Result := TArrayHelper.IndexOf8(fItems, item, index, count);
      end;
end;

procedure TAbstractArrayList<T>.SetItem(index: Integer; const value: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  if not Assigned(Notify) then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    if OwnsObjects then
      FreeObject(fItems[index]);
    fItems[index] := value;
  end
  else
    SetItemInternal(index, value);
end;

procedure TAbstractArrayList<T>.SetItemInternal(index: Integer; const value: T);
var
  oldItem: T;
begin
  oldItem := fItems[index];

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[index] := value;

  Notify(Self, oldItem, caRemoved);
  if OwnsObjects then
    FreeObject(oldItem);
  Notify(Self, value, caAdded);
end;

procedure TAbstractArrayList<T>.SetOwnsObjects(value: Boolean);
begin
  if TType.Kind<T> = tkClass then
    fCount := (fCount and CountMask) or BitMask[value];
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
  InsertInternal(index, item);
end;

function TAbstractArrayList<T>.InsertInternal(index: Integer;
  const item: T): Integer;
var
  listCount: Integer;
  arrayItem: PT;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  listCount := Count;
  if listCount = fCapacity then
  begin
    Grow;
    listCount := Count;
  end;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);
  if index <> listCount then
  begin
    if ItemType.HasWeakRef then
      ItemType.MoveSlow(fItems, index, index + 1, listCount - index)
    else
    begin
      listCount := (listCount - index) * SizeOf(T);
      arrayItem := @fItems[index];
      System.Move(arrayItem^, (arrayItem + 1)^, listCount);
      if ItemType.IsManaged then
        if SizeOf(T) = SizeOf(Pointer) then
          PPointer(arrayItem)^ := nil
        else
          System.FillChar(arrayItem^, SizeOf(T), 0);
    end;
  end;
  fItems[index] := item;

  DoNotify(item, caAdded);
  Result := index;
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer; const values: array of T);
var
  itemCount: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  itemCount := Length(values);
  if itemCount = 0 then
    Exit;

  InsertRangeInternal(index, itemCount, values);
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer;
  const values: IEnumerable<T>);
var
  collection: IReadOnlyCollection<T>;
  listCount, count, i: Integer;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Self.Count), 'index');
{$ENDIF}

  if Supports(values, IReadOnlyCollection<T>, collection) then
  begin
    count := collection.Count;
    listCount := Self.Count;
    if count > 0 then
    begin
      if listCount + count > fCapacity then
        Grow(listCount + count);

      {$Q-}
      Inc(fVersion);
      {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

      if index < listCount then
      begin
        if ItemType.IsManaged then
          if ItemType.HasWeakRef or (collection = this) then
            ItemType.MoveSlow(fItems, index, index + count, listCount - index)
          else
          begin
            System.Move(fItems[index], fItems[index + count], SizeOf(T) * (listCount - index));
            System.FillChar(fItems[index], SizeOf(T) * count, 0);
          end
        else
          System.Move(fItems[index], fItems[index + count], SizeOf(T) * (listCount - index));
      end;

      if collection = this then
      begin
        if ItemType.IsManaged then
        begin
          ItemType.MoveSlow(fItems, 0, index, index);
          ItemType.MoveSlow(fItems, index + count, index * 2, listCount - index);
        end
        else
        begin
          System.Move(fItems[0], fItems[index], SizeOf(T) * index);
          System.Move(fItems[index + count], fItems[index * 2], SizeOf(T) * (listCount - index));
        end;
      end
      else
        collection.CopyTo(fItems, index);
      Inc(fCount, count);

      if Assigned(Notify) then
        for i := index to index + count - 1 do
          Notify(Self, fItems[i], caAdded);
    end;
  end
  else
    for item in values do
    begin
      Insert(index, item);
      Inc(index);
    end;
end;

procedure TAbstractArrayList<T>.InsertRangeInternal(index, count: Integer;
  const values: array of T);
var
  listCount, i: Integer;
begin
  listCount := Self.Count;
  if listCount + count > fCapacity then
    Grow(listCount + count);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if index <> listCount then
  begin
    if ItemType.HasWeakRef then
      ItemType.MoveSlow(fItems, index, index + count, listCount - index)
    else
    begin
      System.Move(fItems[index], fItems[index + count], SizeOf(T) * (listCount - index));
      if ItemType.IsManaged then
        System.FillChar(fItems[index], SizeOf(T) * count, 0);
    end;
  end;

  if ItemType.IsManaged then
    for i := Low(values) to count - 1 do
      fItems[index + i] := values[i]
  else
    System.Move(values[0], fItems[index], SizeOf(T) * count);

  Inc(fCount, count);

  if Assigned(Notify) then
    for i := Low(values) to count - 1 do
      Notify(Self, values[i], caAdded);
end;

function TAbstractArrayList<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  if TType.Kind<T> = tkClass then
    Result := TArrayHelper.LastIndexOfStr(fItems, item, index, count)
  else if UseComparer(TType.Kind<T>) then
  begin
    for Result := index downto index - count do
      if Comparer.Compare(fItems[Result], item) = 0 then
        Exit;
    Result := -1;
  end
  else
    if TType.Kind<T> = tkUString then
      Result := TArrayHelper.LastIndexOfStr(fItems, item, index, count)
    else
      case SizeOf(T) of
        1: Result := TArrayHelper.LastIndexOf1(fItems, item, index, count);
        2: Result := TArrayHelper.LastIndexOf2(fItems, item, index, count);
        4: Result := TArrayHelper.LastIndexOf4(fItems, item, index, count);
        8: Result := TArrayHelper.LastIndexOf8(fItems, item, index, count);
      end;
end;

procedure TAbstractArrayList<T>.DeleteInternal(index: Integer;
  action: TCollectionChangedAction);
var
  listCount: Integer;
  arrayItem: PT;
  oldItem: T;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);
  listCount := Count;

  arrayItem := @fItems[index];
  oldItem := arrayItem^;
  if index <> listCount then
  begin
    if ItemType.HasWeakRef then
      ItemType.MoveSlow(fItems, index + 1, index, listCount - index)
    else
    begin
      if ItemType.IsManaged then
        arrayItem^ := Default(T);
      System.Move((arrayItem + 1)^, arrayItem^, SizeOf(T) * (listCount - index));
      arrayItem := @fItems[listCount];
      if ItemType.IsManaged then
        if SizeOf(T) = SizeOf(Pointer) then
          PPointer(arrayItem)^ := nil
        else
          System.FillChar(arrayItem^, SizeOf(T), 0)
      else
        arrayItem^ := Default(T);
    end;
  end
  else
    fItems[listCount] := Default(T);

  if Assigned(Notify) then
    Notify(Self, oldItem, action);
  if OwnsObjects then
    if action = caRemoved then
      FreeObject(oldItem);
end;

procedure TAbstractArrayList<T>.Clear;
var
  listCount: Integer;
begin
  listCount := Count;
  if listCount > 0 then
  begin
    if OwnsObjects or Assigned(Notify) then
      DeleteRangeInternal(0, listCount, True)
    else
    begin
      {$Q-}
      Inc(fVersion);
      {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
      Dec(fCount, listCount);
    end;
  end;
  fCapacity := 0;
  SetLength(fItems, 0);
end;

procedure TAbstractArrayList<T>.DeleteRangeInternal(index, count: Integer; doClear: Boolean);
var
  oldItems: TArray<T>;
  tailCount, i: Integer;
begin
  SetLength(oldItems, count);
  ItemType.Move(fItems, oldItems, index, 0, count);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  tailCount := Self.Count - (index + count);
  if tailCount > 0 then
  begin
    ItemType.Move(fItems, index + count, index, tailCount);
    Inc(index, tailCount);
  end;
  ItemType.Finalize(fItems, index, count);
  Dec(fCount, count);

  if doClear then
    Reset;

  if Assigned(Notify) then
    if OwnsObjects then
      for i := 0 to count - 1 do
      begin
        Notify(Self, oldItems[i], caRemoved);
        FreeObject(oldItems[i]);
      end
    else
      for i := 0 to count - 1 do
        Notify(Self, oldItems[i], caRemoved)
  else
    if OwnsObjects then
      for i := 0 to count - 1 do
        FreeObject(oldItems[i]);
end;

procedure TAbstractArrayList<T>.DeleteRange(index, count: Integer);
var
  tailCount: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(index >= 0, 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  if OwnsObjects or Assigned(Notify) then
  begin
    DeleteRangeInternal(index, count, False);
    Exit;
  end;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  if ItemType.IsManaged then
    FinalizeArray(@fItems[index], TypeInfo(T), count);

  tailCount := Self.Count - (index + count);
  if tailCount > 0 then
  begin
    if ItemType.HasWeakRef then
    begin
      ItemType.MoveSlow(fItems, index + count, index, tailCount);
      FinalizeArray(@fItems[index + count], TypeInfo(T), tailCount);
    end
    else
      System.Move(fItems[index + count], fItems[index], SizeOf(T) * tailCount);
    Inc(index, count);
  end;
  if not ItemType.HasWeakRef then
    System.FillChar(fItems[index], SizeOf(T) * count, 0);
  Dec(fCount, count);
end;

procedure TAbstractArrayList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) = tkClass then
    TTimSort.Sort(fItems, IComparer<Pointer>(comparer), index, count)
  else
  {$ENDIF}
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

  if currentIndex = newIndex then
    Exit;

  temp := fItems[currentIndex];

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
    ItemType.Move(fItems, currentIndex + 1, currentIndex, newIndex - currentIndex)
  else
    ItemType.Move(fItems, newIndex, newIndex + 1, currentIndex - newIndex);

  if ItemType.HasWeakRef then
    FinalizeArray(@fItems[newIndex], TypeInfo(T), 1);
  if ItemType.IsManaged then
    if SizeOf(T) = SizeOf(Pointer) then
      PPointer(@fItems[newIndex])^ := nil
    else
      System.FillChar(fItems[newIndex], SizeOf(T), 0);

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

procedure TAbstractArrayList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < Count), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < Count), 'index2');
{$ENDIF}

  temp := fItems[index1];

  {$Q-}
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

function TAbstractArrayList<T>.RemoveAll(const match: Predicate<T>): Integer;
begin
  Result := DeleteAllInternal(match, caRemoved, nil);
end;

procedure TAbstractArrayList<T>.Reverse(index, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(index >= 0, 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  TArray.Reverse<T>(fItems, index, count);

  Reset;
end;

procedure TAbstractArrayList<T>.SetCapacity(value: Integer);
begin
  if value < Count then
    DeleteRange(value, Count - value);
  fCapacity := value;
  SetLength(fItems, value);
end;

procedure TAbstractArrayList<T>.SetCount(value: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(value >= 0, 'count');
{$ENDIF}

  if value > fCapacity then
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

function TAbstractArrayList<T>.DeleteAllInternal(const match: Predicate<T>;
  action: TCollectionChangedAction; const items: TArray<T>): Integer;
var
  listCount, freeIndex, current, i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(match), 'match');
{$ENDIF}

  listCount := Self.Count;
  freeIndex := 0;
  current := 0;
  i := 0;
  while current < listCount do
  begin
    // Find the next item that needs to be kept
    while (current < listCount) and match(fItems[current]) do
    begin
      if i = 0 then
        {$Q-}
        Inc(fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

      if Assigned(Notify) then
        Notify(Self, fItems[current], action);
      if action = caExtracted then
        TArray<T>(PPointer(@items)^)[i] := fItems[current]
      else if OwnsObjects then
        FreeObject(fItems[current]);
      Inc(current);
      Inc(i);
    end;

    if current < listCount then
    begin
      fItems[freeIndex] := fItems[current];
      Inc(freeIndex);
      Inc(current);
    end;
  end;
  Result := listCount - freeIndex;
  if Result > 0 then
    if ItemType.IsManaged then
      FinalizeArray(@fItems[freeIndex], TypeInfo(T), Result)
    else
      System.FillChar(fItems[freeIndex], SizeOf(T) * Result, 0);
  Dec(fCount, Result);
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

function TAbstractArrayList<T>.ExtractAll(const match: Predicate<T>): TArray<T>;
var
  count: Integer;
begin
  SetLength(Result, Self.Count);
  count := DeleteAllInternal(match, caExtracted, Result);
  SetLength(Result, count);
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
  ItemType.Move(fItems, Result, index, 0, count);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  tailCount := Self.Count - (index + count);
  if tailCount > 0 then
  begin
    ItemType.Move(fItems, index + count, index, tailCount);
    Inc(index, tailCount);
  end;
  ItemType.Finalize(fItems, index, count);
  Dec(fCount, count);

  if Assigned(Notify) then
    for i := 0 to count - 1 do
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
  itemCount: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(Length(values), index, Count);
{$ENDIF}

  itemCount := Count;
  if itemCount > 0 then
    if ItemType.IsManaged then
      System.CopyArray(@values[index], @fItems[0], TypeInfo(T), itemCount)
    else
      System.Move(fItems[0], values[index], SizeOf(T) * itemCount);
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
  Result := Cardinal(index) < Cardinal(Count);
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
var
  source: TAbstractArrayList<T>;
begin
  source := fSource;
  if fVersion = source.fVersion then
  begin
    if fIndex < source.Count then
    begin
      fCurrent := source.fItems[fIndex];
      Inc(fIndex);
      Exit(True);
    end
    else
    begin
      fCurrent := Default(T);
      Exit(False);
    end;
  end
  else
    raise Error.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TList<T>'}

function TList<T>.AsReadOnly: IReadOnlyList<T>;
begin
  Result := Self;
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

function TSortedList<T>.AsReadOnly: IReadOnlyList<T>;
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


{$REGION 'TCollectionList<T>'}

constructor TCollectionList<T>.Create(const collection: TCollection);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(collection, 'collection');
  Guard.CheckInheritsFrom(collection.ItemClass, TClass(T), 'collection.ItemClass');
{$ENDIF}

  fCollection := collection;
  inherited Create;
end;

function TCollectionList<T>.Add(const item: T): Integer;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  item.Collection := fCollection;
  Result := item.Index;

  DoNotify(item, caAdded);
end;

function TCollectionList<T>.AsReadOnly: IReadOnlyList<T>;
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

  {$Q-}
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

  {$Q-}
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

  {$Q-}
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

  {$Q-}
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
    if fCollection.Items[i].Equals(item) then
      Exit(i);
  Result := -1;
end;

procedure TCollectionList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCollection.Count), 'index');
{$ENDIF}

  {$Q-}
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
    if fCollection.Items[i].Equals(item) then
      Exit(i);
  Result := -1;
end;

procedure TCollectionList<T>.Move(currentIndex, newIndex: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < fCollection.Count), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < fCollection.Count), 'newIndex');
{$ENDIF}

  {$Q-}
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

procedure TCollectionList<T>.SetOwnsObjects(value: Boolean);
begin
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
  for i := 0 to fCollection.Count - 1 do
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
  Result := IndexOf(item, 0, fCount);
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


{$REGION 'TFoldedList<T>'}

constructor TFoldedList<T>.Create(elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  fElementType := elementType;
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

function TFoldedList<T>.CreateList: IList<T>;
begin
  Result := TFoldedList<T>.Create(fElementType, Comparer);
end;

function TFoldedList<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


{$REGION 'TFoldedSortedList<T>'}

constructor TFoldedSortedList<T>.Create(elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  fElementType := elementType;
  inherited Create(comparer);
  SetOwnsObjects(ownsObjects);
end;

function TFoldedSortedList<T>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

{$ENDREGION}


{$REGION 'TObservableObjectList'}

constructor TObservableObjectList.Create(elementType: PTypeInfo; ownsObjects: Boolean);
begin
  inherited Create(elementType, nil);
  fOnPropertyChanged := TPropertyChangedEventImpl.Create;
end;

procedure TObservableObjectList.DoItemPropertyChanged(sender: TObject;
  const eventArgs: IPropertyChangedEventArgs);
begin
  inherited Changed(sender, caChanged);
end;

procedure TObservableObjectList.DoPropertyChanged(const propertyName: string);
begin
  if Assigned(fOnPropertyChanged) and fOnPropertyChanged.CanInvoke then
    fOnPropertyChanged.Invoke(Self,
      TPropertyChangedEventArgs.Create(propertyName) as IPropertyChangedEventArgs);
end;

function TObservableObjectList.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := fOnPropertyChanged;
end;

procedure TObservableObjectList.Changed(const value: TObject;
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


{$REGION 'TObservableInterfaceList'}

constructor TObservableInterfaceList.Create(elementType: PTypeInfo);
begin
  inherited Create(elementType, nil);
  fOnPropertyChanged := TPropertyChangedEventImpl.Create;
end;

procedure TObservableInterfaceList.Changed(const value: IInterface;
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

procedure TObservableInterfaceList.DoItemPropertyChanged(sender: TObject;
  const eventArgs: IPropertyChangedEventArgs);
var
  item: IInterface;
begin
  Supports(sender, fElementType.TypeData.Guid, item);
  inherited Changed(item, caChanged);
end;

procedure TObservableInterfaceList.DoPropertyChanged(
  const propertyName: string);
begin
  if Assigned(fOnPropertyChanged) and fOnPropertyChanged.CanInvoke then
    fOnPropertyChanged.Invoke(Self,
      TPropertyChangedEventArgs.Create(propertyName) as IPropertyChangedEventArgs);
end;

function TObservableInterfaceList.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := fOnPropertyChanged;
end;

{$ENDREGION}


end.
