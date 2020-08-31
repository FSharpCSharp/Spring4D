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

unit Spring.Collections.Lists;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  /// <summary>
  ///   Represents a strongly typed list of elements that can be accessed by
  ///   index. Provides methods to search, sort, and manipulate lists.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the list.
  /// </typeparam>
  TAbstractArrayList<T> = class(TCollectionBase<T>)
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
        procedure BeforeDestruction; override;
        function MoveNext: Boolean;
      end;
      ItemType = TTypeInfo<T>;
      {$POINTERMATH ON}
      PT = ^T;
      {$POINTERMATH OFF}
  {$ENDREGION}
  private
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
    function CanFastCompare: Boolean; inline;
    procedure Grow(capacity: Integer);
    procedure DeleteInternal(index: Integer; action: TCollectionChangedAction);
    function DeleteAllInternal(const match: Predicate<T>;
      action: TCollectionChangedAction; const items: TArray<T>): Integer;
    procedure DeleteRangeInternal(index, count: Integer;
      action: TCollectionChangedAction; doClear: Boolean; result: PPointer);
    function InsertInternal(index: Integer; const item: T): Integer;
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
    procedure SetOwnsObjects(value: Boolean); inline;
  {$ENDREGION}

    function CreateList: IList<T>; virtual;
    function TryGetElementAt(var value: T; index: Integer): Boolean;
    function TryGetFirst(var value: T): Boolean; overload;
    function TryGetLast(var value: T): Boolean; overload;
    function TryGetSingle(var value: T): Boolean; overload;
    function TryGetSingle(var value: T; const predicate: Predicate<T>): Boolean; overload;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(const comparer: IComparer<T>); overload;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  {$ENDREGION}

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

    function Remove(const item: T): Boolean;
    function RemoveAll(const match: Predicate<T>): Integer;

    function Extract(const item: T): T;
    function ExtractAll(const match: Predicate<T>): TArray<T>;

    procedure Clear;

    procedure CopyTo(var values: TArray<T>; index: Integer);
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

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;
    procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T): Integer; overload;
    function LastIndexOf(const item: T; index: Integer): Integer; overload;
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
    procedure Move(sourceIndex, targetIndex: Integer);

    procedure Reverse(index, count: Integer); overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;
    procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;
  {$ENDREGION}
  end;

  TCollectionList<T: TCollectionItem> = class(TCollectionBase<T>, IInterface,
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
    function Remove(const item: T): Boolean;
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
    procedure Move(sourceIndex, targetIndex: Integer);

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T): Integer; overload;
    function LastIndexOf(const item: T; index: Integer): Integer; overload;
    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;
    procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;
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
    function CreateList: IList<T>; override;
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

const
  FastComparableTypes = [tkUnknown, tkInteger, tkChar, tkEnumeration, tkSet,
                         tkClass, tkMethod, tkWChar, tkInterface, tkInt64,
                         tkUString, tkClassRef, tkPointer, tkProcedure];

implementation

uses
{$IFDEF DELPHIXE4}
  Rtti, // suppress hint about inlining
{$ENDIF}
  Spring.Collections.Extensions,
  Spring.Events,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractArrayList<T>'}

constructor TAbstractArrayList<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
end;

function TAbstractArrayList<T>.CreateList: IList<T>;
begin
  Result := TList<T>.Create(fComparer);
end;

procedure TAbstractArrayList<T>.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

function TAbstractArrayList<T>.CanFastCompare: Boolean;
begin
  Result := (TType.Kind<T> in FastComparableTypes) and (SizeOf(T) in [1, 2, 4, 8])
    and (Pointer(fComparer) = _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
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
var
  enumerator: TEnumerator;
begin
  _AddRef;
  enumerator := TEnumerator.Create;
  enumerator.fSource := Self;
  enumerator.fVersion := fVersion;
  Result := enumerator;
end;

function TAbstractArrayList<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TAbstractArrayList<T>.GetItem(index: Integer): T;
begin
  CheckIndex(index, Count);

  Result := fItems[index];
end;

function TAbstractArrayList<T>.GetRange(index, count: Integer): IList<T>;
var
  list: TAbstractArrayList<T>;
begin
  CheckRange(index, count, Self.Count);

  Result := CreateList;
  list := TAbstractArrayList<T>(Result.AsObject);
  list.fCapacity := count;
  list.fCount := (list.fCount and OwnsObjectsMask) or count;
  DynArrayCopyRange(Pointer(list.fItems), fItems, TypeInfo(TArray<T>), index, count);
end;

procedure TAbstractArrayList<T>.Grow(capacity: Integer);
begin
  fCapacity := GrowCapacity(fCapacity, capacity);
  SetLength(fItems, fCapacity);
end;

function TAbstractArrayList<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Count);
end;

function TAbstractArrayList<T>.IndexOf(const item: T; index: Integer): Integer;
var
  listCount: Integer;
begin
  listCount := Count;
  CheckIndex(index, listCount);

  Result := IndexOf(item, index, listCount - index);
end;

function TAbstractArrayList<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  listCount: Integer;
begin
  CheckRange(index, count, Self.Count);

  if TType.Kind<T> = tkClass then
    Result := TArrayHelper.IndexOfObj(fItems, item, index, count)
  else if CanFastCompare then
  begin
    if TType.Kind<T> = tkUString then
      Result := TArrayHelper.IndexOfStr(fItems, item, index, count)
    else
      case SizeOf(T) of
        1: Result := TArrayHelper.IndexOf1(fItems, item, index, count);
        2: Result := TArrayHelper.IndexOf2(fItems, item, index, count);
        4: Result := TArrayHelper.IndexOf4(fItems, item, index, count);
        8: Result := TArrayHelper.IndexOf8(fItems, item, index, count);
      end;
  end
  else
  begin
    for Result := index to index + count - 1 do
      if Comparer.Compare(fItems[Result], item) = 0 then
        Exit;
    Result := -1;
  end;
end;

procedure TAbstractArrayList<T>.SetItem(index: Integer; const value: T);
begin
  CheckIndex(index, Count);

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
    fCount := (fCount and CountMask) or (Ord(value) shl OwnsObjectsBitIndex);
end;

function TAbstractArrayList<T>.Single: T;
begin
  case Count of
    0: RaiseHelper.NoElements;
    1: Result := fItems[0];
  else
    RaiseHelper.MoreThanOneElement;
  end;
end;

function TAbstractArrayList<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := fItems[0];
  else
    RaiseHelper.MoreThanOneElement;
  end;
end;

procedure TAbstractArrayList<T>.Insert(index: Integer; const item: T);
begin
  InsertInternal(index, item);
end;

function TAbstractArrayList<T>.InsertInternal(index: Integer; const item: T): Integer;
var
  listCount, tailCount: Integer;
  arrayItem: PT;
begin
  listCount := Count;
  if Cardinal(index) > Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange_Index;

  if listCount = fCapacity then
    Grow(listCount + 1);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);
  arrayItem := @fItems[index];
  tailCount := listCount - index;
  if tailCount > 0 then
  begin
    if ItemType.HasWeakRef then
      MoveManaged(arrayItem, arrayItem + 1, TypeInfo(T), tailCount)
    else
    begin
      System.Move(arrayItem^, (arrayItem + 1)^, SizeOf(T) * tailCount);
      if ItemType.IsManaged then
        if SizeOf(T) = SizeOf(Pointer) then
          PPointer(arrayItem)^ := nil
        else
          System.FillChar(arrayItem^, SizeOf(T), 0);
    end;
  end;
  arrayItem^ := item;

  DoNotify(item, caAdded);
  Result := index;
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer; const values: array of T);
var
  listCount, valueCount, tailCount, i: Integer;
  arrayItem: PT;
begin
  listCount := Count;
  if Cardinal(index) > Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange_Index;

  valueCount := Length(values);
  if valueCount = 0 then
    Exit;

  if listCount + valueCount > fCapacity then
    Grow(listCount + valueCount);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount, valueCount);
  arrayItem := @fItems[index];
  tailCount := listCount - index;
  if tailCount > 0 then
  begin
    if ItemType.HasWeakRef then
      MoveManaged(arrayItem, arrayItem + valueCount, TypeInfo(T), tailCount)
    else
    begin
      System.Move(arrayItem^, (arrayItem + valueCount)^, SizeOf(T) * tailCount);
      if ItemType.IsManaged then
        System.FillChar(arrayItem^, SizeOf(T) * valueCount, 0);
    end;
  end;

  if ItemType.IsManaged then
    MoveManaged(@values[0], arrayItem, TypeInfo(T), valueCount)
  else
    System.Move(values[0], arrayItem^, SizeOf(T) * valueCount);

  if Assigned(Notify) then
    for i := 0 to High(values) do
      Notify(Self, values[i], caAdded);
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer; const values: IEnumerable<T>);
var
  listCount, valueCount, tailCount, i: Integer;
  arrayItem: PT;
  intf: IInterface;
begin
  listCount := Count;
  if Cardinal(index) > Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange_Index;
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  if values.QueryInterface(IReadOnlyCollectionOfTGuid, Pointer(intf)) = S_OK then
  begin
    valueCount := IReadOnlyCollection<T>(intf).Count;
    if valueCount = 0 then
      Exit;

    if listCount + valueCount > fCapacity then
      Grow(listCount + valueCount);

    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    arrayItem := @fItems[index];
    tailCount := listCount - index;
    if tailCount > 0 then
      if ItemType.IsManaged then
        MoveManaged(arrayItem, arrayItem + valueCount, TypeInfo(T), tailCount)
      else
        System.Move(arrayItem^, (arrayItem + valueCount)^, SizeOf(T) * tailCount);

    IReadOnlyCollection<T>(intf).CopyTo(fItems, index);
    Inc(fCount, valueCount);

    if Assigned(Notify) then
      for i := index to index + valueCount - 1 do
        Notify(Self, fItems[i], caAdded);
  end
  else
  begin
    intf := values.GetEnumerator;
    while IEnumerator<T>(intf).MoveNext do
    begin
      Insert(index, IEnumerator<T>(intf).Current);
      Inc(index);
    end;
  end;
end;

function TAbstractArrayList<T>.LastIndexOf(const item: T): Integer;
var
  listCount: Integer;
begin
  listCount := Count;
  if listCount > 0 then
    Result := LastIndexOf(item, listCount - 1, listCount)
  else
    Result := -1;
end;

function TAbstractArrayList<T>.LastIndexOf(const item: T; index: Integer): Integer;
begin
  CheckIndex(index, Count);

  Result := LastIndexOf(item, index, index + 1);
end;

function TAbstractArrayList<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
var
  listCount: Integer;
begin
  listCount := Count;
  CheckIndex(index, listCount);
  if Cardinal(count) > Cardinal(index + 1) then RaiseHelper.ArgumentOutOfRange_Count;

  if listCount = 0 then
    Exit(-1);

  if TType.Kind<T> = tkClass then
    Result := TArrayHelper.LastIndexOfStr(fItems, item, index, count)
  else if CanFastCompare then
  begin
    if TType.Kind<T> = tkUString then
      Result := TArrayHelper.LastIndexOfStr(fItems, item, index, count)
    else
      case SizeOf(T) of
        1: Result := TArrayHelper.LastIndexOf1(fItems, item, index, count);
        2: Result := TArrayHelper.LastIndexOf2(fItems, item, index, count);
        4: Result := TArrayHelper.LastIndexOf4(fItems, item, index, count);
        8: Result := TArrayHelper.LastIndexOf8(fItems, item, index, count);
      end;
  end
  else
  begin
    for Result := index downto index - count do
      if fComparer.Compare(fItems[Result], item) = 0 then
        Exit;
    Result := -1;
  end;
end;

procedure TAbstractArrayList<T>.DeleteInternal(index: Integer;
  action: TCollectionChangedAction);
var
  listCount, tailCount: Integer;
  arrayItem, tailItem: PT;
  oldItem: T;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);
  listCount := Count;

  arrayItem := @fItems[index];
  oldItem := arrayItem^;
  tailItem := @fItems[listCount];
  tailCount := listCount - index;
  if tailCount > 0 then
  begin
    if ItemType.HasWeakRef then
      MoveManaged(arrayItem + 1, arrayItem, TypeInfo(T), tailCount)
    else
    begin
      if ItemType.IsManaged then
        arrayItem^ := Default(T);
      System.Move((arrayItem + 1)^, arrayItem^, SizeOf(T) * tailCount);
      if ItemType.IsManaged then
        if SizeOf(T) = SizeOf(Pointer) then
          PPointer(tailItem)^ := nil
        else
          System.FillChar(tailItem^, SizeOf(T), 0);
    end;
  end;
  tailItem^ := Default(T);

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
      DeleteRangeInternal(0, listCount, caRemoved, True, nil)
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

procedure TAbstractArrayList<T>.DeleteRangeInternal(index, count: Integer;
  action: TCollectionChangedAction; doClear: Boolean; result: PPointer);
var
  oldItems: TArray<T>;
  tailCount, i: Integer;
begin
  SetLength(oldItems, count);
  if ItemType.HasWeakRef then
    MoveManaged(@fItems[index], @oldItems[0], TypeInfo(T), count)
  else
    System.Move(fItems[index], oldItems[0], SizeOf(T) * count);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  tailCount := Self.Count - (index + count);
  if tailCount > 0 then
  begin
    if ItemType.HasWeakRef then
      MoveManaged(@fItems[index + count], @fItems[index], TypeInfo(T), tailCount)
    else
      System.Move(fItems[index + count], fItems[index], SizeOf(T) * tailCount);
    Inc(index, tailCount);
  end;
  if ItemType.HasWeakRef then
    System.Finalize(fItems[index], count);
  System.FillChar(fItems[index], SizeOf(T) * count, 0);
  Dec(fCount, count);

  if doClear then
    Reset;

  if Assigned(Notify) then
    if OwnsObjects and (action = caRemoved) then
      for i := 0 to count - 1 do
      begin
        Notify(Self, oldItems[i], action);
        FreeObject(oldItems[i]);
      end
    else
      for i := 0 to count - 1 do
        Notify(Self, oldItems[i], action)
  else
    if OwnsObjects and (action = caRemoved) then
      for i := 0 to count - 1 do
        FreeObject(oldItems[i]);

  if Assigned(result) then
    TArray<T>(result^) := oldItems;
end;

procedure TAbstractArrayList<T>.DeleteRange(index, count: Integer);
var
  listCount, tailCount: Integer;
  source, target: PT;
begin
  listCount := Self.Count;
  CheckRange(index, count, listCount);

  if count = 0 then
    Exit;

  if OwnsObjects or Assigned(Notify) then
  begin
    DeleteRangeInternal(index, count, caRemoved, False, nil);
    Exit;
  end;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  tailCount := listCount - (index + count);
  Dec(fCount, count);
  target := @fItems[index];
  source := @fItems[index + count];

  if ItemType.IsManaged then
    FinalizeArray(target, TypeInfo(T), count);
  if tailCount > 0 then
  begin
    if ItemType.HasWeakRef then
    begin
      MoveManaged(source, target, TypeInfo(T), tailCount);
      FinalizeArray(source, TypeInfo(T), tailCount);
    end
    else
      System.Move(source^, target^, SizeOf(T) * tailCount);
    Inc(target, count);
  end;
  System.FillChar(target^, SizeOf(T) * count, 0);
end;

procedure TAbstractArrayList<T>.Sort;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) = tkClass then
    TTimSort.Sort(fItems, IComparer<Pointer>(fComparer), 0, Count)
  else
  {$ENDIF}
  TArray.Sort<T>(fItems, fComparer, 0, Count);

  Reset;
end;

procedure TAbstractArrayList<T>.Sort(const comparer: IComparer<T>);
begin
  Sort(comparer, 0, Count);
end;

procedure TAbstractArrayList<T>.Sort(const comparer: TComparison<T>);
begin
  Sort(IComparer<T>(PPointer(@comparer)^), 0, Count);
end;

procedure TAbstractArrayList<T>.Sort(const comparer: TComparison<T>; index, count: Integer);
begin
  Sort(IComparer<T>(PPointer(@comparer)^), index, count);
end;

procedure TAbstractArrayList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  CheckRange(index, count, Self.Count);

  if count < 2 then
    Exit;

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
  listCount, sourceIndex, targetIndex, itemCount: Integer;
  temp: T;
begin
  listCount := Count;
  if Cardinal(currentIndex) >= Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.sourceIndex);
  if Cardinal(newIndex) >= Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.targetIndex);

  if currentIndex = newIndex then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

  temp := fItems[currentIndex];
  if ItemType.IsManaged then
    fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
  begin
    targetIndex := currentIndex;
    sourceIndex := targetIndex + 1;
    itemCount := newIndex - currentIndex;
  end
  else
  begin
    sourceIndex := newIndex;
    targetIndex := sourceIndex + 1;
    itemCount := currentIndex - newIndex;
  end;
  if ItemType.HasWeakRef then
  begin
    MoveManaged(@fItems[sourceIndex], @fItems[targetIndex], TypeInfo(T), itemCount);
    FinalizeArray(@fItems[newIndex], TypeInfo(T), 1);
  end
  else
    System.Move(fItems[sourceIndex], fItems[targetIndex], SizeOf(T) * itemCount);

  if ItemType.IsManaged then
    if SizeOf(T) = SizeOf(Pointer) then
      PPointer(@fItems[newIndex])^ := nil
    else
      System.FillChar(fItems[newIndex], SizeOf(T), 0);

  fItems[newIndex] := temp;

  DoNotify(temp, caMoved);
end;

function TAbstractArrayList<T>.MoveTo(const collection: ICollection<T>;
  const predicate: Predicate<T>): Integer;
var
  i: Integer;
  item: T;
begin
  if not Assigned(collection) then RaiseHelper.ArgumentNil(ExceptionArgument.collection);

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

function TAbstractArrayList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  case TType.Kind<T> of
    tkClass:
      if IID = IObjectListGuid then
        Exit(TRefCountedObject(Self).QueryInterface(IListOfTGuid, Obj));
    tkInterface:
      if IID = IInterfaceListGuid then
        Exit(TRefCountedObject(Self).QueryInterface(IListOfTGuid, Obj));
  end;
  Result := inherited QueryInterface(IID, Obj);
end;

procedure TAbstractArrayList<T>.Exchange(index1, index2: Integer);
var
  listCount: Integer;
  temp: T;
begin
  listCount := Count;
  if Cardinal(index1) >= Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.index1);
  if Cardinal(index2) >= Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.index2);

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

function TAbstractArrayList<T>.Remove(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item, 0, Count);
  if index >= 0 then
  begin
    DeleteInternal(index, caRemoved);
    Exit(True);
  end;
  Result := False;
end;

function TAbstractArrayList<T>.RemoveAll(const match: Predicate<T>): Integer;
begin
  Result := DeleteAllInternal(match, caRemoved, nil);
end;

procedure TAbstractArrayList<T>.Reverse;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  TArray.Reverse<T>(fItems, 0, Count);

  Reset;
end;

procedure TAbstractArrayList<T>.Reverse(index, count: Integer);
begin
  CheckRange(index, count, Self.Count);

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
var
  deleteCount: Integer;
begin
  if value < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  if value > fCapacity then
    SetCapacity(value);
  deleteCount := Count - value;
  if deleteCount > 0 then
    DeleteRange(value, deleteCount);
  fCount := (fCount and OwnsObjectsMask) or value;
end;

procedure TAbstractArrayList<T>.Delete(index: Integer);
begin
  CheckIndex(index, Self.Count);

  DeleteInternal(index, caRemoved);
end;

function TAbstractArrayList<T>.DeleteAllInternal(const match: Predicate<T>;
  action: TCollectionChangedAction; const items: TArray<T>): Integer;
var
  listCount, freeIndex, current, i: Integer;
begin
  if not Assigned(match) then RaiseHelper.ArgumentNil(ExceptionArgument.match);

  listCount := Count;
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
  CheckIndex(index, Self.Count);

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
begin
  CheckRange(index, count, Self.Count);

  if count = 0 then
    Exit(nil);

  DeleteRangeInternal(index, count, caExtracted, False, @Result);
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
  CheckRange(index, Count, DynArrayLength(values));

  itemCount := Count;
  if itemCount > 0 then
    if ItemType.IsManaged then
      MoveManaged(@fItems[0], @values[index], TypeInfo(T), itemCount)
    else
      System.Move(fItems[0], values[index], SizeOf(T) * itemCount);
end;

function TAbstractArrayList<T>.ToArray: TArray<T>;
begin
  DynArrayCopyRange(Pointer(Result), fItems, TypeInfo(TArray<T>), 0, Count);
end;

procedure TAbstractArrayList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TAbstractArrayList<T>.TryGetElementAt(var value: T; index: Integer): Boolean;
begin
  Result := Cardinal(index) < Cardinal(Count);
  if Result then
    value := fItems[index];
end;

function TAbstractArrayList<T>.TryGetFirst(var value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[0]
  else
    value := Default(T);
end;

function TAbstractArrayList<T>.TryGetLast(var value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := fItems[Count - 1]
  else
    value := Default(T);
end;

function TAbstractArrayList<T>.TryGetSingle(var value: T): Boolean;
begin
  Result := Count = 1;
  if Result then
    value := fItems[0]
  else
    value := Default(T);
end;

function TAbstractArrayList<T>.TryGetSingle(var value: T;
  const predicate: Predicate<T>): Boolean;
var
  item, foundItem: PT;
  index: Integer;
begin
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  item := Pointer(fItems);
  foundItem := nil;
  for index := 1 to Count do
  begin
    if predicate(item^) then
    begin
      if Assigned(foundItem) then
      begin
        foundItem := nil;
        Break;
      end;
      foundItem := item;
    end;
    Inc(item);
  end;

  if Assigned(foundItem) then
  begin
    value := foundItem^;
    Result := True;
  end
  else
  begin
    value := Default(T);
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TAbstractArrayList<T>.TEnumerator'}

procedure TAbstractArrayList<T>.TEnumerator.BeforeDestruction;
begin
  fSource._Release;
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
      Result := True;
    end
    else
    begin
      fCurrent := Default(T);
      Result := False;
    end;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
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
  enumerator: IEnumerator<T>;
  item: T;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    Add(item);
  end;
end;

procedure TSortedList<T>.AddRange(const values: array of T);
var
  i: Integer;
begin
  for i := 0 to High(values) do
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
  RaiseHelper.NotSupported;
end;

function TSortedList<T>.IndexOf(const item: T): Integer;
begin
  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, 0, Count) then
    Result := -1;
end;

function TSortedList<T>.IndexOf(const item: T; index: Integer): Integer;
var
  listCount: Integer;
begin
  listCount := Count;
  CheckIndex(index, listCount);

  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, index, listCount - index) then
    Result := -1;
end;

function TSortedList<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  listCount: Integer;
begin
  listCount := Count;
  CheckRange(index, count, listCount);

  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, index, count) then
    Result := -1;
end;

procedure TSortedList<T>.Insert(index: Integer; const item: T);
begin
  RaiseHelper.NotSupported;
end;

function TSortedList<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
var
  listCount: Integer;
begin
  listCount := Count;
  CheckIndex(index, listCount);
  if Cardinal(count) > Cardinal(index + 1) then RaiseHelper.ArgumentOutOfRange_Count;

  if listCount = 0 then
    Exit(-1);

  if not TArray.BinarySearchUpperBound<T>(
    fItems, item, Result, Comparer, index, count) then
    Result := -1;
end;

procedure TSortedList<T>.Move(sourceIndex, targetIndex: Integer);
begin
  RaiseHelper.NotSupported;
end;

procedure TSortedList<T>.Reverse(index, count: Integer);
begin
  RaiseHelper.NotSupported;
end;

procedure TSortedList<T>.SetItem(index: Integer; const value: T);
begin
  RaiseHelper.NotSupported;
end;

procedure TSortedList<T>.Sort;
begin
end;

procedure TSortedList<T>.Sort(const comparer: IComparer<T>);
begin
end;

procedure TSortedList<T>.Sort(const comparer: TComparison<T>);
begin
end;

procedure TSortedList<T>.Sort(const comparer: TComparison<T>; index, count: Integer);
begin
end;

procedure TSortedList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>'}

constructor TCollectionList<T>.Create(const collection: TCollection);
begin
  if not Assigned(collection) then RaiseHelper.ArgumentNil(ExceptionArgument.collection);
  Guard.CheckInheritsFrom(collection.ItemClass, TClass(T), 'collection.ItemClass');

  fCollection := collection;
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
  CheckIndex(index, fCollection.Count);

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
  CheckRange(index, count, fCollection.Count);

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
    for i := 0 to DynArrayHigh(oldItems) do
    begin
      Notify(Self, oldItems[i], caRemoved);
      oldItems[i].Free;
    end
  else
    for i := 0 to DynArrayHigh(oldItems) do
      oldItems[i].Free;
end;

procedure TCollectionList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
  if Cardinal(index1) >= Cardinal(fCollection.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.index1);
  if Cardinal(index2) >= Cardinal(fCollection.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.index2);

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
  index := IndexOf(item, 0, fCollection.Count);
  if index < 0 then
    Result := Default(T)
  else
    Result := ExtractAt(index);
end;

function TCollectionList<T>.ExtractAt(index: Integer): T;
begin
  CheckIndex(index, fCollection.Count);

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
  CheckRange(index, count, fCollection.Count);

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
  CheckIndex(index, fCollection.Count);

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
  CheckRange(index, count, fCollection.Count);

  Result := TCollections.CreateList<T>;
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

function TCollectionList<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  i: Integer;
begin
  CheckRange(index, count, fCollection.Count);

  for i := index to index + count - 1 do
    if fCollection.Items[i].Equals(item) then
      Exit(i);
  Result := -1;
end;

procedure TCollectionList<T>.Insert(index: Integer; const item: T);
begin
  CheckIndex(index, fCollection.Count + 1);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  item.Collection := fCollection;
  item.Index := index;

  DoNotify(item, caAdded);
end;

procedure TCollectionList<T>.InsertRange(index: Integer; const values: array of T);
var
  i: Integer;
begin
  if Cardinal(index) > Cardinal(fCollection.Count) then RaiseHelper.ArgumentOutOfRange_Index;

  for i := 0 to High(values) do
  begin
    Insert(index, values[i]);
    Inc(index);
  end;
end;

procedure TCollectionList<T>.InsertRange(index: Integer; const values: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  if Cardinal(index) > Cardinal(fCollection.Count) then RaiseHelper.ArgumentOutOfRange_Index;
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    item := enumerator.Current;
    Insert(index, item);
    Inc(index);
  end;
end;

function TCollectionList<T>.LastIndexOf(const item: T): Integer;
var
  listCount: Integer;
begin
  listCount := fCollection.Count;
  if listCount > 0 then
    Result := LastIndexOf(item, listCount - 1, listCount)
  else
    Result := -1;
end;

function TCollectionList<T>.LastIndexOf(const item: T; index: Integer): Integer;
begin
  CheckIndex(index, fCollection.Count);

  Result := LastIndexOf(item, index, index + 1);
end;

function TCollectionList<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
var
  i: Integer;
begin
  CheckIndex(index, fCollection.Count);
  if Cardinal(count) > Cardinal(index + 1) then RaiseHelper.ArgumentOutOfRange_Count;

  for i := index downto index - count + 1 do
    if fCollection.Items[i].Equals(item) then
      Exit(i);
  Result := -1;
end;

procedure TCollectionList<T>.Move(sourceIndex, targetIndex: Integer);
begin
  if Cardinal(sourceIndex) >= Cardinal(fCollection.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.sourceIndex);
  if Cardinal(targetIndex) >= Cardinal(fCollection.Count) then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.targetIndex);

  if sourceIndex = targetIndex then
    Exit;

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  fCollection.Items[sourceIndex].Index := targetIndex;

  DoNotify(T(fCollection.Items[targetIndex]), caMoved);
end;

function TCollectionList<T>.Remove(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item, 0, fCollection.Count);
  if index >= 0 then
  begin
    Delete(index);
    Exit(True);
  end;
  Result := False;
end;

procedure TCollectionList<T>.Reverse;
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.Reverse(index, count: Integer);
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.SetCapacity(value: Integer);
begin
  fCollection.Capacity := value;
end;

procedure TCollectionList<T>.SetCount(value: Integer);
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.SetItem(index: Integer; const value: T);
begin
  CheckIndex(index, fCollection.Count);

  fCollection.Items[index] := value;
end;

procedure TCollectionList<T>.SetOwnsObjects(value: Boolean);
begin
end;

procedure TCollectionList<T>.Sort;
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.Sort(const comparer: IComparer<T>);
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.Sort(const comparer: TComparison<T>);
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.Sort(const comparer: IComparer<T>; index, count: Integer);
begin
  RaiseHelper.NotSupported;
end;

procedure TCollectionList<T>.Sort(const comparer: TComparison<T>; index, count: Integer);
begin
  RaiseHelper.NotSupported;
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
  fSource := list;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TCollectionList<T>.TEnumerator.Destroy;
begin
  fSource._Release;
end;

function TCollectionList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TCollectionList<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion = fSource.fVersion then
  begin
    if fIndex < fSource.fCollection.Count then
    begin
      fCurrent := T(fSource.fCollection.Items[fIndex]);
      Inc(fIndex);
      Result := True;
    end
    else
    begin
      fCurrent := Default(T);
      Result := False;
    end;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TAnonymousReadOnlyList<T>'}

constructor TAnonymousReadOnlyList<T>.Create(const count: Func<Integer>;
  const items: Func<Integer, T>; const iterator: IEnumerable<T>);
begin
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
  CheckRange(index, count, fCount);

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
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));
  for i := index to index + count - 1 do
    if Comparer.Equals(fItems(i), item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TFoldedList<T>'}

constructor TFoldedList<T>.Create(elementType: PTypeInfo;
  const comparer: IComparer<T>; ownsObjects: Boolean);
begin
  fComparer := comparer;
  fElementType := elementType;
  SetOwnsObjects(ownsObjects);
end;

function TFoldedList<T>.CreateList: IList<T>;
begin
  Result := TFoldedList<T>.Create(fElementType, fComparer);
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
  fComparer := comparer;
  fElementType := elementType;
  SetOwnsObjects(ownsObjects);
end;

function TFoldedSortedList<T>.CreateList: IList<T>;
begin
  Result := TFoldedList<T>.Create(fElementType, fComparer);
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
