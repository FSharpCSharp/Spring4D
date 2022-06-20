{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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
  Spring.Collections.Base,
  Spring.Events;

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
      PEnumerator = ^TEnumerator;
      TEnumerator = record
        Vtable: Pointer;
        RefCount: Integer;
        TypeInfo: PTypeInfo;
        fSource: TAbstractArrayList<T>;
        fIndex, fVersion: Integer;
        function GetCurrent: T;
        function MoveNext: Boolean;
        class var Enumerator_Vtable: TEnumeratorVtable;
      end;
      ItemType = TTypeInfo<T>;

      // internal helper type to solve compiler issue with using Slice on the
      // overloaded AddRange method in older Delphi versions
      // keep this in sync with the ICollection<T> interface from Spring.Collections
      ICollectionInternal = interface(IEnumerable<T>)
        function GetOnChanged: ICollectionChangedEvent<T>;
        function Add(const item: T): Boolean;
        procedure AddRange(const values: array of T);
      end;

  {$ENDREGION}
    {$IFDEF DELPHIXE7_UP}
    class var DefaultComparer: Pointer;
    {$ENDIF}
  private
    fItems: TArray<T>;
    fCapacity: Integer;
    fCount: Integer;
    fVersion: Integer;
    function CanInlineEquals: Boolean; inline;
    procedure Grow(capacity: Integer);
    procedure DeleteInternal(index: Integer; action: TCollectionChangedAction);
    function DeleteAllInternal(const match: Predicate<T>;
      action: TCollectionChangedAction; const items: TArray<T>): Integer;
    procedure DeleteRangeInternal(index, count: Integer;
      action: TCollectionChangedAction; doClear: Boolean; result: PPointer);
    function InsertInternal(index: Integer; const item: T): Integer;
    procedure SetItemInternal(index: Integer; const value: T);
    procedure DoNotifyExtracted(count: Integer);
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetCountFast: Integer;
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

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property OwnsObjects: Boolean read GetOwnsObjects;
  public
    constructor Create(const comparer: IComparer<T>); overload;
    procedure AfterConstruction; override;
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

    function CopyTo(var values: TArray<T>; index: Integer): Integer;
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
        fSource: TCollectionList<T>;
        fIndex: Integer;
        fVersion: Integer;
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
    function GetCountFast: Integer;
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
    IEnumerable<T>, IReadOnlyCollection<T>, IReadOnlyList<T>)
  private
    fCount: Func<Integer>;
    fItems: Func<Integer, T>;
    fIterator: IEnumerable<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
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
    fOnPropertyChanged: TPropertyChangedEventImpl;
    function GetOnPropertyChanged: IInvokableEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(sender: TObject;
      const eventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const propertyName: string);
    procedure Changed(const value: TObject; action: TCollectionChangedAction); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TObservableInterfaceList = class(TFoldedList<IInterface>, INotifyPropertyChanged)
  private
    fOnPropertyChanged: TPropertyChangedEventImpl;
    function GetOnPropertyChanged: IInvokableEvent<TPropertyChangedEvent>;
  protected
    procedure DoItemPropertyChanged(sender: TObject;
      const eventArgs: IPropertyChangedEventArgs);
    procedure DoPropertyChanged(const propertyName: string);
    procedure Changed(const value: IInterface; action: TCollectionChangedAction); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
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
  Spring.Comparers,
  Spring.Events.Base,
  Spring.ResourceStrings;

type
  TArray = class(Spring.TArray);


{$REGION 'TAbstractArrayList<T>'}

constructor TAbstractArrayList<T>.Create(const comparer: IComparer<T>);
begin
  fComparer := comparer;
end;

function TAbstractArrayList<T>.CreateList: IList<T>;
begin
  Result := TList<T>.Create(fComparer);
end;

procedure TAbstractArrayList<T>.AfterConstruction;
begin
  inherited AfterConstruction;
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(T) in FastComparableTypes) and (SizeOf(T) in [1, 2, 4, 8]) then
    if DefaultComparer = nil then
      DefaultComparer := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
{$ENDIF}
end;

procedure TAbstractArrayList<T>.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

function TAbstractArrayList<T>.CanInlineEquals: Boolean;
begin
  Result :=
{$IFDEF DELPHIXE7_UP}
    (GetTypeKind(T) = tkClass)
    or ((GetTypeKind(T) in FastComparableTypes) and (SizeOf(T) in [1, 2, 4, 8])
    and (Pointer(fComparer) = DefaultComparer));
{$ELSE}
    PTypeInfo(TypeInfo(T)).Kind = tkClass;
{$ENDIF}
end;

function TAbstractArrayList<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TAbstractArrayList<T>.GetCount: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TAbstractArrayList<T>.GetCountFast: Integer;
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) <> tkClass then
    Result := fCount
  else
  {$ENDIF}
  Result := fCount and CountMask;
end;

function TAbstractArrayList<T>.GetOwnsObjects: Boolean;
begin
  Result := {$IFDEF DELPHIXE7_UP}(GetTypeKind(T) = tkClass) and {$ENDIF}(fCount < 0);
end;

function TAbstractArrayList<T>.Add(const item: T): Integer;
var
  listCount: Integer;
  items: Pointer;
begin
  listCount := Count;
  if (listCount <> fCapacity) and not Assigned(Notify) then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    Inc(fCount);
    items := fItems;
    TArray<T>(items)[listCount] := item;
    Result := listCount;
  end
  else
    Result := InsertInternal(listCount, item);
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
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

function TAbstractArrayList<T>.GetItem(index: Integer): T;
var
  items: Pointer;
begin
  items := fItems;
  if Cardinal(index) < Cardinal(Count) then
    Exit(TArray<T>(items)[index]);
  RaiseHelper.ArgumentOutOfRange_Index;
  {$IFDEF DELPHIXE7_UP}{$IFDEF CPUX86}{$IFDEF OPTIMIZATION_ON}
  // cause the compiler to omit push instruction for types that return in eax
  if GetTypeKind(T) in [tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkClassRef, tkPointer, tkProcedure] then
    Result := Default(T);
  {$ENDIF}{$ENDIF}{$ENDIF}
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
begin
  Result := IndexOf(item, index, Count - index);
end;

function TAbstractArrayList<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  listCount, i: Integer;
{$IFDEF DELPHIXE7_UP}
  i8: Int8;
  i16: Int32;
  i32: Int32;
  i64: Int64;
{$ENDIF}
  obj: TObject;
  items, comparer: Pointer;
begin
  listCount := Self.Count;
  if Cardinal(listCount) >= Cardinal(index) then
    if (count >= 0) and (listCount - count >= index) then
      if CanInlineEquals then
      begin
        items := fItems;
        Result := index;
        {$POINTERMATH ON}
        {$IFDEF DELPHIXE7_UP}
        case GetTypeKind(T) of
          tkClass, tkUString:;
        else
          case SizeOf(T) of
            1: i8 := Int8((@item)^);
            2: i16 := Int16((@item)^);
            4: i32 := Int32((@item)^);
            8: i64 := Int64((@item)^);
          end;
        end;
        for i := 1 to count do
        begin
          case GetTypeKind(T) of
            tkClass:
            begin
              obj := PObject(items)[Result];
              if (obj = PObject(@item)^) or (Assigned(obj) and obj.Equals(PObject(@item)^)) then Exit;
            end;
            tkUString: if PString(items)[Result] = PString(@item)^ then Exit;
          else
            case SizeOf(T) of
              1: if PInt8(items)[Result] = i8 then Exit;
              2: if PInt16(items)[Result] = i16 then Exit;
              4: if PInt32(items)[Result] = i32 then Exit;
              8: if PInt64(items)[Result] = i64 then Exit;
            end;
          end;
          Inc(Result);
        end;
        {$ELSE}
        for i := 1 to count do
        begin
          obj := PObject(items)[Result];
          if (obj = PObject(@item)^) or (Assigned(obj) and obj.Equals(PObject(@item)^)) then Exit;
          Inc(Result);
        end;
        {$ENDIF}
        {$POINTERMATH OFF}
      end
      else
      begin
        comparer := Pointer(fComparer);
        items := fItems;
        i := index + count;
        while True do
        begin
          if index = i then Break;
          if IComparer<T>(comparer).Compare(TArray<T>(items)[index], item) <> 0 then
            Inc(index)
          else
            Exit(index);
        end;
      end
    else
      RaiseHelper.ArgumentOutOfRange_Count
  else
    RaiseHelper.ArgumentOutOfRange_Index;
  Result := -1;
end;

procedure TAbstractArrayList<T>.SetItem(index: Integer; const value: T);
var
  listCount: Integer;
begin
  listCount := Count;
  if Cardinal(index) < Cardinal(listCount) then
  begin
    if not Assigned(Notify) then
    begin
      {$Q-}
      Inc(fVersion);
      {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

      if OwnsObjects then
        FreeObject(fItems[index]);
      fItems[index] := value;
      Exit;
    end;
    SetItemInternal(index, value);
  end
  else
    RaiseHelper.ArgumentOutOfRange_Index;
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
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(T) = tkClass then
  {$ELSE}
  if TType.Kind<T> = tkClass then
  {$ENDIF}
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
begin
  listCount := Count;
  if Cardinal(index) > Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange_Index;

  if listCount = fCapacity then
    Grow(listCount + 1);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Inc(fCount);
  tailCount := listCount - index;
  if tailCount > 0 then
    if ItemType.HasWeakRef then
      MoveManaged(@fItems[index], @fItems[index + 1], TypeInfo(T), tailCount)
    else
    begin
      System.Move(fItems[index], fItems[index + 1], SizeOf(T) * tailCount);
      if ItemType.IsManaged then
        if SizeOf(T) = SizeOf(Pointer) then
          TArray<Pointer>(fItems)[index] := nil
        else
          System.FillChar(fItems[index], SizeOf(T), 0);
    end;
  fItems[index] := item;

  DoNotify(item, caAdded);
  Result := index;
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer; const values: array of T);
var
  listCount, valueCount, tailCount, i: Integer;
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
  tailCount := listCount - index;
  if tailCount > 0 then
    if ItemType.HasWeakRef then
      MoveManaged(@fItems[index], @fItems[index + valueCount], TypeInfo(T), tailCount)
    else
    begin
      System.Move(fItems[index], fItems[index + valueCount], SizeOf(T) * tailCount);
      if ItemType.IsManaged then
        System.FillChar(fItems[index], SizeOf(T) * valueCount, 0);
    end;

  if ItemType.IsManaged then
    MoveManaged(@values[0], @fItems[index], TypeInfo(T), valueCount)
  else
    System.Move(values[0], fItems[index], SizeOf(T) * valueCount);

  if Assigned(Notify) then
    for i := 0 to High(values) do
      Notify(Self, values[i], caAdded);
end;

procedure TAbstractArrayList<T>.InsertRange(index: Integer; const values: IEnumerable<T>);
var
  listCount, valueCount, tailCount, i: Integer;
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
    tailCount := listCount - index;
    if tailCount > 0 then
      if ItemType.IsManaged then
        MoveManaged(@fItems[index], @fItems[index + valueCount], TypeInfo(T), tailCount)
      else
        System.Move(fItems[index], fItems[index + valueCount], SizeOf(T) * tailCount);

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
  Result := LastIndexOf(item, index, index + 1);
end;

function TAbstractArrayList<T>.LastIndexOf(const item: T; index, count: Integer): Integer;
var
  listCount, i: Integer;
{$IFDEF DELPHIXE7_UP}
  i8: Int8;
  i16: Int32;
  i32: Int32;
  i64: Int64;
{$ENDIF}
  obj: TObject;
  items, comparer: Pointer;
begin
  listCount := Self.Count;
  if (Cardinal(index) < Cardinal(listCount)) or (index or listcount = 0) then
    if Cardinal(count) <= Cardinal(index + 1) then
      if CanInlineEquals then
      begin
        items := fItems;
        Result := index;
        {$POINTERMATH ON}
        {$IFDEF DELPHIXE7_UP}
        case GetTypeKind(T) of
          tkClass, tkUString:;
        else
          case SizeOf(T) of
            1: i8 := Int8((@item)^);
            2: i16 := Int16((@item)^);
            4: i32 := Int32((@item)^);
            8: i64 := Int64((@item)^);
          end;
        end;
        for i := 1 to count do
        begin
          case GetTypeKind(T) of
            tkClass:
            begin
              obj := PObject(items)[Result];
              if (obj = PObject(@item)^) or (Assigned(obj) and obj.Equals(PObject(@item)^)) then Exit;
            end;
            tkUString: if PString(items)[Result] = PString(@item)^ then Exit;
          else
            case SizeOf(T) of
              1: if PInt8(items)[Result] = i8 then Exit;
              2: if PInt16(items)[Result] = i16 then Exit;
              4: if PInt32(items)[Result] = i32 then Exit;
              8: if PInt64(items)[Result] = i64 then Exit;
            end;
          end;
          Dec(Result);
        end;
        {$ELSE}
        for i := 1 to count do
        begin
          obj := PObject(items)[Result];
          if (obj = PObject(@item)^) or (Assigned(obj) and obj.Equals(PObject(@item)^)) then Exit;
          Dec(Result);
        end;
        {$ENDIF}
        {$POINTERMATH OFF}
      end
      else
      begin
        comparer := Pointer(fComparer);
        items := fItems;
        i := index - count;
        while True do
        begin
          if index = i then Break;
          if IComparer<T>(comparer).Compare(TArray<T>(items)[index], item) <> 0 then
            Dec(index)
          else
            Exit(index);
        end;
      end
    else
      RaiseHelper.ArgumentOutOfRange_Count
  else
    RaiseHelper.ArgumentOutOfRange_Index;
  Result := -1;
end;

procedure TAbstractArrayList<T>.DeleteInternal(index: Integer;
  action: TCollectionChangedAction);
var
  listCount, tailCount: Integer;
  oldItem: T;
begin
  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  Dec(fCount);
  listCount := Count;

  oldItem := fItems[index];
  tailCount := listCount - index;
  if tailCount > 0 then
    if ItemType.HasWeakRef then
      MoveManaged(@fItems[index + 1], @fItems[index], TypeInfo(T), tailCount)
    else
    begin
      if ItemType.IsManaged then
        fItems[index] := Default(T);
      System.Move(fItems[index + 1], fItems[index], SizeOf(T) * tailCount);
      if ItemType.IsManaged then
        if SizeOf(T) = SizeOf(Pointer) then
          TArray<Pointer>(fItems)[index + tailCount] := nil
        else
          System.FillChar(fItems[index + tailCount], SizeOf(T), 0);
    end;
  fItems[index + tailCount] := Default(T);

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

procedure TAbstractArrayList<T>.DoNotifyExtracted(count: Integer);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    Notify(Self, fItems[i], caExtracted);
end;

procedure TAbstractArrayList<T>.DeleteRange(index, count: Integer);
var
  listCount, tailCount: Integer;
begin
  listCount := Self.Count;
  if Cardinal(index) <= Cardinal(listCount) then
  begin
    if count = 0 then
      Exit;

    tailCount := listCount - index - count;
    if tailCount >= 0 then
    begin
      if not OwnsObjects and not Assigned(Notify) then
      begin
        {$Q-}
        Inc(fVersion);
        {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
        Dec(fCount, count);

        if ItemType.IsManaged then
          System.Finalize(fItems[index], count);
        if tailCount > 0 then
          if ItemType.HasWeakRef then
          begin
            MoveManaged(@fItems[index + count], @fItems[index], TypeInfo(T), tailCount);
            System.Finalize(fItems[index + tailCount], count);
          end
          else
            System.Move(fItems[index + count], fItems[index], SizeOf(T) * tailCount);
        System.FillChar(fItems[index + tailCount], SizeOf(T) * count, 0);
      end
      else
        DeleteRangeInternal(index, count, caRemoved, False, nil);
    end
    else
      RaiseHelper.ArgumentOutOfRange_Count;
  end
  else
    RaiseHelper.ArgumentOutOfRange_Index;
end;

procedure TAbstractArrayList<T>.Sort;
begin
  Sort(fComparer, 0, Count);
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
var
  listCount: Integer;
begin
  listCount := Self.Count;
  if Cardinal(index) <= Cardinal(listCount) then
    {$Q-}
    if (count >= 0) and (index <= listCount - count) then
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    begin
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
      begin
        {$R-}
        {$IFDEF DELPHIXE7_UP}
        case GetTypeKind(T) of
          tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkLString, tkWString,
          tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef, tkPointer, tkProcedure:
            case SizeOf(T) of
              1: TArray.IntroSort_Int8(Slice(TSlice<Int8>((@fItems[index])^), count), IComparer<Int8>(comparer));
              2: TArray.IntroSort_Int16(Slice(TSlice<Int16>((@fItems[index])^), count), IComparer<Int16>(comparer));
              4: TArray.IntroSort_Int32(Slice(TSlice<Int32>((@fItems[index])^), count), IComparer<Int32>(comparer));
              8: TArray.IntroSort_Int64(Slice(TSlice<Int64>((@fItems[index])^), count), IComparer<Int64>(comparer));
            end;
          tkFloat:
            case SizeOf(T) of
              4: TArray.IntroSort_Single(Slice(TSlice<System.Single>((@fItems[index])^), count), IComparer<System.Single>(comparer));
              10,16: TArray.IntroSort_Extended(Slice(TSlice<Extended>((@fItems[index])^), count), IComparer<Extended>(comparer));
            else
              if GetTypeData(TypeInfo(T)).FloatType = ftDouble then
                TArray.IntroSort_Double(Slice(TSlice<Double>((@fItems[index])^), count), IComparer<Double>(comparer))
              else
                TArray.IntroSort_Int64(Slice(TSlice<Int64>((@fItems[index])^), count), IComparer<Int64>(comparer));
            end;
          tkString:
            TArray.IntroSort_Ref(@fItems[index], index + count - 1, IComparerRef(comparer), SizeOf(T));
          tkSet:
            case SizeOf(T) of
              1: TArray.IntroSort_Int8(Slice(TSlice<Int8>((@fItems[index])^), count), IComparer<Int8>(comparer));
              2: TArray.IntroSort_Int16(Slice(TSlice<Int16>((@fItems[index])^), count), IComparer<Int16>(comparer));
              4: TArray.IntroSort_Int32(Slice(TSlice<Int32>((@fItems[index])^), count), IComparer<Int32>(comparer));
            else
              TArray.IntroSort_Ref(@fItems[index], index + count - 1, IComparerRef(comparer), SizeOf(T));
            end;
          tkMethod:
            TArray.IntroSort_Method(Slice(TSlice<TMethodPointer>((@fItems[index])^), count), IComparer<TMethodPointer>(comparer));
          tkVariant,
          {$IF Declared(tkMRecord)}
          tkMRecord,
          {$IFEND}
          tkRecord:
            if not System.HasWeakRef(T) then
              case SizeOf(T) of
                1: TArray.IntroSort_Int8(Slice(TSlice<Int8>((@fItems[index])^), count), IComparer<Int8>(comparer));
                2: TArray.IntroSort_Int16(Slice(TSlice<Int16>((@fItems[index])^), count), IComparer<Int16>(comparer));
                3: TArray.IntroSort_Int24(Slice(TSlice<Int24>((@fItems[index])^), count), IComparer<Int24>(comparer));
                4: TArray.IntroSort_Int32(Slice(TSlice<Int32>((@fItems[index])^), count), IComparer<Int32>(comparer));
              else
                TArray.IntroSort_Ref(@fItems[index], index + count - 1, IComparerRef(comparer), SizeOf(T))
              end
            else
              TArray.IntroSort<T>(Slice(TSlice<T>((@fItems[index])^), count), comparer);
          tkArray:
            case SizeOf(T) of
              1: TArray.IntroSort_Int8(Slice(TSlice<Int8>((@fItems[index])^), count), IComparer<Int8>(comparer));
              2: TArray.IntroSort_Int16(Slice(TSlice<Int16>((@fItems[index])^), count), IComparer<Int16>(comparer));
              3: TArray.IntroSort_Int24(Slice(TSlice<Int24>((@fItems[index])^), count), IComparer<Int24>(comparer));
              4: TArray.IntroSort_Int32(Slice(TSlice<Int32>((@fItems[index])^), count), IComparer<Int32>(comparer));
            else
              TArray.IntroSort_Ref(@fItems[index], index + count - 1, IComparerRef(comparer), SizeOf(T));
            end;
        else
        {$ELSE}
        begin
        {$ENDIF}
          TArray.IntroSort<T>(Slice(TSlice<T>((@fItems[index])^), count), comparer);
        end;
        {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
      end;

      Reset;
    end
    else
      RaiseHelper.ArgumentOutOfRange_Count
  else
    RaiseHelper.ArgumentOutOfRange_Index;
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
    System.Finalize(fItems[newIndex], 1);
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

function TAbstractArrayList<T>.MoveTo(const collection: ICollection<T>): Integer;
begin
  Result := Count;
  if Result > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    Dec(fCount, Result);
    if Assigned(Notify) then
      DoNotifyExtracted(Result);
    // hardcast to solve compiler glitch in older Delphi versions due to AddRange overload
    ICollectionInternal(collection).AddRange(Slice(TSlice<T>((@fItems[0])^), Result));
    if ItemType.IsManaged then
      System.Finalize(fItems[0], Result);
    System.FillChar(fItems[0], SizeOf(T) * Result, 0);
  end;
end;

function TAbstractArrayList<T>.MoveTo(const collection: ICollection<T>;
  const predicate: Predicate<T>): Integer;
var
  values: TArray<T>;
begin
  if not Assigned(collection) then RaiseHelper.ArgumentNil(ExceptionArgument.collection);
  if not Assigned(predicate) then RaiseHelper.ArgumentNil(ExceptionArgument.predicate);

  SetLength(values, Self.Count);
  Result := DeleteAllInternal(predicate, caExtracted, values);
  // hardcast to solve compiler glitch in older Delphi versions due to AddRange overload
  ICollectionInternal(collection).AddRange(Slice(TSlice<T>((@values[0])^), Result));
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
  TArray.Reverse<T>(fItems, Count - 1);

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
  TArray.Reverse<T>(@fItems[index], count - 1);

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
  if Cardinal(index) < Cardinal(Count) then
    DeleteInternal(index, caRemoved)
  else
    RaiseHelper.ArgumentOutOfRange_Index;
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
      System.Finalize(fItems[freeIndex], Result)
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
var
  index: Integer;
begin
  index := IndexOf(value);
  Result := index > -1;
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

function TAbstractArrayList<T>.CopyTo(var values: TArray<T>; index: Integer): Integer;
var
  len, listCount: Integer;
begin
  len := DynArrayLength(values);
  if Cardinal(index) <= Cardinal(len) then
  begin
    listCount := Count;
    if index <= len - listCount then
    begin
      if listCount > 0 then
        if ItemType.IsManaged then
          MoveManaged(@fItems[0], @values[index], TypeInfo(T), listCount)
        else
          System.Move(fItems[0], values[index], SizeOf(T) * listCount);
      Result := listCount;
    end
    else
      Result := RaiseHelper.ArgumentOutOfRange_Count;
  end
  else
    Result := RaiseHelper.ArgumentOutOfRange_Index;
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
var
  items: Pointer;
begin
  items := fItems;
  if Cardinal(index) < Cardinal(Count) then
  begin
    value := TArray<T>(items)[index];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TAbstractArrayList<T>.TryGetFirst(var value: T): Boolean;
begin
  if Count > 0 then
  begin
    value := fItems[0];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TAbstractArrayList<T>.TryGetLast(var value: T): Boolean;
var
  index: Integer;
begin
  index := Count - 1;
  if index >= 0 then
  begin
    value := fItems[index];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

function TAbstractArrayList<T>.TryGetSingle(var value: T): Boolean;
begin
  if Count = 1 then
  begin
    value := fItems[0];
    Exit(True);
  end;
  value := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TAbstractArrayList<T>.TEnumerator'}

function TAbstractArrayList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fSource.fItems[fIndex - 1];
end;

function TAbstractArrayList<T>.TEnumerator.MoveNext: Boolean;
var
  index: Integer;
begin
  if fVersion = fSource.fVersion then
  begin
    index := fIndex;
    fIndex := index + 1;
    Result := index < fSource.Count;
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
  if Cardinal(index) > Cardinal(listCount) then RaiseHelper.ArgumentOutOfRange_Index;

  if not TArray.BinarySearch<T>(fItems, item, Result, Comparer, index, listCount - index) then
    Result := -1;
end;

function TSortedList<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  listCount: Integer;
begin
  listCount := Self.Count;
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

function TCollectionList<T>.GetCountFast: Integer;
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
  Result := T(fSource.fCollection.Items[fIndex - 1]);
end;

function TCollectionList<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion = fSource.fVersion then
  begin
    Result := fIndex < fSource.fCollection.Count;
    Inc(fIndex, Ord(Result));
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

function TAnonymousReadOnlyList<T>.GetCountFast: Integer;
begin
  Result := fCount;
end;

function TAnonymousReadOnlyList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fIterator.GetEnumerator;
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
  comparer: Pointer;
  i: Integer;
begin
  comparer := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
  for i := index to index + count - 1 do
    if IEqualityComparer<T>(comparer).Equals(fItems(i), item) then
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

procedure TObservableObjectList.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnPropertyChanged := TPropertyChangedEventImpl.Create;
  fOnPropertyChanged._AddRef;
end;

procedure TObservableObjectList.BeforeDestruction;
begin
  fOnPropertyChanged._Release;
  fOnPropertyChanged := nil;
  inherited BeforeDestruction;
end;

procedure TObservableObjectList.Changed(const value: TObject;
  action: TCollectionChangedAction);
var
  intf: IInterface;
begin
  if value.GetInterface(INotifyPropertyChanged, Pointer(intf)) then
  begin
    intf := INotifyPropertyChanged(intf).OnPropertyChanged;
    case Action of
      caAdded: IEvent<TPropertyChangedEvent>(intf).Add(DoItemPropertyChanged);
      caRemoved, caExtracted: IEvent<TPropertyChangedEvent>(intf).Remove(DoItemPropertyChanged);
    end;
  end;

  inherited Changed(value, action);
  DoPropertyChanged('Count');
end;

procedure TObservableObjectList.DoItemPropertyChanged(sender: TObject;
  const eventArgs: IPropertyChangedEventArgs);
begin
  inherited Changed(sender, caChanged);
end;

procedure TObservableObjectList.DoPropertyChanged(const propertyName: string);
begin
  with fOnPropertyChanged do if CanInvoke then
    Invoke(Self, TPropertyChangedEventArgs.Create(propertyName) as IPropertyChangedEventArgs);
end;

function TObservableObjectList.GetOnPropertyChanged: IInvokableEvent<TPropertyChangedEvent>;
begin
  Result := fOnPropertyChanged;
end;

{$ENDREGION}


{$REGION 'TObservableInterfaceList'}

procedure TObservableInterfaceList.AfterConstruction;
begin
  inherited AfterConstruction;
  fOnPropertyChanged := TPropertyChangedEventImpl.Create;
  fOnPropertyChanged._AddRef;
end;

procedure TObservableInterfaceList.BeforeDestruction;
begin
  fOnPropertyChanged._Release;
  fOnPropertyChanged := nil;
  inherited BeforeDestruction;
end;

procedure TObservableInterfaceList.Changed(const value: IInterface;
  action: TCollectionChangedAction);
var
  intf: IInterface;
begin
  if value.QueryInterface(INotifyPropertyChanged, Pointer(intf)) = S_OK then
  begin
    intf := INotifyPropertyChanged(intf).OnPropertyChanged;
    case Action of
      caAdded: IEvent<TPropertyChangedEvent>(intf).Add(DoItemPropertyChanged);
      caRemoved, caExtracted: IEvent<TPropertyChangedEvent>(intf).Remove(DoItemPropertyChanged);
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
  sender.GetInterface(fElementType.TypeData.Guid, Pointer(item));
  inherited Changed(item, caChanged);
end;

procedure TObservableInterfaceList.DoPropertyChanged(const propertyName: string);
begin
  with fOnPropertyChanged do if CanInvoke then
    Invoke(Self, TPropertyChangedEventArgs.Create(propertyName) as IPropertyChangedEventArgs);
end;

function TObservableInterfaceList.GetOnPropertyChanged: IInvokableEvent<TPropertyChangedEvent>;
begin
  Result := fOnPropertyChanged;
end;

{$ENDREGION}


end.
