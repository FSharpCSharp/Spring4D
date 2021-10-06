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

unit Spring.Collections.MultiSets;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TAbstractMultiSet<T> = class abstract(TCollectionBase<T>)
  private type
    TEntry = TMultiSetEntry<T>;
  private
    fCount: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
  {$ENDREGION}
    function CreateMultiSet: IMultiSet<T>; virtual; abstract;
  public
  {$REGION 'Implements IMultiSet<T>'}
    function OrderedByCount: IReadOnlyMultiSet<T>;
    function SetEquals(const other: IEnumerable<T>): Boolean;
  {$ENDREGION}
  end;

  THashMultiSetItem<T> = packed record
  public
    HashCode: Integer;
    Item: T;
    Count: Integer;
  end;

  THashMultiSet<T> = class(TAbstractMultiSet<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, IReadOnlyMultiSet<T>,
    ICollection<T>, IMultiSet<T>)
  private type
  {$REGION 'Nested Types'}
    TEntry = TMultiSetEntry<T>;
    TItem = THashMultiSetItem<T>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: THashMultiSet<T>;
      fItemIndex: Integer;
      fRemainingCount: Integer;
      fVersion: Integer;
      fItem: PItem;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TItemCollection = TInnerCollection<T>;

    TEntryCollection = class(TEnumerableBase<TEntry>,
      IEnumerable<TEntry>, IReadOnlyCollection<TEntry>)
    private
      fSource: THashMultiSet<T>;
    {$REGION 'Property Accessors'}
      function GetCount: Integer;
      function GetCountFast: Integer;
    {$ENDREGION}
    public
      constructor Create(const source: THashMultiSet<T>);

    {$REGION 'Implements IInterface'}
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'Implements IEnumerable<TEntry>'}
      function GetEnumerator: IEnumerator<TEntry>;
      function Contains(const value: TEntry): Boolean; overload;
      function ToArray: TArray<TEntry>;
    {$ENDREGION}
    end;

    PEntryEnumerator = ^TEntryEnumerator;
    TEntryEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: THashMultiSet<T>;
      fItemIndex: Integer;
      fVersion: Integer;
      fItem: PItem;
      function GetCurrent: TEntry;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fItems: TItemCollection;
    fEntries: TEntryCollection;
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TEntry>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
    procedure SetItemCount(const item: T; count: Integer);
  {$ENDREGION}
    procedure ClearWithNotify;
  protected
    function CreateMultiSet: IMultiSet<T>; override;
  public
    constructor Create(const comparer: IEqualityComparer<T>);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean; overload;
    function Remove(const item: T): Boolean; overload;
    procedure Clear;
    function Extract(const item: T): T;
  {$ENDREGION}

  {$REGION 'Implements IMultiSet<T>'}
    function Add(const item: T; count: Integer): Integer; overload;
    function Remove(const item: T; count: Integer): Integer; overload;
  {$ENDREGION}
  end;

  TTreeMultiSet<T> = class(TAbstractMultiSet<T>, IEnumerable<T>,
    IReadOnlyCollection<T>, IReadOnlyMultiSet<T>,
    ICollection<T>, IMultiSet<T>)
  private type
  {$REGION 'Nested Types'}
    TEntry = TMultiSetEntry<T>;
    PNode = TNodes<T, Integer>.PRedBlackTreeNode;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TTreeMultiSet<T>;
      fNode: PNode;
      fRemainingCount: Integer;
      fVersion: Integer;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TItemCollection = class(TEnumerableBase<T>,
      IEnumerable<T>, IReadOnlyCollection<T>)
    private
      fSource: TTreeMultiSet<T>;
    {$REGION 'Property Accessors'}
      function GetCount: Integer;
      function GetCountFast: Integer;
    {$ENDREGION}
    public
      constructor Create(const source: TTreeMultiSet<T>);

    {$REGION 'Implements IInterface'}
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'Implements IEnumerable<T>'}
      function GetEnumerator: IEnumerator<T>;
      function Contains(const value: T): Boolean; overload;
      function ToArray: TArray<T>;
    {$ENDREGION}
    end;

    PItemEnumerator = ^TItemEnumerator;
    TItemEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TTreeMultiSet<T>;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TEntryCollection = class(TEnumerableBase<TEntry>,
      IEnumerable<TEntry>, IReadOnlyCollection<TEntry>)
    private
      fSource: TTreeMultiSet<T>;
    {$REGION 'Property Accessors'}
      function GetCount: Integer;
      function GetCountFast: Integer;
    {$ENDREGION}
    public
      constructor Create(const source: TTreeMultiSet<T>);

    {$REGION 'Implements IInterface'}
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'Implements IEnumerable<TEntry>'}
      function GetEnumerator: IEnumerator<TEntry>;
      function Contains(const value: TEntry): Boolean; overload;
      function ToArray: TArray<TEntry>;
    {$ENDREGION}
    end;

    PEntryEnumerator = ^TEntryEnumerator;
    TEntryEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TTreeMultiSet<T>;
      fNode: PBinaryTreeNode;
      fVersion: Integer;
      function GetCurrent: TEntry;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fTree: TRedBlackTree<T, Integer>;
    fVersion: Integer;
    fItems: TItemCollection;
    fEntries: TEntryCollection;
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TEntry>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
    procedure SetItemCount(const item: T; count: Integer);
  {$ENDREGION}
  protected
    function CreateMultiSet: IMultiSet<T>; override;
  public
    constructor Create(const comparer: IComparer<T>);
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean; overload;
    function Remove(const item: T): Boolean; overload;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMultiSet<T>'}
    function Add(const item: T; count: Integer): Integer; overload;
    function Remove(const item: T; count: Integer): Integer; overload;
  {$ENDREGION}
  end;

implementation

uses
  Spring.Comparers,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractMultiSet<T>'}

function TAbstractMultiSet<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TAbstractMultiSet<T>.GetCountFast: Integer;
begin
  Result := fCount;
end;

function TAbstractMultiSet<T>.OrderedByCount: IReadOnlyMultiSet<T>;
var
  entries: TArray<TEntry>;
  items: TArray<TPair<Integer,TEntry>>;
  i: Integer;
  localSet: IMultiSet<T>;
begin
  entries := IMultiSet<T>(this).Entries.ToArray;
  SetLength(items, DynArrayLength(entries));
  for i := 0 to DynArrayHigh(entries) do
  begin
    items[i].Key := i;
    items[i].Value := entries[i];
  end;
  TArray.Sort<TPair<Integer,TEntry>>(items,
    function(const left, right: TPair<Integer,TEntry>): Integer
    begin
      if left.Value.Count > right.Value.Count then
        Result := -1
      else if left.Value.Count < right.Value.Count then
        Result := 1
      else if left.Key < right.Key then
        Result := -1
      else
        Result := 1;
    end);
  localSet := THashMultiSet<T>.Create(nil);
  for i := 0 to DynArrayHigh(items) do
    localSet.Add(items[i].Value.Item, items[i].Value.Count);
  Result := localSet as IReadOnlyMultiSet<T>;
end;

function TAbstractMultiSet<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  localSet: IMultiSet<T>;
  entry: TEntry;
begin
  if not Assigned(other) then RaiseHelper.ArgumentNil(ExceptionArgument.other);

  localSet := CreateMultiSet;
  localSet.AddRange(other);

  if fCount <> localSet.Count then
    Exit(False);
  for entry in localSet.Entries do
    if IMultiSet<T>(this)[entry.Item] <> entry.Count then
      Exit(False);
  Result := True;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>'}

constructor THashMultiSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  fHashTable.Comparer := comparer;
end;

procedure THashMultiSet<T>.AfterConstruction;
var
  elementType: PTypeInfo;
begin
  inherited AfterConstruction;

  elementType := GetElementType;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Initialize(@TComparerThunks<T>.Equals, @TComparerThunks<T>.GetHashCode, elementType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<T>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<T>.FindWithComparer;

  fItems := TItemCollection.Create(Self, @fHashTable, IEqualityComparer<T>(fHashTable.Comparer), elementType, 0);
  fEntries := TEntryCollection.Create(Self);
end;

procedure THashMultiSet<T>.BeforeDestruction;
begin
  Clear;
  fEntries.Free;
  fItems.Free;
  inherited BeforeDestruction;
end;

function THashMultiSet<T>.CreateMultiSet: IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(IEqualityComparer<T>(fHashTable.Comparer));
end;

function THashMultiSet<T>.Add(const item: T): Boolean;
begin
  Add(item, 1);
  Result := True;
end;

function THashMultiSet<T>.Add(const item: T; count: Integer): Integer;
var
  entry: PItem;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  entry := IHashTable<T>(@fHashTable).Find(item, OverwriteExisting or InsertNonExisting);
  entry.Item := item;
  if entry.HashCode < 0 then
  begin
    entry.HashCode := entry.HashCode and not RemovedFlag;
    Result := entry.Count;
    Inc(entry.Count, count);
  end
  else
  begin
    Result := 0;
    entry.Count := count;
  end;
  Inc(fCount, count);

  if Assigned(Notify) then
    for i := 1 to count do
      Notify(Self, item, caAdded);
end;

procedure THashMultiSet<T>.Clear;
begin
  if not Assigned(Notify) then
    THashTable(fHashTable).Clear
  else
    ClearWithNotify;
end;

procedure THashMultiSet<T>.ClearWithNotify;
var
  oldItemCount, i, n: Integer;
  oldItems: TArray<TItem>;
begin
  oldItemCount := fHashTable.ItemCount;
  oldItems := TItems(fHashTable.Items);

  THashTable(fHashTable).Clear;

  for i := 0 to oldItemCount - 1 do
    if oldItems[i].HashCode >= 0 then
      for n := 1 to oldItems[i].Count do
        Notify(Self, oldItems[i].Item, caRemoved);
end;

function THashMultiSet<T>.Contains(const item: T): Boolean;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item);
  Result := Assigned(entry);
end;

function THashMultiSet<T>.Extract(const item: T): T;
begin
  if Remove(item) then // TODO: possibly change if/when implementing ownership
    Result := item
  else
    Result := Default(T);
end;

function THashMultiSet<T>.GetEntries: IReadOnlyCollection<TEntry>;
begin
  Result := fEntries;
end;

function THashMultiSet<T>.GetItems: IReadOnlyCollection<T>;
begin
  Result := fItems;
end;

function THashMultiSet<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fHashTable.Version;
  end;
end;

function THashMultiSet<T>.GetItemCount(const item: T): Integer;
var
  entry: PItem;
begin
  entry := IHashTable<T>(@fHashTable).Find(item);
  if not Assigned(entry) then Exit(Integer(Pointer(entry)));
  Result := entry.Count;
end;

function THashMultiSet<T>.Remove(const item: T): Boolean;
var
  remaining: Integer;
begin
  remaining := Remove(item, 1);
  Result := remaining > 0;
end;

function THashMultiSet<T>.Remove(const item: T; count: Integer): Integer;
var
  entry: THashTableEntry;
  tableItem: PItem;
  remaining, i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  entry.HashCode := IEqualityComparer<T>(fHashTable.Comparer).GetHashCode(item);
  Result := Ord(THashTable(fHashTable).FindEntry(item, entry));
  if Result > 0 then
  begin
    tableItem := @TItems(fHashTable.Items)[entry.ItemIndex];
    remaining := tableItem.Count;
    if remaining <= count then
    begin
      THashTable(fHashTable).DeleteEntry(entry);
      count := remaining;
    end
    else
      Dec(tableItem.Count, count);
    Dec(fCount, count);

    if Assigned(Notify) then
      for i := 1 to count  do
        Notify(Self, item, caRemoved);
    Result := remaining;
  end;
end;

procedure THashMultiSet<T>.SetItemCount(const item: T; count: Integer);
var
  entry: PItem;
  overrideExisting: Boolean;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  if count = 0 then
  begin
    entry := IHashTable<T>(@fHashTable).Find(item, DeleteExisting);
    if Assigned(entry) then
    begin
      Dec(fCount, entry.Count);
      if Assigned(Notify) then
        for i := 1 to entry.Count do
          Notify(Self, item, caRemoved);
    end;
  end
  else
  begin
    entry := IHashTable<T>(@fHashTable).Find(item, InsertNonExisting);
    entry.Item := item;
//    entry.Count := 0;
    Inc(fCount, count - entry.Count);
    i := entry.Count;
    entry.Count := count;
    if Assigned(Notify) then
      while i > count do
      begin
        Notify(Self, item, caRemoved);
        Dec(i);
      end;
      while i < count do
      begin
        Notify(Self, item, caAdded);
        Inc(i);
      end;
  end;
end;

function THashMultiSet<T>.ToArray: TArray<T>;
var
  sourceIndex, targetIndex, count: Integer;
begin
  SetLength(Result, fCount);
  targetIndex := 0;
  for sourceIndex := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[sourceIndex].HashCode >= 0 then
      for count := 1 to TItems(fHashTable.Items)[sourceIndex].Count do
      begin
        Result[targetIndex] := TItems(fHashTable.Items)[sourceIndex].Item;
        Inc(targetIndex);
      end;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>.TEnumerator'}

function THashMultiSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fItem.Item;
end;

function THashMultiSet<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    if fRemainingCount > 0 then
    begin
      Dec(fRemainingCount);
      Exit(True);
    end;

    while fItemIndex < hashTable.ItemCount do
    begin
      item := @TItems(hashTable.Items)[fItemIndex];
      Inc(fItemIndex);
      if item.HashCode >= 0 then
      begin
        fItem := item;
        fRemainingCount := item.Count - 1;
        Exit(True);
      end;
    end;

    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>.TEntryCollection'}

constructor THashMultiSet<T>.TEntryCollection.Create(
  const source: THashMultiSet<T>);
begin
  fSource := source;
end;

function THashMultiSet<T>.TEntryCollection.Contains(const value: TEntry): Boolean;
var
  item: PItem;
begin
  item := IHashTable<T>(@fSource.fHashTable).Find(value.Item);
  if not Assigned(item) then Exit(Boolean(Pointer(item)));
  Result := value.Count = item.Count;
end;

function THashMultiSet<T>.TEntryCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function THashMultiSet<T>.TEntryCollection.GetCountFast: Integer;
begin
  Result := fSource.fCount;
end;

function THashMultiSet<T>.TEntryCollection.GetEnumerator: IEnumerator<TEntry>;
begin
  _AddRef;
  with PEntryEnumerator(TEnumeratorBlock.Create(@Result, @TEntryEnumerator.Enumerator_Vtable,
    TypeInfo(TEntryEnumerator), @TEntryEnumerator.GetCurrent, @TEntryEnumerator.MoveNext))^ do
  begin
    fSource := Self.fSource;
    fVersion := fSource.fHashTable.Version;
  end;
end;

function THashMultiSet<T>.TEntryCollection.ToArray: TArray<TEntry>;
var
  sourceIndex, targetIndex: Integer;
  item: ^TItem;
begin
  SetLength(Result, fSource.fHashTable.Count);
  targetIndex := 0;
  for sourceIndex := 0 to fSource.fHashTable.ItemCount - 1 do
  begin
    item := @TItems(fSource.fHashTable.Items)[sourceIndex];
    if item.HashCode >= 0 then
    begin
      Result[targetIndex].Item := item.Item;
      Result[targetIndex].Count := item.Count;
      Inc(targetIndex);
    end;
  end;
end;

function THashMultiSet<T>.TEntryCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function THashMultiSet<T>.TEntryCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>.TEntryEnumerator'}

function THashMultiSet<T>.TEntryEnumerator.GetCurrent: TEntry;
begin
  Result.Item := fItem.Item;
  Result.Count := fItem.Count;
end;

function THashMultiSet<T>.TEntryEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    while fItemIndex < hashTable.ItemCount do
    begin
      item := @TItems(hashTable.Items)[fItemIndex];
      Inc(fItemIndex);
      if item.HashCode >= 0 then
      begin
        fItem := item;
        Exit(True);
      end;
    end;

    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>'}

constructor TTreeMultiSet<T>.Create(const comparer: IComparer<T>);
begin
  fTree := TRedBlackTree<T, Integer>.Create(comparer);
  fItems := TItemCollection.Create(Self);
  fEntries := TEntryCollection.Create(Self);
end;

procedure TTreeMultiSet<T>.BeforeDestruction;
begin
  Clear;
  fEntries.Free;
  fItems.Free;
  fTree.Free;
  inherited BeforeDestruction;
end;

function TTreeMultiSet<T>.CreateMultiSet: IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(fTree.Comparer);
end;

function TTreeMultiSet<T>.Add(const item: T): Boolean;
begin
  Add(item, 1);
  Result := True;
end;

function TTreeMultiSet<T>.Add(const item: T; count: Integer): Integer;
var
  node: PNode;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := node.Value;
    node.Value := Result + count;
  end
  else
  begin
    Result := 0;
    fTree.Add(item, count);
  end;
  Inc(fCount, count);

  if Assigned(Notify) then
    for i := 1 to count do
      Notify(Self, item, caAdded);
end;

procedure TTreeMultiSet<T>.Clear;
var
  node: PNode;
  count: Integer;
begin
  if fCount > 0 then
  begin
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}

    if Assigned(Notify) then
    begin
      node := fTree.Root.LeftMost;
      while Assigned(node) do
      begin
        for count := node.Value downto 1 do
          Notify(Self, node.Key, caRemoved);
        node := node.Next;
      end;
    end;

    fTree.Clear;
    fCount := 0;
  end;
end;

function TTreeMultiSet<T>.Contains(const value: T): Boolean;
begin
  Result := fTree.Exists(value);
end;

function TTreeMultiSet<T>.Extract(const item: T): T;
begin
  if Remove(item) then // TODO: possibly change if/when implementing ownership
    Result := item
  else
    Result := Default(T);
end;

function TTreeMultiSet<T>.GetEntries: IReadOnlyCollection<TEntry>;
begin
  Result := fEntries;
end;

function TTreeMultiSet<T>.GetItems: IReadOnlyCollection<T>;
begin
  Result := fItems;
end;

function TTreeMultiSet<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fVersion := Self.fVersion;
  end;
end;

function TTreeMultiSet<T>.GetItemCount(const item: T): Integer;
begin
  fTree.Find(item, Result);
end;

function TTreeMultiSet<T>.Remove(const item: T): Boolean;
begin
  Result := Remove(item, 1) > 0;
end;

function TTreeMultiSet<T>.Remove(const item: T; count: Integer): Integer;
var
  node: PNode;
  i: Integer;
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  node := fTree.FindNode(item);
  if not Assigned(node) then Exit(Integer(Pointer(node)));

  Result := node.Value;
  if Result <= count then
  begin
    fTree.DeleteNode(node);
    count := Result;
  end
  else
    node.Value := Result - count;
  Dec(fCount, count);
  if Assigned(Notify) then
    for i := 1 to count do
      Notify(Self, item, caRemoved);
end;

procedure TTreeMultiSet<T>.SetItemCount(const item: T; count: Integer);
begin
  if count < 0 then RaiseHelper.ArgumentOutOfRange(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);

  {$Q-}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  if count = 0 then
    fTree.Delete(item)
  else
    fTree.AddOrSet(item, count);
end;

function TTreeMultiSet<T>.ToArray: TArray<T>;
var
  node: PNode;
  index, count: Integer;
begin
  SetLength(Result, fCount);
  index := 0;
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    for count := 1 to node.Value do
    begin
      Result[index] := node.Key;
      Inc(index);
    end;
    node := node.Next;
  end;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TEnumerator'}

function TTreeMultiSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fNode.Key;
end;

function TTreeMultiSet<T>.TEnumerator.MoveNext: Boolean;
var
  node: PNode;
begin
  if fVersion = fSource.fVersion then
  begin
    if fRemainingCount = 0 then
    begin
      if not Assigned(fNode) then
        node := fSource.fTree.Root.LeftMost
      else
        node := fNode.Next;

      Result := Assigned(node);
      if Result then
      begin
        fNode := node;
        fRemainingCount := node.Value - 1;
      end;
    end
    else
    begin
      Dec(fRemainingCount);
      Result := True;
    end;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TItemCollection'}

constructor TTreeMultiSet<T>.TItemCollection.Create(
  const source: TTreeMultiSet<T>);
begin
  fSource := source;
end;

function TTreeMultiSet<T>.TItemCollection.Contains(const value: T): Boolean;
begin
  Result := fSource.fTree.Exists(value);
end;

function TTreeMultiSet<T>.TItemCollection.GetCount: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TTreeMultiSet<T>.TItemCollection.GetCountFast: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TTreeMultiSet<T>.TItemCollection.GetEnumerator: IEnumerator<T>;
begin
  fSource._AddRef;
  with PItemEnumerator(TEnumeratorBlock.Create(@Result, @TItemEnumerator.Enumerator_Vtable,
    TypeInfo(TItemEnumerator), @TItemEnumerator.GetCurrent, @TItemEnumerator.MoveNext))^ do
  begin
    fSource := Self.fSource;
    fVersion := fSource.fVersion;
  end;
end;

function TTreeMultiSet<T>.TItemCollection.ToArray: TArray<T>;
var
  i: Integer;
  node: PNode;
begin
  SetLength(Result, fSource.fTree.Count);
  i := 0;
  node := fSource.fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := node.Key;
    node := node.Next;
    Inc(i);
  end;
end;

function TTreeMultiSet<T>.TItemCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TTreeMultiSet<T>.TItemCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TItemEnumerator'}

function TTreeMultiSet<T>.TItemEnumerator.GetCurrent: T;
begin
  Result := PNode(fNode).Key;
end;

function TTreeMultiSet<T>.TItemEnumerator.MoveNext: Boolean;
var
  tree: TBinaryTree;
  node: PBinaryTreeNode;
begin
  tree := fSource.fTree;
  if fVersion = fSource.fVersion then
  begin
    if (tree.Count > 0) and (fNode <> Pointer(1)) then
    begin
      if Assigned(fNode) then
        node := fNode.Next
      else
        node := tree.Root.LeftMost;
      if Assigned(node) then
      begin
        fNode := node;
        Exit(True);
      end;
    end;

    fNode := Pointer(1);
    Exit(False);
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TEntryCollection'}

constructor TTreeMultiSet<T>.TEntryCollection.Create(
  const source: TTreeMultiSet<T>);
begin
  fSource := source;
end;

function TTreeMultiSet<T>.TEntryCollection.Contains(
  const value: TEntry): Boolean;
var
  foundCount: Integer;
begin
  Result := fSource.fTree.Find(value.Item, foundCount)
    and (value.Count = foundCount);
end;

function TTreeMultiSet<T>.TEntryCollection.GetCount: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TTreeMultiSet<T>.TEntryCollection.GetCountFast: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TTreeMultiSet<T>.TEntryCollection.GetEnumerator: IEnumerator<TEntry>;
begin
  fSource._AddRef;
  with PEntryEnumerator(TEnumeratorBlock.Create(@Result, @TEntryEnumerator.Enumerator_Vtable,
    TypeInfo(TEntryEnumerator), @TEntryEnumerator.GetCurrent, @TEntryEnumerator.MoveNext))^ do
  begin
    fSource := Self.fSource;
    fVersion := fSource.fVersion;
  end;
end;

function TTreeMultiSet<T>.TEntryCollection.ToArray: TArray<TEntry>;
begin
  TArray<TPair<T,Integer>>(Result) := fSource.fTree.ToArray;
end;

function TTreeMultiSet<T>.TEntryCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TTreeMultiSet<T>.TEntryCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TEntryEnumerator'}

function TTreeMultiSet<T>.TEntryEnumerator.GetCurrent: TEntry;
begin
  Result.Item := PNode(fNode).Key;
  Result.Count := PNode(fNode).Value;
end;

function TTreeMultiSet<T>.TEntryEnumerator.MoveNext: Boolean;
var
  tree: TBinaryTree;
  node: PBinaryTreeNode;
begin
  tree := fSource.fTree;
  if fVersion = fSource.fVersion then
  begin
    if (tree.Count > 0) and (fNode <> Pointer(1)) then
    begin
      if Assigned(fNode) then
        node := fNode.Next
      else
        node := tree.Root.LeftMost;
      if Assigned(node) then
      begin
        fNode := node;
        Exit(True);
      end;
    end;

    fNode := Pointer(1);
    Result := False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


end.
