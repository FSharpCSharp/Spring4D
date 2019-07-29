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

unit Spring.Collections.MultiSets;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.HashTable,
  Spring.Collections.Trees;

type
  TAbstractMultiSet<T> = class abstract(TCollectionBase<T>)
  {$REGION 'Nested Types'}
    type
      TEntry = TMultiSetEntry<T>;
  {$ENDREGION}
  private
    fCount: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
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
  private
  {$REGION 'Nested Types'}
    type
      TEntry = TMultiSetEntry<T>;
      TItem = THashMultiSetItem<T>;
      TItems = TArray<TItem>;

      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: THashMultiSet<T>;
        fItemIndex: Integer;
        fRemainingCount: Integer;
        fVersion: Integer;
        fCurrent: T;
        function GetCurrent: T;
      public
        constructor Create(const source: THashMultiSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TKeyCollection = TInnerCollection<T>;

      TEntryCollection = class(TEnumerableBase<TEntry>,
        IEnumerable<TEntry>, IReadOnlyCollection<TEntry>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: THashMultiSet<T>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: THashMultiSet<T>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; override;
        function _Release: Integer; override;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TEntry>;
        function Contains(const value: TEntry): Boolean; overload;
        function ToArray: TArray<TEntry>;
      {$ENDREGION}
      end;

      TEntryEnumerator = class(TRefCountedObject, IEnumerator<TEntry>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: THashMultiSet<T>;
        fItemIndex: Integer;
        fVersion: Integer;
        fCurrent: TEntry;
        function GetCurrent: TEntry;
      public
        constructor Create(const source: THashMultiSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeyComparer: IEqualityComparer<T>;
    fKeys: TKeyCollection;
    fEntries: TEntryCollection;
  {$REGION 'Property Accessors'}
    function GetElements: IReadOnlyCollection<T>;
    function GetEntries: IReadOnlyCollection<TEntry>;
    function GetItem(const item: T): Integer;
    procedure SetItem(const item: T; count: Integer);
  {$ENDREGION}
    class function EqualsThunk(instance: Pointer; const left, right): Boolean; static;
    procedure ClearInternal;
  protected
    function CreateMultiSet: IMultiSet<T>; override;
  public
    constructor Create; override;
    constructor Create(const comparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;

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
  private
  {$REGION 'Nested Types'}
    type
      TEntry = TMultiSetEntry<T>;
      PNode = TNodes<T, Integer>.PRedBlackTreeNode;

      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TTreeMultiSet<T>;
        fNode: PNode;
        fRemainingCount: Integer;
        fVersion: Integer;
        function GetCurrent: T;
      public
        constructor Create(const source: TTreeMultiSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TKeyCollection = class(TEnumerableBase<T>,
        IEnumerable<T>, IReadOnlyCollection<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TTreeMultiSet<T>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TTreeMultiSet<T>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; override;
        function _Release: Integer; override;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<T>;
        function Contains(const value: T): Boolean; overload;
        function ToArray: TArray<T>;
      {$ENDREGION}
      end;

      TKeyEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TTreeMultiSet<T>;
        fCurrentNode: PNode;
        fFinished: Boolean;
        fVersion: Integer;
        function GetCurrent: T;
      public
        constructor Create(const source: TTreeMultiSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TEntryCollection = class(TEnumerableBase<TEntry>,
        IEnumerable<TEntry>, IReadOnlyCollection<TEntry>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TTreeMultiSet<T>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TTreeMultiSet<T>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; override;
        function _Release: Integer; override;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TEntry>;
        function Contains(const value: TEntry): Boolean; overload;
        function ToArray: TArray<TEntry>;
      {$ENDREGION}
      end;

      TEntryEnumerator = class(TRefCountedObject, IEnumerator<TEntry>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TTreeMultiSet<T>;
        fCurrentNode: PNode;
        fFinished: Boolean;
        fVersion: Integer;
        function GetCurrent: TEntry;
      public
        constructor Create(const source: TTreeMultiSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fTree: TRedBlackTree<T, Integer>;
    fVersion: Integer;
    fKeys: TKeyCollection;
    fEntries: TEntryCollection;
  {$REGION 'Property Accessors'}
    function GetElements: IReadOnlyCollection<T>;
    function GetEntries: IReadOnlyCollection<TEntry>;
    function GetItem(const item: T): Integer;
    procedure SetItem(const item: T; count: Integer);
  {$ENDREGION}
    function DoMoveNext(var currentNode: PNode; var finished: Boolean;
      iteratorVersion: Integer): Boolean;
  protected
    function CreateMultiSet: IMultiSet<T>; override;
  public
    constructor Create; override;
    constructor Create(const comparer: IComparer<T>); overload;
    destructor Destroy; override;

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
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TAbstractMultiSet<T>'}

function TAbstractMultiSet<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TAbstractMultiSet<T>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TAbstractMultiSet<T>.OrderedByCount: IReadOnlyMultiSet<T>;
var
  entries: TArray<TEntry>;
  items: TArray<TPair<Integer,TEntry>>;
  i: Integer;
  localSet: IMultiSet<T>;
begin
  entries := IMultiSet<T>(this).Entries.ToArray;
  SetLength(items, Length(entries));
  for i := 0 to High(entries) do
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
  localSet := THashMultiSet<T>.Create;
  for i := 0 to High(items) do
    localSet.Add(items[i].Value.Key, items[i].Value.Count);
  Result := localSet as IReadOnlyMultiSet<T>;
end;

function TAbstractMultiSet<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  localSet: IMultiSet<T>;
  entry: TEntry;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  localSet := CreateMultiSet;
  localSet.AddRange(other);

  if fCount <> localSet.Count then
    Exit(False);
  for entry in localSet.Entries do
    if IMultiSet<T>(this)[entry.Key] <> entry.Count then
      Exit(False);
  Result := True;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>'}

constructor THashMultiSet<T>.Create;
begin
  Create(nil);
end;

constructor THashMultiSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  inherited Create;
  if Assigned(comparer) then
    fKeyComparer := comparer
  else
    fKeyComparer := IEqualityComparer<T>(_LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T)));

  fHashTable.Initialize(TypeInfo(TItems), @EqualsThunk, fKeyComparer);
  fKeys := TKeyCollection.Create(Self, @fHashTable, GetElementType, fKeyComparer, 0);
  fEntries := TEntryCollection.Create(Self);
end;

destructor THashMultiSet<T>.Destroy;
begin
  Clear;
  fEntries.Free;
  fKeys.Free;
  inherited Destroy;
end;

function THashMultiSet<T>.CreateMultiSet: IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(fKeyComparer);
end;

function THashMultiSet<T>.Add(const item: T): Boolean;
begin
  Add(item, 1);
  Result := True;
end;

function THashMultiSet<T>.Add(const item: T; count: Integer): Integer;
var
  entry: ^TItem;
  isExisting: Boolean;
  i: Integer;
begin
  Guard.CheckRange(count >= 0, 'count');

  entry := fHashTable.AddOrSet(item, fKeyComparer.GetHashCode(item), isExisting);

  entry.Item := item;
  if isExisting then
  begin
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
    fHashTable.Clear
  else
    ClearInternal;
end;

procedure THashMultiSet<T>.ClearInternal;
var
  oldItemIndex, oldItemCount, i: Integer;
  oldItems: TArray<TItem>;
begin
  oldItemCount := fHashTable.ItemCount;
  oldItems := TItems(fHashTable.Items);

  fHashTable.Clear;

  for oldItemIndex := 0 to oldItemCount - 1 do
    if oldItems[oldItemIndex].HashCode >= 0 then
      for i := 1 to oldItems[oldItemIndex].Count do
        Changed(oldItems[oldItemIndex].Item, caRemoved);
end;

class function THashMultiSet<T>.EqualsThunk(instance: Pointer; const left, right): Boolean;
begin
  Result := TEqualsMethod<T>(instance^)(T(left), T(right));
end;

function THashMultiSet<T>.Contains(const item: T): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(item);
  Result := fHashTable.Find(item, entry);
end;

function THashMultiSet<T>.Extract(const item: T): T;
begin
  if Remove(item) then // TODO: possibly change if/when implementing ownership
    Result := item
  else
    Result := Default(T);
end;

function THashMultiSet<T>.GetElements: IReadOnlyCollection<T>;
begin
  Result := fKeys;
end;

function THashMultiSet<T>.GetEntries: IReadOnlyCollection<TEntry>;
begin
  Result := fEntries;
end;

function THashMultiSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function THashMultiSet<T>.GetItem(const item: T): Integer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(item);
  if fHashTable.Find(item, entry) then
    Result := TItems(fHashTable.Items)[entry.ItemIndex].Count
  else
    Result := 0;
end;

function THashMultiSet<T>.Remove(const item: T): Boolean;
begin
  Result := Remove(item, 1) > 0;
end;

function THashMultiSet<T>.Remove(const item: T; count: Integer): Integer;
var
  entry: THashTableEntry;
  tableItem: ^TItem;
  i: Integer;
begin
  Guard.CheckRange(count >= 0, 'count');

  entry.HashCode := fKeyComparer.GetHashCode(item);
  if fHashTable.Find(item, entry) then
  begin
    tableItem := @TItems(fHashTable.Items)[entry.ItemIndex];
    Result := tableItem.Count;
    if Result <= count then
    begin
      fHashTable.Delete(entry);
      count := Result;
    end
    else
      Dec(tableItem.Count, count);
    Dec(fCount, count);

    if Assigned(Notify) then
      for i := 1 to count  do
        Notify(Self, item, caRemoved);
  end
  else
    Result := 0;
end;

procedure THashMultiSet<T>.SetItem(const item: T; count: Integer);
var
  entry: ^TItem;
  isExisting: Boolean;
  i: Integer;
begin
  Guard.CheckRange(count >= 0, 'count');

  if count = 0 then
  begin
    entry := fHashTable.Delete(item, fKeyComparer.GetHashCode(item));
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
    entry := fHashTable.AddOrSet(item, fKeyComparer.GetHashCode(item), isExisting);
    if not isExisting then
    begin
      entry.Item := item;
      entry.Count := 0;
    end;
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

constructor THashMultiSet<T>.TEnumerator.Create(const source: THashMultiSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fItemIndex := -1;
  fRemainingCount := 0;
  fVersion := fSource.fHashTable.Version;
end;

destructor THashMultiSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited Destroy;
end;

function THashMultiSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function THashMultiSet<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: ^THashTable;
  entry: ^TItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion <> hashTable.Version then
    raise Error.EnumFailedVersion;

  if fRemainingCount = 0 then
    while fItemIndex < hashTable.ItemCount - 1 do
    begin
      Inc(fItemIndex);
      entry := @TItems(hashTable.Items)[fItemIndex];
      if entry.HashCode >= 0 then
      begin
        fCurrent := entry.Item;
        fRemainingCount := entry.Count - 1;
        Exit(True);
      end;
    end
  else
  begin
    Dec(fRemainingCount);
    Exit(True);
  end;

  fCurrent := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'THashMultiSet<T>.TEntryCollection'}

constructor THashMultiSet<T>.TEntryCollection.Create(
  const source: THashMultiSet<T>);
begin
  inherited Create;
  fSource := source;
end;

function THashMultiSet<T>.TEntryCollection.Contains(
  const value: TEntry): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fSource.fKeyComparer.GetHashCode(value.Key);
  Result := fSource.fHashTable.Find(value.Key, entry)
    and (value.Count = TItems(fSource.fHashTable.Items)[entry.ItemIndex].Count);
end;

function THashMultiSet<T>.TEntryCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function THashMultiSet<T>.TEntryCollection.GetEnumerator: IEnumerator<TEntry>;
begin
  Result := TEntryEnumerator.Create(fSource);
end;

function THashMultiSet<T>.TEntryCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fCount = 0;
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
      Result[targetIndex].Key := item.Item;
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

constructor THashMultiSet<T>.TEntryEnumerator.Create(
  const source: THashMultiSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fHashTable.Version;
end;

destructor THashMultiSet<T>.TEntryEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function THashMultiSet<T>.TEntryEnumerator.GetCurrent: TEntry;
begin
  Result := fCurrent;
end;

function THashMultiSet<T>.TEntryEnumerator.MoveNext: Boolean;
var
  hashTable: ^THashTable;
  entry: ^TItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion <> hashTable.Version then
    raise Error.EnumFailedVersion;

  while fItemIndex < hashTable.ItemCount - 1 do
  begin
    Inc(fItemIndex);
    entry := @TItems(hashTable.Items)[fItemIndex];
    if entry.HashCode >= 0 then
    begin
      fCurrent.Key := entry.Item;
      fCurrent.Count := entry.Count;
      Exit(True);
    end;
  end;

  fCurrent := Default(TEntry);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>'}

constructor TTreeMultiSet<T>.Create;
begin
  Create(nil);
end;

constructor TTreeMultiSet<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fTree := TRedBlackTree<T, Integer>.Create(comparer);
  fKeys := TKeyCollection.Create(Self);
  fEntries := TEntryCollection.Create(Self);
end;

destructor TTreeMultiSet<T>.Destroy;
begin
  Clear;
  fEntries.Free;
  fKeys.Free;
  fTree.Free;
  inherited;
end;

function TTreeMultiSet<T>.CreateMultiSet: IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(fTree.Comparer);
end;

function TTreeMultiSet<T>.DoMoveNext(var currentNode: PNode;
  var finished: Boolean; iteratorVersion: Integer): Boolean;
begin
  if iteratorVersion <> fVersion then
    raise Error.EnumFailedVersion;

  if (fTree.Count = 0) or finished then
    Exit(False);

  if not Assigned(currentNode) then
    currentNode := fTree.Root.LeftMost
  else
    currentNode := currentNode.Next;
  Result := Assigned(currentNode);
  finished := not Result;
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
  Guard.CheckTrue(count >= 0, 'count');

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
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
    {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}

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

function TTreeMultiSet<T>.GetElements: IReadOnlyCollection<T>;
begin
  Result := fKeys;
end;

function TTreeMultiSet<T>.GetEntries: IReadOnlyCollection<TEntry>;
begin
  Result := fEntries;
end;

function TTreeMultiSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TTreeMultiSet<T>.GetItem(const item: T): Integer;
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
  Guard.CheckTrue(count >= 0, 'count');

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
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
  end
  else
    Result := 0;
end;

procedure TTreeMultiSet<T>.SetItem(const item: T; count: Integer);
begin
  if count < 0 then
    raise Error.ArgumentOutOfRange('count');

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
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

constructor TTreeMultiSet<T>.TEnumerator.Create(
  const source: TTreeMultiSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fRemainingCount := 0;
  fVersion := fSource.fVersion;
end;

destructor TTreeMultiSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TTreeMultiSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fNode.Key;
end;

function TTreeMultiSet<T>.TEnumerator.MoveNext: Boolean;
var
  node: PNode;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

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
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TKeyCollection'}

constructor TTreeMultiSet<T>.TKeyCollection.Create(
  const source: TTreeMultiSet<T>);
begin
  inherited Create;
  fSource := source;
end;

function TTreeMultiSet<T>.TKeyCollection.Contains(const value: T): Boolean;
begin
  Result := fSource.fTree.Exists(value);
end;

function TTreeMultiSet<T>.TKeyCollection.GetCount: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TTreeMultiSet<T>.TKeyCollection.GetEnumerator: IEnumerator<T>;
begin
  Result := TKeyEnumerator.Create(fSource);
end;

function TTreeMultiSet<T>.TKeyCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fTree.Count = 0;
end;

function TTreeMultiSet<T>.TKeyCollection.ToArray: TArray<T>;
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

function TTreeMultiSet<T>.TKeyCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TTreeMultiSet<T>.TKeyCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TKeyEnumerator'}

constructor TTreeMultiSet<T>.TKeyEnumerator.Create(
  const source: TTreeMultiSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TTreeMultiSet<T>.TKeyEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TTreeMultiSet<T>.TKeyEnumerator.GetCurrent: T;
begin
  Result := fCurrentNode.Key;
end;

function TTreeMultiSet<T>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fCurrentNode, fFinished, fVersion);
end;

{$ENDREGION}


{$REGION 'TTreeMultiSet<T>.TEntryCollection'}

constructor TTreeMultiSet<T>.TEntryCollection.Create(
  const source: TTreeMultiSet<T>);
begin
  inherited Create;
  fSource := source;
end;

function TTreeMultiSet<T>.TEntryCollection.Contains(
  const value: TEntry): Boolean;
var
  foundCount: Integer;
begin
  Result := fSource.fTree.Find(value.Key, foundCount)
    and (value.Count = foundCount);
end;

function TTreeMultiSet<T>.TEntryCollection.GetCount: Integer;
begin
  Result := fSource.fTree.Count;
end;

function TTreeMultiSet<T>.TEntryCollection.GetEnumerator: IEnumerator<TEntry>;
begin
  Result := TEntryEnumerator.Create(fSource);
end;

function TTreeMultiSet<T>.TEntryCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fTree.Count = 0;
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

constructor TTreeMultiSet<T>.TEntryEnumerator.Create(
  const source: TTreeMultiSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TTreeMultiSet<T>.TEntryEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TTreeMultiSet<T>.TEntryEnumerator.GetCurrent: TEntry;
begin
  Result.Key := fCurrentNode.Key;
  Result.Count := fCurrentNode.Value;
end;

function TTreeMultiSet<T>.TEntryEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fCurrentNode, fFinished, fVersion);
end;

{$ENDREGION}


end.
