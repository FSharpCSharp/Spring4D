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

unit Spring.Collections.Sets;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees;

type
  /// <summary>
  ///   The abstract base class for all set implementations.
  /// </summary>
  TSetBase<T> = class abstract(TCollectionBase<T>)
  protected
    class function CreateSet: ISet<T>; virtual; abstract;
  public
    procedure ExceptWith(const other: IEnumerable<T>);
    procedure IntersectWith(const other: IEnumerable<T>);
    procedure UnionWith(const other: IEnumerable<T>);
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;
    function SetEquals(const other: IEnumerable<T>): Boolean;
    function Overlaps(const other: IEnumerable<T>): Boolean;
  end;

  THashSetItem<T> = record
  private
    // use the MSB of the HashCode to note removed items
    const RemovedFlag = Integer($80000000);
  public
    HashCode: Integer;
    Item: T;
    function Removed: Boolean; inline;
  end;

  /// <summary>
  ///   Represents a set of values.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the hash set.
  /// </typeparam>
  THashSet<T> = class(TSetBase<T>, INotifyCollectionChanged<T>, IEnumerable<T>,
    ICollection<T>, IReadOnlyCollection<T>, ISet<T>)
  private
  {$REGION 'Nested Types'}
    type
      TItem = THashSetItem<T>;

      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: THashSet<T>;
        fItemIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T;
      public
        constructor Create(const source: THashSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    const
      MinCapacity = 8;
      BucketSentinelFlag = Integer($80000000); // note: the same as RemovedFlag
      EmptyBucket = -1; // must be negative, note choice of BucketSentinelFlag
      UsedBucket  = -2; // likewise
  private
    fBuckets: TArray<Integer>;
    fItems: TArray<TItem>;
    fCapacity: Integer;
    fCount: Integer;
    fItemCount: Integer;
    fBucketIndexMask: Integer;
    fBucketHashCodeMask: Integer;
    fEqualityComparer: IEqualityComparer<T>;
    fVersion: Integer;
    procedure Rehash(newCapacity: Integer);
    procedure EnsureCompact;
    function Grow: Boolean;
    function Find(const item: T; hashCode: Integer;
      out bucketIndex, itemIndex: Integer): Boolean;
    function Hash(const item: T): Integer; inline;
    procedure DoAdd(hashCode, bucketIndex, itemIndex: Integer; const item: T);
    procedure DoRemove(bucketIndex, itemIndex: Integer;
      action: TCollectionChangedAction);
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer; 
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): T;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    class function CreateSet: ISet<T>; override;
    function TryGetElementAt(out item: T; index: Integer): Boolean; override;
  public
    constructor Create; overload; override;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IEqualityComparer<T>); overload;
    constructor Create(capacity: Integer;
      const comparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;

    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure TrimExcess;
  {$ENDREGION}
  end;

  TSortedSet<T> = class(TSetBase<T>, INotifyCollectionChanged<T>, IEnumerable<T>,
    ICollection<T>, IReadOnlyCollection<T>, ISet<T>)
  private
  {$REGION 'Nested Types'}
    type
      PNode = TNodes<T>.PRedBlackTreeNode;
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TSortedSet<T>;
        fVersion: Integer;
        fCurrent: PNode;
        fFinished: Boolean;
      protected
        function GetCurrent: T;
      public
        constructor Create(const source: TSortedSet<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fTree: TRedBlackTree<T>;
    fVersion: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer; 
    procedure SetCapacity(value: Integer);
  {$ENDREGION}
    class function CreateSet: ISet<T>; override;
  public
    constructor Create; overload; override;
    constructor Create(const comparer: IComparer<T>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const item: T): Boolean;
    function ToArray: TArray<T>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<T>'}
    function Add(const item: T): Boolean;
    function Remove(const item: T): Boolean;
    function Extract(const item: T): T;
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements ISet<T>'}
    procedure TrimExcess;
  {$ENDREGION}
  end;

implementation

uses
  Math,
  Spring,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TSetBase<T>'}

procedure TSetBase<T>.ExceptWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    ICollection<T>(this).Remove(item);
end;

procedure TSetBase<T>.IntersectWith(const other: IEnumerable<T>);
var
  item: T;
  list: IList<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  list := TCollections.CreateList<T>;
  for item in this do
    if not other.Contains(item) then
      list.Add(item);

  for item in list do
    ICollection<T>(this).Remove(item);
end;

function TSetBase<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in this do
    if not other.Contains(item) then
      Exit(False);

  Result := True;
end;

function TSetBase<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if not Contains(item) then
      Exit(False);

  Result := True;
end;

function TSetBase<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if Contains(item) then
      Exit(True);

  Result := False;
end;

function TSetBase<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  localSet := CreateSet;

  for item in other do
  begin
    localSet.Add(item);
    if not Contains(item) then
      Exit(False);
  end;

  for item in this do
    if not localSet.Contains(item) then
      Exit(False);

  Result := True;
end;

procedure TSetBase<T>.UnionWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    ICollection<T>(this).Add(item);
end;

{$ENDREGION}


{$REGION 'THashSetItem<T>'}

function THashSetItem<T>.Removed: Boolean;
begin
  Result := HashCode and RemovedFlag <> 0;
end;

{$ENDREGION}


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
begin
  Create(0, nil);
end;

constructor THashSet<T>.Create(capacity: Integer);
begin
  Create(capacity, nil);
end;

constructor THashSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  Create(0, comparer);
end;

constructor THashSet<T>.Create(capacity: Integer; const comparer: IEqualityComparer<T>);
begin
  inherited Create;
  if Assigned(comparer) then
    fEqualityComparer := comparer
  else
    fEqualityComparer := TEqualityComparer<T>.Default;
  SetCapacity(capacity);
end;

destructor THashSet<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class function THashSet<T>.CreateSet: ISet<T>;
begin
  Result := THashSet<T>.Create;
end;

procedure THashSet<T>.SetCapacity(value: Integer);
var
  newCapacity: Integer;
begin
  Guard.CheckRange(value >= fCount, 'capacity');

  if value = 0 then
    newCapacity := 0
  else
    newCapacity := Math.Max(MinCapacity, value);
  if newCapacity <> fCapacity then
    Rehash(newCapacity);
end;

procedure THashSet<T>.TrimExcess;
begin
  SetCapacity(fCount);
end;

function THashSet<T>.TryGetElementAt(out item: T; index: Integer): Boolean;
begin
  Result := InRange(index, 0, fCount - 1);
  if Result then
  begin
    EnsureCompact;
    item := fItems[index].Item;
  end;
end;

procedure THashSet<T>.Rehash(newCapacity: Integer);
var
  bucketIndex, itemIndex: Integer;
  sourceItemIndex, targetItemIndex: Integer;
begin
  if newCapacity > 0 then
    newCapacity := NextPowerOf2(newCapacity - 1);

  fCapacity := newCapacity;
  if fCapacity = 0 then
  begin
    Assert(fCount = 0);
    Assert(fItemCount = 0);
    Assert(not Assigned(fBuckets));
    Assert(not Assigned(fItems));
    Exit;
  end;

  IncUnchecked(fVersion);

  // compact the items array, if necessary
  if fItemCount > fCount then
  begin
    targetItemIndex := 0;
    for sourceItemIndex := 0 to fItemCount - 1 do
      if not fItems[sourceItemIndex].Removed then
      begin
        if targetItemIndex < sourceItemIndex then
          TArrayManager<TItem>.Move(fItems, sourceItemIndex, targetItemIndex, 1);
        Inc(targetItemIndex);
      end;
    TArrayManager<TItem>.Finalize(fItems, targetItemIndex, fItemCount - fCount);
  end;

  // resize the items array, safe now that we have compacted it
  SetLength(fItems, (fCapacity * 3) div 4); // max load factor of 0.75
  Assert(Length(fItems) >= fCount);

  // repopulate the bucket array
  Assert(IsPowerOf2(fCapacity));
  fBucketIndexMask := fCapacity - 1;
  fBucketHashCodeMask := not fBucketIndexMask and not BucketSentinelFlag;
  SetLength(fBuckets, fCapacity);
  for bucketIndex := 0 to fCapacity - 1 do
    fBuckets[bucketIndex] := EmptyBucket;
  fItemCount := 0;
  while fItemCount < fCount do
  begin
    Find(fItems[fItemCount].Item, fItems[fItemCount].HashCode, bucketIndex, itemIndex);
    Assert(itemIndex = fItemCount);
    fBuckets[bucketIndex] := itemIndex or (fItems[itemIndex].HashCode and fBucketHashCodeMask);
    Inc(fItemCount);
  end;
end;

function THashSet<T>.Grow: Boolean;
var
  newCapacity: Integer;
begin
  Result := fItemCount >= Length(fItems);
  if not Result then
    Exit;

  if fCapacity = 0 then
    newCapacity := MinCapacity
  else if 2 * fCount >= fCapacity then
    // only grow if load factor is greater than 0.5
    newCapacity := fCapacity * 2
  else
    newCapacity := fCapacity;
  Rehash(newCapacity);
end;

function THashSet<T>.Find(const item: T; hashCode: Integer;
  out bucketIndex, itemIndex: Integer): Boolean;
var
  bucketValue: Integer;
begin
  if fCapacity = 0 then
  begin
    bucketIndex := EmptyBucket;
    itemIndex := -1;
    Exit(False);
  end;

  bucketIndex := hashCode and fBucketIndexMask;
  while True do
  begin
    bucketValue := fBuckets[bucketIndex];

    if bucketValue = EmptyBucket then
    begin
      itemIndex := fItemCount;
      Exit(False);
    end;

    if (bucketValue <> UsedBucket)
      and (bucketValue and fBucketHashCodeMask = hashCode and fBucketHashCodeMask) then
    begin
      itemIndex := bucketValue and fBucketIndexMask;
      if fEqualityComparer.Equals(fItems[itemIndex].Item, item) then
        Exit(True);
    end;

    bucketIndex := (bucketIndex + 1) and fBucketIndexMask;
  end;
end;

function THashSet<T>.Hash(const item: T): Integer;
begin
  Result := fEqualityComparer.GetHashCode(item) and not TItem.RemovedFlag;
end;

procedure THashSet<T>.DoAdd(hashCode, bucketIndex, itemIndex: Integer; const item: T);
begin
  IncUnchecked(fVersion);
  fBuckets[bucketIndex] := itemIndex or (hashCode and fBucketHashCodeMask);
  fItems[itemIndex].HashCode := hashCode;
  fItems[itemIndex].Item := item;
  Inc(fCount);
  Inc(fItemCount);

  Changed(item, caAdded);
end;

procedure THashSet<T>.DoRemove(bucketIndex, itemIndex: Integer;
  action: TCollectionChangedAction);
var
  item: T;
begin
  item := fItems[itemIndex].Item;

  IncUnchecked(fVersion);
  fBuckets[bucketIndex] := UsedBucket;
  fItems[itemIndex].Item := Default(T);
  fItems[itemIndex].HashCode := fItems[itemIndex].HashCode or TItem.RemovedFlag;
  Dec(fCount);

  Changed(item, action);
end;

function THashSet<T>.Add(const item: T): Boolean;
var
  bucketIndex, itemIndex, hashCode: Integer;
begin
  hashCode := Hash(item);
  Result := not Find(item, hashCode, bucketIndex, itemIndex);
  if Result then
  begin
    if Grow then
      // rehash invalidates the indices
      Find(item, hashCode, bucketIndex, itemIndex);
    DoAdd(hashCode, bucketIndex, itemIndex, item);
  end;
end;

procedure THashSet<T>.Clear;
var
  oldItemIndex, oldItemCount: Integer;
  oldItems: TArray<TItem>;
begin
  oldItemCount := fItemCount;
  oldItems := fItems;

  IncUnchecked(fVersion);
  fCount := 0;
  fItemCount := 0;
  fBuckets := nil;
  fItems := nil;
  SetCapacity(0);

  for oldItemIndex := 0 to oldItemCount - 1 do
    if not oldItems[oldItemIndex].Removed then
      Changed(oldItems[oldItemIndex].Item, caRemoved);
end;

function THashSet<T>.Contains(const item: T): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := Find(item, Hash(item), bucketIndex, itemIndex);
end;

procedure THashSet<T>.EnsureCompact;
begin
  if fCount <> fItemCount then
    Rehash(fCapacity);
end;

function THashSet<T>.Extract(const item: T): T;
var
  bucketIndex, itemIndex: Integer;
begin
  if Find(item, Hash(item), bucketIndex, itemIndex) then
  begin
    Result := fItems[itemIndex].Item;
    DoRemove(bucketIndex, itemIndex, caExtracted)
  end
  else
    Result := Default(T);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function THashSet<T>.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function THashSet<T>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function THashSet<T>.GetItem(index: Integer): T;
begin
  if fCount <> fItemCount then
    Rehash(fCapacity);
  Result := fItems[index].Item;
end;

function THashSet<T>.Remove(const item: T): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := Find(item, Hash(item), bucketIndex, itemIndex);
  if Result then
    DoRemove(bucketIndex, itemIndex, caRemoved);
end;

function THashSet<T>.ToArray: TArray<T>;
var
  sourceIndex, targetIndex: Integer;
begin
  SetLength(Result, fCount);
  targetIndex := 0;
  for sourceIndex := 0 to fItemCount - 1 do
    if not fItems[sourceIndex].Removed then
    begin
      Result[targetIndex] := fItems[sourceIndex].Item;
      Inc(targetIndex);
    end;
end;

{$ENDREGION}


{$REGION 'THashSet<T>.TEnumerator'}

constructor THashSet<T>.TEnumerator.Create(const source: THashSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fItemIndex := -1;
  fVersion := fSource.fVersion;
end;

destructor THashSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function THashSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function THashSet<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  while fItemIndex < fSource.fItemCount - 1 do
  begin
    Inc(fItemIndex);
    if not fSource.fItems[fItemIndex].Removed then
    begin
      fCurrent := fSource.fItems[fItemIndex].Item;
      Exit(True);
    end;
  end;
  fCurrent := Default(T);
  Result := False;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>'}

constructor TSortedSet<T>.Create;
var
  // use variable to pass nil because of codegen bug in XE2 and XE3 in x64
  comparer: IComparer<T>;
begin
  Create(comparer);
end;

constructor TSortedSet<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fTree := TRedBlackTree<T>.Create(comparer);
end;

class function TSortedSet<T>.CreateSet: ISet<T>;
begin
  Result := TSortedSet<T>.Create;
end;

destructor TSortedSet<T>.Destroy;
begin
  Clear;
  fTree.Free;
  inherited Destroy;
end;

function TSortedSet<T>.Add(const item: T): Boolean;
begin
  IncUnchecked(fVersion);
  Result := fTree.Add(item);
  if Result then
    Changed(item, caAdded);
end;

procedure TSortedSet<T>.Clear;
var
  node: PNode;
begin
  if fTree.Count = 0 then
    Exit;

  IncUnchecked(fVersion);
  if fOnChanged.CanInvoke then // optimization: if no notification needs to be send the entire tree traversal won't be done
    for node in fTree.Root^ do
      Changed(PNode(node).Key, caRemoved);

  fTree.Clear;
end;

function TSortedSet<T>.Contains(const item: T): Boolean;
begin
  Result := Assigned(fTree.FindNode(item));
end;

function TSortedSet<T>.Extract(const item: T): T;
var
  node: PNode;
begin
  node := fTree.FindNode(item);
  if Assigned(node) then
  begin
    Result := node.Key;
    IncUnchecked(fVersion);
    fTree.DeleteNode(node);
    Changed(Result, caExtracted);
  end
  else
    Result := Default(T);
end;

function TSortedSet<T>.GetCapacity: Integer;
begin
  Result := fTree.Capacity;
end;

function TSortedSet<T>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSortedSet<T>.Remove(const item: T): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(item);
  Result := Assigned(node);
  if Result then
  begin
    IncUnchecked(fVersion);
    fTree.DeleteNode(node);
    Changed(item, caRemoved);
  end;
end;

function TSortedSet<T>.ToArray: TArray<T>;
var
  i: Integer;
  node: PNode;
begin
  SetLength(Result, fTree.Count);
  i := 0;
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    Result[i] := node.Key;
    node := node.Next;
    Inc(i);
  end;
end;

{$ENDREGION}


{$REGION 'TSortedSet<T>.TEnumerator'}

constructor TSortedSet<T>.TEnumerator.Create(const source: TSortedSet<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TSortedSet<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TSortedSet<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent.Key;
end;

function TSortedSet<T>.TEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise Error.EnumFailedVersion;

  if fFinished then
    Exit(False);
  if not Assigned(fCurrent) then
    fCurrent := fSource.fTree.Root.LeftMost
  else
    fCurrent := fCurrent.Next;
  Result := Assigned(fCurrent);
  if not Result then
    fFinished := True;
end;

procedure TSortedSet<T>.SetCapacity(value: Integer);
begin
  fTree.Capacity := value;
end;

procedure TSortedSet<T>.TrimExcess;
begin
  fTree.TrimExcess;
end;

{$ENDREGION}


end.
