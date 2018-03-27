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

unit Spring.Collections.Dictionaries;

interface

uses
  Classes,
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees;

type
  TDictionaryItem<TKey, TValue> = record
  private
    // use the MSB of the HashCode to note removed items
    const RemovedFlag = Integer($80000000);
  public
    HashCode: Integer;
    Key: TKey;
    Value: TValue;
    function Removed: Boolean; inline;
  end;

  TDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IDictionary<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>,
    IOrderedDictionary<TKey, TValue>)
  protected
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = Generics.Collections.TPair<TKey, TValue>;
      TKeyCollectionBase = TContainedReadOnlyCollection<TKey>;
      TValueCollectionBase = TContainedReadOnlyCollection<TValue>;
      TItem = TDictionaryItem<TKey, TValue>;

      TEnumerator = class(TEnumeratorBase<TKeyValuePair>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TDictionary<TKey, TValue>;
        fItemIndex: Integer;
        fVersion: Integer;
      protected
        function GetCurrent: TKeyValuePair; override;
      public
        constructor Create(const source: TDictionary<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TKeyEnumerator = class(TEnumeratorBase<TKey>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TDictionary<TKey, TValue>;
        fItemIndex: Integer;
        fVersion: Integer;
      protected
        function GetCurrent: TKey; override;
      public
        constructor Create(const source: TDictionary<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TKeyCollection = class(TKeyCollectionBase)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fDictionary: TDictionary<TKey, TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const dictionary: TDictionary<TKey, TValue>);

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>; override;
        function Contains(const value: TKey;
          const comparer: IEqualityComparer<TKey>): Boolean; override;
        function ToArray: TArray<TKey>; override;
      {$ENDREGION}
      end;

      TValueEnumerator = class(TEnumeratorBase<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TDictionary<TKey, TValue>;
        fItemIndex: Integer;
        fVersion: Integer;
      protected
        function GetCurrent: TValue; override;
      public
        constructor Create(const source: TDictionary<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TValueCollection = class(TValueCollectionBase)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fDictionary: TDictionary<TKey, TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const dictionary: TDictionary<TKey, TValue>);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;

      TOrderedEnumerable = class(TIterator<TKeyValuePair>)
      private
        fSource: TDictionary<TKey, TValue>;
        fSortedItemIndices: TArray<Integer>;
        fIndex: Integer;
        fVersion: Integer;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
        procedure Dispose; override;
        procedure Start; override;
        function TryMoveNext(var current: TKeyValuePair): Boolean; override;
      public
        constructor Create(const source: TDictionary<TKey, TValue>);
        destructor Destroy; override;
        function Clone: TIterator<TKeyValuePair>; override;
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
    fEqualityComparer: IEqualityComparer<TKey>;
    fVersion: Integer;
    fKeys: TKeyCollectionBase;
    fValues: TValueCollectionBase;
    procedure SetCapacity(value: Integer);
    procedure Rehash(newCapacity: Integer);
    function Grow: Boolean;
    function Find(const key: TKey; hashCode: Integer;
      out bucketIndex, itemIndex: Integer): Boolean;
    function Hash(const key: TKey): Integer; inline;
    procedure DoAdd(hashCode, bucketIndex, itemIndex: Integer;
      const key: TKey; const value: TValue);
    procedure DoSetValue(itemIndex: Integer; const value: TValue);
    function DoRemove(const key: TKey; bucketIndex, itemIndex: Integer;
      action: TCollectionChangedAction): TValue;
    function DoMoveNext(var itemIndex: Integer;
      iteratorVersion: Integer): Boolean;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
  public
    constructor Create; overload; override;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(capacity: Integer;
      const comparer: IEqualityComparer<TKey>); overload;

    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>; override;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; override;
    function Ordered: IEnumerable<TKeyValuePair>; override;
    function ToArray: TArray<TKeyValuePair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); override;
    function Remove(const key: TKey): Boolean; override;
    function Remove(const key: TKey; const value: TValue): Boolean; override;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; overload;
    function ExtractPair(const key: TKey): TKeyValuePair;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}

  {$REGION 'Implements IOrderedDictionary<TKey, TValue>'}
    function GetItemByIndex(index: Integer): TKeyValuePair;
    function IndexOf(const key: TKey): Integer;
    function IOrderedDictionary<TKey, TValue>.GetItem = GetItemByIndex;
  {$ENDREGION}
  end;

  TObjectDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    fOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); override;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); override;
  public
    constructor Create(ownerships: TDictionaryOwnerships; capacity: Integer;
      const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(ownerships: TDictionaryOwnerships;
      capacity: Integer); overload;
    constructor Create(ownerships: TDictionaryOwnerships;
      const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(ownerships: TDictionaryOwnerships); overload;
  end;

  TContainedDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    fController: Pointer;
    function GetController: IInterface;
  protected
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  {$ENDREGION}
  public
    constructor Create(const controller: IInterface;
      const comparer: IEqualityComparer<TKey>);
    property Controller: IInterface read GetController;
  end;

  TBidiDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IReadOnlyDictionary<TKey, TValue>, IDictionary<TKey, TValue>,
    IBidiDictionary<TKey, TValue>)
  private
    type
      TKeyValuePair = Generics.Collections.TPair<TKey, TValue>;
  private
    fValuesByKey: IDictionary<TKey, TValue>;
    fKeysByValue: IDictionary<TValue, TKey>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; inline;
    function GetKey(const value: TValue): TKey;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValue(const key: TKey): TValue;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); inline;
    procedure SetKey(const value: TValue; const key: TKey);
    procedure SetValue(const key: TKey; const value: TValue);
  {$ENDREGION}
  public
    constructor Create; overload; override;
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>); overload;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); override;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; override;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; reintroduce; overload;
    function ExtractPair(const key: TKey): TKeyValuePair; reintroduce; overload;
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
  {$ENDREGION}

  {$REGION 'Implements IBidiDictionary<TKey, TValue>'}
    function ExtractKey(const value: TValue): TKey;
    function ExtractValue(const key: TKey): TValue;
    function GetKeyOrDefault(const value: TValue): TKey; overload;
    function GetKeyOrDefault(const value: TValue; const defaultValue: TKey): TKey; overload;
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    function RemoveKey(const key: TKey): Boolean;
    function RemoveValue(const value: TValue): Boolean;
    function TryGetKey(const value: TValue; out key: TKey): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
  {$ENDREGION}
  end;

  TSortedDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>, IDictionary<TKey, TValue>,
    IReadOnlyDictionary<TKey, TValue>)
  private
    {$REGION 'Private types'}
    type
      TKeyValue = Generics.Collections.TPair<TKey, TValue>;
      PNode = TRedBlackTreeNodeHelper<TKey, TValue>.PNode;
    {$ENDREGION}
  private
    fTree: IRedBlackTree<TKey,TValue>;
    fKeyComparer: IComparer<TKey>;
    fValueComparer: IComparer<TValue>;
    fKeyValueComparerByKey: IComparer<TKeyValue>;
    fKeys: IList<TKey>;
    fValues: IList<TValue>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}
  public
    constructor Create; override;
    constructor Create(const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValue>; override;
    function Contains(const value: TKeyValue;
      const comparer: IEqualityComparer<TKeyValue>): Boolean; override;
    function Ordered: IEnumerable<TKeyValue>; override;
    function ToArray: TArray<TKeyValue>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); overload; override;
    function Remove(const key: TKey): Boolean; overload; override;
    function Remove(const key: TKey; const value: TValue): Boolean; override;
    function Extract(const key: TKey; const value: TValue): TKeyValue; overload; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; reintroduce; overload;
    function ExtractPair(const key: TKey): TKeyValue; reintroduce; overload;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}

  {$REGION 'Implements IReadOnlyDictionary<TKey, TValue>'}
    function GetValueOrDefault(const key: TKey): TValue; overload;
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
  {$ENDREGION}
  end;

implementation

uses
  RTLConsts,
  SysConst,
  SysUtils,
  Types,
  TypInfo,
  Math,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.ResourceStrings;


{$REGION 'TDictionaryItem<TKey, TValue>' }

function TDictionaryItem<TKey, TValue>.Removed: Boolean;
begin
  Result := HashCode and RemovedFlag <> 0;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create;
begin
  Create(0, nil);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer);
begin
  Create(capacity, nil);
end;

constructor TDictionary<TKey, TValue>.Create(
  const comparer: IEqualityComparer<TKey>);
begin
  Create(0, comparer);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer;
  const comparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  fKeys := TKeyCollection.Create(Self);
  fValues := TValueCollection.Create(Self);
  if Assigned(comparer) then
    fEqualityComparer := comparer
  else
    fEqualityComparer := TEqualityComparer<TKey>.Default;
  SetCapacity(capacity);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  fKeys.Free;
  fValues.Free;
  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.SetCapacity(value: Integer);
var
  newCapacity: Integer;
begin
  if value = 0 then
    newCapacity := 0
  else
    newCapacity := Math.Max(MinCapacity, value);
  if newCapacity <> fCapacity then
    Rehash(newCapacity);
end;

procedure TDictionary<TKey, TValue>.Rehash(newCapacity: Integer);
var
  bucketIndex, itemIndex: Integer;
  sourceItemIndex, destItemIndex: Integer;
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
    destItemIndex := 0;
    for sourceItemIndex := 0 to fItemCount - 1 do
      if not fItems[sourceItemIndex].Removed then
      begin
        if destItemIndex < sourceItemIndex then
          TArrayManager<TItem>.Move(fItems, sourceItemIndex, destItemIndex, 1);
        Inc(destItemIndex);
      end;
    TArrayManager<TItem>.Finalize(fItems, destItemIndex, fItemCount - fCount);
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
    Find(fItems[fItemCount].Key, fItems[fItemCount].HashCode, bucketIndex, itemIndex);
    Assert(itemIndex = fItemCount);
    fBuckets[bucketIndex] := itemIndex or (fItems[itemIndex].HashCode and fBucketHashCodeMask);
    Inc(fItemCount);
  end;
end;

function TDictionary<TKey, TValue>.Grow: Boolean;
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

function TDictionary<TKey, TValue>.Find(const key: TKey; hashCode: Integer;
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
      if fEqualityComparer.Equals(fItems[itemIndex].Key, key) then
        Exit(True);
    end;

    bucketIndex := (bucketIndex + 1) and fBucketIndexMask;
  end;
end;

function TDictionary<TKey, TValue>.Hash(const key: TKey): Integer;
begin
  Result := fEqualityComparer.GetHashCode(key) and not TItem.RemovedFlag;
end;

procedure TDictionary<TKey, TValue>.DoAdd(hashCode, bucketIndex, itemIndex: Integer;
  const key: TKey; const value: TValue);
var
  item: TKeyValuePair;
begin
  IncUnchecked(fVersion);
  fBuckets[bucketIndex] := itemIndex or (hashCode and fBucketHashCodeMask);
  fItems[itemIndex].HashCode := hashCode;
  fItems[itemIndex].Key := key;
  fItems[itemIndex].Value := value;
  Inc(fCount);
  Inc(fItemCount);

  item.Key := key;
  item.Value := value;
  Changed(item, caAdded);
  KeyChanged(item.key, caAdded);
  ValueChanged(item.value, caAdded);
end;

procedure TDictionary<TKey, TValue>.DoSetValue(itemIndex: Integer;
  const value: TValue);
var
  oldValue: TValue;
  item: TKeyValuePair;
begin
  oldValue := fItems[itemIndex].Value;

  IncUnchecked(fVersion);
  fItems[itemIndex].Value := value;

  item.Key := fItems[itemIndex].Key;
  item.Value := oldValue;
  Changed(item, caRemoved);
  ValueChanged(item.Value, caRemoved);
  item.Value := value;
  Changed(item, caAdded);
  ValueChanged(item.Value, caAdded);
end;

function TDictionary<TKey, TValue>.DoRemove(const key: TKey;
  bucketIndex, itemIndex: Integer;
  action: TCollectionChangedAction): TValue;
var
  item: TKeyValuePair;
begin
  Result := fItems[itemIndex].Value;

  IncUnchecked(fVersion);
  fBuckets[bucketIndex] := UsedBucket;
  fItems[itemIndex].Key := Default(TKey);
  fItems[itemIndex].Value := Default(TValue);
  fItems[itemIndex].HashCode := fItems[itemIndex].HashCode or TItem.RemovedFlag;
  Dec(fCount);

  item.Key := key;
  item.Value := Result;
  Changed(item, action);
  KeyChanged(item.Key, action);
  ValueChanged(item.Value, action);
end;

function TDictionary<TKey, TValue>.DoMoveNext(var itemIndex: Integer;
  iteratorVersion: Integer): Boolean;
begin
  if iteratorVersion <> fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  while itemIndex < fItemCount - 1 do
  begin
    Inc(itemIndex);
    if not fItems[itemIndex].Removed then
      Exit(True);
  end;
  Result := False;
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(self);
end;

procedure TDictionary<TKey, TValue>.Clear;
var
  oldItemIndex, oldItemCount: Integer;
  oldItems: TArray<TItem>;
  item: TKeyValuePair;
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
    begin
      item.Key := oldItems[oldItemIndex].Key;
      item.Value := oldItems[oldItemIndex].Value;
      Changed(item, caRemoved);
      KeyChanged(item.Key, caRemoved);
      ValueChanged(item.Value, caRemoved);
    end;
end;

function TDictionary<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  pair: TKeyValuePair;
begin
  pair.Key := value.Key;
  Result := TryGetValue(value.Key, pair.Value);
  if Result then
    Result := comparer.Equals(pair, value);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  found: Boolean;
  foundValue: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  found := TryGetValue(key, foundValue);
  if found then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    found := comparer.Equals(foundValue, value);
    if found then
      Result := ExtractPair(key);
  end;
  if not found then
  begin
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TKeyValuePair>;
var
  sourceIndex, destIndex: Integer;
begin
  SetLength(Result, fCount);
  destIndex := 0;
  for sourceIndex := 0 to fItemCount - 1 do
    if not fItems[sourceIndex].Removed then
    begin
      Result[destIndex].Key := fItems[sourceIndex].Key;
      Result[destIndex].Value := fItems[sourceIndex].Value;
      Inc(destIndex);
    end;
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
var
  bucketIndex, itemIndex, hashCode: Integer;
begin
  hashCode := Hash(key);
  if Find(key, hashCode, bucketIndex, itemIndex) then
    raise EListError.CreateRes(@SGenericDuplicateItem);
  if Grow then
    // rehash invalidates the indices
    Find(key, hashCode, bucketIndex, itemIndex);
  DoAdd(hashCode, bucketIndex, itemIndex, key, value);
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
var
  bucketIndex, itemIndex, hashCode: Integer;
begin
  hashCode := Hash(key);
  if Find(key, hashCode, bucketIndex, itemIndex) then
    // modify existing value
    DoSetValue(itemIndex, value)
  else
  begin
    // add new value
    if Grow then
      // rehash invalidates the indices
      Find(key, hashCode, bucketIndex, itemIndex);
    DoAdd(hashCode, bucketIndex, itemIndex, key, value);
  end;
end;

function TDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := Find(key, Hash(key), bucketIndex, itemIndex);
end;

function TDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(item, value);
end;

function TDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
var
  itemIndex: Integer;
  comparer: IEqualityComparer<TValue>;
begin
  comparer := TEqualityComparer<TValue>.Default;
  for itemIndex := 0 to fItemCount - 1 do
    if not fItems[itemIndex].Removed then
      if comparer.Equals(fItems[itemIndex].Value, value) then
        Exit(True);
  Result := False;
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  Result := ExtractPair(key).Value;
end;

function TDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TKeyValuePair;
var
  bucketIndex, itemIndex: Integer;
begin
  Result.Key := key;
  if Find(key, Hash(key), bucketIndex, itemIndex) then
    Result.Value := DoRemove(key, bucketIndex, itemIndex, caExtracted)
  else
    Result.Value := Default(TValue);
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := Find(key, Hash(key), bucketIndex, itemIndex);
  if Result then
    value := fItems[itemIndex].Value
  else
    value := Default(TValue);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  bucketIndex, itemIndex: Integer;
begin
  Result := Find(key, Hash(key), bucketIndex, itemIndex);
  if Result then
    DoRemove(key, bucketIndex, itemIndex, caRemoved);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  bucketIndex, itemIndex: Integer;
  comparer: IEqualityComparer<TValue>;
begin
  Result := Find(key, Hash(key), bucketIndex, itemIndex);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(fItems[itemIndex].value, value);
    if Result then
      DoRemove(key, bucketIndex, itemIndex, caRemoved);
  end;
end;

function TDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  TryGetValue(key, Result);
end;

function TDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
var
  bucketIndex, itemIndex: Integer;
begin
  if not Find(key, Hash(key), bucketIndex, itemIndex) then
    raise EListError.CreateRes(@SGenericItemNotFound);
  Result := fItems[itemIndex].Value;
end;

function TDictionary<TKey, TValue>.Ordered: IEnumerable<TKeyValuePair>;
begin
  Result := TOrderedEnumerable.Create(Self);
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
begin
  AddOrSetValue(key, value);
end;

function TDictionary<TKey, TValue>.GetItemByIndex(index: Integer): TKeyValuePair;
begin
  if fCount <> fItemCount then
    Rehash(fCapacity);
  Result.Key := fItems[index].Key;
  Result.Value := fItems[index].Value;
end;

function TDictionary<TKey, TValue>.IndexOf(const key: TKey): Integer;
var
  bucketIndex: Integer;
begin
  if fCount <> fItemCount then
    Rehash(fCapacity);
  if not Find(key, Hash(key), bucketIndex, Result) then
    Result := -1;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TEnumerator' }

constructor TDictionary<TKey, TValue>.TEnumerator.Create(
  const source: TDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fItemIndex := -1;
  fVersion := fSource.fVersion;
end;

function TDictionary<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fSource.fItems[fItemIndex].Key;
  Result.Value := fSource.fItems[fItemIndex].value;
end;

function TDictionary<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fItemIndex, fVersion);
end;

{$ENDREGION }


{$REGION 'TDictionary<TKey, TValue>.TKeyEnumerator' }

constructor TDictionary<TKey, TValue>.TKeyEnumerator.Create(
  const source: TDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fItemIndex := -1;
  fVersion := fSource.fVersion;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := fSource.fItems[fItemIndex].Key;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fItemIndex, fVersion);
end;

{$ENDREGION }


{$REGION 'TDictionary<TKey, TValue>.TKeyCollection'}

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(
  const dictionary: TDictionary<TKey, TValue>);
begin
  inherited Create(dictionary);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TKeyCollection.Contains(const value: TKey;
  const comparer: IEqualityComparer<TKey>): Boolean;
begin
  Result := fDictionary.ContainsKey(value);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  key: TKey;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for key in fDictionary.Keys do
  begin
    Result[index] := key;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(fDictionary);
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TValueEnumerator'}

constructor TDictionary<TKey, TValue>.TValueEnumerator.Create(
  const source: TDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fItemIndex := -1;
  fVersion := fSource.fVersion;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := fSource.fItems[fItemIndex].Value;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  Result := fSource.DoMoveNext(fItemIndex, fVersion);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TValueCollection'}

constructor TDictionary<TKey, TValue>.TValueCollection.Create(
  const dictionary: TDictionary<TKey, TValue>);
begin
  inherited Create(dictionary);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  value: TValue;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for value in fDictionary.Values do
  begin
    Result[index] := value;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(fDictionary);
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TOrderedEnumerable'}

constructor TDictionary<TKey, TValue>.TOrderedEnumerable.Create(
  const source: TDictionary<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
{$IFNDEF AUTOREFCOUNT}
  fSource._AddRef;
{$ENDIF}
end;

destructor TDictionary<TKey, TValue>.TOrderedEnumerable.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  fSource._Release;
{$ENDIF}
  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.TOrderedEnumerable.Dispose;
begin
  fSortedItemIndices := nil;
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.Clone: TIterator<TKeyValuePair>;
begin
  Result := TOrderedEnumerable.Create(fSource);
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.GetCount: Integer;
begin
  Result := fSource.Count;
end;

procedure TDictionary<TKey, TValue>.TOrderedEnumerable.Start;
var
  sourceIndex, destIndex: Integer;
  comparer: IComparer<TKey>;
begin
  fIndex := 0;
  fVersion := fSource.fVersion;

  comparer := TComparer<TKey>.Default;
  SetLength(fSortedItemIndices, fSource.Count);
  destIndex := 0;
  for sourceIndex := 0 to fSource.fItemCount - 1 do
    if not fSource.fItems[sourceIndex].Removed then
    begin
      fSortedItemIndices[destIndex] := sourceIndex;
      Inc(destIndex);
    end;

  TArray.Sort<Integer>(
    fSortedItemIndices,
    function(const Left, Right: Integer): Integer
    begin
      Result := comparer.Compare(fSource.fItems[Left].Key,
        fSource.fItems[Right].Key);
    end
  );
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.TryMoveNext(var current: TKeyValuePair): Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < Length(fSortedItemIndices) then
  begin
    current.Key := fSource.fItems[fSortedItemIndices[fIndex]].Key;
    current.Value := fSource.fItems[fSortedItemIndices[fIndex]].Value;
    Inc(fIndex);
    Exit(True);
  end;
  Result := False;
end;

{$ENDREGION}


{$REGION 'TObjectDictionary<TKey, TValue>'}

constructor TObjectDictionary<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships; capacity: Integer;
  const comparer: IEqualityComparer<TKey>);
begin
  inherited Create(capacity, Comparer);

  if doOwnsKeys in Ownerships then
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey)).Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);

   if doOwnsValues in Ownerships then
     if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue)).Kind <> tkClass) then
       raise EInvalidCast.CreateRes(@SInvalidCast);

  fOwnerships := ownerships;
end;

constructor TObjectDictionary<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships; capacity: Integer);
begin
  Create(ownerships, capacity, nil);
end;

constructor TObjectDictionary<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships; const comparer: IEqualityComparer<TKey>);
begin
  Create(ownerships, 0, comparer);
end;

constructor TObjectDictionary<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships);
begin
  Create(ownerships, 0, nil);
end;

procedure TObjectDictionary<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  inherited KeyChanged(item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

procedure TObjectDictionary<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  inherited ValueChanged(item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TContainedDictionary<TKey, TValue>'}

constructor TContainedDictionary<TKey, TValue>.Create(
  const controller: IInterface; const comparer: IEqualityComparer<TKey>);
begin
  inherited Create(comparer);
  fController := Pointer(controller);
end;

function TContainedDictionary<TKey, TValue>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedDictionary<TKey, TValue>._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TContainedDictionary<TKey, TValue>._Release: Integer;
begin
  Result := IInterface(FController)._Release;
end;

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>'}

constructor TBidiDictionary<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TBidiDictionary<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>);
begin
  inherited Create;
  fKeysByValue := TDictionary<TValue, TKey>.Create(valueComparer);
  fValuesByKey := TDictionary<TKey, TValue>.Create(keyComparer);
end;

procedure TBidiDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  if fValuesByKey.ContainsKey(key) then
    raise EInvalidOperationException.Create('key');
  if fKeysByValue.ContainsKey(value) then
    raise EInvalidOperationException.Create('value');
  fValuesByKey.Add(key, value);
  fKeysByValue.Add(value, key);
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
var
  oldValue: TValue;
begin
  RemoveValue(value);
  if fValuesByKey.TryGetValue(key, oldValue) then
    fKeysByValue.Remove(oldValue);
  fKeysByValue.Add(value, key);
  fValuesByKey[key] := value;
end;

function TBidiDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TBidiDictionary<TKey, TValue>.Clear;
begin
  fValuesByKey.Clear;
  fKeysByValue.Clear;
end;

function TBidiDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fValuesByKey.ContainsKey(key);
end;

function TBidiDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(value, item);
end;

function TBidiDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fKeysByValue.ContainsKey(value);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  Result := ExtractValue(key);
end;

function TBidiDictionary<TKey, TValue>.ExtractKey(const value: TValue): TKey;
begin
  if fKeysByValue.TryGetValue(value, Result) then
  begin
    fKeysByValue.Extract(value);
    fValuesByKey.Extract(Result);
  end
  else
    Result := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TKeyValuePair;
begin
  raise ENotImplementedException.Create('ExtractPair');
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
begin
  Result := fValuesByKey.Extract(key, value);
  fKeysByValue.Extract(value, key);
end;

function TBidiDictionary<TKey, TValue>.ExtractValue(const key: TKey): TValue;
begin
  if fValuesByKey.TryGetValue(key, Result) then
  begin
    fKeysByValue.Extract(Result);
    fValuesByKey.Extract(key);
  end
  else
    Result := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fValuesByKey.Count;
end;

function TBidiDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := fValuesByKey.GetEnumerator();
end;

function TBidiDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := GetValue(key);
end;

function TBidiDictionary<TKey, TValue>.GetKey(const value: TValue): TKey;
begin
  Result := fKeysByValue[value];
end;

function TBidiDictionary<TKey, TValue>.GetKeyOrDefault(
  const value: TValue): TKey;
begin
  if not fKeysByValue.TryGetValue(value, Result) then
    Result := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.GetKeyOrDefault(const value: TValue;
  const defaultValue: TKey): TKey;
begin
  if not fKeysByValue.TryGetValue(value, Result) then
    Result := defaultValue;
end;

function TBidiDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fValuesByKey.Keys;
end;

function TBidiDictionary<TKey, TValue>.GetValue(const key: TKey): TValue;
begin
  Result := fValuesByKey[key];
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(
  const key: TKey): TValue;
begin
  if not fValuesByKey.TryGetValue(key, Result) then
    Result := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fValuesByKey.TryGetValue(key, Result) then
    Result := defaultValue;
end;

function TBidiDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fKeysByValue.Keys;
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
begin
  Result := RemoveKey(key);
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(value, item);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);
  end;
end;

function TBidiDictionary<TKey, TValue>.RemoveKey(const key: TKey): Boolean;
var
  value: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, value);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);

    // notify
  end;
end;

function TBidiDictionary<TKey, TValue>.RemoveValue(const value: TValue): Boolean;
var
  key: TKey;
begin
  Result := fKeysByValue.TryGetValue(value, key);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);

    // notify
  end;
end;

procedure TBidiDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  SetValue(key, value);
end;

procedure TBidiDictionary<TKey, TValue>.SetKey(const value: TValue;
  const key: TKey);
var
  oldKey: TKey;
begin
  if fValuesByKey.ContainsKey(key) then
    raise EInvalidOperationException.Create('key');
  if fKeysByValue.TryGetValue(value, oldKey) then
    fValuesByKey.Remove(oldKey);
  fValuesByKey.Add(key, value);
  fKeysByValue[value] := key;
end;

procedure TBidiDictionary<TKey, TValue>.SetValue(const key: TKey;
  const value: TValue);
var
  oldValue: TValue;
begin
  if fKeysByValue.ContainsKey(value) then
    raise EInvalidOperationException.Create('value');
  if fValuesByKey.TryGetValue(key, oldValue) then
    fKeysByValue.Remove(oldValue);
  fKeysByValue.Add(value, key);
  fValuesByKey[key] := value;
end;

function TBidiDictionary<TKey, TValue>.TryGetKey(const value: TValue;
  out key: TKey): Boolean;
begin
  Result := fKeysByValue.TryGetValue(value, key);
end;

function TBidiDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fValuesByKey.TryGetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TSortedDictionary<TKey, TValue>'}

constructor TSortedDictionary<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TSortedDictionary<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>);
begin
  inherited Create;

  fKeyComparer := keyComparer;
  if not Assigned(fkeyComparer) then
    fKeyComparer := TComparer<TKey>.Default;
  fValueComparer := valueComparer;
  if not Assigned(fValueComparer) then
    fValueComparer := TComparer<TValue>.Default;
  fTree := TRedBlackTree<TKey,TValue>.Create(keyComparer);

  fKeys := TCollections.CreateList<TKey>;
  fValues := TCollections.CreateList<TValue>;
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSortedDictionary<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  if not fTree.Add(key, value) then
    raise EListError.CreateRes(@SGenericDuplicateItem);
  KeyChanged(key, caAdded);
  ValueChanged(value, caAdded);
end;

procedure TSortedDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    ValueChanged(node.Value, caRemoved);
    node.Value := value;
    ValueChanged(value, caAdded);
  end
  else
  begin
    fTree.Add(key, value);
    KeyChanged(key, caAdded);
    ValueChanged(value, caAdded);
  end;
end;

function TSortedDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    KeyChanged(PNode(node).Key, caRemoved);
    ValueChanged(PNode(node).Value, caRemoved);
    node := node.Next;
  end;

  fTree.Clear;
end;

function TSortedDictionary<TKey, TValue>.Contains(const value: TKeyValue;
  const comparer: IEqualityComparer<TKeyValue>): Boolean;
var
  found: TValue;
begin
  Result := fTree.Find(value.Key, found)
    and comparer.Equals(value, TKeyValue.Create(value.Key, found));
end;

function TSortedDictionary<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  found: TValue;
begin
  Result := fTree.Find(key, found)
    and (fValueComparer.Compare(value, found) = EqualsValue);
end;

function TSortedDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fTree.Exists(key);
end;

function TSortedDictionary<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  found: TKeyValue;
begin
  for found in fTree do
    if fValueComparer.Compare(value, found.Value) = EqualsValue then
      Exit(True);
  Result := False;
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValue;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node)
    and (fValueComparer.Compare(value, node.Value) = EqualsValue) then
  begin
    Result.Key := node.Key;
    Result.Value := node.Value;
    fTree.DeleteNode(node);
    KeyChanged(Result.Key, caExtracted);
    ValueChanged(Result.Value, caExtracted);
  end
  else
    Result := Default(TKeyValue);
end;

function TSortedDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    Result := node.Value;
    fTree.DeleteNode(node);
    KeyChanged(key, caExtracted);
    ValueChanged(Result, caExtracted);
  end
  else
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.ExtractPair(const key: TKey): TKeyValue;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  if Assigned(node) then
  begin
    Result.Key := node.Key;
    Result.Value := node.Value;
    fTree.DeleteNode(node);
    KeyChanged(Result.Key, caExtracted);
    ValueChanged(Result.Value, caExtracted);
  end
  else
    Result := Default(TKeyValue);
end;

function TSortedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fTree.Count;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValue>;
begin
  Result := fTree.GetEnumerator;
end;

function TSortedDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  if not TryGetValue(key, Result) then
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
var
  item: TKeyValue;
begin
  // TODO: implement this properly to always provide up to date information
  fKeys.Clear;
  for item in Self do
    fKeys.Add(item.Key);
  Result := fKeys.AsReadOnlyList;
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  if not fTree.Find(key, Result) then
    Result := Default(TValue);
end;

function TSortedDictionary<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fTree.Find(key, Result) then
    Result := defaultValue;
end;

function TSortedDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
var
  item: TKeyValue;
begin
  // TODO: implement this properly to always provide up to date information
  fValues.Clear;
  for item in Self do
    fValues.Add(item.Value);
  Result := fValues.AsReadOnlyList;
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
  if Result then
  begin
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    fTree.DeleteNode(node);
  end;
end;

function TSortedDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  node: PNode;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node)
    and (fValueComparer.Compare(value, node.Value) = EqualsValue);
  if Result then
  begin
    KeyChanged(node.Key, caRemoved);
    ValueChanged(node.Value, caRemoved);
    fTree.DeleteNode(node);
  end;
end;

procedure TSortedDictionary<TKey, TValue>.SetItem(const key: TKey; const value: TValue);
begin
  AddOrSetValue(key, value);
end;

function TSortedDictionary<TKey, TValue>.Ordered: IEnumerable<TKeyValue>;
begin
  Result := TOrderedIterator<TKeyValue>.Create(Self, fKeyValueComparerByKey);
end;

function TSortedDictionary<TKey, TValue>.ToArray: TArray<TKeyValue>;
var
  i: Integer;
  item: TKeyValue;
begin
  // TODO: consider adding ToArray to IBinaryTree
  SetLength(Result, Count);
  i := 0;
  for item in Self do
  begin
    Result[i] := item;
    Inc(i);
  end;
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fTree.Find(key, value);
end;

{$ENDREGION}


end.
