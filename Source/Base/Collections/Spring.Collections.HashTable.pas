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

unit Spring.Collections.HashTable;

interface

uses
  TypInfo;

type
  TEqualsKey = function(Self: Pointer; const left, right): Boolean;
  TGetHashCode = function(Self: Pointer; const item): Integer;

  THashTableEntry = record
    HashCode, BucketIndex, ItemIndex: Integer;
  end;

  PHashTable = ^THashTable;
  THashTable = record
  const
    KeyOffset = SizeOf(Integer);
  private
    fBuckets: TArray<Integer>;
    fItems: PByte;             // TArray<TItem>;
    fCount: Integer;
    fItemCount: Integer;
    fBucketIndexMask: Integer;
    fBucketHashCodeMask: Integer;
    fEqualsKey: TEqualsKey;
    fItemsInfo: PTypeInfo;     // TypeInfo(TArray<TItem>)
    fItemSize: Integer;        // SizeOf(TItem)
    fVersion: Integer;

    function GetKey(index: Integer): Pointer;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  public
    constructor Create(itemsInfo: PTypeInfo; equalsKey: TEqualsKey);

    procedure EnsureCompact;
    procedure Grow;
    procedure Pack;
    function Find(const key; var entry: THashTableEntry): Boolean;
    procedure Rehash(newCapacity: NativeInt);
    procedure IncrementVersion; inline;

    function Add(const key; hashCode: Integer): Pointer;
    function AddOrSet(const key; hashCode: Integer; out isExistingEntry: Boolean): Pointer;
    function Delete(const key; hashCode: Integer): Pointer; overload;
    function Delete(const entry: THashTableEntry): Pointer; overload;
    procedure Clear;

    property Buckets: TArray<Integer> read fBuckets;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read fCount;
    property ItemCount: Integer read fItemCount;
    property Items: PByte read fItems;
    property ItemSize: Integer read fItemSize;
    property Version: Integer read fVersion;
  end;

implementation

uses
  Math,
  Spring;

const
  // use the MSB of the HashCode to note removed items
  RemovedFlag        = Integer($80000000);
  MinCapacity        = 6; // 75% load factor leads to min bucket count of 8
  EmptyBucket        = -1; // must be negative, note choice of BucketSentinelFlag
  UsedBucket         = -2; // likewise


{$REGION 'THashTable'}

constructor THashTable.Create(itemsInfo: PTypeInfo; equalsKey: TEqualsKey);
begin
  fItemsInfo := itemsInfo;
  fItemSize := itemsInfo.TypeData.elSize;
  fEqualsKey := equalsKey;
end;

function THashTable.GetCapacity: Integer;
begin
  Result := DynArrayLength(fItems);
end;

function THashTable.GetKey(index: Integer): Pointer;
begin
  Result := @fItems[index * fItemSize + KeyOffset];
end;

function THashTable.Add(const key; hashCode: Integer): Pointer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := hashCode;
  if not Find(key, entry) then
  begin
    if fItemCount = DynArrayLength(fItems) then
    begin
      Grow;
      Find(key, entry);
    end;

    {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
    fBuckets[entry.BucketIndex] := entry.ItemIndex or (entry.HashCode and fBucketHashCodeMask);

    Result := fItems + entry.ItemIndex * fItemSize;
    PInteger(Result)^ := entry.HashCode;

    Inc(fCount);
    Inc(fItemCount);
  end
  else
    Result := nil;
end;

function THashTable.AddOrSet(const key; hashCode: Integer;
  out isExistingEntry: Boolean): Pointer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := hashCode;
  isExistingEntry := Find(key, entry);
  if not isExistingEntry then
    if fItemCount = DynArrayLength(fItems) then
    begin
      Grow;
      Find(key, entry);
    end;

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}

  fBuckets[entry.BucketIndex] := entry.ItemIndex or (entry.HashCode and fBucketHashCodeMask);

  Result := fItems + entry.ItemIndex * fItemSize;
  PInteger(Result)^ := entry.HashCode;

  if not isExistingEntry then
  begin
    Inc(fCount);
    Inc(fItemCount);
  end;
end;

function THashTable.Delete(const key; hashCode: Integer): Pointer;
var
  entry: THashTableEntry;
begin
  entry.HashCode := hashCode;
  if Find(key, entry) then
    Result := Delete(entry)
  else
    Result := nil;
end;

function THashTable.Delete(const entry: THashTableEntry): Pointer;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}

  fBuckets[entry.BucketIndex] := UsedBucket;

  Result := fItems + entry.ItemIndex * fItemSize;
  PInteger(Result)^ := RemovedFlag;

  Dec(fCount);
end;

procedure THashTable.Clear;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
  fCount := 0;
  fItemCount := 0;
  fBuckets := nil;
  DynArrayClear(Pointer(fItems), fItemsInfo);
  Capacity := 0;
end;

procedure THashTable.EnsureCompact;
begin
  if fCount <> fItemCount then
    Rehash(Capacity);
end;

procedure THashTable.Grow;
var
  newCapacity: Integer;
begin
  newCapacity := Capacity;
  if newCapacity = 0 then
    newCapacity := MinCapacity
  else if 2 * fCount >= DynArrayLength(fBuckets) then
    // only grow if load factor is greater than 0.5
    newCapacity := newCapacity * 2;
  Rehash(newCapacity);
end;

procedure THashTable.IncrementVersion;
begin
  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}
end;

procedure THashTable.Pack;
var
  sourceItem, targetItem: PByte;
  i: Integer;
begin
  sourceItem := fItems;
  targetItem := fItems;
  for i := 0 to fItemCount - 1 do
  begin
    if PInteger(sourceItem)^ >= 0 then // not removed
    begin
      if targetItem < sourceItem then // need to fill a gap caused by previous items that were removed
        CopyRecord(targetItem, sourceItem, fItemsInfo.TypeData.elType2^);
      Inc(targetItem, fItemSize);
    end;
    Inc(sourceItem, fItemSize);
  end;
  FinalizeArray(targetItem, fItemsInfo.TypeData.elType2^, fItemCount - fCount); // clear remaining items that were previously moved
end;

function THashTable.Find(const key; var entry: THashTableEntry): Boolean;
var
  bucketValue: Integer;
begin
  if fItems <> nil then
  begin
    entry.HashCode := entry.HashCode and not RemovedFlag;
    entry.BucketIndex := entry.HashCode and fBucketIndexMask;
    while True do
    begin
      bucketValue := fBuckets[entry.BucketIndex];

      if bucketValue = EmptyBucket then
      begin
        entry.ItemIndex := fItemCount;
        Exit(False);
      end;

      if (bucketValue <> UsedBucket)
        and (bucketValue and fBucketHashCodeMask = entry.HashCode and fBucketHashCodeMask) then
      begin
        entry.ItemIndex := bucketValue and fBucketIndexMask;
        if fEqualsKey(@Self, GetKey(entry.ItemIndex)^, key) then
          Exit(True);
      end;

      entry.BucketIndex := (entry.BucketIndex + 1) and fBucketIndexMask;
    end;
  end;
  Result := False;
end;

procedure THashTable.Rehash(newCapacity: NativeInt);
var
  newBucketCount, i: Integer;
  entry: THashTableEntry;
  item: PByte;
begin
  if newCapacity = 0 then
  begin
    Assert(fCount = 0);
    Assert(fItemCount = 0);
    Assert(not Assigned(fBuckets));
    Assert(not Assigned(fItems));
    Exit;
  end;

  Assert(newCapacity >= fCount);

  {$IFOPT Q+}{$DEFINE OVERFLOWCHECKS_OFF}{$Q-}{$ENDIF}
  Inc(fVersion);
  {$IFDEF OVERFLOWCHECKS_OFF}{$UNDEF OVERFLOWCHECKS_OFF}{$Q+}{$ENDIF}

  newBucketCount := NextPowerOf2(newCapacity * 4 div 3 - 1); // 75% load factor

  // compact the items array, if necessary
  if fItemCount > fCount then
    Pack;

  // resize the items array, safe now that we have compacted it
  newCapacity := newBucketCount * 3 div 4;
  DynArraySetLength(Pointer(fItems), fItemsInfo, 1, @newCapacity);
  Assert(Capacity >= fCount);

  // repopulate the bucket array
  fBucketIndexMask := newBucketCount - 1;
  fBucketHashCodeMask := not fBucketIndexMask and not RemovedFlag;
  SetLength(fBuckets, newBucketCount);
  for i := 0 to newBucketCount - 1 do
    fBuckets[i] := EmptyBucket;

  item := fItems;
  fItemCount := 0;
  while fItemCount < fCount do
  begin
    entry.HashCode := PInteger(item)^;
    Find((item + KeyOffset)^, entry);
    fBuckets[entry.BucketIndex] := fItemCount or (entry.HashCode and fBucketHashCodeMask);
    Inc(item, fItemSize);
    Inc(fItemCount);
  end;
end;

procedure THashTable.SetCapacity(const value: Integer);
var
  newCapacity: Integer;
begin
  Guard.CheckRange(value >= fCount, 'capacity');

  if value = 0 then
    newCapacity := 0
  else
    newCapacity := Math.Max(MinCapacity, value);
  if newCapacity <> Capacity then
    Rehash(newCapacity);
end;

{$ENDREGION}


end.
