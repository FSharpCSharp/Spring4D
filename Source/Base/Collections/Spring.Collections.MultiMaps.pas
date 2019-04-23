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

unit Spring.Collections.MultiMaps;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.HashTable,
  Spring.Events,
  Spring.Events.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  TMultiMapItem<TKey, TValue> = packed record
  public
    HashCode: Integer;
    Key: TKey;
    Values: ICollection<TValue>;
  end;

  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = TPair<TKey, TValue>;
      TItem = TMultiMapItem<TKey, TValue>;
      TItems = TArray<TItem>;
      PItem = ^TItem;

      TEnumerator = class(TRefCountedObject,
        IEnumerator<TKeyValuePair>, IEnumerator<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
        fIndex: Integer;
        fVersion: Integer;
        fEnumerator: IEnumerator<TValue>;
        fCurrent: TKeyValuePair;
        function GetCurrent: TKeyValuePair;
        function GetCurrentValue: TValue;
        function IEnumerator<TValue>.GetCurrent = GetCurrentValue;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TKeyCollection = TInnerCollection<TKey>;

      TValueCollection = class(TEnumerableBase<TValue>,
        IEnumerable<TValue>, IReadOnlyCollection<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
      {$REGION 'Property Accessors'}
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
      {$ENDREGION}
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);

      {$REGION 'Implements IInterface'}
        function _AddRef: Integer; override;
        function _Release: Integer; override;
      {$ENDREGION}

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; overload;
        function ToArray: TArray<TValue>;
      {$ENDREGION}
      end;

      TWrappedCollection = class(TEnumerableBase<TValue>,
        IEnumerable<TValue>, IReadOnlyCollection<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fMap: TMultiMapBase<TKey, TValue>;
        fKey: TKey;
        fDelegate: ICollection<TValue>;
        function GetCount: Integer;
        procedure RefreshIfEmpty;
        procedure HandleDestroy(Sender: TObject);
      public
        constructor Create(const key: TKey;
          const map: TMultiMapBase<TKey, TValue>;
          const delegate: ICollection<TValue>);
        destructor Destroy; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; overload;
        function GetEnumerator: IEnumerator<TValue>;
        function ToArray: TArray<TValue>;
      end;

      TWrappedEnumerator = class(TRefCountedObject, IEnumerator<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TWrappedCollection;
        fDelegate: IEnumerator<TValue>;
        fOriginal: ICollection<TValue>;
        procedure ValidateEnumerator;
        function GetCurrent: TValue;
      public
        constructor Create(const source: TWrappedCollection);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeyComparer: IEqualityComparer<TKey>;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fCount: Integer;
    fOwnerships: TDictionaryOwnerships;
    fOnDestroy: TNotifyEventImpl;
    procedure DoValueChanged(sender: TObject; const item: TValue;
      action: TCollectionChangedAction);
    class function CompareKey(Self: Pointer; const left, right): Boolean; static;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    function CreateCollection: ICollection<TValue>; virtual; abstract;
    procedure DoRemove(const entry: THashTableEntry;
      action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); inline;
  public
    constructor Create; override;
    constructor Create(ownerships: TDictionaryOwnerships); overload;
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships = []); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; out values: IReadOnlyCollection<TValue>): Boolean;
  {$ENDREGION}
  end;

  TListMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  protected
    function CreateCollection: ICollection<TValue>; override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  end;

  THashMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IEqualityComparer<TValue>;
  protected
    function CreateCollection: ICollection<TValue>; override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships = []); overload;
  end;

  TTreeMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IComparer<TValue>;
  protected
    function CreateCollection: ICollection<TValue>; override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>;
      ownerships: TDictionaryOwnerships = []); overload;
  end;

implementation

uses
  Classes,
  TypInfo,
  Spring.ResourceStrings;


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create;
begin
  Create(nil, []);
end;

constructor TMultiMapBase<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships);
begin
  Create(nil, ownerships);
end;

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  if doOwnsKeys in ownerships then
    if TType.Kind<TKey> <> tkClass then
      raise Error.NoClassType(TypeInfo(TKey));

  if doOwnsValues in ownerships then
    if TType.Kind<TValue> <> tkClass then
      raise Error.NoClassType(TypeInfo(TValueType));

  inherited Create;
  fOwnerships := ownerships;
  if Assigned(keyComparer) then
    fKeyComparer := keyComparer
  else
    fKeyComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, TypeInfo(TKey), SizeOf(TKey)));

  fKeys := TKeyCollection.Create(Self, @fHashTable, KeyType, fKeyComparer, 0);
  fValues := TValueCollection.Create(Self);

  fHashTable := THashTable.Create(TypeInfo(TItems), CompareKey);

  fOnDestroy := TNotifyEventImpl.Create;
end;

destructor TMultiMapBase<TKey, TValue>.Destroy;
begin
  fOnDestroy.Invoke(Self);
  fOnDestroy.Free;
  Clear;
  fKeys.Free;
  fValues.Free;
  inherited Destroy;
end;

class function TMultiMapBase<TKey, TValue>.CompareKey(Self: Pointer; const left,
  right): Boolean;
begin
  Result := IEqualityComparer<TKey>(PPointer(PByte(Self) + SizeOf(THashTable))^).Equals(TKey(left), TKey(right));
end;

procedure TMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    FreeObject(item);
end;

procedure TMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    FreeObject(item);
end;

function TMultiMapBase<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := Low(values) to High(values) do
    Add(key, values[i]);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  item: TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(values), 'values');
{$ENDIF}

  for item in values do
    Add(key, item);
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
var
  oldItemCount, i: Integer;
  oldItems: TArray<TItem>;
  oldValue: TValue;
begin
  oldItems := TItems(fHashTable.Items);
  oldItemCount := fHashTable.ItemCount;

  fHashTable.Clear;

  for i := 0 to oldItemCount - 1 do
    if oldItems[i].HashCode >= 0 then
    begin
      oldItems[i].Values.OnChanged.Remove(DoValueChanged);
      for oldValue in oldItems[i].Values do
      begin
        if Assigned(Notify) then
          DoNotify(oldItems[i].Key, oldValue, caRemoved);
        ValueChanged(oldValue, caRemoved);
      end;
      oldItems[i].Values.Clear;
      oldItems[i].Values := nil;
      KeyChanged(oldItems[i].Key, caRemoved);
    end;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
    Result := TItems(fHashTable.Items)[entry.ItemIndex].Values.Contains(value)
  else
    Result := False;
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  Result := fHashTable.Find(key, entry);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      if TItems(fHashTable.Items)[i].Values.Contains(value) then
        Exit(True);
  Result := False;
end;

procedure TMultiMapBase<TKey, TValue>.DoRemove(const entry: THashTableEntry;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
var
  item: PItem;
  value: TValue;
begin
  item := fHashTable.Delete(entry);
  item.Values.OnChanged.Remove(DoValueChanged);
  for value in item.Values do
  begin
    Dec(fCount);
    if Assigned(Notify) then
      DoNotify(item.Key, value, action);
    ValueChanged(value, action);
  end;
  if action = caRemoved then
    item.Values.Clear
  else
    item.Values.MoveTo(extractTarget);
  KeyChanged(item.Key, action);
  item.HashCode := RemovedFlag;
  item.Key := Default(TKey);
  item.Values := nil;
end;

procedure TMultiMapBase<TKey, TValue>.DoValueChanged(sender: TObject;
  const item: TValue; action: TCollectionChangedAction);
begin
  case action of
    caAdded: Inc(fCount);
    caRemoved, caExtracted: Dec(fCount);
  end;
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    FreeObject(item);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  entry: THashTableEntry;
  item: PItem;
  count: Integer;
begin
  Result.Key := key;
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
  begin
    item := @TItems(fHashTable.Items)[entry.ItemIndex];
    count := item.Values.Count;
    Result.Value := item.Values.Extract(value);
    if item.Values.Count < count then
    begin
      if Assigned(Notify) then
        DoNotify(key, value, caExtracted);
      KeyChanged(item.Key, caExtracted);
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey): ICollection<TValue>;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
  begin
    Result := CreateCollection;
    DoRemove(entry, caExtracted, Result);
  end;
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(Self);
end;

function TMultiMapBase<TKey, TValue>.GetIsEmpty: Boolean;
begin
  Result := fCount = 0;
end;

function TMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
    Result := TItems(fHashTable.Items)[entry.ItemIndex].Values as IReadOnlyCollection<TValue>
  else
    Result := TWrappedCollection.Create(key, Self, CreateCollection);
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
  begin
    item := @TItems(fHashTable.Items)[entry.ItemIndex];
    if item.Values.Remove(value) then
    begin
      if Assigned(Notify) then
        DoNotify(key, value, caRemoved);
      if not item.Values.Any then
        DoRemove(entry, caRemoved, nil);
      Exit(True);
    end;
  end;
  Result := False;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
  begin
    DoRemove(entry, caRemoved, nil);
    Result := True;
  end
  else
    Result := False;
end;

function TMultiMapBase<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  entry: PItem;
  isExisting: Boolean;
begin
  entry := fHashTable.AddOrSet(key, fKeyComparer.GetHashCode(key), isExisting);

  if not isExisting then
  begin
    entry.Key := key;
    entry.Values := CreateCollection;
    entry.Values.OnChanged.Add(DoValueChanged);
    KeyChanged(key, caAdded);
  end;

  Result := entry.Values.Add(value);
  if Result then
    if Assigned(Notify) then
      DoNotify(key, value, caAdded);
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  out values: IReadOnlyCollection<TValue>): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(key);
  if fHashTable.Find(key, entry) then
  begin
    values := TItems(fHashTable.Items)[entry.ItemIndex].Values as IReadOnlyCollection<TValue>;
    Result := True;
  end
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fHashTable.Version;
end;

destructor TMultiMapBase<TKey, TValue>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result := fCurrent;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrentValue: TValue;
begin
  Result := fCurrent.Value;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;

  if fVersion = hashTable.Version then
  begin
    while True do
    begin
      if Assigned(fEnumerator) then
        if fEnumerator.MoveNext then
        begin
          fCurrent.Value := fEnumerator.Current;
          Exit(True);
        end
        else
          fEnumerator := nil;

      if fIndex >= hashTable.ItemCount then
        Break;

      item := @TItems(hashTable.Items)[fIndex];
      Inc(fIndex);
      if item.HashCode < 0 then
        Continue;

      fCurrent.Key := item.Key;
      fEnumerator := item.Values.GetEnumerator;
    end;
    Exit(False);
  end
  else
    raise Error.EnumFailedVersion;

//  if not Assigned(fDictionaryEnumerator) then
//    fDictionaryEnumerator := fSource.fDictionary.GetEnumerator;
//
//  if fVersion <> fSource.fVersion then
//    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);
//
//  repeat
//    if Assigned(fCollectionEnumerator) and fCollectionEnumerator.MoveNext then
//      Exit(True)
//    else
//    begin
//      Result := fDictionaryEnumerator.MoveNext;
//      if Result then
//        fCollectionEnumerator := fDictionaryEnumerator.Current.Value.GetEnumerator;
//    end;
//  until not Result;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueCollection'}

constructor TMultiMapBase<TKey, TValue>.TValueCollection.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fSource.ContainsValue(value);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fSource.fCount;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumerator.Create(fSource);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fCount = 0;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  i, index: Integer;
  item: PItem;
begin
  SetLength(Result, fSource.fCount);
  index := 0;
  item := PItem(fSource.fHashTable.Items);
  for i := 1 to fSource.fHashTable.ItemCount do
  begin
    if item.HashCode <> RemovedFlag then
    begin
      item.Values.CopyTo(Result, index);
      Inc(index, item.Values.Count);
    end;
    Inc(item);
  end;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TWrappedCollection'}

constructor TMultiMapBase<TKey, TValue>.TWrappedCollection.Create(
  const key: TKey; const map: TMultiMapBase<TKey, TValue>;
  const delegate: ICollection<TValue>);
begin
  inherited Create;
  fKey := key;
  fMap := map;
  fDelegate := delegate;
  fMap.fOnDestroy.Add(HandleDestroy);
end;

destructor TMultiMapBase<TKey, TValue>.TWrappedCollection.Destroy;
begin
  if Assigned(fMap) then
    fMap.fOnDestroy.Remove(HandleDestroy);
  inherited Destroy;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.Contains(
  const value: TValue; const comparer: IEqualityComparer<TValue>): Boolean;
begin
  RefreshIfEmpty;
  Result := fDelegate.Contains(value, comparer);
end;

procedure TMultiMapBase<TKey, TValue>.TWrappedCollection.RefreshIfEmpty;
var
  newDelegate: IReadOnlyCollection<TValue>;
begin
  if fDelegate.IsEmpty and Assigned(fMap) then
    if fMap.TryGetValues(fKey, newDelegate) then
      fDelegate := newDelegate as ICollection<TValue>;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fDelegate.Count;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.GetEnumerator: IEnumerator<TValue>;
begin
  RefreshIfEmpty;
  Result := TWrappedEnumerator.Create(Self);
end;

procedure TMultiMapBase<TKey, TValue>.TWrappedCollection.HandleDestroy(
  Sender: TObject);
begin
  fMap := nil;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.ToArray: TArray<TValue>;
begin
  RefreshIfEmpty;
  Result := fDelegate.ToArray;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TWrappedEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TWrappedEnumerator.Create(
  const source: TWrappedCollection);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fOriginal := fSource.fDelegate;
  fDelegate := fSource.fDelegate.GetEnumerator;
end;

destructor TMultiMapBase<TKey, TValue>.TWrappedEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TMultiMapBase<TKey, TValue>.TWrappedEnumerator.GetCurrent: TValue;
begin
  ValidateEnumerator;
  Result := fDelegate.Current;
end;

function TMultiMapBase<TKey, TValue>.TWrappedEnumerator.MoveNext: Boolean;
begin
  ValidateEnumerator;
  Result := fDelegate.MoveNext;
end;

procedure TMultiMapBase<TKey, TValue>.TWrappedEnumerator.ValidateEnumerator;
begin
  fSource.RefreshIfEmpty;
  if fSource.fDelegate <> fOriginal then
    raise Error.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TListMultiMap<TKey, TValue>'}

function TListMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

function TListMultiMap<TKey, TValue>.CreateCollection: ICollection<TValue>;
begin
  Result := TCollections.CreateList<TValue>;
end;

{$ENDREGION}


{$REGION 'THashMultiMap<TKey, TValue>'}

constructor THashMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  inherited Create(keyComparer, ownerships);
  fValueComparer := valueComparer;
end;

function THashMultiMap<TKey, TValue>.CreateCollection: ICollection<TValue>;
begin
  Result := TCollections.CreateSet<TValue>(fValueComparer);
end;

function THashMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TTreeMultiMap<TKey, TValue>'}

constructor TTreeMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>; ownerships: TDictionaryOwnerships);
begin
  inherited Create(keyComparer, ownerships);
  fValueComparer := valueComparer;
end;

function TTreeMultiMap<TKey, TValue>.CreateCollection: ICollection<TValue>;
begin
  Result := TCollections.CreateSortedSet<TValue>(fValueComparer);
end;

function TTreeMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


end.
