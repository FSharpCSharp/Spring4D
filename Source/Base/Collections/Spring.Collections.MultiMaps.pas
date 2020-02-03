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

  TValueCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fCount: PInteger;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  {$ENDREGION}
  public
    constructor Create(const source: TRefCountedObject;
      hashTable: PHashTable; count: PInteger);

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}
  end;

  TMultiMapEnumerator = class(TRefCountedObject)
  private
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fIndex: Integer;
    fVersion: Integer;
    fEnumerator: IInterface;
  public
    constructor Create(const source: TRefCountedObject; hashTable: PHashTable);
    destructor Destroy; override;
    function MoveNext: Boolean;
  end;

  TValueEnumerator<T> = class(TMultiMapEnumerator, IEnumerator<T>)
  private
    fCurrent: T;
    function GetCurrent: T;
    function MoveNext: Boolean;
  end;

  TUpdateValues = procedure(const key; var values: IInterface) of object;

  TWrappedCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private
  {$REGION 'Nested Types'}
    type
      TEnumerator = class(TRefCountedObject, IEnumerator<T>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TWrappedCollection<T>;
        fDelegate: IEnumerator<T>;
        fOriginal: ICollection<T>;
        procedure ValidateEnumerator;
        function GetCurrent: T;
      public
        constructor Create(const source: TWrappedCollection<T>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;
  {$ENDREGION}
  private
    fOnDestroy: TNotifyEventImpl;
    fUpdateValues: TUpdateValues;
    fDelegate: ICollection<T>;
    fKey: Pointer;
    function GetCount: Integer;
    procedure RefreshIfEmpty;
    procedure HandleDestroy(Sender: TObject);
  public
    constructor Create(key: Pointer;
      const onDestroy: TNotifyEventImpl;
      const updateValues: TUpdateValues;
      const delegate: ICollection<T>);
    destructor Destroy; override;
    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = TPair<TKey, TValue>;
      TItem = TMultiMapItem<TKey, TValue>;
      TItems = TArray<TItem>;
      PItem = ^TItem;

      TEnumerator = class(TMultiMapEnumerator, IEnumerator<TKeyValuePair>)
      private
        fCurrent: TKeyValuePair;
        function GetCurrent: TKeyValuePair;
        function MoveNext: Boolean;
      end;

      TKeyCollection = TInnerCollection<TKey>;
      TValueCollection = TValueCollection<TValue>;

      TWrappedCollection = class(TWrappedCollection<TValue>)
      private
        fKey: TKey;
      public
        constructor Create(const key: TKey;
          const onDestroy: TNotifyEventImpl;
          const updateValues: TUpdateValues;
          const delegate: ICollection<TValue>);
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
    class function EqualsThunk(instance: Pointer; const left, right): Boolean; static;
    procedure UpdateValues(const key; var values: IInterface);
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
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships);
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
      ownerships: TDictionaryOwnerships);
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
      ownerships: TDictionaryOwnerships);
  end;

  TFoldedListMultiMap<TKey, TValue> = class(TListMultiMap<TKey, TValue>)
  private
    fElementType: PTypeInfo;
    fKeyType: PTypeInfo;
    fValueType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
    function GetKeyType: PTypeInfo; override;
    function GetValueType: PTypeInfo; override;
  public
    constructor Create(keyType, valueType, elementType: PTypeInfo;
      const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships);
  end;

implementation

uses
  Classes,
  TypInfo,
  Spring.ResourceStrings;


{$REGION 'TValueCollection<T>'}

constructor TValueCollection<T>.Create(
  const source: TRefCountedObject; hashTable: PHashTable; count: PInteger);
begin
  inherited Create;
  fSource := source;
  fHashTable := hashTable;
  fCount := count;
end;

function TValueCollection<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize: Integer;
begin
  hashTable := fHashTable;
  item := hashTable.Items;
  itemCount := hashTable.ItemCount;
  itemSize := hashTable.ItemSize;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
      if ICollection<T>(PPointer(item + itemSize - SizeOf(Pointer))^).Contains(value, comparer) then
        Exit(True);
    Inc(item, itemSize);
    Dec(itemCount);
  end;
  Result := False;
end;

function TValueCollection<T>.GetCount: Integer;
begin
  Result := fCount^;
end;

function TValueCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TValueEnumerator<T>.Create(fSource, fHashTable);
end;

function TValueCollection<T>.GetIsEmpty: Boolean;
begin
  Result := fCount^ = 0;
end;

function TValueCollection<T>.ToArray: TArray<T>;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize, targetIndex, offset: Integer;
  collection: Pointer;
begin
  hashTable := fHashTable;
  SetLength(Result, fCount^);
  item := hashTable.Items;
  itemCount := hashTable.ItemCount;
  itemSize := hashTable.ItemSize;
  targetIndex := 0;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
    begin
      collection := PPointer(item + itemSize - SizeOf(Pointer))^;
      ICollection<T>(collection).CopyTo(Result, targetIndex);
      Inc(targetIndex, ICollection<T>(collection).Count);
    end;
    Inc(item, itemSize);
    Dec(itemCount);
  end;
end;

function TValueCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TValueCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TMultiMapEnumerator'}

constructor TMultiMapEnumerator.Create(const source: TRefCountedObject;
  hashTable: PHashTable);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fHashTable := hashTable;
  fVersion := fHashTable.Version;
end;

destructor TMultiMapEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TMultiMapEnumerator.MoveNext: Boolean;
var
  item: PByte;
begin
  if fVersion = fHashTable.Version then
  begin
    while True do
    begin
      if Assigned(fEnumerator) then
        if IEnumerator(fEnumerator).MoveNext then
          Exit(True)
        else
          fEnumerator := nil;

      if fIndex >= fHashTable.ItemCount then
        Break;

      item := fHashTable.Items + fIndex * fHashTable.ItemSize;
      Inc(fIndex);
      if PInteger(item)^ >= 0 then
        fEnumerator := IEnumerable(PPointer(item + fHashTable.ItemSize - SizeOf(Pointer))^).GetEnumerator;
    end;
    Exit(False);
  end
  else
    raise Error.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TValueEnumerator<T>'}

function TValueEnumerator<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TValueEnumerator<T>.MoveNext: Boolean;
begin
  Result := inherited MoveNext;
  if Result then
    fCurrent := IEnumerator<T>(fEnumerator).Current;
end;

{$ENDREGION}


{$REGION 'TWrappedCollection<T>'}

constructor TWrappedCollection<T>.Create(key: Pointer;
  const onDestroy: TNotifyEventImpl; const updateValues: TUpdateValues;
  const delegate: ICollection<T>);
begin
  inherited Create;
  fOnDestroy := onDestroy;
  fOnDestroy.Add(HandleDestroy);
  fUpdateValues := updateValues;
  fDelegate := delegate;
  fKey := key;
end;

destructor TWrappedCollection<T>.Destroy;
begin
  if Assigned(fOnDestroy) then
    fOnDestroy.Remove(HandleDestroy);
  inherited Destroy;
end;

function TWrappedCollection<T>.Contains(
  const value: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  RefreshIfEmpty;
  Result := fDelegate.Contains(value, comparer);
end;

procedure TWrappedCollection<T>.RefreshIfEmpty;
begin
  if fDelegate.IsEmpty and Assigned(fUpdateValues) then
    fUpdateValues(fKey^, IInterface(fDelegate));
end;

function TWrappedCollection<T>.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fDelegate.Count;
end;

function TWrappedCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  RefreshIfEmpty;
  Result := TEnumerator.Create(Self);
end;

procedure TWrappedCollection<T>.HandleDestroy(
  Sender: TObject);
begin
  fOnDestroy := nil;
  fUpdateValues := nil;
end;

function TWrappedCollection<T>.ToArray: TArray<T>;
begin
  RefreshIfEmpty;
  Result := fDelegate.ToArray;
end;

{$ENDREGION}


{$REGION 'TWrappedCollection<T>.TEnumerator'}

constructor TWrappedCollection<T>.TEnumerator.Create(
  const source: TWrappedCollection<T>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fOriginal := fSource.fDelegate;
  fDelegate := fSource.fDelegate.GetEnumerator;
end;

destructor TWrappedCollection<T>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TWrappedCollection<T>.TEnumerator.GetCurrent: T;
begin
  ValidateEnumerator;
  Result := fDelegate.Current;
end;

function TWrappedCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  ValidateEnumerator;
  Result := fDelegate.MoveNext;
end;

procedure TWrappedCollection<T>.TEnumerator.ValidateEnumerator;
begin
  fSource.RefreshIfEmpty;
  if fSource.fDelegate <> fOriginal then
    raise Error.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  if doOwnsKeys in ownerships then
    if KeyType.Kind <> tkClass then
      raise Error.NoClassType(KeyType);

  if doOwnsValues in ownerships then
    if ValueType.Kind <> tkClass then
      raise Error.NoClassType(ValueType);

  inherited Create;
  fOwnerships := ownerships;
  if Assigned(keyComparer) then
    fKeyComparer := keyComparer
  else
    fKeyComparer := IEqualityComparer<TKey>(_LookupVtableInfo(giEqualityComparer, KeyType, SizeOf(TKey)));

  fKeys := TKeyCollection.Create(Self, @fHashTable, KeyType, fKeyComparer, 0);
  fValues := TValueCollection.Create(Self, @fHashTable, @fCount);

  fHashTable.Initialize(TypeInfo(TItems), @EqualsThunk, fKeyComparer);

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

class function TMultiMapBase<TKey, TValue>.EqualsThunk(instance: Pointer; const left, right): Boolean;
begin
  Result := TEqualsMethod<TKey>(instance^)(TKey(left), TKey(right));
end;

procedure TMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    FreeObject(item);
end;

procedure TMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
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
  Result := TEnumerator.Create(Self, @fHashTable);
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
    Result := TWrappedCollection.Create(key, fOnDestroy, UpdateValues, CreateCollection);
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

procedure TMultiMapBase<TKey, TValue>.UpdateValues(const key;
  var values: IInterface);
var
  entry: THashTableEntry;
begin
  entry.HashCode := fKeyComparer.GetHashCode(TKey(key));
  if fHashTable.Find(key, entry) then
    values := TItems(fHashTable.Items)[entry.ItemIndex].Values;
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

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result := fCurrent;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  Result := inherited MoveNext;
  if Result then
  begin
    fCurrent.Key := TItems(fHashTable.Items)[fIndex - 1].Key;
    fCurrent.Value := IEnumerator<TValue>(fEnumerator).Current;
  end;
end;

{$ENDREGION}


{$REGION 'TListMultiMap<TKey, TValue>'}

function TListMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

function TListMultiMap<TKey, TValue>.CreateCollection: ICollection<TValue>;
begin
  Result := TCollections.CreateList<TValue>(IComparer<TValue>(_LookupVtableInfo(giComparer, ValueType, SizeOf(TValue))));
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


{$REGION 'TMultiMapBase<TKey, TValue>.TWrappedCollection'}

constructor TMultiMapBase<TKey, TValue>.TWrappedCollection.Create(
  const key: TKey; const onDestroy: TNotifyEventImpl;
  const updateValues: TUpdateValues; const delegate: ICollection<TValue>);
begin
  inherited Create(@fKey, onDestroy, updateValues, delegate);
  fKey := key;
end;

{$ENDREGION}


{$REGION 'TFoldedListMultiMap<TKey, TValue>'}

constructor TFoldedListMultiMap<TKey, TValue>.Create(keyType,
  valueType, elementType: PTypeInfo; const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
  inherited Create(keyComparer, ownerships);
end;

function TFoldedListMultiMap<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


end.
