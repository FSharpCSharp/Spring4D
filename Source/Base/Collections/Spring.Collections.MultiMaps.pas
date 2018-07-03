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

unit Spring.Collections.MultiMaps;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Dictionaries;

type
  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>,
    IMultiMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>)
  private
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = Generics.Collections.TPair<TKey, TValue>;
      TMultiMapEntry = TMultiMapEntry<TKey, TValue>;

      TEnumerator = class(TEnumeratorBase<TKeyValuePair>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
        fVersion: Integer;
        fDictionaryEnumerator: IEnumerator<TPair<TKey, ICollection<TValue>>>;
        fCollectionEnumerator: IEnumerator<TValue>;
      protected
        function GetCurrent: TKeyValuePair; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;

      TValueEnumerator = class(TEnumeratorBase<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
        fVersion: Integer;
        fSourceEnumerator: IEnumerator<TKeyValuePair>;
      protected
        function GetCurrent: TValue; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;

      TValueCollection = class(TContainedReadOnlyCollection<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fOwner: TMultiMapBase<TKey, TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const owner: TMultiMapBase<TKey, TValue>);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;

      TEntryEnumerator = class(TEnumeratorBase<TMultiMapEntry>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
        fEnumerator: IEnumerator<TPair<TKey, ICollection<TValue>>>;
        fVersion: Integer;
      protected
        function GetCurrent: TMultiMapEntry; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;

      TEntryCollection = class(TContainedReadOnlyCollection<TMultiMapEntry>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);

      {$REGION 'Implements IEnumerable<TMultiMapEntry>'}
        function GetEnumerator: IEnumerator<TMultiMapEntry>; override;
      {$ENDREGION}
      end;

      TWrappedCollection = class(TEnumerableBase<TValue>, IReadOnlyCollection<TValue>)
      private
        fMap: Weak<TDictionary<TKey, ICollection<TValue>>>;
        fKey: TKey;
        fDelegate: ICollection<TValue>;
        procedure RefreshIfEmpty;
      protected
        function GetCount: Integer; override;
      public
        constructor Create(const key: TKey;
          const map: TDictionary<TKey, ICollection<TValue>>;
          const delegate: ICollection<TValue>);
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function GetEnumerator: IEnumerator<TValue>; override;
        function ToArray: TArray<TValue>; override;
      end;
  {$ENDREGION}
  private
    fDictionary: TDictionary<TKey, ICollection<TValue>>;
    fValues: TValueCollection;
    fEntries: TEntryCollection;
    fOwnerships: TDictionaryOwnerships;
    fCount: Integer;
    fVersion: Integer;
    function AsReadOnlyMultiMap: IReadOnlyMultiMap<TKey,TValue>;
    procedure DoKeyChanged(Sender: TObject; const Item: TKey;
      Action: TCollectionChangedAction);
    procedure DoValueChanged(Sender: TObject; const Item: TValue;
      Action: TCollectionChangedAction);
    procedure DoValuesChanged(Sender: TObject; const Item: ICollection<TValue>;
      Action: TCollectionChangedAction);
    function GetEntries: IReadOnlyCollection<TMultiMapEntry>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
  {$ENDREGION}
    function CreateCollection: ICollection<TValue>; virtual; abstract;
    function CreateDictionary(const comparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships): TDictionary<TKey, ICollection<TValue>>;
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); override;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); override;
    function TryAddInternal(const key: TKey; const value: TValue): Boolean; override;
  public
    constructor Create; override;
    constructor Create(ownerships: TDictionaryOwnerships); overload;
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships = []); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>; override;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload; override;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; override;
    function Contains(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const collection: IEnumerable<TValue>); overload;
    function ExtractValues(const key: TKey): ICollection<TValue>;
    function TryGetValues(const key: TKey; out values: IReadOnlyCollection<TValue>): Boolean;
    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  {$ENDREGION}
  end;

  TListMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>)
  protected
    function CreateCollection: ICollection<TValue>; override;
  end;

  THashMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>)
  private
    fValueComparer: IEqualityComparer<TValue>;
  protected
    function CreateCollection: ICollection<TValue>; override;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships = []); overload;
  end;

  TTreeMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>)
  private
    fValueComparer: IComparer<TValue>;
  protected
    function CreateCollection: ICollection<TValue>; override;
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
  fDictionary := CreateDictionary(keyComparer, ownerships - [doOwnsValues]);
  fValues := TValueCollection.Create(Self);
  fEntries := TEntryCollection.Create(Self);
  fOwnerships := ownerships;
end;

destructor TMultiMapBase<TKey, TValue>.Destroy;
begin
  fEntries.Free;
  fValues.Free;
  fDictionary.Free;
  inherited Destroy;
end;

function TMultiMapBase<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAddInternal(key, value);
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
  const collection: IEnumerable<TValue>);
var
  item: TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
    Add(key, item);
end;

function TMultiMapBase<TKey, TValue>.AsReadOnlyMultiMap: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
begin
  IncUnchecked(fVersion);
  fDictionary.Clear;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  values: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(value.key, values)
    and values.Contains(value.Value);
end;

function TMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  values: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, values) and values.Contains(value);
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  values: ICollection<TValue>;
begin
  for values in fDictionary.Values do
    if values.Contains(value) then
      Exit(True);
  Result := False;
end;

function TMultiMapBase<TKey, TValue>.CreateDictionary(
  const comparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): TDictionary<TKey, ICollection<TValue>>;
begin
  Result := TContainedDictionary<TKey, ICollection<TValue>>.Create(Self, comparer, ownerships);
  Result.OnKeyChanged.Add(DoKeyChanged);
  Result.OnValueChanged.Add(DoValuesChanged);
end;

procedure TMultiMapBase<TKey, TValue>.DoKeyChanged(Sender: TObject;
  const Item: TKey; Action: TCollectionChangedAction);
begin
  KeyChanged(Item, Action);
end;

procedure TMultiMapBase<TKey, TValue>.DoValueChanged(Sender: TObject;
  const Item: TValue; Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Inc(fCount);
    caRemoved, caExtracted: Dec(fCount);
  end;
  ValueChanged(Item, Action);
end;

procedure TMultiMapBase<TKey, TValue>.DoValuesChanged(Sender: TObject;
  const Item: ICollection<TValue>; Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Item.OnChanged.Add(DoValueChanged);
    caRemoved:
    begin
      Item.Clear;
      Item.OnChanged.Remove(DoValueChanged);
    end;
  end;
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  values: ICollection<TValue>;
begin
  Result.Key := key;
  if fDictionary.TryGetValue(key, values) then
  begin
    IncUnchecked(fVersion);
    Result.Value := values.Extract(value);
  end
  else
    Result.Value := Default(TValue);
end;

function TMultiMapBase<TKey, TValue>.ExtractValues(
  const key: TKey): ICollection<TValue>;
var
  values: ICollection<TValue>;
begin
  if not fDictionary.TryExtract(key, values) then
    raise Error.KeyNotFound;

  IncUnchecked(fVersion);
  Result := TCollections.CreateList<TValue>;
  values.MoveTo(Result);
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEntries: IReadOnlyCollection<TMultiMapEntry>;
begin
  Result := fEntries;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  Result := TEnumerator.Create(Self);
end;

function TMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  items: ICollection<TValue>;
begin
  if not fDictionary.TryGetValue(key, items) then
    items := CreateCollection;
  Result := TWrappedCollection.Create(key, fDictionary, items) as IReadOnlyCollection<TValue>
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fDictionary.Keys;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

procedure TMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
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

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  values: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, values) and values.Remove(value);
  if Result then
  begin
    IncUnchecked(fVersion);
    if not values.Any then
      fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  values: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, values);
  if Result then
  begin
    IncUnchecked(fVersion);
    values.Clear;
    fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.TryAddInternal(const key: TKey;
  const value: TValue): Boolean;
var
  values: ICollection<TValue>;
begin
  if not fDictionary.TryGetValue(key, values) then
  begin
    values := CreateCollection;
    fDictionary[key] := values;
  end;

  IncUnchecked(fVersion);
  Result := values.Add(value);
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  out values: IReadOnlyCollection<TValue>): Boolean;
var
  items: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, items);
  if Result then
    values := items as IReadOnlyCollection<TValue>;
end;

procedure TMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
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


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TMultiMapBase<TKey, TValue>.TEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(fDictionaryEnumerator, 'dictionaryEnumerator');
  Guard.CheckNotNull(fCollectionEnumerator, 'collectionEnumerator');
{$ENDIF}

  Result.Key := fDictionaryEnumerator.Current.Key;
  Result.Value := fCollectionEnumerator.Current;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fDictionaryEnumerator) then
    fDictionaryEnumerator := fSource.fDictionary.GetEnumerator;

  if fVersion <> fSource.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  repeat
    if Assigned(fCollectionEnumerator) and fCollectionEnumerator.MoveNext then
      Exit(True)
    else
    begin
      Result := fDictionaryEnumerator.MoveNext;
      if Result then
        fCollectionEnumerator := fDictionaryEnumerator.Current.Value.GetEnumerator;
    end;
  until not Result;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TValueEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TMultiMapBase<TKey, TValue>.TValueEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TMultiMapBase<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := fSourceEnumerator.Current.Value;
end;

function TMultiMapBase<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fSourceEnumerator) then
    fSourceEnumerator := fSource.GetEnumerator;

  if fVersion <> fSource.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  Result := fSourceEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueCollection'}

constructor TMultiMapBase<TKey, TValue>.TValueCollection.Create(
  const owner: TMultiMapBase<TKey, TValue>);
begin
  inherited Create(owner);
  fOwner := owner;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fOwner.ContainsValue(value);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fOwner.Count;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(fOwner);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  list: ICollection<TValue>;
  i: Integer;
begin
  SetLength(Result, fOwner.Count);
  i := 0;
  for list in fOwner.fDictionary.Values do
  begin
    list.CopyTo(Result, i);
    Inc(i, list.Count);
  end;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEntryEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TEntryEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
  fVersion := fSource.fVersion;
end;

destructor TMultiMapBase<TKey, TValue>.TEntryEnumerator.Destroy;
begin
  fSource._Release;
  inherited;
end;

function TMultiMapBase<TKey, TValue>.TEntryEnumerator.GetCurrent: TMultiMapEntry;
var
  current: TPair<TKey, ICollection<TValue>>;
begin
  current := fEnumerator.Current;
  Result.Key := current.Key;
  Result.Values := current.Value as IReadOnlyCollection<TValue>;
end;

function TMultiMapBase<TKey, TValue>.TEntryEnumerator.MoveNext: Boolean;
begin
  if fVersion <> fSource.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if not Assigned(fEnumerator) then
    fEnumerator := fSource.fDictionary.GetEnumerator;

  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEntryCollection'}

constructor TMultiMapBase<TKey, TValue>.TEntryCollection.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create(source);
  fSource := source;
end;

function TMultiMapBase<TKey, TValue>.TEntryCollection.GetEnumerator: IEnumerator<TMultiMapEntry>;
begin
  Result := TEntryEnumerator.Create(fSource);
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TWrappedCollection'}

constructor TMultiMapBase<TKey, TValue>.TWrappedCollection.Create(
  const key: TKey; const map: TDictionary<TKey, ICollection<TValue>>;
  const delegate: ICollection<TValue>);
begin
  inherited Create;
  fKey := key;
  fMap := map;
  fDelegate := delegate;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.Contains(
  const value: TValue; const comparer: IEqualityComparer<TValue>): Boolean;
begin
  RefreshIfEmpty;
  Result := fDelegate.Contains(value, comparer);
end;

procedure TMultiMapBase<TKey, TValue>.TWrappedCollection.RefreshIfEmpty;
var
  newDelegate: ICollection<TValue>;
begin
  if fDelegate.IsEmpty and fMap.IsAlive then
    if fMap.Target.TryGetValue(fKey, newDelegate) then
      fDelegate := newDelegate;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fDelegate.Count;
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.GetEnumerator: IEnumerator<TValue>;
begin
  RefreshIfEmpty;
  Result := fDelegate.GetEnumerator; // TODO: implement TWrappedEnumerator
end;

function TMultiMapBase<TKey, TValue>.TWrappedCollection.ToArray: TArray<TValue>;
begin
  RefreshIfEmpty;
  Result := fDelegate.ToArray;
end;

{$ENDREGION}


{$REGION 'TListMultiMap<TKey, TValue>'}

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

{$ENDREGION}


end.
