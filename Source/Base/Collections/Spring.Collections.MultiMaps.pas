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
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Dictionaries;

type
  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private
  {$REGION 'Nested Types'}
    type
      TKeyValuePair = Generics.Collections.TPair<TKey, TValue>;

      TEnumerator = class(TInterfacedObject, IEnumerator<TKeyValuePair>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
        fDictionaryEnumerator: IEnumerator<TPair<TKey, IList<TValue>>>;
        fCollectionEnumerator: IEnumerator<TValue>;
        function GetCurrent: TKeyValuePair;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TValueEnumerator = class(TInterfacedObject, IEnumerator<TValue>)
      private
        {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
        fSource: TMultiMapBase<TKey, TValue>;
        fSourceEnumerator: IEnumerator<TKeyValuePair>;
        function GetCurrent: TValue;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        destructor Destroy; override;
        function MoveNext: Boolean;
      end;

      TValueCollection = class(TContainedReadOnlyCollection<TValue>,
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

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean;
        function ToArray: TArray<TValue>;
      {$ENDREGION}
      end;
  {$ENDREGION}
  private
    fDictionary: TDictionary<TKey, IList<TValue>>;
    fCount: Integer;
    fValues: TValueCollection;
    fValueComparer: IComparer<TValue>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItems(const key: TKey): IReadOnlyList<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    function CreateCollection(const comparer: IComparer<TValue>): IList<TValue>; virtual; abstract;
    function CreateDictionary(const comparer: IEqualityComparer<TKey>): TDictionary<TKey, IList<TValue>>; virtual; abstract;
  public
    constructor Create; override;
    constructor Create(const keyComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>); overload;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
    function Contains(const value: TKeyValuePair;
      const comparer: IEqualityComparer<TKeyValuePair>): Boolean; overload;
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
    procedure AddRange(const key: TKey; const collection: IEnumerable<TValue>); overload;
    function ExtractValues(const key: TKey): IList<TValue>;
    function TryGetValues(const key: TKey; out values: IReadOnlyList<TValue>): Boolean;
    property Items[const key: TKey]: IReadOnlyList<TValue> read GetItems; default;
  {$ENDREGION}
  end;

  TMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>,
    IEnumerable<TPair<TKey, TValue>>,
    ICollection<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IMap<TKey, TValue>, IReadOnlyMap<TKey, TValue>,
    IMultiMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>)
  private
    fOwnerships: TDictionaryOwnerships;
    function AsReadOnlyMultiMap: IReadOnlyMultiMap<TKey,TValue>;
    procedure DoKeyChanged(Sender: TObject; const Item: TKey;
      Action: TCollectionChangedAction);
    procedure DoValueChanged(Sender: TObject; const Item: TValue;
      Action: TCollectionChangedAction);
    procedure DoValuesChanged(Sender: TObject; const Item: IList<TValue>;
      Action: TCollectionChangedAction);
  protected
    function CreateCollection(const comparer: IComparer<TValue>): IList<TValue>; override;
    function CreateDictionary(const comparer: IEqualityComparer<TKey>): TDictionary<TKey, IList<TValue>>; override;
  public
    constructor Create(ownerships: TDictionaryOwnerships); overload;
    constructor Create(ownerships: TDictionaryOwnerships;
      const keyComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ownerships: TDictionaryOwnerships;
      const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>); overload;
  end;

implementation

uses
  Classes,
  TypInfo,
  Spring,
  Spring.Events.Base,
  Spring.ResourceStrings;


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>);
begin
  Create(keyComparer, nil);
end;

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>);
begin
  inherited Create;
  fDictionary := CreateDictionary(keyComparer);
  fValues := TValueCollection.Create(Self);
  fValueComparer := valueComparer;
end;

destructor TMultiMapBase<TKey, TValue>.Destroy;
begin
  fValues.Free;
  fDictionary.Free;
  inherited Destroy;
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

procedure TMultiMapBase<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const value: TKeyValuePair;
  const comparer: IEqualityComparer<TKeyValuePair>): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(value.key, list)
    and list.Contains(value.Value);
end;

function TMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  values: IReadOnlyList<TValue>;
begin
  Result := TryGetValues(key, values) and values.Contains(value);
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  list: ICollection<TValue>;
begin
  for list in fDictionary.Values do
    if list.Contains(value) then
      Exit(True);
  Result := False;
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  list: IList<TValue>;
begin
  Result.Key := key;
  if fDictionary.TryGetValue(key, list) then
    Result.Value := list.Extract(value)
  else
    Result.Value := Default(TValue);
end;

function TMultiMapBase<TKey, TValue>.ExtractValues(
  const key: TKey): IList<TValue>;
begin
  if not fDictionary.TryExtract(key, Result) then
    raise Error.KeyNotFound;

  Dec(fCount, Result.Count);
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
  const key: TKey): IReadOnlyList<TValue>;
var
  list: IList<TValue>;
begin
  if fDictionary.TryGetValue(key, list) then
    Result := list.AsReadOnlyList
  else
    Result := TEnumerable.Empty<TValue>;
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fDictionary.Keys;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list) and list.Remove(value);
  if Result then
  begin
    Dec(fCount);
    if not list.Any then
      fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list);
  if Result then
  begin
    Dec(fCount, list.Count);
    fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  list: IList<TValue>;
begin
  if not fDictionary.TryGetValue(key, list) then
  begin
    list := CreateCollection(fValueComparer);
    fDictionary[key] := list;
  end;

  list.Add(value);
  Inc(fCount);
  Result := True;
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  out values: IReadOnlyList<TValue>): Boolean;
var
  list: IList<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list);
  if Result then
    values := list as IReadOnlyList<TValue>;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
  fSource._AddRef;
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
  Result := fSourceEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueCollection'}

constructor TMultiMapBase<TKey, TValue>.TValueCollection.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create(source);
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
  Result := TValueEnumerator.Create(fSource);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := fSource.fCount = 0;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  list: ICollection<TValue>;
  i: Integer;
begin
  SetLength(Result, fSource.fCount);
  i := 0;
  for list in fSource.fDictionary.Values do
  begin
    list.CopyTo(Result, i);
    Inc(i, list.Count);
  end;
end;

{$ENDREGION}


{$REGION 'TMultiMap<TKey, TValue>'}

constructor TMultiMap<TKey, TValue>.Create(ownerships: TDictionaryOwnerships);
begin
  Create(ownerships, nil, nil);
end;

constructor TMultiMap<TKey, TValue>.Create(ownerships: TDictionaryOwnerships;
  const keyComparer: IEqualityComparer<TKey>);
begin
  Create(ownerships, keyComparer, nil);
end;

constructor TMultiMap<TKey, TValue>.Create(ownerships: TDictionaryOwnerships;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>);
begin
  if doOwnsKeys in ownerships then
    if TType.Kind<TKey> <> tkClass then
      raise Error.NoClassType(KeyType);

  if doOwnsValues in ownerships then
    if TType.Kind<TValue> <> tkClass then
      raise Error.NoClassType(ValueType);

  inherited Create(keyComparer, valueComparer);
  fOwnerships := ownerships;
end;

function TMultiMap<TKey, TValue>.CreateCollection(
  const comparer: IComparer<TValue>): IList<TValue>;
begin
  Result := TCollections.CreateList<TValue>(comparer);
end;

function TMultiMap<TKey, TValue>.AsReadOnlyMultiMap: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

function TMultiMap<TKey, TValue>.CreateDictionary(
  const comparer: IEqualityComparer<TKey>): TDictionary<TKey, IList<TValue>>;
begin
  Result := TContainedDictionary<TKey, IList<TValue>>.Create(Self, comparer);
  Result.fOnKeyChanged.Add(DoKeyChanged);
  Result.fOnValueChanged.Add(DoValuesChanged);
end;

procedure TMultiMap<TKey, TValue>.DoKeyChanged(Sender: TObject;
  const Item: TKey; Action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);

{$IFDEF DELPHIXE7_UP}
  if TType.Kind<TKey> = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

procedure TMultiMap<TKey, TValue>.DoValueChanged(Sender: TObject;
  const Item: TValue; Action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);

{$IFDEF DELPHIXE7_UP}
  if TType.Kind<TValue> = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

procedure TMultiMap<TKey, TValue>.DoValuesChanged(Sender: TObject;
  const Item: IList<TValue>; Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: Item.OnChanged.Add(DoValueChanged);
    caRemoved:
    begin
      Item.Clear;
      Item.OnChanged.Remove(DoValueChanged);
    end;
    caExtracted: Item.OnChanged.Remove(DoValueChanged);
  end;
end;

{$ENDREGION}


end.
