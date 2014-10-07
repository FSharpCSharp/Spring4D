{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  ///	<summary>
  ///	  Represents a collection of keys and values.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of the keys in the dictionary.
  ///	</typeparam>
  ///	<typeparam name="TValue">
  ///	  The type of the values in the dictionary.
  ///	</typeparam>
  TDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IDictionary<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>)
  protected
    type
      TGenericDictionary = Generics.Collections.TDictionary<TKey, TValue>;
      TGenericPair = Generics.Collections.TPair<TKey, TValue>;

      TKeyCollection = class(TContainedReadOnlyCollection<TKey>)
      private
        fDictionary: TGenericDictionary;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TGenericDictionary);

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>; override;
        function Contains(const value: TKey;
          const comparer: IEqualityComparer<TKey>): Boolean; override;
        function ToArray: TArray<TKey>; override;
      {$ENDREGION}
      end;

      TValueCollection = class(TContainedReadOnlyCollection<TValue>)
      private
        fDictionary: TGenericDictionary;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TGenericDictionary);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;
  private
    fDictionary: TGenericDictionary;
    fOwnership: TOwnershipType;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fOnKeyNotify: TCollectionNotifyEvent<TKey>;
    fOnValueNotify: TCollectionNotifyEvent<TValue>;
    procedure DoKeyNotify(Sender: TObject; const Item: TKey; Action: TCollectionNotification);
    procedure DoValueNotify(Sender: TObject; const Item: TValue; Action: TCollectionNotification);
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; virtual;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); virtual;
  {$ENDREGION}
    procedure AddInternal(const item: TGenericPair); override;
  public
    constructor Create; overload; override;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(capacity: Integer; const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(dictionary: TGenericDictionary; ownership: TOwnershipType); overload;

    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TGenericPair>; override;
    function Contains(const value: TGenericPair;
      const comparer: IEqualityComparer<TGenericPair>): Boolean; override;
    function ToArray: TArray<TGenericPair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
    function Remove(const item: TGenericPair): Boolean; overload; override;
    function Extract(const item: TGenericPair): TGenericPair; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); reintroduce; overload;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; reintroduce; overload;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function ExtractPair(const key: TKey): TGenericPair;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}
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
    constructor Create(const controller: IInterface);
    property Controller: IInterface read GetController;
  end;

implementation

uses
  Spring.Collections.Extensions;


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(dictionary: TGenericDictionary;
  ownership: TOwnershipType);
begin
  inherited Create;
  fDictionary := dictionary;
  fKeys := TKeyCollection.Create(Self, fDictionary);
  fValues := TValueCollection.Create(Self, fDictionary);
  fOwnership := ownership;
  fOnKeyNotify := fDictionary.OnKeyNotify;
  fOnValueNotify := fDictionary.OnValueNotify;
  fDictionary.OnKeyNotify := DoKeyNotify;
  fDictionary.OnValueNotify := DoValueNotify;
end;

constructor TDictionary<TKey, TValue>.Create;
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create;
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer);
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create(capacity);
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(
  const comparer: IEqualityComparer<TKey>);
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create(comparer);
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer;
  const comparer: IEqualityComparer<TKey>);
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create(capacity, comparer);
  Create(dictionary, otOwned);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  fKeys.Free;
  fValues.Free;
  if fOwnership = otOwned then
    fDictionary.Free
  else
  begin
    fDictionary.OnKeyNotify := fOnKeyNotify;
    fDictionary.OnValueNotify := fOnValueNotify;
  end;

  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.DoKeyNotify(Sender: TObject;
  const Item: TKey; Action: TCollectionNotification);
begin
  inherited KeyChanged(Item, TCollectionChangedAction(action));
end;

procedure TDictionary<TKey, TValue>.DoValueNotify(Sender: TObject;
  const Item: TValue; Action: TCollectionNotification);
begin
  inherited ValueChanged(Item, TCollectionChangedAction(Action));
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TGenericPair>;
var
  dictionary: TEnumerable<TGenericPair>;
begin
  dictionary := TEnumerable<TGenericPair>(fDictionary);
  Result := TEnumeratorAdapter<TGenericPair>.Create(dictionary);
end;

procedure TDictionary<TKey, TValue>.AddInternal(const item: TGenericPair);
begin
  fDictionary.Add(item.Key, item.Value);
end;

procedure TDictionary<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
end;

function TDictionary<TKey, TValue>.Contains(const value: TGenericPair;
  const comparer: IEqualityComparer<TGenericPair>): Boolean;
var
  item: TValue;
begin
  Result := fDictionary.TryGetValue(value.Key, item);
  if Result then
    Result := comparer.Equals(TGenericPair.Create(value.Key, item), value);
end;

function TDictionary<TKey, TValue>.Remove(const item: TGenericPair): Boolean;
begin
  Result := Remove(item.Key, item.Value);
end;

function TDictionary<TKey, TValue>.Extract(
  const item: TGenericPair): TGenericPair;
var
  value: TValue;
  found: Boolean;
  comparer: IEqualityComparer<TValue>;
begin
  found := fDictionary.TryGetValue(item.Key, value);
  if found then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    found := comparer.Equals(value, item.Value);
    if found then
{$IFDEF DELPHIXE2_UP}
      Result := fDictionary.ExtractPair(item.Key);
{$ELSE}
    begin
      Result := item;
      fDictionary.ExtractPair(item.Key);
    end;
{$ENDIF}
  end;
  if not found then
  begin
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TGenericPair>;
{$IFDEF DELPHI2010}
var
  pair: TGenericPair;
  index: Integer;
begin
  SetLength(Result, fDictionary.Count);
  index := 0;
  for pair in fDictionary do
  begin
    Result[index] := pair;
    Inc(index);
  end;
{$ELSE}
begin
  Result := fDictionary.ToArray;
{$ENDIF}
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  fDictionary.Add(key, value);
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

function TDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TGenericPair;
begin
{$IFDEF DELPHIXE2_UP}
  Result := fDictionary.ExtractPair(key);
{$ELSE}
  if fDictionary.TryGetValue(key, Result.Value) then
  begin
    Result.Key := key;
    fDictionary.ExtractPair(key);
  end
  else
    Result := fDictionary.ExtractPair(key);
{$ENDIF}
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fDictionary.TryGetValue(key, value);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
  if Result then
    fDictionary.Remove(key);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.ContainsKey(key);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(fDictionary[key], value);
    if Result then
      fDictionary.Remove(key);
  end;
end;

function TDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := fDictionary[key];
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TKeyCollection'}

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(
  const controller: IInterface; const dictionary: TGenericDictionary);
begin
  inherited Create(controller);
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
  Result := TEnumeratorAdapter<TKey>.Create(fDictionary.Keys);
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TValueCollection'}

constructor TDictionary<TKey, TValue>.TValueCollection.Create(
  const controller: IInterface; const dictionary: TGenericDictionary);
begin
  inherited Create(controller);
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
  Result := TEnumeratorAdapter<TValue>.Create(fDictionary.Values);
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TContainedDictionary<TKey, TValue>'}

constructor TContainedDictionary<TKey, TValue>.Create(
  const controller: IInterface);
begin
  inherited Create;
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


end.
