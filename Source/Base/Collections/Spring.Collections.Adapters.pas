{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

{TODO -Paul -cGeneral : TListAdapter<T> Support IList}
{TODO -Paul -cGeneral : TDictionary<TKey, TValue> Support IDictionary}
{TODO -Paul -cGeneral : Add IOrderedDictionary}

unit Spring.Collections.Adapters;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  Spring.Collections;

type
  /// <summary>
  /// TEnumeratorAdapter<T>
  /// </summary>
  TEnumeratorAdapter<T> = class(TEnumeratorBase<T>, IEnumerator<T>, IEnumerator, IInterface)
  private
    fEnumerator: TEnumerator<T>;
  protected
    function DoGetCurrent: T; override;
  public
    constructor Create(collection: TEnumerable<T>);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    property Current: T read DoGetCurrent;
  end;

//  TListAdapter = class(TEnumerableEx<TValue>, IList, ICollection)
//  public
//    class function From<T>: TListAdapter;
//  end;

  /// <summary>
  /// TListAdapter<T>
  /// </summary>
  TListAdapter<T> = class(TEnumerableEx<T>, IList<T>, ICollection<T>)
  protected
    fList: TList<T>;
    fOwnership: TCollectionOwnership;
    function GetIsReadOnly: Boolean; virtual;
    function GetItem(index: Integer): T;
    procedure SetItem(index: Integer; const Value: T);
  protected
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(list: TList<T>; ownership: TCollectionOwnership = coReference);
    destructor Destroy; override;

    function Contains(const item: T): Boolean; override;
    function ToArray: TArray<T>; override;
    function ToList: IList<T>; override;

    procedure Add(const item: T);
    procedure Clear;
    procedure Insert(index: Integer; const item: T);
    procedure RemoveAt(index: Integer);
    function Extract(const item: T): T;
    function IndexOf(const item: T): Integer;
    function Remove(const item: T): Boolean;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  /// <summary>
  /// TDictionaryAdapter<TKey, TValue>
  /// </summary>
  TDictionaryAdapter<TKey, TValue> = class(TCollectionBase<TPair<TKey, TValue>>, IDictionary<TKey, TValue>)
  private
    type
      /// <summary>
      /// Provides a read-only ICollection<TKey> implementation
      /// </summary>
      TKeyCollection = class(TContainedEnumerableEx<TKey>, ICollection<TKey>,
        IEnumerableEx<TKey>, IEnumerable<TKey>, IEnumerable, IInterface)
      private
        fDictionary: TDictionary<TKey,TValue>;
      protected
        function GetCount: Integer; override;
        function GetIsEmpty: Boolean; override;
        function GetIsReadOnly: Boolean;
        function DoGetEnumerator: IEnumerator<TKey>; override;
      public
        constructor Create(const controller: IInterface; dictionary: TDictionary<TKey,TValue>);
        { IEnumerableEx<TKey> }
        function Contains(const item: TKey): Boolean; override;
        function ToArray: TArray<TKey>; override;
        { ICollection<TKey> }
        procedure Add(const item: TKey); overload;
        procedure Clear;
        function Remove(const item: TKey): Boolean; overload;
        function Extract(const item: TKey): TKey;
      end;

      /// <summary>
      /// Provides a read-only ICollection<TValue> implementation
      /// </summary>
      TValueCollection = class(TContainedEnumerableEx<TValue>, ICollection<TValue>,
        IEnumerableEx<TValue>, IEnumerable<TValue>, IEnumerable, IInterface)
      private
        fDictionary: TDictionary<TKey, TValue>;
        function GetIsReadOnly: Boolean;
      protected
        function GetCount: Integer; override;
        function GetIsEmpty: Boolean; override;
        function DoGetEnumerator: IEnumerator<TValue>; override;
      public
        constructor Create(const controller: IInterface; dictionary: TDictionary<TKey,TValue>);
        { IEnumerableEx<TValue> }
        function Contains(const item: TValue): Boolean; override;
        function ToArray: TArray<TValue>; override;
        { ICollection<TValue> }
        procedure Add(const item: TValue); overload;
        procedure Clear;
        function Remove(const item: TValue): Boolean; overload;
        function Extract(const item: TValue): TValue;
        property IsReadOnly: Boolean read GetIsReadOnly;
      end;
  private
    fDictionary: TDictionary<TKey,TValue>;
    fOwnership: TCollectionOwnership;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
  protected
    function DoGetEnumerator: IEnumerator<TPair<TKey,TValue>>; override;
  public
    constructor Create(dictionary: TDictionary<TKey,TValue>;
      ownership: TCollectionOwnership = coReference);
    destructor Destroy; override;

    { Implements IEnumerableEx<TPair<TKey, TValue>> }
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function Contains(const item: TPair<TKey,TValue>): Boolean; override;
    function ToArray: TArray<TPair<TKey,TValue>>; override;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;

    { Implements ICollection<TPair<TKey, TValue>> }
    procedure Add(const item: TPair<TKey,TValue>); overload; override;
    procedure Clear; override;
    function Remove(const item: TPair<TKey,TValue>): Boolean; overload; override;
    function Extract(const item: TPair<TKey,TValue>): TPair<TKey,TValue>;

    { Implements IDictionary<TKey,TValue> }
    function GetItem(const key: TKey): TValue;
    function GetKeys: ICollection<TKey>;
    function GetValues: ICollection<TValue>;
    procedure SetItem(const key: TKey; const value: TValue);
    procedure Add(const key: TKey; const value: TValue); reintroduce; overload;
    procedure Remove(const key: TKey); reintroduce; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ExtractPair(const key: TKey): TPair<TKey, TValue>;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
    property Keys: ICollection<TKey> read GetKeys;
    property Values: ICollection<TValue> read GetValues;
  end;

implementation

uses
  Spring.ResourceStrings;

{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(collection: TEnumerable<T>);
begin
  inherited Create;
  fEnumerator := collection.GetEnumerator;
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  fEnumerator.Free;
  inherited Destroy;
end;

function TEnumeratorAdapter<T>.DoGetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TListAdapter<T>'}

constructor TListAdapter<T>.Create(list: TList<T>;
  ownership: TCollectionOwnership);
begin
  inherited Create;
  fList := list;
  fOwnership := ownership;
end;

destructor TListAdapter<T>.Destroy;
begin
  if fOwnership = coOwned then
    fList.Free;
  inherited Destroy;
end;

procedure TListAdapter<T>.Add(const item: T);
begin
  fList.Add(item);
end;

procedure TListAdapter<T>.Clear;
begin
  fList.Clear;
end;

function TListAdapter<T>.Contains(const item: T): Boolean;
begin
  Result := fList.Contains(item);
end;

function TListAdapter<T>.IndexOf(const item: T): Integer;
begin
  Result := fList.IndexOf(item);
end;

procedure TListAdapter<T>.Insert(index: Integer; const item: T);
begin
  fList.Insert(index, item);
end;

function TListAdapter<T>.Remove(const item: T): Boolean;
begin
  Result := fList.Remove(item) <> -1;
end;

procedure TListAdapter<T>.RemoveAt(index: Integer);
begin
  fList.Delete(index);
end;

function TListAdapter<T>.Extract(const item: T): T;
begin
  Result := fList.Extract(item);
end;

function TListAdapter<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := Items[i];
  end;
end;

function TListAdapter<T>.ToList: IList<T>;
begin
  Result := Self;
end;

function TListAdapter<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fList);
end;

function TListAdapter<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TListAdapter<T>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TListAdapter<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

function TListAdapter<T>.GetItem(index: Integer): T;
begin
  Result := fList[index];
end;

procedure TListAdapter<T>.SetItem(index: Integer; const Value: T);
begin
  fList[index] := value;
end;

{$ENDREGION}


{$REGION 'TDictionaryAdapter<TKey, TValue>'}

constructor TDictionaryAdapter<TKey, TValue>.Create(
  dictionary: TDictionary<TKey, TValue>; ownership: TCollectionOwnership);
begin
  inherited Create;
  fDictionary := dictionary;
  fOwnership := ownership;
end;

destructor TDictionaryAdapter<TKey, TValue>.Destroy;
begin
  fKeys.Free;
  fValues.Free;
  if fOwnership = coOwned then
  begin
    fDictionary.Free;
  end;
  inherited Destroy;
end;

function TDictionaryAdapter<TKey, TValue>.DoGetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TEnumeratorAdapter<TPair<TKey, TValue>>.Create(fDictionary);
end;

procedure TDictionaryAdapter<TKey, TValue>.Add(const item: TPair<TKey, TValue>);
begin
  fDictionary.Add(item.Key, item.Value);
end;

procedure TDictionaryAdapter<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
end;

function TDictionaryAdapter<TKey, TValue>.Contains(
  const item: TPair<TKey, TValue>): Boolean;
var
  value: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.TryGetValue(item.Key, value);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(value, item.Value);
  end;
end;

function TDictionaryAdapter<TKey, TValue>.Remove(
  const item: TPair<TKey, TValue>): Boolean;
var
  value: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.TryGetValue(item.Key, value);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(value, item.Value);
    if Result then
    begin
      fDictionary.Remove(item.Key);
    end;
  end;
end;

function TDictionaryAdapter<TKey, TValue>.Extract(
  const item: TPair<TKey, TValue>): TPair<TKey, TValue>;
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
    begin
      Result := fDictionary.ExtractPair(item.Key);
    end;
  end;
  if not found then
  begin
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end;
end;

function TDictionaryAdapter<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  pair: TPair<TKey, TValue>;
  index: Integer;
begin
  SetLength(Result, fDictionary.Count);
  index := 0;
  for pair in fDictionary do
  begin
    Result[index].Key := pair.Key;
    Result[index].Value := pair.Value;
    Inc(index);
  end;
end;

function TDictionaryAdapter<TKey, TValue>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

function TDictionaryAdapter<TKey, TValue>.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TDictionaryAdapter<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  fDictionary.Add(key, value);
end;

function TDictionaryAdapter<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TDictionaryAdapter<TKey, TValue>.ExtractPair(
  const key: TKey): TPair<TKey, TValue>;
begin
  Result := fDictionary.ExtractPair(key);
end;

function TDictionaryAdapter<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fDictionary.TryGetValue(key, value);
end;

procedure TDictionaryAdapter<TKey, TValue>.Remove(const key: TKey);
begin
  fDictionary.Remove(key);
end;

function TDictionaryAdapter<TKey, TValue>.GetKeys: ICollection<TKey>;
begin
  if fKeys = nil then
  begin
    fKeys := TKeyCollection.Create(Self, fDictionary);
  end;
  Result := fKeys;
end;

function TDictionaryAdapter<TKey, TValue>.GetValues: ICollection<TValue>;
begin
  if fValues = nil then
  begin
    fValues := TValueCollection.Create(Self, fDictionary);
  end;
  Result := fValues;
end;

function TDictionaryAdapter<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := fDictionary[key];
end;

procedure TDictionaryAdapter<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TDictionaryAdapter<TKey, TValue>.TKeyCollection'}

constructor TDictionaryAdapter<TKey, TValue>.TKeyCollection.Create(
  const controller: IInterface; dictionary: TDictionary<TKey, TValue>);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.Contains(
  const item: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
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

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.DoGetEnumerator: IEnumerator<TKey>;
begin
  Result := TEnumeratorAdapter<TKey>.Create(fDictionary.Keys);
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

procedure TDictionaryAdapter<TKey, TValue>.TKeyCollection.Add(const item: TKey);
begin
  raise ENotSupportedException.Create('Add');
end;

procedure TDictionaryAdapter<TKey, TValue>.TKeyCollection.Clear;
begin
  raise ENotSupportedException.Create('Clear');
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.Extract(
  const item: TKey): TKey;
begin
  raise ENotSupportedException.Create('Extract');
end;

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.Remove(
  const item: TKey): Boolean;
begin
  raise ENotSupportedException.Create('Remove');
end;

{$ENDREGION}


{$REGION 'TDictionaryAdapter<TKey, TValue>.TValueCollection'}

constructor TDictionaryAdapter<TKey, TValue>.TValueCollection.Create(
  const controller: IInterface; dictionary: TDictionary<TKey, TValue>);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.Contains(
  const item: TValue): Boolean;
begin
  Result := fDictionary.ContainsValue(item);
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
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

function TDictionaryAdapter<TKey, TValue>.TValueCollection.DoGetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumeratorAdapter<TValue>.Create(fDictionary.Values);
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Values.Count;
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

procedure TDictionaryAdapter<TKey, TValue>.TValueCollection.Add(const item: TValue);
begin
  raise ENotSupportedException.Create('Add');
end;

procedure TDictionaryAdapter<TKey, TValue>.TValueCollection.Clear;
begin
  raise ENotSupportedException.Create('Clear');
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.Extract(
  const item: TValue): TValue;
begin
  raise ENotSupportedException.Create('Extract');
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.Remove(
  const item: TValue): Boolean;
begin
  raise ENotSupportedException.Create('Remove');
end;

{$ENDREGION}

end.
