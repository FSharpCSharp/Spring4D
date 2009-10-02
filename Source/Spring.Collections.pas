{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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

unit Spring.Collections;

{$I Spring.inc}

interface

uses
  Classes, SysUtils,
  Generics.Defaults, Generics.Collections;

type
  { Forward Declarations }
  IEnumerator<T> = interface;
  IEnumerable<T> = interface;
//  IEnumerableExtensions<T> = interface;
  ICollection<T> = interface;
  IList<T> = interface;
  IDictionary<TKey, TValue> = interface;

  TContainer = class;

//  TCollectionNotification = Generics.Collections.TCollectionNotification;

  /// <summary>
  /// Supports a simple iteration over a generic collection.
  /// </summary>
  IEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// Exposes the enumerator, which supports a simple iteration over a
  /// collection of a specified type.
  /// </summary>
  IEnumerable<T> = interface
    function GetEnumerator: IEnumerator<T>;
  end;

  /// <summary>
  /// Provides Extension methods for IEnumerable<T>
  /// </summary>
//  IEnumerableExtensions<T> = interface(IEnumerable<T>)
//    { Aggregation }
//    function Max: T;
//    function Min: T;
//
//    function Where(const predicate: TPredicate<T>): IEnumerableExtensions<T>;
//    function ToArray: TArray<T>;
//    function ToList: IList<T>;
//  end;

  /// <summary>
  /// Defines methods to manipulate generic collections.
  /// </summary>
  ICollection<T> = interface(IEnumerable<T>)  // IEnumerableExtensions<T>
    {$REGION 'Property Getters & Setters'}
      function GetIsEmpty: Boolean;
      function GetCount: Integer;
      function GetIsReadOnly: Boolean;
    {$ENDREGION}
    procedure Add(const item: T); overload;
    procedure Clear;
    function  Contains(const item: T): Boolean;
    function Remove(const item: T): Boolean; overload;
//    function Extract(const item: T): T;
    function ToArray: TArray<T>;  { TODO: MoveTo IEnumerableExtensions<T>? }
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  /// <summary>
  /// Represents a collection of objects that can be individually accessed by index.
  /// </summary>
  IList<T> = interface(ICollection<T>)
    {$REGION 'Property Getters & Setters'}
      function GetItem(index: Integer): T;
      procedure SetItem(index: Integer; const item: T);
    {$ENDREGION}
    procedure Insert(index: Integer; const item: T);
    procedure RemoveAt(index: Integer);
    function IndexOf(const item: T): Integer;
    property Items[index: Integer]: T read GetItem write SetItem; default;
  end;

  /// <summary>
  /// Represents a generic collection of key/value pairs.
  /// </summary>
  IDictionary<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>)
    {$REGION 'Property Getters & Setters'}
      function GetItem(const key: TKey): TValue;
      function GetKeys: ICollection<TKey>;
      function GetValues: ICollection<TValue>;
      procedure SetItem(const key: TKey; const value: TValue);
    {$ENDREGION}
    procedure Add(const key: TKey; const value: TValue); overload;
    procedure Remove(const key: TKey); overload;
    function ContainsKey(const key: TKey): Boolean;
//    function ExtractPair(const key: TKey): TPair<TKey, TValue>;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
    property Keys: ICollection<TKey> read GetKeys;
    property Values: ICollection<TValue> read GetValues;
  end;

  /// <summary>
  /// TCollectionOwnership
  /// </summary>
  TCollectionOwnership = (
    coReference,
    coOwned
  );

  TDictionaryOwnerships = Generics.Collections.TDictionaryOwnerships;

  /// <summary>
  /// Containers Facade
  /// </summary>
  /// <remarks>
  /// Developers should use TContainer class to create container instance,
  /// insteading of using the concrete classes, such as TListAdapter<T>,
  /// TDictionaryAdapter<TKey, TValue>, etc.
  /// </remarks>
  TContainer = class
  public
//    class function CreateEnumerator<T>(collection: TEnumerable<T>): IEnumerator<T>; overload;
//    class function CreateEnumerator<T>(enumerator: TEnumerator<T>): IEnumerator<T>; overload;
    class function CreateList<T>: IList<T>; overload;
    class function CreateList<T>(const comparer: IComparer<T>): IList<T>; overload;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload;
    class function CreateList<T: class>(ownsObjects: Boolean; const comparer: IComparer<T>): IList<T>; overload;
    class function CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(capacity: Integer): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
  end;

  /// <summary>
  /// Implements IEnumerableExtension<T>
  /// </summary>
  /// TEnumerable
//  TEnumerableExtensions<T> = class(TAggregatedObject, IEnumerableExtensions<T>, IEnumerable<T>, IInterface)
//  private
//    fCollection: Pointer;
//    function GetCollection: IEnumerable<T>;
//  public
//    constructor Create(const collection: IEnumerable<T>);
//    function GetEnumerator: IEnumerator<T>;
//    property Collection: IEnumerable<T> read GetCollection;
//  end;

  /// <summary>
  /// TEnumeratorAdapter<T>
  /// </summary>
  TEnumeratorAdapter<T> = class(TInterfacedObject, IEnumerator<T>, IInterface)
  private
    fEnumerator: TEnumerator<T>;
  public
    constructor Create(collection: TEnumerable<T>); overload;
    constructor Create(enumerator: TEnumerator<T>); overload;
    destructor Destroy; override;
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// TListAdapter<T>
  /// </summary>
  TListAdapter<T> = class(TInterfacedObject, IList<T>, ICollection<T>, IEnumerable<T>, IInterface)
  protected
    fList: TList<T>;
    fOwnership: TCollectionOwnership;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetIsReadOnly: Boolean; virtual;
    function GetItem(index: Integer): T;
    function GetEnumerator: IEnumerator<T>;
    procedure SetItem(index: Integer; const Value: T);
  public
    constructor Create(list: TList<T>; ownership: TCollectionOwnership = coReference); overload;
    destructor Destroy; override;
    procedure Add(const item: T);
    procedure Clear;
    procedure Insert(index: Integer; const item: T);
    procedure RemoveAt(index: Integer);
    function Extract(const item: T): T;
    function Contains(const item: T): Boolean;
    function IndexOf(const item: T): Integer;
    function Remove(const item: T): Boolean;
    function ToArray: TArray<T>;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  /// <summary>
  /// TDictionaryAdapter<TKey, TValue>
  /// </summary>
  TDictionaryAdapter<TKey, TValue> = class(TInterfacedObject, IDictionary<TKey, TValue>, ICollection<TPair<TKey, TValue>>, IEnumerable<TPair<TKey, TValue>>, IInterface)
  private
    fDictionary: TDictionary<TKey,TValue>;
    fOwnership: TCollectionOwnership;
    fKeys: ICollection<TKey>;
    fValues: ICollection<TValue>;
  private
    type
      /// <summary>
      /// Provides a read-only ICollection<TKey> implementation
      /// </summary>
      TKeyCollection = class(TInterfacedObject, ICollection<TKey>, IEnumerable<TKey>, IInterface)
      private
        fDictionary: TDictionary<TKey,TValue>;
      protected
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
        function GetIsReadOnly: Boolean;
      public
        constructor Create(dictionary: TDictionary<TKey,TValue>);
        { IEnumerable<TKey> }
        function GetEnumerator: IEnumerator<TKey>;
        { ICollection<TKey> }
        procedure Add(const item: TKey); overload;
        procedure Clear;
        function  Contains(const item: TKey): Boolean;
        function Remove(const item: TKey): Boolean; overload;
        function Extract(const item: TKey): TKey;
        function ToArray: TArray<TKey>;
        property Count: Integer read GetCount;
        property IsEmpty: Boolean read GetIsEmpty;
        property IsReadOnly: Boolean read GetIsReadOnly;
      end;

      /// <summary>
      /// Provides a read-only ICollection<TValue> implementation
      /// </summary>
      TValueCollection = class(TInterfacedObject, ICollection<TValue>, IEnumerable<TValue>, IInterface)
      private
        fDictionary: TDictionary<TKey, TValue>;
      protected
        function GetCount: Integer;
        function GetIsEmpty: Boolean;
        function GetIsReadOnly: Boolean;
      public
        constructor Create(dictionary: TDictionary<TKey,TValue>);
        { IEnumerable<TValue> }
        function GetEnumerator: IEnumerator<TValue>;
        { ICollection<TValue> }
        procedure Add(const item: TValue); overload;
        procedure Clear;
        function  Contains(const item: TValue): Boolean;
        function Remove(const item: TValue): Boolean; overload;
        function Extract(const item: TValue): TValue;
        function ToArray: TArray<TValue>;
        property Count: Integer read GetCount;
        property IsEmpty: Boolean read GetIsEmpty;
        property IsReadOnly: Boolean read GetIsReadOnly;
      end;
  public
    { Implements IEnumerable<TPair<TKey, TValue>> }
    function GetEnumerator: IEnumerator<TPair<TKey,TValue>>;
    { Implements ICollection<TPair<TKey, TValue>> }
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetIsReadOnly: Boolean;
    procedure Add(const item: TPair<TKey,TValue>); overload;
    procedure Clear;
    function  Contains(const item: TPair<TKey,TValue>): Boolean;
    function Remove(const item: TPair<TKey,TValue>): Boolean; overload;
    function Extract(const item: TPair<TKey,TValue>): TPair<TKey,TValue>;
    function ToArray: TArray<TPair<TKey,TValue>>;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsReadOnly: Boolean read GetIsReadOnly;
  public
    constructor Create(dictionary: TDictionary<TKey,TValue>;
      ownership: TCollectionOwnership = coReference); overload;
    destructor Destroy; override;
    { Implements IDictionary<TKey,TValue> }
    function GetItem(const key: TKey): TValue;
    function GetKeys: ICollection<TKey>;
    function GetValues: ICollection<TValue>;
    procedure SetItem(const key: TKey; const value: TValue);
    procedure Add(const key: TKey; const value: TValue); overload;
    procedure Remove(const key: TKey); overload;
    function ContainsKey(const key: TKey): Boolean;
    function ExtractPair(const key: TKey): TPair<TKey, TValue>;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
    property Keys: ICollection<TKey> read GetKeys;
    property Values: ICollection<TValue> read GetValues;
  end;

//  TReadOnlyCollection<T> = class(TInterfacedObject, IList<T>, ICollection<T>, IEnumerable<T>, IInterface)
//  end;

//  TObservableCollection<T> = class(TInterfacedObject, IList<T>)
//  end;


implementation

uses
  Spring.System;


{$REGION 'TContainer'}

class function TContainer.CreateList<T>: IList<T>;
begin
  Result := TListAdapter<T>.Create;
end;

class function TContainer.CreateList<T>(const comparer: IComparer<T>): IList<T>;
var
  list: TList<T>;
begin
  list := TList<T>.Create(comparer);
  Result := TListAdapter<T>.Create(list, coOwned);
end;

class function TContainer.CreateList<T>(ownsObjects: Boolean): IList<T>;
begin
  Result := TContainer.CreateList<T>(ownsObjects, TComparer<T>.Default);
end;

class function TContainer.CreateList<T>(ownsObjects: Boolean;
  const comparer: IComparer<T>): IList<T>;
var
  list: TObjectList<T>;
begin
  list := TObjectList<T>.Create(comparer, ownsObjects);
  Result := TListAdapter<T>.Create(list, coOwned);
end;

class function TContainer.CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>;
begin
  Result := TContainer.CreateDictionary<TKey,TValue>(0, TEqualityComparer<TKey>.Default);
end;

class function TContainer.CreateDictionary<TKey, TValue>(
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TContainer.CreateDictionary<TKey, TValue>(capacity, TEqualityComparer<TKey>.Default);
end;

class function TContainer.CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
begin
  Result := TContainer.CreateDictionary<TKey, TValue>(0, comparer);
end;

class function TContainer.CreateDictionary<TKey, TValue>(capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: TDictionary<TKey,TValue>;
begin
  TArgument.CheckRange(capacity >= 0, 'capacity');
  dictionary := TDictionary<TKey,TValue>.Create(capacity, comparer);
  Result := TDictionaryAdapter<TKey, TValue>.Create(dictionary, coOwned);
end;

class function TContainer.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TContainer.CreateDictionary<TKey, TValue>(ownerships, 0, TEqualityComparer<TKey>.Default);
end;

class function TContainer.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships;
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TContainer.CreateDictionary<TKey, TValue>(ownerships, capacity, TEqualityComparer<TKey>.Default);
end;

class function TContainer.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships; capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: TObjectDictionary<TKey,TValue>;
begin
  dictionary := TObjectDictionary<TKey, TValue>.Create(ownerships, capacity, comparer);
  Result := TDictionaryAdapter<TKey, TValue>.Create(dictionary, coOwned);
end;

{$ENDREGION}


{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(enumerator: TEnumerator<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
end;

constructor TEnumeratorAdapter<T>.Create(collection: TEnumerable<T>);
begin
  Create(collection.GetEnumerator);
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  fEnumerator.Free;
  inherited Destroy;
end;

function TEnumeratorAdapter<T>.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

procedure TEnumeratorAdapter<T>.Reset;
begin
  raise EInvalidOperation.Create('SCannotResetEnumerator');
end;

{$ENDREGION}


{$REGION 'TEnumerableExtensions<T>'}

//constructor TEnumerableExtensions<T>.Create(const collection: IEnumerable<T>);
//begin
//  inherited Create(collection);
//  fCollection := Pointer(collection);  // Weak Reference
//end;
//
//function TEnumerableExtensions<T>.GetCollection: IEnumerable<T>;
//begin
//  Result := IEnumerable<T>(fCollection);
//end;
//
//function TEnumerableExtensions<T>.GetEnumerator: IEnumerator<T>;
//begin
//  Result := GetCollection.GetEnumerator;
//end;

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

function TListAdapter<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TListAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fList);
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
  if fOwnership = coOwned then
    fDictionary.Free;
  inherited Destroy;
end;

function TDictionaryAdapter<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
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

function TDictionaryAdapter<TKey, TValue>.GetIsReadOnly: Boolean;
begin
  Result := False;
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
    fKeys := TKeyCollection.Create(fDictionary);
  end;
  Result := fKeys;
end;

function TDictionaryAdapter<TKey, TValue>.GetValues: ICollection<TValue>;
begin
  if fValues = nil then
  begin
    fValues := TValueCollection.Create(fDictionary);
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
  fDictionary[key] := value;
end;

{$ENDREGION}


{$REGION 'TDictionaryAdapter<TKey, TValue>.TKeyCollection'}

constructor TDictionaryAdapter<TKey, TValue>.TKeyCollection.Create(
  dictionary: TDictionary<TKey, TValue>);
begin
  inherited Create;
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

function TDictionaryAdapter<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
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
  dictionary: TDictionary<TKey, TValue>);
begin
  inherited Create;
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

function TDictionaryAdapter<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumeratorAdapter<TValue>.Create(fDictionary.Values);
end;

function TDictionaryAdapter<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
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
