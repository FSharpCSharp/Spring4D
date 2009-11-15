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

{ TODO: Add IQueue<T>, ISet<T> }
{ TODO: Consider Non-Generic interfaces }
{ TODO: Consider LINQ-Like Enumerable Extension }

unit Spring.Collections;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  Spring.System;

type
  { Forward Declarations }
  IEnumeratorEx<T> = interface;
  IEnumerableEx<T> = interface;
//  IEnumerableExtensions<T> = interface;
  ICollection<T> = interface;
  IList<T> = interface;
  IDictionary<TKey, TValue> = interface;

  TCollections = class;

  /// <summary>
  /// Supports a simple iteration over a generic collection.
  /// </summary>
  IEnumeratorEx<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// Exposes the enumerator, which supports a simple iteration over a
  /// collection of a specified type.
  /// </summary>
  IEnumerableEx<T> = interface
    function GetEnumerator: IEnumeratorEx<T>;
  end;

  /// <summary>
  /// Provides Extension methods for IEnumerableEx<T>
  /// </summary>
//  IEnumerableExtensions<T> = interface(IEnumerableEx<T>)
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
  ICollection<T> = interface(IEnumerableEx<T>)  // IEnumerableExtensions<T>
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
    function ToArray: TArray<T>;                // experimental
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;  // experimental
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
  /// Provides static methods to create generic interface collections.
  /// </summary>
  /// <remarks>
  /// Use the TCollections class to create collection instance,
  /// insteading of the implementations in Spring.Collections.Adapters,
  /// such as TListAdapter<T>, TDictionaryAdapter<TKey, TValue>, etc.
  /// </remarks>
  TCollections = class
  public
//    class function CreateEnumerator<T>(collection: TEnumerable<T>): IEnumeratorEx<T>; overload;
//    class function CreateEnumerator<T>(enumerator: TEnumerator<T>): IEnumeratorEx<T>; overload;
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

  TContainers = TCollections deprecated 'Use TCollections instead.';

//  TReadOnlyCollection<T> = class(TInterfacedObject, IList<T>, ICollection<T>, IEnumerableEx<T>, IInterface)
//  end;

//  TObservableCollection<T> = class(TInterfacedObject, IList<T>)
//  end;


implementation

uses
  Spring.Collections.Adapters,
  Spring.ResourceStrings;


{$REGION 'TCollections'}

class function TCollections.CreateList<T>: IList<T>;
var
  list: TList<T>;
begin
  list := TList<T>.Create;
  Result := TListAdapter<T>.Create(list, coOwned);
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>): IList<T>;
var
  list: TList<T>;
begin
  list := TList<T>.Create(comparer);
  Result := TListAdapter<T>.Create(list, coOwned);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean): IList<T>;
begin
  Result := TCollections.CreateList<T>(ownsObjects, TComparer<T>.Default);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean;
  const comparer: IComparer<T>): IList<T>;
var
  list: TObjectList<T>;
begin
  list := TObjectList<T>.Create(comparer, ownsObjects);
  Result := TListAdapter<T>.Create(list, coOwned);
end;

class function TCollections.CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey,TValue>(0, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(capacity, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(0, comparer);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: TDictionary<TKey,TValue>;
begin
  TArgument.CheckRange(capacity >= 0, 'capacity');
  dictionary := TDictionary<TKey,TValue>.Create(capacity, comparer);
  Result := TDictionaryAdapter<TKey, TValue>.Create(dictionary, coOwned);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(ownerships, 0, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships;
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(ownerships, capacity, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships; capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: TObjectDictionary<TKey,TValue>;
begin
  dictionary := TObjectDictionary<TKey, TValue>.Create(ownerships, capacity, comparer);
  Result := TDictionaryAdapter<TKey, TValue>.Create(dictionary, coOwned);
end;

{$ENDREGION}

end.
