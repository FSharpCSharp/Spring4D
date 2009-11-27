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
  /// Infrastructure. Exposes the enumerator, which supports a simple
  /// iteration over a collection of a specified type.
  /// </summary>
  IEnumerable_<T> = interface
    function GetEnumerator: IEnumeratorEx<T>;
  end;

  /// <summary>
  /// Provides enumerable extension methods for _IEnumerable<T>
  /// </summary>
  IEnumerableEx<T> = interface(IEnumerable_<T>)
    {$REGION 'Property Getters & Setters'}
      function GetCount: Integer;
      function GetIsEmpty: Boolean;
    {$ENDREGION}
    function First: T; overload;
    function First(const predicate: TPredicate<T>): T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload;
    function Last: T; overload;
    function Last(const predicate: TPredicate<T>): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const predicate: TPredicate<T>): T; overload;
    function Where(const predicate: TPredicate<T>): IEnumerableEx<T>;
    function Contains(const item: T): Boolean; overload;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>;
    function ToList: IList<T>;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  /// Defines methods to manipulate generic collections.
  /// </summary>
  ICollection<T> = interface(IEnumerableEx<T>)
    {$REGION 'Property Getters & Setters'}
      function GetIsReadOnly: Boolean;
    {$ENDREGION}
    procedure Add(const item: T); overload;
    procedure Clear;
    function Remove(const item: T): Boolean; overload;
//    function Extract(const item: T): T;
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
//    procedure AddOrSetValue(const key: TKey; const value: TValue);
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

  /// <summary>
  /// Provides an abstract base class for IEnumeratorEx<T>.
  /// </summary>
  TEnumeratorBase<T> = class abstract(TInterfacedObject, IEnumeratorEx<T>, IInterface)
  protected
    function GetCurrent: T; virtual; abstract;
  public
    function MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// Provides a default implementation for IEnumerableEx<T> (Extension Methods).
  /// </summary>
  /// <remarks>
  /// Since generic interfaces can not be implemented by delegation, it's reasonable that
  /// Inheriting from TEnumerableBase<T> directly.
  /// </remarks>
  TEnumerableBase<T> = class abstract(TInterfacedObject,
    IEnumerableEx<T>, IEnumerable_<T>, IInterface)
  protected
    function GetCount: Integer; virtual;
    function GetIsEmpty: Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; virtual;
    function TryGetLast(out value: T): Boolean; virtual;
  public
    { _IEnumerable<T> }
    function GetEnumerator: IEnumeratorEx<T>; virtual; abstract;
    { IEnumerableEx<T> }
    function First: T; overload; virtual;
    function First(const predicate: TPredicate<T>): T; overload; virtual;
    function FirstOrDefault: T; overload; virtual;
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function Last: T; overload; virtual;
    function Last(const predicate: TPredicate<T>): T; overload; virtual;
    function LastOrDefault: T; overload; virtual;
    function LastOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function Where(const predicate: TPredicate<T>): IEnumerableEx<T>; virtual;
    function Contains(const item: T): Boolean; overload; virtual;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function ToArray: TArray<T>; virtual;
    function ToList: IList<T>; virtual;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

//  TReadOnlyCollection<T> = class(TInterfacedObject, IList<T>, ICollection<T>, IEnumerableEx<T>, IInterface)
//  end;

//  TObservableCollection<T> = class(TInterfacedObject, IList<T>)
//  end;


implementation

uses
  Spring.Collections.Adapters,
  Spring.ResourceStrings, Spring.Collections.Extensions;


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


{$REGION 'TEnumeratorBase<T>'}

procedure TEnumeratorBase<T>.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

function TEnumerableBase<T>.Contains(const item: T): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
//  TArgument.CheckNotNull<T>(item, 'item');
  comparer := TEqualityComparer<T>.Default;
  Result := Contains(item, comparer);
end;

function TEnumerableBase<T>.Contains(const item: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  enumerator: IEnumeratorEx<T>;
begin
//  TArgument.CheckNotNull<T>(item, 'item');
  enumerator := GetEnumerator;
  Result := False;
  while enumerator.MoveNext do
  begin
    if comparer.Equals(enumerator.Current, item) then
    begin
      Exit(True);
    end;
  end;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumeratorEx<T>;
begin
  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
  if Result then
  begin
    value := enumerator.Current;
  end
end;

function TEnumerableBase<T>.TryGetLast(out value: T): Boolean;
var
  enumerator: IEnumeratorEx<T>;
  hasNext: Boolean;
begin
  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
  hasNext := Result;
  while hasNext do
  begin
    value := enumerator.Current;
    hasNext := enumerator.MoveNext;
  end;
end;

function TEnumerableBase<T>.First: T;
begin
  if not TryGetFirst(Result) then
  begin
    raise EInvalidOperation.Create('First');  // TEMP
  end;
end;

function TEnumerableBase<T>.First(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).First; // TEMP
end;

function TEnumerableBase<T>.FirstOrDefault: T;
begin
  if not TryGetFirst(Result) then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).FirstOrDefault; // TEMP
end;

function TEnumerableBase<T>.Last: T;
begin
  if not TryGetLast(Result) then
  begin
    raise EInvalidOperation.Create('Last');  // TEMP
  end;
end;

function TEnumerableBase<T>.Last(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).Last;  // TEMP
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  if not TryGetLast(Result) then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).LastOrDefault;  // TEMP
end;

function TEnumerableBase<T>.Where(
  const predicate: TPredicate<T>): IEnumerableEx<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');
  Result := TEnumerableWithPredicate<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
begin
  Result := ToList.ToArray;
end;

function TEnumerableBase<T>.ToList: IList<T>;
var
  enumerator: IEnumeratorEx<T>;
begin
  Result := TCollections.CreateList<T>;
  enumerator := GetEnumerator;
  while enumerator.MoveNext do
  begin
    Result.Add(enumerator.Current);
  end;
end;

function TEnumerableBase<T>.GetCount: Integer;
var
  enumerator: IEnumeratorEx<T>;
begin
  Result := 0;
  enumerator := GetEnumerator;
  while enumerator.MoveNext do
  begin
    Inc(Result);
  end;
end;

function TEnumerableBase<T>.GetIsEmpty: Boolean;
begin
  Result := not GetEnumerator.MoveNext;
end;

{$ENDREGION}

end.
