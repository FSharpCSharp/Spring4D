{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Collections;  // experimental

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  Spring;

type
  { Forward Declarations }
  IEnumerableEx<T> = interface;
  ICollection<T> = interface;
  IList<T> = interface;
  IDictionary<TKey, TValue> = interface;
  TCollections = class;

  /// <summary>
  /// Provides limited LINQ-like enumerable extension methods for IEnumerable<T>.
  /// </summary>
  IEnumerableEx<T> = interface(IEnumerable<T>)
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

  IEnumerableAware = interface
    ['{3F276486-B108-422C-B0EE-F2FC7CB4B52E}']
    function GetEnumerable: IEnumerable;
  end;

  ICollection = interface(IEnumerableEx<TValue>)
    ['{B87ABB57-1E75-4DAD-96CE-C077B565F11A}']
  {$REGION 'Property Getters & Setters'}
    function GetIsReadOnly: Boolean;
  {$ENDREGION}
    procedure Add(const item: TValue); overload;
    procedure Clear;
    function Remove(const item: TValue): Boolean; overload;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  IList = interface(ICollection)
    ['{629166B3-E538-430F-BE5A-D6FE42704965}']
  {$REGION 'Property Getters & Setters'}
    function GetItem(index: Integer): TValue;
    procedure SetItem(index: Integer; const item: TValue);
  {$ENDREGION}
    procedure Insert(index: Integer; const item: TValue);
    procedure RemoveAt(index: Integer);
    function IndexOf(const item: TValue): Integer;
    property Items[index: Integer]: TValue read GetItem write SetItem; default;
  end;

  IDictionary = interface(ICollection)
    ['{BAA9A5D9-BBE1-4512-9AA3-9E1F81908857}']
  {$REGION 'Property Getters & Setters'}
    function GetItem(const key: TValue): TValue;
    function GetKeys: ICollection;
    function GetValues: ICollection;
    procedure SetItem(const key: TValue; const value: TValue);
  {$ENDREGION}
    procedure Add(const key: TValue; const value: TValue); overload;
    procedure Remove(const key: TValue); overload;
    function ContainsKey(const key: TValue): Boolean;
    function TryGetValue(const key: TValue; out value: TValue): Boolean;
    property Items[const key: TValue]: TValue read GetItem write SetItem; default;
    property Keys: ICollection read GetKeys;
    property Values: ICollection read GetValues;
  end;

  /// <summary>
  /// Defines methods to manipulate generic collections.
  /// </summary>
  ICollection<T> = interface(IEnumerableEx<T>)
  {$REGION 'Property Getters & Setters'}
    function GetIsReadOnly: Boolean;
  {$ENDREGION}
    procedure Add(const item: T); // overload;
    procedure Clear;
    function Remove(const item: T): Boolean; // overload;
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
    class function CreateList<T>(list: TList<T>; ownership: TCollectionOwnership): IList<T>; overload;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload;
    class function CreateList<T: class>(ownsObjects: Boolean; const comparer: IComparer<T>): IList<T>; overload;
//    class function CreateObjectList<T: class>: IList<T>; overload;
//    class function CreateObjectList<T: class>(const comparer: IComparer<T>): IList<T>; overload;
    class function CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(capacity: Integer): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload;
    class function CreateDictionary<TKey, TValue>(dictionary: TDictionary<TKey, TValue>; ownership: TCollectionOwnership): IDictionary<TKey, TValue>; overload;
  end;

  TContainers = TCollections deprecated 'Use TCollections instead.';

  /// <summary>
  /// Provides an abstract implementation for IEnumerator<T>.
  /// </summary>
  TEnumeratorBase<T> = class abstract(TInterfacedObject, IEnumerator<T>, IEnumerator, IInterface)
  protected
    function DoGetCurrent: T; virtual; abstract;
    function IEnumerator<T>.GetCurrent = DoGetCurrent;
  public
    function GetCurrent: TObject; virtual;
    function MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual;
    property Current: T read DoGetCurrent;
  end;

  /// <summary>
  /// Provides an abstract implementation IEnumerable<T>.
  /// </summary>
  TEnumerableBase<T> = class abstract(TInterfacedObject, IEnumerableAware, IEnumerable<T>, IEnumerable, IInterface)
  protected
    function DoGetEnumerator: IEnumerator<T>; virtual; abstract;
    function IEnumerable<T>.GetEnumerator = DoGetEnumerator;
    { IEnumerableAware }
    function GetEnumerable: IEnumerable;
  public
    function GetEnumerator: IEnumerator; virtual;
  end;

  /// <summary>
  /// Provides a default implementation for IEnumerableEx<T> (Extension Methods).
  /// </summary>
  TEnumerableEx<T> = class abstract(TEnumerableBase<T>, IEnumerableEx<T>,
    IEnumerable<T>, IEnumerable, IInterface)
  protected
    function GetCount: Integer; virtual;
    function GetIsEmpty: Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; virtual;
    function TryGetLast(out value: T): Boolean; virtual;
  public
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

  TContainedEnumerableEx<T> = class abstract(TEnumerableEx<T>, IInterface)
  private
    fController: Pointer;  // Weak reference to controller
    function GetController: IInterface;
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  TNullEnumerable<T> = class(TEnumerableEx<T>)
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  end;

  TNullEnumerator<T> = class(TEnumeratorBase<T>)
  protected
    function DoGetCurrent: T; override;
  public
    function MoveNext: Boolean; override;
  end;

  TCollectionBase<T> = class(TEnumerableEx<T>, ICollection<T>)
  protected
    function GetIsReadOnly: Boolean; virtual;
  public
    procedure Add(const item: T); virtual; abstract;
    procedure Clear; virtual; abstract;
    function Remove(const item: T): Boolean; virtual; abstract;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

//  TReadOnlyCollection<T> = class(TInterfacedObject, ICollection<T>, IEnumerableEx<T>, IInterface)
//  end;

//  TReadOnlyList<T> = class(TReadOnlyCollection<T>, IList<T>, ICollection<T>, IEnumerableEx<T>, IInterface)
//  end;

//  TObservableCollection<T> = class(TInterfacedObject, IList<T>)
//  end;


implementation

uses
  TypInfo,
  Spring.Collections.Adapters,
  Spring.Collections.Extensions,
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

class function TCollections.CreateList<T>(list: TList<T>;
  ownership: TCollectionOwnership): IList<T>;
begin
  TArgument.CheckNotNull(list, 'list');
  Result := TListAdapter<T>.Create(list, ownership);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  dictionary: TDictionary<TKey, TValue>;
  ownership: TCollectionOwnership): IDictionary<TKey, TValue>;
begin
  TArgument.CheckNotNull(dictionary, 'dictionary');
  Result := TDictionaryAdapter<TKey, TValue>.Create(dictionary, ownership);
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

{ TODO: Consider support (boxing & unboxing) TEnumeratorBase<T>.GetCurrent: TObject }
function TEnumeratorBase<T>.GetCurrent: TObject;
var
  typeKind: TTypeKind;
  value: T;
begin
  value := DoGetCurrent;
  typeKind := PTypeInfo(TypeInfo(T)).Kind;
  if typeKind = tkClass then
  begin
    Pointer(Result) := PPointer(@value)^;
  end
  else
  begin
    raise ENotImplementedException.Create('function GetCurrent: TObject;');
  end;
end;

procedure TEnumeratorBase<T>.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

function TEnumerableBase<T>.GetEnumerable: IEnumerable;
begin
  Result := Self;
end;

function TEnumerableBase<T>.GetEnumerator: IEnumerator;
begin
  Result := DoGetEnumerator;
end;

{$ENDREGION}


{$REGION 'TEnumerableEx<T>'}

function TEnumerableEx<T>.Contains(const item: T): Boolean;
var
  comparer: IEqualityComparer<T>;
begin
  TArgument.CheckNotNull<T>(item, 'item');
  comparer := TEqualityComparer<T>.Default;
  Result := Contains(item, comparer);
end;

function TEnumerableEx<T>.Contains(const item: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  TArgument.CheckNotNull<T>(item, 'item');
  enumerator := DoGetEnumerator;
  Result := False;
  while enumerator.MoveNext do
  begin
    if comparer.Equals(enumerator.Current, item) then
    begin
      Exit(True);
    end;
  end;
end;

function TEnumerableEx<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := DoGetEnumerator;
  Result := enumerator.MoveNext;
  if Result then
  begin
    value := enumerator.Current;
  end
end;

function TEnumerableEx<T>.TryGetLast(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
  hasNext: Boolean;
begin
  enumerator := DoGetEnumerator;
  Result := enumerator.MoveNext;
  hasNext := Result;
  while hasNext do
  begin
    value := enumerator.Current;
    hasNext := enumerator.MoveNext;
  end;
end;

function TEnumerableEx<T>.First: T;
begin
  if not TryGetFirst(Result) then
  begin
    raise EInvalidOperation.Create('First');  // TEMP
  end;
end;

function TEnumerableEx<T>.First(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).First; // TEMP
end;

function TEnumerableEx<T>.FirstOrDefault: T;
begin
  if not TryGetFirst(Result) then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableEx<T>.FirstOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).FirstOrDefault; // TEMP
end;

function TEnumerableEx<T>.Last: T;
begin
  if not TryGetLast(Result) then
  begin
    raise EInvalidOperation.Create('Last');  // TEMP
  end;
end;

function TEnumerableEx<T>.Last(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).Last;
end;

function TEnumerableEx<T>.LastOrDefault: T;
begin
  if not TryGetLast(Result) then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableEx<T>.LastOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).LastOrDefault;
end;

function TEnumerableEx<T>.Where(
  const predicate: TPredicate<T>): IEnumerableEx<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');
  Result := TEnumerableWithPredicate<T>.Create(Self, predicate);
end;

function TEnumerableEx<T>.ToArray: TArray<T>;
begin
  Result := ToList.ToArray;
end;

function TEnumerableEx<T>.ToList: IList<T>;
var
  enumerator: IEnumerator<T>;
begin
  Result := TCollections.CreateList<T>;
  enumerator := DoGetEnumerator;
  while enumerator.MoveNext do
  begin
    Result.Add(enumerator.Current);
  end;
end;

function TEnumerableEx<T>.GetCount: Integer;
var
  enumerator: IEnumerator<T>;
begin
  Result := 0;
  enumerator := DoGetEnumerator;
  while enumerator.MoveNext do
  begin
    Inc(Result);
  end;
end;

function TEnumerableEx<T>.GetIsEmpty: Boolean;
begin
  Result := not DoGetEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TContainedEnumerableEx<T>'}

constructor TContainedEnumerableEx<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedEnumerableEx<T>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedEnumerableEx<T>._AddRef: Integer;
begin
  Result := Controller._AddRef;
end;

function TContainedEnumerableEx<T>._Release: Integer;
begin
  Result := Controller._Release;
end;

{$ENDREGION}


{$REGION 'TNullEnumerator<T>'}

function TNullEnumerator<T>.DoGetCurrent: T;
begin
  raise EInvalidOperation.CreateRes(@SEnumEmpty);
end;

function TNullEnumerator<T>.MoveNext: Boolean;
begin
  Result := False;
end;

{$ENDREGION}


{$REGION 'TNullEnumerable<T>'}

function TNullEnumerable<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TNullEnumerator<T>.Create;
end;

{$ENDREGION}

{ TCollectionBase<T> }

function TCollectionBase<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

end.
