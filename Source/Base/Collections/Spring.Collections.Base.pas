{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
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

unit Spring.Collections.Base;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections;

type
  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|IEnumerator" /> interface.
  ///	</summary>
  TEnumeratorBase = class abstract(TInterfacedObject, IEnumerator)
  private
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
  public
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;

    property Current: TValue read GetCurrentNonGeneric;
  end;

  ///	<summary>
  ///	  Provides a default implementation for the
  ///	  <see cref="Spring.Collections|IEnumerator&lt;T&gt;" /> interface.
  ///	</summary>
  TEnumeratorBase<T> = class abstract(TEnumeratorBase, IEnumerator<T>)
  private
    function GetCurrentNonGeneric: TValue; override; final;
  protected
    function GetCurrent: T; virtual;
  public
    property Current: T read GetCurrent;
  end;

  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|IEnumerable" /> interface.
  ///	</summary>
  TEnumerableBase = class abstract(TInterfacedObject, IInterface,
    IElementType, ICountable, IEnumerable)
  private
    function GetEnumeratorNonGeneric: IEnumerator; virtual; abstract;
    function IEnumerable.GetEnumerator = GetEnumeratorNonGeneric;
  {$REGION 'Property Accessors'}
    function GetElementType: PTypeInfo; virtual; abstract;
  protected
    function GetCount: Integer; virtual;
    function GetIsEmpty: Boolean; virtual;
  {$ENDREGION}
  protected
  {$REGION 'Implements IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  {$ENDREGION}
  public
    function AsObject: TObject;

    function GetEnumerator: IEnumerator;

    property Count: Integer read GetCount;
    property ElementType: PTypeInfo read GetElementType;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  ///	<summary>
  ///	  Provides a default implementation for the
  ///	  <see cref="Spring.Collections|IEnumerable&lt;T&gt;" /> interface.
  ///	</summary>
  TEnumerableBase<T> = class abstract(TEnumerableBase, IEnumerable<T>)
  private
    fComparer: IComparer<T>;
    class var fEqualityComparer: IEqualityComparer<T>;
    function GetEnumeratorNonGeneric: IEnumerator; override; final;
  {$REGION 'Property Accessors'}
    function GetComparer: IComparer<T>;
    function GetElementType: PTypeInfo; override; final;
    class function GetEqualityComparer: IEqualityComparer<T>; static;
  {$ENDREGION}
  protected
    function TryGetElementAt(out value: T; index: Integer): Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; overload;
    function TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean; overload; virtual;
    function TryGetLast(out value: T): Boolean; overload;
    function TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean; overload; virtual;
    function TryGetSingle(out value: T): Boolean; overload; virtual;
    function TryGetSingle(out value: T; const predicate: TPredicate<T>): Boolean; overload;

    property Comparer: IComparer<T> read GetComparer;
    class property EqualityComparer: IEqualityComparer<T> read GetEqualityComparer;
  public
    constructor Create; overload; virtual;
    constructor Create(const comparer: IComparer<T>); overload;
    constructor Create(const comparer: TComparison<T>); overload;

    class destructor Destroy;

    function GetEnumerator: IEnumerator<T>; virtual;

    function All(const predicate: TPredicate<T>): Boolean;
    function Any: Boolean; overload;
    function Any(const predicate: TPredicate<T>): Boolean; overload;

    function Concat(const second: IEnumerable<T>): IEnumerable<T>;

    function Contains(const value: T): Boolean; overload; virtual;
    function Contains(const value: T; comparer: IEqualityComparer<T>): Boolean; overload; virtual;

    function ElementAt(index: Integer): T;
    function ElementAtOrDefault(index: Integer): T; overload;
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    function EqualsTo(const collection: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const collection: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    function First: T; overload; virtual;
    function First(const predicate: TPredicate<T>): T; overload; virtual;
    function FirstOrDefault: T; overload; virtual;
    function FirstOrDefault(const defaultValue: T): T; overload;
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function FirstOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    procedure ForEach(const action: TAction<T>); overload;

    function Last: T; overload; virtual;
    function Last(const predicate: TPredicate<T>): T; overload; virtual;
    function LastOrDefault: T; overload; virtual;
    function LastOrDefault(const defaultValue: T): T; overload;
    function LastOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function LastOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    function Max: T; overload;
    function Max(const comparer: IComparer<T>): T; overload;
    function Min: T; overload;
    function Min(const comparer: IComparer<T>): T; overload;

//    function Ordered: IEnumerable<T>; overload;
//    function Ordered(const comparison: TComparison<T>): IEnumerable<T>; overload;

    function Reversed: IEnumerable<T>; virtual;

    function Single: T; overload;
    function Single(const predicate: TPredicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload; virtual;
    function SingleOrDefault(const predicate: TPredicate<T>): T; overload;
    function SingleOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    function Skip(count: Integer): IEnumerable<T>;
    function SkipWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function SkipWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Take(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Where(const predicate: TPredicate<T>): IEnumerable<T>; virtual;

    function ToArray: TArray<T>; virtual;
    function ToList: IList<T>; virtual;
    function ToSet: ISet<T>; virtual;
  end;

  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|ICollection&lt;T&gt;" /> interface.
  ///	</summary>
  ///	<remarks>
  ///	  The Add/Remove/Extract/Clear methods are abstract. IsReadOnly returns
  ///	  <c>False</c> by default.
  ///	</remarks>
  TCollectionBase<T> = class abstract(TEnumerableBase<T>, ICollection<T>)
  protected
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean; virtual;
  {$ENDREGION}
  public
    procedure Add(const item: T); virtual; abstract;
    procedure AddRange(const collection: array of T); overload; virtual;
    procedure AddRange(const collection: IEnumerable<T>); overload; virtual;
    procedure AddRange(const collection: TEnumerable<T>); overload; virtual;

    procedure Clear; virtual; abstract;

    function Remove(const item: T): Boolean; virtual; abstract;
    procedure RemoveRange(const collection: array of T); overload; virtual;
    procedure RemoveRange(const collection: IEnumerable<T>); overload; virtual;
    procedure RemoveRange(const collection: TEnumerable<T>); overload; virtual;

//    function Extract(const item: T): T; virtual; abstract;
//    procedure ExtractRange(const collection: array of T); overload; virtual;
//    procedure ExtractRange(const collection: IEnumerable<T>); overload; virtual;
//    procedure ExtractRange(const collection: TEnumerable<T>); overload; virtual;

//    procedure CopyTo(var values: TArray<T>; index: Integer); virtual;

    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TContainedCollectionBase<T> = class(TCollectionBase<T>)
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

  TListBase<T> = class abstract(TCollectionBase<T>, IList<T>)
  protected
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TListBase<T>;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TListBase<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fOnChanged: ICollectionChangedEvent<T>;
    function GetOnChanged: ICollectionChangedEvent<T>;
  protected
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
    procedure DoSort(const comparer: IComparer<T>); virtual;
    procedure DoInsert(index: Integer; const item: T); virtual; abstract;
    procedure DoDelete(index: Integer; notification: TCollectionChangedAction); virtual; abstract;
    procedure DoDeleteRange(index, count: Integer; notification: TCollectionChangedAction); virtual; abstract;
    function GetItem(index: Integer): T; virtual; abstract;
    procedure SetItem(index: Integer; const value: T); virtual; abstract;
  public
    constructor Create; overload; override;
    constructor Create(const collection: array of T); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean; override;
    function TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean; override;
    function Contains(const item: T): Boolean; override;
    function ToArray: TArray<T>; override;
    function Reversed: IEnumerable<T>; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;
    function  Remove(const item: T): Boolean; override;
    procedure Clear; override;

    procedure Insert(index: Integer; const item: T); virtual;
    procedure InsertRange(index: Integer; const collection: array of T); overload; virtual;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload; virtual;
    procedure InsertRange(index: Integer; const collection: TEnumerable<T>); overload; virtual;
    procedure Delete(index: Integer);
    procedure DeleteRange(startIndex, count: Integer);
    function Extract(const item: T): T;
    function IndexOf(const item: T): Integer;
    function LastIndexOf(const item: T): Integer;
    procedure Exchange(index1, index2: Integer); virtual; abstract;
    procedure Move(currentIndex, newIndex: Integer); virtual; abstract;
    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparison: TComparison<T>); overload;
    procedure Reverse; virtual; abstract;

    property Items[index: Integer]: T read GetItem write SetItem; default;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

implementation

uses
  Spring.Collections.Events,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Collections.Sets,
  Spring.ResourceStrings;


{$REGION 'TEnumeratorBase'}

function TEnumeratorBase.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TEnumeratorBase.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TEnumeratorBase<T>'}

function TEnumeratorBase<T>.GetCurrent: T;
begin
  raise ENotSupportedException.Create('GetCurrent');
end;

function TEnumeratorBase<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(GetCurrent);
end;

{$ENDREGION}


{$REGION 'TEnumerableBase'}

function TEnumerableBase.AsObject: TObject;
begin
  Result := Self;
end;

function TEnumerableBase.GetCount: Integer;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := GetEnumerator;
  while enumerator.MoveNext do
    Inc(Result);
end;

function TEnumerableBase.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorNonGeneric;
end;

function TEnumerableBase.GetIsEmpty: Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := GetEnumerator;
  Result := not enumerator.MoveNext;
end;

function TEnumerableBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TEnumerableBase._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TEnumerableBase._Release: Integer;
begin
  Result := inherited _Release;
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

constructor TEnumerableBase<T>.Create;
begin
  inherited Create;
  fComparer := TComparer<T>.Default;
end;

constructor TEnumerableBase<T>.Create(const comparer: IComparer<T>);
begin
  Create;
  if Assigned(comparer) then
    fComparer := comparer;
end;

constructor TEnumerableBase<T>.Create(const comparer: TComparison<T>);
begin
  Create(TComparer<T>.Construct(comparer));
end;

class destructor TEnumerableBase<T>.Destroy;
begin
  fEqualityComparer := nil;
end;

function TEnumerableBase<T>.All(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := True;
  for item in Self do
    if not predicate(item) then
      Exit(False);
end;

function TEnumerableBase<T>.Any: Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
end;

function TEnumerableBase<T>.Any(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := False;
  for item in Self do
    if predicate(item) then
      Exit(True);
end;

function TEnumerableBase<T>.Concat(
  const second: IEnumerable<T>): IEnumerable<T>;
begin
  Guard.CheckNotNull(Assigned(second), 'second');

  Result := TConcatEnumerable<T>.Create(Self, second);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
begin
  Result := Contains(value, nil);
end;

function TEnumerableBase<T>.Contains(const value: T;
  comparer: IEqualityComparer<T>): Boolean;
var
  item: T;
begin
  Guard.CheckNotNull<T>(value, 'value');

  if not Assigned(comparer) then
    comparer := EqualityComparer;

  for item in Self do
    if comparer.Equals(value, item) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase<T>.ElementAt(index: Integer): T;
begin
  if not TryGetElementAt(Result, index) then
    raise EArgumentOutOfRangeException.Create('index');
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
begin
  if not TryGetElementAt(Result, index) then
    Result := Default(T);
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer;
  const defaultValue: T): T;
begin
  if not TryGetElementAt(Result, index) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>): Boolean;
begin
  Result := EqualsTo(collection, EqualityComparer);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
var
  e1, e2: IEnumerator<T>;
  hasNext: Boolean;
begin
  Guard.CheckNotNull(Assigned(collection), 'collection');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');

  e1 := GetEnumerator;
  e2 := collection.GetEnumerator;

  while True do
  begin
    hasNext := e1.MoveNext;
    if hasNext <> e2.MoveNext then
      Exit(False)
    else if not hasNext then
      Exit(True);
    if hasNext and not comparer.Equals(e1.Current, e2.Current) then
      Exit(False);
  end;
end;

function TEnumerableBase<T>.First: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
    Result := enumerator.Current
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TEnumerableBase<T>.First(const predicate: TPredicate<T>): T;
var
  item: T;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  for item in Self do
    if predicate(item) then
      Exit(item);
  raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.FirstOrDefault: T;
begin
  Result := FirstOrDefault(Default(T));
end;

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
    Result := enumerator.Current
  else
    Result := defaultValue;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := FirstOrDefault(predicate, Default(T));
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>;
  const defaultValue: T): T;
var
  item: T;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  for item in Self do
    if predicate(item) then
      Exit(item);
  Result := defaultValue;
end;

procedure TEnumerableBase<T>.ForEach(const action: TAction<T>);
var
  item: T;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  for item in Self do
    action(item);
end;

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerableBase<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorBase<T>.Create;
end;

function TEnumerableBase<T>.GetEnumeratorNonGeneric: IEnumerator;
begin
  Result := GetEnumerator;
end;

class function TEnumerableBase<T>.GetEqualityComparer: IEqualityComparer<T>;
begin
  if not Assigned(fEqualityComparer) then
    fEqualityComparer := TEqualityComparer<T>.Default;
  Result := fEqualityComparer;
end;

function TEnumerableBase<T>.Last: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  repeat
    Result := enumerator.Current;
  until not enumerator.MoveNext;
end;

function TEnumerableBase<T>.Last(const predicate: TPredicate<T>): T;
var
  item: T;
  found: Boolean;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  found := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      found := True;
      Result := item;
    end;
  end;
  if not found then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  Result := LastOrDefault(Default(T));
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
var
  item: T;
begin
  Result := defaultValue;
  for item in Self do
    Result := item;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := LastOrDefault(predicate, Default(T));
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: TPredicate<T>;
  const defaultValue: T): T;
var
  item: T;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := defaultValue;
  for item in Self do
    if predicate(item) then
      Result := item;
end;

function TEnumerableBase<T>.Max: T;
begin
  Result := Max(Comparer);
end;

function TEnumerableBase<T>.Max(const comparer: IComparer<T>): T;
var
  flag: Boolean;
  item: T;
begin
  Guard.CheckNotNull(Assigned(comparer), 'comparer');

  flag := False;
  for item in Self do
  begin
    if flag then
    begin
      if comparer.Compare(item, Result) > 0 then
        Result := item;
    end
    else
    begin
      flag := True;
      Result := item;
    end;
  end;
  if not flag then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TEnumerableBase<T>.Min: T;
begin
  Result := Min(Comparer);
end;

function TEnumerableBase<T>.Min(const comparer: IComparer<T>): T;
var
  flag: Boolean;
  item: T;
begin
  Guard.CheckNotNull(Assigned(comparer), 'comparer');

  flag := False;
  for item in Self do
  begin
    if flag then
    begin
      if fComparer.Compare(item, Result) < 0 then
        Result := item;
    end
    else
    begin
      flag := True;
      Result := item;
    end;
  end;
  if not flag then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

//function TEnumerableBase<T>.Ordered: IEnumerable<T>;
//var
//  comparer: IComparer<T>;
//begin
//  comparer := TComparer<T>.Default;
//  Result := TOrderedIterator<T>.Create(Self, comparer);
//end;
//
//function TEnumerableBase<T>.Ordered(
//  const comparison: TComparison<T>): IEnumerable<T>;
//var
//  comparer: IComparer<T>;
//begin
//  Guard.CheckNotNull(Assigned(comparison), 'comparison');
//
//  comparer := TComparer<T>.Construct(comparison);
//  Result := TOrderedIterator<T>.Create(Self, comparer);
//end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
var
  list: IList<T>;
begin
  list := ToList;
  Result := TReversedEnumerable<T>.Create(list);
//  Result := TReversedIterator<T>.Create(Self);
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  Result := enumerator.Current;
  if enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
end;

function TEnumerableBase<T>.Single(const predicate: TPredicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  found: Boolean;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  found := False;
  repeat
    item := enumerator.Current;
    if predicate(item) then
    begin
      if found then
        raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneMatchingElement);
      found := True;
      Result := item;
    end;
  until not enumerator.MoveNext;
  if not found then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.SingleOrDefault: T;
begin
  Result := SingleOrDefault(Default(T));
end;

function TEnumerableBase<T>.SingleOrDefault(const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
  Result := defaultValue;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    Result := enumerator.Current;
    if enumerator.MoveNext then
      raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := SingleOrDefault(predicate, Default(T));
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: TPredicate<T>;
  const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  found: Boolean;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    Exit(Default(T));
  found := False;
  repeat
    item := enumerator.Current;
    if predicate(item) then
    begin
      if found then
        raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneMatchingElement);
      found := True;
      Result := item;
    end;
  until not enumerator.MoveNext;
  if not found then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>;
begin
  Result := TSkipEnumerable<T>.Create(Self, count);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileIndexEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>;
begin
  Result := TTakeEnumerable<T>.Create(Self, count);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileIndexEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
begin
  Result := ToList.ToArray;
end;

function TEnumerableBase<T>.ToList: IList<T>;
begin
  Result := TList<T>.Create;
  Result.AddRange(Self);
end;

function TEnumerableBase<T>.ToSet: ISet<T>;
begin
  Result := THashSet<T>.Create;
  Result.AddRange(Self);
end;

function TEnumerableBase<T>.TryGetElementAt(out value: T;
  index: Integer): Boolean;
var
  item: T;
begin
  if index < 0 then
    Exit(False);
  for item in Self do
  begin
    if index = 0 then
    begin
      value := item;
      Exit(True);
    end;
    Dec(index);
  end;
  Result := False;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  Result := False;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    value := enumerator.Current;
    Result := True;
  end;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      value := item;
      Exit(True);
    end;
  end;
end;

function TEnumerableBase<T>.TryGetLast(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  Result := False;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    repeat
      value := enumerator.Current;
    until not enumerator.MoveNext;
    Result := True;
  end;
end;

function TEnumerableBase<T>.TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      value := item;
      Result := True;
    end;
  end;
end;

function TEnumerableBase<T>.TryGetSingle(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  Result := False;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    value := enumerator.Current;
    Result := not enumerator.MoveNext;
  end;
end;

function TEnumerableBase<T>.TryGetSingle(out value: T;
  const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      if Result then
        Exit(False);
      value := item;
      Result := True;
    end;
  end;
end;

function TEnumerableBase<T>.Where(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TWhereEnumerable<T>.Create(Self, predicate);
end;

{$ENDREGION}


{$REGION 'TCollectionBase<T>'}

procedure TCollectionBase<T>.AddRange(const collection: array of T);
var
  item: T;
begin
  for item in collection do
  begin
    Add(item);
  end;
end;

procedure TCollectionBase<T>.AddRange(const collection: IEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Add(item);
  end;
end;

procedure TCollectionBase<T>.AddRange(const collection: TEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Add(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const collection: array of T);
var
  item: T;
begin
  for item in collection do
  begin
    Remove(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const collection: TEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Remove(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const collection: IEnumerable<T>);
var
  item: T;
begin
  for item in collection do
  begin
    Remove(item);
  end;
end;

function TCollectionBase<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

{$ENDREGION}


{$REGION 'TContainedCollectionBase<T>'}

constructor TContainedCollectionBase<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedCollectionBase<T>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedCollectionBase<T>._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TContainedCollectionBase<T>._Release: Integer;
begin
  Result := IInterface(FController)._Release;
end;

{$ENDREGION}


{$REGION 'TListBase<T>'}

constructor TListBase<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TListBase<T>.Create(const collection: array of T);
begin
  Create;
  AddRange(collection);
end;

constructor TListBase<T>.Create(const collection: IEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

constructor TListBase<T>.Create(const collection: TEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

destructor TListBase<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TListBase<T>.Remove(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item);
  Result := index > -1;
  if Result then
  begin
    DoDelete(index, caRemoved);
  end;
end;

procedure TListBase<T>.DoSort(const comparer: IComparer<T>);
begin
end;

function TListBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TReversedEnumerable<T>.Create(Self);
end;

function TListBase<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := Items[index];
    DoDelete(index, caExtracted);
  end;
end;

procedure TListBase<T>.Delete(index: Integer);
begin
  Guard.CheckRange((index >= 0) and (index < Count), 'index');

  DoDelete(index, caRemoved);
end;

procedure TListBase<T>.DeleteRange(startIndex, count: Integer);
begin
  if (startIndex < 0) or
    (count < 0) or
    (startIndex + count > Self.Count) or
    (startIndex + count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRangeException);

  if count = 0 then
    Exit;

  DoDeleteRange(startIndex, count, caRemoved);
end;

procedure TListBase<T>.Add(const item: T);
begin
  Insert(Count, item);
end;

procedure TListBase<T>.Clear;
begin
  if Count > 0 then
  begin
    DeleteRange(0, Count);
  end;
end;

function TListBase<T>.Contains(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item);
  Result := index > -1;
end;

function TListBase<T>.IndexOf(const item: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if fComparer.Compare(Items[i], item) = 0 then
      Exit(i);
  end;
  Result := -1;
end;

procedure TListBase<T>.InsertRange(index: Integer; const collection: array of T);
var
  item: T;
begin
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.InsertRange(index: Integer;
  const collection: IEnumerable<T>);
var
  item: T;
begin
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.InsertRange(index: Integer;
  const collection: TEnumerable<T>);
var
  item: T;
begin
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.Insert(index: Integer; const item: T);
begin
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');

  DoInsert(index, item);
end;

function TListBase<T>.LastIndexOf(const item: T): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if fComparer.Compare(Items[i], item) = 0 then
      Exit(i);
  end;
  Result := -1;
end;

procedure TListBase<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  fOnChanged.Invoke(Self, item, action);
end;

function TListBase<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := Items[i];
  end;
end;

function TListBase<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TListBase<T>.TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean;
begin
  if not Assigned(predicate) then
  begin
    Result := Count > 0;
    if Result then
      value := Items[0];
  end
  else
    Result := inherited;
end;

function TListBase<T>.TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean;
begin
  if not Assigned(predicate) then
  begin
    Result := Count > 0;
    if Result then
      value := Items[Count - 1];
  end
  else
    Result := inherited;
end;

function TListBase<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

procedure TListBase<T>.Sort;
begin
  DoSort(fComparer);
end;

procedure TListBase<T>.Sort(const comparer: IComparer<T>);
begin
  DoSort(comparer);
end;

procedure TListBase<T>.Sort(const comparison: TComparison<T>);
var
  comparer: IComparer<T>;
begin
  comparer := TComparer<T>.Construct(comparison);
  DoSort(comparer);
end;

{$ENDREGION}


{$REGION 'TListBase<T>.TEnumerator'}

constructor TListBase<T>.TEnumerator.Create(const list: TListBase<T>);
begin
  inherited Create;
  fList := list;
  fIndex := -1;
end;

function TListBase<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fList.Count - 1;
  if Result then
    Inc(fIndex);
end;

function TListBase<T>.TEnumerator.GetCurrent: T;
begin
  Result := fList[fIndex];
end;

{$ENDREGION}


end.
