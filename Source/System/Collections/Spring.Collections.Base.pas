{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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
  SysUtils,
  TypInfo,
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections;

type

  {$REGION 'Documentation'}
  ///	<summary>Provides an abstract implementation for the <see cref=
  ///	"IEnumerator{T}">IEnumerator&lt;T&gt;</see> interface.</summary>
  {$ENDREGION}
  TEnumeratorBase<T> = class abstract(TInterfacedObject, IEnumerator<T>, IInterface)
  protected
    function GetCurrent: T; virtual; abstract;
  public
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;
    property Current: T read GetCurrent;
  end;

  /// <summary>
  /// Provides a default implementation for <c>IEnumerable(T)</c> (Extension Methods).
  /// </summary>
  TEnumerableBase<T> = class abstract(TInterfacedObject, IEnumerable<T>, IElementType)
  protected
  {$REGION 'Implements IElementType'}
    function GetElementType: PTypeInfo;
  {$ENDREGION}
    function GetComparer: IComparer<T>; virtual;
    function GetCount: Integer; virtual;
    function GetIsEmpty: Boolean; virtual;
  public
    function GetEnumerator: IEnumerator<T>; virtual; abstract;
    function AsObject: TObject;
    function TryGetFirst(out value: T): Boolean; virtual;
    function TryGetLast(out value: T): Boolean; virtual;
    function First: T; overload; virtual;
    function First(const predicate: TPredicate<T>): T; overload; virtual;
    function FirstOrDefault: T; overload; virtual;
    function FirstOrDefault(const defaultValue: T): T; overload;
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function Last: T; overload; virtual;
    function Last(const predicate: TPredicate<T>): T; overload; virtual;
    function LastOrDefault: T; overload; virtual;
    function LastOrDefault(const defaultValue: T): T; overload;
    function LastOrDefault(const predicate: TPredicate<T>): T; overload; virtual;
    function Single: T; overload;
    function Single(const predicate: TPredicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const predicate: TPredicate<T>): T; overload;
    function ElementAt(index: Integer): T;
    function ElementAtOrDefault(index: Integer): T;
    function Min: T;
    function Max: T;
    function Contains(const item: T): Boolean; overload; virtual;
    function Contains(const item: T; const comparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function All(const predicate: TPredicate<T>): Boolean;
    function Any(const predicate: TPredicate<T>): Boolean;
    function Where(const predicate: TPredicate<T>): IEnumerable<T>; virtual;
    function Skip(count: Integer): IEnumerable<T>;
    function SkipWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function SkipWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;
    function Take(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;
    function Concat(const collection: IEnumerable<T>): IEnumerable<T>;
    function Reversed: IEnumerable<T>; virtual;
    procedure ForEach(const action: TAction<T>); overload;
    procedure ForEach(const action: TActionProc<T>); overload;
    procedure ForEach(const action: TActionMethod<T>); overload;
    function EqualsTo(const collection: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const collection: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>; virtual;
    function ToList: IList<T>; virtual;
    function ToSet: ISet<T>; virtual;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  /// Provides an abstract class base for <c>IColleciton{T}</c>.
  /// </summary>
  /// <remarks>
  ///   Notes: The Add/Remove/Clear methods are abstract. IsReadOnly returns False by default.
  /// </remarks>
  TCollectionBase<T> = class abstract(TEnumerableBase<T>, ICollection<T>)
  protected
    function GetIsReadOnly: Boolean; virtual;
  public
    procedure Add(const item: T); virtual; abstract;
    procedure AddRange(const collection: array of T); overload; virtual;
    procedure AddRange(const collection: IEnumerable<T>); overload; virtual;
    procedure AddRange(const collection: TEnumerable<T>); overload; virtual;

    function  Remove(const item: T): Boolean; virtual; abstract;
    procedure RemoveRange(const collection: array of T); overload; virtual;
    procedure RemoveRange(const collection: IEnumerable<T>); overload; virtual;
    procedure RemoveRange(const collection: TEnumerable<T>); overload; virtual;

    procedure Clear; virtual; abstract;

    /// <value>Returns false, by default.</value>
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TContainedCollectionBase<T> = class(TCollectionBase<T>)
  private
    fController: Pointer;
    function GetController: IInterface;
  protected

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
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

      TReversedEnumerable = class(TEnumerableBase<T>)
      private
        fList: TListBase<T>;
        fReference: IInterface;
      public
        constructor Create(const list: TListBase<T>);
        function GetEnumerator: IEnumerator<T>; override;
      end;

      TReversedEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TListBase<T>;
        fCount: Integer;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TListBase<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fComparer: IComparer<T>;
    fOnNotify: ICollectionNotifyDelegate<T>;
    function GetOnNotify: ICollectionNotifyDelegate<T>;
  protected
    function GetComparer: IComparer<T>; override;
    procedure Notify(const item: T; action: TCollectionNotification); virtual;
    procedure DoSort(const comparer: IComparer<T>); virtual;
    procedure DoInsert(index: Integer; const item: T); virtual; abstract;
    procedure DoDelete(index: Integer; notification: TCollectionNotification); virtual; abstract;
    procedure DoDeleteRange(index, count: Integer; notification: TCollectionNotification); virtual; abstract;
    function GetItem(index: Integer): T; virtual; abstract;
    procedure SetItem(index: Integer; const value: T); virtual; abstract;
  public
    constructor Create; overload;
    constructor Create(const comparer: IComparer<T>); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(const collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function TryGetFirst(out value: T): Boolean; override;
    function TryGetLast(out value: T): Boolean; override;
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
    property OnNotify: ICollectionNotifyDelegate<T> read GetOnNotify;
  end;

implementation

uses
  Spring.ResourceStrings,
  Spring.Collections.Sets,
  Spring.Collections.Lists,
  Spring.Collections.Extensions;

{$REGION 'TEnumeratorBase<T>'}

function TEnumeratorBase<T>.MoveNext: Boolean;
begin
  Result := False;
end;

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
  TArgument.CheckNotNull<T>(item, 'item');

  comparer := TEqualityComparer<T>.Default;
  Result := Contains(item, comparer);
end;

function TEnumerableBase<T>.All(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := True;
  for item in Self do
  begin
    if not predicate(item) then
      Exit(False);
  end;
end;

function TEnumerableBase<T>.Any(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := False;
  for item in Self do
  begin
    if predicate(item) then
      Exit(True);
  end;
end;

function TEnumerableBase<T>.AsObject: TObject;
begin
  Result := Self;
end;

function TEnumerableBase<T>.Concat(
  const collection: IEnumerable<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(collection), 'collection');

  Result := TConcatEnumerable<T>.Create(Self, collection);
end;

function TEnumerableBase<T>.Contains(const item: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  TArgument.CheckNotNull<T>(item, 'item');

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

function TEnumerableBase<T>.ElementAt(index: Integer): T;
var
  enumerator: IEnumerator<T>;
  localIndex: Integer;
begin
  TArgument.CheckRange(index >= 0, 'index');

  enumerator := GetEnumerator;
  localIndex := 0;
  while enumerator.MoveNext do
  begin
    if localIndex = index then
    begin
      Exit(enumerator.Current);
    end;
    Inc(localIndex);
  end;
  TArgument.RaiseArgumentOutOfRangeException('index');
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
var
  enumerator: IEnumerator<T>;
  localIndex: Integer;
begin
  TArgument.CheckRange(index >= 0, 'index');

  enumerator := GetEnumerator;
  localIndex := 0;
  while enumerator.MoveNext do
  begin
    if localIndex = index then
    begin
      Exit(enumerator.Current);
    end;
    Inc(localIndex);
  end;
  Result := Default(T);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>): Boolean;
begin
  Result := EqualsTo(collection, TEqualityComparer<T>.Default);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
var
  e1, e2: IEnumerator<T>;
  hasNext: Boolean;
begin
  TArgument.CheckNotNull(Assigned(collection), 'collection');
  TArgument.CheckNotNull(Assigned(comparer), 'comparer');

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
    begin
      Exit(False);
    end;
  end;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
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
  enumerator: IEnumerator<T>;
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
    raise EInvalidOperationException.Create('First');  // TEMP
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

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
begin
  if not TryGetFirst(Result) then
  begin
    Result := defaultValue;
  end;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).FirstOrDefault; // TEMP
end;

procedure TEnumerableBase<T>.ForEach(const action: TAction<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(action), 'action');

  for item in Self do
  begin
    action(item);
  end;
end;

procedure TEnumerableBase<T>.ForEach(const action: TActionProc<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(action), 'action');

  for item in Self do
  begin
    action(item);
  end;
end;

procedure TEnumerableBase<T>.ForEach(const action: TActionMethod<T>);
var
  item: T;
begin
  TArgument.CheckNotNull(Assigned(action), 'action');

  for item in Self do
  begin
    action(item);
  end;
end;

function TEnumerableBase<T>.Last: T;
begin
  if not TryGetLast(Result) then
  begin
    raise EInvalidOperationException.Create('Last');  // TEMP
  end;
end;

function TEnumerableBase<T>.Last(const predicate: TPredicate<T>): T;
begin
  Result := Where(predicate).Last;
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
begin
  if not TryGetLast(Result) then
  begin
    Result := defaultValue;
  end;
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
  Result := Where(predicate).LastOrDefault;
end;

function TEnumerableBase<T>.Max: T;
var
  comparer: IComparer<T>;
  hasElement: Boolean;
  item: T;
begin
  comparer := GetComparer;
  hasElement := False;
  for item in Self do
  begin
    if hasElement then
    begin
      if comparer.Compare(item, Result) > 0 then
      begin
        Result := item;
      end;
    end
    else
    begin
      hasElement := True;
      Result := item;
    end;
  end;
  if not hasElement then
  begin
    raise EInvalidOperationException.CreateRes(@SSequenceIsEmpty);
  end;
end;

function TEnumerableBase<T>.Min: T;
var
  comparer: IComparer<T>;
  hasElement: Boolean;
  item: T;
begin
  comparer := GetComparer;
  hasElement := False;
  for item in Self do
  begin
    if hasElement then
    begin
      if comparer.Compare(item, Result) < 0 then
      begin
        Result := item;
      end;
    end
    else
    begin
      hasElement := True;
      Result := item;
    end;
  end;
  if not hasElement then
  begin
    raise EInvalidOperationException.CreateRes(@SSequenceIsEmpty);
  end;
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
var
  list: IList<T>;
begin
  list := TList<T>.Create;
  list.AddRange(Self);
  list.Reverse;
  Result := list;
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
  begin
    raise EInvalidOperationException.CreateRes(@SSequenceIsEmpty);
  end;
  Result := enumerator.Current;
  if enumerator.MoveNext then
  begin
    raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TEnumerableBase<T>.Single(const predicate: TPredicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  isSatisfied: Boolean;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  enumerator := GetEnumerator;

  if not enumerator.MoveNext then
  begin
    raise EInvalidOperationException.CreateRes(@SSequenceIsEmpty);
  end;

  Result := enumerator.Current;
  isSatisfied := predicate(Result);

  while enumerator.MoveNext do
  begin
    Result := enumerator.Current;
    if predicate(Result) then
    begin
      if isSatisfied then
      begin
        raise EInvalidOperationException.CreateRes(@SMoreThanOneElementSatisfied);
      end;
      isSatisfied := True;
    end;
  end;
  if not isSatisfied then
  begin
    raise EInvalidOperationException.CreateRes(@SNoElementSatisfiesCondition);
  end;
end;

function TEnumerableBase<T>.SingleOrDefault: T;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
  begin
    Exit(Default(T));
  end;
  Result := enumerator.Current;
  if enumerator.MoveNext then
  begin
    raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: TPredicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  isSatisfied: Boolean;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
  begin
    Exit(Default(T));
  end;

  Result := enumerator.Current;
  isSatisfied := predicate(Result);

  while enumerator.MoveNext do
  begin
    Result := enumerator.Current;
    if predicate(Result) then
    begin
      if isSatisfied then
      begin
        raise EInvalidOperationException.CreateRes(@SMoreThanOneElementSatisfied);
      end;
      isSatisfied := True;
    end;
  end;

  if not isSatisfied then
  begin
    Result := Default(T);
  end;
end;

function TEnumerableBase<T>.Where(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TWhereEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>;
begin
  Result := TSkipEnumerable<T>.Create(Self, count);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileIndexEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>;
begin
  Result := TTakeEnumerable<T>.Create(Self, count);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileEnumerable<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
  TArgument.CheckNotNull(Assigned(predicate), 'predicate');

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

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := TComparer<T>.Default;
end;

function TEnumerableBase<T>.GetCount: Integer;
var
  enumerator: IEnumerator<T>;
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
  Result := Count = 0;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
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
  Result := Controller._AddRef;
end;

function TContainedCollectionBase<T>._Release: Integer;
begin
  Result := Controller._Release;
end;

{$ENDREGION}


{$REGION 'TListBase<T>'}

constructor TListBase<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TListBase<T>.Create(const comparer: IComparer<T>);
begin
  inherited Create;
  fComparer := comparer;
  if fComparer = nil then
    fComparer := TComparer<T>.Default;
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
    DoDelete(index, cnRemoved);
  end;
end;

procedure TListBase<T>.DoSort(const comparer: IComparer<T>);
begin
end;

function TListBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TReversedEnumerable.Create(Self);
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
    DoDelete(index, cnExtracted);
  end;
end;

procedure TListBase<T>.Delete(index: Integer);
begin
  TArgument.CheckRange((index >= 0) and (index < Count), 'index');

  DoDelete(index, cnRemoved);
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

  DoDeleteRange(startIndex, count, cnRemoved);
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
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

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
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

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
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.Insert(index: Integer; const item: T);
begin
  TArgument.CheckRange((index >= 0) and (index <= Count), 'index');

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

procedure TListBase<T>.Notify(const item: T; action: TCollectionNotification);
begin
  if (fOnNotify <> nil) and not fOnNotify.IsEmpty and fOnNotify.Enabled then
  begin
    fOnNotify.Invoke(Self, item, action);
  end;
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

function TListBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TListBase<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Items[0];
end;

function TListBase<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Items[Count - 1];
end;

function TListBase<T>.GetOnNotify: ICollectionNotifyDelegate<T>;
begin
  if fOnNotify = nil then
  begin
    fOnNotify := TCollectionNotifyDelegate<T>.Create;
  end;
  Result := fOnNotify;
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


{$REGION 'TListBase<T>.TReversedEnumerator'}

constructor TListBase<T>.TReversedEnumerator.Create(const list: TListBase<T>);
begin
  inherited Create;
  fList := list;
  fCount := fList.Count;
  fIndex := fCount;
end;

function TListBase<T>.TReversedEnumerator.GetCurrent: T;
begin
  Result := fList[fIndex];
end;

function TListBase<T>.TReversedEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex > 0) and (fIndex <= fCount);
  Dec(fIndex);
end;

{ TListBase<T>.TReversedEnumerable }

constructor TListBase<T>.TReversedEnumerable.Create(const list: TListBase<T>);
begin
  inherited Create;
  fList := list;
  fReference := list;
end;

function TListBase<T>.TReversedEnumerable.GetEnumerator: IEnumerator<T>;
begin
  Result := TReversedEnumerator.Create(fList);
end;

{$ENDREGION}

end.
