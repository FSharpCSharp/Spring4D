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

unit Spring.Collections.Extensions;

{$I Spring.inc}

interface

uses
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  TEmptyEnumerable<T> = class(TEnumerableBase<T>);

  TBuffer<T> = record
  private
    items: TArray<T>;
    count: Integer;
  public
    constructor Create(source: IEnumerable<T>);
  end;

  TIteratorBase<T> = class(TEnumerableBase<T>, IEnumerator)
  protected
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
  public
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;
  end;

  TIterator<T> = class(TIteratorBase<T>, IEnumerator<T>)
  private
    fInitialThreadId: Cardinal;
  protected
    fState: Integer;
    fCurrent: T;
    const
      STATE_INITIAL    = -2; // initial state, before GetEnumerator
      STATE_FINISHED   = -1; // end of enumerator
      STATE_ENUMERATOR = 0;  // before calling MoveNext
      STATE_RUNNING    = 1;  // enumeration is running
  protected
    function Clone: TIterator<T>; virtual; abstract;
    function GetCurrent: T;
    function GetCurrentNonGeneric: TValue; override; final;
  public
    constructor Create; override;
    function GetEnumerator: IEnumerator<T>; override; final;
  end;

  TArrayIterator<T> = class(TIterator<T>)
  private
    fValues: TArray<T>;
    fIndex: Integer;
  public
    constructor Create(const values: array of T); overload;
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  ///	<summary>
  ///	  The adapter implementation for
  ///	  <see cref="Spring.Collections|IEnumerator&lt;T&gt;" />.
  ///	</summary>
  TEnumeratorAdapter<T> = class(TEnumeratorBase<T>)
  private
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
      TGenericEnumerator = Generics.Collections.TEnumerator<T>;
  private
    fSource: TGenericEnumerable;
    fEnumerator: TGenericEnumerator;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(source: TGenericEnumerable);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    property Current: T read GetCurrent;
  end;

  ///	<summary>
  ///	  The adapter implementation for
  ///	  <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />.
  ///	</summary>
  TEnumerableAdapter<T> = class(TEnumerableBase<T>)
  private
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
  private
    fSource: TGenericEnumerable;
  public
    constructor Create(source: TGenericEnumerable);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TEnumeratorDecorator<T> = class abstract(TEnumeratorBase<T>)
  private
    fEnumerator: IEnumerator<T>;
  protected
    function GetCurrent: T; override;
    property Enumerator: IEnumerator<T> read fEnumerator;
  public
    constructor Create(const enumerator: IEnumerator<T>);
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  TEnumerableDecorator<T> = class abstract(TEnumerableBase<T>)
  private
    fCollection: IEnumerable<T>;
  protected
    property Collection: IEnumerable<T> read fCollection;
  public
    constructor Create(const collection: IEnumerable<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TEnumeratorWithPredicate<T> = class(TEnumeratorDecorator<T>)
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
    function MoveNext: Boolean; override;
  end;

  TWhereEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TPredicate<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TSkipEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fEnumerator: IEnumerator<T>;
        fCount: Integer;
        fSkipped: Boolean;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const enumerator: IEnumerator<T>; count: Integer);
        function MoveNext: Boolean; override;
      end;
  private
    fCount: Integer;
  public
    constructor Create(const collection: IEnumerable<T>; count: Integer);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TSkipWhileEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fEnumerator: IEnumerator<T>;
        fPredicate: TPredicate<T>;
        fSkipped: Boolean;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TPredicate<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TSkipWhileIndexEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fEnumerator: IEnumerator<T>;
        fPredicate: TFunc<T, Integer, Boolean>;
        fSkipped: Boolean;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const enumerator: IEnumerator<T>; const predicate: TFunc<T, Integer, Boolean>);
        function MoveNext: Boolean; override;
      end;
  private
    fPredicate: TFunc<T, Integer, Boolean>;
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TTakeEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fEnumerator: IEnumerator<T>;
        fCount: Integer;
        fTakenCount: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const enumerator: IEnumerator<T>; count: Integer);
        function MoveNext: Boolean; override;
      end;
  private
    fCount: Integer;
  public
    constructor Create(const collection: IEnumerable<T>; count: Integer);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TTakeWhileEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fEnumerator: IEnumerator<T>;
        fPredicate: TPredicate<T>;
        fStopped: Boolean;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fPredicate: TPredicate<T>;
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TPredicate<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TTakeWhileIndexEnumerable<T> = class(TEnumerableDecorator<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fEnumerator: IEnumerator<T>;
        fPredicate: TFunc<T, Integer, Boolean>;
        fStopped: Boolean;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const enumerator: IEnumerator<T>; const predicate: TFunc<T, Integer, Boolean>);
        function MoveNext: Boolean; override;
      end;
  private
    fPredicate: TFunc<T, Integer, Boolean>;
  public
    constructor Create(const collection: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TConcatEnumerable<T> = class(TEnumerableBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fFirst: IEnumerator<T>;
        fSecond: IEnumerator<T>;
        fCurrentEnumerator: IEnumerator<T>;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const first, second: IEnumerable<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fFirst: IEnumerable<T>;
    fSecond: IEnumerable<T>;
  protected
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
  public
    constructor Create(const first, second: IEnumerable<T>);
    function GetEnumerator: IEnumerator<T>; override;
    function TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean; override;
    function TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean; override;
  end;

  TReversedEnumerable<T> = class(TEnumerableBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: IList<T>;
        fCount: Integer;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: IList<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fList: IList<T>;
  public
    constructor Create(const list: IList<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

implementation

uses
  Classes,
  Spring.ResourceStrings;


{$REGION 'TBuffer<T>'}

constructor TBuffer<T>.Create(source: IEnumerable<T>);
var
  item: T;
  collection: ICollection<T>;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  items := nil;
  count := 0;
  if Supports(source, ICollection<T>, collection) then
  begin
    count := collection.Count;
    if count > 0 then
    begin
      SetLength(items, count);
      collection.CopyTo(items, 0);
    end;
  end
  else
  begin
    for item in source do
    begin
      if items = nil then
        SetLength(items, 4)
      else if Length(items) = count then
        SetLength(items, count * 2);
      items[count] := item;
      Inc(count);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TIteratorBase<T>' }

function TIteratorBase<T>.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TIteratorBase<T>.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TIterator<T>'}

constructor TIterator<T>.Create;
begin
  inherited Create;
  fState := STATE_INITIAL;
  fInitialThreadId := TThread.CurrentThread.ThreadID;
end;

function TIterator<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TIterator<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(GetCurrent);
end;

function TIterator<T>.GetEnumerator: IEnumerator<T>;
var
  iterator: TIterator<T>;
begin
  if (fInitialThreadId = TThread.CurrentThread.ThreadID) and (fState = STATE_INITIAL) then
  begin
    fState := STATE_ENUMERATOR;
    Result := Self;
  end
  else
  begin
    iterator := Clone;
    iterator.fState := STATE_ENUMERATOR;
    Result := iterator;
  end;
end;

{$ENDREGION}


{$REGION 'TArrayIterator<T>'}

constructor TArrayIterator<T>.Create(const values: array of T);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fValues, Length(values));
  for i := 0 to High(values) do
    fValues[i] := values[i];
end;

function TArrayIterator<T>.Clone: TIterator<T>;
begin
  Result := TArrayIterator<T>.Create(fValues);
end;

function TArrayIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < Length(fValues) - 1 then
    begin
      Inc(fIndex);
      fCurrent := fValues[fIndex];
      Result := True;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(source: TGenericEnumerable);
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  inherited Create;
  fSource := source;
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
  if not Assigned(fEnumerator) then
    fEnumerator := fSource.GetEnumerator;
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TEnumerableAdapter<T>'}

constructor TEnumerableAdapter<T>.Create(source: TGenericEnumerable);
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  inherited Create;
  fSource := source;
end;

function TEnumerableAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fSource);
end;

{$ENDREGION}


{$REGION 'TEnumerableDecorator<T>'}

constructor TEnumerableDecorator<T>.Create(const collection: IEnumerable<T>);
begin
  inherited Create;
  fCollection := collection;
end;

function TEnumerableDecorator<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fCollection.GetEnumerator;
end;

{$ENDREGION}


{$REGION 'TEnumeratorDecorator<T>'}

constructor TEnumeratorDecorator<T>.Create(const enumerator: IEnumerator<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
end;

function TEnumeratorDecorator<T>.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorDecorator<T>.MoveNext: Boolean;
begin
  Result := fEnumerator.MoveNext;
end;

procedure TEnumeratorDecorator<T>.Reset;
begin
  fEnumerator.Reset;
end;

{$ENDREGION}


{$REGION 'TEnumeratorWithPredicate<T>'}

constructor TEnumeratorWithPredicate<T>.Create(
  const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
begin
  inherited Create(enumerator);
  fPredicate := predicate;
end;

function TEnumeratorWithPredicate<T>.MoveNext: Boolean;
begin
  Result := Enumerator.MoveNext;
  while Result and not fPredicate(Enumerator.Current) do
  begin
    Result := Enumerator.MoveNext;
  end;
end;

{$ENDREGION}


{$REGION 'TEnumerableWithPredicate'}

constructor TWhereEnumerable<T>.Create(
  const collection: IEnumerable<T>; const predicate: TPredicate<T>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TWhereEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorWithPredicate<T>.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}


{$REGION 'TSkipEnumerable<T>'}

constructor TSkipEnumerable<T>.Create(const collection: IEnumerable<T>;
  count: Integer);
begin
  inherited Create(collection);
  fCount := count;
end;

function TSkipEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Collection.GetEnumerator, fCount);
end;

{$ENDREGION}


{$REGION 'TSkipEnumerable<T>.TEnumerator'}

constructor TSkipEnumerable<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>; count: Integer);
begin
  inherited Create;
  fEnumerator := enumerator;
  fCount := count;
end;

function TSkipEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  if not fSkipped then
    raise EInvalidOperationException.Create('GetCurrent');
  Result := fEnumerator.Current;
end;

function TSkipEnumerable<T>.TEnumerator.MoveNext: Boolean;
var
  n: Integer;
begin
  if fSkipped then
  begin
    Result := fEnumerator.MoveNext;
  end
  else
  begin
    n := 0;
    while not fSkipped and fEnumerator.MoveNext do
    begin
      Inc(n);
      fSkipped := n > fCount;
    end;
    Result := fSkipped;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhileEnumerable<T>'}

constructor TSkipWhileEnumerable<T>.Create(const collection: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TSkipWhileEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}


{$REGION 'TSkipWhileEnumerable<T>.TEnumerator'}

constructor TSkipWhileEnumerable<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
  fPredicate := predicate;
end;

function TSkipWhileEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  if not fSkipped then
    raise EInvalidOperationException.Create('GetCurrent');
  Result := fEnumerator.Current;
end;

function TSkipWhileEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  if fSkipped then
  begin
    Result := fEnumerator.MoveNext;
  end
  else
  begin
    while not fSkipped and fEnumerator.MoveNext do
    begin
      fSkipped := not fPredicate(fEnumerator.Current);
    end;
    Result := fSkipped;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhile2Enumerable<T>'}

constructor TSkipWhileIndexEnumerable<T>.Create(const collection: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TSkipWhileIndexEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}


{$REGION 'TSkipWhile2Enumerable<T>.TEnumerator'}

constructor TSkipWhileIndexEnumerable<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  inherited Create;
  fEnumerator := enumerator;
  fPredicate := predicate;
end;

function TSkipWhileIndexEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  if not fSkipped then
    raise EInvalidOperationException.Create('GetCurrent');
  Result := fEnumerator.Current;
end;

function TSkipWhileIndexEnumerable<T>.TEnumerator.MoveNext: Boolean;
var
  index: Integer;
begin
  if fSkipped then
  begin
    Result := fEnumerator.MoveNext;
  end
  else
  begin
    index := 0;
    while not fSkipped and fEnumerator.MoveNext do
    begin
      fSkipped := not fPredicate(fEnumerator.Current, index);
      Inc(index);
    end;
    Result := fSkipped;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeEnumerable<T>'}

constructor TTakeEnumerable<T>.Create(const collection: IEnumerable<T>;
  count: Integer);
begin
  inherited Create(collection);
  fCount := count;
end;

function TTakeEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Collection.GetEnumerator, fCount);
end;

{$ENDREGION}


{$REGION 'TTakeEnumerable<T>.TEnumerator'}

constructor TTakeEnumerable<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>; count: Integer);
begin
  inherited Create;
  fEnumerator := enumerator;
  fCount := count;
end;

function TTakeEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TTakeEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := (fTakenCount < fCount) and fEnumerator.MoveNext;
  if Result then
  begin
    Inc(fTakenCount);
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhileEnumerable<T>'}

constructor TTakeWhileEnumerable<T>.Create(const collection: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TTakeWhileEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}


{$REGION 'TTakeWhileEnumerable<T>.TEnumerator'}

constructor TTakeWhileEnumerable<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>; const predicate: TPredicate<T>);
begin
  inherited Create;
  fEnumerator := enumerator;
  fPredicate := predicate;
end;

function TTakeWhileEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  if fStopped then
    raise EInvalidOperationException.Create('GetCurrent');
  Result := fEnumerator.Current;
end;

function TTakeWhileEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := not fStopped;
  if Result then
  begin
    fStopped := not fEnumerator.MoveNext or not fPredicate(fEnumerator.Current);
    Result := not fStopped;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIndexEnumerable<T>'}

constructor TTakeWhileIndexEnumerable<T>.Create(
  const collection: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  inherited Create(collection);
  fPredicate := predicate;
end;

function TTakeWhileIndexEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Collection.GetEnumerator, fPredicate);
end;

{$ENDREGION}


{$REGION 'TTakeWhileIndexEnumerable<T>.TEnumerator'}

constructor TTakeWhileIndexEnumerable<T>.TEnumerator.Create(
  const enumerator: IEnumerator<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  inherited Create;
  fEnumerator := enumerator;
  fPredicate := predicate;
  fIndex := -1;
end;

function TTakeWhileIndexEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  if fStopped then
    raise EInvalidOperationException.Create('GetCurrent');
  Result := fEnumerator.Current;
end;

function TTakeWhileIndexEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := not fStopped;
  if Result then
  begin
    Inc(fIndex);
    fStopped := not fEnumerator.MoveNext or not fPredicate(fEnumerator.Current, fIndex);
    Result := not fStopped;
  end;
end;

{$ENDREGION}


{$REGION 'TConcatEnumerable<T>'}

constructor TConcatEnumerable<T>.Create(const first, second: IEnumerable<T>);
begin
  inherited Create;
  fFirst := first;
  fSecond := second;
end;

function TConcatEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(fFirst, fSecond);
end;

function TConcatEnumerable<T>.GetCount: Integer;
begin
  Result := fFirst.Count + fSecond.Count;
end;

function TConcatEnumerable<T>.GetIsEmpty: Boolean;
begin
  Result := fFirst.IsEmpty and fSecond.IsEmpty;
end;

function TConcatEnumerable<T>.TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean;
begin
  Result := fFirst.TryGetFirst(value, predicate) or fSecond.TryGetFirst(value, predicate);
end;

function TConcatEnumerable<T>.TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean;
begin
  Result := fSecond.TryGetLast(value, predicate) or fFirst.TryGetLast(value, predicate);
end;

{$ENDREGION}


{$REGION 'TConcatEnumerable<T>.TEnumerator'}

constructor TConcatEnumerable<T>.TEnumerator.Create(const first,
  second: IEnumerable<T>);
begin
  inherited Create;
  fFirst := first.GetEnumerator;
  fSecond := second.GetEnumerator;
  fCurrentEnumerator := fFirst;
end;

function TConcatEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrentEnumerator.Current;
end;

function TConcatEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fCurrentEnumerator.MoveNext;

  if not Result and (fCurrentEnumerator = fFirst) then
  begin
    fCurrentEnumerator := fSecond;
    Result := fCurrentEnumerator.MoveNext;
  end;
end;

{$ENDREGION}


{$REGION 'TReversedEnumerable<T>'}

constructor TReversedEnumerable<T>.Create(const list: IList<T>);
begin
  inherited Create;
  fList := list;
end;

function TReversedEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(fList);
end;

{$ENDREGION}


{$REGION 'TReversedEnumerable<T>.TEnumerator'}

constructor TReversedEnumerable<T>.TEnumerator.Create(const list: IList<T>);
begin
  inherited Create;
  fList := list;
  fCount := fList.Count;
  fIndex := fCount;
end;

function TReversedEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fList[fIndex];
end;

function TReversedEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex > 0) and (fIndex <= fCount);
  Dec(fIndex);
end;

{$ENDREGION}

end.
