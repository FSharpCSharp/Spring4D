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

  TWhereIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: TPredicate<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TWhereIndexIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: TFunc<T, Integer, Boolean>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSkipIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fCount: Integer;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>; count: Integer);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSkipWhileIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
    fYielding: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TPredicate<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSkipWhileIndexIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fYielding: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TTakeIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fCount: Integer;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>; count: Integer);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TTakeWhileIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
    fStopped: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TPredicate<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TTakeWhileIndexIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fStopped: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TConcatIterator<T> = class(TIterator<T>)
  private
    fFirst: IEnumerable<T>;
    fSecond: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
    fFlag: Boolean;
  public
    constructor Create(const first, second: IEnumerable<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TReversedIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fBuffer: TBuffer<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
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


{$REGION 'TWhereIterator<T>'}

constructor TWhereIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TWhereIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIterator<T>.Create(fSource, fPredicate);
end;

function TWhereIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fPredicate(current) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TWhereIndexIterator<T>'}

constructor TWhereIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TWhereIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIndexIterator<T>.Create(fSource, fPredicate);
end;

function TWhereIndexIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      if fPredicate(current, fIndex) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipIterator<T>'}

constructor TSkipIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  inherited Create(source.Comparer);
  fSource := source;
  fCount := count;
end;

function TSkipIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipIterator<T>.Create(fSource, fCount);
end;

function TSkipIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fIndex := fCount;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while (fIndex > 0) and fEnumerator.MoveNext do
      Dec(fIndex);
    if fEnumerator.MoveNext then
    begin
      fCurrent := fEnumerator.Current;
      Result := True;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIterator<T>'}

constructor TSkipWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  inherited Create(source.Comparer);;
  fSource := source;
  fPredicate := predicate;
end;

function TSkipWhileIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipWhileIterator<T>.Create(fSource, fPredicate);
end;

function TSkipWhileIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if not fYielding and not fPredicate(current) then
        fYielding := True;
      if fYielding then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIndexIterator<T>'}

constructor TSkipWhileIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  inherited Create(source.Comparer);;
  fSource := source;
  fPredicate := predicate;
end;

function TSkipWhileIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipWhileIndexIterator<T>.Create(fSource, fPredicate);
end;

function TSkipWhileIndexIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fIndex := -1;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      if not fYielding and not fPredicate(current, fIndex) then
        fYielding := True;
      if fYielding then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeIterator<T>'}

constructor TTakeIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  inherited Create(source.Comparer);;
  fSource := source;
  fCount := count;
end;

function TTakeIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeIterator<T>.Create(fSource, fCount);
end;

function TTakeIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fIndex := 0;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while (fIndex < fCount) and fEnumerator.MoveNext do
    begin
      fCurrent := fEnumerator.Current;
      Inc(fIndex);
      Exit(True);
    end;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIterator<T>'}

constructor TTakeWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  inherited Create(source.Comparer);;
  fSource := source;
  fPredicate := predicate;
end;

function TTakeWhileIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeWhileIterator<T>.Create(fSource, fPredicate);
end;

function TTakeWhileIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fStopped and fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fPredicate(current) then
      begin
        fCurrent := current;
        Exit(True);
      end
      else
        fStopped := True;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIndexIterator<T>'}

constructor TTakeWhileIndexIterator<T>.Create(
  const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TTakeWhileIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeWhileIndexIterator<T>.Create(fSource, fPredicate);
end;

function TTakeWhileIndexIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fStopped and fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      if fPredicate(current, findex) then
      begin
        fCurrent := current;
        Exit(True);
      end
      else
        fStopped := True;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TConcatIterator<T>'}

constructor TConcatIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');

  inherited Create;
  fFirst := first;
  fSecond := second;
end;

function TConcatIterator<T>.Clone: TIterator<T>;
begin
  Result := TConcatIterator<T>.Create(fFirst, fSecond);
end;

function TConcatIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fFirst.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    repeat
      if fEnumerator.MoveNext then
      begin
        fCurrent := fEnumerator.Current;
        Result := True;
      end
      else
      begin
        if not fFlag then
        begin
          fEnumerator := fSecond.GetEnumerator;
          fFlag := True;
        end
        else
        begin
          fState := STATE_FINISHED;
          fEnumerator := nil;
          Break;
        end;
      end;
    until Result;
  end;
end;

{$ENDREGION}


{$REGION 'TReversedIterator<T>'}

constructor TReversedIterator<T>.Create(const source: IEnumerable<T>);
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  inherited Create(source.Comparer);
  fSource := source;
end;

function TReversedIterator<T>.Clone: TIterator<T>;
begin
  Result := TReversedIterator<T>.Create(fSource);
end;

function TReversedIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fBuffer := TBuffer<T>.Create(fSource);
    fIndex := fBuffer.count - 1;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if (fIndex >= 0) and (fIndex <= fBuffer.count) then
    begin
      fCurrent := fBuffer.items[fIndex];
      Dec(fIndex);
      Result := True;
    end;
  end;
end;

{$ENDREGION}


end.
