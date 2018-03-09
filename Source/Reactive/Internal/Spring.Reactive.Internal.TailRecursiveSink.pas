{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Reactive.Internal.TailRecursiveSink;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Reactive,
  Spring.Reactive.Concurrency.AsyncLock,
  Spring.Reactive.Internal.Sink;

type
  TTailRecursiveSink<TSource> = class abstract(TSink<TSource>, IObserver<TSource>)
  private
    fIsDisposed: Boolean;
    fSubscription: ISerialDisposable;
    fGate: IAsyncLock;
    fStack: IStack<IEnumerator<IObservable<TSource>>>;
    fLength: IStack<Nullable<Integer>>;
    procedure MoveNext;
    function TryGetEnumerator(const sources: IEnumerable<IObservable<TSource>>;
      out enumerator: IEnumerator<IObservable<TSource>>): Boolean;

    function DisposableCreate: IDisposable;
  protected
    fRecurse: Action;
    function Extract(const source: IObservable<TSource>): IEnumerable<IObservable<TSource>>; virtual; abstract;
    procedure Done; virtual;
    function Fail(const error: Exception): Boolean; virtual;
  public
    procedure Dispose; override;
    function Run(const sources: IEnumerable<IObservable<TSource>>): IDisposable;
    procedure OnNext(const value: TSource); virtual; abstract;
    procedure OnCompleted; virtual; abstract;
  end;

implementation

uses
  Spring.Reactive.Concurrency.SchedulerDefaults,
  Spring.Reactive.Disposables,
  Spring.Reactive.Internal.Helpers;


{$REGION 'TTailRecursiveSink<TSource>'}

procedure TTailRecursiveSink<TSource>.Dispose;
var
  e: IEnumerator<IObservable<TSource>>;
begin
  while fStack.Count > 0 do
  begin
    e := fStack.Pop;
    fLength.Pop;

    e := nil;
  end;

  fIsDisposed := True;

  inherited Dispose;
end;

function TTailRecursiveSink<TSource>.Run(
  const sources: IEnumerable<IObservable<TSource>>): IDisposable;
var
  e: IEnumerator<IObservable<TSource>>;
  cancelable: IDisposable;
begin
  fIsDisposed := False;
  fSubscription := TSerialDisposable.Create;
  fGate := TAsyncLock.Create;
  fStack := TCollections.CreateStack<IEnumerator<IObservable<TSource>>>;
  fLength := TCollections.CreateStack<Nullable<Integer>>;
  e := nil;
  if not TryGetEnumerator(sources, e) then
    Exit(Disposable.Empty);

  fStack.Push(e);
  fLength.Push(Helpers.GetLength<IObservable<TSource>>(sources));

  cancelable := SchedulerDefaults.TailRecursion.Schedule(
    procedure(const _self: Action)
    begin
      fRecurse := _self;
      fGate.Wait(MoveNext);
    end);

  Result := TStableCompositeDisposable.Create([
    fSubscription, cancelable, DisposableCreate]);
end;

function TTailRecursiveSink<TSource>.DisposableCreate: IDisposable;
begin
  Result := Disposable.Create(
    procedure
    begin
      fGate.Wait(Dispose);
    end);
end;

procedure TTailRecursiveSink<TSource>.MoveNext;
var
  hasNext: Boolean;
  next: IObservable<TSource>;
  e: IEnumerator<IObservable<TSource>>;
  l: Nullable<Integer>;
  current: IObservable<TSource>;
  r: Nullable<Integer>;
  nextSeq: IEnumerable<IObservable<TSource>>;
  nextEnumerator: IEnumerator<IObservable<TSource>>;
  d: ISingleAssignmentDisposable;
begin
  hasNext := False;

  repeat
    if fStack.Count = 0 then
      Break;

    if fIsDisposed then
      Exit;

    e := fStack.Peek;
    l := fLength.Peek;

    try
      hasNext := e.MoveNext;
      if hasNext then
        current := e.Current;
    except
      on e: Exception do
      begin
        e := nil; //Dispose();

        Observer.OnError(e);
        Dispose;
        Exit;
      end;
    end;

    if not hasNext then
    begin
      e := nil; //Dispose();
      fStack.Pop;
      fLength.Pop;
    end
    else
    begin
      if l.HasValue then
        r := l.Value - 1
      else
        r := nil;
      fLength.Pop;
      fLength.Push(r);

      try
        next := current;  // TODO: check what Helpers.Unpack(current) does
      except
        on e: Exception do
        begin
          // TODO

          Exit;
        end;
      end;

      if r = 0 then
      begin
        e := nil; //Dispose();
        fStack.Pop;
        fLength.Pop;
      end;

      nextSeq := Extract(next);
      if nextSeq <> nil then
      begin
        if not TryGetEnumerator(nextSeq, nextEnumerator) then
          Exit;

        fStack.Push(nextEnumerator);
        fLength.Push(Helpers.GetLength<IObservable<TSource>>(nextSeq));

        hasNext := False;
      end;
    end;
  until hasNext;

  if not hasNext then
  begin
    Done;
    Exit;
  end;

  d := TSingleAssignmentDisposable.Create;
  fSubscription.Disposable := d;
  d.Disposable := next.Subscribe(Self);
end;

function TTailRecursiveSink<TSource>.TryGetEnumerator(
  const sources: IEnumerable<IObservable<TSource>>;
  out enumerator: IEnumerator<IObservable<TSource>>): Boolean;
begin
  try
    enumerator := sources.GetEnumerator;
    Result := True;
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;

      enumerator := nil;
      Result := False
    end;
  end;
end;

procedure TTailRecursiveSink<TSource>.Done;
begin
  Observer.OnCompleted;
  Dispose;
end;

function TTailRecursiveSink<TSource>.Fail(const error: Exception): Boolean;
begin
  Observer.OnError(error);
  Dispose;

  Result := True;
end;

{$ENDREGION}


end.
