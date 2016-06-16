{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Reactive.Observable.ToObservable;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TToObservable<T> = class(TProducer<T>)
  private
    fSource: IEnumerable<T>;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>)
      private
        fParent: TToObservable<T>;
        procedure LoopRec(const state: TValue; const recurse: Action<TValue>);
      public
        constructor Create(const parent: TToObservable<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IEnumerable<T>; const scheduler: IScheduler);
  end;

  TState<T> = record
  public
    flag: ICancelable;
    enumerator: IEnumerator<T>;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TToObservable<T>'}

constructor TToObservable<T>.Create(const source: IEnumerable<T>;
  const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fScheduler := scheduler;
end;

function TToObservable<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TToObservable<T>.TSink'}

constructor TToObservable<T>.TSink.Create(const parent: TToObservable<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TToObservable<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TToObservable<T>.TSink.LoopRec(const state: TValue;
  const recurse: Action<TValue>);
var
  hasNext: Boolean;
  ex: Exception;
  current: T;
  _state: TState<T>;
begin
  hasNext := False;
  ex := nil;
  current := Default(T);
  _state := state.AsType<TState<T>>;

  if _state.flag.IsDisposed then
  begin
//    _state.enumerator.Dispose;
    _state.enumerator := nil; //TODO: review
    Exit;
  end;

  try
    hasNext := _state.enumerator.MoveNext;
    if hasNext then
      current := _state.enumerator.Current;
  except
    on e: Exception do
      ex := Exception(AcquireExceptionObject);
  end;

  if Assigned(ex) then
  begin
//    _state.enumerator.Dispose;
    _state.enumerator := nil; // TODO: review

    fObserver.OnError(ex);
    Dispose;
    Exit;
  end;

  if not hasNext then
  begin
//    _state.enumerator.Dispose;
    _state.enumerator := nil; // TODO: review

    fObserver.OnCompleted;
    Dispose;
    Exit;
  end;

  fObserver.OnNext(current);
  recurse(state);
end;

function TToObservable<T>.TSink.Run: IDisposable;
var
  e: IEnumerator<T>;
  flag: ICancelable;
  state: TState<T>;
begin
  try
    e := fParent.fSource.GetEnumerator;
  except
    on e: Exception do
    begin
      fObserver.OnError(e);
      Dispose;
      Result := Disposable.Empty;
    end;
  end;

  flag := TBooleanDisposable.Create;
  state.flag := flag;
  state.enumerator := e;
  fParent.fScheduler.Schedule(TValue.From(state), LoopRec);
  Result := flag;
end;

{$ENDREGION}


end.

