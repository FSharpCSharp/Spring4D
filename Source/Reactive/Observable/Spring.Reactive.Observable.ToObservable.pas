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

unit Spring.Reactive.Observable.ToObservable;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TToObservable<TSource> = class(TProducer<TSource>)
  private
    fSource: IEnumerable<TSource>;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<TSource>)
      private type
        TState = record
          flag: ICancelable;
          enumerator: IEnumerator<TSource>;
        end;
      private
        fParent: TToObservable<TSource>;
        procedure LoopRec(const state: TState; const recurse: Action<TValue>);
      public
        constructor Create(const parent: TToObservable<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IEnumerable<TSource>; const scheduler: IScheduler);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TToObservable<TSource>'}

constructor TToObservable<TSource>.Create(const source: IEnumerable<TSource>;
  const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fScheduler := scheduler;
end;

function TToObservable<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TToObservable<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TToObservable<TSource>.TSink'}

constructor TToObservable<TSource>.TSink.Create(const parent: TToObservable<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TToObservable<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TToObservable<TSource>.TSink.LoopRec(const state: TState;
  const recurse: Action<TValue>);
var
  hasNext: Boolean;
  ex: Exception;
  current: TSource;
begin
  hasNext := False;
  ex := nil;
  current := Default(TSource);

  if state.flag.IsDisposed then
  begin
    // TODO: review
//    state.enumerator.Dispose;
//    state.enumerator := nil;
    Exit;
  end;

  try
    hasNext := state.enumerator.MoveNext;
    if hasNext then
      current := state.enumerator.Current;
  except
    on e: Exception do
      ex := Exception(AcquireExceptionObject);
  end;

  if Assigned(ex) then
  begin
    // TODO: review
//    state.enumerator.Dispose;
//    state.enumerator := nil;

    Observer.OnError(ex);
    Dispose;
    Exit;
  end;

  if not hasNext then
  begin
    // TODO: review
//    state.enumerator.Dispose;
//    state.enumerator := nil;

    Observer.OnCompleted;
    Dispose;
    Exit;
  end;

  Observer.OnNext(current);
  recurse(TValue.From(state));
end;

function TToObservable<TSource>.TSink.Run: IDisposable;
var
  e: IEnumerator<TSource>;
  flag: ICancelable;
  state: TState;
  guard: IInterface;
begin
  try
    e := fParent.fSource.GetEnumerator;
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Result := Disposable.Empty;
    end;
  end;

  flag := TBooleanDisposable.Create;
  state.flag := flag;
  state.enumerator := e;
  guard := Self; // make sure that self is kept alive by capturing it
  fParent.fScheduler.Schedule(TValue.From(state),
    procedure (const state: TValue; const recurse: Action<TValue>)
    begin
      if Assigned(guard) then
        LoopRec(state.AsType<TState>, recurse);
    end);
  Result := flag;
end;

{$ENDREGION}


end.
