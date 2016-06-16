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

unit Spring.Reactive.Internal.Producer;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TProducer<T> = class(TObservableBase<T>)
  private type
    PState = ^TState;
    TState = record
      sink: ISingleAssignmentDisposable;
      subscription: ISingleAssignmentDisposable;
      observer: IObserver<T>;
      procedure Assign(const s: IDisposable);
    end;
  strict protected
    constructor Create; // hide default constructor
    function RunInternal(const _: IScheduler; const state: TValue): IDisposable; overload;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; overload; virtual; abstract;
  public
    function Subscribe(const observer: IObserver<T>): IDisposable; override;
    function SubscribeRaw(const observer: IObserver<T>; enableSafeguard: Boolean): IDisposable;
  end;

implementation

uses
  Spring.Reactive.Concurrency.CurrentThreadScheduler,
  Spring.Reactive.Disposables;


{$REGION 'TProducer<T>'}

constructor TProducer<T>.Create;
begin
  inherited Create;
end;

function TProducer<T>.Subscribe(const observer: IObserver<T>): IDisposable;
var
  sink: ISingleAssignmentDisposable;
  subscription: ISingleAssignmentDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');

  Result := SubscribeRaw(observer, True);
end;

function TProducer<T>.SubscribeRaw(const observer: IObserver<T>;
  enableSafeguard: Boolean): IDisposable;
var
  state: TState;
  d: ICancelable;
begin
  state.observer := observer;
  state.sink := TSingleAssignmentDisposable.Create;
  state.subscription := TSingleAssignmentDisposable.Create;

  d := TStableCompositeDisposable.Create(state.sink, state.subscription);

//  if enableSafeguard then
//    state.observer := TSafeObserver<T>.Create(state.observer, d);

  if TCurrentThreadScheduler.IsScheduleRequired then
    TCurrentThreadScheduler.Instance.Schedule(TValue.From(state), RunInternal)
  else
    state.subscription.Disposable := Run(state.observer, state.subscription, state.Assign);

  Result := d;
end;

function TProducer<T>.RunInternal(const _: IScheduler; const state: TValue): IDisposable;
var
  x: TState;
begin
  x := PState(state.GetReferenceToRawData)^;
//  x := state.AsType<TState>();
  x.subscription.Disposable := Run(x.observer, x.subscription, x.Assign);
  Result := Disposable.Empty;
end;

{$ENDREGION}


{$REGION 'TProducer<T>.TState'}

procedure TProducer<T>.TState.Assign(const s: IDisposable);
begin
  sink.Disposable := s;
end;

{$ENDREGION}


end.
