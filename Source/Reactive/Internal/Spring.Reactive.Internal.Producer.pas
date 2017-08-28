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
  TBasicProducer<TSource> = class(TObservableBase<TSource>)
  private type
    PState = ^TState;
    TState = record
      subscription: ISingleAssignmentDisposable;
      observer: IObserver<TSource>;
    end;
  private
    function RunInternal(const _: IScheduler; const state: TValue): IDisposable;
  strict protected
    constructor Create; // hide default constructor
  protected
    function Run(const observer: IObserver<TSource>): IDisposable; virtual; abstract;
  public
    function Subscribe(const observer: IObserver<TSource>): IDisposable; override;
    function SubscribeRaw(const observer: IObserver<TSource>{; enableSafeguard: Boolean}): IDisposable;
  end;

  TProducer<TSource> = class(TObservableBase<TSource>)
  private type
    PState = ^TState;
    TState = record
      sink: TObject;
      inner: ISingleAssignmentDisposable;
    end;
  strict protected
    constructor Create; // hide default constructor
    function RunInternal(const _: IScheduler; const state: TValue): IDisposable; overload;
  protected
    function CreateSink(const observer: IObserver<TSource>; const cancel: IDisposable): TObject; virtual; abstract;
    function Run(const sink: TObject): IDisposable; virtual; abstract;
  public
    function Subscribe(const observer: IObserver<TSource>): IDisposable; override;
    function SubscribeRaw(const observer: IObserver<TSource>; enableSafeguard: Boolean): IDisposable;
  end;

  TSubscriptionDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  private
    Sink: IDisposable;
    Inner: ISingleAssignmentDisposable;
    function GetIsDisposed: Boolean;
  public
    constructor Create;
    procedure Dispose;
  end;

implementation

uses
  Spring.Reactive.Concurrency.CurrentThreadScheduler,
  Spring.Reactive.Disposables;


{$REGION 'TBasicProducer<TSource>'}

constructor TBasicProducer<TSource>.Create;
begin
  inherited Create;
end;

function TBasicProducer<TSource>.Subscribe(
  const observer: IObserver<TSource>): IDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');

  Result := SubscribeRaw(observer{, True});
end;

function TBasicProducer<TSource>.SubscribeRaw(const observer: IObserver<TSource>): IDisposable;
var
  subscription: ISingleAssignmentDisposable;
  state: TState;
begin
  subscription := TSingleAssignmentDisposable.Create;

  if TCurrentThreadScheduler.IsScheduleRequired then
  begin
    state.subscription := subscription;
    state.observer := observer;
    TCurrentThreadScheduler.Instance.Schedule(TValue.From(state), RunInternal);
  end
  else
    subscription.Disposable := Run(observer);

  Result := subscription;
end;

function TBasicProducer<TSource>.RunInternal(const _: IScheduler;
  const state: TValue): IDisposable;
var
  x: PState;
begin
  x := PState(state.GetReferenceToRawData);
  x.subscription.Disposable := Run(x.observer);
  Result := Disposable.Empty;
end;

{$ENDREGION}


{$REGION 'TProducer<TSource>'}

constructor TProducer<TSource>.Create;
begin
  inherited Create;
end;

function TProducer<TSource>.Subscribe(const observer: IObserver<TSource>): IDisposable;
var
  sink: ISingleAssignmentDisposable;
  subscription: ISingleAssignmentDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');

  Result := SubscribeRaw(observer, True);
end;

function TProducer<TSource>.SubscribeRaw(const observer: IObserver<TSource>;
  enableSafeguard: Boolean): IDisposable;
var
  subscription: TSubscriptionDisposable;
  sink: TObject;
  state: TState;
begin
  subscription := TSubscriptionDisposable.Create;

//  if enableSafeguard then
//    observer := TSafeObserver<TSource>.Create(observer, subscription);

  sink := CreateSink(observer, subscription.Inner);

  subscription.Sink := TInterfacedObject(sink) as IDisposable;

  if TCurrentThreadScheduler.IsScheduleRequired then
  begin
    state.sink := sink;
    state.inner := subscription.Inner;
    TCurrentThreadScheduler.Instance.Schedule(TValue.From(state), RunInternal);
  end
  else
    subscription.Inner.Disposable := Run(sink);

  Result := subscription;
end;

function TProducer<TSource>.RunInternal(const _: IScheduler; const state: TValue): IDisposable;
var
  x: TState;
begin
  x := PState(state.GetReferenceToRawData)^;
  x.inner.Disposable := Run(x.sink);
  Result := Disposable.Empty;
end;

{$ENDREGION}


{$REGION 'TSubscriptionDisposable'}

constructor TSubscriptionDisposable.Create;
begin
  Inner := TSingleAssignmentDisposable.Create;
end;

procedure TSubscriptionDisposable.Dispose;
var
  old: IDisposable;
begin
  old := TInterlocked.Exchange<IDisposable>(Sink, nil);
  if Assigned(old) then
    old.Dispose;
  Inner.Dispose;
end;

function TSubscriptionDisposable.GetIsDisposed: Boolean;
begin
  Result := Sink = nil;
end;

{$ENDREGION}


end.
