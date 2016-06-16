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

unit Spring.Reactive.Testing.TestScheduler;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.VirtualTimeScheduler,
  Spring.Reactive.Notification,
  Spring.Reactive.Testing,
  Spring.Reactive.Testing.Recorded;

type
//  ITestScheduler =

  TTestScheduler = class(TVirtualTimeScheduler<Int64, Int64>)
  protected
    function Add(const absolute: Int64; const relative: Int64): Int64; override;
    function ToDateTimeOffset(const absolute: Int64): TDateTime; override;
    function ToRelative(const timeSpan: TTimeSpan): Int64; override;
  public
    function ScheduleAbsolute(const state: TValue; const dueTime: Int64;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;

    function Start<T>(const create: Func<IObservable<T>>;
      const created, subscribed, disposed: Int64): ITestableObserver<T>; overload;
    function Start<T>(const create: Func<IObservable<T>>): ITestableObserver<T>; overload;

    function CreateHotObservable<T>(const messages: array of TRecorded<INotification<T>>): ITestableObservable<T>;
    function CreateObserver<T>: ITestableObserver<T>;
  end;

implementation

uses
  DateUtils,
  Spring.Reactive.Disposables,
  Spring.Reactive.Testing.ReactiveTest,
  Spring.Reactive.Testing.HotObservable,
  Spring.Reactive.Testing.MockObserver;


{$REGION 'TTestScheduler'}

function TTestScheduler.ScheduleAbsolute(const state: TValue;
  const dueTime: Int64;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  dt: Int64;
begin
  if dueTime <= Clock then
    dt := Clock + 1
  else
    dt := dueTime;

  Result := inherited ScheduleAbsolute(state, dt, action);
end;

function TTestScheduler.Add(const absolute, relative: Int64): Int64;
begin
  Result := absolute + relative;
end;

function TTestScheduler.ToDateTimeOffset(const absolute: Int64): TDateTime;
begin
  with TTimeSpan.FromTicks(absolute) do
    Result := Days + EncodeDateTime(0, 0, 0, Hours, Minutes, Seconds, Milliseconds);
end;

function TTestScheduler.ToRelative(const timeSpan: TTimeSpan): Int64;
begin
  Result := timeSpan.Ticks;
end;

function TTestScheduler.Start<T>(const create: Func<IObservable<T>>;
  const created, subscribed, disposed: Int64): ITestableObserver<T>;
var
  _create: Func<IObservable<T>>; // need local variable to avoid compile error
  source: IObservable<T>;
  subscription: IDisposable;
  observer: ITestableObserver<T>;
begin
  Guard.CheckNotNull(Assigned(create), 'create');

  _create := create;
  observer := CreateObserver<T>;

  ScheduleAbsolute(TValue.Empty, created,
    function(const scheduler: IScheduler; const state: TValue): IDisposable
    begin
      source := _create();
      Result := Disposable.Empty;
    end);
  ScheduleAbsolute(TValue.Empty, subscribed,
    function(const scheduler: IScheduler; const state: TValue): IDisposable
    begin
      subscription := source.Subscribe(observer);
      Result := Disposable.Empty;
    end);
  ScheduleAbsolute(TValue.Empty, disposed,
    function(const scheduler: IScheduler; const state: TValue): IDisposable
    begin
      subscription.Dispose;
      Result := Disposable.Empty;
    end);

  Start;

  Result := observer;
end;

function TTestScheduler.Start<T>(
  const create: Func<IObservable<T>>): ITestableObserver<T>;
begin
  Guard.CheckNotNull(Assigned(create), 'create');

  Result := Start<T>(create, TReactiveTest.Created, TReactiveTest.Subscribed, TReactiveTest.Disposed);
end;

function TTestScheduler.CreateHotObservable<T>(
  const messages: array of TRecorded<INotification<T>>): ITestableObservable<T>;
begin
  Guard.CheckNotNull(Length(messages) > 0, 'messages');
  Result := THotObservable<T>.Create(Self, messages);
end;

function TTestScheduler.CreateObserver<T>: ITestableObserver<T>;
begin
  Result := TMockObserver<T>.Create(Self);
end;

{$ENDREGION}


end.
