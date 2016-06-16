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

unit Spring.Reactive.Testing.HotObservable;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Notification,
  Spring.Reactive.ObservableBase,
  Spring.Reactive.Testing,
  Spring.Reactive.Testing.Recorded,
  Spring.Reactive.Testing.Subscription,
  Spring.Reactive.Testing.TestScheduler;

type
  THotObservable<T> = class(TObservableBase<T>, ITestableObservable<T>)
  private
    fScheduler: TTestScheduler;
    fObservers: IList<IObserver<T>>;
    fSubscriptions: IList<TSubscription>;
    fMessages: IList<TRecorded<INotification<T>>>;
    function GetSubscriptions: IList<TSubscription>;
    function GetMessages: IList<TRecorded<INotification<T>>>;

    procedure ScheduleInternal(const scheduler: TTestScheduler;
      const messages: array of TRecorded<INotification<T>>; const i: Integer);
  public
    constructor Create(const scheduler: TTestScheduler;
      const messages: array of TRecorded<INotification<T>>);

    function Subscribe(const observer: IObserver<T>): IDisposable; override;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'THotObservable<T>'}

constructor THotObservable<T>.Create(const scheduler: TTestScheduler;
  const messages: array of TRecorded<INotification<T>>);
var
  i: Integer;
begin
  Guard.CheckNotNull(scheduler, 'scheduler');
  Guard.CheckNotNull(Length(messages) > 0, 'messages');
  inherited Create;
  fScheduler := scheduler;
  fObservers := TCollections.CreateList<IObserver<T>>;
  fSubscriptions := TCollections.CreateList<TSubscription>;
  fMessages := TCollections.CreateList<TRecorded<INotification<T>>>(messages);

  for i := 0 to High(messages) do
    ScheduleInternal(scheduler, messages, i);
end;

procedure THotObservable<T>.ScheduleInternal(const scheduler: TTestScheduler;
  const messages: array of TRecorded<INotification<T>>; const i: Integer);
var
  notification: INotification<T>; // need to put into own method because of variable capturing
begin
  notification := messages[i].Value;
  scheduler.ScheduleAbsolute(TValue.Empty, messages[i].Time,
    function(const scheduler: IScheduler; const state1: TValue): IDisposable
    var
      observers: TArray<IObserver<T>>;
      j: Integer;
    begin
      observers := fObservers.ToArray;
      for j := 0 to High(observers) do
        notification.Accept(fobservers[j]);
      Result := Disposable.Empty;
    end);
end;

function THotObservable<T>.GetMessages: IList<TRecorded<INotification<T>>>;
begin
  Result := fMessages;
end;

function THotObservable<T>.GetSubscriptions: IList<TSubscription>;
begin
  Result := fSubscriptions;
end;

function THotObservable<T>.Subscribe(const observer: IObserver<T>): IDisposable;
var
  index: Integer;
begin
  Guard.CheckNotNull(observer, 'observer');
  fObservers.Add(observer);
  fSubscriptions.Add(TSubscription.Create(fScheduler.Clock));
  index := fSubscriptions.Count - 1;

  Result := Disposable.Create(
    procedure
    begin
      fObservers.Remove(observer);
      fSubscriptions[index] := TSubscription.Create(fSubscriptions[index].Subscribe, fScheduler.Clock);
    end);
end;

{$ENDREGION}


end.
