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

unit Spring.Reactive.Concurrency.CurrentThreadScheduler;

interface

uses
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Concurrency.LocalScheduler,
  Spring.Reactive.Concurrency.Scheduler,
  Spring.Reactive.Concurrency.SchedulerQueue;

type
  TCurrentThreadScheduler = class(TLocalScheduler, ICurrentThreadScheduler)
  private
    class var fInstance: ICurrentThreadScheduler;
    class var fClocks: IDictionary<TThreadID, TStopwatch>;
    class var fQueues: IDictionary<TThreadID, TSchedulerQueue<TTimeSpan>>;
    class var fLock: TCriticalSection;
    class function GetQueue: TSchedulerQueue<TTimeSpan>; static;
    class procedure SetQueue(const newQueue: TSchedulerQueue<TTimeSpan>); static;
    class function GetTime: TTimeSpan; static;
    function GetScheduleRequired: Boolean;
    class function GetIsScheduleRequired: Boolean; static;
    class property Time: TTimeSpan read GetTime;
    class function GetInstance: ICurrentThreadScheduler; static;
    type
      TTrampoline = class
        class procedure Run(const queue: TSchedulerQueue<TTimeSpan>); static;
      end;
  public
    class constructor Create;
    class destructor Destroy;
    class property Instance: ICurrentThreadScheduler read GetInstance;
    class property IsScheduleRequired: Boolean read GetIsScheduleRequired;
  public
    function Schedule(const state: TValue; const dueTime: TTimeSpan;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;
  end;

implementation

uses
  Spring.Reactive.Concurrency.ScheduledItem,
  Spring.Reactive.Disposables;


{$REGION 'TCurrentThreadScheduler'}

class constructor TCurrentThreadScheduler.Create;
begin
  fClocks := TCollections.CreateDictionary<TThreadID, TStopwatch>;
  fQueues := TCollections.CreateDictionary<TThreadID, TSchedulerQueue<TTimeSpan>>([doOwnsValues]);
  fLock := TCriticalSection.Create;
end;

class destructor TCurrentThreadScheduler.Destroy;
begin
  fLock.Free;
end;

function TCurrentThreadScheduler.GetScheduleRequired: Boolean;
begin
  Result := IsScheduleRequired;
end;

class function TCurrentThreadScheduler.GetInstance: ICurrentThreadScheduler;
begin
  if not Assigned(fInstance) then
    fInstance := TCurrentThreadScheduler.Create;
  Result := fInstance;
end;

class function TCurrentThreadScheduler.GetIsScheduleRequired: Boolean;
begin
  Result := GetQueue = nil;
end;

class function TCurrentThreadScheduler.GetQueue: TSchedulerQueue<TTimeSpan>;
begin
  fLock.Enter;
  try
    fQueues.TryGetValue(TThread.CurrentThread.ThreadID, Result);
  finally
    fLock.Leave;
  end;
end;

class procedure TCurrentThreadScheduler.SetQueue(
  const newQueue: TSchedulerQueue<TTimeSpan>);
var
  currentThreadId: TThreadID;
begin
  fLock.Enter;
  try
    currentThreadId := TThread.CurrentThread.ThreadID;
    if newQueue = nil then
      fQueues.Remove(currentThreadId)
    else
      fQueues[currentThreadId] := newQueue;
  finally
    fLock.Leave;
  end;
end;

class function TCurrentThreadScheduler.GetTime: TTimeSpan;
var
  currentThreadId: TThreadID;
  clock: TStopwatch;
begin
  fLock.Enter;
  try
    currentThreadId := TThread.CurrentThread.ThreadID;
    if not fClocks.TryGetValue(currentThreadId, clock) then
    begin
      clock := TStopwatch.StartNew;
      fClocks.Add(currentThreadId, clock);
    end;
    Result := clock.Elapsed;
  finally
    fLock.Leave;
  end;
end;

function TCurrentThreadScheduler.Schedule(const state: TValue;
  const dueTime: TTimeSpan;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
var
  dt: TTimeSpan;
  si: IScheduledItem<TTimeSpan, TValue>;
  queue: TSchedulerQueue<TTimeSpan>;
begin
  dt := Time + Normalize(dueTime);
  si := TScheduledItem<TTimeSpan, TValue>.Create(Self, state, action, dt);
  queue := GetQueue;
  if queue = nil then
  begin
    queue := TSchedulerQueue<TTimeSpan>.Create(4);
    queue.Enqueue(si);

    SetQueue(queue);
    try
      TTrampoline.Run(queue);
    finally
      SetQueue(nil);
    end;
  end
  else
    queue.Enqueue(si);
  Result := Disposable.Create(si);
end;

{$ENDREGION}


{$REGION 'TCurrentThreadScheduler.TTrampoline'}

class procedure TCurrentThreadScheduler.TTrampoline.Run(
  const queue: TSchedulerQueue<TTimeSpan>);
var
  item: IScheduledItem<TTimeSpan>;
  wait: TTimeSpan;
begin
  while queue.Count > 0 do
  begin
    item := queue.Dequeue;
    if not item.IsCanceled then
    begin
      wait := item.DueTime - Time;
      if wait.Ticks > 0 then
        TThread.Sleep(wait);
      if not item.IsCanceled then
        item.Invoke;
    end;
  end;
end;

{$ENDREGION}


end.
