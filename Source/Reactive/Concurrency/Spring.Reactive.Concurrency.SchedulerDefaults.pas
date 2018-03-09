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

unit Spring.Reactive.Concurrency.SchedulerDefaults;

interface

uses
  Spring.Reactive;

type
  SchedulerDefaults = record
  strict private
    class var fConstantTimeOperations: IScheduler;
    class var fTailRecursion: IScheduler;
    class var fIteration: IScheduler;
    class var fTimeBasedOperations: IScheduler;
    class var fAsyncConversions: IScheduler;
  public
    class constructor Create;
    class destructor Destroy;

    class property ConstantTimeOperations: IScheduler read fConstantTimeOperations;
    class property TailRecursion: IScheduler read fTailRecursion;
    class property Iteration: IScheduler read fIteration;
    class property TimeBasedOperations: IScheduler read fTimeBasedOperations;
    class property AsyncConversions: IScheduler read fAsyncConversions;
  end;

implementation

uses
  Spring.Reactive.Concurrency.CurrentThreadScheduler,
  Spring.Reactive.Concurrency.DefaultScheduler,
  Spring.Reactive.Concurrency.ImmediateScheduler;


{$REGION 'SchedulerDefaults'}

class constructor SchedulerDefaults.Create;
begin
  fConstantTimeOperations := TImmediateScheduler.Instance;
  fTailRecursion := TImmediateScheduler.Instance;
  fIteration := TCurrentThreadScheduler.Instance;
  fTimeBasedOperations := TDefaultScheduler.Instance;
  fAsyncConversions := TDefaultScheduler.Instance;
end;

class destructor SchedulerDefaults.Destroy;
begin
  fConstantTimeOperations := nil;
  fTailRecursion := nil;
  fIteration := nil;
  fTimeBasedOperations := nil;
  fAsyncConversions := nil;
end;

{$ENDREGION}


end.
