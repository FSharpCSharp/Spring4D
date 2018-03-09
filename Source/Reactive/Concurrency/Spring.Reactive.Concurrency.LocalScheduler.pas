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

unit Spring.Reactive.Concurrency.LocalScheduler;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Concurrency.Scheduler;

type
  TLocalScheduler = class abstract(TScheduler, IScheduler, IStopwatchProvider)
  private
    function GetNow: TDateTime;
  public
    function Schedule(const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>): IDisposable; override;

    function StartStopwatch: IStopwatch; virtual;
  end;

implementation


{$REGION 'TLocalScheduler'}

function TLocalScheduler.GetNow: TDateTime;
begin
  Result := TScheduler.Now;
end;

function TLocalScheduler.Schedule(const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>): IDisposable;
begin
  Guard.CheckTrue(Assigned(action), 'action');

  Result := Schedule(state, TTimeSpan.Zero, action);
end;

function TLocalScheduler.StartStopwatch: IStopwatch;
begin
//  ConcurrencyAbstractionLayer.Current.StartStopwatch();
// TODO: implement
end;

{$ENDREGION}


end.
