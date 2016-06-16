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

unit Spring.Reactive.Observable.Timer;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTimer = class(TProducer<Integer>)
  private
    fDueTimeR: TTimeSpan;
    fPeriod: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<Integer>)
      private
        fParent: TTimer;
        fPeriod: TTimeSpan;
      public
        constructor Create(const parent: TTimer;
          const observer: IObserver<Integer>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<Integer>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const dueTime, period: TTimeSpan; const scheduler: IScheduler);
  end;

implementation


{$REGION 'TTimer'}

constructor TTimer.Create(const dueTime, period: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create;
  fDueTimeR := dueTime;
  fPeriod := period;
  fScheduler := scheduler;
end;

function TTimer.Run(const observer: IObserver<Integer>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  // TODO use nullable to support multiple ways

  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TTimer.TSink'}

constructor TTimer.TSink.Create(const parent: TTimer;
  const observer: IObserver<Integer>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fPeriod := fParent.fPeriod;
end;

destructor TTimer.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TTimer.TSink.Run: IDisposable;
var
  dueTime: TTimeSpan;
  count: Integer;
begin
  dueTime := fParent.fDueTimeR;
  if dueTime = fPeriod then
  begin
    count := 0;
    Result := (fParent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(fPeriod,
    procedure
    begin
      if Assigned(fObserver) then
      begin
        fObserver.OnNext(count);
        Inc(count);
      end;
    end);
  end;
end;

{$ENDREGION}


end.
