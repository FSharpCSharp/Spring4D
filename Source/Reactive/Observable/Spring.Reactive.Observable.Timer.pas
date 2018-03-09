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

unit Spring.Reactive.Observable.Timer;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTimer = class
  private type
    TSingle = class(TProducer<Int64>)
    private
      fScheduler: IScheduler;

      type
        TSink = class(TSink<Int64>)
        private
          procedure Invoke;
        public
          function Run(const parent: TSingle; const dueTime: TTimeSpan): IDisposable; overload;
    //      function Run(const parent: TSingle; const dueTime: TDateTimeOffset): IDisposable; overload;
        end;
    public
      constructor Create(const scheduler: IScheduler);
    end;

    TPeriodic = class(TProducer<Int64>)
    private
      fPeriod: TTimeSpan;
      fScheduler: IScheduler;

      type
        TSink = class(TSink<Int64>)
        private
          fPeriod: TTimeSpan;
          procedure InvokeStart;
          function Tick(const count: Int64): Int64;
        public
          constructor Create(const period: TTimeSpan;
            const observer: IObserver<Int64>; const cancel: IDisposable);
          function Run(const parent: TPeriodic; const dueTime: TTimeSpan): IDisposable; overload;
    //      function Run(const parent: TPeriodic; const dueTime: TDateTimeOffset): IDisposable; overload;
        end;
    public
      constructor Create(const period: TTimeSpan; const scheduler: IScheduler);
    end;
  public type
    TSingleRelative = class(TSingle)
    private
      fDueTime: TTimeSpan;
    protected
      function CreateSink(const observer: IObserver<Int64>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const dueTime: TTimeSpan; const scheduler: IScheduler);
    end;

    TPeriodicRelative = class(TPeriodic)
    private
      fDueTime: TTimeSpan;
    protected
      function CreateSink(const observer: IObserver<Int64>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const dueTime, period: TTimeSpan; const scheduler: IScheduler);
    end;
  end;

implementation


{$REGION 'TTimer.TSingle'}

constructor TTimer.TSingle.Create(const scheduler: IScheduler);
begin
  inherited Create;
  fScheduler := scheduler;
end;

{$ENDREGION}


{$REGION 'TTimer.TSingle.TSink'}

procedure TTimer.TSingle.TSink.Invoke;
begin
  Observer.OnNext(0);
  Observer.OnCompleted;
  Dispose;
end;

function TTimer.TSingle.TSink.Run(const parent: TSingle; const dueTime: TTimeSpan): IDisposable;
var
  guard: IInterface;
begin
  guard := Self; // make sure that self is kept alive by capturing it
  Result := parent.fScheduler.Schedule(dueTime,
    procedure
    begin
      if Assigned(guard) then
        Invoke;
    end);
end;

{$ENDREGION}


{$REGION 'TTimer.TSingleRelative'}

constructor TTimer.TSingleRelative.Create(const dueTime: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create(scheduler);
  fDueTime := dueTime;
end;

function TTimer.TSingleRelative.CreateSink(const observer: IObserver<Int64>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TTimer.TSingleRelative.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self, fDueTime);
end;

{$ENDREGION}


{$REGION 'TTimer.TPeriodic'}

constructor TTimer.TPeriodic.Create(const period: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create;
  fPeriod := period;
  fScheduler := scheduler;
end;

{$ENDREGION}


{$REGION 'TTimer.TPeriodic.TSink'}

constructor TTimer.TPeriodic.TSink.Create(const period: TTimeSpan;
  const observer: IObserver<Int64>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fPeriod := period;
end;

procedure TTimer.TPeriodic.TSink.InvokeStart;
begin
  // TODO: implement
  raise ENotImplementedException.Create('InvokeStart');
end;

function TTimer.TPeriodic.TSink.Run(const parent: TPeriodic;
  const dueTime: TTimeSpan): IDisposable;
var
  guard: IInterface;
begin
  guard := Self; // make sure that self is kept alive by capturing it
  if dueTime = fPeriod then
    Result := (parent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(0, fPeriod,
      function (const count: TValue): TValue
      begin
        if Assigned(guard) then
          Result := Tick(count.AsInt64);
      end)
  else
    Result := parent.fScheduler.Schedule(dueTime,
      procedure
      begin
        if Assigned(guard) then
          InvokeStart;
      end);
end;

function TTimer.TPeriodic.TSink.Tick(const count: Int64): Int64;
begin
  Observer.OnNext(count);
  Result := count + 1; // TODO: unchecked
end;

{$ENDREGION}


{$REGION 'TTimer.TPeriodicRelative'}

constructor TTimer.TPeriodicRelative.Create(const dueTime, period: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create(period, scheduler);
  fDueTime := dueTime;
end;

function TTimer.TPeriodicRelative.CreateSink(const observer: IObserver<Int64>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fPeriod, observer, cancel);
end;

function TTimer.TPeriodicRelative.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self, fDueTime);
end;

{$ENDREGION}


end.
