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

unit Spring.Reactive.Tests.ImmediateScheduler;

interface

uses
  TestFramework;

type
  ImmediateSchedulerTest = class(TTestCase)
  published
    procedure Immediate_Now;
    procedure Immediate_ScheduleAction;
    procedure Immediate_ScheduleActionError;
    procedure Immediate_ArgumentChecking;
    procedure Immediate_Simple1;
//    procedure Immediate_Simple2; // TODO: implement TDateTimeOffset overloads
    procedure Immediate_Simple3;
    procedure Immediate_Recursive1;
//    procedure Immediate_Recursive2; // TODO: implement TDateTimeOffset overloads
    procedure Immediate_Recursive3;
    procedure Immediate_ArgumentChecking_More;
    procedure Immediate_ScheduleActionDue;
  end;

implementation

uses
  DateUtils,
  SysUtils,
  Spring,
  Spring.TestUtils,
  Spring.Reactive,
  Spring.Reactive.Concurrency.Scheduler,
  Spring.Reactive.Disposables;


{$REGION 'ImmediateSchedulerTest'}

procedure ImmediateSchedulerTest.Immediate_Now;
var
  res: TDateTime;
begin
  res := TScheduler.Immediate.Now - Now;
  CheckTrue(SecondOf(res) < 1);
end;

procedure ImmediateSchedulerTest.Immediate_ScheduleAction;
var
  id: TThreadID;
  ran: Boolean;
begin
  ran := False;
  TScheduler.Immediate.Schedule(
    procedure
    begin
      id := TThread.CurrentThread.ThreadID;
      ran := True;
    end);
  CheckEquals(TThread.CurrentThread.ThreadID, id);
  CheckTrue(ran);
end;

procedure ImmediateSchedulerTest.Immediate_ScheduleActionError;
var
  ex: Exception;
begin
  ex := Exception.Create('');
  try
    TScheduler.Immediate.Schedule(
      procedure
      begin
        raise ex;
      end);
    Check(False);
  except
    on e: Exception do
      CheckSame(e, ex);
  end;
end;

procedure ImmediateSchedulerTest.Immediate_ArgumentChecking;
begin
  CheckException(EArgumentNilException, procedure begin TScheduler.Immediate.Schedule(42, Func<IScheduler,TValue,IDisposable>(nil)) end);
//  CheckException(EArgumentNilException, procedure begin TScheduler.Immediate.Schedule(42, TDateTimeOffset.Now, Func<IScheduler,TValue,IDisposable>(nil)) end);
  CheckException(EArgumentNilException, procedure begin TScheduler.Immediate.Schedule(42, TTimeSpan.Zero, Func<IScheduler,TValue,IDisposable>(nil)) end);
end;

procedure ImmediateSchedulerTest.Immediate_Simple1;
var
  _x: Integer;
begin
  _x := 0;
  TScheduler.Immediate.Schedule(42,
    function(const self: IScheduler; const x: TValue): IDisposable
    begin
      _x := x.AsInteger;
      Result := Disposable.Empty;
    end);
  CheckEquals(42, _x);
end;

//procedure ImmediateSchedulerTest.Immediate_Simple2;
//var
//  _x: Integer;
//begin
//  _x := 0;
//  TScheduler.Immediate.Schedule(42, Now
//    function(const self: IScheduler; const x: TValue): IDisposable
//    begin
//      _x := x.AsInteger;
//      Result := Disposable.Empty;
//    end);
//  CheckEquals(42, _x);
//end;

procedure ImmediateSchedulerTest.Immediate_Simple3;
var
  _x: Integer;
begin
  _x := 0;
  TScheduler.Immediate.Schedule(42, TTimeSpan.Zero,
    function(const self: IScheduler; const x: TValue): IDisposable
    begin
      _x := x.AsInteger;
      Result := Disposable.Empty;
    end);
  CheckEquals(42, _x);
end;

procedure ImmediateSchedulerTest.Immediate_Recursive1;
var
  _x, _y: Integer;
begin
  _x := 0;
  _y := 0;
  TScheduler.Immediate.Schedule(42,
    function(const self: IScheduler; const x: TValue): IDisposable
    begin
      _x := x.AsInteger;
      Result := self.Schedule(43,
        function(const self2: IScheduler; const y: TValue): IDisposable
        begin
          _y := y.AsInteger;
          Result := Disposable.Empty;
        end);
    end);
  CheckEquals(42, _x);
  CheckEquals(43, _y);
end;

//procedure ImmediateSchedulerTest.Immediate_Recursive2;
//var
//  _x, _y: Integer;
//begin
//  _x := 0;
//  _y := 0;
//  TScheduler.Immediate.Schedule(42,
//    function(const self: IScheduler; const x: TValue): IDisposable
//    begin
//      _x := x.AsInteger;
//      Result := self.Schedule(43, Now,
//        function(const self2: IScheduler; const y: TValue): IDisposable
//        begin
//          _y := y.AsInteger;
//          Result := Disposable.Empty;
//        end);
//    end);
//  CheckEquals(42, _x);
//  CheckEquals(43, _y);
//end;

procedure ImmediateSchedulerTest.Immediate_Recursive3;
var
  _x, _y: Integer;
begin
  _x := 0;
  _y := 0;
  TScheduler.Immediate.Schedule(42,
    function(const self: IScheduler; const x: TValue): IDisposable
    begin
      _x := x.AsInteger;
      Result := self.Schedule(43, TTimeSpan.FromMilliseconds(100),
        function(const self2: IScheduler; const y: TValue): IDisposable
        begin
          _y := y.AsInteger;
          Result := Disposable.Empty;
        end);
    end);
  CheckEquals(42, _x);
  CheckEquals(43, _y);
end;

procedure ImmediateSchedulerTest.Immediate_ArgumentChecking_More;
begin
  TScheduler.Immediate.Schedule(42,
    function(const self: IScheduler; const state: TValue): IDisposable
    begin
      CheckException(EArgumentNullException,
        procedure
        begin
          self.Schedule(43, Func<IScheduler,TValue,IDisposable>(nil));
        end);
      Result := Disposable.Empty;
    end);

  TScheduler.Immediate.Schedule(42,
    function(const self: IScheduler; const state: TValue): IDisposable
    begin
      CheckException(EArgumentNullException,
        procedure
        begin
          self.Schedule(43, TTimeSpan.FromSeconds(1), Func<IScheduler,TValue,IDisposable>(nil));
        end);
      Result := Disposable.Empty;
    end);

//  TScheduler.Immediate.Schedule(42,
//    function(const self: IScheduler; const state: TValue): IDisposable
//    begin
//      CheckException(EArgumentNullException,
//        procedure
//        begin
//          self.Schedule(43, Now + 1, Default(Func<IScheduler,TValue,IDisposable>));
//        end);
//      Result := Disposable.Empty;
//    end);
end;

procedure ImmediateSchedulerTest.Immediate_ScheduleActionDue;
var
  id: TThreadID;
  ran: Boolean;
  sw: TStopwatch;
begin
  ran := False;
  sw := TStopwatch.Create;
  sw.Start;
  TScheduler.Immediate.Schedule(TTimeSpan.FromSeconds(0.2),
    procedure
    begin
      sw.Stop;
      id := TThread.CurrentThread.ThreadID;
      ran := True;
    end);
  CheckEquals(TThread.Current.ThreadID, id);
  CheckTrue(ran);
  CheckTrue(sw.ElapsedMilliseconds > 180);
end;

{$ENDREGION}


initialization
  RegisterTest(ImmediateSchedulerTest.Suite);

end.
