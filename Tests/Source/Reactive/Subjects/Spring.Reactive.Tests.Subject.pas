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

unit Spring.Reactive.Tests.Subject;

interface

uses
  TestFramework,
  Spring.Reactive.Testing.ReactiveTest,
  Spring.Reactive.Testing.TestScheduler;

type
  SubjectTest = class(TReactiveTest)
  published
    procedure Subscribe_ArgumentChecking;
    procedure OnError_ArgumentChecking;

    procedure Infinite;
    procedure Finite;
    procedure Error;
    procedure Canceled;
    procedure Dispose;
    procedure PreComplete;
    procedure SubjectDispose;

    procedure Subject_Create_ArgumentChecking;
    procedure Subject_Create1;
    procedure Subject_Create2;
  end;

implementation

uses
  Spring,
  Spring.TestUtils,
  Spring.Reactive,
  Spring.Reactive.Internal.Observers,
  Spring.Reactive.Subjects.Subject,
  Spring.Reactive.Testing;


{$REGION 'SubjectTest'}

procedure SubjectTest.Subscribe_ArgumentChecking;
begin
  CheckException(EArgumentNullException,
    procedure
    var
      s: ISubject<Integer>;
    begin
      s := TSubject<Integer>.Create;
      s.Subscribe(IObserver<Integer>(nil));
    end);
end;

procedure SubjectTest.OnError_ArgumentChecking;
begin
  CheckException(EArgumentNullException,
    procedure
    var
      s: ISubject<Integer>;
    begin
      s := TSubject<Integer>.Create;
      s.OnError(nil);
    end);
end;

procedure SubjectTest.Infinite;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  xs: ITestableObservable<Integer>;
  s: ISubject<Integer>;
  subscription: IDisposable;
  results1: ITestableObserver<Integer>;
  subscription1: IDisposable;
  results2: ITestableObserver<Integer>;
  subscription2: IDisposable;
  results3: ITestableObserver<Integer>;
  subscription3: IDisposable;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;
  xs := scheduler.CreateHotObservable([
    OnNext(70, 1),
    OnNext(110, 2),
    OnNext(220, 3),
    OnNext(270, 4),
    OnNext(340, 5),
    OnNext(410, 6),
    OnNext(520, 7),
    OnNext(630, 8),
    OnNext(710, 9),
    OnNext(870, 10),
    OnNext(940, 11),
    OnNext(1020, 12)
  ]);

  results1 := scheduler.CreateObserver<Integer>;
  results2 := scheduler.CreateObserver<Integer>;
  results3 := scheduler.CreateObserver<Integer>;

  scheduler.ScheduleAbsolute(100,
    procedure
    begin
      s := TSubject<Integer>.Create;
    end);
  scheduler.ScheduleAbsolute(200,
    procedure
    begin
      subscription := xs.Subscribe(s as IObserver<Integer>);
    end);
  scheduler.ScheduleAbsolute(1000,
    procedure
    begin
      subscription.Dispose;
    end);

  scheduler.ScheduleAbsolute(300,
    procedure
    begin
      subscription1 := s.Subscribe(results1);
    end);
  scheduler.ScheduleAbsolute(400,
    procedure
    begin
      subscription2 := s.Subscribe(results2);
    end);
  scheduler.ScheduleAbsolute(900,
    procedure
    begin
      subscription3 := s.Subscribe(results3);
    end);

  scheduler.ScheduleAbsolute(600,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(700,
    procedure
    begin
      subscription2.Dispose;
    end);
  scheduler.ScheduleAbsolute(800,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(950,
    procedure
    begin
      subscription3.Dispose;
    end);

  scheduler.Start;

  Check(results1.Messages.EqualsTo([
    OnNext(340, 5),
    OnNext(410, 6),
    OnNext(520, 7)
  ]));

  Check(results2.Messages.EqualsTo([
    OnNext(410, 6),
    OnNext(520, 7),
    OnNext(630, 8)
  ]));

  Check(results3.Messages.EqualsTo([
    OnNext(940, 11)
  ]));
end;

procedure SubjectTest.Finite;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  xs: ITestableObservable<Integer>;
  s: TSubject<Integer>;
  subscription: IDisposable;
  results1: ITestableObserver<Integer>;
  subscription1: IDisposable;
  results2: ITestableObserver<Integer>;
  subscription2: IDisposable;
  results3: ITestableObserver<Integer>;
  subscription3: IDisposable;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;

  xs := scheduler.CreateHotObservable([
    OnNext(70, 1),
    OnNext(110, 2),
    OnNext(220, 3),
    OnNext(270, 4),
    OnNext(340, 5),
    OnNext(410, 6),
    OnNext(520, 7),
    OnCompleted<Integer>(630),
    OnNext(640, 9),
    OnCompleted<Integer>(650),
    OnError<Integer>(660, Exception.Create(''), True)
  ]);

  results1 := scheduler.CreateObserver<Integer>;
  results2 := scheduler.CreateObserver<Integer>;
  results3 := scheduler.CreateObserver<Integer>;

  scheduler.ScheduleAbsolute(100,
    procedure
    begin
      s := TSubject<Integer>.Create;
    end);
  scheduler.ScheduleAbsolute(200,
    procedure
    begin
      subscription := xs.Subscribe(s);
    end);
  scheduler.ScheduleAbsolute(1000,
    procedure
    begin
      subscription.Dispose;
    end);

  scheduler.ScheduleAbsolute(300,
    procedure
    begin
      subscription1 := s.Subscribe(results1);
    end);
  scheduler.ScheduleAbsolute(400,
    procedure
    begin
      subscription2 := s.Subscribe(results2);
    end);
  scheduler.ScheduleAbsolute(900,
    procedure
    begin
      subscription3 := s.Subscribe(results3);
    end);

  scheduler.ScheduleAbsolute(600,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(700,
    procedure
    begin
      subscription2.Dispose;
    end);
  scheduler.ScheduleAbsolute(800,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(950,
    procedure
    begin
      subscription3.Dispose;
    end);

  scheduler.Start;

  Check(results1.Messages.EqualsTo([
    OnNext(340, 5),
    OnNext(410, 6),
    OnNext(520, 7)
  ]));

  Check(results2.Messages.EqualsTo([
    OnNext(410, 6),
    OnNext(520, 7),
    OnCompleted<Integer>(630)
  ]));

  Check(results3.Messages.EqualsTo([
    OnCompleted<Integer>(900)
  ]));
end;

procedure SubjectTest.Error;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  ex: Exception;
  xs: ITestableObservable<Integer>;
  s: TSubject<Integer>;
  subscription: IDisposable;
  results1: ITestableObserver<Integer>;
  subscription1: IDisposable;
  results2: ITestableObserver<Integer>;
  subscription2: IDisposable;
  results3: ITestableObserver<Integer>;
  subscription3: IDisposable;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;

  ex := Exception.Create('');

  xs := scheduler.CreateHotObservable([
    OnNext(70, 1),
    OnNext(110, 2),
    OnNext(220, 3),
    OnNext(270, 4),
    OnNext(340, 5),
    OnNext(410, 6),
    OnNext(520, 7),
    OnError<Integer>(630, ex, True),
    OnNext(640, 9),
    OnCompleted<Integer>(650),
    OnError<Integer>(660, Exception.Create(''), True)
  ]);

  results1 := scheduler.CreateObserver<Integer>;
  results2 := scheduler.CreateObserver<Integer>;
  results3 := scheduler.CreateObserver<Integer>;

  scheduler.ScheduleAbsolute(100,
    procedure
    begin
      s := TSubject<Integer>.Create;
    end);
  scheduler.ScheduleAbsolute(200,
    procedure
    begin
      subscription := xs.Subscribe(s);
    end);
  scheduler.ScheduleAbsolute(1000,
    procedure
    begin
      subscription.Dispose;
    end);

  scheduler.ScheduleAbsolute(300,
    procedure
    begin
      subscription1 := s.Subscribe(results1);
    end);
  scheduler.ScheduleAbsolute(400,
    procedure
    begin
      subscription2 := s.Subscribe(results2);
    end);
  scheduler.ScheduleAbsolute(900,
    procedure
    begin
      subscription3 := s.Subscribe(results3);
    end);

  scheduler.ScheduleAbsolute(600,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(700,
    procedure
    begin
      subscription2.Dispose;
    end);
  scheduler.ScheduleAbsolute(800,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(950,
    procedure
    begin
      subscription3.Dispose;
    end);

  scheduler.Start;

  Check(results1.Messages.EqualsTo([
    OnNext(340, 5),
    OnNext(410, 6),
    OnNext(520, 7)
  ]));

  Check(results2.Messages.EqualsTo([
    OnNext(410, 6),
    OnNext(520, 7),
    OnError<Integer>(630, ex)
  ]));

  Check(results3.Messages.EqualsTo([
    OnError<Integer>(900, ex)
  ]));
end;

procedure SubjectTest.Canceled;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  xs: ITestableObservable<Integer>;
  s: TSubject<Integer>;
  subscription: IDisposable;
  results1: ITestableObserver<Integer>;
  subscription1: IDisposable;
  results2: ITestableObserver<Integer>;
  subscription2: IDisposable;
  results3: ITestableObserver<Integer>;
  subscription3: IDisposable;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;

  xs := scheduler.CreateHotObservable([
    OnCompleted<Integer>(630),
    OnNext(640, 9),
    OnCompleted<Integer>(650),
    OnError<Integer>(660, Exception.Create(''), True)
  ]);

  results1 := scheduler.CreateObserver<Integer>;
  results2 := scheduler.CreateObserver<Integer>;
  results3 := scheduler.CreateObserver<Integer>;

  scheduler.ScheduleAbsolute(100,
    procedure
    begin
      s := TSubject<Integer>.Create;
    end);
  scheduler.ScheduleAbsolute(200,
    procedure
    begin
      subscription := xs.Subscribe(s);
    end);
  scheduler.ScheduleAbsolute(1000,
    procedure
    begin
      subscription.Dispose;
    end);

  scheduler.ScheduleAbsolute(300,
    procedure
    begin
      subscription1 := s.Subscribe(results1);
    end);
  scheduler.ScheduleAbsolute(400,
    procedure
    begin
      subscription2 := s.Subscribe(results2);
    end);
  scheduler.ScheduleAbsolute(900,
    procedure
    begin
      subscription3 := s.Subscribe(results3);
    end);

  scheduler.ScheduleAbsolute(600,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(700,
    procedure
    begin
      subscription2.Dispose;
    end);
  scheduler.ScheduleAbsolute(800,
    procedure
    begin
      subscription1.Dispose;
    end);
  scheduler.ScheduleAbsolute(950,
    procedure
    begin
      subscription3.Dispose;
    end);

  scheduler.Start;

  Check(results1.Messages.EqualsTo([]));

  Check(results2.Messages.EqualsTo([
    OnCompleted<Integer>(630)
  ]));

  Check(results3.Messages.EqualsTo([
    OnCompleted<Integer>(900)
  ]));
end;

procedure SubjectTest.Dispose;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  s: ISubject<Integer>;
  results: ITestableObserver<Integer>;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;

  s := TSubject<Integer>.Create;

  scheduler.ScheduleAbsolute(300, procedure begin s.OnNext(1) end);
  scheduler.ScheduleAbsolute(998, procedure begin s.OnNext(2) end);
  scheduler.ScheduleAbsolute(999, procedure begin s.OnNext(3) end);
  scheduler.ScheduleAbsolute(1001, procedure begin s.OnNext(3) end);

  results := scheduler.Start<Integer>(function: IObservable<Integer> begin Result := s; end);

  Check(results.Messages.EqualsTo([
    OnNext(300, 1),
    OnNext(998, 2),
    OnNext(999, 3)
  ]));
end;

procedure SubjectTest.PreComplete;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  s: ISubject<Integer>;
  results: ITestableObserver<Integer>;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;

  s := TSubject<Integer>.Create;

  scheduler.ScheduleAbsolute(90, procedure begin s.OnCompleted end);

  results := scheduler.Start<Integer>(function: IObservable<Integer> begin Result := s; end);

  Check(results.Messages.EqualsTo([
    OnCompleted<Integer>(200)
  ]));
end;

procedure SubjectTest.SubjectDispose;
var
  scheduler: TTestScheduler;
  lifetime: IInterface;
  subject: ISubject<Integer>;
  results1,
  results2,
  results3: ITestableObserver<Integer>;
  subscription1,
  subscription2,
  subscription3: IDisposable;
  ex: Shared<Exception>;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;

  results1 := scheduler.CreateObserver<Integer>;
  results2 := scheduler.CreateObserver<Integer>;
  results3 := scheduler.CreateObserver<Integer>;

  scheduler.ScheduleAbsolute(100, procedure begin subject := TSubject<Integer>.Create; end);
  scheduler.ScheduleAbsolute(200, procedure begin subscription1 := subject.Subscribe(results1); end);
  scheduler.ScheduleAbsolute(300, procedure begin subscription2 := subject.Subscribe(results2); end);
  scheduler.ScheduleAbsolute(400, procedure begin subscription3 := subject.Subscribe(results3); end);
  scheduler.ScheduleAbsolute(500, procedure begin subscription1.Dispose; end);
  scheduler.ScheduleAbsolute(600,
    procedure
    begin
      subject.Dispose;
    end);
  scheduler.ScheduleAbsolute(700, procedure begin subscription2.Dispose; end);
  scheduler.ScheduleAbsolute(800, procedure begin subscription3.Dispose; end);

  scheduler.ScheduleAbsolute(150, procedure begin subject.OnNext(1); end);
  scheduler.ScheduleAbsolute(250, procedure begin subject.OnNext(2); end);
  scheduler.ScheduleAbsolute(350, procedure begin subject.OnNext(3); end);
  scheduler.ScheduleAbsolute(450, procedure begin subject.OnNext(4); end);
  scheduler.ScheduleAbsolute(550, procedure begin subject.OnNext(5); end);
  scheduler.ScheduleAbsolute(650,
    procedure
    begin
      CheckException(EObjectDisposedException,
        procedure
        begin
          subject.OnNext(6);
        end);
    end);
  scheduler.ScheduleAbsolute(750,
    procedure
    begin
      CheckException(EObjectDisposedException,
        procedure
        begin
          subject.OnCompleted;
        end);
    end);
  scheduler.ScheduleAbsolute(850,
    procedure
    begin
      CheckException(EObjectDisposedException,
        procedure
        begin
          ex := Exception.Create('');
          subject.OnError(ex);
        end);
    end);
  scheduler.ScheduleAbsolute(950,
    procedure
    begin
      CheckException(EObjectDisposedException,
        procedure
        begin
          subject.Subscribe();
        end);
    end);

  scheduler.Start;

  Check(results1.Messages.EqualsTo([
    OnNext(250, 2),
    OnNext(350, 3),
    OnNext(450, 4)
  ]));

  Check(results2.Messages.EqualsTo([
    OnNext(350, 3),
    OnNext(450, 4),
    OnNext(550, 5)
  ]));

  Check(results3.Messages.EqualsTo([
    OnNext(450, 4),
    OnNext(550, 5)
  ]));
end;

procedure SubjectTest.Subject_Create_ArgumentChecking;
begin
  CheckException(EArgumentNullException,
    procedure
    begin
      TSubject.Create<Integer, Integer>(nil, TObservable.Return<Integer>(42));
    end);
  CheckException(EArgumentNullException,
    procedure
    begin
      TSubject.Create<Integer, Integer>(TObserver.Create<Integer>(
        procedure(const x: Integer) begin end), nil);
    end);
  CheckException(EArgumentNullException,
    procedure
    begin
      TSubject.Create<Integer>(nil, TObservable.Return<Integer>(42));
    end);
  CheckException(EArgumentNullException,
    procedure
    begin
      TSubject.Create<Integer>(TObserver.Create<Integer>(
        procedure(const x: Integer) begin end), nil);
    end);
end;

procedure SubjectTest.Subject_Create1;
var
  _x: Integer;
  _ex: Exception;
  done: Boolean;
  v: IObserver<Integer>;
  o: IObservable<Integer>;
  s: ISubject<Integer, Integer>;
  e: Shared<Exception>;
begin
  _x := 0;
  _ex := nil;
  done := False;

  v := TObserver.Create<Integer>(
    procedure(const x: Integer)
    begin
      _x := x;
    end,
    procedure(const ex: Exception)
    begin
      _ex := ex;
    end,
    procedure
    begin
      done := True;
    end);

  o := TObservable.Return<Integer>(42);

  s := TSubject.Create<Integer, Integer>(v, o);

  CheckException(EArgumentNullException,
    procedure
    begin
      s.Subscribe(IObserver<Integer>(nil));
    end);
  s.Subscribe(
    procedure(const x: Integer)
    begin
      _x := x;
    end);
  CheckEquals(42, _x);

  s.OnNext(21);
  CheckEquals(21, _x);

  CheckException(EArgumentNullException,
    procedure
    begin
      s.OnError(nil)
    end);
  e := Exception.Create('');
  s.OnError(e);
  CheckSame(e, _ex);

  s.OnCompleted;
  CheckFalse(done);

  s := nil; // need to set to nil explicitly to resolve circular reference caused by capturing s
end;

procedure SubjectTest.Subject_Create2;
var
  _x: Integer;
  _ex: Exception;
  done: Boolean;
  v: IObserver<Integer>;
  o: IObservable<Integer>;
  s: ISubject<Integer>;
  e: Shared<Exception>;
begin
  _x := 0;
  _ex := nil;
  done := False;

  v := TObserver.Create<Integer>(
    procedure(const x: Integer)
    begin
      _x := x;
    end,
    procedure(const ex: Exception)
    begin
      _ex := ex;
    end,
    procedure
    begin
      done := True;
    end);

  o := TObservable.Return<Integer>(42);

  s := TSubject.Create<Integer>(v, o);

  CheckException(EArgumentNullException,
    procedure
    begin
      s.Subscribe(IObserver<Integer>(nil));
    end);
  s.Subscribe(
    procedure(const x: Integer)
    begin
      _x := x;
    end);
  CheckEquals(42, _x);

  s.OnNext(21);
  CheckEquals(21, _x);

  CheckException(EArgumentNullException,
    procedure
    begin
      s.OnError(nil)
    end);
  e := Exception.Create('');
  s.OnError(e);
  CheckSame(e, _ex);

  s.OnCompleted;
  CheckFalse(done);

  s := nil; // need to set to nil explicitly to resolve circular reference caused by capturing s
end;

{$ENDREGION}


initialization
  RegisterTest(SubjectTest.Suite);

end.
