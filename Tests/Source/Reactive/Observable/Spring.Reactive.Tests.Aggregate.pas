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

unit Spring.Reactive.Tests.Aggregate;

interface

uses
  TestFramework,
  Spring.Reactive.Testing.ReactiveTest,
  Spring.Reactive.Testing.TestScheduler;

type
  ObservableAggregateTest = class(TReactiveTest)
  private
    scheduler: TTestScheduler;
    lifetime: IInterface;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure All_Empty;
    procedure All_Return;
    procedure All_ReturnNotMatch;
    procedure All_SomeNoneMatch;
    procedure All_SomeMatch;
    procedure All_SomeAllMatch;
    procedure All_Throw;
    procedure All_Never;
    procedure All_PredicateThrows;

    procedure Any_Empty;
    procedure Any_Return;
    procedure Any_Throw;
    procedure Any_Never;
    procedure Any_Predicate_Empty;
    procedure Any_Predicate_Return;
    procedure Any_Predicate_ReturnNotMatch;
    procedure Any_Predicate_SomeNoneMatch;
    procedure Any_Predicate_SomeMatch;
    procedure Any_Predicate_Throw;
  end;

implementation

uses
  Spring.TestUtils,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Testing;

{ ObservableAggregateTest }

procedure ObservableAggregateTest.SetUp;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;
end;

procedure ObservableAggregateTest.TearDown;
begin
  scheduler := nil;
  lifetime := nil;
end;

procedure ObservableAggregateTest.All_Empty;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(250, True),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.All_Return;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, 2),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(250, True),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.All_ReturnNotMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, -2),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(210, False),
    OnCompleted<Boolean>(210)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.All_SomeNoneMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, -2),
    OnNext(220, -3),
    OnNext(230, -4),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(210, False),
    OnCompleted<Boolean>(210)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.All_SomeMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, -2),
    OnNext(220, 3),
    OnNext(230, -4),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(210, False),
    OnCompleted<Boolean>(210)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.All_SomeAllMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, 2),
    OnNext(220, 3),
    OnNext(230, 4),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(250, True),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.All_Throw;
var
  ex: Shared<Exception>;
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  ex := Exception.Create('');

  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnError<Integer>(210, ex)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnError<Boolean>(210, ex)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.All_Never;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 1000)
  ]));
end;

procedure ObservableAggregateTest.All_PredicateThrows;
var
  ex: Exception;
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  ex := Exception.Create('');

  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, 2),
    OnNext(220, 3),
    OnNext(230, 4),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.All(function(const x: Integer): Boolean begin if x < 4 then Result := True else raise ex end) end);

  Check(res.Messages.EqualsTo([
    OnError<Boolean>(230, ex)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 230)
  ]));
end;

procedure ObservableAggregateTest.Any_Empty;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any end);

  Check(res.Messages.EqualsTo([
    OnNext(250, False),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.Any_Return;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, 2),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any end);

  Check(res.Messages.EqualsTo([
    OnNext(210, True),
    OnCompleted<Boolean>(210)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.Any_Throw;
var
  ex: Shared<Exception>;
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  ex := Exception.Create('');

  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnError<Integer>(210, ex)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any end);

  Check(res.Messages.EqualsTo([
    OnError<Boolean>(210, ex)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.Any_Never;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any end);

  Check(res.Messages.EqualsTo([
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 1000)
  ]));
end;

procedure ObservableAggregateTest.Any_Predicate_Empty;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(250, False),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.Any_Predicate_Return;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, 2),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(210, True),
    OnCompleted<Boolean>(210)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

procedure ObservableAggregateTest.Any_Predicate_ReturnNotMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, -2),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(250, False),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.Any_Predicate_SomeNoneMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, -2),
    OnNext(220, -3),
    OnNext(230, -4),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(250, False),
    OnCompleted<Boolean>(250)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 250)
  ]));
end;

procedure ObservableAggregateTest.Any_Predicate_SomeMatch;
var
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, -2),
    OnNext(220, 3),
    OnNext(230, -4),
    OnCompleted<Integer>(250)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnNext(220, True),
    OnCompleted<Boolean>(220)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 220)
  ]));
end;

procedure ObservableAggregateTest.Any_Predicate_Throw;
var
  ex: Shared<Exception>;
  xs: ITestableObservable<Integer>;
  res: ITestableObserver<Boolean>;
begin
  ex := Exception.Create('');

  xs := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnError<Integer>(210, ex)
  ]);

  res := scheduler.Start<Boolean>(function: IObservable<Boolean> begin Result := xs.Any(function(const x: Integer): Boolean begin Result := x > 0 end) end);

  Check(res.Messages.EqualsTo([
    OnError<Boolean>(210, ex)
  ]));

  Check(xs.Subscriptions.EqualsTo([
    Subscribe(200, 210)
  ]));
end;

initialization
  RegisterTest(ObservableAggregateTest.Suite);

end.
