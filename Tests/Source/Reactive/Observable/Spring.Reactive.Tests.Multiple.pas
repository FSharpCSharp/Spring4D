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

unit Spring.Reactive.Tests.Multiple;

interface

uses
  TestFramework,
  Spring.Reactive.Testing.ReactiveTest,
  Spring.Reactive.Testing.TestScheduler;

type
  ObservableMultipleTest = class(TReactiveTest)
  private
    scheduler: TTestScheduler;
    lifetime: IInterface;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TakeUntil_Preempt_SomeData_Next;
  end;

implementation

uses
  Spring.TestUtils,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Testing;


{ ObservableMultipleTest }

procedure ObservableMultipleTest.SetUp;
begin
  scheduler := TTestScheduler.Create;
  lifetime := scheduler;
end;

procedure ObservableMultipleTest.TakeUntil_Preempt_SomeData_Next;
var
  l, r: ITestableObservable<Integer>;
  res: ITestableObserver<Integer>;
begin
  l := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(210, 2),
    OnNext(220, 3),
    OnNext(230, 4),
    OnNext(240, 5),
    OnCompleted<Integer>(250)
  ]);

  r := scheduler.CreateHotObservable([
    OnNext(150, 1),
    OnNext(225, 99),
    OnCompleted<Integer>(230)
   ]);

  res := scheduler.Start<Integer>(
    function: IObservable<Integer>
    begin
      Result := l._.TakeUntil<Integer,Integer>(r);
    end);

  Check(res.Messages.EqualsTo([
    OnNext(210, 2),
    OnNext(220, 3),
    OnCompleted<Integer>(225)
  ]));

  Check(l.Subscriptions.EqualsTo([
    Subscribe(200, 225)
  ]));

  Check(r.Subscriptions.EqualsTo([
    Subscribe(200, 225)
  ]));
end;

procedure ObservableMultipleTest.TearDown;
begin
  lifetime := nil;
end;

initialization
  RegisterTest(ObservableMultipleTest.Suite);

end.
