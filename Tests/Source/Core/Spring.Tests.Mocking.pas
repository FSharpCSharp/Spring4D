{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
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

unit Spring.Tests.Mocking;

{$I Spring.Tests.inc}

interface

uses
  TestFramework,
  Spring.TestUtils;

type
  TParameterMatchingTests = class(TTestCase)
  published
    procedure ArgsEvaluationOrder;
    procedure OutParameterCanBeSet;
    procedure VerifyChecksParameterValuesProperly;
  end;

implementation

uses
  Spring.Mocking;

type
  IMockTest = interface(IInvokable)
    procedure Test1(i: Integer; const s: string);
    procedure Test2(const s: string; i: Integer; b: Boolean);
    procedure Test3(const s1: string; o: TObject; const s2: string);
    procedure Test4(const s1: string; o: ITest; const s2: string);
  end;

procedure TParameterMatchingTests.ArgsEvaluationOrder;
var
  mock: Mock<IMockTest>;
  sut: IMockTest;
begin
  mock := Mock<IMockTest>.Create(TMockBehavior.Strict);
  sut := mock.Instance;

  with mock.Setup.Executes do
  begin
    When.Test1(Arg.IsAny<Integer>, Arg.IsEqual('test'));
    sut.Test1(10, 'test');
    sut.Test1(-10, 'test');

    When.Test1(Arg.IsAny<Integer>, Arg.IsAny<string>);
    sut.Test1(0, '');

    When.Test1(Arg.IsEqual(10), Arg.IsEqual('test'));
    sut.Test1(10, 'test');

    When.Test2(Arg.IsAny<string>, Arg.IsEqual(10), Arg.IsEqual(True));
    sut.Test2('', 10, True);

    When.Test3(Arg.IsAny<string>, Arg.IsEqual(Self), Arg.IsAny<string>);
    sut.Test3('', Self, '');

    When.Test4(Arg.IsAny<string>, Arg.IsEqual<ITest>(Self), Arg.IsAny<string>);
    sut.Test4('', Self, '');
  end;

  mock.Reset;
  with mock.Setup.Executes do
  begin
    When.Test1(Arg.IsInRange(3, 6), Arg.IsAny<string>);
    sut.Test1(4, '');
    CheckException(EMockException,
      procedure
      begin
        sut.Test1(2, '');
      end);
  end;

  mock.Reset;
  with mock.Setup.Executes do
  begin
    When.Test1(Arg.IsIn([3, 4, 5, 6]), Arg.IsAny<string>);
    sut.Test1(4, '');
    CheckException(EMockException,
      procedure
      begin
        sut.Test1(2, '');
      end);
  end;

  Pass;
end;

type
  IOutParamTest = interface(IInvokable)
    procedure Test(out value: Integer);
  end;

procedure TParameterMatchingTests.OutParameterCanBeSet;
var
  mock: Mock<IOutParamTest>;
  i: Integer;
begin
  mock.Setup.Executes(
    function(const call: TCallInfo): TValue
    begin
      CheckEquals(42, call[0].AsInteger);
      call[0] := 43;
    end).When(Arg.Any).Test(i);
  i := 42;
  mock.Instance.Test(i);
  CheckEquals(43, i);
end;

procedure TParameterMatchingTests.VerifyChecksParameterValuesProperly;
var
  mock: Mock<IMockTest>;
  sut: IMockTest;
begin
  sut := mock;
  sut.Test1(4, 'test');
  mock.Received(Times.Once).Test1(Arg.IsAny<Integer>, 'test');
  CheckException(EMockException,
    procedure
    begin
      mock.Received.Test1(Arg.IsIn<Integer>([3, 5]), Arg.IsAny<string>);
    end);
end;

end.
