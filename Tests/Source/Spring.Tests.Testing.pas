{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit Spring.Tests.Testing;

interface

{$I Spring.inc}
{$I Spring.Tests.inc}

uses
  Spring.Testing;

type
  TTestEnum = (zero, one, two, three);


  TestCaseAttribute = class(Spring.Testing.TestCaseAttribute)
  public
    constructor Create(x, y, expected: Integer); overload;
  end;

  TSelfTest = class(TTestCase)
    [Sequential]
    procedure TestEnum(
      [Values]value: TTestEnum;
      [Range(Ord(Low(TTestEnum)), Ord(High(TTestEnum)))]ordValue: Integer);

    [TestCase('foo')]
    procedure TestParams(const s1, s2: string);

    [TestCase('foo;bar;foobar')]
    function TestResult(const s1, s2: string): string;

    [TestCase(1, 2, 3)]
    [TestCase(-1, 2, 1)]
    [TestCase(1, -2, -1)]
    [TestCase(-1, -2, -3)]
    function TestCustomAttribute(const x, y: Integer): Integer;
  end;

implementation

uses
  Rtti,
  TypInfo;


{$REGION 'TSelfTest'}

function TSelfTest.TestCustomAttribute(const x, y: Integer): Integer;
begin
  Result := x + y;
end;

procedure TSelfTest.TestEnum(value: TTestEnum; ordValue: Integer);
begin
  CheckEquals(ordValue, Ord(value));
end;

procedure TSelfTest.TestParams(const s1, s2: string);
begin
  CheckEqualsString('foo', s1);
  CheckEqualsString('', s2);
end;

function TSelfTest.TestResult(const s1, s2: string): string;
begin
  Result := s1 + s2;
end;

{$ENDREGION}


{$REGION 'TestCaseAttribute'}

constructor TestCaseAttribute.Create(x, y, expected: Integer);
begin
  fValues := TArray<TValue>.Create(x, y, expected);
end;

{$ENDREGION}


initialization
  TSelfTest.Register('Spring.Testing');

end.
