{***************************************************************************}
{                                                                           }
{               Generic Number Generator                                    }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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

unit Spring.Tests.Numbering;

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring.Numbering,
  Spring.Numbering.Rules;

type
  TNumberRuleTestCase = class abstract(TTestCase)
  protected
    fBuilder: TNumberRuleBuilder;
    fMockDateTime: TDateTime;
    function GetMockDateTime: TDateTime;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestNumberRuleBuilder = class(TNumberRuleTestCase)
  private
    fRule: INumberRule;
  protected
    procedure TearDown; override;
  published
    procedure TestCode;
    procedure TestLetters;
    procedure TestDigits;
    procedure TestDateYYYYMM;
    procedure TestDateYYYYMMDD;
    procedure TestDateYYYYWW;
    procedure TestCompositeNumber;
    procedure TestFirstAndLast;
    procedure TestSuffix;
    procedure TestSkipEndsWithFour;
  end;

implementation

{$REGION 'TNumberRuleTestCase'}

procedure TNumberRuleTestCase.SetUp;
begin
  inherited;
  fBuilder := TNumberRuleBuilder.Create;
end;

procedure TNumberRuleTestCase.TearDown;
begin
  fBuilder.Clear;
  inherited;
end;

function TNumberRuleTestCase.GetMockDateTime: TDateTime;
begin
  Result := fMockDateTime;
end;

{$ENDREGION}


{$REGION 'TTestNumberRuleBuilder'}

procedure TTestNumberRuleBuilder.TearDown;
begin
  fRule := nil;
  inherited TearDown;
end;

procedure TTestNumberRuleBuilder.TestCode;
begin
  fBuilder.AddCode('RK');
  fRule := fBuilder.ToRule;
  CheckEquals('RK', fRule.GetNextNumber('RK'));
end;

procedure TTestNumberRuleBuilder.TestLetters;
var
  i: AnsiChar;
begin
  fBuilder.AddLetters;
  fRule := fBuilder.ToRule;
  for i := 'A' to 'Y' do
  begin
    CheckEquals(Chr(Ord(i)+1), fRule.GetNextNumber(string(i)));
  end;
end;

procedure TTestNumberRuleBuilder.TestDigits;
begin
  fBuilder.AddDigits('0001', '9999');
  fRule := fBuilder.ToRule;
  CheckEquals('0002', fRule.GetNextNumber('0001'));
  CheckEquals('0999', fRule.GetNextNumber('0998'));
  CheckEquals('1000', fRule.GetNextNumber('0999'));
end;

procedure TTestNumberRuleBuilder.TestCompositeNumber;
begin
  with fBuilder do
  begin
    AddCode('RK');
    AddLetters;
    AddDigits('001', '999');
  end;
  fRule := fBuilder.ToRule;
  CheckEquals('RKA002', fRule.GetNextNumber('RKA001'));
  CheckEquals('RKA999', fRule.GetNextNumber('RKA998'));
  CheckEquals('RKB001', fRule.GetNextNumber('RKA999'));
  CheckEquals('RKZ999', fRule.GetNextNumber('RKZ998'));
end;

procedure TTestNumberRuleBuilder.TestDateYYYYMM;
begin
  with fBuilder do
  begin
    AddCode('RK');
    AddDateTime('YYYYMM', GetMockDateTime);
    AddDigits('0001', '9999');
  end;
  fRule := fBuilder.ToRule;
  fMockDateTime := EncodeDate(2009, 1, 1);
  CheckEquals('RK2009010002', fRule.GetNextNumber('RK2009010001'));
  fMockDateTime := EncodeDate(2009, 2, 1);
  CheckEquals('RK2009020001', fRule.GetNextNumber('RK2009010999'));
end;

procedure TTestNumberRuleBuilder.TestDateYYYYMMDD;
begin
  with fBuilder do
  begin
    AddCode('RK');
    AddDateTime('YYYYMMDD', GetMockDateTime);
    AddDigits('0001', '9999');
  end;
  fRule := fBuilder.ToRule;
  fMockDateTime := EncodeDate(2009, 1, 1);
  CheckEquals('RK200901010002', fRule.GetNextNumber('RK200901010001'));
  fMockDateTime := EncodeDate(2009, 2, 1);
  CheckEquals('RK200902010001', fRule.GetNextNumber('RK200901010999'));
end;

procedure TTestNumberRuleBuilder.TestDateYYYYWW;
begin
  with fBuilder do
  begin
    AddCode('RK');
    AddDateTime('YYYYWW', GetMockDateTime);
    AddDigits('0001', '9999');
  end;
  fRule := fBuilder.ToRule;

  fMockDateTime := EncodeDate(2009, 12, 7);   // 50th
  CheckEquals('RK2009500001', fRule.GetNextNumber('RK2009010001'));
  fMockDateTime := EncodeDate(2009, 12, 20);  // 51th
  CheckEquals('RK2009510002', fRule.GetNextNumber('RK2009510001'));
  fMockDateTime := EncodeDate(2009, 12, 21);  // 52th
  CheckEquals('RK2009520001', fRule.GetNextNumber('RK2009510099'));


  // 2008.12.29 - 2008.12.31 belongs the first week of the year 2009
  fMockDateTime := EncodeDate(2008, 12, 29);
  CheckEquals('RK2009010001', fRule.GetNextNumber('RK2008500002'));
  fMockDateTime := EncodeDate(2009, 1, 1);
  CheckEquals('RK2009010001', fRule.GetNextNumber('RK2008500002'));
  fMockDateTime := EncodeDate(2009, 1, 2);
  CheckEquals('RK2009010001', fRule.GetNextNumber('RK2008520002'));
  fMockDateTime := EncodeDate(2009, 1, 3);
  CheckEquals('RK2009010001', fRule.GetNextNumber('RK2008520002'));
  fMockDateTime := EncodeDate(2009, 1, 4);
  CheckEquals('RK2009010001', fRule.GetNextNumber('RK2008520099'));
  fMockDateTime := EncodeDate(2009, 1, 5);
  CheckEquals('RK2009020001', fRule.GetNextNumber('RK2009010099'));

  // 2010.1.1 - 2010.1.3 belongs the last week of the year 2009
  fMockDateTime := EncodeDate(2009, 12, 31);
  CheckEquals('RK2009530001', fRule.GetNextNumber('RK2009120001'));
  fMockDateTime := EncodeDate(2010, 1, 1);
  CheckEquals('RK2009530002', fRule.GetNextNumber('RK2009530001'));
  fMockDateTime := EncodeDate(2010, 1, 2);
  CheckEquals('RK2009530002', fRule.GetNextNumber('RK2009530001'));
  fMockDateTime := EncodeDate(2010, 1, 3);
  CheckEquals('RK2009530002', fRule.GetNextNumber('RK2009530001'));
  fMockDateTime := EncodeDate(2010, 1, 4);
  CheckEquals('RK2010010001', fRule.GetNextNumber('RK2009530099'));
end;

procedure TTestNumberRuleBuilder.TestFirstAndLast;
begin
  with fBuilder do
  begin
    AddCode('RK');
    AddDateTime('YYYYMM', GetMockDateTime);
    AddLetters;
    AddDigits('0001', '9999');
  end;
  fRule := fBuilder.ToRule;
  fMockDateTime := EncodeDate(2009, 1, 1);
  CheckEquals('RK200901A0001', fRule.GetFirstNumber);
  CheckEquals('RK200901Z9999', fRule.GetLastNumber);
end;

procedure TTestNumberRuleBuilder.TestSuffix;
begin
  with fBuilder do
  begin
    AddCode('RK');
    AddDigits('0001', '9999');
    AddCode('07');
  end;
  fRule := fBuilder.ToRule;
  fMockDateTime := EncodeDate(2009, 1, 1);
  CheckEquals('RK000107', fRule.GetFirstNumber);
  CheckEquals('RK999907', fRule.GetLastNumber);
  CheckEquals('RK000207', fRule.GetNextNumber('RK000107'));
  CheckEquals('RK999907', fRule.GetNextNumber('RK999807'));
end;

procedure TTestNumberRuleBuilder.TestSkipEndsWithFour;
begin
  with fBuilder do
  begin
    fBuilder.AddCode('8000');
    fBuilder.AddDigits('000', '999');
    fBuilder.AddSequence('012356789', 1, '0', '9');
  end;
  fRule := fBuilder.ToRule;
  CheckEquals('80000005', fRule.GetNextNumber('80000003'));
  CheckEquals('80000010', fRule.GetNextNumber('80000009'));
  CheckEquals('80000040', fRule.GetNextNumber('80000039'));
  // TODO: Failed.
//  CheckEquals('80001111', fRule.GetEndNumber('80000001', 1000));
end;

{$ENDREGION}

end.
