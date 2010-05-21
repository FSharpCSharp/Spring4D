{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Tests.Utils;

{$I Spring.inc}

interface

uses
  TestFramework,
  Spring.Utils;

type
  TTestDecimalCalculator = class(TTestCase)
  private
    fCalculator: TBaseNCalculator;
  protected
    procedure SetUp; override;
  published
    procedure TestAdd;
    procedure TestSubtract;
    procedure TestConvertToDecimal;
    procedure TestConvertFromDecimal;
    procedure TestGetQuantity;
    procedure TestFormatNumber;
  end;

  TTestHexCalculator = class(TTestCase)
  private
    fCalculator: TBaseNCalculator;
  protected
    procedure SetUp; override;
  published
    procedure TestAdd;
    procedure TestSubtract;
    procedure TestConvertToDecimal;
    procedure TestConvertFromDecimal;
  end;

  /// Base-9: '0'-'9' excepts '4'.
  TTestBaseNineCalculator = class(TTestCase)
  private
    fCalculator: TBaseNCalculator;
  protected
    procedure SetUp; override;
  published
    procedure TestConvertToDecimal;
    procedure TestConvertFromDecimal;
    procedure TestGetQuantity;
    procedure TestGetEndNumber;
  end;

implementation


{$REGION 'TTestDecimalCalculator'}

procedure TTestDecimalCalculator.SetUp;
begin
  inherited;
  fCalculator := TBaseNCalculator.Create('0123456789');
end;

procedure TTestDecimalCalculator.TestAdd;
begin
  CheckEquals('0', fCalculator.Add('0', '0'));
  CheckEquals('2', fCalculator.Add('1', '1'));
  CheckEquals('10', fCalculator.Add('9', '1'));
  CheckEquals('11', fCalculator.Add('9', '2'));
  CheckEquals('30', fCalculator.Add('19', '11'));
  CheckEquals(
    '123456789022222221011111111100',
    fCalculator.Add('123456789012345678901234567890', '9876542109876543210')
  );
end;

procedure TTestDecimalCalculator.TestSubtract;
begin
  CheckEquals('0', fCalculator.Subtract('0', '0'));
  CheckEquals('1', fCalculator.Subtract('9', '8'));
  CheckEquals('00', fCalculator.Subtract('10', '10'));
  CheckEquals('07', fCalculator.Subtract('10', '3'));
  CheckEquals('01', fCalculator.Subtract('10', '09'));
  CheckEquals('08', fCalculator.Subtract('17', '09'));
  CheckEquals(
    '123456789012345678901234567890',
    fCalculator.Subtract('123456789022222221011111111100', '9876542109876543210')
  );
end;

procedure TTestDecimalCalculator.TestConvertFromDecimal;
begin
  CheckEquals('12345', fCalculator.ConvertFromDecimal(12345));
end;

procedure TTestDecimalCalculator.TestConvertToDecimal;
begin
  CheckEquals(123456, fCalculator.ConvertToDecimal('123456'));
end;

procedure TTestDecimalCalculator.TestGetQuantity;
begin
  CheckEquals(1, fCalculator.GetQuantity('12345', '12345'));
  CheckEquals(54321 - 12345 + 1, fCalculator.GetQuantity('54321', '12345'));
  CheckEquals(1 - 0 + 1, fCalculator.GetQuantity('1', '0'));
end;

procedure TTestDecimalCalculator.TestFormatNumber;
begin
  CheckEquals('000001', fCalculator.FormatNumber('1', 6));
  CheckEquals('123456', fCalculator.FormatNumber('123456', 6));
end;

{$ENDREGION}


{$REGION 'TTestHexCalculator'}

procedure TTestHexCalculator.SetUp;
begin
  inherited;
  fCalculator := TBaseNCalculator.Create('0123456789ABCDEF');
end;

procedure TTestHexCalculator.TestAdd;
begin
  CheckEquals('11', fCalculator.Add('09', '08'));
  CheckEquals('3E', fCalculator.Add('1F', '1F'));
end;

procedure TTestHexCalculator.TestSubtract;
begin
  CheckEquals('0F', fCalculator.Subtract('10', '01'));
  CheckEquals('05', fCalculator.Subtract('1F', '1A'));
end;

procedure TTestHexCalculator.TestConvertFromDecimal;
begin
  CheckEquals('FFFF', fCalculator.ConvertFromDecimal(65535));
end;

procedure TTestHexCalculator.TestConvertToDecimal;
begin
  CheckEquals(65535, fCalculator.ConvertToDecimal('FFFF'));
end;

{$ENDREGION}


{$REGION 'TTestBaseNineCalculator'}

procedure TTestBaseNineCalculator.SetUp;
begin
  inherited;
  fCalculator := TBaseNCalculator.Create('012356789');
end;

procedure TTestBaseNineCalculator.TestGetQuantity;
begin
  CheckEquals(350, fCalculator.GetQuantity('802009000001', '802009000529'));
  CheckEquals(350, fCalculator.GetQuantity('802009000530', '802009000968'));
  CheckEquals(400, fCalculator.GetQuantity('802009000969', '802009001562'));
end;

procedure TTestBaseNineCalculator.TestConvertFromDecimal;
begin
  CheckEquals('5', fCalculator.ConvertFromDecimal(4));
end;

procedure TTestBaseNineCalculator.TestConvertToDecimal;
begin
  CheckEquals(4, fCalculator.ConvertToDecimal('5'));
end;

procedure TTestBaseNineCalculator.TestGetEndNumber;
begin
  CheckEquals('802009000000', fCalculator.GetEndNumber('802009000000', 1));
  CheckEquals('802009000001', fCalculator.GetEndNumber('802009000000', 2));
  CheckEquals('802009000005', fCalculator.GetEndNumber('802009000000', 5));
  CheckEquals('802009000001', fCalculator.GetEndNumber('802009000001', 1));
  CheckEquals('802009000002', fCalculator.GetEndNumber('802009000001', 2));
  CheckEquals('802009000005', fCalculator.GetEndNumber('802009000001', 4));
  CheckEquals('802009000529', fCalculator.GetEndNumber('802009000001', 350));
  CheckEquals('802009000968', fCalculator.GetEndNumber('802009000530', 350));
  CheckEquals('802009001562', fCalculator.GetEndNumber('802009000969', 400));
end;

{$ENDREGION}

end.
