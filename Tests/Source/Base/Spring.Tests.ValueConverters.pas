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

unit Spring.Tests.ValueConverters;

interface

uses
  SysUtils,
  TestFramework,
  TestExtensions,
  Rtti,
  Spring.ValueConverters;

type
  TEnumeration = (teFirst, teSecond, teLast);

  TTestFromString = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestStringToInteger;
    procedure TestStringToFloat;
    procedure TestStringToEnum;
    procedure TestStringToBoolean;
    procedure TestStringToColor;
    procedure TestStringToCurrency;
    procedure TestStringToDateTime;
    procedure TestStringToDateTimeF;
    procedure TestStringToWideString;
    procedure TestStringToAnsiString;
    procedure TestStringToNullableString;
    procedure TestStringToNullableAnsiString;
    procedure TestStringToNullableWideString;
    procedure TestStringToNullableInteger;
    procedure TestStringToNullableFloat;
    procedure TestStringToNullableBoolean;
    procedure TestStringToNullableColor;
    procedure TestStringToNullableCurrency;
    procedure TestStringToNullableDateTime;
  end;

  TTestFromWideString = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestWStringToInteger;
    procedure TestWStringToFloat;
    procedure TestWStringToEnum;
    procedure TestWStringToBoolean;
    procedure TestWStringToColor;
    procedure TestWStringToCurrency;
    procedure TestWStringToDateTime;
    procedure TestWStringToDateTimeF;
    procedure TestWStringToString;
    procedure TestWStringToAnsiString;
    procedure TestWStringToNullableString;
    procedure TestWStringToNullableAnsiString;
    procedure TestWStringToNullableWideString;
    procedure TestWStringToNullableInteger;
    procedure TestWStringToNullableFloat;
    procedure TestWStringToNullableBoolean;
    procedure TestWStringToNullableColor;
    procedure TestWStringToNullableCurrency;
    procedure TestWStringToNullableDateTime;
  end;

  TTestFromInteger = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestIntegerToString;
    procedure TestSmallIntToString;
    procedure TestIntegerToWideString;
    procedure TestIntegerToAnsiString;
    procedure TestIntegerToEnum;
    procedure TestIntegerToBoolean;
    procedure TestIntegerToFloat;
    procedure TestIntegerToNullableInteger;
    procedure TestIntegerToNullableString;
    procedure TestIntegerToNullableAnsiString;
    procedure TestIntegerToNullableWideString;
    procedure TestIntegerToNullableBoolean;
    procedure TestIntegerToNullableFloat;
  end;

  TTestFromBoolean = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestBooleanToInteger;
    procedure TestBooleanToString;
    procedure TestBooleanToWideString;
    procedure TestBooleanToAnsiString;
    procedure TestBooleanToNullableBoolean;
    procedure TestBooleanToNullableInteger;
    procedure TestBooleanToNullableString;
    procedure TestBooleanToNullableAnsiString;
    procedure TestBooleanToNullableWideString;
  end;

  TTestFromEnum = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestEnumToInteger;
    procedure TestEnumToString;
    procedure TestEnumToAnsiString;
    procedure TestEnumToWideString;
    procedure TestEnumToNullableString;
    procedure TestEnumToNullableAnsiString;
    procedure TestEnumToNullableWideString;
    procedure TestEnumToNullableInteger;
  end;

  TTestFromFloat = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestFloatToInteger;
    procedure TestFloatToString;
    procedure TestFloatToAnsiString;
    procedure TestFloatToWideString;
    procedure TestFloatToStringF;
    procedure TestFloatToNullableFloat;
    procedure TestFloatToNullableInteger;
    procedure TestFloatToNullableString;
    procedure TestFloatToNullableAnsiString;
    procedure TestFloatToNullableWideString;
  end;

  TTestFromColor = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestColorToString;
    procedure TestColorToAnsiString;
    procedure TestColorToWideString;
    procedure TestColorToInteger;
    procedure TestColorToNullableColor;
    procedure TestColorToNullableString;
    procedure TestColorToNullableAnsiString;
    procedure TestColorToNullableWideString;
    procedure TestColorToNullableInteger;
  end;

  TTestFromCurrency = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestCurrencyToString;
    procedure TestCurrencyToAnsiString;
    procedure TestCurrencyToWideString;
    procedure TestCurrencyToStringF;
    procedure TestCurrencyToNullableString;
    procedure TestCurrencyToNullableAnsiString;
    procedure TestCurrencyToNullableWideString;
  end;

  TTestFromDateTime = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestDateTimeToString;
    procedure TestDateTimeToAnsiString;
    procedure TestDateTimeToWideString;
    procedure TestDateTimeToStringF;
    procedure TestDateTimeToNullableString;
    procedure TestDateTimeToNullableAnsiString;
    procedure TestDateTimeToNullableWideString;
  end;

  TTestFromObject = class(TTestCase)
  private
    fConverter: IValueConverter;

    type
      ITestInterface = interface
      ['{F98EAF66-33B6-4687-8052-4B76471D978B}']
        procedure Test;
      end;

      TTestObject = class(TInterfacedObject, ITestInterface)
      protected
        procedure Test;
      public
        function ToString: string; override;
      end;
  protected
    procedure SetUp; override;
  published
    procedure TestObjectToString;
    procedure TestObjectToAnsiString;
    procedure TestObjectToWideString;
    procedure TestObjectToInterface;
    procedure TestObjectToClass;
    procedure TestObjectToNullableString;
    procedure TestObjectToNullableAnsiString;
    procedure TestObjectToNullableWideString;
  end;

  TTestFromNullable = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestNullableIntegerToInteger;
    procedure TestNullableIntegerToFloat;
    procedure TestNullableIntegerToString;
    procedure TestNullableIntegerToWideString;
    procedure TestNullableIntegerToAnsiString;
    procedure TestNullableFloatToString;
    procedure TestNullableFloatToInteger;
    procedure TestNullableFloatToAnsiString;
    procedure TestNullableFloatToWideString;
    procedure TestNullableStringToString;
    procedure TestNullableAnsiStringToString;
    procedure TestNullableWideStringToWideString;
    procedure TestNullableStringToFloat;
    procedure TestNullableAnsiStringToFloat;
    procedure TestNullableWideStringToFloat;
    procedure TestNullableDateTimeToString;
    procedure TestNullableDateTimeToAnsiString;
    procedure TestNullableDateTimeToWideString;
    procedure TestNullableColorToString;
    procedure TestNullableColorToAnsiString;
    procedure TestNullableColorToWideString;
  end;

implementation

uses
  Graphics,
  DateUtils,
  Spring;


{$REGION 'TTestFromString'}

procedure TTestFromString.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromString.TestStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outValue.AsInteger, 1);
end;

procedure TTestFromString.TestStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromString.TestStringToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('teLast'),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teLast);
end;

procedure TTestFromString.TestStringToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('True'),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outBool, True);
end;

procedure TTestFromString.TestStringToColor;
var
  outValue: TValue;
  outColor: TColor;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('clBlue'),
    TypeInfo(TColor));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TColor>(outColor));
  CheckEquals(outColor, clBlue);
end;

procedure TTestFromString.TestStringToCurrency;
var
  outValue: TValue;
  outCurrency: Currency;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Currency));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Currency>(outCurrency));
  CheckEquals(outCurrency, 1.11);
end;

procedure TTestFromString.TestStringToDateTime;
var
  outValue: TValue;
  outStamp: TDateTime;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<string>(DateTimeToStr(stamp)),
    TypeInfo(TDateTime));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString(DateTimeToStr(outStamp), DateTimeToStr(stamp));
end;

procedure TTestFromString.TestStringToDateTimeF;
var
  outValue: TValue;
  outStamp: TDateTime;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('10.10.2010'),
    TypeInfo(TDateTime), TValue.From<string>('dd.mm.yyyy'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString(FormatDateTime('dd.mm.yyyy', outStamp), '10.10.2010');
end;

procedure TTestFromString.TestStringToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test AnsiString'),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEqualsString(string(outAStr), 'Test AnsiString');
end;

procedure TTestFromString.TestStringToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<UnicodeString>('Test WideString'),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, 'Test WideString');
end;

procedure TTestFromString.TestStringToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromString.TestStringToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, AnsiString('Test'));
end;

procedure TTestFromString.TestStringToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromString.TestStringToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromString.TestStringToNullableFloat;
var
  outValue: TValue;
  outNullable: TNullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(TNullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromString.TestStringToNullableBoolean;
var
  outValue: TValue;
  outNullable: TNullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('True'),
    TypeInfo(TNullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Boolean>>(outNullable));
  CheckEquals(outNullable.Value, True);
end;

procedure TTestFromString.TestStringToNullableColor;
var
  outValue: TValue;
  outNullable: TNullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(ColorToString(clRed)),
    TypeInfo(TNullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<TColor>>(outNullable));
  CheckEquals(ColorToString(outNullable.Value), 'clRed');
end;

procedure TTestFromString.TestStringToNullableCurrency;
var
  outValue: TValue;
  outNullable: TNullable<Currency>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(TNullable<Currency>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Currency>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromString.TestStringToNullableDateTime;
var
  outValue: TValue;
  outNullable: TNullable<TDateTime>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<string>(DateTimeToStr(stamp)),
    TypeInfo(TNullable<TDateTime>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<TDateTime>>(outNullable));
  CheckEqualsString(DateTimeToStr(outNullable.Value), DateTimeToStr(stamp));
end;

{$ENDREGION}


{$REGION 'TTestFromInteger'}

procedure TTestFromInteger.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromInteger.TestIntegerToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outBool, True);
end;

procedure TTestFromInteger.TestIntegerToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromInteger.TestIntegerToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outValue.AsExtended, 1.0);
end;

procedure TTestFromInteger.TestIntegerToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromInteger.TestSmallIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromInteger.TestIntegerToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromInteger.TestIntegerToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromInteger.TestIntegerToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromInteger.TestIntegerToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, '1');
end;

procedure TTestFromInteger.TestIntegerToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('1'));
end;

procedure TTestFromInteger.TestIntegerToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, '1');
end;

procedure TTestFromInteger.TestIntegerToNullableBoolean;
var
  outValue: TValue;
  outNullable: TNullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(0),
    TypeInfo(TNullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableFloat;
var
  outValue: TValue;
  outNullable: TNullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(0),
    TypeInfo(TNullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 0.0);
end;

{$ENDREGION}


{$REGION 'TTestFromBoolean'}

procedure TTestFromBoolean.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromBoolean.TestBooleanToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 0);
end;

procedure TTestFromBoolean.TestBooleanToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, 'False');
end;

procedure TTestFromBoolean.TestBooleanToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('False'));
end;

procedure TTestFromBoolean.TestBooleanToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, 'False');
end;

procedure TTestFromBoolean.TestBooleanToNullableBoolean;
var
  outValue: TValue;
  outNullable: TNullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(TNullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 0);
end;

procedure TTestFromBoolean.TestBooleanToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, 'False');
end;

procedure TTestFromBoolean.TestBooleanToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('False'));
end;

procedure TTestFromBoolean.TestBooleanToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEqualsString(outNullable.Value, 'False');
end;

{$ENDREGION}


{$REGION 'TTestFromEnum'}

procedure TTestFromEnum.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromEnum.TestEnumToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, 'teFirst');
end;

procedure TTestFromEnum.TestEnumToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('teFirst'));
end;

procedure TTestFromEnum.TestEnumToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, 'teFirst');
end;

procedure TTestFromEnum.TestEnumToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromEnum.TestEnumToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, 'teFirst');
end;

procedure TTestFromEnum.TestEnumToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('teFirst'));
end;

procedure TTestFromEnum.TestEnumToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, 'teFirst');
end;

procedure TTestFromEnum.TestEnumToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 2);
end;

{$ENDREGION}


{$REGION 'TTestFromFloat'}

procedure TTestFromFloat.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromFloat.TestFloatToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromFloat.TestFloatToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, FloatToStr(1.11));
end;

procedure TTestFromFloat.TestFloatToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString(FloatToStr(1.11)));
end;

procedure TTestFromFloat.TestFloatToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, FloatToStr(1.11));
end;

procedure TTestFromFloat.TestFloatToStringF;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(string), TValue.From<string>('#.000'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, FormatFloat('#.000', 1.11));
end;

procedure TTestFromFloat.TestFloatToNullableFloat;
var
  outValue: TValue;
  outNullable: TNullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(TNullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromFloat.TestFloatToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, FloatToStr(1.11));
end;

procedure TTestFromFloat.TestFloatToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(FloatToStr(1.11)));
end;

procedure TTestFromFloat.TestFloatToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, FloatToStr(1.11));
end;

procedure TTestFromFloat.TestFloatToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

{$ENDREGION}


{$REGION 'TTestFromColor'}

procedure TTestFromColor.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromColor.TestColorToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, 'clRed');
end;

procedure TTestFromColor.TestColorToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('clRed'));
end;

procedure TTestFromColor.TestColorToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEqualsString(outWStr, 'clRed');
end;

procedure TTestFromColor.TestColorToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, ColorToRGB(clRed));
end;

procedure TTestFromColor.TestColorToNullableColor;
var
  outValue: TValue;
  outNullable: TNullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(TNullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<TColor>>(outNullable));
  CheckEquals(outNullable.Value, clRed);
end;

procedure TTestFromColor.TestColorToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, 'clRed');
end;

procedure TTestFromColor.TestColorToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('clRed'));
end;

procedure TTestFromColor.TestColorToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEqualsString(outNullable.Value, 'clRed');
end;

procedure TTestFromColor.TestColorToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, ColorToRGB(clRed));
end;

{$ENDREGION}


{$REGION 'TTestFromCurrency'}

procedure TTestFromCurrency.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromCurrency.TestCurrencyToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, FloatToStr(1.11));
end;

procedure TTestFromCurrency.TestCurrencyToStringF;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.112),
    TypeInfo(string), TValue.From<string>('#.00 $'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, FormatCurr('#.00 $', 1.11));
end;

procedure TTestFromCurrency.TestCurrencyToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString(FloatToStr(1.11)));
end;

procedure TTestFromCurrency.TestCurrencyToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEqualsString(outWStr, FloatToStr(1.11));
end;

procedure TTestFromCurrency.TestCurrencyToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, CurrToStr(1.11));
end;

procedure TTestFromCurrency.TestCurrencyToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(CurrToStr(1.11)));
end;

procedure TTestFromCurrency.TestCurrencyToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEqualsString(outNullable.Value, CurrToStr(1.11));
end;

{$ENDREGION}


{$REGION 'TTestFromDateTime'}

procedure TTestFromDateTime.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromDateTime.TestDateTimeToString;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, DateTimeToStr(stamp));
end;

procedure TTestFromDateTime.TestDateTimeToStringF;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(string), TValue.From<string>('dd-mm-yyyy'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, FormatDateTime('dd-mm-yyyy', stamp));
end;

procedure TTestFromDateTime.TestDateTimeToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString(DateTimeToStr(stamp)));
end;

procedure TTestFromDateTime.TestDateTimeToWideString;
var
  outValue: TValue;
  outWStr: WideString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEqualsString(outWStr, DateTimeToStr(stamp));
end;

procedure TTestFromDateTime.TestDateTimeToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, DateTimeToStr(stamp));
end;

procedure TTestFromDateTime.TestDateTimeToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(DateTimeToStr(stamp)));
end;

procedure TTestFromDateTime.TestDateTimeToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEqualsString(outNullable.Value, DateTimeToStr(stamp));
end;

{$ENDREGION}


{$REGION 'TTestFromObject'}

  {$REGION 'TTestFromObject.TTestObject'}

  procedure TTestFromObject.TTestObject.Test;
  begin

  end;

  function TTestFromObject.TTestObject.ToString: string;
  begin
    inherited;
    Result := 'ObjectToString test object';
  end;

  {$ENDREGION}

procedure TTestFromObject.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromObject.TestObjectToString;
var
  outValue: TValue;
  obj: TObject;
  outStr: string;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(outStr, obj.ToString);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToAnsiString;
var
  outValue: TValue;
  obj: TObject;
  outAStr: AnsiString;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString(obj.ToString));
  obj.Free;
end;

procedure TTestFromObject.TestObjectToWideString;
var
  outValue: TValue;
  obj: TObject;
  outWStr: WideString;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, obj.ToString);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToClass;
var
  outValue: TValue;
  obj: TObject;
  outClass: TClass;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(TClass));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TClass>(outClass));
  CheckTrue(outClass = obj.ClassType);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToInterface;
var
  outValue: TValue;
  obj: TTestObject;
  outIntf, objIntf: ITestInterface;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(ITestInterface));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ITestInterface>(outIntf));
  obj.QueryInterface(ITestInterface, objIntf);
  CheckSame(outIntf, objIntf);
end;

procedure TTestFromObject.TestObjectToNullableString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: TNullable<string>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, obj.ToString);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToNullableWideString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: TNullable<WideString>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, obj.ToString);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToNullableAnsiString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: TNullable<AnsiString>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(obj.ToString));
  obj.Free;
end;

{$ENDREGION}


{$REGION 'TTestFromNullable'}

procedure TTestFromNullable.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromNullable.TestNullableIntegerToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.00, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromNullable.TestNullableIntegerToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromNullable.TestNullableIntegerToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromNullable.TestNullableFloatToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Extended>>(TNullable<Extended>.Create(1.11)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, FloatToStr(1.11));
end;

procedure TTestFromNullable.TestNullableFloatToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Extended>>(TNullable<Extended>.Create(1.11)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableFloatToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Extended>>(TNullable<Extended>.Create(1.11)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString(FloatToStr(1.11)));
end;

procedure TTestFromNullable.TestNullableFloatToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Extended>>(TNullable<Extended>.Create(1.11)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, FloatToStr(1.11));
end;

procedure TTestFromNullable.TestNullableStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<string>>(TNullable<string>.Create(FloatToStr(1.11))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromNullable.TestNullableStringToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<string>>(TNullable<string>.Create('Test')),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, 'Test');
end;

procedure TTestFromNullable.TestNullableAnsiStringToString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<string>>(TNullable<string>.Create('Test')),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, 'Test');
end;

procedure TTestFromNullable.TestNullableWideStringToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<string>>(TNullable<string>.Create('Test')),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, 'Test');
end;

procedure TTestFromNullable.TestNullableAnsiStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<AnsiString>>(TNullable<AnsiString>.Create(AnsiString(FloatToStr(1.11)))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromNullable.TestNullableWideStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<WideString>>(TNullable<WideString>.Create(FloatToStr(1.11))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromNullable.TestNullableDateTimeToString;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TNullable<TDateTime>>(TNullable<TDateTime>.Create(stamp)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, DateTimeToStr(stamp));
end;

procedure TTestFromNullable.TestNullableDateTimeToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TNullable<TDateTime>>(TNullable<TDateTime>.Create(stamp)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString(DateTimeToStr(stamp)));
end;

procedure TTestFromNullable.TestNullableDateTimeToWideString;
var
  outValue: TValue;
  outWStr: WideString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TNullable<TDateTime>>(TNullable<TDateTime>.Create(stamp)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, DateTimeToStr(stamp));
end;

procedure TTestFromNullable.TestNullableColorToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<TColor>>(TNullable<TColor>.Create(clRed)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, 'clRed');
end;

procedure TTestFromNullable.TestNullableColorToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<TColor>>(TNullable<TColor>.Create(clRed)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('clRed'));
end;

procedure TTestFromNullable.TestNullableColorToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<TColor>>(TNullable<TColor>.Create(clRed)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, 'clRed');
end;

{$ENDREGION}


{$REGION 'TTestFromWideString'}

procedure TTestFromWideString.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromWideString.TestWStringToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test WideString'),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(outAStr), 'Test WideString');
end;

procedure TTestFromWideString.TestWStringToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('True'),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outBool, True);
end;

procedure TTestFromWideString.TestWStringToColor;
var
  outValue: TValue;
  outColor: TColor;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('clBlue'),
    TypeInfo(TColor));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TColor>(outColor));
  CheckEquals(outColor, clBlue);
end;

procedure TTestFromWideString.TestWStringToCurrency;
var
  outValue: TValue;
  outCurrency: Currency;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Currency));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Currency>(outCurrency));
  CheckEquals(outCurrency, 1.11);
end;

procedure TTestFromWideString.TestWStringToDateTime;
var
  outValue: TValue;
  outStamp: TDateTime;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<WideString>(DateTimeToStr(stamp)),
    TypeInfo(TDateTime));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString(DateTimeToStr(outStamp), DateTimeToStr(stamp));
end;

procedure TTestFromWideString.TestWStringToDateTimeF;
var
  outValue: TValue;
  outStamp: TDateTime;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('10.10.2010'),
    TypeInfo(TDateTime), TValue.From<string>('dd.mm.yyyy'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString(FormatDateTime('dd.mm.yyyy', outStamp), '10.10.2010');
end;

procedure TTestFromWideString.TestWStringToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('teLast'),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teLast);
end;

procedure TTestFromWideString.TestWStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromWideString.TestWStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outValue.AsInteger, 1);
end;

procedure TTestFromWideString.TestWStringToNullableAnsiString;
var
  outValue: TValue;
  outNullable: TNullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(TNullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<AnsiString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, AnsiString('Test'));
end;

procedure TTestFromWideString.TestWStringToNullableBoolean;
var
  outValue: TValue;
  outNullable: TNullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('True'),
    TypeInfo(TNullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Boolean>>(outNullable));
  CheckEquals(outNullable.Value, True);
end;

procedure TTestFromWideString.TestWStringToNullableColor;
var
  outValue: TValue;
  outNullable: TNullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(ColorToString(clRed)),
    TypeInfo(TNullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<TColor>>(outNullable));
  CheckEquals(ColorToString(outNullable.Value), 'clRed');
end;

procedure TTestFromWideString.TestWStringToNullableCurrency;
var
  outValue: TValue;
  outNullable: TNullable<Currency>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(TNullable<Currency>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Currency>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromWideString.TestWStringToNullableDateTime;
var
  outValue: TValue;
  outNullable: TNullable<TDateTime>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<WideString>(DateTimeToStr(stamp)),
    TypeInfo(TNullable<TDateTime>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<TDateTime>>(outNullable));
  CheckEqualsString(DateTimeToStr(outNullable.Value), DateTimeToStr(stamp));
end;

procedure TTestFromWideString.TestWStringToNullableFloat;
var
  outValue: TValue;
  outNullable: TNullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(TNullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromWideString.TestWStringToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(TNullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromWideString.TestWStringToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(TNullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromWideString.TestWStringToNullableWideString;
var
  outValue: TValue;
  outNullable: TNullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(TNullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<WideString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromWideString.TestWStringToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test WideString'),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, 'Test WideString');
end;

{$ENDREGION}

end.
