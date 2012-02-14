{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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

unit Spring.Tests.Reflection.ValueConverters;

interface

uses
  SysUtils,
  TestFramework,
  TestExtensions,
  Rtti,
  Spring.Reflection.ValueConverters;

type
  TEnumeration = (teFirst, teSecond, teLast);

  TTestFromString = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestStringToInteger;
    procedure TestStringToSmallInt;
    procedure TestStringToShortInt;
    procedure TestStringToLongInt;
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
    procedure TestStringToNullableSmallInt;
    procedure TestStringToNullableShortInt;
    procedure TestStringToNullableLongInt;
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
    procedure TestWStringToSmallInt;
    procedure TestWStringToShortInt;
    procedure TestWStringToLongInt;
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
    procedure TestWStringToNullableSmallInt;
    procedure TestWStringToNullableShortInt;
    procedure TestWStringToNullableLongInt;
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
    procedure TestIntegerToWideString;
    procedure TestIntegerToAnsiString;
    procedure TestIntegerToEnum;
    procedure TestIntegerToBoolean;
    procedure TestIntegerToFloat;
    procedure TestIntegerToNullableInteger;
    procedure TestIntegerToNullableSmallInt;
    procedure TestIntegerToNullableShortInt;
    procedure TestIntegerToNullableLongInt;
    procedure TestIntegerToNullableString;
    procedure TestIntegerToNullableAnsiString;
    procedure TestIntegerToNullableWideString;
    procedure TestIntegerToNullableBoolean;
    procedure TestIntegerToNullableFloat;
  end;

  TTestFromSmallInt = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestSmallIntToString;
    procedure TestSmallIntToWideString;
    procedure TestSmallIntToAnsiString;
    procedure TestSmallIntToEnum;
    procedure TestSmallIntToBoolean;
    procedure TestSmallIntToFloat;
    procedure TestSmallIntToNullableInteger;
    procedure TestSmallIntToNullableSmallInt;
    procedure TestSmallIntToNullableShortInt;
    procedure TestSmallIntToNullableLongInt;
    procedure TestSmallIntToNullableString;
    procedure TestSmallIntToNullableAnsiString;
    procedure TestSmallIntToNullableWideString;
    procedure TestSmallIntToNullableBoolean;
    procedure TestSmallIntToNullableFloat;
  end;

  TTestFromShortInt = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestShortIntToString;
    procedure TestShortIntToWideString;
    procedure TestShortIntToAnsiString;
    procedure TestShortIntToEnum;
    procedure TestShortIntToBoolean;
    procedure TestShortIntToFloat;
    procedure TestShortIntToNullableInteger;
    procedure TestShortIntToNullableSmallInt;
    procedure TestShortIntToNullableShortInt;
    procedure TestShortIntToNullableLongInt;
    procedure TestShortIntToNullableString;
    procedure TestShortIntToNullableAnsiString;
    procedure TestShortIntToNullableWideString;
    procedure TestShortIntToNullableBoolean;
    procedure TestShortIntToNullableFloat;
  end;

  TTestFromLongInt = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestLongIntToString;
    procedure TestLongIntToWideString;
    procedure TestLongIntToAnsiString;
    procedure TestLongIntToEnum;
    procedure TestLongIntToBoolean;
    procedure TestLongIntToFloat;
    procedure TestLongIntToNullableInteger;
    procedure TestLongIntToNullableSmallInt;
    procedure TestLongIntToNullableShortInt;
    procedure TestLongIntToNullableLongInt;
    procedure TestLongIntToNullableString;
    procedure TestLongIntToNullableAnsiString;
    procedure TestLongIntToNullableWideString;
    procedure TestLongIntToNullableBoolean;
    procedure TestLongIntToNullableFloat;
  end;

  TTestFromBoolean = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestBooleanToInteger;
    procedure TestBooleanToSmallInt;
    procedure TestBooleanToShortInt;
    procedure TestBooleanToLongInt;
    procedure TestBooleanToString;
    procedure TestBooleanToWideString;
    procedure TestBooleanToAnsiString;
    procedure TestBooleanToNullableBoolean;
    procedure TestBooleanToNullableInteger;
    procedure TestBooleanToNullableSmallInt;
    procedure TestBooleanToNullableShortInt;
    procedure TestBooleanToNullableLongInt;
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
    procedure TestEnumToSmallInt;
    procedure TestEnumToShortInt;
    procedure TestEnumToLongInt;
    procedure TestEnumToString;
    procedure TestEnumToAnsiString;
    procedure TestEnumToWideString;
    procedure TestEnumToNullableInteger;
    procedure TestEnumToNullableSmallInt;
    procedure TestEnumToNullableShortInt;
    procedure TestEnumToNullableLongInt;
    procedure TestEnumToNullableString;
    procedure TestEnumToNullableAnsiString;
    procedure TestEnumToNullableWideString;
  end;

  TTestFromFloat = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestFloatToInteger;
    procedure TestFloatToSmallInt;
    procedure TestFloatToShortInt;
    procedure TestFloatToLongInt;
    procedure TestFloatToString;
    procedure TestFloatToAnsiString;
    procedure TestFloatToWideString;
    procedure TestFloatToStringF;
    procedure TestFloatToNullableInteger;
    procedure TestFloatToNullableSmallInt;
    procedure TestFloatToNullableShortInt;
    procedure TestFloatToNullableLongInt;
    procedure TestFloatToNullableFloat;
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
    procedure TestColorToInteger;
    procedure TestColorToSmallInt;
    procedure TestColorToLongInt;
    procedure TestColorToString;
    procedure TestColorToAnsiString;
    procedure TestColorToWideString;
    procedure TestColorToNullableColor;
    procedure TestColorToNullableInteger;
    procedure TestColorToNullableSmallInt;
    procedure TestColorToNullableLongInt;
    procedure TestColorToNullableString;
    procedure TestColorToNullableAnsiString;
    procedure TestColorToNullableWideString;
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

  TTestFromInterface = class(TTestCase)
  private
    fConverter: IValueConverter;

    type
      ITestInterface = interface
      ['{F98EAF66-33B6-4687-8052-4B76471D978B}']
        function ToString: string;
      end;

      TTestObject = class(TInterfacedObject, ITestInterface)
      public
        function ToString: string; override;
      end;
  protected
    procedure SetUp; override;
  published
    procedure TestInterfaceToObject;
  end;

  TTestFromNullable = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestNullableIntegerToInteger;
    procedure TestNullableIntegerToSmallInt;
    procedure TestNullableIntegerToShortInt;
    procedure TestNullableIntegerToLongInt;
    procedure TestNullableIntegerToFloat;
    procedure TestNullableIntegerToString;
    procedure TestNullableIntegerToWideString;
    procedure TestNullableIntegerToAnsiString;
    procedure TestNullableSmallIntToInteger;
    procedure TestNullableSmallIntToSmallInt;
    procedure TestNullableSmallIntToShortInt;
    procedure TestNullableSmallIntToLongInt;
    procedure TestNullableSmallIntToFloat;
    procedure TestNullableSmallIntToString;
    procedure TestNullableSmallIntToWideString;
    procedure TestNullableSmallIntToAnsiString;
    procedure TestNullableShortIntToInteger;
    procedure TestNullableShortIntToSmallInt;
    procedure TestNullableShortIntToShortInt;
    procedure TestNullableShortIntToLongInt;
    procedure TestNullableShortIntToFloat;
    procedure TestNullableShortIntToString;
    procedure TestNullableShortIntToWideString;
    procedure TestNullableShortIntToAnsiString;
    procedure TestNullableLongIntToInteger;
    procedure TestNullableLongIntToSmallInt;
    procedure TestNullableLongIntToLongInt;
    procedure TestNullableLongIntToShortInt;
    procedure TestNullableLongIntToFloat;
    procedure TestNullableLongIntToString;
    procedure TestNullableLongIntToWideString;
    procedure TestNullableLongIntToAnsiString;
    procedure TestNullableFloatToInteger;
    procedure TestNullableFloatToSmallInt;
    procedure TestNullableFloatToShortInt;
    procedure TestNullableFloatToLongInt;
    procedure TestNullableFloatToString;
    procedure TestNullableFloatToAnsiString;
    procedure TestNullableFloatToWideString;
    procedure TestNullableStringToString;
    procedure TestNullableAnsiStringToString;
    procedure TestNullableWideStringToWideString;
    procedure TestNullableStringToInteger;
    procedure TestNullableStringToSmallInt;
    procedure TestNullableStringToShortInt;
    procedure TestNullableStringToLongInt;
    procedure TestNullableAnsiStringToInteger;
    procedure TestNullableAnsiStringToSmallInt;
    procedure TestNullableAnsiStringToShortInt;
    procedure TestNullableAnsiStringToLongInt;
    procedure TestNullableWideStringToInteger;
    procedure TestNullableWideStringToSmallInt;
    procedure TestNullableWideStringToShortInt;
    procedure TestNullableWideStringToLongInt;
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
  CheckEquals(outInt, 1);
end;

procedure TTestFromString.TestStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromString.TestStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromString.TestStringToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
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
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromString.TestStringToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, AnsiString('Test'));
end;

procedure TTestFromString.TestStringToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromString.TestStringToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromString.TestStringToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromString.TestStringToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromString.TestStringToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromString.TestStringToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromString.TestStringToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('True'),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckEquals(outNullable.Value, True);
end;

procedure TTestFromString.TestStringToNullableColor;
var
  outValue: TValue;
  outNullable: Nullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(ColorToString(clRed)),
    TypeInfo(Nullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TColor>>(outNullable));
  CheckEquals(ColorToString(outNullable.Value), 'clRed');
end;

procedure TTestFromString.TestStringToNullableCurrency;
var
  outValue: TValue;
  outNullable: Nullable<Currency>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Nullable<Currency>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Currency>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromString.TestStringToNullableDateTime;
var
  outValue: TValue;
  outNullable: Nullable<TDateTime>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<string>(DateTimeToStr(stamp)),
    TypeInfo(Nullable<TDateTime>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TDateTime>>(outNullable));
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
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromInteger.TestIntegerToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromInteger.TestIntegerToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromInteger.TestIntegerToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromInteger.TestIntegerToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, '1');
end;

procedure TTestFromInteger.TestIntegerToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('1'));
end;

procedure TTestFromInteger.TestIntegerToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, '1');
end;

procedure TTestFromInteger.TestIntegerToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 0.0);
end;

{$ENDREGION}


{$REGION 'TTestFromSmallInt'}

procedure TTestFromSmallInt.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromSmallInt.TestSmallIntToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outBool, True);
end;

procedure TTestFromSmallInt.TestSmallIntToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromSmallInt.TestSmallIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outValue.AsExtended, 1.0);
end;

procedure TTestFromSmallInt.TestSmallIntToString;
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

procedure TTestFromSmallInt.TestSmallIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromSmallInt.TestSmallIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromSmallInt.TestSmallIntToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, '1');
end;

procedure TTestFromSmallInt.TestSmallIntToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('1'));
end;

procedure TTestFromSmallInt.TestSmallIntToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, '1');
end;

procedure TTestFromSmallInt.TestSmallIntToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 0.0);
end;

{$ENDREGION}


{$REGION 'TTestFromShortInt'}

procedure TTestFromShortInt.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromShortInt.TestShortIntToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outBool, True);
end;

procedure TTestFromShortInt.TestShortIntToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromShortInt.TestShortIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outValue.AsExtended, 1.0);
end;

procedure TTestFromShortInt.TestShortIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromShortInt.TestShortIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromShortInt.TestShortIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromShortInt.TestShortIntToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromShortInt.TestShortIntToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromShortInt.TestShortIntToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromShortInt.TestShortIntToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromShortInt.TestShortIntToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, '1');
end;

procedure TTestFromShortInt.TestShortIntToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('1'));
end;

procedure TTestFromShortInt.TestShortIntToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, '1');
end;

procedure TTestFromShortInt.TestShortIntToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 0.0);
end;

{$ENDREGION}


{$REGION 'TTestFromLongInt'}

procedure TTestFromLongInt.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromLongInt.TestLongIntToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outBool, True);
end;

procedure TTestFromLongInt.TestLongIntToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromLongInt.TestLongIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outValue.AsExtended, 1.0);
end;

procedure TTestFromLongInt.TestLongIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromLongInt.TestLongIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromLongInt.TestLongIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromLongInt.TestLongIntToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromLongInt.TestLongIntToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromLongInt.TestLongIntToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromLongInt.TestLongIntToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromLongInt.TestLongIntToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, '1');
end;

procedure TTestFromLongInt.TestLongIntToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('1'));
end;

procedure TTestFromLongInt.TestLongIntToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, '1');
end;

procedure TTestFromLongInt.TestLongIntToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromLongInt.TestLongIntToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<LongInt>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
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

procedure TTestFromBoolean.TestBooleanToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 0);
end;

procedure TTestFromBoolean.TestBooleanToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 0);
end;

procedure TTestFromBoolean.TestBooleanToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
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
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 0);
end;

procedure TTestFromBoolean.TestBooleanToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 0);
end;

procedure TTestFromBoolean.TestBooleanToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 0);
end;

procedure TTestFromBoolean.TestBooleanToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckEquals(outNullable.Value, 0);
end;

procedure TTestFromBoolean.TestBooleanToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, 'False');
end;

procedure TTestFromBoolean.TestBooleanToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('False'));
end;

procedure TTestFromBoolean.TestBooleanToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
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

procedure TTestFromEnum.TestEnumToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromEnum.TestEnumToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromEnum.TestEnumToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromEnum.TestEnumToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, 'teFirst');
end;

procedure TTestFromEnum.TestEnumToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('teFirst'));
end;

procedure TTestFromEnum.TestEnumToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, 'teFirst');
end;

procedure TTestFromEnum.TestEnumToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 2);
end;

procedure TTestFromEnum.TestEnumToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 2);
end;

procedure TTestFromEnum.TestEnumToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 2);
end;

procedure TTestFromEnum.TestEnumToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
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

procedure TTestFromFloat.TestFloatToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromFloat.TestFloatToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromFloat.TestFloatToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
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
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromFloat.TestFloatToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, FloatToStr(1.11));
end;

procedure TTestFromFloat.TestFloatToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(FloatToStr(1.11)));
end;

procedure TTestFromFloat.TestFloatToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, FloatToStr(1.11));
end;

procedure TTestFromFloat.TestFloatToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromFloat.TestFloatToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromFloat.TestFloatToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(outNullable.Value, 1);
end;

procedure TTestFromFloat.TestFloatToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
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

procedure TTestFromColor.TestColorToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, ColorToRGB(clRed));
end;

procedure TTestFromColor.TestColorToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, ColorToRGB(clRed));
end;

procedure TTestFromColor.TestColorToNullableColor;
var
  outValue: TValue;
  outNullable: Nullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TColor>>(outNullable));
  CheckEquals(outNullable.Value, clRed);
end;

procedure TTestFromColor.TestColorToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, 'clRed');
end;

procedure TTestFromColor.TestColorToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString('clRed'));
end;

procedure TTestFromColor.TestColorToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEqualsString(outNullable.Value, 'clRed');
end;

procedure TTestFromColor.TestColorToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, ColorToRGB(clRed));
end;

procedure TTestFromColor.TestColorToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(outNullable.Value, ColorToRGB(clRed));
end;

procedure TTestFromColor.TestColorToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(clRed),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
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
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, CurrToStr(1.11));
end;

procedure TTestFromCurrency.TestCurrencyToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(CurrToStr(1.11)));
end;

procedure TTestFromCurrency.TestCurrencyToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
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
  outNullable: Nullable<string>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, DateTimeToStr(stamp));
end;

procedure TTestFromDateTime.TestDateTimeToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(DateTimeToStr(stamp)));
end;

procedure TTestFromDateTime.TestDateTimeToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
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
    Result := 'ObjectToString test case.';
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
  outNullable: Nullable<string>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(outNullable.Value, obj.ToString);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToNullableWideString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: Nullable<WideString>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(outNullable.Value, obj.ToString);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToNullableAnsiString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: Nullable<AnsiString>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableFloatToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableFloatToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableFloatToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableFloatToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, FloatToStr(1.11));
end;

procedure TTestFromNullable.TestNullableSmallIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromNullable.TestNullableSmallIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.00, 1);
end;

procedure TTestFromNullable.TestNullableSmallIntToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableSmallIntToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableSmallIntToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableSmallIntToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableSmallIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromNullable.TestNullableSmallIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromNullable.TestNullableShortIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromNullable.TestNullableShortIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.00, 1);
end;

procedure TTestFromNullable.TestNullableShortIntToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableShortIntToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableShortIntToSmallInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableShortIntToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableShortIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromNullable.TestNullableShortIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromNullable.TestNullableLongIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('1'));
end;

procedure TTestFromNullable.TestNullableLongIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.00, 1);
end;

procedure TTestFromNullable.TestNullableLongIntToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableLongIntToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableLongIntToSmallInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableLongIntToShortInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableLongIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, '1');
end;

procedure TTestFromNullable.TestNullableLongIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<LongInt>>(Nullable<LongInt>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, '1');
end;

procedure TTestFromNullable.TestNullableStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create(FloatToStr(1.11))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromNullable.TestNullableStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableStringToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableStringToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('Test')),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, 'Test');
end;

procedure TTestFromNullable.TestNullableAnsiStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableAnsiStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableAnsiStringToString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('Test')),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(outAStr, AnsiString('Test'));
end;

procedure TTestFromNullable.TestNullableWideStringToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('Test')),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create(AnsiString(FloatToStr(1.11)))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromNullable.TestNullableAnsiStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableAnsiStringToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableWideStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create(FloatToStr(1.11))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(outFloat, 1.11);
end;

procedure TTestFromNullable.TestNullableWideStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableWideStringToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableWideStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableWideStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 2);
end;

procedure TTestFromNullable.TestNullableDateTimeToString;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(stamp)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(stamp)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(stamp)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TColor>>(Nullable<TColor>.Create(clRed)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TColor>>(Nullable<TColor>.Create(clRed)),
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
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TColor>>(Nullable<TColor>.Create(clRed)),
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
  CheckEquals(outAStr, AnsiString('Test WideString'));
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
  CheckEquals(outInt, 1);
end;

procedure TTestFromWideString.TestWStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromWideString.TestWStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromWideString.TestWStringToLongInt;
var
  outValue: TValue;
  outInt: LongInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(LongInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<LongInt>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromWideString.TestWStringToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, AnsiString('Test'));
end;

procedure TTestFromWideString.TestWStringToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('True'),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckEquals(outNullable.Value, True);
end;

procedure TTestFromWideString.TestWStringToNullableColor;
var
  outValue: TValue;
  outNullable: Nullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(ColorToString(clRed)),
    TypeInfo(Nullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TColor>>(outNullable));
  CheckEquals(ColorToString(outNullable.Value), 'clRed');
end;

procedure TTestFromWideString.TestWStringToNullableCurrency;
var
  outValue: TValue;
  outNullable: Nullable<Currency>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Nullable<Currency>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Currency>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromWideString.TestWStringToNullableDateTime;
var
  outValue: TValue;
  outNullable: Nullable<TDateTime>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<WideString>(DateTimeToStr(stamp)),
    TypeInfo(Nullable<TDateTime>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TDateTime>>(outNullable));
  CheckEqualsString(DateTimeToStr(outNullable.Value), DateTimeToStr(stamp));
end;

procedure TTestFromWideString.TestWStringToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(outNullable.Value, 1.11);
end;

procedure TTestFromWideString.TestWStringToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromWideString.TestWStringToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromWideString.TestWStringToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromWideString.TestWStringToNullableLongInt;
var
  outValue: TValue;
  outNullable: Nullable<LongInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<LongInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<LongInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 15);
end;

procedure TTestFromWideString.TestWStringToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(outNullable.Value, 'Test');
end;

procedure TTestFromWideString.TestWStringToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
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


{$REGION 'TTestFromInterface'}

  {$REGION 'TTestFromObject.TTestObject'}

  function TTestFromInterface.TTestObject.ToString: string;
  begin
    inherited;
    Result := 'InterfaceToObject test case.';
  end;

  {$ENDREGION}

procedure TTestFromInterface.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromInterface.TestInterfaceToObject;
var
  outValue: TValue;
  intf: ITestInterface;
  obj: TTestObject;
begin
  intf := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<ITestInterface>(intf),
    TypeInfo(TTestObject));

  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TTestObject>(obj));
  CheckEqualsString(intf.ToString, obj.ToString);
end;

{$ENDREGION}

end.
