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
    procedure TestStringToWideString;
    procedure TestStringToNullableString;
    procedure TestStringToNullableInteger;
    procedure TestStringToNullableFloat;
    procedure TestStringToNullableBoolean;
    procedure TestStringToNullableColor;
    procedure TestStringToNullableCurrency;
    procedure TestStringToNullableDateTime;
  end;

  TTestFromInteger = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestIntegerToString;
    procedure TestIntegerToEnum;
    procedure TestIntegerToBoolean;
    procedure TestIntegerToFloat;
    procedure TestIntegerToNullableInteger;
    procedure TestIntegerToNullableString;
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
    procedure TestBooleanToNullableBoolean;
    procedure TestBooleanToNullableInteger;
    procedure TestBooleanToNullableString;
  end;

  TTestFromEnum = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestEnumToInteger;
    procedure TestEnumToString;
    procedure TestEnumToNullableString;
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
    procedure TestFloatToNullableFloat;
    procedure TestFloatToNullableInteger;
    procedure TestFloatToNullableString;
  end;

  TTestFromColor = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestColorToString;
    procedure TestColorToInteger;
    procedure TestColorToNullableColor;
    procedure TestColorToNullableString;
    procedure TestColorToNullableInteger;
  end;

  TTestFromCurrency = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestCurrencyToString;
    procedure TestCurrencyToNullableString;
  end;

  TTestFromDateTime = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestDateTimeToString;
    procedure TestDateTimeToNullableString;
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
    procedure TestObjectToInterface;
    procedure TestObjectToNullableString;
  end;

  TTestFromNullable = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestNullableIntegerToInteger;
    procedure TestNullableIntegerToString;
    procedure TestNullableFloatToString;
    procedure TestNullableStringToFloat;
    procedure TestNullableDateTimeToString;
    procedure TestNullableColorToString;
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

procedure TTestFromString.TestStringToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<UnicodeString>('Test'),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(outWStr, 'Test');
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
  CheckEquals(outValue.AsString, '1');
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

{$ENDREGION}

end.
