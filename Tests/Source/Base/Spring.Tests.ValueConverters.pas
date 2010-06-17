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
  TestFramework,
  TestExtensions,
  Rtti,
  Spring.ValueConverters;

type
  TTestValueConverters = class(TTestCase)
  private
    fConverter: IValueConverter;
    type
      TEnumeration = (teFirst, teSecond, teLast);
  protected
    procedure SetUp; override;
  published
    procedure TestIntegerToString;
    procedure TestStringToInteger;
    procedure TestEnumToString;
    procedure TestEnumToInteger;
    procedure TestIntegerToEnum;
    procedure TestStringToEnum;
    procedure TestIntegerToBoolean;
    procedure TestBooleanToInteger;
    procedure TestStringToBoolean;
    procedure TestBooleanToString;
    procedure TestNullableIntegerToInteger;
    procedure TestNullableIntegerToString;
    procedure TestStringToNullableString;
    procedure TestStringToNullableInteger;
  end;

implementation

uses
  Spring;

{$REGION 'TTestValueConverters'}

procedure TTestValueConverters.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestValueConverters.TestBooleanToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Integer), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outValue.AsInteger, 0);
end;

procedure TTestValueConverters.TestBooleanToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(string), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outValue.AsString, 'False');
end;

procedure TTestValueConverters.TestIntegerToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Boolean), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outValue.AsBoolean, True);
end;

procedure TTestValueConverters.TestStringToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('True'),
    TypeInfo(Boolean), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(outValue.AsBoolean, True);
end;

procedure TTestValueConverters.TestEnumToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(Integer), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outValue.AsInteger, 1);
end;

procedure TTestValueConverters.TestEnumToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(string), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(outStr, 'teFirst');
end;

procedure TTestValueConverters.TestIntegerToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TEnumeration), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestValueConverters.TestStringToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('teLast'),
    TypeInfo(TEnumeration), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teLast);
end;

procedure TTestValueConverters.TestIntegerToString;
var
  outValue: TValue;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(string), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckEquals(outValue.AsString, '1');
end;

procedure TTestValueConverters.TestStringToInteger;
var
  outValue: TValue;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Integer), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckEquals(outValue.AsInteger, 1);
end;

procedure TTestValueConverters.TestNullableIntegerToInteger;
var
  outValue: TValue;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(Integer), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckEquals(outValue.AsInteger, 1);
end;

procedure TTestValueConverters.TestNullableIntegerToString;
var
  outValue: TValue;
begin
  outValue := fConverter.ConvertTo(TValue.From<TNullable<Integer>>(TNullable<Integer>.Create(1)),
    TypeInfo(string), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckEquals(outValue.AsString, '1');
end;

procedure TTestValueConverters.TestStringToNullableInteger;
var
  outValue: TValue;
  outNullable: TNullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('0'),
    TypeInfo(TNullable<Integer>), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<Integer>>(outNullable));
  CheckEquals(outNullable.Value, 0);
end;

procedure TTestValueConverters.TestStringToNullableString;
var
  outValue: TValue;
  outNullable: TNullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('0'),
    TypeInfo(TNullable<string>), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TNullable<string>>(outNullable));
  CheckEquals(outNullable.Value, '0');
end;

{$ENDREGION}

end.
