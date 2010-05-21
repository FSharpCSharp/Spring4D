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

unit Spring.Tests.System;

{$I Spring.inc}

interface

uses
  Classes,
  TypInfo,
  DateUtils,
  SysUtils,
  Graphics,
  Variants,
  Types,
  TestFramework,
  TestExtensions,
  Generics.Defaults,
  Spring;

type
  TTestSplitString = class(TTestCase)
  private
    fStrings: TStringDynArray;
  published
    procedure TestEmptyString;
    procedure TestOneEntry;
    procedure TestEmptyEntry;
    procedure TestMoreEntries;
    procedure TestRemoveEmptyEntries;
  end;

  TTestTryConvertStrToDateTime = class(TTestCase)
  published
    procedure TestParseDate;
    procedure TestParseTime;
    procedure TestParseDateTime;
    procedure TestFailedCases;
  end;

  TTestSplitNullTerminatedStrings = class(TTestCase)
  private
    fStrings: TStringDynArray;
    fBuffer: TCharArray;
  published
    procedure TestNil;
    procedure TestEmpty;
    procedure TestOneEntry;
    procedure TestThreeEntries;
    procedure TestVariousStrings;
  end;

  TTestVersion = class(TTestCase)
  published
    procedure TestCompareTo;
    procedure TestFromString;
    procedure TestToString;
    procedure TestToStringException;
    procedure TestArgumentException;
    procedure TestArgumentOutOfRangeException;
    procedure TestFormatException;
  end;

  TBufferTestCase = class abstract(TTestCase)
  protected
    fBuffer: TBuffer;
    fBytes: TBytes;
  end;

  TTestBuffer = class(TBufferTestCase)
  private
    fInteger: Integer;
  published
    procedure TestGetByte;
    procedure TestSetByte;
    procedure TestBytesOf;
  end;

  TTestEmptyBuffer = class(TBufferTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestSizeIsZero;
    procedure TestIsEmpty;
    procedure TestToBytes;
    procedure TestToHexString;
    procedure TestEquals;
    procedure TestMemoryIsNil;
  end;

  TTestFiveByteBuffer = class(TBufferTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestSizeIsFive;
    procedure TestIsEmpty;
    procedure TestToHexString;
    procedure TestEquals;
    procedure TestToBytes;
    procedure TestToString;
    procedure TestToAnsiString;
    procedure TestBytes;
    procedure TestFromHexString;
    procedure TestImplicitConversion;
  end;

  TTestRtti = class(TTestCase)
  published

  end;

  TTestArgument = class(TTestCase)
  published

  end;

  TTestEnum = class(TTestCase)
  published
    procedure TestGetNameByEnum;
    procedure TestGetNameByInteger;
    procedure TestGetValueByEnum;
    procedure TestGetValueByName;
    procedure TestIsValid;
    procedure TestTryParse;
    procedure TestParse;
    procedure TestParseIntegerException;
    procedure TestParseStringException;
  end;

  TTestNullableInteger = class(TTestCase)
  private
    fInteger: TNullable<Integer>;
  published
    procedure TestInitialValue;
    procedure GetValueOrDefault;
    procedure TestAssignFive;
    procedure TestAssignNil;
    procedure TestException;
    procedure TestLocalVariable;
    procedure TestFromVariant;
  end;

  TTestDelegate = class(TTestCase)
  private
    fDelegate: IDelegate<TProc>;
    fCallback: TProc<TProc>;
    fHandler1: TProc;
    fHandler2: TProc;
    fHandler1Times: Integer;
    fHandler2Times: Integer;
    procedure Handler1;
    procedure Handler2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOneHandler;
    procedure TestTwoHandlers;
  end;

implementation

uses
  Spring.Utils.IO;


{$REGION 'TTestSplitString'}

procedure TTestSplitString.TestEmptyString;
begin
  fStrings := SplitString('', []);
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitString.TestOneEntry;
begin
  fStrings := SplitString('word', [' ']);
  CheckEquals(1, Length(fStrings));
  CheckEquals('word', fStrings[0]);

  fStrings := SplitString('2', [' ']);
  CheckEquals(1, Length(fStrings));
  CheckEquals('2', fStrings[0]);
end;

procedure TTestSplitString.TestMoreEntries;
begin
  fStrings := SplitString('one word', [' ']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('word', fStrings[1]);

  fStrings := SplitString('one two three four', [' ']);
  CheckEquals(4, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('two', fStrings[1]);
  CheckEquals('three', fStrings[2]);
  CheckEquals('four', fStrings[3]);

  fStrings := SplitString('2.0', ['.']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('2', fStrings[0]);
  CheckEquals('0', fStrings[1]);
end;

procedure TTestSplitString.TestEmptyEntry;
begin
  fStrings := SplitString('one  word', [' ']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('word', fStrings[2]);

  fStrings := SplitString('1..2', ['.']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('1', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('2', fStrings[2]);

  fStrings := SplitString('12..3..456', ['.']);
  CheckEquals(5, Length(fStrings));
  CheckEquals('12', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('3', fStrings[2]);
  CheckEquals('', fStrings[3]);
  CheckEquals('456', fStrings[4]);
  
  fStrings := SplitString('.', ['.']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('', fStrings[0]);
  CheckEquals('', fStrings[1]);
  
  fStrings := SplitString('.1.', ['.']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('', fStrings[0]);
  CheckEquals('1', fStrings[1]);
  CheckEquals('', fStrings[2]);
end;

procedure TTestSplitString.TestRemoveEmptyEntries;
begin
  fStrings := SplitString('1..2', ['.'], True);
  CheckEquals(2, Length(fStrings));
  CheckEquals('1', fStrings[0]);
  CheckEquals('2', fStrings[1]);
  
  fStrings := SplitString('.', ['.'], True);
  CheckEquals(0, Length(fStrings));
end;

{$ENDREGION}


{$REGION 'TTestSplitNullTerminatedStrings'}

procedure TTestSplitNullTerminatedStrings.TestEmpty;
begin
  fBuffer := TCharArray.Create(#0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitNullTerminatedStrings.TestNil;
begin
  fStrings := SplitString(nil);
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitNullTerminatedStrings.TestOneEntry;
begin
  fBuffer := TCharArray.Create('C', ':', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(1, Length(fStrings));
  CheckEquals('C:', fStrings[0]);
end;

procedure TTestSplitNullTerminatedStrings.TestThreeEntries;
begin
  fBuffer := TCharArray.Create('C', ':', #0, 'D', ':', #0, 'E', ':', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(3, Length(fStrings));
  CheckEquals('C:', fStrings[0]);
  CheckEquals('D:', fStrings[1]);
  CheckEquals('E:', fStrings[2]);
end;

procedure TTestSplitNullTerminatedStrings.TestVariousStrings;
begin
  fBuffer := TCharArray.Create('A', 'B', 'C', #0, 'D', 'E', #0, 'F', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(3, Length(fStrings));
  CheckEquals('ABC', fStrings[0]);
  CheckEquals('DE', fStrings[1]);
  CheckEquals('F', fStrings[2]);
end;

{$ENDREGION}


{$REGION 'TTestEnum'}

procedure TTestEnum.TestGetNameByEnum;
var
  expectedName: string;
  actualName: string;
  item: TDriveType;
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(TDriveType);
  for item := Low(TDriveType) to High(TDriveType) do
  begin
    expectedName := GetEnumName(pInfo, Integer(item));
    actualName := TEnum.GetName<TDriveType>(item);
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetNameByInteger;
var
  expectedName: string;
  actualName: string;
  item: TDriveType;
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(TDriveType);
  for item := Low(TDriveType) to High(TDriveType) do
  begin
    expectedName := GetEnumName(pInfo, Integer(item));
    actualName := TEnum.GetName<TDriveType>(Integer(item));
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetValueByEnum;
var
  expectedValue: Integer;
  actualValue: Integer;
  item: TDriveType;
begin
  for item := Low(TDriveType) to High(TDriveType) do
  begin
    expectedValue := Integer(item);
    actualValue := TEnum.GetValue<TDriveType>(item);
    CheckEquals(expectedValue, actualValue);
  end;
end;

procedure TTestEnum.TestGetValueByName;
var
  expectedValue: Integer;
  actualValue: Integer;
  item: TDriveType;
  name: string;
begin
  for item := Low(TDriveType) to High(TDriveType) do
  begin
    expectedValue := Integer(item);
    name := GetEnumName(TypeInfo(TDriveType), expectedValue);
    actualValue := TEnum.GetValue<TDriveType>(name);
    CheckEquals(expectedValue, actualValue);
  end;
end;

procedure TTestEnum.TestIsValid;
var
  item: TDriveType;
begin
  for item := Low(TDriveType) to High(TDriveType) do
  begin
    Check(TEnum.IsValid<TDriveType>(item));
    Check(TEnum.IsValid<TDriveType>(Integer(item)));
  end;
  CheckFalse(TEnum.IsValid<TDriveType>(Integer(Low(TDriveType)) - 1));
  CheckFalse(TEnum.IsValid<TDriveType>(Integer(High(TDriveType)) + 1));
end;

procedure TTestEnum.TestParse;
var
  item: TDriveType;
  actual: TDriveType;
begin
  for item := Low(TDriveType) to High(TDriveType) do
  begin
    actual := TEnum.Parse<TDriveType>(Integer(item));
    CheckEquals(Integer(item), Integer(actual));
    actual := TEnum.Parse<TDriveType>(GetEnumName(TypeInfo(TDriveType), Integer(item)));
    CheckEquals(Integer(item), Integer(actual));
  end;
end;

procedure TTestEnum.TestTryParse;
var
  driveType: TDriveType;
begin
  Check(TEnum.TryParse<TDriveType>(Integer(dtNetwork), driveType));
  CheckEquals(Integer(dtNetwork), Integer(driveType));
  Check(TEnum.TryParse<TDriveType>('dtNetwork', driveType));
  CheckEquals(Integer(dtNetwork), Integer(driveType));

  CheckFalse(TEnum.TryParse<TDriveType>(Integer(Low(TDriveType)) - 1, driveType));
  CheckFalse(TEnum.TryParse<TDriveType>('dummy', driveType));
end;

procedure TTestEnum.TestParseIntegerException;
begin
  ExpectedException := EFormatException;
  TEnum.Parse<TDriveType>(Integer(Low(TDriveType))-1);
end;

procedure TTestEnum.TestParseStringException;
begin
  ExpectedException := EFormatException;
  TEnum.Parse<TDriveType>('dummy');
end;

{$ENDREGION}


{$REGION 'TTestVersion'}

//  Version 1.1 is older than version 1.1.0.
//  Version 1.1 is older than version 1.1.1.
//  Version 1.1 is older than version 1.1.2.3.
//  Version 1.1.2 is older than version 1.1.2.4.
//  Version 1.2.5 is newer than version 1.2.3.4.

procedure TTestVersion.TestCompareTo;
var
  v1, v2: TVersion;
begin
  v1 := TVersion.Create('1.1');
  v2 := TVersion.Create('1.1.0');
  CheckTrue(v1.CompareTo(v2) < 0);    // v1 is older than v2
  CheckTrue(v2.CompareTo(v1) > 0);    // v2 is newer than v1

  v1 := TVersion.Create('1.1');
  v2 := TVersion.Create('1.1.2.3');
  CheckTrue(v1.CompareTo(v2) < 0);
  CheckTrue(v2.CompareTo(v1) > 0);

  v1 := TVersion.Create('1.1.2');
  v2 := TVersion.Create('1.1.2.4');
  CheckTrue(v1.CompareTo(v2) < 0);
  CheckTrue(v2.CompareTo(v1) > 0);

  v1 := TVersion.Create('1.1.5');
  v2 := TVersion.Create('1.1.3.4');
  CheckTrue(v1.CompareTo(v2) > 0);
  CheckTrue(v2.CompareTo(v1) < 0);

  v1 := TVersion.Create('1.1.5');
  v2 := TVersion.Create('1.1.5');
  CheckTrue(v1.CompareTo(v2) = 0);
  CheckTrue(v2.CompareTo(v1) = 0);
end;

procedure TTestVersion.TestFromString;
var
  ver: TVersion;
begin
  ver := TVersion.Create('1.0');
  CheckEquals(1, ver.Major);
  CheckEquals(0, ver.Minor);
  CheckEquals(-1, ver.Build);
  CheckEquals(-1, ver.Reversion);

  ver := TVersion.Create('1.1.0');
  CheckEquals(1, ver.Major);
  CheckEquals(1, ver.Minor);
  CheckEquals(0, ver.Build);
  CheckEquals(-1, ver.Reversion);

  ver := TVersion.Create('1.1.1.0');
  CheckEquals(1, ver.Major);
  CheckEquals(1, ver.Minor);
  CheckEquals(1, ver.Build);
  CheckEquals(0, ver.Reversion);
end;

procedure TTestVersion.TestToString;
var
  ver: TVersion;
begin
  ver := TVersion.Create(1, 0);
  CheckEquals('1.0', ver.ToString);

  ver := TVersion.Create(1, 0, 6);
  CheckEquals('1.0.6', ver.ToString);
  CheckEquals('1.0', ver.ToString(2));
  CheckEquals('1.0.6', ver.ToString(3));

  ver := TVersion.Create(10, 8, 2608, 8);
  CheckEquals('10.8.2608.8', ver.ToString);
  CheckEquals('10.8', ver.ToString(2));
  CheckEquals('10.8.2608', ver.ToString(3));
  CheckEquals('10.8.2608.8', ver.ToString(4));
end;

procedure TTestVersion.TestToStringException;
var
  ver: TVersion;
begin
  ExpectedException := EArgumentException;
  ver := TVersion.Create('1.0');
  ver.ToString(3);
end;

procedure TTestVersion.TestArgumentException;
begin
  ExpectedException := EArgumentException;
  TVersion.Create('1');
end;

procedure TTestVersion.TestFormatException;
begin
  ExpectedException := EFormatException;
  TVersion.Create('1.c.d');
end;

procedure TTestVersion.TestArgumentOutOfRangeException;
begin
  ExpectedException := EArgumentOutOfRangeException;
  TVersion.Create('1.-1.0');
end;

{$ENDREGION}


{$REGION 'TTestNullableInteger'}

procedure TTestNullableInteger.TestInitialValue;
begin
  CheckFalse(fInteger.HasValue);
end;

procedure TTestNullableInteger.GetValueOrDefault;
begin
  Assert(not fInteger.HasValue);
  CheckEquals(Default(Integer), fInteger.GetValueOrDefault);
  CheckEquals(18, fInteger.GetValueOrDefault(18));
end;

procedure TTestNullableInteger.TestAssignFive;
begin
  fInteger := 5;
  Check(fInteger.HasValue);
  CheckEquals(5, fInteger.Value);
  Check(fInteger.Value = 5);
  Check(fInteger.Value <> 3);
end;

procedure TTestNullableInteger.TestAssignNil;
begin
  fInteger := 5;
  Assert(fInteger.HasValue);
  fInteger := nil;
  CheckFalse(fInteger.HasValue);
end;

procedure TTestNullableInteger.TestException;
begin
  ExpectedException := EInvalidOperation;
  fInteger.Value;
end;

procedure TTestNullableInteger.TestLocalVariable;
var
  dirtyValue: TNullable<Integer>;  { lives in stack }
begin
  CheckFalse(dirtyValue.HasValue);
  dirtyValue := 5;
end;

procedure TTestNullableInteger.TestFromVariant;
var
  value: Variant;
const
  ExpectedInteger: Integer = 5;
begin
  value := Null;
  fInteger := TNullable<Integer>.Create(value);
  CheckFalse(fInteger.HasValue);

  fInteger := value;
  CheckFalse(fInteger.HasValue);

  value := ExpectedInteger;
  fInteger := TNullable<Integer>.Create(value);
  CheckTrue(fInteger.HasValue);
  CheckEquals(ExpectedInteger, fInteger.Value);
end;

{$ENDREGION}


{$REGION 'TTestEmptyBuffer'}

procedure TTestEmptyBuffer.SetUp;
begin
  inherited;
  fBuffer := TBuffer.Create([]);
end;

procedure TTestEmptyBuffer.TestEquals;
begin
  CheckTrue(fBuffer.Equals([]));
  CheckTrue(fBuffer.Equals(fBytes));
  CheckTrue(fBuffer.Equals(fBuffer));
  CheckFalse(fBuffer.Equals([$00]));
end;

procedure TTestEmptyBuffer.TestIsEmpty;
begin
  CheckTrue(fBuffer.IsEmpty);
end;

procedure TTestEmptyBuffer.TestMemoryIsNil;
begin
  CheckTrue(fBuffer.Memory = nil);
end;

procedure TTestEmptyBuffer.TestSizeIsZero;
begin
  CheckEquals(0, fBuffer.Size);
end;

procedure TTestEmptyBuffer.TestToBytes;
var
  bytes: TBytes;
begin
  bytes := fBuffer.ToBytes;
  CheckTrue(Length(bytes) = 0, 'bytes should be empty.');
end;

procedure TTestEmptyBuffer.TestToHexString;
begin
  CheckEquals('', fBuffer.ToHexString);
end;

{$ENDREGION}


{$REGION 'TTestFiveByteBuffer'}

procedure TTestFiveByteBuffer.SetUp;
begin
  inherited;
  fBytes := TBytes.Create($01, $10, $AB, $CD, $EF);
  fBuffer := TBuffer.Create(fBytes);
end;

procedure TTestFiveByteBuffer.TestSizeIsFive;
begin
  CheckEquals(5, fBuffer.Size);
end;

procedure TTestFiveByteBuffer.TestToBytes;
var
  bytes: TBytes;
begin
  bytes := fBuffer.ToBytes;
  CheckEquals(fBuffer.Size, Length(bytes));
  CheckTrue(CompareMem(PByte(fBytes), PByte(bytes), Length(bytes)));
  CheckTrue(Integer(bytes) <> Integer(fBuffer.Memory),
    'ToBytes should return a new allocated TBytes.');
end;

procedure TTestFiveByteBuffer.TestToString;
var
  bytes: TBytes;
  expected: string;
begin
  bytes := WideBytesOf('ABC中国');
  expected := WideStringOf(bytes);
  fBuffer := bytes;
  {$WARNINGS OFF}
  CheckEquals(expected, fBuffer.ToString);
  {$WARNINGS ON}
end;

procedure TTestFiveByteBuffer.TestToAnsiString;
var
  bytes: TBytes;
  expected: AnsiString;
begin
  bytes := BytesOf('ABC中国');
  expected := AnsiString(StringOf(bytes));
  fBuffer := bytes;
  {$WARNINGS OFF}
  CheckEquals(expected, fBuffer.ToAnsiString);
  {$WARNINGS ON}
end;

procedure TTestFiveByteBuffer.TestToHexString;
var
  actual: string;
begin
  actual := fBuffer.ToHexString;
  CheckEquals('0110ABCDEF', actual);

  actual := fBuffer.ToHexString('');
  CheckEquals('01 10 AB CD EF', actual);

  actual := fBuffer.ToHexString('$');
  CheckEquals('$01 $10 $AB $CD $EF', actual);

  actual := fBuffer.ToHexString('#$');
  CheckEquals('#$01 #$10 #$AB #$CD #$EF', actual);

  actual := fBuffer.ToHexString('0x');
  CheckEquals('0x01 0x10 0xAB 0xCD 0xEF', actual);
end;

procedure TTestFiveByteBuffer.TestBytes;
var
  i: Integer;
begin
  for i := 0 to Length(fBytes) - 1 do
  begin
    CheckEquals(fBytes[i], fBuffer[i], Format('fBuffer[%d]', [i]));
  end;
end;

procedure TTestFiveByteBuffer.TestEquals;
begin
  CheckTrue(fBuffer.Equals(fBytes));
  CheckTrue(not fBuffer.Equals(Copy(fBytes, 0, 1)));
end;

procedure TTestFiveByteBuffer.TestFromHexString;
var
  buffer: TBuffer;
begin
  buffer := TBuffer.FromHexString('0110ABCDEF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('01 10 AB CD EF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('$0110ABCDEF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('$01 $10 $AB $CD $EF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('#$01 #$10 #$AB #$CD #$EF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('0x01 0x10 0xAB 0xCD 0xEF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('0x0110ABCDEF');
  CheckTrue(buffer.Equals(fBuffer));

  buffer := TBuffer.FromHexString('');
  CheckTrue(buffer.IsEmpty);
end;

procedure TTestFiveByteBuffer.TestImplicitConversion;
var
  bytes: TBytes;
begin
  bytes := fBuffer;
  CheckTrue(Integer(bytes) = Integer(fBuffer.Memory));   // Check Reference

  fBuffer := fBytes;
  CheckTrue(Integer(fBuffer.Memory) = Integer(fBytes));  // Check Reference
end;

procedure TTestFiveByteBuffer.TestIsEmpty;
begin
  CheckFalse(fBuffer.IsEmpty);
end;

{$ENDREGION}


{$REGION 'TTestBuffer'}

procedure TTestBuffer.TestBytesOf;
var
  bytes: TBytes;
  item: Byte;
begin
  bytes := TBuffer.BytesOf(0, 16);
  for item in bytes do
  begin
    CheckEquals(item, 0);
  end;
  bytes := TBuffer.BytesOf($AB, 16);
  for item in bytes do
  begin
    CheckEquals(item, $AB);
  end;
end;

procedure TTestBuffer.TestGetByte;
begin
  fInteger := $12345678;
  CheckEquals($78, TBuffer.GetByte(fInteger, 0));
  CheckEquals($56, TBuffer.GetByte(fInteger, 1));
  CheckEquals($34, TBuffer.GetByte(fInteger, 2));
  CheckEquals($12, TBuffer.GetByte(fInteger, 3));
end;

procedure TTestBuffer.TestSetByte;
begin
  fInteger := 0;
  TBuffer.SetByte(fInteger, 0, $78);
  TBuffer.SetByte(fInteger, 1, $56);
  TBuffer.SetByte(fInteger, 2, $34);
  TBuffer.SetByte(fInteger, 3, $12);
  CheckEquals($12345678, fInteger);
end;

{$ENDREGION}


{$REGION 'TTestTryConvertStrToDateTime'}

procedure TTestTryConvertStrToDateTime.TestParseDate;
var
  actual, expected: TDateTime;
begin
  expected := EncodeDate(2009, 10, 18);

  CheckTrue(TryConvertStrToDateTime('20091018', 'YYYYMMDD', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryConvertStrToDateTime('091018', 'YYMMDD', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryConvertStrToDateTime('10-18-2009', 'MM-DD-YYYY', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryConvertStrToDateTime(' 2009-10-18 ', 'YYYY-MM-DD', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryConvertStrToDateTime.TestParseTime;
var
  actual, expected: TDateTime;
begin
  expected := EncodeTime(12, 10, 18, 35);
  CheckTrue(TryConvertStrToDateTime('12:10:18.035', 'hh:nn:ss.zzz', actual));
  CheckTrue(SameDateTime(actual, expected));

  expected := EncodeTime(12, 10, 0, 0);
  CheckTrue(TryConvertStrToDateTime('12:10 ', 'hh:nn', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryConvertStrToDateTime.TestParseDateTime;
var
  actual, expected: TDateTime;
begin
  expected := EncodeDateTime(2009, 10, 18, 12, 30, 59, 200);
  CheckTrue(TryConvertStrToDateTime('2009-10-18 12:30:59.200', 'YYYY-MM-DD HH:NN:SS.ZZZ', actual));
  CheckTrue(SameDateTime(actual, expected));

  expected := EncodeDateTime(2009, 10, 18, 12, 30, 59, 200);
  CheckTrue(TryConvertStrToDateTime('20091018123059200', 'YYYYMMDDHHNNSSZZZ', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryConvertStrToDateTime.TestFailedCases;
var
  value: TDateTime;
begin
  CheckFalse(TryConvertStrToDateTime('', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime(' ', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('2009', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('2009080', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('2009080A', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('200908011230', 'YYYYMMDDHHNNSS', value));
  CheckFalse(TryConvertStrToDateTime('20090801123007', 'YYYYMMDDHHNNSSZZZ', value));
end;

{$ENDREGION}


{$REGION 'TTestDelegate'}

procedure TTestDelegate.SetUp;
begin
  inherited;
  fDelegate := TDelegate<TProc>.Create;
  fCallback :=
    procedure (handler: TProc)
    begin
      handler();
    end;
  fHandler1 := Handler1;
  fHandler2 := Handler2;
  fHandler1Times := 0;
  fHandler2Times := 0;
end;

procedure TTestDelegate.TearDown;
begin
  fDelegate := nil;
  fHandler1 := nil;
  fHandler2 := nil;
  fCallback := nil;
  inherited;
end;

procedure TTestDelegate.Handler1;
begin
  Inc(fHandler1Times);
end;

procedure TTestDelegate.Handler2;
begin
  Inc(fHandler2Times);
end;

procedure TTestDelegate.TestOneHandler;
begin
  fDelegate.AddHandler(fHandler1);
  fDelegate.Invoke(fCallback);
  CheckEquals(1, fHandler1Times);
  CheckEquals(0, fHandler2Times);
end;

procedure TTestDelegate.TestTwoHandlers;
begin
  fDelegate.AddHandler(fHandler1);
  fDelegate.AddHandler(fHandler2);
  fDelegate.Invoke(fCallback);
  CheckEquals(1, fHandler1Times);
  CheckEquals(1, fHandler2Times);

  fDelegate.RemoveHandler(fHandler2);
  fDelegate.Invoke(fCallback);
  CheckEquals(2, fHandler1Times);
  CheckEquals(1, fHandler2Times);

  fDelegate.RemoveHandler(fHandler1);
  fDelegate.Invoke(fCallback);
  CheckEquals(2, fHandler1Times);
  CheckEquals(1, fHandler2Times);
end;

{$ENDREGION}

end.
