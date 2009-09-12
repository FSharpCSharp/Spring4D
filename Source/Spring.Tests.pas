{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
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

unit Spring.Tests;

{$I Spring.inc}

interface

uses
  Classes, TypInfo, SysUtils, Graphics, Variants, TestFramework, TestExtensions,
  Generics.Defaults, Types,
  Spring.System, Spring.Cryptography, Spring.Helpers, Spring.Patterns, Spring.Core;

type
  TTestSplitString = class(TTestCase)
  private
    fStrings: TStringDynArray;
  published
    procedure TestNormalText;
  end;

  (*

  [DisplayName('Int64')]
  TInt64 = type Int64;

  TEnumExample = (
    [DisplayName('One')]
    eeOne,
    [DisplayName('Two')]
    eeTwo,
    [DisplayName('Three')]
    eeThree
  );

  //*)

  TTestVersionNumber = class(TTestCase)
  published
    procedure TestCompareTo;
    procedure TestFromString;
    procedure TestToString;
    procedure TestToStringException;
    procedure TestArgumentException;
    procedure TestArgumentOutOfRangeException;
    procedure TestFormatException;
  end;

  TTestGuidHelper = class(TTestCase)
  published
    procedure TestNewGUID;
    procedure TestEmpty;
    procedure TestEquals;
    procedure TestToString;
    procedure TestToQuotedString;
  end;

  TTestPersistentSnapshot = class(TTestCase)
  published
    procedure TestFont;
    procedure TestStrings;
  end;

  TTestCryptography = class(TTestCase)
  published
    procedure TestMD5;
  end;

  TTestRtti = class(TTestCase)
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
  {$IFDEF DELPHI2010_UP}
    procedure TestFromVariant;
  {$ENDIF ~DELPHI2010_UP}
  end;

  TTestAdapter = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestGetAdapter;
  end;

  TTestSingleton = class(TTestcase)
  published
    procedure TestGetInstance;
    procedure TestCreate;
    procedure TestFree;
  end;

implementation

{ TTestSplitString }

procedure TTestSplitString.TestNormalText;
begin
  fStrings := SplitString('Normal', '');
  CheckEquals(1, Length(fStrings));
  CheckEquals('Normal', fStrings[0]);
end;

{$IFDEF SUPPORTS_REGION} {$REGION 'TTestEnum'} {$ENDIF}

{$IFDEF SUPPORTS_GENERICS}

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
  ExpectedException := EInvalidEnumArgumentException;
  TEnum.Parse<TDriveType>(Integer(Low(TDriveType))-1);
end;

procedure TTestEnum.TestParseStringException;
begin
  ExpectedException := EInvalidEnumArgumentException;
  TEnum.Parse<TDriveType>('dummy');
end;

{$ENDIF ~SUPPORTS_GENERICS}

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$REGION 'TTestVersion'} {$ENDIF}

//  Version 1.1 is older than version 1.1.0.
//  Version 1.1 is older than version 1.1.1.
//  Version 1.1 is older than version 1.1.2.3.
//  Version 1.1.2 is older than version 1.1.2.4.
//  Version 1.2.5 is newer than version 1.2.3.4.

procedure TTestVersionNumber.TestCompareTo;
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

procedure TTestVersionNumber.TestFromString;
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

procedure TTestVersionNumber.TestToString;
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

procedure TTestVersionNumber.TestToStringException;
var
  ver: TVersion;
begin
  ExpectedException := EArgumentException;
  ver := TVersion.Create('1.0');
  ver.ToString(3);
end;

procedure TTestVersionNumber.TestArgumentException;
begin
  ExpectedException := EArgumentException;
  TVersion.Create('1');
end;

procedure TTestVersionNumber.TestFormatException;
begin
  ExpectedException := EFormatException;
  TVersion.Create('1.c.d');
end;

procedure TTestVersionNumber.TestArgumentOutOfRangeException;
begin
  ExpectedException := EArgumentOutOfRangeException;
  TVersion.Create('1.-1.0');
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$REGION 'TTestNullableInteger'} {$ENDIF}

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
  dirtyValue: TNullable<Integer>;  { live in stack }
begin
  CheckFalse(dirtyValue.HasValue);
end;

{$IFDEF DELPHI2010_UP}

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

{$ENDIF ~DELPHI2010_UP}

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$REGION 'TTestGuidHelper'} {$ENDIF}

procedure TTestGuidHelper.TestNewGUID;
var
  guid: TGUID;
begin
  guid := TGUID.NewGUID;
  CheckEquals(38, Length(guid.ToString));
end;

procedure TTestGuidHelper.TestEmpty;
var
  empty: TGUID;
begin
  empty := TGUID.Empty;
  CheckEquals('{00000000-0000-0000-0000-000000000000}', empty.ToString);
  CheckTrue(empty.IsEmpty);
end;

procedure TTestGuidHelper.TestEquals;
var
  guid: TGUID;
const
  GuidString = '{93585BA2-B43B-4C55-AAAB-6DE6EB4C0E57}';
begin
  guid := TGUID.Create(GuidString);
  Check(guid.Equals(guid));
  CheckFalse(guid.Equals(TGUID.Empty));
end;

procedure TTestGuidHelper.TestToString;
var
  guid: TGUID;
const
  GuidString = '{93585BA2-B43B-4C55-AAAB-6DE6EB4C0E57}';
begin
  guid := TGuid.Create(GuidString);
  CheckEquals(GuidString, guid.ToString);
end;

procedure TTestGuidHelper.TestToQuotedString;
var
  guid: TGUID;
const
  GuidString = '{93585BA2-B43B-4C55-AAAB-6DE6EB4C0E57}';
begin
  guid := TGuid.Create(GuidString);
  CheckEquals(QuotedStr(GuidString), guid.ToQuotedString);
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$REGION 'TTestPersistentSnapshot'} {$ENDIF}

procedure TTestPersistentSnapshot.TestFont;
var
  font: TFont;
  snapshot: ISnapshot;
const
  OriginalSize = 9;
  OriginalColor = clRed;
begin
  font := TFont.Create;
  try
    font.Size := OriginalSize;
    font.Color := OriginalColor;
    snapshot := font.CreateSnapshot<TFont>;    // OR TSnapshot<TFont>.Create(font);
    try
      font.Size := OriginalSize + 2;
      font.Color := Pred(OriginalColor);
    finally
      font.Restore(snapshot);
    end;
    CheckEquals(OriginalSize, font.Size);
    CheckEquals(OriginalColor, font.Color);
  finally
    font.Free;
  end;
end;

procedure TTestPersistentSnapshot.TestStrings;
var
  strings: TStrings;
  snapshot: ISnapshot;
const
  OriginalText = 'Hello'#13#10'World'#13#10;
begin
  strings := TStringList.Create;
  try
    strings.Text := OriginalText;
    snapshot := strings.CreateSnapshot<TStringList>;
    try
      strings.Add('Dummy');
    finally
      strings.Restore(snapshot);
    end;
    CheckEquals(OriginalText, strings.Text);
  finally
    strings.Free;
  end;
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$REGION 'TTestSingleton'} {$ENDIF}

type
  TTestableSingleton = class(TSingleton)
  end;

procedure TTestSingleton.TestGetInstance;
var
  singleton: TSingleton;
begin
  singleton := TTestableSingleton.GetInstance;
  CheckNotNull(singleton);
  CheckSame(singleton, TTestableSingleton.GetInstance);
  CheckTrue(singleton <> TSingleton.GetInstance);
end;

procedure TTestSingleton.TestCreate;
begin
  ExpectedException := ENotSupportedException;
  {$HINTS OFF}
  TTestableSingleton.Create;
  {$HINTS ON}
end;

procedure TTestSingleton.TestFree;
begin
  ExpectedException := ENotSupportedException;
  TTestableSingleton.GetInstance.Free;
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$REGION 'TTestAdapter'} {$ENDIF}

procedure TTestAdapter.SetUp;
begin
  inherited;
end;

procedure TTestAdapter.TestGetAdapter;
begin
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$REGION 'TTestCryptography'}

procedure TTestCryptography.TestMD5;
var
  md5: TRawBytes;
begin
  md5 := TMD5.ComputeHash('');
  CheckEquals('D41D8CD98F00B204E9800998ECF8427E', md5.ToHexString);
  md5 := TMD5.ComputeHash('this is a test string.');
  CheckEquals('89CC6719F16EE3FBDC7A9F11379F14CF', md5.ToHexString);
end;

{$ENDREGION}

initialization
  RegisterTests('Spring.System.Tests', [
    TTestSplitString.Suite,
    TTestVersionNumber.Suite,
    TTestEnum.Suite,
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3)
  ]);

  RegisterTests('Spring.Cryptography.Tests', [
    TTestCryptography.Suite
  ]);

  RegisterTests('Spring.Patterns.Tests', [
    TTestSingleton.Suite,
    TTestAdapter.Suite
  ]);

  RegisterTests('Spring.Helpers.Tests', [
    TTestGuidHelper.Suite,
    TTestPersistentSnapshot.Suite
  ]);

end.
