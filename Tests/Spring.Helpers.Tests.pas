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

unit Spring.Helpers.Tests;

{$I Spring.inc}

interface

uses
  Classes, SysUtils, Graphics, TestFramework,
  Spring.System, Spring.Helpers, Spring.Patterns;

type
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


implementation


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
const
  EmptyGuidString = '{00000000-0000-0000-0000-000000000000}';
begin
  empty := TGUID.Empty;
  CheckEquals(EmptyGuidString, empty.ToString);
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

end.
