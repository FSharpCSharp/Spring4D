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

unit Spring.Security.Tests;

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring.System,
  Spring.Cryptography;

type
  TTestMD5 = class(TTestCase)
  private
    fHash: TBuffer;
  published
    procedure TestEmptyString;
    procedure TestNormalString;
  end;

  TSymmetricAlgorithmTestCase = class abstract(TTestCase)
  protected
    fActualBuffer: TBuffer;
    fInputBuffer: TBuffer;
    fOutputBuffer: TBuffer;
    fKey: TBuffer;
  end;

  TTestDES = class(TSymmetricAlgorithmTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
  end;

  TTestTripleDES = class(TSymmetricAlgorithmTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestEncrypt;
    procedure TestDecrypt;
  end;

implementation


{$REGION 'TTestMD5'}

procedure TTestMD5.TestEmptyString;
begin
  fHash := TMD5.ComputeHash('');
  CheckEquals('D41D8CD98F00B204E9800998ECF8427E', fHash.ToHexString);
end;

procedure TTestMD5.TestNormalString;
begin
  fHash := TMD5.ComputeHash('this is a test string.');
  CheckEquals('89CC6719F16EE3FBDC7A9F11379F14CF', fHash.ToHexString);
end;

{$ENDREGION}


{$REGION 'TTestDES'}

procedure TTestDES.TestCase1;
const
  inputData: array [0 .. 7] of Byte = ($07, $56, $D8, $E0, $77, $47, $61, $D2);
  outputData: array [0 .. 7] of Byte = ($0C, $D3, $DA, $02, $00, $21, $DC, $09);
  key: array [0 .. 7] of Byte = ($01, $70, $F1, $75, $46, $8F, $B5, $E6);
begin
  fInputBuffer := TBuffer.Create(inputData);
  fOutputBuffer := TBuffer.Create(outputData);
  fKey := TBuffer.Create(key);

  fActualBuffer := TDES.Encrypt(fInputBuffer, fKey);
  CheckTrue(fActualBuffer.Equals(fOutputBuffer));

  fActualBuffer := TDES.Decrypt(fOutputBuffer, fKey);
  CheckTrue(fActualBuffer.Equals(fInputBuffer));
end;

procedure TTestDES.TestCase2;
const
  inputData: array [0 .. 7] of Byte = ($48, $0D, $39, $00, $6E, $E7, $62, $F2);
  outputData: array [0 .. 7] of Byte = ($A1, $F9, $91, $55, $41, $02, $0B, $56);
  key: array [0 .. 7] of Byte = ($02, $58, $16, $16, $46, $29, $B0, $07);
begin
  fInputBuffer := TBuffer.Create(inputData);
  fOutputBuffer := TBuffer.Create(outputData);
  fKey := TBuffer.Create(key);

  fActualBuffer := TDES.Encrypt(fInputBuffer, fKey);
  CheckTrue(fActualBuffer.Equals(fOutputBuffer));

  fActualBuffer := TDES.Decrypt(fOutputBuffer, fKey);
  CheckTrue(fActualBuffer.Equals(fInputBuffer));
end;

{$ENDREGION}


{$REGION 'TTestTripleDES'}

procedure TTestTripleDES.SetUp;
const
  key: array [0 .. 23] of Byte = ($01, $23, $45, $67, $89, $AB, $CD, $EF, $FE,
    $DC, $BA, $98, $76, $54, $32, $10, $89, $AB, $CD, $EF, $01, $23, $45, $67);
  plainText: array [0 .. 7] of Byte = ($01, $23, $45, $67, $89, $AB, $CD, $E7);
  cipherText: array [0 .. 7] of Byte = ($DE, $0B, $7C, $06, $AE, $5E, $0E, $D5);
begin
  inherited;
  fInputBuffer := TBuffer.Create(plainText);
  fOutputBuffer := TBuffer.Create(cipherText);
  fKey := TBuffer.Create(key);
end;

procedure TTestTripleDES.TestEncrypt;
begin
  fActualBuffer := TTripleDES.Encrypt(fInputBuffer, fKey);
  CheckTrue(fActualBuffer.Equals(fOutputBuffer));
end;

procedure TTestTripleDES.TestDecrypt;
begin
  fActualBuffer := TTripleDES.Decrypt(fOutputBuffer, fKey);
  CheckTrue(fActualBuffer.Equals(fInputBuffer));
end;

{$ENDREGION}

end.
