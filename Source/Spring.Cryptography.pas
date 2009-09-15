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

{TODO: Support PaddingMode}
{TODO: Support IV}
{TODO: Support ECB}
{TODO: Support AES}
{TODO: Support CRC-16, CRC-32}
{TODO: Support Base64}
{TODO: ICryptoTransform}
{TODO: Optimize DES & Triple-DES}

(*

Hash Algorithms
* MD5   (128 bits, 16 Bytes)

Checksum Algorithms
* CRC-16
* CRC-32

Encryption Algorithms
* DES
* TripleDES
* AES

*)


unit Spring.Cryptography;

{$I Spring.inc}

interface

uses
  Classes, Windows, SysUtils, Math, Spring.System;

type
  /// <summary>
  /// Defines the basic operations of cryptographic transformations.
  /// </summary>
  ICryptoTransform = interface
    function GetCanTransformMultipleBlocks: Boolean;
    function GetCanReuseTransform: Boolean;
    function GetInputBlockSize: Integer;
    function GetOutputBlockSize: Integer;
    function TransformBlock(const inputBuffer: Pointer; inputCount: Integer; outBuffer: Pointer): Integer;
    function TransformFinalBlock(const inputBuffer: Pointer; inputCount: Integer): TBytes;
    property CanReuseTransform: Boolean read GetCanReuseTransform;
    property CanTransformMultipleBlocks: Boolean read GetCanTransformMultipleBlocks;
    property InputBlockSize: Integer read GetInputBlockSize;
    property OutputBlockSize: Integer read GetOutputBlockSize;
  end;

  ECryptographicException = class(Exception);

  THashAlgorithm = class
  protected
    class function DoComputeHash(const buffer: Pointer; count: Integer): TBuffer; virtual; abstract;
  public
    class function ComputeHash(const buffer: array of Byte): TBuffer; overload; virtual;
    class function ComputeHash(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload; virtual;
    class function ComputeHash(const buffer: Pointer; count: Integer): TBuffer; overload; virtual;
    class function ComputeHash(const inputString: UnicodeString): TBuffer; overload; virtual;
    class function ComputeHash(const inputString: WideString): TBuffer; overload; virtual;
    class function ComputeHash(const inputString: RawByteString): TBuffer; overload; virtual;
//    class function ComputeHash(const inputStream: TCustomMemoryStream): TBuffer; overload; virtual;
//    class function ComputeHash(const inputStream: TCustomMemoryStream; offset: Integer; count: Integer): TBuffer; overload; virtual;
//    class function ComputeHashOfFile(const fileName: TFileName): TBuffer; overload; virtual;
  end;

  TMD5 = class(THashAlgorithm)
  protected
    class function DoComputeHash(const buffer: Pointer; count: Integer): TBuffer; override;
  end;

  { NOT READY }
//  TSHA1 = class(THashAlgorithm)
//  protected
//    class function DoComputeHash(const buffer: Pointer; count: Integer): TBuffer; override;
//  end;

  { NOT READY }
  TCRC16 = class(THashAlgorithm)
//  protected
//    class function DoComputeHash(const buffer: Pointer; count: Integer): TBuffer; override;
  end;

  { NOT READY }
  TCRC32 = class(THashAlgorithm)
  protected
    class function DoComputeHash(const buffer: Pointer; count: Integer): TBuffer; override;
  end;

  //(*
  TSymmetricAlgorithm = class
  protected
    class function DoEncrypt(const buffer: Pointer;  count: Integer; const key: TBuffer): TBuffer; virtual; abstract;
    class function DoDecrypt(const buffer: Pointer;  count: Integer; const key: TBuffer): TBuffer; virtual; abstract;
  public
    class function Encrypt(const buffer, key: TBuffer): TBuffer; overload; virtual;
    class function Encrypt(const buffer: array of Byte; const key: TBuffer): TBuffer; overload; virtual;
    class function Encrypt(const buffer: array of Byte; startIndex, count: Integer; const key: TBuffer): TBuffer; overload; virtual;
    class function Encrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; overload; virtual;
    class function Encrypt(const inputString: UnicodeString; const key: TBuffer): TBuffer; overload; virtual;
    class function Encrypt(const inputString: WideString; const key: TBuffer): TBuffer; overload; virtual;
    class function Encrypt(const inputString: RawByteString; const key: TBuffer): TBuffer; overload; virtual;
//    class procedure Encrypt(inputStream, outputStream: TStream; const key: TBuffer); overload; virtual;
    class function Decrypt(const buffer, key: TBuffer): TBuffer; overload; virtual;
    class function Decrypt(const buffer: array of Byte; const key: TBuffer): TBuffer; overload; virtual;
    class function Decrypt(const buffer: array of Byte; startIndex, count: Integer; const key: TBuffer): TBuffer; overload; virtual;
    class function Decrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; overload; virtual;
    class function Decrypt(const inputString: UnicodeString; const key: TBuffer): TBuffer; overload; virtual;
    class function Decrypt(const inputString: WideString; const key: TBuffer): TBuffer; overload; virtual;
    class function Decrypt(const inputString: RawByteString; const key: TBuffer): TBuffer; overload; virtual;
//    class procedure Decrypt(inputStream, outputStream: TStream; const key: TBuffer); overload; virtual;
  end;

  /// <summary>
  /// Data Encryption Standard (DES)
  /// </summary>
  TDES = class(TSymmetricAlgorithm)
  protected
    class function DoEncrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; override;
    class function DoDecrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; override;
  end;

  /// <summary>
  /// Triple Data Encryption Algorithm
  /// </summary>
  TTripleDES = class(TSymmetricAlgorithm)
  protected
    class function DoEncrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; override;
    class function DoDecrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; override;
  end;

  /// <summary>
  /// AES Algorithm
  /// </summary>
  TAES = class(TSymmetricAlgorithm)
  protected
//    class function DoEncrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; override;
//    class function DoDecrypt(const buffer: Pointer; count: Integer; const key: TBuffer): TBuffer; override;
  end;

  //*)

//  TAsymmetricAlgorithm = class abstract(TObject)
//
//  end;

implementation

uses
  MD5Impl, CRCImpl, DESImpl;


{$REGION 'THashAlgorithm'}

class function THashAlgorithm.ComputeHash(const buffer: array of Byte): TBuffer;
begin
  Result := DoComputeHash(@buffer[0], Length(buffer));
end;

class function THashAlgorithm.ComputeHash(const buffer: array of Byte; startIndex,
  count: Integer): TBuffer;
begin
  CheckBufferRange(buffer, startIndex, count);
  Result := DoComputeHash(PByte(@buffer), count);
end;

class function THashAlgorithm.ComputeHash(const buffer: Pointer;
  count: Integer): TBuffer;
begin
  Result := DoComputeHash(buffer, count);
end;

class function THashAlgorithm.ComputeHash(
  const inputString: UnicodeString): TBuffer;
begin
  Result := DoComputeHash(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

class function THashAlgorithm.ComputeHash(
  const inputString: WideString): TBuffer;
begin
  Result := DoComputeHash(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

class function THashAlgorithm.ComputeHash(
  const inputString: RawByteString): TBuffer;
begin
  Result := DoComputeHash(PByte(inputString), Length(inputString));
end;

{$ENDREGION}


{$REGION 'TMD5'}

class function TMD5.DoComputeHash(const buffer: Pointer;
  count: Integer): TBuffer;
var
  md5Context: TMD5Context;
  md5Digest: TMD5Digest;
  i: Byte;
begin
  for i := 0 to 15 do
  begin
    md5Digest[i] := i + 1;
  end;
  MD5Init(md5Context);
  MD5UpdateBuffer(md5Context, buffer, count);
  MD5Final(md5Digest, md5Context);
  Result := TBuffer.Create(@md5Digest, SizeOf(md5Digest));
end;

{$ENDREGION}


{$REGION 'TCRC32'}

class function TCRC32.DoComputeHash(const buffer: Pointer;
  count: Integer): TBuffer;
begin
  Result := TCrc32Impl.ComputeHash(buffer, count);
end;

{$ENDREGION}


{$REGION 'TSymmetricAlgorithm'}

class function TSymmetricAlgorithm.Encrypt(const buffer: array of Byte;
  const key: TBuffer): TBuffer;
begin
  Result := DoEncrypt(PByte(@buffer[0]), Length(buffer), key);
end;

class function TSymmetricAlgorithm.Encrypt(const buffer: Pointer;
  count: Integer; const key: TBuffer): TBuffer;
begin
  Result := DoEncrypt(buffer, count, key);
end;

class function TSymmetricAlgorithm.Encrypt(const buffer, key: TBuffer): TBuffer;
begin
  Result := DoEncrypt(buffer.Memory, buffer.Size, key);
end;

class function TSymmetricAlgorithm.Encrypt(const buffer: array of Byte; startIndex,
  count: Integer; const key: TBuffer): TBuffer;
begin
  CheckBufferRange(buffer, startIndex, count);
  Result := DoEncrypt(@buffer[startIndex], count, key);
end;

class function TSymmetricAlgorithm.Encrypt(const inputString: UnicodeString;
  const key: TBuffer): TBuffer;
begin
  Result := DoEncrypt(PByte(inputString), Length(inputString) * SizeOf(Char), key);
end;

class function TSymmetricAlgorithm.Encrypt(const inputString: WideString;
  const key: TBuffer): TBuffer;
begin
  Result := DoEncrypt(PByte(inputString), Length(inputString) * SizeOf(Char), key);
end;

class function TSymmetricAlgorithm.Encrypt(const inputString: RawByteString;
  const key: TBuffer): TBuffer;
begin
  Result := DoEncrypt(PByte(inputString), Length(inputString), key);
end;

class function TSymmetricAlgorithm.Decrypt(const buffer: Pointer;
  count: Integer; const key: TBuffer): TBuffer;
begin
  Result := DoDecrypt(buffer, count, key);
end;

class function TSymmetricAlgorithm.Decrypt(const buffer, key: TBuffer): TBuffer;
begin
  Result := DoDecrypt(buffer.Memory, buffer.Size, key);
end;

class function TSymmetricAlgorithm.Decrypt(const buffer: array of Byte; startIndex,
  count: Integer; const key: TBuffer): TBuffer;
begin
  CheckBufferRange(buffer, startIndex, count);
  Result := DoEncrypt(@buffer[startIndex], count, key);
end;

class function TSymmetricAlgorithm.Decrypt(const buffer: array of Byte;
  const key: TBuffer): TBuffer;
begin
  Result := DoDecrypt(PByte(@buffer[0]), Length(buffer), key);
end;

class function TSymmetricAlgorithm.Decrypt(const inputString: UnicodeString;
  const key: TBuffer): TBuffer;
begin
  Result := DoDecrypt(PByte(inputString), Length(inputString) * SizeOf(Char), key);
end;

class function TSymmetricAlgorithm.Decrypt(const inputString: WideString;
  const key: TBuffer): TBuffer;
begin
  Result := DoDecrypt(PByte(inputString), Length(inputString) * SizeOf(Char), key);
end;

class function TSymmetricAlgorithm.Decrypt(const inputString: RawByteString;
  const key: TBuffer): TBuffer;
begin
  Result := DoDecrypt(PByte(inputString), Length(inputString), key);
end;

{$ENDREGION}


{$REGION 'TDES'}

class function TDES.DoEncrypt(const buffer: Pointer; count: Integer;
  const key: TBuffer): TBuffer;
var
  inBuffer: TBytes;
  outBuffer: TBytes;
  keyBuffer: TBytes;
  p: PByte;
  i: Integer;
  blockCount: Integer;
  paddingSize: Integer;
begin
  SetLength(inBuffer, 8);
  SetLength(outBuffer, 8);
  keyBuffer := key.EnsureSize(8, #0);
  if count mod 8 <> 0 then
  begin
    blockCount := count div 8 + 1;
    paddingSize := 8 - (count mod 8);
  end
  else
  begin
    blockCount := count div 8;
    paddingSize := 0;
  end;
  p := buffer;
  for i := 0 to blockCount - 2 do
  begin
    Move(p^, inBuffer[0], 8);
    DESImpl.EncryData(keyBuffer, inBuffer, outBuffer);
    Result := Result + outBuffer;
    Inc(p, 8);
  end;
  Move(p^, inBuffer[0], 8 - paddingSize);
  FillChar(inBuffer[8-paddingSize-1], paddingSize, 0);   // TODO: Padding
  DESImpl.EncryData(keyBuffer, inBuffer, outBuffer);
  Result := Result + outBuffer;
end;

class function TDES.DoDecrypt(const buffer: Pointer; count: Integer;
  const key: TBuffer): TBuffer;
var
  inBuffer: TBytes;
  outBuffer: TBytes;
  keyBuffer: TBytes;
  p: PByte;
  i: Integer;
begin
  SetLength(inBuffer, 8);
  SetLength(outBuffer, 8);
  keyBuffer := key.EnsureSize(8, #0);
  p := buffer;
  for i := 0 to count div 8 - 1 do
  begin
    Move(p^, inBuffer[0], 8);
    DESImpl.DecryData(keyBuffer, inBuffer, outBuffer);
    Result := Result + outBuffer;
    Inc(p, 8);
  end;
end;

{$ENDREGION}


{$REGION 'TTripleDES'}

/// <remarks>
/// 3DES（即Triple DES）是DES向AES过渡的加密算法（1999年，NIST将3-DES指定为
/// 过渡的加密标准），是DES的一个更安全的变形。它以DES为基本模块，通过组合分组
/// 方法设计出分组加密算法，其具体实现如下：设Ek()和Dk()代表DES算法的加密和解密过程，
/// K代表DES算法使用的密钥，P代表明文，C代表密表，这样，
///　　3DES加密过程为：C=Ek3(Dk2(Ek1(P)))
///　　3DES解密过程为：P=Dk1((EK2(Dk3(C)))
/// </remarks>

function TripleDESEncrypt(const buffer: Pointer; count: Integer;
  const key1, key2, key3: TBuffer): TBuffer;
begin
  Result := TDES.Encrypt(TDES.Decrypt(TDES.Encrypt(buffer, count, key1), key2), key3);
end;

function TripleDESDecrypt(const buffer: Pointer; count: Integer;
  const key1, key2, key3: TBuffer): TBuffer;
begin
  Result := TDES.Decrypt(TDES.Encrypt(TDES.Decrypt(buffer, count, key3), key2), key1);
end;

class function TTripleDES.DoEncrypt(const buffer: Pointer; count: Integer;
  const key: TBuffer): TBuffer;
var
  keyBuffer: TBuffer;
  key1, key2, key3: TBuffer;
  bytesLeft: Integer;
  p: PByte;
begin
  keyBuffer := key.EnsureSize(24, #0);
  key1 := keyBuffer.Copy(0, 8);
  key2 := keyBuffer.Copy(8, 8);
  if key.Size > 16 then
  begin
    key3 := keyBuffer.Copy(16, 8);
  end
  else
  begin
    key3 := key1;
  end;
  bytesLeft := count;
  p := buffer;
  while bytesLeft > 0 do
  begin
    Result := Result + TripleDESEncrypt(p, Min(8, bytesLeft), key1, key2, key3);
    Dec(bytesLeft, 8);
    Inc(p, 8);
  end;
end;

class function TTripleDES.DoDecrypt(const buffer: Pointer; count: Integer;
  const key: TBuffer): TBuffer;
var
  keyBuffer: TBuffer;
  key1, key2, key3: TBuffer;
  bytesLeft: Integer;
  p: PByte;
begin
  keyBuffer := key.EnsureSize(24, #0);
  key1 := keyBuffer.Copy(0, 8);
  key2 := keyBuffer.Copy(8, 8);
  if key.Size > 16 then
  begin
    key3 := keyBuffer.Copy(16, 8);
  end
  else
  begin
    key3 := key1;
  end;
  bytesLeft := count;
  p := buffer;
  while bytesLeft > 0 do
  begin
    Result := Result + TripleDESDecrypt(p, Min(8, bytesLeft), key1, key2, key3);
    Dec(bytesLeft, 8);
    Inc(p, 8);
  end;
end;

{$ENDREGION}

end.
