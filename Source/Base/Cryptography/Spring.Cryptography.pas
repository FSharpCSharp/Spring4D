{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

{ TODO: Support more Cipher Modes: cmOFB, cmCFB, cmCTS  }
{ TODO: Support AES }
{ TODO: Support MACTripleDES }
{ TODO: AsymmetricAlgorithm, RSA }

/// <seealso>http://msdn.microsoft.com/en-us/library/92f9ye3s(VS.71).aspx</seealso>
/// <seealso>http://msdn.microsoft.com/en-us/library/system.security.cryptography.aspx</seealso>
/// <seealso>http://en.wikipedia.org/wiki/Cryptography</seealso>
unit Spring.Cryptography;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Spring.System,
  Spring.Cryptography.CRC,
  Spring.Cryptography.MD5,
  Spring.Cryptography.SHA,
  Spring.Cryptography.DES;

type
  {$REGION 'Core Types'}

  /// <summary>
  /// Defines the basic operations of hash algorithms.
  /// </summary>
  IHashAlgorithm = interface
    ['{D33C6DB1-7C51-4DDE-BB7D-ACE98BB61EBE}']
  {$REGION 'Property Getters and Setters'}
    function GetHashSize: Integer;
  {$ENDREGION}
    function ComputeHash(const buffer: array of Byte): TBuffer; overload;
    function ComputeHash(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function ComputeHash(const buffer: Pointer; count: Integer): TBuffer; overload;
    function ComputeHash(const inputString: string): TBuffer; overload;
    function ComputeHash(const inputString: WideString): TBuffer; overload;
    function ComputeHash(const inputString: RawByteString): TBuffer; overload;
    function ComputeHash(const inputStream: TStream): TBuffer; overload;
    function ComputeHashOfFile(const fileName: string): TBuffer;  // callback?
    /// <summary>
    /// Gets the hash size, in bytes, of the algorithm.
    /// </summary>
    property HashSize: Integer read GetHashSize;
  end;

  /// <summary>
  /// IKeyedHashAlgorithm
  /// </summary>
  /// <remarks>
  /// From MSDN:
  /// A keyed hash algorithm is a key-dependent, one-way hash function used as
  /// a message authentication code. Only someone who knows the key can verify
  /// the hash. Keyed hash algorithms provide authenticity without secrecy.
  /// </remarks>
  IKeyedHashAlgorithm = interface(IHashAlgorithm)
    ['{0D6838E7-05C0-4874-86B0-732DF42105F5}']
  {$REGION 'Property Getters and Setters'}
    function GetKey: TBuffer;
    procedure SetKey(const value: TBuffer);
  {$ENDREGION}
    /// <summary>
    /// Gets or sets the key to use in the hash algorithm.
    /// </summary>
    property Key: TBuffer read GetKey write SetKey;
  end;

  /// <summary>
  /// TCipherMode
  /// </summary>
  /// <seealso>http://en.wikipedia.org/wiki/Block_cipher_mode_of_operation</seealso>
  TCipherMode = (
    /// <summary>
    /// Cipher-Block Chaining
    /// </summary>
    cmCBC,
    /// <summary>
    /// Electronic Codebook
    /// </summary>
    cmECB (*,
    /// <summary>
    /// Output Feedback
    /// </summary>
    cmOFB,
    /// <summary>
    /// Cipher Feedback
    /// </summary>
    cmCFB,
    /// <summary>
    /// Cipher Text Stealing
    /// </summary>
    cmCTS
    //*)
  );

  /// <summary>
  /// Specifies the type of padding to apply when the message data block is
  /// shorter than the full number of bytes needed for a cryptographic operation.
  /// </summary>
  /// <seealso>http://msdn.microsoft.com/en-us/library/system.security.cryptography.paddingmode.aspx</seealso>
  /// <seealso>http://en.wikipedia.org/wiki/Padding_(cryptography)</seealso>
  TPaddingMode = (
    /// <summary>
    /// No padding is done.
    /// </summary>
    pmNone,
    /// <summary>
    /// The PKCS #7 padding string consists of a sequence of bytes, each of
    /// which is equal to the total number of padding bytes added.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// PKCS7 padding: FF FF FF FF FF FF FF FF FF 07 07 07 07 07 07 07
    /// </remarks>
    pmPKCS7,
    /// <summary>
    /// The padding string consists of bytes set to zero.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// Zeros padding: FF FF FF FF FF FF FF FF FF 00 00 00 00 00 00 00
    /// </remarks>
    pmZeros,
    /// <summary>
    /// The ANSIX923 padding string consists of a sequence of bytes filled with
    /// zeros before the length.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// X923 padding: FF FF FF FF FF FF FF FF FF 00 00 00 00 00 00 07
    /// </remarks>
    pmANSIX923,
    /// <summary>
    /// The ISO10126 padding string consists of random data before the length.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// ISO10126 padding: FF FF FF FF FF FF FF FF FF 7D 2A 75 EF F8 EF 07
    /// </remarks>
    pmISO10126
  );

  /// <summary>
  /// Represents a size list that can determine whether a size is valid.
  /// </summary>
  TSizes = record
  private
    fValues: TArray<Integer>;
  public
    constructor Create(size: Integer); overload;
    constructor Create(const sizes: array of Integer); overload;
    function Contains(const size: Integer): Boolean;
    property Values: TArray<Integer> read fValues;
  end;

  /// <summary>
  /// Performs a transformation on data to keep it from being read by third parties.
  /// This type of encryption uses a single shared, secret key to encrypt and decrypt data.
  /// </summary>
  /// <seealso>http://en.wikipedia.org/wiki/Cipher</seealso>
  /// <seealso>http://en.wikipedia.org/wiki/Block_cipher_mode_of_operation</seealso>
  ISymmetricAlgorithm = interface
    ['{98E0E218-2BD4-4AFA-87B2-E8C4812B2105}']
  {$REGION 'Property Getters and Setters'}
    function GetBlockSize: Integer;
    function GetKeySize: Integer;
    function GetLegalBlockSizes: TSizes;
    function GetLegalKeySizes: TSizes;
    function GetCipherMode: TCipherMode;
    function GetPaddingMode: TPaddingMode;
    function GetKey: TBuffer;
    function GetIV: TBuffer;
    procedure SetBlockSize(const value: Integer);
    procedure SetKeySize(const value: Integer);
    procedure SetCipherMode(const value: TCipherMode);
    procedure SetPaddingMode(const value: TPaddingMode);
    procedure SetKey(const value: TBuffer);
    procedure SetIV(const value: TBuffer);
  {$ENDREGION}
    function Encrypt(const buffer: TBuffer): TBuffer; overload;
    function Encrypt(const buffer: array of Byte): TBuffer; overload;
    function Encrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function Encrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function Encrypt(const inputString: string): TBuffer; overload;
    function Encrypt(const inputString: WideString): TBuffer; overload;
    function Encrypt(const inputString: RawByteString): TBuffer; overload;
    procedure Encrypt(inputStream, outputStream: TStream); overload;
    function Decrypt(const buffer: TBuffer): TBuffer; overload;
    function Decrypt(const buffer: array of Byte): TBuffer; overload;
    function Decrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function Decrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function Decrypt(const inputString: string): TBuffer; overload;
    function Decrypt(const inputString: WideString): TBuffer; overload;
    function Decrypt(const inputString: RawByteString): TBuffer; overload;
    procedure Decrypt(inputStream, outputStream: TStream); overload;
    /// <summary>
    /// Gets or sets the ciphyer mode for operation of the symmetric algorithm.
    /// </summary>
    property CipherMode: TCipherMode read GetCipherMode write SetCipherMode;
    /// <summary>
    /// Gets or sets the padding mode used in the symmetric algorithm.
    /// </summary>
    property PaddingMode: TPaddingMode read GetPaddingMode write SetPaddingMode;
    /// <summary>
    /// Gets or sets the secret key for the symmetric algorithm.
    /// </summary>
    property Key: TBuffer read GetKey write SetKey;
    /// <summary>
    /// Gets or sets the value of initialization vector.
    /// </summary>
    property IV: TBuffer read GetIV write SetIV;
    /// <summary>
    /// Gets or sets the block size, in bytes, of the cryptographic operation.
    /// </summary>
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
    /// <summary>
    /// Gets or sets the size, in bytes, of the secret key used by the symmetric algorithm.
    /// </summary>
    property KeySize: Integer read GetKeySize write SetKeySize;
    /// <summary>
    /// Gets the block sizes, in bytes, that are supported by the symmetric algorithm.
    /// </summary>
    property LegalBlockSizes: TSizes read GetLegalBlockSizes;
    /// <summary>
    /// Gets the key sizes, in bytes, that are supported by the symmetric algorithm.
    /// </summary>
    property LegalKeySizes: TSizes read GetLegalKeySizes;
  end;

  (*
  /// <summary>
  /// Performs a transformation on data to keep it from being read by third parties.
  /// This type of encryption uses a public/private key pair to encrypt and decrypt data.
  /// </summary>
  IAsymmetricAlgorithm = interface
    ['{2BD66C06-1054-43A5-B9B6-2A8B9193BAE3}']
    //...
  end;
  //*)

  /// <summary>
  /// Generates cryptographically strong random values.
  /// </summary>
  IRandomNumberGenerator = interface
    ['{64B180B9-E192-4542-A45D-4E7402ED7BA8}']
    procedure GetBytes(var data: TBytes);
    procedure GetNonZeroBytes(var data: TBytes);
  end;

  /// <summary>
  /// Defines the basic operations of cryptographic transformations.
  /// </summary>
  ICryptoTransform = interface
    ['{0A0F5C2D-5E4A-4AD3-B09A-92E43A946D3F}']
  {$REGION 'Property Getters and Setters'}
    function GetCanTransformMultipleBlocks: Boolean;
    function GetCanReuseTransform: Boolean;
    function GetInputBlockSize: Integer;
    function GetOutputBlockSize: Integer;
  {$ENDREGION}
//    procedure Initialize; ?
//    BeginTransform, EndTransform ?
    function TransformBlock(const inputBuffer: Pointer; inputCount: Integer; outBuffer: Pointer): Integer;
    function TransformFinalBlock(const inputBuffer: Pointer; inputCount: Integer): TBytes;
    property CanReuseTransform: Boolean read GetCanReuseTransform;
    property CanTransformMultipleBlocks: Boolean read GetCanTransformMultipleBlocks;
    property InputBlockSize: Integer read GetInputBlockSize;
    property OutputBlockSize: Integer read GetOutputBlockSize;
  end;

  ECryptographicException = class(Exception);

  {$ENDREGION}


  {$REGION 'Hash Algorithms'}

type
  ICRC16 = interface(IHashAlgorithm)
    ['{7FEC815E-52E3-4C48-AAC1-7DEE905A6C1F}']
  end;

  ICRC32 = interface(IHashAlgorithm)
    ['{96581075-EC4C-4C3F-A031-88FCD4D9F3EA}']
  end;

  IMD5 = interface(IHashAlgorithm)
    ['{84E96BE7-0959-490D-9CF0-A62FEF72BFE7}']
  end;

  ISHA1 = interface(IHashAlgorithm)
    ['{FB202EDF-7846-4F3C-9088-D581CC8E1BC0}']
  end;

  ISHA256 = interface(IHashAlgorithm)
    ['{1DD90B12-CB33-44F2-A996-E7A0B5F0C541}']
  end;

  ISHA384 = interface(IHashAlgorithm)
    ['{B8CFF3B2-D319-4D21-89E5-5A1E0DB540B5}']
  end;

  ISHA512 = interface(IHashAlgorithm)
    ['{986B1C68-C156-46B8-A36C-822E7E5BC35E}']
  end;

  (*
  /// <summary>
  /// Message Authentication Code
  /// </summary>
  IMACTripleDES = interface(IKeyedHashAlgorithm)
    ['{AD01CC26-2B2F-4895-AA28-A67924DB7EB3}']
  end;
  //*)

  /// <summary>
  /// Abstract base class for hash algorithms.
  /// </summary>
  THashAlgorithmBase = class abstract(TInterfacedObject, IHashAlgorithm, ICryptoTransform, IInterface)
  protected
    fHash: TBuffer;
//    fState: Integer;
  protected
    procedure HashInit; virtual; abstract;
    procedure HashUpdate(const buffer: Pointer; count: Integer); virtual; abstract;
    function HashFinal: TBuffer; virtual; abstract;
    function GetHashSize: Integer; virtual; abstract;
    property Hash: TBuffer read fHash;
  protected
    { ICryptoTransform }
    function GetCanTransformMultipleBlocks: Boolean; virtual;
    function GetCanReuseTransform: Boolean; virtual;
    function GetInputBlockSize: Integer; virtual;
    function GetOutputBlockSize: Integer; virtual;
    function TransformBlock(const inputBuffer: Pointer; inputCount: Integer; outBuffer: Pointer): Integer; virtual;
    function TransformFinalBlock(const inputBuffer: Pointer; inputCount: Integer): TBytes; virtual;
    property CanReuseTransform: Boolean read GetCanReuseTransform;
    property CanTransformMultipleBlocks: Boolean read GetCanTransformMultipleBlocks;
    property InputBlockSize: Integer read GetInputBlockSize;
    property OutputBlockSize: Integer read GetOutputBlockSize;
  public
    function ComputeHash(const buffer: array of Byte): TBuffer; overload;
    function ComputeHash(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function ComputeHash(const buffer: Pointer; count: Integer): TBuffer; overload;
    function ComputeHash(const inputString: string): TBuffer; overload;
    function ComputeHash(const inputString: WideString): TBuffer; overload;
    function ComputeHash(const inputString: RawByteString): TBuffer; overload;
    function ComputeHash(const inputStream: TStream): TBuffer; overload; virtual;
    function ComputeHashOfFile(const fileName: string): TBuffer; virtual;
    property HashSize: Integer read GetHashSize;
  end;

  /// <summary>
  /// CRC16 Hash (CheckSum)
  /// </summary>
  TCRC16 = class(THashAlgorithmBase, ICRC16)
  private
    const fCHashSize = 2;  // 16 bits
  private
    fCRCValue: Word;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  /// <summary>
  /// CRC32 Hash (CheckSum)
  /// </summary>
  TCRC32 = class(THashAlgorithmBase, ICRC32)
  private
    const fCHashSize = 4;  // 32 bits
  private
    fCRCValue: LongWord;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  /// <summary>
  /// MD5 Hash
  /// </summary>
  TMD5 = class(THashAlgorithmBase, IMD5)
  private
    const fCHashSize = 16; // 256 bits
  private
    fContext: TMD5Context;
    fDigest: TMD5Digest;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  /// <summary>
  /// SHA1 Hash
  /// </summary>
  TSHA1 = class(THashAlgorithmBase, ISHA1)
  private
    const fCHashSize = 20;  // 160 bits
  private
    fContext: TSHA256Ctx;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  /// <summary>
  /// SHA256 Hash
  /// </summary>
  TSHA256 = class(THashAlgorithmBase, ISHA256)
  private
    const fCHashSize = 32;  // 256 bits
  private
    fContext: TSHA256Ctx;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  /// <summary>
  /// SHA384 Hash
  /// </summary>
  TSHA384 = class(THashAlgorithmBase, ISHA384)
  private
    const fCHashSize = 48;  // 384 bits
  private
    fContext: TSHA512Ctx;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  /// <summary>
  /// SHA512 Hash
  /// </summary>
  TSHA512 = class(THashAlgorithmBase, ISHA512)
  private
    const fCHashSize = 64;  // 512 bits
  private
    fContext: TSHA512Ctx;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

  {$ENDREGION}


  {$REGION 'Symmetric Algorithms'}

type
  IDES = interface(ISymmetricAlgorithm)
    ['{2123E0C7-A747-49D4-A7CD-A2A9BC1A0042}']
  end;

  ITripleDES = interface(ISymmetricAlgorithm)
    ['{81D5101D-B3EA-437D-8A1A-80E74A9EDCDF}']
  end;

//  IAES = interface(ISymmetricAlgorithm)
//    ['{E5EF09B3-8A6D-432A-87A6-DDB818C59789}']
//  end;

//  TBlockTransformDelegate = reference to procedure(const inputBuffer: TBytes;
//    var outputBuffer: TBytes);

  /// <summary>
  /// Abstract base class for symmetric algorithms.
  /// </summary>
  TSymmetricAlgorithmBase = class abstract(TInterfacedObject, ISymmetricAlgorithm)
  private
    fCipherMode: TCipherMode;
    fPaddingMode: TPaddingMode;
    fLegalBlockSizes: TSizes;
    fLegalKeySizes: TSizes;
    fBlockSize: Integer;
    fKeySize: Integer;
    fKey: TBuffer;
    fIV: TBuffer;
  protected
    function GetBlockSize: Integer; virtual;
    function GetKeySize: Integer; virtual;
    function GetLegalBlockSizes: TSizes; virtual;
    function GetLegalKeySizes: TSizes; virtual;
    function GetCipherMode: TCipherMode; virtual;
    function GetPaddingMode: TPaddingMode; virtual;
    function GetKey: TBuffer; virtual;
    function GetIV: TBuffer; virtual;
    procedure SetBlockSize(const value: Integer); virtual;
    procedure SetKeySize(const value: Integer); virtual;
    procedure SetPaddingMode(const value: TPaddingMode); virtual;
    procedure SetCipherMode(const value: TCipherMode); virtual;
    procedure SetKey(const value: TBuffer); virtual;
    procedure SetIV(const value: TBuffer); virtual;
  protected
    function GenerateIV: TBuffer; virtual;
    function GenerateKey: TBuffer; virtual;
    procedure AddPadding(var buffer: TBuffer; startIndex: Integer; count: Integer);
    procedure RemovePadding(var buffer: TBuffer);
    procedure ValidateKey(const key: TBuffer); virtual;
  protected
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); virtual; abstract;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); virtual; abstract;
  public
    constructor Create(const legalBlockSizes, legalKeySizes: array of Integer);
    function Encrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function Encrypt(const buffer: TBuffer): TBuffer; overload;
    function Encrypt(const buffer: array of Byte): TBuffer; overload;
    function Encrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function Encrypt(const inputString: string): TBuffer; overload;
    function Encrypt(const inputString: WideString): TBuffer; overload;
    function Encrypt(const inputString: RawByteString): TBuffer; overload;
    procedure Encrypt(inputStream, outputStream: TStream); overload;
    function Decrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function Decrypt(const buffer: TBuffer): TBuffer; overload;
    function Decrypt(const buffer: array of Byte): TBuffer; overload;
    function Decrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function Decrypt(const inputString: string): TBuffer; overload;
    function Decrypt(const inputString: WideString): TBuffer; overload;
    function Decrypt(const inputString: RawByteString): TBuffer; overload;
    procedure Decrypt(inputStream, outputStream: TStream); overload;
    property CipherMode: TCipherMode read GetCipherMode write SetCipherMode;
    property PaddingMode: TPaddingMode read GetPaddingMode write SetPaddingMode;
    property Key: TBuffer read GetKey write SetKey;
    property IV: TBuffer read GetIV write SetIV;
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
    property KeySize: Integer read GetKeySize write SetKeySize;
    property LegalBlockSizes: TSizes read GetLegalBlockSizes;
    property LegalKeySizes: TSizes read GetLegalKeySizes;
  end;

  /// <summary>
  /// Data Encryption Standard (DES)
  /// </summary>
  TDES = class(TSymmetricAlgorithmBase, IDES)
  private
    const
      fCDefaultBlockSize = 8;
      fCDefaultKeySize = 8;
  protected
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
  public
    constructor Create;
  end;

  /// <summary>
  /// Triple Data Encryption Standard Algorithm
  /// </summary>
  TTripleDES = class(TSymmetricAlgorithmBase, ITripleDES)
  private
    const
      fCDefaultBlockSize = 8;
      fCDefaultKeySize = 24;
  private
    fKey1: TBytes;
    fKey2: TBytes;
    fKey3: TBytes;
  protected
    procedure SetKey(const value: TBuffer); override;
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
  public
    constructor Create;
  end;

  (*
  /// <summary>
  /// AES Algorithm
  /// </summary>
  TAES = class(TSymmetricAlgorithmBase, IAES)
  end;
  //*)

  {$ENDREGION}


  {$REGION 'Random Number Generator'}

type
  /// <summary>
  /// TRandomNumberGenerator
  /// </summary>
  TRandomNumberGenerator = class(TInterfacedObject, IRandomNumberGenerator)
  public
    procedure GetBytes(var data: TBytes);
    procedure GetNonZeroBytes(var data: TBytes);
  end;

  {$ENDREGION}

implementation

uses
  Math,
  Spring.ResourceStrings;


{$REGION 'TSizes'}

constructor TSizes.Create(size: Integer);
begin
  Create([size]);
end;

constructor TSizes.Create(const sizes: array of Integer);
begin
  SetLength(fValues, Length(sizes));
  Move(sizes[0], fValues[0], Length(sizes) * SizeOf(Integer));
end;

function TSizes.Contains(const size: Integer): Boolean;
var
  value: Integer;
begin
  Result := False;
  for value in fValues do
  begin
    if value = size then
      Exit(True);
  end;
end;

{$ENDREGION}


{$REGION 'THashAlgorithmBase'}

function THashAlgorithmBase.ComputeHash(const buffer: Pointer;
  count: Integer): TBuffer;
begin
  HashInit;
  HashUpdate(buffer, count);
  fHash := HashFinal;
  Result := fHash;
end;

function THashAlgorithmBase.ComputeHash(const buffer: array of Byte): TBuffer;
begin
  Result := ComputeHash(@buffer[0], Length(buffer));
end;

function THashAlgorithmBase.ComputeHash(const buffer: array of Byte; startIndex,
  count: Integer): TBuffer;
begin
  TArgument.CheckRange(buffer, startIndex, count);
  Result := ComputeHash(@buffer[startIndex], count);
end;

function THashAlgorithmBase.ComputeHash(const inputString: string): TBuffer;
begin
  Result := ComputeHash(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

function THashAlgorithmBase.ComputeHash(const inputString: WideString): TBuffer;
begin
  Result := ComputeHash(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

function THashAlgorithmBase.ComputeHash(
  const inputString: RawByteString): TBuffer;
begin
  Result := ComputeHash(PByte(inputString), Length(inputString));
end;

function THashAlgorithmBase.ComputeHash(const inputStream: TStream): TBuffer;
var
  buffer: array[0..512-1] of Byte;
  count: Integer;
begin
  TArgument.CheckNotNull(inputStream, 'inputStream');
  HashInit;
  count := inputStream.Read(buffer[0], Length(buffer));
  while count > 0 do
  begin
    HashUpdate(@buffer[0], count);
    count := inputStream.Read(buffer[0], Length(buffer));
  end;
  fHash := HashFinal;
  Result := fHash;
end;

function THashAlgorithmBase.ComputeHashOfFile(const fileName: string): TBuffer;
var
  stream: TStream;
begin
  stream := TFileStream.Create(fileName, fmOpenRead or fmShareExclusive);
  try
    Result := ComputeHash(stream);
  finally
    stream.Free;
  end;
end;

function THashAlgorithmBase.TransformBlock(const inputBuffer: Pointer;
  inputCount: Integer; outBuffer: Pointer): Integer;
begin
  HashUpdate(inputBuffer, inputCount);
  CopyMemory(outBuffer, inputBuffer, inputCount);
  Result := inputCount;
end;

function THashAlgorithmBase.TransformFinalBlock(const inputBuffer: Pointer;
  inputCount: Integer): TBytes;
begin
  HashUpdate(inputBuffer, inputCount);
  Result := HashFinal;
end;

function THashAlgorithmBase.GetCanReuseTransform: Boolean;
begin
  Result := True;
end;

function THashAlgorithmBase.GetCanTransformMultipleBlocks: Boolean;
begin
  Result := True;
end;

function THashAlgorithmBase.GetInputBlockSize: Integer;
begin
  Result := 1;
end;

function THashAlgorithmBase.GetOutputBlockSize: Integer;
begin
  Result := 1;
end;

{$ENDREGION}


{$REGION 'TCRC16'}

function TCRC16.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TCRC16.HashInit;
begin
  CRC16Init(fCRCValue);
end;

procedure TCRC16.HashUpdate(const buffer: Pointer; count: Integer);
begin
  CRC16Update(fCRCValue, buffer, count);
end;

function TCRC16.HashFinal: TBuffer;
begin
  Result := CRC16Final(fCRCValue);
end;

{$ENDREGION}


{$REGION 'TCRC32'}

function TCRC32.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TCRC32.HashInit;
begin
  CRC32Init(fCRCValue);
end;

procedure TCRC32.HashUpdate(const buffer: Pointer; count: Integer);
begin
  CRC32BUpdate(fCRCValue, buffer, count);
end;

function TCRC32.HashFinal: TBuffer;
begin
  Result := CRC32Final(fCRCValue);
end;

{$ENDREGION}


{$REGION 'TMD5'}

function TMD5.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TMD5.HashUpdate(const buffer: Pointer; count: Integer);
begin
  MD5Update(fContext, buffer, count);
end;

function TMD5.HashFinal: TBuffer;
begin
  MD5Final(fContext, fDigest);
  Result := TBuffer.Create(fDigest);
end;

procedure TMD5.HashInit;
var
  i: Byte;
begin
  for i := 0 to High(fDigest) do
  begin
    fDigest[i] := i + 1;
  end;
  MD5Init(fContext);
end;

{$ENDREGION}


{$REGION 'TSHA1'}

function TSHA1.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TSHA1.HashInit;
begin
  SHA1Init(fContext);
end;

procedure TSHA1.HashUpdate(const buffer: Pointer; count: Integer);
begin
  SHA256Update(fContext, buffer, count, 1);
end;

function TSHA1.HashFinal: TBuffer;
begin
  Result := SHA256Final(fContext, 1);
end;

{$ENDREGION}


{$REGION 'TSHA256'}

function TSHA256.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TSHA256.HashInit;
begin
  SHA256Init(fContext);
end;

procedure TSHA256.HashUpdate(const buffer: Pointer; count: Integer);
begin
  SHA256Update(fContext, buffer, count, 256);
end;

function TSHA256.HashFinal: TBuffer;
begin
  Result := SHA256Final(fContext, 256);
end;

{$ENDREGION}


{$REGION 'TSHA384'}

function TSHA384.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TSHA384.HashInit;
begin
  SHA384Init(fContext);
end;

procedure TSHA384.HashUpdate(const buffer: Pointer; count: Integer);
begin
  SHA512Update(fContext, buffer, count);
end;

function TSHA384.HashFinal: TBuffer;
begin
  Result := SHA512Final(fContext, 384);
end;

{$ENDREGION}


{$REGION 'TSHA512'}

function TSHA512.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TSHA512.HashInit;
begin
  SHA512Init(fContext);
end;

procedure TSHA512.HashUpdate(const buffer: Pointer; count: Integer);
begin
  SHA512Update(fContext, buffer, count);
end;

function TSHA512.HashFinal: TBuffer;
begin
  Result := SHA512Final(fContext, 512);
end;

{$ENDREGION}


{$REGION 'TSymmetricAlgorithmBase'}

constructor TSymmetricAlgorithmBase.Create(
  const legalBlockSizes, legalKeySizes: array of Integer);
begin
  inherited Create;
  fLegalBlockSizes := TSizes.Create(legalBlockSizes);
  fLegalKeySizes := TSizes.Create(legalKeySizes);
  fCipherMode := cmCBC;
  fPaddingMode := pmPKCS7;
end;

procedure TSymmetricAlgorithmBase.ValidateKey(const key: TBuffer);
begin
  if not LegalKeySizes.Contains(key.Size) then
  begin
    raise ECryptographicException.CreateResFmt(@SIllegalKeySize, [key.Size]);
  end;
end;

function TSymmetricAlgorithmBase.Encrypt(const buffer: TBuffer): TBuffer;
begin
  Result := Encrypt(buffer.Memory, buffer.Size);
end;

function TSymmetricAlgorithmBase.Encrypt(const buffer: array of Byte): TBuffer;
begin
  Result := Encrypt(@buffer[0], Length(buffer));
end;

function TSymmetricAlgorithmBase.Encrypt(const buffer: array of Byte;
  startIndex, count: Integer): TBuffer;
begin
  TArgument.CheckRange(buffer, startIndex, count);
  Result := Encrypt(@buffer[startIndex], count);
end;

function TSymmetricAlgorithmBase.Encrypt(const buffer: Pointer;
  count: Integer): TBuffer;
var
  p: PByte;
  plainText: TBuffer;
  cipherText: TBytes;
  paddingSize: Integer;
  startIndex: Integer;
  firstBlock: Boolean;
begin
  TArgument.CheckRange(count >= 0, 'count');
  if count = 0 then
  begin
    Exit(TBuffer.Empty);
  end;
  p := buffer;
  plainText.Size := BlockSize;
  SetLength(cipherText, BlockSize);
  firstBlock := True;
  while count >= 0 do
  begin
    if count >= BlockSize then
    begin
      Move(p^, plainText.Memory^, BlockSize);
    end
    else if PaddingMode <> pmNone then
    begin
      Move(p^, plainText.Memory^, count);
      paddingSize := BlockSize - (count mod BlockSize);
      startIndex := BlockSize - paddingSize;
      AddPadding(plainText, startIndex, paddingSize);
    end
    else if count > 0 then
    begin
      raise ECryptographicException.CreateRes(@SPaddingModeMissing);
    end
    else
    begin
      Exit;
    end;
    if CipherMode = cmCBC then
    begin
      if firstBlock then
        plainText := plainText xor IV
      else
        plainText := plainText xor cipherText;
      firstBlock := False;
    end;
    DoEncryptBlock(plainText, cipherText);
    Result := Result + cipherText;
    Dec(count, BlockSize);
    Inc(p, BlockSize);
  end;
end;

function TSymmetricAlgorithmBase.Encrypt(const inputString: string): TBuffer;
begin
  Result := Encrypt(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

function TSymmetricAlgorithmBase.Encrypt(
  const inputString: WideString): TBuffer;
begin
  Result := Encrypt(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

function TSymmetricAlgorithmBase.Encrypt(
  const inputString: RawByteString): TBuffer;
begin
  Result := Encrypt(PByte(inputString), Length(inputString));
end;

procedure TSymmetricAlgorithmBase.Encrypt(inputStream, outputStream: TStream);
var
  inputBuffer: TBuffer;
  outputBuffer: TBuffer;
  bytes: Integer;
begin
  TArgument.CheckNotNull(inputStream, 'inputStream');
  TArgument.CheckNotNull(outputStream, 'outputStream');
  inputBuffer.Size := BlockSize;
  outputBuffer.Size := BlockSize;
  bytes := inputStream.Read(inputBuffer.Memory^, inputBuffer.Size);
  while bytes > 0 do
  begin
    outputBuffer := Encrypt(inputBuffer);
    outputStream.WriteBuffer(outputBuffer.Memory^, outputBuffer.Size);
    bytes := inputStream.Read(inputBuffer.Memory^, inputBuffer.Size);
  end;
end;

procedure TSymmetricAlgorithmBase.AddPadding(var buffer: TBuffer; startIndex,
  count: Integer);
var
  i: Integer;
begin
  TArgument.CheckRange(buffer.Size, startIndex, count);
  case PaddingMode of
    pmNone: ;
    pmPKCS7:
    begin
      for i := 0 to count - 1 do
      begin
        buffer[startIndex + i] := Byte(count);
      end;
    end;
    pmZeros:
    begin
      for i := 0 to count - 1 do
      begin
        buffer[startIndex + i] := 0;
      end;
    end;
    pmANSIX923:
    begin
      for i := 0 to count - 2 do
      begin
        buffer[startIndex + i] := 0;
      end;
      buffer[startIndex + count - 1] := Byte(count);
    end;
    pmISO10126:
    begin
      Randomize;
      for i := 0 to count - 2 do
      begin
        buffer[startIndex + i] := RandomRange(0, 256);
      end;
      buffer[startIndex + count - 1] := Byte(count);
    end;
  end;
end;

procedure TSymmetricAlgorithmBase.RemovePadding(var buffer: TBuffer);
var
  paddingSize: Integer;
  count: Integer;
  i: Integer;
begin
  Assert(buffer.Size = BlockSize);
  case PaddingMode of
    pmNone: ;
    pmPKCS7, pmANSIX923, pmISO10126:
    begin
      paddingSize := Integer(buffer.Last);
      if paddingSize = BlockSize then
      begin
        // Validate
        buffer := TBuffer.Empty;
      end
      else if paddingSize < BlockSize then
      begin
        count := BlockSize - paddingSize;
        buffer := buffer.Left(count);
      end
      else
      begin
        raise ECryptographicException.CreateRes(@SInvalidCipherText);
      end;
    end;
    pmZeros:
    begin
      for i := buffer.Size - 1 downto 0 do
      begin
        if buffer[i] = 0 then
        begin
          buffer.Size := buffer.Size - 1;
        end;
      end;
    end;
  end;
end;

function TSymmetricAlgorithmBase.Decrypt(const buffer: Pointer;
  count: Integer): TBuffer;
var
  inputBuffer, plainText: TBuffer;
  outputBuffer: TBytes;
  lastCipherText: TBuffer;
  p: PByte;
  firstBlock: Boolean;
begin
  TArgument.CheckRange(count >= 0, 'count');

  firstBlock := True;
  Result := TBuffer.Empty;
  p := buffer;
  inputBuffer.Size := BlockSize;
  plainText.Size := BlockSize;
  SetLength(outputBuffer, BlockSize);
  while count >= BlockSize do
  begin
    inputBuffer := TBuffer.Create(p, BlockSize);
    DoDecryptBlock(inputBuffer, outputBuffer);
    if CipherMode = cmCBC then
    begin
      if firstBlock then
      begin
        plainText := outputBuffer xor IV;
        firstBlock := False;
      end
      else
      begin
        plainText := outputBuffer xor lastCipherText;
      end;
      lastCipherText := inputBuffer.Clone;
    end
    else
    begin
      plainText := outputBuffer;
    end;
    if count = BlockSize then // FinalBlock
    begin
      RemovePadding(plainText);
    end;
    Result := Result + plainText;
    Dec(count, BlockSize);
    Inc(p, BlockSize);
  end;
  if count > 0 then
  begin
    raise ECryptographicException.CreateRes(@SInvalidCipherText);
  end;
end;

function TSymmetricAlgorithmBase.Decrypt(const buffer: TBuffer): TBuffer;
begin
  Result := Decrypt(buffer.Memory, buffer.Size);
end;

function TSymmetricAlgorithmBase.Decrypt(const buffer: array of Byte): TBuffer;
begin
  Result := Decrypt(@buffer[0], Length(buffer));
end;

function TSymmetricAlgorithmBase.Decrypt(const buffer: array of Byte;
  startIndex, count: Integer): TBuffer;
begin
  TArgument.CheckRange(buffer, startIndex, count);
  Result := Decrypt(@buffer[startIndex], count);
end;

function TSymmetricAlgorithmBase.Decrypt(const inputString: string): TBuffer;
begin
  Result := Decrypt(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

function TSymmetricAlgorithmBase.Decrypt(
  const inputString: WideString): TBuffer;
begin
  Result := Decrypt(PByte(inputString), Length(inputString) * SizeOf(Char));
end;

function TSymmetricAlgorithmBase.Decrypt(
  const inputString: RawByteString): TBuffer;
begin
  Result := Decrypt(PByte(inputString), Length(inputString));
end;

procedure TSymmetricAlgorithmBase.Decrypt(inputStream, outputStream: TStream);
var
  buffer: TBytes;
  count: Integer;
  outputBuffer: TBuffer;
begin
  TArgument.CheckNotNull(inputStream, 'inputStream');
  TArgument.CheckNotNull(outputStream, 'outputStream');
  SetLength(buffer, BlockSize);
  count := inputStream.Read(buffer[0], Length(buffer));
  while count >= BlockSize do
  begin
    outputBuffer := Decrypt(buffer);
    outputBuffer.SaveToStream(outputStream);
    count := inputStream.Read(buffer[0], Length(buffer));
  end;
  if count > 0 then
  begin
    raise ECryptographicException.CreateRes(@SInvalidCipherText);
  end;
end;

function TSymmetricAlgorithmBase.GetCipherMode: TCipherMode;
begin
  Result := fCipherMode;
end;

function TSymmetricAlgorithmBase.GetPaddingMode: TPaddingMode;
begin
  Result := fPaddingMode;
end;

function TSymmetricAlgorithmBase.GetIV: TBuffer;
begin
  if fIV.IsEmpty then
  begin
    fIV := GenerateIV;
  end;
  Result := fIV;
end;

function TSymmetricAlgorithmBase.GetKey: TBuffer;
begin
  if fKey.IsEmpty then
  begin
    fKey := GenerateKey;
  end;
  Result := fKey;
end;

function TSymmetricAlgorithmBase.GenerateIV: TBuffer;
var
  generator: IRandomNumberGenerator;
  buffer: TBytes;
begin
  generator := TRandomNumberGenerator.Create;
  SetLength(buffer, BlockSize);
  generator.GetBytes(buffer);
  Result := buffer;
end;

function TSymmetricAlgorithmBase.GenerateKey: TBuffer;
var
  generator: IRandomNumberGenerator;
  buffer: TBytes;
begin
  generator := TRandomNumberGenerator.Create;
  SetLength(buffer, KeySize);
  generator.GetBytes(buffer);
  Result := buffer;
end;

function TSymmetricAlgorithmBase.GetBlockSize: Integer;
begin
  Result := fBlockSize;
end;

function TSymmetricAlgorithmBase.GetKeySize: Integer;
begin
  Result := fKeySize;
end;

function TSymmetricAlgorithmBase.GetLegalBlockSizes: TSizes;
begin
  Result := fLegalBlockSizes;
end;

function TSymmetricAlgorithmBase.GetLegalKeySizes: TSizes;
begin
  Result := fLegalKeySizes;
end;

procedure TSymmetricAlgorithmBase.SetBlockSize(const value: Integer);
begin
  if not fLegalBlockSizes.Contains(value) then
  begin
    raise ECryptographicException.CreateResFmt(@SIllegalBlockSize, [value]);
  end;
  fBlockSize := value;
end;

procedure TSymmetricAlgorithmBase.SetKeySize(const value: Integer);
begin
  if not fLegalKeySizes.Contains(value) then
  begin
    raise ECryptographicException.CreateResFmt(@SIllegalKeySize, [value]);
  end;
  fKeySize := value;
end;

procedure TSymmetricAlgorithmBase.SetCipherMode(const value: TCipherMode);
begin
  fCipherMode := value;
end;

procedure TSymmetricAlgorithmBase.SetPaddingMode(const value: TPaddingMode);
begin
  fPaddingMode := value;
end;

procedure TSymmetricAlgorithmBase.SetIV(const value: TBuffer);
begin
  if value.Size <> BlockSize then
  begin
    raise ECryptographicException.CreateResFmt(@SIllegalIVSize, [value.Size]);
  end;
  fIV := value;
end;

procedure TSymmetricAlgorithmBase.SetKey(const value: TBuffer);
begin
  ValidateKey(value);
  fKey := value.Clone;
end;

{$ENDREGION}


{$REGION 'TDES'}

constructor TDES.Create;
begin
  inherited Create([8], [8]);
  BlockSize := fCDefaultBlockSize;
  KeySize := fCDefaultKeySize;
end;

procedure TDES.DoEncryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
begin
  EncryptData(fKey.AsBytes, inputBuffer, outputBuffer);
end;

procedure TDES.DoDecryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
begin
  DecryptData(fKey.AsBytes, inputBuffer, outputBuffer);
end;

{$ENDREGION}


{$REGION 'TTripleDES'}

constructor TTripleDES.Create;
begin
  inherited Create([8], [16, 24]);
  BlockSize := fCDefaultBlockSize;
  KeySize := fCDefaultKeySize;
end;

procedure TTripleDES.DoDecryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
var
  temp1, temp2: TBytes;
begin
  SetLength(temp1, BlockSize);
  SetLength(temp2, BlockSize);
  DecryptData(fKey3, inputBuffer, temp1);
  EncryptData(fKey2, temp1, temp2);
  DecryptData(fKey1, temp2, outputBuffer);
end;

procedure TTripleDES.DoEncryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
var
  temp1, temp2: TBytes;
begin
  SetLength(temp1, BlockSize);
  SetLength(temp2, BlockSize);
  EncryptData(fKey1, inputBuffer, temp1);
  DecryptData(fKey2, temp1, temp2);
  EncryptData(fKey3, temp2, outputBuffer);
end;

procedure TTripleDES.SetKey(const value: TBuffer);
begin
  inherited SetKey(value);
  fKey1 := Key.Left(8);
  fKey2 := Key.Mid(8, 8);
  if Key.Size = 16 then
    fKey3 := fKey1
  else if Key.Size = 24 then
    fKey3 := Key.Right(8)
  else
    raise ECryptographicException.CreateResFmt(@SIllegalKeySize, [value.Size]);
end;

{$ENDREGION}


{$REGION 'TRandomNumberGenerator'}

procedure TRandomNumberGenerator.GetBytes(var data: TBytes);
var
  i: Integer;
begin
  Randomize;
  for i := 0 to High(data) do
  begin
    data[i] := RandomRange(0, $FF + 1);
  end;
end;

procedure TRandomNumberGenerator.GetNonZeroBytes(var data: TBytes);
var
  i: Integer;
begin
  Randomize;
  for i := 0 to High(data) do
  begin
    data[i] := RandomRange(1, $FF + 1);
  end;
end;

{$ENDREGION}

end.
