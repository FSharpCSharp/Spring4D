{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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

{TODO -oPaul -cGeneral : Support More CipherModes (cmOFB, cmCFB and cmCTS)}
{TODO -oPaul -cGeneral : Add TAES}
{TODO -oPaul -cGeneral : Add TMACTripleDES}

/// <preliminary />
/// <seealso href="http://msdn.microsoft.com/en-us/library/92f9ye3s(VS.71).aspx"></seealso>
/// <seealso href="http://msdn.microsoft.com/en-us/library/system.security.cryptography.aspx"></seealso>
/// <seealso href="http://en.wikipedia.org/wiki/Cryptography"></seealso>
unit Spring.Cryptography;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Spring;

type
  {$IFDEF SUPPORTS_REGION}{$REGION 'Core Types'}{$ENDIF}

  {$REGION 'Documentation'}
  ///	<summary>TCipherMode</summary>
  ///	<remarks>The cipher modes cmOFB, cmCFB and cmCTS have not been supported
  ///	yet.</remarks>
  ///	<seealso href=
  ///	"http://en.wikipedia.org/wiki/Block_cipher_mode_of_operation">Block
  ///	Cipher Mode of Operation</seealso>
  {$ENDREGION}
  TCipherMode = (
    ///	<summary>Cipher-Block Chaining</summary>
    cmCBC,

    ///	<summary>Electronic Codebook</summary>
    cmECB,

    ///	<summary>Output Feedback (<b>Not yet implemented</b>)</summary>
    cmOFB,

    ///	<summary>Cipher Feedback (<b>Not yet implemented</b>)</summary>
    cmCFB,

    ///	<summary>Cipher Text Stealing (<b>Not yet implemented</b>)</summary>
    cmCTS
  );

  /// <summary>
  /// Specifies the type of padding to apply when the message data block is
  /// shorter than the full number of bytes needed for a cryptographic operation.
  /// </summary>
  /// <seealso href="http://msdn.microsoft.com/en-us/library/system.security.cryptography.paddingmode.aspx"></seealso>
  /// <seealso href="http://en.wikipedia.org/wiki/Padding_(cryptography)"></seealso>
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
  /// Represents a list of size.
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

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents a series of bytes in memory.
  ///	</summary>
  ///	<remarks>
  ///	  The <c>TBuffer</c> structure is actually a wrapper of a value of <c>TByt
  ///	  es</c> while provideing some easy-going methods and properties.
  ///	  <note type="warning">
  ///	    This type needs to be reviewed.
  ///	  </note>
  ///	</remarks>
  {$ENDREGION}
  TBuffer = record
  strict private
    fBytes: TBytes;
    function GetIsEmpty: Boolean;
    function GetMemory: PByte; inline;
    function GetSize: Integer; inline;
    function GetByteItem(const index: Integer): Byte;
    procedure SetSize(const value: Integer);
    procedure SetByteItem(const index: Integer; const value: Byte);
  private
    class var fEmpty: TBuffer;
  public
    constructor Create(size: Integer); overload;
    constructor Create(const buffer: Pointer; count: Integer); overload;
    constructor Create(const buffer: Pointer; startIndex, count: Integer); overload;
    constructor Create(const buffer: array of Byte); overload;
    constructor Create(const buffer: array of Byte; startIndex, count: Integer); overload;
//    constructor Create(const buffer: array of Char); overload;
//    constructor Create(const buffer: array of Char; startIndex, count: Integer); overload;
    constructor Create(const s: string); overload;
    constructor Create(const s: WideString); overload;
    constructor Create(const s: RawByteString); overload;

    class function FromHexString(const s: string): TBuffer; static;
    ///	<seealso cref="FromHexString(string)"></seealso>
    class function ConvertToHexString(const buffer: Pointer; count: Integer): string; overload; static;
    class function ConvertToHexString(const buffer: Pointer; count: Integer;
      const prefix: string; const delimiter: string = ' '): string; overload; static;

    class function BytesOf(const value: Byte; count: Integer): TBytes; static;
    class function GetByte(const buffer; const index: Integer): Byte; static;
    class procedure SetByte(var buffer; const index: Integer; const value: Byte); static;

//    procedure CopyTo(var dest: array of Byte; index, count: Integer);

    function Clone: TBuffer;
    function Copy(startIndex, count: Integer): TBytes;
    function Reverse: TBuffer; // experimental;

    function Left(count: Integer): TBuffer;
    function Mid(startIndex, count: Integer): TBuffer;
    function Right(count: Integer): TBuffer;

    function First: Byte;
    function Last: Byte;

    function EnsureSize(size: Integer): TBuffer; overload;
    function EnsureSize(size: Integer; value: Byte): TBuffer; overload;
    function EnsureSize(size: Integer; value: AnsiChar): TBuffer; overload;

    function Equals(const buffer: TBuffer): Boolean; overload;
    function Equals(const buffer: array of Byte): Boolean; overload;
    function Equals(const buffer: Pointer; count: Integer): Boolean; overload;
    function Equals(const hexString: string): Boolean; overload;

    procedure SaveToStream(stream: TStream);

    function ToBytes: TBytes;
    function ToString: string; experimental;
    function ToWideString: WideString; experimental;  // deprecated;
    function ToAnsiString: RawByteString; experimental; // deprecated;
    function ToUtf8String: UTF8String; experimental; // deprecated;

    function ToHexString: string; overload;
    function ToHexString(const prefix: string; const delimiter: string = ' '): string; overload;

    property AsBytes: TBytes read fBytes;
    property IsEmpty: Boolean read GetIsEmpty;
    property Memory: PByte read GetMemory;
    property Size: Integer read GetSize write SetSize;
    property Bytes[const index: Integer]: Byte read GetByteItem write SetByteItem; default;

    class property Empty: TBuffer read fEmpty;

    { Operator Overloads }
    class operator Implicit(const value: TBytes): TBuffer;
    class operator Implicit(const value: TBuffer): TBytes;
    class operator Implicit(const value: TBuffer): PByte;
    class operator Explicit(const value: TBytes): TBuffer;
    class operator Explicit(const value: TBuffer): TBytes;
    class operator Explicit(const value: TBuffer): PByte;
    class operator Add(const left, right: TBuffer): TBuffer;
    class operator Add(const left: TBuffer; const right: Byte): TBuffer; overload;
    class operator Add(const left: Byte; const right: TBuffer): TBuffer; overload;
    class operator Equal(const left, right: TBuffer): Boolean;
    class operator NotEqual(const left, right: TBuffer): Boolean;
//    class operator BitwiseAnd(const left, right: TBuffer): TBuffer;
//    class operator BitwiseOr(const left, right: TBuffer): TBuffer;
    class operator BitwiseXor(const left, right: TBuffer): TBuffer;
  end;

  /// <summary>
  /// Defines the basic operations of hash algorithms.
  /// </summary>
  IHashAlgorithm = interface
    ['{D33C6DB1-7C51-4DDE-BB7D-ACE98BB61EBE}']
  {$IFDEF SUPPORTS_REGION}{$REGION 'Property Getters and Setters'}{$ENDIF}
    function GetHashSize: Integer;
  {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}
    function ComputeHash(const buffer: array of Byte): TBuffer; overload;
    function ComputeHash(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function ComputeHash(const buffer: Pointer; count: Integer): TBuffer; overload;
    function ComputeHash(const inputString: string): TBuffer; overload;
    function ComputeHash(const inputString: WideString): TBuffer; overload;
    function ComputeHash(const inputString: RawByteString): TBuffer; overload;
    function ComputeHash(const inputStream: TStream): TBuffer; overload;  // experimental
    function ComputeHashOfFile(const fileName: string): TBuffer;  // callback?
    /// <summary>
    /// Gets the hash size, <b>in bits</b>, of the algorithm.
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
  {$IFDEF SUPPORTS_REGION}{$REGION 'Property Getters and Setters'}{$ENDIF}
    function GetKey: TBuffer;
    procedure SetKey(const value: TBuffer);
  {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}
    /// <summary>
    /// Gets or sets the key to use in the hash algorithm.
    /// </summary>
    property Key: TBuffer read GetKey write SetKey;
  end;

  /// <summary>
  /// Performs a transformation on data to keep it from being read by third parties.
  /// This type of encryption uses a single shared, secret key to encrypt and decrypt data.
  /// </summary>
  /// <seealso href="http://en.wikipedia.org/wiki/Cipher"></seealso>
  /// <seealso href="http://en.wikipedia.org/wiki/Block_cipher_mode_of_operation"></seealso>
  ISymmetricAlgorithm = interface
    ['{98E0E218-2BD4-4AFA-87B2-E8C4812B2105}']
  {$IFDEF SUPPORTS_REGION}{$REGION 'Property Getters and Setters'}{$ENDIF}
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
  {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}

    function  Encrypt(const buffer: TBuffer): TBuffer; overload;
    function  Encrypt(const buffer: array of Byte): TBuffer; overload;
    function  Encrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function  Encrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function  Encrypt(const inputString: string): TBuffer; overload;
    function  Encrypt(const inputString: WideString): TBuffer; overload;
    function  Encrypt(const inputString: RawByteString): TBuffer; overload;
    procedure Encrypt(inputStream, outputStream: TStream); overload;

    function  Decrypt(const buffer: TBuffer): TBuffer; overload;
    function  Decrypt(const buffer: array of Byte): TBuffer; overload;
    function  Decrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function  Decrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function  Decrypt(const inputString: string): TBuffer; overload;
    function  Decrypt(const inputString: WideString): TBuffer; overload;
    function  Decrypt(const inputString: RawByteString): TBuffer; overload;
    procedure Decrypt(inputStream, outputStream: TStream); overload;

    /// <summary>
    /// Gets or sets the cipher mode for operation of the symmetric algorithm.
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
    /// Gets or sets the block size, in bits, of the cryptographic operation.
    /// </summary>
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
    /// <summary>
    /// Gets or sets the size, in bits, of the secret key used by the symmetric algorithm.
    /// </summary>
    property KeySize: Integer read GetKeySize write SetKeySize;
    /// <summary>
    /// Gets the block sizes, in bits, that are supported by the symmetric algorithm.
    /// </summary>
    property LegalBlockSizes: TSizes read GetLegalBlockSizes;
    /// <summary>
    /// Gets the key sizes, in bits, that are supported by the symmetric algorithm.
    /// </summary>
    property LegalKeySizes: TSizes read GetLegalKeySizes;
  end;

  /// <summary>
  /// Generates cryptographically strong random values.
  /// </summary>
  IRandomNumberGenerator = interface
    ['{64B180B9-E192-4542-A45D-4E7402ED7BA8}']
    procedure GetBytes(var data: TBytes);
    procedure GetNonZeroBytes(var data: TBytes);
  end;

  /// <summary>
  /// Abstract base class for hash algorithms.
  /// </summary>
  THashAlgorithmBase = class abstract(TInterfacedObject, IHashAlgorithm, IInterface)
  protected
    fHash: TBuffer;
//    fState: Integer;
  protected
    procedure HashInit; virtual; abstract;
    procedure HashUpdate(const buffer: Pointer; count: Integer); virtual; abstract;
    function HashFinal: TBuffer; virtual; abstract;
    function GetHashSize: Integer; virtual; abstract;
    property Hash: TBuffer read fHash;
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

  {TODO -oPaul -cGeneral : Refactoring: EncryptBlock/EncryptFinalBlock/Decrypt***}
  {TODO 5 -oPaul -cGeneral : BUG FIXES: TSymmetricAlgorithmBase.Encrypt/Decrypt(inputStream, outputStream)}

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
    function GetBlockSizeInBytes: Integer;
    function GetKeySizeInBytes: Integer;
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
    procedure AddPadding(var buffer: TBuffer; startIndex: Integer; count: Integer);
    procedure RemovePadding(var buffer: TBuffer);
    procedure ValidateKey(const key: TBuffer); virtual;
    function GenerateIV: TBuffer; virtual;
    function GenerateKey: TBuffer; virtual;
    property BlockSizeInBytes: Integer read GetBlockSizeInBytes;
    property KeySizeInBytes: Integer read GetKeySizeInBytes;
  protected
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); virtual; abstract;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); virtual; abstract;
    function DoEncrypt(const buffer: Pointer; count: Integer): TBuffer; virtual;
    function DoDecrypt(const buffer: Pointer; count: Integer): TBuffer; virtual;
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
  /// TRandomNumberGenerator
  /// </summary>
  TRandomNumberGenerator = class(TInterfacedObject, IRandomNumberGenerator)
  public
    procedure GetBytes(var data: TBytes);
    procedure GetNonZeroBytes(var data: TBytes);
  end;

  {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


  {$IFDEF SUPPORTS_REGION}{$REGION 'Hash Algorithms'}{$ENDIF}

  ICRC16 = interface(IHashAlgorithm)
    ['{7FEC815E-52E3-4C48-AAC1-7DEE905A6C1F}']
    function GetCrcValue: UInt16;
    property CrcValue: UInt16 read GetCrcValue;
  end;

  ICRC32 = interface(IHashAlgorithm)
    ['{96581075-EC4C-4C3F-A031-88FCD4D9F3EA}']
    function GetCrcValue: UInt32;
    property CrcValue: UInt32 read GetCrcValue;
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

  /// <summary>
  /// Message Authentication Code
  /// </summary>
  IMACTripleDES = interface(IKeyedHashAlgorithm)
    ['{AD01CC26-2B2F-4895-AA28-A67924DB7EB3}']
  end;

  {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


  {$IFDEF SUPPORTS_REGION}{$REGION 'Symmetric Algorithms'}{$ENDIF}

  IDES = interface(ISymmetricAlgorithm)
    ['{2123E0C7-A747-49D4-A7CD-A2A9BC1A0042}']
  end;

  ITripleDES = interface(ISymmetricAlgorithm)
    ['{81D5101D-B3EA-437D-8A1A-80E74A9EDCDF}']
  end;

  IAES = interface(ISymmetricAlgorithm)
    ['{E5EF09B3-8A6D-432A-87A6-DDB818C59789}']
  end;

  {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}



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
  (*
  ICryptographicServiceProvider = interface
    ['{CA9D2D5A-8562-4F9E-8CDC-397C4E0B14D6}']

    function CreateCRC16: ICRC16;

    function CreateCRC32: ICRC32;

    function CreateMD5: IMD5;

    function CreateSHA1: ISHA1;

    function CreateSHA256: ISHA256;

    function CreateSHA384: ISHA384;

    function CreateSHA512: ISHA512;

    function CreateDES: IDES;

    function CreateTripleDES: ITripleDES;

    function CreateRandomNumberGenerator: IRandomNumberGenerator;
  end;
  //*)

  {$REGION 'Documentation'}
  ///	<summary>Provides static methods to create various cryptographic
  ///	algorithms.</summary>
  {$ENDREGION}
  TCryptographicServiceProvider = class
  public
    class function CreateCRC16: ICRC16; static;
    class function CreateCRC32: ICRC32; static;
    class function CreateMD5: IMD5; static;
    class function CreateSHA1: ISHA1; static;
    class function CreateSHA256: ISHA256; static;
    class function CreateSHA384: ISHA384; static;
    class function CreateSHA512: ISHA512; static;
    class function CreateDES: IDES; static;
    class function CreateTripleDES: ITripleDES; static;
    class function CreateRandomNumberGenerator: IRandomNumberGenerator; static;
  end;

  TCryptoServiceProvider = TCryptographicServiceProvider;

  ECryptographicException = class(Exception);


{$IFDEF SUPPORTS_REGION}{$REGION 'Routines'}{$ENDIF}

function CreateCRC16: ICRC16;

function CreateCRC32: ICRC32;

function CreateMD5: IMD5;

function CreateSHA1: ISHA1;

function CreateSHA256: ISHA256;

function CreateSHA384: ISHA384;

function CreateSHA512: ISHA512;

function CreateDES: IDES;

function CreateTripleDES: ITripleDES;

function CreateRandomNumberGenerator: IRandomNumberGenerator;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}

implementation

uses
  Math,
  Spring.Utils,
  Spring.Cryptography.CRC,
  Spring.Cryptography.MD5,
  Spring.Cryptography.SHA,
  Spring.Cryptography.DES,
  Spring.ResourceStrings;


{$IFDEF SUPPORTS_REGION}{$REGION 'Routines'}{$ENDIF}

function CreateCRC16: ICRC16;
begin
  Result := TCryptographicServiceProvider.CreateCRC16;
end;

function CreateCRC32: ICRC32;
begin
  Result := TCryptographicServiceProvider.CreateCRC32;
end;

function CreateMD5: IMD5;
begin
  Result := TCryptographicServiceProvider.CreateMD5;
end;

function CreateSHA1: ISHA1;
begin
  Result := TCryptographicServiceProvider.CreateSHA1;
end;

function CreateSHA256: ISHA256;
begin
  Result := TCryptographicServiceProvider.CreateSHA256;
end;

function CreateSHA384: ISHA384;
begin
  Result := TCryptographicServiceProvider.CreateSHA384;
end;

function CreateSHA512: ISHA512;
begin
  Result := TCryptographicServiceProvider.CreateSHA512;
end;

function CreateDES: IDES;
begin
  Result := TCryptographicServiceProvider.CreateDES;
end;

function CreateTripleDES: ITripleDES;
begin
  Result := TCryptographicServiceProvider.CreateTripleDES;
end;

function CreateRandomNumberGenerator: IRandomNumberGenerator;
begin
  Result := TCryptographicServiceProvider.CreateRandomNumberGenerator;
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'TSizes'}{$ENDIF}

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

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$REGION 'TBuffer'}

constructor TBuffer.Create(size: Integer);
begin
  TArgument.CheckRange(size >= 0, 'size');

  SetLength(fBytes, size);
end;

constructor TBuffer.Create(const buffer: Pointer; count: Integer);
begin
  TArgument.CheckRange(count >= 0, 'count');

  SetLength(fBytes, count);
  Move(buffer^, fBytes[0], count);
end;

constructor TBuffer.Create(const buffer: Pointer; startIndex, count: Integer);
begin
  TArgument.CheckRange(startIndex >= 0, 'startIndex');
  TArgument.CheckRange(count >= 0, 'count');

  SetLength(fBytes, count);
  Move(PByte(buffer)[startIndex], fBytes[0], count);
end;

constructor TBuffer.Create(const buffer: array of Byte);
begin
  Create(@buffer[0], Length(buffer));
end;

constructor TBuffer.Create(const buffer: array of Byte; startIndex, count: Integer);
begin
  TArgument.CheckRange(buffer, startIndex, count);

  Create(@buffer[startIndex], count);
end;

constructor TBuffer.Create(const s: string);
begin
  Create(PByte(s), Length(s) * SizeOf(Char));
end;

constructor TBuffer.Create(const s: WideString);
begin
  Create(PByte(s), Length(s) * SizeOf(Char));
end;

constructor TBuffer.Create(const s: RawByteString);
begin
  Create(PByte(s), Length(s));
end;

class function TBuffer.BytesOf(const value: Byte; count: Integer): TBytes;
begin
  TArgument.CheckRange(count >= 0, 'count');

  SetLength(Result, count);
  FillChar(Result[0], count, value);
end;

class function TBuffer.GetByte(const buffer; const index: Integer): Byte;
begin
  TArgument.CheckRange(index >= 0, 'index');

  Result := PByte(@buffer)[index];
end;

procedure TBuffer.SaveToStream(stream: TStream);
begin
  TArgument.CheckNotNull(stream, 'stream');

  stream.WriteBuffer(fBytes[0], Length(fBytes));
end;

class procedure TBuffer.SetByte(var buffer; const index: Integer;
  const value: Byte);
begin
  TArgument.CheckRange(index >= 0, 'index');

  PByte(@buffer)[index] := value;
end;

class function TBuffer.FromHexString(const s: string): TBuffer;
var
  buffer: string;
  text: string;
  bytes: TBytes;
  index: Integer;
  i: Integer;
const
  HexCharSet: TSysCharSet = ['0'..'9', 'a'..'f', 'A'..'F'];
begin
  buffer := StringReplace(s, '0x', '', [rfIgnoreCase, rfReplaceAll]);
  SetLength(text, Length(buffer));
  index := 0;
  for i := 1 to Length(buffer) do
  begin
    if CharInSet(buffer[i], HexCharSet) then
    begin
      Inc(index);
      text[index] := buffer[i];
    end;
  end;
  SetLength(bytes, index div 2);
  Classes.HexToBin(PChar(text), PByte(bytes), Length(bytes));
  Result := TBuffer.Create(bytes);
end;

class function TBuffer.ConvertToHexString(const buffer: Pointer;
  count: Integer): string;
begin
  SetLength(Result, count * 2);
  Classes.BinToHex(buffer, PChar(Result), count);
end;

class function TBuffer.ConvertToHexString(const buffer: Pointer; count: Integer;
  const prefix, delimiter: string): string;
const
  Convert: array[0..15] of Char = '0123456789ABCDEF';
var
  p: PByte;
  stringBuilder: TStringBuilder;
  captacity: Integer;
  text: array[0..1] of Char;
  i: Integer;
begin
  if count = 0 then Exit('');
  p := buffer;
  captacity := (Length(prefix) + 2 + Length(delimiter)) * count;
  stringBuilder := TStringBuilder.Create(captacity);
  try
    stringBuilder.Append(prefix);
    text[0] := Convert[p[0] shr 4];
    text[1] := Convert[p[0] and $0F];
    stringBuilder.Append(text);
    for i := 1 to count - 1 do
    begin
      stringBuilder.Append(delimiter);
      stringBuilder.Append(prefix);
      text[0] := Convert[p[i] shr 4];
      text[1] := Convert[p[i] and $0F];
      stringBuilder.Append(text);
    end;
    Result := stringBuilder.ToString;
  finally
    stringBuilder.Free;
  end;
end;

//procedure TBuffer.CopyTo(var dest: array of Byte; index: Integer);
//begin
//  TArgument.CheckRange(index >= 0, 'index');
//  if Length(dest) - index < Size then
//  begin
//    raise EInsufficientMemoryException.CreateRes(@SInsufficientMemoryException);
//  end;
//  Move(fBytes[0], dest[index], Size);
//end;

function TBuffer.Clone: TBuffer;
begin
  Result := ToBytes;
end;

function TBuffer.Reverse: TBuffer;
var
  i: Integer;
  p: PByte;
begin
  SetLength(Result.fBytes, Size);
  p := @Result.fBytes[Size - 1];
  for i := 0 to Size - 1 do
  begin
    p^ := fBytes[i];
    Dec(p);
  end;
end;

function TBuffer.Copy(startIndex, count: Integer): TBytes;
begin
  TArgument.CheckRange(fBytes, startIndex, count);

  SetLength(Result, count);
  Move(fBytes[startIndex], Result[0], count);
end;

function TBuffer.First: Byte;
begin
  Result := Bytes[0];
end;

function TBuffer.Last: Byte;
begin
  Result := Bytes[Size-1];
end;

function TBuffer.Left(count: Integer): TBuffer;
begin
  TArgument.CheckRange((count >= 0) and (count <= Size), 'count');

  Result := Mid(0, count);
end;

function TBuffer.Mid(startIndex, count: Integer): TBuffer;
begin
  Result := Self.Copy(startIndex, count);
end;

function TBuffer.Right(count: Integer): TBuffer;
begin
  TArgument.CheckRange((count >= 0) and (count <= Size), 'count');

  Result := Mid(Size - count, count);
end;

function TBuffer.EnsureSize(size: Integer): TBuffer;
begin
  Result := Self.EnsureSize(size, 0);
end;

function TBuffer.EnsureSize(size: Integer; value: Byte): TBuffer;
var
  data: TBytes;
begin
  if Self.Size < size then
  begin
    SetLength(data, size);
    Move(fBytes[0], data[0], Self.Size);
    FillChar(data[Self.Size], size - Self.Size, value);
  end
  else
  begin
    data := Self.ToBytes;
  end;
  Result := data;
end;

function TBuffer.EnsureSize(size: Integer; value: AnsiChar): TBuffer;
begin
  Result := Self.EnsureSize(size, Byte(value));
end;

function TBuffer.Equals(const buffer: TBuffer): Boolean;
begin
  Result := Equals(buffer.fBytes);
end;

function TBuffer.Equals(const buffer: array of Byte): Boolean;
begin
  Result := (Size = Length(buffer)) and
    CompareMem(Memory, @buffer[0], Size);
end;

function TBuffer.Equals(const buffer: Pointer; count: Integer): Boolean;
begin
  TArgument.CheckRange(count >= 0, 'count');

  Result := (count = Self.Size) and CompareMem(Self.Memory, buffer, count);
end;

function TBuffer.Equals(const hexString: string): Boolean;
var
  buffer: TBuffer;
begin
  buffer := TBuffer.FromHexString(hexString);
  Result := Equals(buffer);
end;

function TBuffer.ToString: string;
begin
  SetLength(Result, Length(fBytes) div SizeOf(Char));
  Move(fBytes[0], Result[1], Length(Result) * SizeOf(Char));
end;

function TBuffer.ToWideString: WideString;
begin
  SetLength(Result, Length(fBytes) div SizeOf(Char));
  Move(fBytes[0], Result[1], Length(Result) * SizeOf(Char));
end;

function TBuffer.ToAnsiString: RawByteString;
begin
  SetLength(Result, Length(fBytes));
  Move(fBytes[0], Result[1], Length(fBytes));
end;

function TBuffer.ToUtf8String: UTF8String;
begin
  SetLength(Result, Length(fBytes));
  Move(fBytes[0], Result[1], Length(fBytes));
end;

function TBuffer.ToBytes: TBytes;
begin
  SetLength(Result, Length(fBytes));
  Move(fBytes[0], Result[0], Length(fBytes));
end;

function TBuffer.ToHexString: string;
begin
  Result := TBuffer.ConvertToHexString(Memory, Size);
end;

function TBuffer.ToHexString(const prefix: string; const delimiter: string): string;
begin
  Result := TBuffer.ConvertToHexString(Memory, Size, prefix, delimiter);
end;

function TBuffer.GetSize: Integer;
begin
  Result := Length(fBytes);
end;

function TBuffer.GetIsEmpty: Boolean;
begin
  Result := Length(fBytes) = 0;
end;

function TBuffer.GetMemory: PByte;
begin
  Result := PByte(fBytes);
end;

function TBuffer.GetByteItem(const index: Integer): Byte;
begin
  TArgument.CheckRange((index >= 0) and (index < Size), 'index');

  Result := fBytes[index];
end;

procedure TBuffer.SetByteItem(const index: Integer; const value: Byte);
begin
  TArgument.CheckRange((index >= 0) and (index < Size), 'index');

  fBytes[index] := value;
end;

procedure TBuffer.SetSize(const value: Integer);
begin
  SetLength(fBytes, value);
end;

class operator TBuffer.Implicit(const value: TBytes): TBuffer;
begin
  Result.fBytes := value;
end;

class operator TBuffer.Implicit(const value: TBuffer): TBytes;
begin
  Result := value.fBytes;
end;

class operator TBuffer.Explicit(const value: TBuffer): PByte;
begin
  Result := PByte(value.fBytes);
end;

class operator TBuffer.Explicit(const value: TBytes): TBuffer;
begin
  Result.fBytes := value;
end;

class operator TBuffer.Explicit(const value: TBuffer): TBytes;
begin
  Result := value.fBytes;
end;

class operator TBuffer.Implicit(const value: TBuffer): PByte;
begin
  Result := PByte(value.fBytes);
end;

class operator TBuffer.Add(const left, right: TBuffer): TBuffer;
begin
  SetLength(Result.fBytes, left.Size + right.Size);
  Move(left.fBytes[0], Result.fBytes[0], left.Size);
  Move(right.fBytes[0], Result.fBytes[left.Size], right.Size);
end;

class operator TBuffer.Add(const left: TBuffer; const right: Byte): TBuffer;
begin
  Result.Size := left.Size + 1;
  Move(left.Memory^, Result.Memory^, left.Size);
  Result[Result.Size-1] := right;
end;

class operator TBuffer.Add(const left: Byte; const right: TBuffer): TBuffer;
begin
  Result.Size := right.Size + 1;
  Move(right.Memory^, Result.Memory[1], right.Size);
  Result[0] := left;
end;

class operator TBuffer.Equal(const left, right: TBuffer): Boolean;
begin
  Result := left.Equals(right);
end;

class operator TBuffer.NotEqual(const left, right: TBuffer): Boolean;
begin
  Result := not left.Equals(right);
end;

class operator TBuffer.BitwiseXor(const left, right: TBuffer): TBuffer;
var
  i: Integer;
begin
  if left.Size <> right.Size then
  begin
    raise EInvalidOperation.CreateRes(@SInvalidOperationBufferSizeShouldBeSame);
  end;
  Result.Size := left.Size;
  for i := 0 to Result.Size - 1 do
  begin
    Result[i] := left[i] xor right[i];
  end;
end;

{$ENDREGION}


{$IFDEF SUPPORTS_REGION}{$REGION 'THashAlgorithmBase'}{$ENDIF}

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

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'TSymmetricAlgorithmBase'}{$ENDIF}

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
var
  sizes: TSizes;
begin
  sizes := fLegalKeySizes;
  if not sizes.Contains(key.Size * 8) then
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
begin
  Result := DoEncrypt(buffer, count);
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
  inputBuffer.Size := BlockSizeInBytes;
  outputBuffer.Size := BlockSizeInBytes;
  bytes := inputStream.Read(inputBuffer.Memory^, inputBuffer.Size);
  while bytes > 0 do
  begin
    outputBuffer := Encrypt(inputBuffer);
    outputStream.WriteBuffer(outputBuffer.Memory^, outputBuffer.Size);
    bytes := inputStream.Read(inputBuffer.Memory^, inputBuffer.Size);
  end;
end;

function TSymmetricAlgorithmBase.DoEncrypt(const buffer: Pointer;
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
  plainText.Size := BlockSizeInBytes;
  SetLength(cipherText, BlockSizeInBytes);
  firstBlock := True;
  while count >= 0 do
  begin
    if count >= BlockSizeInBytes then
    begin
      Move(p^, plainText.Memory^, BlockSizeInBytes);
    end
    else if PaddingMode <> pmNone then
    begin
      Move(p^, plainText.Memory^, count);
      paddingSize := BlockSizeInBytes - (count mod BlockSizeInBytes);
      startIndex := BlockSizeInBytes - paddingSize;
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
    Dec(count, BlockSizeInBytes);
    Inc(p, BlockSizeInBytes);
  end;
end;

function TSymmetricAlgorithmBase.DoDecrypt(const buffer: Pointer;
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
  inputBuffer.Size := BlockSizeInBytes;
  plainText.Size := BlockSizeInBytes;
  SetLength(outputBuffer, BlockSizeInBytes);
  while count >= BlockSizeInBytes do
  begin
    inputBuffer := TBuffer.Create(p, BlockSizeInBytes);
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
    if count = BlockSizeInBytes then // FinalBlock
    begin
      RemovePadding(plainText);
    end;
    Result := Result + plainText;
    Dec(count, BlockSizeInBytes);
    Inc(p, BlockSizeInBytes);
  end;
  if count > 0 then
  begin
    raise ECryptographicException.CreateRes(@SInvalidCipherText);
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
  Assert(buffer.Size = BlockSizeInBytes);
  case PaddingMode of
    pmNone: ;
    pmPKCS7, pmANSIX923, pmISO10126:
    begin
      paddingSize := Integer(buffer.Last);
      if paddingSize = BlockSizeInBytes then
      begin
        // Validate
        buffer := TBuffer.Empty;
      end
      else if paddingSize < BlockSizeInBytes then
      begin
        count := BlockSizeInBytes - paddingSize;
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
begin
  Result := DoDecrypt(buffer, count);
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
  SetLength(buffer, BlockSizeInBytes);
  count := inputStream.Read(buffer[0], Length(buffer));
  while count >= BlockSizeInBytes do
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
  SetLength(buffer, BlockSizeInBytes);
  generator.GetBytes(buffer);
  Result := buffer;
end;

function TSymmetricAlgorithmBase.GenerateKey: TBuffer;
var
  generator: IRandomNumberGenerator;
  buffer: TBytes;
begin
  generator := TRandomNumberGenerator.Create;
  SetLength(buffer, KeySizeInBytes);
  generator.GetBytes(buffer);
  Result := buffer;
end;

function TSymmetricAlgorithmBase.GetBlockSize: Integer;
begin
  Result := fBlockSize;
end;

function TSymmetricAlgorithmBase.GetBlockSizeInBytes: Integer;
begin
  Result := BlockSize div 8;
end;

function TSymmetricAlgorithmBase.GetKeySize: Integer;
begin
  Result := fKeySize;
end;

function TSymmetricAlgorithmBase.GetKeySizeInBytes: Integer;
begin
  Result := fKeySize div 8;
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
  if not (value in [cmCBC, cmECB]) then
  begin
    raise ENotSupportedException.CreateResFmt(@SNotSupportedCipherMode, [TEnum.GetName<TCipherMode>(value)]);
  end;
  fCipherMode := value;
end;

procedure TSymmetricAlgorithmBase.SetPaddingMode(const value: TPaddingMode);
begin
  fPaddingMode := value;
end;

procedure TSymmetricAlgorithmBase.SetIV(const value: TBuffer);
begin
  if value.Size <> BlockSizeInBytes then
  begin
    raise ECryptographicException.CreateResFmt(@SIllegalIVSize, [value.Size]);
  end;
  fIV := value;
end;

procedure TSymmetricAlgorithmBase.SetKey(const value: TBuffer);
begin
  ValidateKey(value);
  fKey := TBuffer.Create(value.Memory, value.Size);
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'TRandomNumberGenerator'}{$ENDIF}

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

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'TCryptographicServiceProvider'}{$ENDIF}

class function TCryptographicServiceProvider.CreateCRC16: ICRC16;
begin
  Result := TCRC16.Create;
end;

class function TCryptographicServiceProvider.CreateCRC32: ICRC32;
begin
  Result := TCRC32.Create;
end;

class function TCryptographicServiceProvider.CreateMD5: IMD5;
begin
  Result := TMD5.Create;
end;

class function TCryptographicServiceProvider.CreateSHA1: ISHA1;
begin
  Result := TSHA1.Create;
end;

class function TCryptographicServiceProvider.CreateSHA256: ISHA256;
begin
  Result := TSHA256.Create;
end;

class function TCryptographicServiceProvider.CreateSHA384: ISHA384;
begin
  Result := TSHA384.Create;
end;

class function TCryptographicServiceProvider.CreateSHA512: ISHA512;
begin
  Result := TSHA512.Create;
end;

class function TCryptographicServiceProvider.CreateDES: IDES;
begin
  Result := TDES.Create;
end;

class function TCryptographicServiceProvider.CreateTripleDES: ITripleDES;
begin
  Result := TTripleDES.Create;
end;

class function TCryptographicServiceProvider.CreateRandomNumberGenerator: IRandomNumberGenerator;
begin
  Result := TRandomNumberGenerator.Create;
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}

end.
