{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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

{ TODO: Complete TServiceController class }
{ TODO: Complete TOperatingSystem class (ProductName) }

{ TODO: TClipboardWatcher }

{ TODO: IAsyncResult }
{ TODO: TUri class }

unit Spring.System;

{$I Spring.inc}

interface

uses
  Classes,
  Contnrs,
  Windows,
  Messages,
  SysUtils,
  Types,
  TypInfo,
  StrUtils,
  Controls,
  Variants,
  ShellAPI,
  ShlObj,
  DB,
  Registry,
  WinSvc,
  TimeSpan,
  Character,
  Diagnostics,
  RTTI,
  Generics.Defaults,
  Generics.Collections,
  Spring.Win32API,
  Spring.ResourceStrings;

type
  {$REGION 'Simple Types & Aliases'}

  /// <summary>
  /// Represents a dynamic array of Byte.
  /// </summary>
  TBytes = SysUtils.TBytes;

  /// <summary>
  /// Represents a dynamic array of string.
  /// </summary>
  TStringDynArray = Types.TStringDynArray;

  /// <summary>
  /// Represents a time interval.
  /// </summary>
  TTimeSpan = TimeSpan.TTimeSpan;

  /// <summary>
  /// Provides a set of methods and properties to accurately measure elapsed time.
  /// </summary>
  TStopwatch = Diagnostics.TStopwatch;

  PTypeInfo = TypInfo.PTypeInfo;

  TCharacter = Character.TCharacter;

//  UTF16Char = UCS2Char;
//  UTF32Char = UCS4Char;

  TIntegerRec = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
    B4: Byte;
  end;

  TInt32Rec = TIntegerRec;

  TInt64Rec = SysUtils.Int64Rec;

  {$ENDREGION}


  {$REGION 'TInterfaceBase'}

  /// <summary>
  /// Provides a default implementation of IInterface without reference-counting
  /// </summary>
  TInterfaceBase= class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {$ENDREGION}


  {$REGION 'Exceptions'}

  Exception = SysUtils.Exception;

  ENotSupportedException    = SysUtils.ENotSupportedException;
  ENotImplementedException  = class(Exception);

  EInvalidOperation         = SysUtils.EInvalidOp;
  EInvalidCastException     = SysUtils.EConvertError;

  EInsufficientMemoryException = class(EOutOfMemory);

  EFormatException          = class(Exception);
  EIndexOutOfRangeException = class(Exception);

  EArgumentException            = SysUtils.EArgumentException;
  EArgumentOutOfRangeException  = SysUtils.EArgumentOutOfRangeException;
  EArgumentNullException        = class(EArgumentException);
  EInvalidEnumArgumentException = class(EArgumentException);

  EIOException                  = SysUtils.EInOutError;
  EFileNotFoundException        = SysUtils.EFileNotFoundException;
  EDirectoryNotFoundException   = SysUtils.EDirectoryNotFoundException;
  EDriveNotFoundException       = class(EIOException);

  EArithmeticException = EMathError;

  ETimeoutException = class(Exception);

  ERttiException = class(Exception);

  {$ENDREGION}


  {$REGION 'TArgument'}

  /// <summary>
  /// Provides static methods to check arguments and raise argument exceptions.
  /// </summary>
  /// <remarks>
  /// All arguments of public methods, including global routines, class and record methods,
  /// should be checked.
  /// </remarks>
  TArgument = record
  strict private
    class procedure DoCheckIndex(const length, index, indexBase: Integer); overload; static; inline;
    class procedure DoCheckRange(const length, startIndex, count, indexBase: Integer); overload; static; inline;
  private
    class procedure DoCheckArrayIndex(const length, index: Integer); static; inline;
    class procedure DoCheckArrayRange(const length, startIndex, count: Integer); static; inline;
    class procedure DoCheckStringIndex(const length, index: Integer); static; inline;
    class procedure DoCheckStringRange(const length, startIndex, count: Integer); static; inline;
  public
    class procedure CheckTrue(condition: Boolean; const argumentName: string); static; inline;
    class procedure CheckFalse(condition: Boolean; const argumentName: string); static; inline;

    class procedure CheckNotNull(obj: TObject; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(p: Pointer; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(const intf: IInterface; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(condition: Boolean; const argumentName: string); overload; static; inline;

    class procedure CheckEnum<T{:enum}>(const value: T; const argumentName: string); overload; static; inline;
    class procedure CheckEnum<T>(const value: Integer; const argumentName: string); overload; static; inline;

    class procedure CheckRange(const buffer: array of Byte; const index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Byte; const startIndex, count: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; const index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; const startIndex, count: Integer); overload; static;
    class procedure CheckRange(const s: string; const index: Integer); overload; static; inline;
    class procedure CheckRange(const s: string; const startIndex, count: Integer); overload; static; inline;
    class procedure CheckRange(const s: WideString; const index: Integer); overload; static; inline;
    class procedure CheckRange(const s: WideString; const startIndex, count: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; const index: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; const startIndex, count: Integer); overload; static; inline;
    class procedure CheckRange(condition: Boolean; const argumentName: string); overload; static; inline;

    class procedure RaiseArgumentException(const argumentName: string); overload; static; inline;
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static; inline;
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static; inline;
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static; inline;
  end;

  TArg = TArgument;

  {$ENDREGION}


  {$REGION 'TBuffer (Experimental)'}

  /// <summary>
  /// Represents a series of bytes in memory.
  /// </summary>
  /// <remarks>
  /// </remarks>
  TBuffer = record
  strict private
    fBytes: TBytes;
    function GetIsEmpty: Boolean;
    function GetMemory: PByte;
    function GetSize: Integer;
    function GetByteItem(const index: Integer): Byte;
    procedure SetByteItem(const index: Integer; const value: Byte);
  public
    constructor Create(const buffer: Pointer; count: Integer); overload;
    constructor Create(const buffer: Pointer; startIndex, count: Integer); overload;
    constructor Create(const buffer: array of Byte); overload;
    constructor Create(const buffer: array of Byte; startIndex, count: Integer); overload;
    constructor Create(const s: string); overload;
    constructor Create(const s: WideString); overload;
    constructor Create(const s: RawByteString); overload;

    class function FromHexString(const s: string): TBuffer; static;
    class function ConvertToHexString(const buffer: Pointer; count: Integer): string; overload; static;
    class function ConvertToHexString(const buffer: Pointer; count: Integer;
      const prefix: string; const delimiter: string = ' '): string; overload; static;

    class function BytesOf(const value: Byte; count: Integer): TBytes; static;
    class function GetByte(const buffer; const index: Integer): Byte; static;
    class procedure SetByte(var buffer; const index: Integer; const value: Byte); static;

//    procedure CopyTo(var dest: array of Byte; index, count: Integer);

    function Copy(startIndex, count: Integer): TBytes;

    function Left(count: Integer): TBytes;
    function Mid(startIndex, count: Integer): TBytes;
    function Right(count: Integer): TBytes;

    function EnsureSize(size: Integer): TBytes; overload;
    function EnsureSize(size: Integer; value: Byte): TBytes; overload;
    function EnsureSize(size: Integer; value: AnsiChar): TBytes; overload;

    function Equals(const buffer: TBuffer): Boolean; overload;
    function Equals(const buffer: array of Byte): Boolean; overload;
    function Equals(const buffer: Pointer; count: Integer): Boolean; overload;

    function ToBytes: TBytes;
    function ToString: string; experimental;
    function ToWideString: WideString; experimental;
    function ToAnsiString: RawByteString; experimental;
    function ToUtf8String: UTF8String; experimental;

    function ToHexString: string; overload;
    function ToHexString(const prefix: string; const delimiter: string = ' '): string; overload;

    property IsEmpty: Boolean read GetIsEmpty;
    property Memory: PByte read GetMemory;
    property Size: Integer read GetSize;
    property Bytes[const index: Integer]: Byte read GetByteItem write SetByteItem; default;

    { Operator Overloads }
    class operator Implicit(const value: TBytes): TBuffer;
    class operator Implicit(const value: TBuffer): TBytes;
    class operator Explicit(const value: TBytes): TBuffer;
    class operator Explicit(const value: TBuffer): TBytes;
    class operator Add(const left, right: TBuffer): TBuffer;
    class operator Equal(const left, right: TBuffer): Boolean;
    class operator NotEqual(const left, right: TBuffer): Boolean;
  end;

  {$ENDREGION}


  {$REGION 'TBufferStream (NOT READY)'}

  /// <summary>
  /// Provides a reusable memory stream.
  /// </summary>
//  TBufferStream = class(TCustomMemoryStream)
//
//  end;

  {$ENDREGION}


  {$REGION 'TValueType (NOT READY)'}

  TValueType = class abstract(TInterfacedObject)

  end;

  {$ENDREGION}


  {$REGION 'TEnum'}

  /// <summary>
  /// Provides static methods to manipulate Enumeration type.
  /// </summary>
  TEnum = class sealed(TValueType)
  private
    class function GetEnumTypeInfo<T>: PTypeInfo;
    class function GetEnumTypeData<T>: PTypeData;
    { Internal function without range check }
    class function ConvertToInteger<T>(const value: T): Integer;
  public
    class function IsValid<T>(const value: T): Boolean; overload;
    class function IsValid<T>(const value: Integer): Boolean; overload;
    class function GetName<T>(const value: T): string; overload;
    class function GetName<T>(const value: Integer): string; overload;
    class function GetNames<T>: TStringDynArray;
    class function GetValue<T>(const value: T): Integer; overload;
    class function GetValue<T>(const value: string): Integer; overload;
    class function GetValues<T>: TIntegerDynArray;
    class function TryParse<T>(const value: Integer; out enum: T): Boolean; overload;
    class function TryParse<T>(const value: string; out enum: T): Boolean; overload;
    class function Parse<T>(const value: Integer): T; overload;
    class function Parse<T>(const value: string): T; overload;
  end;

  {$ENDREGION}


  {$REGION 'TNullable<T>'}

  /// <summary>
  /// Represents an object whose underlying type is a value type that can also
  /// be assigned nil like a reference type.
  /// </summary>
  TNullable<T> = record
  strict private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;
  private
    const fCHasValue = 'HasValue';  // DO NOT LOCALIZE
  public
    constructor Create(const value: T); overload;
    constructor Create(const value: Variant); overload;
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(const default: T): T; overload;
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue;
    { Operator Overloads }
    class operator Implicit(const value: TNullable<T>): T;
    class operator Implicit(const value: T): TNullable<T>;
    class operator Implicit(const value: TNullable<T>): Variant;
    class operator Implicit(const value: Variant): TNullable<T>;
    class operator Implicit(value: Pointer): TNullable<T>;
    class operator Explicit(const value: TNullable<T>): T;
  end;

  {$ENDREGION}


  {$REGION 'IValueProvider<T>, TValueProvider<T>'}

  /// <summary>
  /// IValueProvider<T>
  /// </summary>
  IValueProvider<T> = interface
    function GetValue: T;
    function GetIsReadOnly: Boolean;
    procedure SetValue(const value: T);
    property Value: T read GetValue write SetValue;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TCleanUpProc<T> = reference to procedure(var arg: T);

  TValueProvider<T> = class(TInterfacedObject, IValueProvider<T>, IInterface)
  private
    fValue: T;
    fCleanUpProc: TCleanUpProc<T>;
    function GetValue: T;
    procedure SetValue(const value: T);
  protected
    function GetIsReadOnly: Boolean; virtual;
  public
    constructor Create; overload;
    constructor Create(const value: T); overload;
    constructor Create(const value: T; cleanUpProc: TCleanUpProc<T>); overload;
    destructor Destroy; override;
    property Value: T read GetValue write SetValue;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  {$ENDREGION}


  {$REGION 'TValueHolder<T>'}

  /// <summary>
  /// Wraps value-type
  /// </summary>
  TValueHolder<T> = record
  strict private
    fProvider: IValueProvider<T>;
    procedure ProviderNeeded;
    function  GetValue: T;
    procedure SetValue(const Value: T);
  public
    constructor Create(const value: T); overload;
    constructor Create(const value: T; cleanUpProc: TCleanUpProc<T>); overload;
    constructor Create(const valueProvider: IValueProvider<T>); overload;
    property Value: T read GetValue write SetValue;
    { Conversion }
    class operator Implicit(const value: T): TValueHolder<T>;
    class operator Implicit(const valueHolder: TValueHolder<T>): T;
    class operator Implicit(const valueProvider: IValueProvider<T>): TValueHolder<T>;
    class operator Implicit(const valueHolder: TValueHolder<T>): IValueProvider<T>;
  end;

  {$ENDREGION}


  {$REGION 'TObjectHolder<T>'}

  /// <summary>
  /// Manages object's lifetime by anonymous methods (TFunc<T>),
  /// which is implemented as a reference-counting interface in Delphi for Win32.
  /// </summary>
  TObjectHolder<T: class> = class(TInterfacedObject, TFunc<T>)
  private
    fObject: T;
  public
    constructor Create(obj: T);
    destructor Destroy; override;
    function Invoke: T;
  end;

  {$ENDREGION}


  {$REGION 'TRtti'}

  /// <summary>
  /// Provides static methods to get RTTI information of parameterized type.
  /// </summary>
  TRtti = class
  public
    class procedure CheckTypeKind<T>(const typeKind: TypInfo.TTypeKind); overload;
    class procedure CheckTypeKind<T>(const typeKinds: TypInfo.TTypeKinds); overload;
    class function GetTypeInfo<T>: PTypeInfo;
    class function GetTypeData<T>: PTypeData;
    class function GetTypeKind<T>: TypInfo.TTypeKind;
    class function GetTypeName<T>: string;
    { TODO: TRtti.GetFullName }
    class function GetFullName(typeInfo: PTypeInfo): string;
//    class function GetType<T>(const obj: T): IType<T>;
//    class function GetFullQualifiedName<T>: string;
//    class function IsManagedType<T>: Boolean;
  end;

  {$ENDREGION}


  {$REGION 'IDelegate<T> (Experimental)'}

  IDelegate<T> = interface
    procedure Add(const delegate: T);
    procedure Remove(const delegate: T);
    procedure Clear;
    procedure Invoke(proc: TProc<T>);
  end;

  TDelegate<T> = class(TInterfacedObject, IDelegate<T>, IInterface)
  private
    fList: TList<T>;
  public
    destructor Destroy; override;
    procedure Add(const delegate: T);
    procedure Remove(const delegate: T);
    procedure Clear;
    procedure Invoke(proc: TProc<T>);
  end;

  {$ENDREGION}


  {$REGION 'TType (NOT READY)'}

//  TType = class abstract(TSingleton)
//
//  end;

  {$ENDREGION}


  {$REGION 'TVersion'}

  /// <summary>
  /// Represents version number in the format of "major.minor[.build[.revision]]"
  /// </summary>
  TVersion = record
  strict private
    fMajor: Integer;
    fMinor: Integer;
    fBuild: Integer;      // -1 if undefined.
    fReversion: Integer;  // -1 if undefined.
    function GetMajorReversion: Int16;
    function GetMinorReversion: Int16;
  private
    const fCUndefined: Integer = -1;
  private
    constructor InternalCreate(defined, major, minor, build, reversion: Integer);
    function CompareComponent(a, b: Integer): Integer;
    function IsDefined(const component: Integer): Boolean; inline;
  public
    constructor Create(major, minor: Integer); overload;
    constructor Create(major, minor, build: Integer); overload;
    constructor Create(major, minor, build, reversion: Integer); overload;
    constructor Create(const versionString: string); overload;
    function CompareTo(const version: TVersion): Integer;
    function Equals(const version: TVersion): Boolean;
    function ToString: string; overload;
    function ToString(fieldCount: Integer): string; overload;
    property Major: Integer read fMajor;
    property MajorReversion: Int16 read GetMajorReversion;
    property Minor: Integer read fMinor;
    property MinorReversion: Int16 read GetMinorReversion;
    property Build: Integer read fBuild;
    property Reversion: Integer read fReversion;
    { Comparison Operators }
    class operator Equal(const left, right: TVersion): Boolean;
    class operator NotEqual(const left, right: TVersion): Boolean;
    class operator GreaterThan(const left, right: TVersion): Boolean;
    class operator GreaterThanOrEqual(const left, right: TVersion): Boolean;
    class operator LessThan(const left, right: TVersion): Boolean;
    class operator LessThanOrEqual(const left, right: TVersion): Boolean;
  end;

  {$ENDREGION}


  {$REGION 'TFileVersionInfo'}

  /// <summary>
  /// Provides version information for a physical file on disk.
  /// </summary>
  TFileVersionInfo = record
  private
    type
      TLangAndCodePage = record
        Language: Word;
        CodePage: Word;
      end;

      TLangAndCodePageArray  = array[0..9] of TLangAndCodePage;
      PTLangAndCodePageArray = ^TLangAndCodePageArray;

      TFileVersionResource = record
      private
        fBlock: Pointer;
        fLanguage: Word;
        fCodePage: Word;
      public
        constructor Create(block: Pointer; language, codePage: Word);
        function ReadString(const stringName: string): string;
        property Language: Word read fLanguage;
        property CodePage: Word read fCodePage;
      end;
  strict private
    fExists: Boolean;
    fFileFlags: DWORD;
    fComments: string;
    fCompanyName: string;
    fFileName: string;
    fFileVersion: string;
    fFileVersionNumber: TVersion;
    fFileDescription: string;
    fProductName: string;
    fProductVersion: string;
    fProductVersionNumber: TVersion;
    fInternalName: string;
    fLanguage: string;
    fLegalCopyright: string;
    fLegalTrademarks: string;
    fOriginalFilename: string;
    fPrivateBuild: string;
    fSpecialBuild: string;
    function GetIsDebug: Boolean;
    function GetIsPatched: Boolean;
    function GetIsPreRelease: Boolean;
    function GetIsPrivateBuild: Boolean;
    function GetIsSpecialBuild: Boolean;
  private
    constructor Create(const fileName: string);
    procedure LoadVersionResource(const resource: TFileVersionResource);
  public
    class function GetVersionInfo(fileName: string): TFileVersionInfo; static;
    function ToString: string;
    property Exists: Boolean read fExists;
    property Comments: string read fComments;
    property CompanyName: string read fCompanyName;
    property FileName: string read fFileName;
    property FileDescription: string read fFileDescription;
    property FileVersion: string read fFileVersion;
    property FileVersionNumber: TVersion read fFileVersionNumber;
    property InternalName: string read fInternalName;
    property Language: string read fLanguage;
    property LegalCopyright: string read fLegalCopyright;
    property LegalTrademarks: string read fLegalTrademarks;
    property OriginalFilename: string read fOriginalFilename;
    property ProductName: string read fProductName;
    property ProductVersion: string read fProductVersion;
    property ProductVersionNumber: TVersion read fProductVersionNumber;
    property PrivateBuild: string read fPrivateBuild;
    property SpecialBuild: string read fSpecialBuild;
    property IsDebug: Boolean read GetIsDebug;
    property IsPatched: Boolean read GetIsPatched;
    property IsPreRelease: Boolean read GetIsPreRelease;
    property IsSpecialBuild: Boolean read GetIsSpecialBuild;
    property IsPrivateBuild: Boolean read GetIsPrivateBuild;
  end;

  {$ENDREGION}


  {$REGION 'TOperatingSystem (Experimental)'}

  TOSPlatformType = (
    ptUnknown,
    ptWin3x,
    ptWin9x,
    ptWinNT
  );

  TOSVersionType = (
    vtUnknown,
    vtWin95,            // DEPRECATED
    vtWin98,            // DEPRECATED
    vtWinME,            // DEPRECATED
    vtWinNT351,         // DEPRECATED
    vtWinNT4,           // DEPRECATED
    vtWinServer2000,
    vtWinXP,
    vtWinServer2003,
    vtWinVista,
    vtWinServer2008,
    vtWin7
  );

  TOSProductType = (
    ptInvalid,
    ptWorkstation,
    ptServer,
    ptDomainController
  );

  TOSSuiteType = (
    etUnknown,
    etWorkStation,
    etServer,
    etAdvancedServer,
    etPersonal,
    etProfessional,
    etDatacenterServer,
    etEnterprise,
    etWebEdition
  );

  /// <summary>
  /// Represents information about the operating system.
  /// </summary>
  TOperatingSystem = class // sealed(TSingleton)
  strict private
    fEdition: string;
    fPlatformType: TOSPlatformType;
    fProductType: TOSProductType;
    fServicePack: string;
    fVersion: TVersion;
    fVersionType: TOSVersionType;
    function GetIsWin9x: Boolean;
    function GetIsWinNT: Boolean;
    function GetVersionString: string;
    function GetOSVersionType(platformType: TOSPlatformType; productType: TOSProductType;
      majorVersion, minorVersion: Integer): TOSVersionType;
  strict private
    class var
      fInstance: TOperatingSystem;
    class constructor Create; overload;
    class destructor Destroy;
  public
    constructor Create; overload;
    function ToString: string; override;
//    property Edition: string read fEdition;
    property IsWin9x: Boolean read GetIsWin9x;
    property IsWinNT: Boolean read GetIsWinNT;
    property PlatformType: TOSPlatformType read fPlatformType;
    property ProductType: TOSProductType read fProductType;
    property ServicePack: string read fServicePack;
    property Version: TVersion read fVersion;
    property VersionString: string read GetVersionString;
    property VersionType: TOSVersionType read fVersionType;
    class property Instance: TOperatingSystem read fInstance;
  end;

  {$ENDREGION}


  {$REGION 'Special Folder Enumeration'}

  TSpecialFolder = (
    sfDesktop,                // <desktop>
    sfInternet,               // Internet Explorer (icon on desktop)
    sfPrograms,               // Start Menu\Programs
    sfControls,               // My Computer\Control Panel
    sfPrinters,               // My Computer\Printers
    sfPersonal,               // My Documents
    sfFavorites,              // <user name>\Favorites
    sfStartup,                // Start Menu\Programs\Startup
    sfRecent,                 // <user name>\Recent
    sfSendTo,                 // <user name>\SendTo
    sfBitBucket,              // <desktop>\Recycle Bin
    sfStartMenu,              // <user name>\Start Menu
    { For Windows >= XP }
    sfMyDocuments,            // logical "My Documents" desktop icon
    sfMyMusic,                // "My Music" folder
    { For Windows >= XP }
    sfMyVideo,                // "My Videos" folder
    sfDesktopDirectory,       // <user name>\Desktop
    sfDrives,                 // My Computer
    sfNetwork,                // Network Neighborhood (My Network Places)
    sfNethood,                // <user name>\nethood
    sfFonts,                  // windows\fonts
    sfTemplates,              // <user name>\Templates
    sfCommonStartMenu,        // All Users\Start Menu
    sfCommonPrograms,         // All Users\Start Menu\Programs
    sfCommonStartup,          // All Users\Startup
    sfCommonDesktopDirectory, // All Users\Desktop
    sfAppData,                // <user name>\Application Data
    sfPrinthood,              // <user name>\PrintHood
    sfLocalAppData,           // <user name>\Local Settings\Applicaiton Data (non roaming)
    sfALTStartup,             // non localized startup
    sfCommonALTStartup,       // non localized common startup
    sfCommonFavorites,        // All Users\Favorites
    sfInternetCache,          // <user name>\Local Settings\Temporary Internet Files
    sfCookies,                // <user name>\Cookies
    sfHistory,                // <user name>\Local Settings\History
    sfCommonAppData,          // All Users\Application Data
    sfWindows,                // GetWindowsDirectory()
    sfSystem,                 // GetSystemDirectory()
    sfProgramFiles,           // C:\Program Files
    sfMyPictures,             // C:\Program Files\My Pictures
    sfProfile,                // USERPROFILE
    sfSystemX86,              // x86 system directory on RISC
    sfProgramFilesX86,        // x86 C:\Program Files on RISC
    sfProgramFilesCommon,     // C:\Program Files\Common
    sfProgramFilesCommonX86,  // x86 Program Files\Common on RISC
    sfCommonTemplates,        // All Users\Templates
    sfCommonDocuments,        // All Users\Documents
    sfCommonAdminTools,       // All Users\Start Menu\Programs\Administrative Tools
    sfAdminTools,             // <user name>\Start Menu\Programs\Administrative Tools
    sfConnections,            // Network and Dial-up Connections
    { For Windows >= XP }
    sfCommonMusic,            // All Users\My Music
    { For Windows >= XP }
    sfCommonPictures,         // All Users\My Pictures
    { For Windows >= XP }
    sfCommonVideo,            // All Users\My Video
    sfResources,              // Resource Direcotry
    sfResourcesLocalized,     // Localized Resource Direcotry
    sfCommonOEMLinks,         // Links to All Users OEM specific apps
    { For Windows >= XP }
    sfCDBurnArea,             // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    sfComputersNearMe         // Computers Near Me (computered from Workgroup membership)
  );

  {$ENDREGION}


  {$REGION 'TEnvironment'}

  /// <summary>
  /// Specifies the location where an environment variable is stored or
  /// retrieved in a set or get operation.
  /// </summary>
  TEnvironmentVariableTarget = (
    /// <summary>
    /// The environment variable is stored or retrieved from the environment
    /// block associated with the current process.
    /// </summary>
    evtProcess,
    /// <summary>
    /// The environment variable is stored or retrieved from the
    /// HKEY_CURRENT_USER\Environment key in the Windows operating system registry.
    /// </summary>
    evtUser,
    /// <summary>
    /// The environment variable is stored or retrieved from the
    /// HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment key
    /// in the Windows operating system registry.
    /// </summary>
    evtMachine
  );

  /// <summary>
  /// Identifies the processor and bits-per-word of the platform targeted by an executable.
  /// </summary>
  TProcessorArchitecture = (
    paUnknown,    // Unknown processor
    paX86,        // Intel x86 and compatible microprocessors.
    paIA64,       // 64-bit Intel and compatible microprocessors.
    paAmd64       // 64-bit AMD microprocessors.
  );

  /// <summary>
  /// Provides information about, and means to manipulate, the current environment.
  /// </summary>
  TEnvironment = record
  private
    class function CreateRegistry(target: TEnvironmentVariableTarget; out key: string): TRegistry; static;
    class function GetCurrentVersionKey: string; static;
    class procedure GetProcessEnvironmentVariables(list: TStrings); static;
  private
    class function GetCurrentDirectory: string; static;
    class function GetMachineName: string; static;
    class function GetIsAdmin: Boolean; static;
    class function GetUserDomainName: string; static;
    class function GetUserName: string; static;
    class function GetTickCount: Cardinal; static;
    class function GetNewLine: string; static;
    class function GetUserInteractive: Boolean; static;
    class function GetCommandLine: string; static;
    class function GetSystemDirectory: string; static;
    class function GetOperatingSystem: TOperatingSystem; static;
    class function GetProcessorCount: Integer; static;
    class function GetProcessorArchitecture: TProcessorArchitecture; static;
    class function GetRegisteredOrganization: string; static;
    class function GetRegisteredOwner: string; static;
    class procedure SetCurrentDirectory(const value: string); static;
  public
    class function  GetCommandLineArgs: TStringDynArray; overload; static;
    class procedure GetCommandLineArgs(list: TStrings); overload; static;
    class function  GetLogicalDrives: TStringDynArray; overload; static;
    class procedure GetLogicalDrives(list: TStrings); overload; static;
    class function  GetFolderPath(const folder: TSpecialFolder): string; static;
    class function  GetEnvironmentVariable(const variable: string): string; overload; static;
    class function  GetEnvironmentVariable(const variable: string; target: TEnvironmentVariableTarget): string; overload; static;
    class procedure GetEnvironmentVariables(list: TStrings); overload; static;
    class procedure GetEnvironmentVariables(list: TStrings; target: TEnvironmentVariableTarget); overload; static;
    class procedure SetEnvironmentVariable(const variable, value: string); overload; static;
    class procedure SetEnvironmentVariable(const variable, value: string; target: TEnvironmentVariableTarget); overload; static;
    class function  ExpandEnvironmentVariables(const variable: string): string; static;
    class property CommandLine: string read GetCommandLine;
    class property CurrentDirectory: string read GetCurrentDirectory write SetCurrentDirectory;
    class property IsAdmin: Boolean read GetIsAdmin; { experimental }
    class property MachineName: string read GetMachineName;
    class property NewLine: string read GetNewLine;
    class property OperatingSystem: TOperatingSystem read GetOperatingSystem;
    class property ProcessorCount: Integer read GetProcessorCount;
    class property ProcessorArchitecture: TProcessorArchitecture read GetProcessorArchitecture;
    class property RegisteredOrganization: string read GetRegisteredOrganization;
    class property RegisteredOwner: string read GetRegisteredOwner;
    class property SystemDirectory: string read GetSystemDirectory;
    class property TickCount: Cardinal read GetTickCount;
    class property UserDomainName: string read GetUserDomainName;
    class property UserName: string read GetUserName;
    class property UserInteractive: Boolean read GetUserInteractive;
  end;

  /// <summary>
  /// Provides a type alias for TEnvironment class.
  /// </summary>
  Environment = TEnvironment;

  {$ENDREGION}


  {$REGION 'TServiceController (NOT COMPLETED)'}

  TServiceType = (
    stKernelDriver,       // A Kernel device driver such as a hard disk or other low-level hardware device driver.
    stFileSystemDriver,   // A file system driver, which is also a Kernel device driver.
    stAdapter,            // A service for a hardware device that requires its own driver.
    stRecognizerDriver,   // A file system driver used during startup to determine the file systems present on the system.
    stWin32OwnProcess,    // A Win32 program that can be started by the Service Controller and that obeys the service control protocol. This type of Win32 service runs in a process by itself.
    stWin32ShareProcess,  // A Win32 service that can share a process with other Win32 services.
    stInteractiveProcess  // A service that can communicate with the desktop.
  );

  TServiceTypes = set of TServiceType;

  TServiceStatus = (
    ssUnknown,            // The status is unknown.
    ssStopped,            // The service is not running.
    ssStartPending,       // The service is starting.
    ssStopPending,        // The service is stopping.
    ssRunning,            // The service is running.
    ssContinuePending,    // The service continue is pending.
    ssPausePending,       // The service pause is pending.
    ssPaused              // The service is paused.
  );

  TServiceStartMode = (
    smBoot,
    smSystem,
    smAutomatic,
    smManual,
    smDisabled
  );

  TServiceAccount = (
    LocalService,         // An account that acts as a non-privileged user on the local computer, and presents anonymous credentials to any remote server.
    NetworkService,       // An account that provides extensive local privileges, and presents the computer's credentials to any remote server.
    LocalSystem,          // An account, used by the service control manager, that has extensive privileges on the local computer and acts as the computer on the network.
    User                  // An account defined by a specific user on the network.
  );

  TServiceControlAccepted = (
    caStop,               // SERVICE_ACCEPT_STOP
    caPauseAndResume,     // SERVICE_ACCEPT_PAUSE_CONTINUE
    caShutdown            // SERVICE_ACCEPT_SHUTDOWN
  );

  TServiceControls = set of TServiceControlAccepted;

  /// <summary>
  /// Represents a Windows service and allows you to connect to a running or
  /// stopped service, manipulate it, or get information about it.
  /// </summary>
  TServiceController = record
  private
    type
      TServiceKind = (
        skDrive,  // SERVICE_DRIVER (0x0000000B)
        skWin32   // SERVICE_WIN32  (0x00000030)
      );
      TServiceAction = (
        saStart,
        saStop,
        saPause,
        saResume
      );
    const
      fCInvalidServiceHandle: THandle = 0;
      fCPendingStatusSet = [
        ssStartPending,
        ssStopPending,
        ssContinuePending,
        ssPausePending
      ];
  strict private
    fManagerHandle: TValueHolder<THandle>;
    fServiceHandle: TValueHolder<THandle>;
    fServiceStatusProcess: TServiceStatusProcess;
    fServiceConfig: TValueHolder<PQueryServiceConfig>;
    fStatus: TServiceStatus;
    fDisplayName: string;
    fServiceName: string;
    fMachineName: string;
    fDescription: string;
    fFileName:    string;
    fServiceType: TServiceTypes;
    fStartMode: TServiceStartMode;
    fCanStop: Boolean;
    fCanPauseAndResume: Boolean;
    fCanShutdown: Boolean;
    fDependentServices: TArray<TServiceController>;
    fServicesDependedOn: TArray<TServiceController>;
    function GetDisplayName: string;
    function GetMachineName: string;
    function GetDescription: string;
    function GetServiceName: string;
    function GetServiceType: TServiceTypes;
    function GetServiceHandle: THandle;
    function GetStatus: TServiceStatus;
    function GetStartMode: TServiceStartMode;
    function GetCanPauseAndResume: Boolean;
    function GetCanStop: Boolean;
    function GetCanShutdown: Boolean;
    function GetExists: Boolean;
    function GetDependentServices: TArray<TServiceController>;
    function GetServicesDependedOn: TArray<TServiceController>;
  private
    fIsLoaded: Boolean;
    procedure Loaded;
    procedure ManagerHandleNeeded;
    procedure Open(desiredAccess: Cardinal);
    procedure Close;
    procedure PerformAction(action: TServiceAction);
    procedure UpdateStatus;
    procedure UpdateConfig;
    procedure UpdateDescription;
    procedure UpdateDependentServices;
    function ParseStatus(const value: Cardinal): TServiceStatus;
    function ParseServiceTypes(const value: Cardinal): TServiceTypes;
    function ParseStartMode(const value: Cardinal): TServiceStartMode;
  private
    constructor Create(const serviceName, machineName: string; const managerHandle: TValueHolder<THandle>); overload;
    class procedure GetManagerHandleHolder(const machineName: string; out handleHolder: TValueHolder<THandle>); static;
    class procedure GetHandleHolder(handle: THandle; out valueHolder: TValueHolder<THandle>); static;
    class procedure InternalEnumServices(const machineName: string; serviceKind: TServiceKind; out services: TArray<TServiceController>); static;
  public
    constructor Create(const serviceName: string); overload;
    constructor Create(const serviceName, machineName: string); overload;
    class function GetDevices: TArray<TServiceController>; overload; static;
    class function GetDevices(const machineName: string): TArray<TServiceController>; overload; static;
    class function GetServices: TArray<TServiceController>; overload; static;
    class function GetServices(const machineName: string): TArray<TServiceController>; overload; static;
    procedure ExecuteCommand(command: Integer);
    procedure Start; overload;
    procedure Start(const args: array of string); overload;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    procedure Restart;
    procedure Refresh;
    procedure WaitForStatus(status: TServiceStatus); overload;
    procedure WaitForStatus(desiredStatus: TServiceStatus; const timeout: TTimeSpan); overload;
    property DisplayName: string read GetDisplayName;
    property Description: string read GetDescription;
    property MachineName: string read GetMachineName;
    property ServiceName: string read GetServiceName;
    property Status: TServiceStatus read GetStatus;
    property ServiceHandle: THandle read GetServiceHandle;
    property ServiceType: TServiceTypes read GetServiceType;
    property StartMode: TServiceStartMode read GetStartMode;
    property CanStop: Boolean read GetCanStop;
    property CanPauseAndResume: Boolean read GetCanPauseAndResume;
    property CanShutdown: Boolean read GetCanShutdown;
    property Exists: Boolean read GetExists;
    property DependentServices: TArray<TServiceController> read GetDependentServices;
    property ServicesDependedOn: TArray<TServiceController> read GetServicesDependedOn;
  end;

  {$ENDREGION}


  {$REGION 'TLifetimeWatcher (Experimental)'}

  /// <summary>
  /// TLifetimeWatcher
  /// </summary>
  /// <author>BARRY KELLY</author>
  TLifetimeWatcher = class(TInterfacedObject)
  private
    fProc: TProc;
  public
    constructor Create(const proc: TProc);
    destructor Destroy; override;
  end;

  {$ENDREGION}


  {$REGION 'TCallbackFunc'}

  /// <summary>
  /// TCallbackFunc
  /// </summary>
  TCallbackFunc = TFunc<Pointer>;

  /// <summary>
  /// Adapts class instance (object) method as Windows standard callback function.
  /// </summary>
  /// <remarks>
  /// Both the object method and the callback function need to be declared as stdcall.
  /// </remarks>
  TCallback = class(TInterfacedObject, TCallbackFunc)
  private
    fInstance: Pointer;
  public
    constructor Create(objectAddress: TObject; methodAddress: Pointer);
    destructor Destroy; override;
    function Invoke: Pointer;
  end;


  {$ENDREGION}


  {$REGION 'ILockable (NOT READY)'}

//  ILockable = interface
//    procedure Lock;
//    procedure Unlock;
//    function TryLock: Boolean;
////    function GetIsLocked: Boolean;
////    property IsLocked: Boolean read GetIsLocked;
//  end;

  {$ENDREGION}


  {$REGION 'IAsyncResult (NOT READY)'}

  (*

  /// <summary>
  /// Represents the status of an asynchronous operation.
  /// </summary>
  IAsyncResult = interface
    function GetIsCompleted: Boolean;
    property IsCompleted: Boolean read GetIsCompleted;
  end;

  /// <summary>
  /// TAsyncCallback
  /// </summary>
  TAsyncCallback = reference to procedure(const result: IAsyncResult);

  //*)

  {$ENDREGION}


  {$REGION 'Global Routines'}

  /// <summary>
  /// Determines whether a specified file exists. It will raise an
  /// EFileNotFoundException exception when not found.
  /// </summary>
  procedure CheckFileExists(const fileName: string);

  /// <summary>
  /// Determines whether a specified directory exists. It will raise an
  /// EDirectoryNotFoundException exception when not found.
  /// </summary>
  procedure CheckDirectoryExists(const directory: string);

  function CreateCallback(objectAddress: TObject; methodAddress: Pointer): TCallbackFunc;

  /// <summary>
  /// SplitString
  /// </summary>
  /// <remarks>
  /// Each element of separator defines a separate delimiter character. If two
  /// delimiters are adjacent, or a delimiter is found at the beginning or end
  /// of the buffer, the corresponding array element contains Empty.
  /// </remarks>
  function SplitString(const buffer: string; const separators: TSysCharSet;
    removeEmptyEntries: Boolean = False): TStringDynArray; overload;
  function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
    removeEmptyEntries: Boolean = False): TStringDynArray; overload;
  function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
    removeEmptyEntries: Boolean = False): TStringDynArray; overload;

  /// <summary>
  /// Returns a string array that contains the substrings in the buffer that are
  /// delimited by null char (#0) and ends with an additional null char.
  /// e.g.
  /// </summary>
  /// <example>
  /// <code>
  /// procedure TestSplitNullTerminatedStrings;
  /// var
  ///   buffer: string;
  ///   strings: TStringDynArray;
  ///   s: string;
  /// begin
  ///   buffer := 'C:'#0'D:'#0'E:'#0#0;
  ///   strings := SplitNullTerminatedStrings(PChar(buffer));
  ///   for s in strings do
  ///   begin
  ///     Writeln(s);
  ///   end;
  /// end;
  /// </code>
  /// </example>
  function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;

  /// <summary>
  /// Synchronize
  /// </summary>
  procedure Synchronize(threadProc: TThreadProcedure);

  /// <summary>
  /// Try getting property information of an object.
  /// </summary>
  function TryGetPropInfo(const instance: TObject; const propertyName: string;
    out propInfo: PPropInfo): Boolean;

  /// <summary>
  /// Try setting focus to a control.
  /// </summary>
  function TryFocusControl(control: TWinControl): Boolean;

  /// <summary>
  /// Determines if a variant is null or empty. The parameter "trimeWhiteSpace"
  /// is an option only for string variant.
  /// </summary>
  function VarIsNullOrEmpty(const value: Variant; trimWhiteSpace: Boolean = False): Boolean;

  /// <summary>
  /// Obtains a mutual-exclusion lock for the given object, executes a procedure
  /// and then release the lock.
  /// </summary>
  procedure Lock(obj: TObject; proc: TProc); inline;

  /// <summary>
  /// Update strings by calling BeginUpdate and EndUpdate
  /// </summary>
  procedure UpdateStrings(strings: TStrings; proc: TProc);

  /// <summary>
  /// Walkthrough all child controls in tab-order, recursively.
  /// </summary>
  procedure EnumerateControls(container: TWinControl; callback: TProc<TWinControl>); overload;
  procedure EnumerateControls(container: TWinControl; predicate: TPredicate<TWinControl>; callback: TProc<TWinControl>); overload;

  /// <summary>
  /// Walkthrough all dataset records from the first one.
  /// </summary>
  procedure EnumerateDataSet(dataSet: TDataSet; proc: TProc); experimental;

  {$ENDREGION}


  {$REGION 'Constants'}

const
  OneKB: Int64 = 1024;            // 1KB = 1024 Bytes
  OneMB: Int64 = 1048576;         // 1MB = 1024 KB
  OneGB: Int64 = 1073741824;      // 1GB = 1024 MB
  OneTB: Int64 = 1099511627776;   // 1TB = 1024 GB

const
  OSVersionTypeStrings: array[TOSVersionType] of string = (
    SUnknownDescription,
    'Microsoft Windows 95',           // DEPRECATED
    'Microsoft Windows 98',           // DEPRECATED
    'Microsoft Windows ME',           // DEPRECATED
    'Microsoft Windows NT 3.51',      // DEPRECATED
    'Microsoft Windows NT 4',         // DEPRECATED
    'Microsoft Windows Server 2000',
    'Microsoft Windows XP',
    'Microsoft Windows Server 2003',
    'Microsoft Windows Vista',
    'Microsoft Windows Server 2008',
    'Microsoft Windows 7'
  );

const
  SpecialFolderCSIDLs: array[TSpecialFolder] of Integer = (
    CSIDL_DESKTOP,                  // <desktop>
    CSIDL_INTERNET,                 // Internet Explorer (icon on desktop)
    CSIDL_PROGRAMS,                 // Start Menu\Programs
    CSIDL_CONTROLS,                 // My Computer\Control Panel
    CSIDL_PRINTERS,                 // My Computer\Printers
    CSIDL_PERSONAL,                 // My Documents.  This is equivalent to CSIDL_MYDOCUMENTS in XP and above
    CSIDL_FAVORITES,                // <user name>\Favorites
    CSIDL_STARTUP,                  // Start Menu\Programs\Startup
    CSIDL_RECENT,                   // <user name>\Recent
    CSIDL_SENDTO,                   // <user name>\SendTo
    CSIDL_BITBUCKET,                // <desktop>\Recycle Bin
    CSIDL_STARTMENU,                // <user name>\Start Menu
    CSIDL_MYDOCUMENTS,              // logical "My Documents" desktop icon
    CSIDL_MYMUSIC,                  // "My Music" folder
    CSIDL_MYVIDEO,                  // "My Video" folder
    CSIDL_DESKTOPDIRECTORY,         // <user name>\Desktop
    CSIDL_DRIVES,                   // My Computer
    CSIDL_NETWORK,                  // Network Neighborhood (My Network Places)
    CSIDL_NETHOOD,                  // <user name>\nethood
    CSIDL_FONTS,                    // windows\fonts
    CSIDL_TEMPLATES,
    CSIDL_COMMON_STARTMENU,         // All Users\Start Menu
    CSIDL_COMMON_PROGRAMS,          // All Users\Start Menu\Programs
    CSIDL_COMMON_STARTUP,           // All Users\Startup
    CSIDL_COMMON_DESKTOPDIRECTORY,  // All Users\Desktop
    CSIDL_APPDATA,                  // <user name>\Application Data
    CSIDL_PRINTHOOD,                // <user name>\PrintHood
    CSIDL_LOCAL_APPDATA,            // <user name>\Local Settings\Application Data (non roaming)
    CSIDL_ALTSTARTUP,               // non localized startup
    CSIDL_COMMON_ALTSTARTUP,        // non localized common startup
    CSIDL_COMMON_FAVORITES,
    CSIDL_INTERNET_CACHE,
    CSIDL_COOKIES,
    CSIDL_HISTORY,
    CSIDL_COMMON_APPDATA,           // All Users\Application Data
    CSIDL_WINDOWS,                  // GetWindowsDirectory()
    CSIDL_SYSTEM,                   // GetSystemDirectory()
    CSIDL_PROGRAM_FILES,            // C:\Program Files
    CSIDL_MYPICTURES,               // C:\Program Files\My Pictures
    CSIDL_PROFILE,                  // USERPROFILE
    CSIDL_SYSTEMX86,                // x86 system directory on RISC
    CSIDL_PROGRAM_FILESX86,         // x86 C:\Program Files on RISC
    CSIDL_PROGRAM_FILES_COMMON,     // C:\Program Files\Common
    CSIDL_PROGRAM_FILES_COMMONX86,  // x86 C:\Program Files\Common on RISC
    CSIDL_COMMON_TEMPLATES,         // All Users\Templates
    CSIDL_COMMON_DOCUMENTS,         // All Users\Documents
    CSIDL_COMMON_ADMINTOOLS,        // All Users\Start Menu\Programs\Administrative Tools
    CSIDL_ADMINTOOLS,               // <user name>\Start Menu\Programs\Administrative Tools
    CSIDL_CONNECTIONS,              // Network and Dial-up Connections
    CSIDL_COMMON_MUSIC,             // All Users\My Music
    CSIDL_COMMON_PICTURES,          // All Users\My Pictures
    CSIDL_COMMON_VIDEO,             // All Users\My Video
    CSIDL_RESOURCES,                // Resource Directory
    CSIDL_RESOURCES_LOCALIZED,      // Localized Resource Directory
    CSIDL_COMMON_OEM_LINKS,         // Links to All Users OEM specific apps
    CSIDL_CDBURN_AREA,              // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    CSIDL_COMPUTERSNEARME           // Computers Near Me (computered from Workgroup membership)
  );

  {$ENDREGION}


var
  ApplicationPath: string;
  ApplicationVersion: TVersion;
  ApplicationVersionString: string;

implementation

uses
  ComObj;

var
  ApplicationVersionInfo: TFileVersionInfo;


{$REGION 'Global Routines'}

procedure CheckFileExists(const fileName: string);
begin
  if not FileExists(fileName) then
  begin
    raise EFileNotFoundException.CreateFmt(SFileNotFoundException, [fileName]);
  end;
end;

procedure CheckDirectoryExists(const directory: string);
begin
  if not DirectoryExists(directory) then
  begin
    raise EDirectoryNotFoundException.CreateFmt(SDirectoryNotFoundException, [directory]);
  end;
end;

function CreateCallback(objectAddress: TObject; methodAddress: Pointer): TCallbackFunc;
begin
  Result := TCallback.Create(objectAddress, methodAddress);
end;

function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries);
end;

function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries)
end;

function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
var
  head: PChar;
  tail: PChar;
  p: PChar;

  procedure AppendEntry(buffer: PChar; len: Integer; var strings: TStringDynArray);
  var
    entry: string;
  begin
    SetString(entry, buffer, len);
    if not removeEmptyEntries or (entry <> '') then
    begin
      SetLength(strings, Length(strings) + 1);
      strings[Length(strings) - 1] := entry;
    end;
  end;
begin
  TArgument.CheckRange(len >= 0, 'len');

  if (buffer = nil) or (len = 0) then Exit;
  head := buffer;
  tail := head + len - 1;
  p := head;
  while p <= tail do
  begin
    if CharInSet(p^, separators) then
    begin
      AppendEntry(head, p - head, Result);
      head := StrNextChar(p);
    end;
    if p = tail then
    begin
      AppendEntry(head, p - head + 1, Result);
    end;
    p := StrNextChar(p);
  end;
end;

function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
var
  p: PChar;
  entry: string;
begin
  if (buffer = nil) or (buffer^ = #0) then Exit;
  p := buffer;
  while p^ <> #0 do
  begin
    entry := p;
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)-1] := entry;
    Inc(p, Length(entry) + 1);  // Jump to the next entry
  end;
end;

procedure Synchronize(threadProc: TThreadProcedure);
begin
  TThread.Synchronize(nil, threadProc);
end;

function TryGetPropInfo(const instance: TObject; const propertyName: string;
  out propInfo: PPropInfo): Boolean;
begin
  TArgument.CheckNotNull(instance, 'instance');

  propInfo := GetPropInfo(instance, propertyName);
  Result := propInfo <> nil;
end;

function TryFocusControl(control: TWinControl): Boolean;
begin
  TArgument.CheckNotNull(control, 'control');

  Result := control.Showing and control.CanFocus;
  if Result then
  begin
    control.SetFocus;
  end;
end;

function VarIsNullOrEmpty(const value: Variant; trimWhiteSpace: Boolean): Boolean;
var
  s: string;
begin
  Result := VarIsNull(value) or VarIsEmpty(value);
  if not Result and trimWhiteSpace and VarIsStr(value) then
  begin
    s := VarToStrDef(value, '');
    s := Trim(s);
    Result := (s = '');
  end;
end;

procedure Lock(obj: TObject; proc: TProc);
begin
  TArgument.CheckNotNull(obj, 'obj');
  TArgument.CheckNotNull(Assigned(proc), 'proc');

  System.MonitorEnter(obj);
  try
    proc;
  finally
    System.MonitorExit(obj);
  end;
end;

procedure UpdateStrings(strings: TStrings; proc: TProc);
begin
  TArgument.CheckNotNull(strings, 'strings');
  TArgument.CheckNotNull(Assigned(proc), 'proc');

  strings.BeginUpdate;
  try
    strings.Clear;
    proc;
  finally
    strings.EndUpdate;
  end;
end;

procedure EnumerateControls(container: TWinControl; callback: TProc<TWinControl>);
begin
  EnumerateControls(container, nil, callback);
end;

procedure EnumerateControls(container: TWinControl;
  predicate: TPredicate<TWinControl>; callback: TProc<TWinControl>); overload;
var
  list: TList;
  i: Integer;
begin
  TArgument.CheckNotNull(container, 'container');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  list := TList.Create;
  try
    container.GetTabOrderList(list);
    if Assigned(predicate) then
    begin
      for i := 0 to list.Count - 1 do
      begin
        if predicate(list[i]) then
          callback(TWinControl(list[i]));
      end;
    end
    else
    begin
      for i := 0 to list.Count - 1 do
      begin
        callback(TWinControl(list[i]));
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure EnumerateDataSet(dataSet: TDataSet; proc: TProc);
begin
  TArgument.CheckNotNull(dataSet, 'dataSet');
  TArgument.CheckNotNull(Assigned(proc), 'proc');

  dataSet.DisableControls;
  try
    dataSet.First;
    while not dataSet.Eof do
    begin
      proc;
      dataSet.Next;
    end;
  finally
    dataSet.EnableControls;
  end;
end;

{$ENDREGION}


{$REGION 'TInterfaceBase'}

function TInterfaceBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfaceBase._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfaceBase._Release: Integer;
begin
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TArgument'}

class procedure TArgument.DoCheckArrayIndex(const length, index: Integer);
begin
  DoCheckIndex(length, index, 0);
end;

class procedure TArgument.DoCheckArrayRange(const length, startIndex,
  count: Integer);
begin
  TArgument.DoCheckRange(length, startIndex, count, 0);
end;

class procedure TArgument.DoCheckStringIndex(const length, index: Integer);
begin
  DoCheckIndex(length, index, 1);
end;

class procedure TArgument.DoCheckStringRange(const length, startIndex,
  count: Integer);
begin
  TArgument.DoCheckRange(length, startIndex, count, 1);
end;

class procedure TArgument.DoCheckIndex(const length, index, indexBase: Integer);
const
  IndexArgName = 'index';
begin
  if (index < indexBase) or (index > length - indexBase - 1) then
  begin
    TArgument.RaiseArgumentOutOfRangeException(IndexArgName);
  end;
end;

class procedure TArgument.DoCheckRange(const length, startIndex,
  count, indexBase: Integer);
const
  StartIndexArgName = 'startIndex';
  CountArgName = 'count';
begin
  TArgument.CheckRange(
    (startIndex >= indexBase) and (startIndex <= indexBase + length - 1),
    StartIndexArgName
  );
  TArgument.CheckRange(count >= 0, CountArgName);
  if count > 0 then
  begin
    TArgument.CheckRange(startIndex + count <= indexBase + length, CountArgName);
  end;
end;

class procedure TArgument.CheckTrue(condition: Boolean;
  const argumentName: string);
begin
  if not condition then
  begin
    TArgument.RaiseArgumentException(argumentName);
  end;
end;

class procedure TArgument.CheckFalse(condition: Boolean;
  const argumentName: string);
begin
  if condition then
  begin
    TArgument.RaiseArgumentException(argumentName);
  end;
end;

class procedure TArgument.CheckNotNull(condition: Boolean;
  const argumentName: string);
begin
  if not condition then
  begin
    TArgument.RaiseArgumentNullException(argumentName);
  end;
end;

class procedure TArgument.CheckNotNull(p: Pointer; const argumentName: string);
begin
  TArgument.CheckNotNull(p <> nil, argumentName);
end;

class procedure TArgument.CheckNotNull(const intf: IInterface;
  const argumentName: string);
begin
  TArgument.CheckNotNull(intf <> nil, argumentName);
end;

class procedure TArgument.CheckNotNull(obj: TObject;
  const argumentName: string);
begin
  TArgument.CheckNotNull(obj <> nil, argumentName);
end;

class procedure TArgument.CheckEnum<T>(const value: T;
  const argumentName: string);
var
  intValue: Integer;
begin
  intValue := TEnum.ConvertToInteger<T>(value);    // No Range Check
  TArgument.CheckEnum<T>(intValue, argumentName);
end;

class procedure TArgument.CheckEnum<T>(const value: Integer;
  const argumentName: string);
var
  msg: string;
begin
  if not TEnum.IsValid<T>(value) then
  begin
    msg := Format(
      SInvalidEnumArgument,
      [argumentName, TRtti.GetTypeName<T>, value]
    );
    raise EInvalidEnumArgumentException.Create(msg);
  end;
end;

class procedure TArgument.CheckRange(condition: Boolean;
  const argumentName: string);
begin
  if not condition then
  begin
    TArgument.RaiseArgumentOutOfRangeException(argumentName);
  end;
end;

class procedure TArgument.CheckRange(const buffer: array of Byte;
  const startIndex, count: Integer);
begin
  TArgument.DoCheckArrayRange(Length(buffer), startIndex, count);
end;

class procedure TArgument.CheckRange(const buffer: array of Char;
  const startIndex, count: Integer);
begin
  TArgument.DoCheckArrayRange(Length(buffer), startIndex, count);
end;

class procedure TArgument.CheckRange(const buffer: array of Byte;
  const index: Integer);
begin
  TArgument.DoCheckArrayIndex(Length(buffer), index);
end;

class procedure TArgument.CheckRange(const buffer: array of Char;
  const index: Integer);
begin
  TArgument.DoCheckArrayIndex(Length(buffer), index);
end;

class procedure TArgument.CheckRange(const s: string; const index: Integer);
begin
  TArgument.DoCheckStringIndex(Length(s), index);
end;

class procedure TArgument.CheckRange(const s: string; const startIndex,
  count: Integer);
begin
  TArgument.DoCheckStringRange(Length(s), startIndex, count);
end;

class procedure TArgument.CheckRange(const s: WideString; const index: Integer);
begin
  TArgument.DoCheckStringIndex(Length(s), index);
end;

class procedure TArgument.CheckRange(const s: WideString; const startIndex,
  count: Integer);
begin
  TArgument.DoCheckStringRange(Length(s), startIndex, count);
end;

class procedure TArgument.CheckRange(const s: RawByteString;
  const index: Integer);
begin
  TArgument.DoCheckStringIndex(Length(s), index);
end;

class procedure TArgument.CheckRange(const s: RawByteString; const startIndex,
  count: Integer);
begin
  TArgument.DoCheckStringRange(Length(s), startIndex, count);
end;

class procedure TArgument.RaiseArgumentException(const argumentName: string);
begin
  raise EArgumentException.Create(argumentName);
end;

class procedure TArgument.RaiseArgumentNullException(
  const argumentName: string);
begin
  raise EArgumentNullException.Create(argumentName);
end;

class procedure TArgument.RaiseArgumentOutOfRangeException(
  const argumentName: string);
begin
  raise EArgumentOutOfRangeException.Create(argumentName);
end;

class procedure TArgument.RaiseInvalidEnumArgumentException(
  const argumentName: string);
begin
  raise EInvalidEnumArgumentException.Create(argumentName);
end;

{$ENDREGION}


{$REGION 'TBuffer'}

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
//    raise EInsufficientMemoryException.Create(SInsufficientMemoryException);
//  end;
//  Move(fBytes[0], dest[index], Size);
//end;

function TBuffer.Copy(startIndex, count: Integer): TBytes;
begin
  Result := Mid(startIndex, count);
end;

function TBuffer.Left(count: Integer): TBytes;
begin
  TArgument.CheckRange((count >= 0) and (count <= Size), 'count');
  Result := Mid(0, count);
end;

function TBuffer.Mid(startIndex, count: Integer): TBytes;
begin
  TArgument.CheckRange(fBytes, startIndex, count);
  SetLength(Result, count);
  Move(fBytes[startIndex], Result[0], count);
end;

function TBuffer.Right(count: Integer): TBytes;
begin
  TArgument.CheckRange((count >= 0) and (count <= Size), 'count');
  Result := Mid(Size - count, count);
end;

function TBuffer.EnsureSize(size: Integer): TBytes;
begin
  Result := Self.EnsureSize(size, 0);
end;

function TBuffer.EnsureSize(size: Integer; value: Byte): TBytes;
begin
  if Self.Size < size then
  begin
    SetLength(Result, size);
    Move(fBytes[0], Result[0], Self.Size);
    FillChar(Result[Self.Size], size - Self.Size, value);
  end
  else
  begin
    Result := Self.ToBytes;
  end;
end;

function TBuffer.EnsureSize(size: Integer; value: AnsiChar): TBytes;
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

function TBuffer.ToString: string;
begin
  SetLength(Result, Length(fBytes) div SizeOf(Char));
  Move(fBytes[0], Result[1], Length(fBytes));
end;

function TBuffer.ToWideString: WideString;
begin
  SetLength(Result, Length(fBytes) div SizeOf(Char));
  Move(fBytes[0], Result[1], Length(fBytes));
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
  Result := TBuffer.ConvertToHexString(Memory, Size)
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
  Result := Self.Size = 0;
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

class operator TBuffer.Implicit(const value: TBytes): TBuffer;
begin
  Result.fBytes := value;
end;

class operator TBuffer.Implicit(const value: TBuffer): TBytes;
begin
  Result := value.fBytes;
end;

class operator TBuffer.Explicit(const value: TBytes): TBuffer;
begin
  Result.fBytes := value;
end;

class operator TBuffer.Explicit(const value: TBuffer): TBytes;
begin
  Result := value.fBytes;
end;

class operator TBuffer.Add(const left, right: TBuffer): TBuffer;
begin
  SetLength(Result.fBytes, left.Size + right.Size);
  Move(left.fBytes[0], Result.fBytes[0], left.Size);
  Move(right.fBytes[0], Result.fBytes[left.Size], right.Size);
end;

class operator TBuffer.Equal(const left, right: TBuffer): Boolean;
begin
  Result := left.Equals(right);
end;

class operator TBuffer.NotEqual(const left, right: TBuffer): Boolean;
begin
  Result := not left.Equals(right);
end;

{$ENDREGION}


{$REGION 'TEnum'}

class function TEnum.GetEnumTypeInfo<T>: PTypeInfo;
begin
  TRtti.CheckTypeKind<T>(tkEnumeration);
  Result := TRtti.GetTypeInfo<T>;
end;

class function TEnum.GetEnumTypeData<T>: PTypeData;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := TEnum.GetEnumTypeInfo<T>;
  Result := GetTypeData(typeInfo);
end;

class function TEnum.ConvertToInteger<T>(const value: T): Integer;
begin
  Result := 0;  // *MUST* initialize Result
  Move(value, Result, SizeOf(T));
end;

class function TEnum.IsValid<T>(const value: Integer): Boolean;
var
  data: PTypeData;
begin
  TRtti.CheckTypeKind<T>(tkEnumeration);
  data := TRtti.GetTypeData<T>;
  Assert(data <> nil, 'data must not be nil.');
  Result := (value >= data.MinValue) and (value <= data.MaxValue);
end;

class function TEnum.IsValid<T>(const value: T): Boolean;
var
  intValue: Integer;
begin
  intValue := TEnum.ConvertToInteger<T>(value);
  Result := TEnum.IsValid<T>(intValue);
end;

class function TEnum.GetName<T>(const value: Integer): string;
var
  info: PTypeInfo;
begin
  TArgument.CheckEnum<T>(value, 'value');

  info := GetEnumTypeInfo<T>;
  Result := GetEnumName(info, value);
end;

class function TEnum.GetName<T>(const value: T): string;
var
  intValue: Integer;
begin
  intValue := TEnum.ConvertToInteger<T>(value);
  Result := TEnum.GetName<T>(intValue);
end;

class function TEnum.GetNames<T>: TStringDynArray;
var
  typeData: PTypeData;
  p: PShortString;
  i: Integer;
begin
  typeData := TEnum.GetEnumTypeData<T>;
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  p := @typedata.NameList;
  for i := 0 to High(Result) do
  begin
    Result[i] := UTF8ToString(p^);
    Inc(Integer(p), Length(p^)+1);
  end;
end;

class function TEnum.GetValue<T>(const value: T): Integer;
begin
  TArgument.CheckEnum<T>(value, 'value');

  Result := TEnum.ConvertToInteger<T>(value);
end;

class function TEnum.GetValue<T>(const value: string): Integer;
var
  temp: T;
begin
  temp := TEnum.Parse<T>(value);
  Result := TEnum.ConvertToInteger<T>(temp);
end;

class function TEnum.GetValues<T>: TIntegerDynArray;
var
  typeData: PTypeData;
  i: Integer;
begin
  typeData := TEnum.GetEnumTypeData<T>;
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  for i := 0 to High(Result) do
  begin
    Result[i] := i;
  end;
end;

class function TEnum.TryParse<T>(const value: Integer; out enum: T): Boolean;
begin
  Result := TEnum.IsValid<T>(value);
  if Result then
    Move(value, enum, SizeOf(T));
end;

class function TEnum.TryParse<T>(const value: string; out enum: T): Boolean;
var
  pInfo: PTypeInfo;
  intValue: Integer;
begin
  pInfo := TEnum.GetEnumTypeInfo<T>;
  intValue := GetEnumValue(pInfo, value);
  Result := TEnum.TryParse<T>(intValue, enum);
end;

class function TEnum.Parse<T>(const value: Integer): T;
begin
  if not TEnum.TryParse<T>(value, Result) then
    raise EFormatException.CreateFmt(SIncorrectFormat, [IntToStr(value)]);
end;

class function TEnum.Parse<T>(const value: string): T;
begin
  if not TEnum.TryParse<T>(value, Result) then
    raise EFormatException.CreateFmt(SIncorrectFormat, [value]);
end;

{$ENDREGION}


{$REGION 'TNullable<T>'}

constructor TNullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := fCHasValue;
end;

constructor TNullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    fValue := v.AsType<T>;
    fHasValue := fCHasValue;
  end;
end;

function TNullable<T>.GetHasValue: Boolean;
begin
  Result := Length(fHasValue) > 0;
end;

function TNullable<T>.GetValue: T;
begin
  if not HasValue then
  begin
    raise EInvalidOperation.Create(SNullableTypeHasNoValue);
  end;
  Result := fValue;
end;

function TNullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := value
  else
    Result := Default(T);
end;

function TNullable<T>.GetValueOrDefault(const default: T): T;
begin
  if HasValue then
    Result := value
  else
    Result := default;
end;

class operator TNullable<T>.Implicit(const value: T): TNullable<T>;
begin
  Result := TNullable<T>.Create(value);
end;

class operator TNullable<T>.Implicit(const value: TNullable<T>): T;
begin
  Result := value.value;
end;

class operator TNullable<T>.Implicit(const value: TNullable<T>): Variant;
var
  v: TValue;
begin
  if value.HasValue then
  begin
    v := TValue.From<T>(value.Value);
    Result := v.AsVariant;
  end
  else
  begin
    Result := Null;
  end;
end;

class operator TNullable<T>.Implicit(const value: Variant): TNullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result := TNullable<T>.Create(v.AsType<T>)
  end
  else
  begin
    Result.fHasValue := '';
  end;
end;

class operator TNullable<T>.Implicit(value: Pointer): TNullable<T>;
begin
  if value = nil then
  begin
    Result.fHasValue := '';
  end
  else
  begin
    raise EInvalidOperation.Create(SCannotAssignPointerToNullable);
  end;
end;

class operator TNullable<T>.Explicit(const value: TNullable<T>): T;
begin
  Result := value.value;
end;

{$ENDREGION}


{$REGION 'TObjectHolder<T>'}

constructor TObjectHolder<T>.Create(obj: T);
begin
  inherited Create;
  fObject := obj;
end;

destructor TObjectHolder<T>.Destroy;
begin
  fObject.Free;
  inherited Destroy;
end;

function TObjectHolder<T>.Invoke: T;
begin
  Result := fObject;
end;

{$ENDREGION}


{$REGION 'TValueProvider<T>'}

constructor TValueProvider<T>.Create;
begin
  Create(Default(T), nil);
end;

constructor TValueProvider<T>.Create(const value: T);
begin
  Create(value, nil);
end;

constructor TValueProvider<T>.Create(const value: T;
  cleanUpProc: TCleanUpProc<T>);
begin
  inherited Create;
  fValue := value;
  fCleanUpProc := cleanUpProc;
end;

destructor TValueProvider<T>.Destroy;
begin
  if Assigned(fCleanUpProc) then
    fCleanUpProc(fValue);
  inherited Destroy;
end;

function TValueProvider<T>.GetValue: T;
begin
  Result := fValue;
end;

function TValueProvider<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

procedure TValueProvider<T>.SetValue(const value: T);
begin
  if IsReadOnly then
  begin
    raise ENotSupportedException.Create(SCannotModifyReadOnlyValue);
  end;
  fValue := value;
end;

{$ENDREGION}


{$REGION 'TValueHolder<T>'}

constructor TValueHolder<T>.Create(const value: T);
begin
  Create(value, nil);
end;

constructor TValueHolder<T>.Create(const value: T; cleanUpProc: TCleanUpProc<T>);
var
  provider: IValueProvider<T>;
begin
  provider := TValueProvider<T>.Create(value, cleanUpProc);
  Create(provider);
end;

constructor TValueHolder<T>.Create(const valueProvider: IValueProvider<T>);
begin
  fProvider := valueProvider;
end;

procedure TValueHolder<T>.ProviderNeeded;
begin
  if fProvider = nil then
  begin
    fProvider := TValueProvider<T>.Create;
  end;
end;

function TValueHolder<T>.GetValue: T;
begin
  if fProvider <> nil then
  begin
    Result := fProvider.Value;
  end
  else
  begin
    Result := Default(T);
  end;
end;

procedure TValueHolder<T>.SetValue(const Value: T);
begin
  ProviderNeeded;
  fProvider.Value := value;
end;

class operator TValueHolder<T>.Implicit(const value: T): TValueHolder<T>;
begin
  Result := TValueHolder<T>.Create(value);
end;

class operator TValueHolder<T>.Implicit(const valueHolder: TValueHolder<T>): T;
begin
  Result := valueHolder.Value;
end;

class operator TValueHolder<T>.Implicit(
  const valueProvider: IValueProvider<T>): TValueHolder<T>;
begin
  Result := TValueHolder<T>.Create(valueProvider);
end;

class operator TValueHolder<T>.Implicit(
  const valueHolder: TValueHolder<T>): IValueProvider<T>;
begin
  valueHolder.ProviderNeeded;
  Result := valueHolder.fProvider;
end;

{$ENDREGION}


{$REGION 'TRtti'}

class procedure TRtti.CheckTypeKind<T>(const typeKind: TypInfo.TTypeKind);
begin
  TRtti.CheckTypeKind<T>([typeKind]);
end;

class procedure TRtti.CheckTypeKind<T>(const typeKinds: TypInfo.TTypeKinds);
var
  typeInfo: PTypeInfo;
begin
  typeInfo := TRtti.GetTypeInfo<T>;
  if not (typeInfo.Kind in typeKinds) then
    raise ERttiException.CreateFmt(SUnexpectedTypeKind, [TRtti.GetTypeName<T>]);
end;

class function TRtti.GetTypeName<T>: string;
begin
  Result := TypInfo.GetTypeName(TRtti.GetTypeInfo<T>);
end;

class function TRtti.GetTypeKind<T>: TypInfo.TTypeKind;
begin
  Result := TRtti.GetTypeInfo<T>.Kind;
end;

class function TRtti.GetTypeInfo<T>: PTypeInfo;
begin
  Result := System.TypeInfo(T);
  if Result = nil then
    raise ERttiException.Create(SNoTypeInfo);
end;

class function TRtti.GetFullName(typeInfo: PTypeInfo): string;
begin
  Result := TypInfo.GetTypeName(typeInfo);
end;

class function TRtti.GetTypeData<T>: PTypeData;
var
  info: PTypeInfo;
begin
  info := TRtti.GetTypeInfo<T>;
  Result := TypInfo.GetTypeData(info);
end;

{$ENDREGION}


{$REGION 'TVersion'}

constructor TVersion.Create(const versionString: string);
var
  components: TStringDynArray;
  major: Integer;
  minor: Integer;
  build: Integer;
  reversion: Integer;
begin
  components := SplitString(versionString, ['.']);
  if not (Length(components) in [2..4]) then
  begin
    raise EArgumentException.Create('version');
  end;
  try
    major := StrToInt(components[0]);
    minor := StrToInt(components[1]);
    if Length(components) >= 3 then
    begin
      build := StrToInt(components[2]);
    end
    else
    begin
      build := -1;
    end;
    if Length(components) = 4 then
    begin
      reversion := StrToInt(components[3]);
    end
    else
    begin
      reversion := -1;
    end;
  except on e: Exception do
    raise EFormatException.Create(e.Message);
  end;
  InternalCreate(Length(components), major, minor, build, reversion);
end;

constructor TVersion.Create(major, minor: Integer);
begin
  InternalCreate(2, major, minor, -1, -1);
end;

constructor TVersion.Create(major, minor, build: Integer);
begin
  InternalCreate(3, major, minor, build, -1);
end;

constructor TVersion.Create(major, minor, build, reversion: Integer);
begin
  InternalCreate(4, major, minor, build, reversion);
end;

constructor TVersion.InternalCreate(defined, major, minor, build, reversion: Integer);
begin
  Assert(defined in [2, 3, 4], '"defined" should be in [2, 3, 4].');
  TArgument.CheckRange(IsDefined(major), 'major');
  TArgument.CheckRange(IsDefined(minor), 'minor');
  fMajor := major;
  fMinor := minor;
  case defined of
    2:
    begin
      fBuild := fCUndefined;
      fReversion := fCUndefined;
    end;
    3:
    begin
      TArgument.CheckRange(IsDefined(build), 'build');
      fBuild := build;
      fReversion := fCUndefined;
    end;
    4:
    begin
      TArgument.CheckRange(IsDefined(build), 'build');
      TArgument.CheckRange(IsDefined(reversion), 'reversion');
      fBuild := build;
      fReversion := reversion;
    end;
  end;
end;

function TVersion.IsDefined(const component: Integer): Boolean;
begin
  Result := component <> fCUndefined;
end;

function TVersion.Equals(const version: TVersion): Boolean;
begin
  Result := CompareTo(version) = 0;
end;

function TVersion.CompareComponent(a, b: Integer): Integer;
begin
  if IsDefined(a) and IsDefined(b) then
  begin
    Result := a - b;
  end
  else if IsDefined(a) and not IsDefined(b) then
  begin
    Result := 1;
  end
  else if not IsDefined(a) and IsDefined(b) then
  begin
    Result := -1;
  end
  else
  begin
    Result := 0;
  end;
end;

function TVersion.CompareTo(const version: TVersion): Integer;
begin
  Result := Major - version.Major;
  if Result = 0 then
  begin
    Result := Minor - version.Minor;
    if Result = 0 then
    begin
      Result := CompareComponent(Build, version.Build);
      if Result = 0 then
      begin
        Result := CompareComponent(Reversion, version.Reversion);
      end;
    end;
  end;
end;

function TVersion.ToString: string;
begin
  if not IsDefined(fBuild) then
    Result := ToString(2)
  else if not IsDefined(fReversion) then
    Result := ToString(3)
  else
    Result := ToString(4);
end;

function TVersion.ToString(fieldCount: Integer): string;
begin
  TArgument.CheckRange(fieldCount in [0..4], 'fieldCount');
  case fieldCount of
    0: Result := '';
    1: Result := Format('%d', [major]);
    2: Result := Format('%d.%d', [major, minor]);
    3:
    begin
      TArgument.CheckTrue(IsDefined(build), SIllegalFieldCount);
      Result := Format('%d.%d.%d', [major, minor, build]);
    end;
    4:
    begin
      TArgument.CheckTrue(IsDefined(build) and IsDefined(reversion), SIllegalFieldCount);
      Result := Format('%d.%d.%d.%d', [major, minor, build, reversion]);
    end;
  end;
end;

function TVersion.GetMajorReversion: Int16;
begin
  Result := Reversion shr 16;
end;

function TVersion.GetMinorReversion: Int16;
begin
  Result := Reversion and $0000FFFF;
end;

class operator TVersion.Equal(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) = 0;
end;

class operator TVersion.NotEqual(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) <> 0;
end;

class operator TVersion.GreaterThan(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) > 0;
end;

class operator TVersion.GreaterThanOrEqual(const left,
  right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) >= 0;
end;

class operator TVersion.LessThan(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) < 0;
end;

class operator TVersion.LessThanOrEqual(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) <= 0;
end;

{$ENDREGION}


{$REGION 'TFileVersionInfo'}

constructor TFileVersionInfo.Create(const fileName: string);
var
  block: Pointer;
  fixedFileInfo: PVSFixedFileInfo;
  translations: PTLangAndCodePageArray;
  size: DWORD;
  valueSize: DWORD;
  translationSize: Cardinal;
  translationCount: Integer;
  dummy: DWORD;
begin
  Finalize(Self);
  ZeroMemory(@Self, SizeOf(Self));
  fFileName := fileName;
  CheckFileExists(fFileName);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  UniqueString(fFileName);
  size := GetFileVersionInfoSize(PChar(fFileName), dummy);
  fExists := size <> 0;
  if fExists then
  begin
    block := AllocMem(size);
    try
      Win32Check(Windows.GetFileVersionInfo(
        PChar(fFileName),
        0,
        size,
        block
      ));
      Win32Check(VerQueryValue(
        block,
        '\',
        Pointer(fixedFileInfo),
        valueSize
      ));
      Win32Check(VerQueryValue(
        block,
        '\VarFileInfo\Translation',
        Pointer(translations),
        translationSize
      ));
      fFileVersionNumber := TVersion.Create(
        HiWord(fixedFileInfo.dwFileVersionMS),
        LoWord(fixedFileInfo.dwFileVersionMS),
        HiWord(fixedFileInfo.dwFileVersionLS),
        LoWord(fixedFileInfo.dwFileVersionLS)
      );
      fProductVersionNumber := TVersion.Create(
        HiWord(fixedFileInfo.dwProductVersionMS),
        LoWord(fixedFileInfo.dwProductVersionMS),
        HiWord(fixedFileInfo.dwProductVersionLS),
        LoWord(fixedFileInfo.dwProductVersionLS)
      );
      fFileFlags := fixedFileInfo.dwFileFlags;
      translationCount := translationSize div SizeOf(TLangAndCodePage);
      if translationCount > 0 then
      begin
        LoadVersionResource(
          TFileVersionResource.Create(
            block,
            translations[0].Language,
            translations[0].CodePage
          )
        );
      end;
    finally
      FreeMem(block);
    end;
  end;
end;

class function TFileVersionInfo.GetVersionInfo(
  fileName: string): TFileVersionInfo;
begin
  fileName := Environment.ExpandEnvironmentVariables(fileName);
  Result := TFileVersionInfo.Create(fileName);
end;

procedure TFileVersionInfo.LoadVersionResource(const resource: TFileVersionResource);
begin
  fCompanyName := resource.ReadString('CompanyName');
  fFileDescription := resource.ReadString('FileDescription');
  fFileVersion := resource.ReadString('FileVersion');
  fInternalName := resource.ReadString('InternalName');
  fLegalCopyright := resource.ReadString('LegalCopyright');
  fLegalTrademarks := resource.ReadString('LegalTrademarks');
  fOriginalFilename := resource.ReadString('OriginalFilename');
  fProductName := resource.ReadString('ProductName');
  fProductVersion := resource.ReadString('ProductVersion');
  fComments := resource.ReadString('Comments');
  fLanguage := Languages.NameFromLocaleID[resource.Language];
end;

function TFileVersionInfo.ToString: string;
begin
  Result := Format(SFileVersionInfoFormat, [
    FileName,
    InternalName,
    OriginalFilename,
    FileVersion,
    FileDescription,
    ProductName,
    ProductVersion,
    BoolToStr(IsDebug, True),
    BoolToStr(IsPatched, True),
    BoolToStr(IsPreRelease, True),
    BoolToStr(IsPrivateBuild, True),
    BoolToStr(IsSpecialBuild, True),
    Language
  ]);
end;

function TFileVersionInfo.GetIsDebug: Boolean;
begin
  Result := (fFileFlags and VS_FF_DEBUG) <> 0;
end;

function TFileVersionInfo.GetIsPatched: Boolean;
begin
  Result := (fFileFlags and VS_FF_PATCHED) <> 0;
end;

function TFileVersionInfo.GetIsPreRelease: Boolean;
begin
  Result := (fFileFlags and VS_FF_PRERELEASE) <> 0;
end;

function TFileVersionInfo.GetIsPrivateBuild: Boolean;
begin
  Result := (fFileFlags and VS_FF_PRIVATEBUILD) <> 0;
end;

function TFileVersionInfo.GetIsSpecialBuild: Boolean;
begin
  Result := (fFileFlags and VS_FF_SPECIALBUILD) <> 0;
end;

{ TFileVersionInfo.TFileVersionData }

constructor TFileVersionInfo.TFileVersionResource.Create(block: Pointer;
  language, codePage: Word);
begin
  fBlock := block;
  fLanguage := language;
  fCodePage := codePage;
end;

function TFileVersionInfo.TFileVersionResource.ReadString(
  const stringName: string): string;
var
  subBlock: string;
  data: PChar;
  len: Cardinal;
const
  SubBlockFormat = '\StringFileInfo\%4.4x%4.4x\%s';   // do not localize
begin
  subBlock := Format(
    SubBlockFormat,
    [fLanguage, fCodePage, stringName]
  );
  data := nil;
  len := 0;
  VerQueryValue(fBlock, PChar(subBlock), Pointer(data), len);
  Result := data;
end;

{$ENDREGION}


{$REGION 'TOperatingSystem'}

class constructor TOperatingSystem.Create;
begin
  fInstance := TOperatingSystem.Create;
end;

class destructor TOperatingSystem.Destroy;
begin
  FreeAndNil(fInstance);
end;

constructor TOperatingSystem.Create;
var
  versionInfo: TOSVersionInfoEx;
begin
  ZeroMemory(@versionInfo, SizeOf(versionInfo));
  versionInfo.dwOSVersionInfoSize := SizeOf(versionInfo);
  Win32Check(Windows.GetVersionEx(versionInfo));
  case versionInfo.dwPlatformId of
    VER_PLATFORM_WIN32s:        fPlatformType := ptWin3x;
    VER_PLATFORM_WIN32_WINDOWS: fPlatformType := ptWin9x;
    VER_PLATFORM_WIN32_NT:      fPlatformType := ptWinNT;
    else fPlatformType := ptUnknown;
  end;
  fProductType := ptInvalid;
  case versionInfo.wProductType of
    VER_NT_WORKSTATION:       fProductType := ptWorkstation;
    VER_NT_DOMAIN_CONTROLLER: fProductType := ptDomainController;
    VER_NT_SERVER:            fProductType := ptServer;
  end;
  fVersion := TVersion.Create(
    versionInfo.dwMajorVersion,
    versionInfo.dwMinorVersion,
    versionInfo.dwBuildNumber
  );
  fVersionType := GetOSVersionType(
    fPlatformType,
    fProductType,
    versionInfo.dwMajorVersion,
    versionInfo.dwMinorVersion
  );
  fServicePack := versionInfo.szCSDVersion;
end;

function TOperatingSystem.GetOSVersionType(platformType: TOSPlatformType;
  productType: TOSProductType; majorVersion, minorVersion: Integer): TOSVersionType;
begin
  Result := vtUnknown;
  case platformType of
    ptWin9x:
    begin
      if majorVersion = 4 then
      case minorVersion of
        0:  Result := vtWin95;
        10: Result := vtWin98;
        90: Result := vtWinMe;
      end;
    end;
    ptWinNT:
    begin
      if (majorVersion = 3) and (minorVersion = 51) then
      begin
        Result := vtWinNT351;
      end
      else if (majorVersion = 4) and (minorVersion = 0) then
      begin
        Result := vtWinNT4;
      end
      else if majorVersion = 5 then
      case minorVersion of
        0: Result := vtWinServer2000;
        1: Result := vtWinXP;
        2: Result := vtWinServer2003;
      end
      else if majorVersion = 6 then
      case minorVersion of
        0:
        begin
          if productType = ptWorkstation then
            Result := vtWinVista
          else
            Result := vtWinServer2008;
        end;
        1:
        begin
          if productType = ptWorkstation then
            Result := vtWin7
          else
            Result := vtWinServer2008;   { TODO: WinServer2008 R2 }
        end;
      end;
    end;
  end;
end;

function TOperatingSystem.ToString: string;
begin
  Result := OSVersionTypeStrings[fVersionType];
  if fVersionType <> vtUnknown then
  begin
    Result := Result + ' Version ' + fVersion.ToString;
    if ServicePack <> '' then
      Result := Result + ' ' + ServicePack;
  end;
end;

function TOperatingSystem.GetIsWin9x: Boolean;
begin
  Result := Self.PlatformType = ptWin9x;
end;

function TOperatingSystem.GetIsWinNT: Boolean;
begin
  Result := Self.PlatformType = ptWinNT;
end;

function TOperatingSystem.GetVersionString: string;
begin
  Result := ToString;
end;

{$ENDREGION}


{$REGION 'TEnvironment'}

class function TEnvironment.GetCommandLineArgs: TStringDynArray;
var
  pArgs: PPWideChar;
  count: Integer;
  i: Integer;
begin
  pArgs := ShellAPI.CommandLineToArgvW(PWideChar(Windows.GetCommandLineW), count);
  if pArgs <> nil then
  try
    SetLength(Result, count);
    for i := 0 to count - 1 do
    begin
      Result[i] := string(pArgs^);
      Inc(pArgs);
    end;
  finally
    Windows.LocalFree(HLocal(pArgs));
  end;
end;

class procedure TEnvironment.GetCommandLineArgs(list: TStrings);
var
  args: TStringDynArray;
begin
  args := GetCommandLineArgs;
  UpdateStrings(list,
    procedure
    var
      i: Integer;
    begin
      for i := 0 to High(args) do
      begin
        list.Add(args[i]);
      end;
    end
  );
end;

class function TEnvironment.GetLogicalDrives: TStringDynArray;
var
  len: Cardinal;
  buffer: string;
begin
  len := Windows.GetLogicalDriveStrings(0, nil);
  SetLength(buffer, len);
  Windows.GetLogicalDriveStrings(len * SizeOf(Char), PChar(buffer));
  Result := SplitNullTerminatedStrings(PChar(buffer));
end;

class procedure TEnvironment.GetLogicalDrives(list: TStrings);
var
  drives: TStringDynArray;
begin
  drives := TEnvironment.GetLogicalDrives;
  UpdateStrings(list,
    procedure
    var
      drive: string;
    begin
      for drive in drives do
      begin
        list.Add(drive);
      end;
    end
  );
end;

function TryGetAccessToken(out hToken: THandle): Boolean;
begin
  Result := Windows.OpenThreadToken(GetCurrentThread, TOKEN_QUERY, TRUE, hToken);
  if not Result and (Windows.GetLastError = ERROR_NO_TOKEN) then
  begin
    Result := Windows.OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);
  end;
end;

class function TEnvironment.GetFolderPath(const folder: TSpecialFolder): string;
var
  pidl : PItemIDList;
  buffer: array[0..MAX_PATH-1] of Char;
  returnCode: HRESULT;
  hasToken: Boolean;
  hToken : THandle;
begin
  if TryGetAccessToken(hToken) then
  try
    returnCode := ShlObj.SHGetFolderLocation(INVALID_HANDLE_VALUE,
      SpecialFolderCSIDLs[folder], hToken, 0, pidl);
    ShlObj.SHGetPathFromIDList(pidl, @buffer[0]);
    Result := buffer;
  finally
    CloseHandle(hToken);
  end;
end;

class function TEnvironment.CreateRegistry(
  target: TEnvironmentVariableTarget; out key: string): TRegistry;
begin
  Assert(target in [evtUser, evtMachine], 'Illegal argument: target');
  Result := TRegistry.Create;
  if target = evtUser then
  begin
    Result.RootKey := HKEY_CURRENT_USER;
    key := 'Environment';
  end
  else
  begin
    Result.RootKey := HKEY_LOCAL_MACHINE;
    key := 'System\CurrentControlSet\Control\Session Manager\Environment';
  end;
end;

class function TEnvironment.GetEnvironmentVariable(
  const variable: string): string;
begin
  Result := TEnvironment.GetEnvironmentVariable(variable, evtProcess);
end;

class function TEnvironment.GetEnvironmentVariable(const variable: string;
  target: TEnvironmentVariableTarget): string;
var
  registry: TRegistry;
  key: string;

  function GetProcessEnvironmentVariable: string;
  var
    len: DWORD;
  begin
    len := Windows.GetEnvironmentVariable(PChar(variable), nil, 0);
    if len > 0 then
    begin
      SetLength(Result, len - 1);
      Windows.GetEnvironmentVariable(PChar(variable), PChar(Result), len);
    end
    else
    begin
      Result := '';
    end;
  end;
begin
  TArgument.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    Result := GetProcessEnvironmentVariable;
    Exit;
  end;
  registry := TEnvironment.CreateRegistry(target, key);
  try
    if registry.OpenKeyReadOnly(key) and registry.ValueExists(variable) then
    begin
      Result := registry.GetDataAsString(variable);
    end
    else
    begin
      Result := '';
    end;
  finally
    registry.Free;
  end;
end;

class procedure TEnvironment.GetProcessEnvironmentVariables(list: TStrings);
var
  p: PChar;
  strings: TStringDynArray;
begin
  Assert(list <> nil, 'list should not be nil.');
  p := Windows.GetEnvironmentStrings;
  try
    strings := SplitNullTerminatedStrings(p);
    UpdateStrings(list,
      procedure
      var
        s: string;
      begin
        for s in strings do
        begin
          if (Length(s) > 0) and (s[1] <> '=') then // Skip entries starts with '='
          begin
            list.Add(s);
          end;
        end;
      end
    );
  finally
    Win32Check(Windows.FreeEnvironmentStrings(p));
  end;
end;

class procedure TEnvironment.GetEnvironmentVariables(list: TStrings);
begin
  TEnvironment.GetEnvironmentVariables(list, evtProcess);
end;

class procedure TEnvironment.GetEnvironmentVariables(list: TStrings;
  target: TEnvironmentVariableTarget);
var
  registry: TRegistry;
  key: string;
  value: string;
  i: Integer;

begin
  TArgument.CheckNotNull(list, 'list');
  TArgument.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    GetProcessEnvironmentVariables(list);
    Exit;
  end;
  registry := TEnvironment.CreateRegistry(target, key);
  try
    if registry.OpenKeyReadOnly(key) then
    begin
      registry.GetValueNames(list);
      for i := 0 to list.Count - 1 do
      begin
        value := registry.GetDataAsString(list[i]);
        list[i] := list[i] + list.NameValueSeparator + value;
      end;
    end;
  finally
    registry.Free;
  end;
end;

class procedure TEnvironment.SetEnvironmentVariable(const variable, value: string);
begin
  TEnvironment.SetEnvironmentVariable(variable, value, evtProcess);
end;

class procedure TEnvironment.SetEnvironmentVariable(const variable,
  value: string; target: TEnvironmentVariableTarget);
var
  registry: TRegistry;
  key: string;
begin
  TArgument.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    Win32Check(Windows.SetEnvironmentVariable(PChar(variable), PChar(value)));
    Exit;
  end;
  registry := TEnvironment.CreateRegistry(target, key);
  try
    registry.Access := KEY_WRITE;
    if not registry.OpenKey(key, False) then
    begin
      raise EArgumentException.Create('variable');
    end;
    if AnsiPos('%', value) > 0 then
      registry.WriteExpandString(variable, value)
    else
      registry.WriteString(variable, value);
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
  finally
    registry.Free;
  end;
end;

class function TEnvironment.ExpandEnvironmentVariables(
  const variable: string): string;
var
  len: Cardinal;
begin
  len := MAX_PATH;
  SetLength(Result, len);
  len := Windows.ExpandEnvironmentStrings(PChar(variable), PChar(Result), len);
  Win32Check(len > 0);
  SetLength(Result, len - 1);
end;

class function TEnvironment.GetCommandLine: string;
begin
  Result := Windows.GetCommandLine;
end;

class function TEnvironment.GetCurrentDirectory: string;
var
  size: DWORD;
begin
  size := Windows.GetCurrentDirectory(0, nil);
  SetLength(Result, size - 1);
  Windows.GetCurrentDirectory(size, PChar(Result));
end;

class function TEnvironment.GetCurrentVersionKey: string;
const
  HKLM_CURRENT_VERSION_NT      = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  HKLM_CURRENT_VERSION_WINDOWS = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
begin
  if OperatingSystem.IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

class function TEnvironment.GetMachineName: string;
var
  size: Cardinal;
begin
  size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, size);
  if GetComputerName(PChar(Result), size) then
  begin
    SetLength(Result, size);
  end;
end;

class function TEnvironment.GetNewLine: string;
begin
  Result := System.sLineBreak;
end;

class function TEnvironment.GetOperatingSystem: TOperatingSystem;
begin
  Result := TOperatingSystem.Instance;
end;

class function TEnvironment.GetProcessorArchitecture: TProcessorArchitecture;
var
  systemInfo: TSystemInfo;
const
  PROCESSOR_ARCHITECTURE_INTEL          = 0;
  PROCESSOR_ARCHITECTURE_AMD64          = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64  = 10;
  PROCESSOR_ARCHITECTURE_IA64           = 6;
begin
  ZeroMemory(@systemInfo, Sizeof(systemInfo));
  Windows.GetSystemInfo(systemInfo);
  case systemInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL:
      Result := paX86;
    PROCESSOR_ARCHITECTURE_IA64:
      Result := paIA64;
    PROCESSOR_ARCHITECTURE_AMD64:
      Result := paAmd64;
    else
      Result := paUnknown;
  end;
end;

class function TEnvironment.GetProcessorCount: Integer;
var
  systemInfo: TSystemInfo;
begin
  ZeroMemory(@systemInfo, Sizeof(systemInfo));
  Windows.GetSystemInfo(systemInfo);
  Result := systemInfo.dwNumberOfProcessors;
end;

class function TEnvironment.GetRegisteredOrganization: string;
begin
  Result := ComObj.GetRegStringValue(
    GetCurrentVersionKey,
    'RegisteredOrganization',  // DO NOT LOCALIZE
    HKEY_LOCAL_MACHINE
  );
end;

class function TEnvironment.GetRegisteredOwner: string;
begin
  Result := ComObj.GetRegStringValue(
    GetCurrentVersionKey,
    'RegisteredOwner',  // DO NOT LOCALIZE
    HKEY_LOCAL_MACHINE
  );
end;

class function TEnvironment.GetSystemDirectory: string;
begin
  Result := TEnvironment.GetFolderPath(sfSystem);
end;

class function TEnvironment.GetUserDomainName: string;
var
  hasToken: Boolean;
  hToken : THandle;
  ptiUser: PSIDAndAttributes;
  cbti   : DWORD;
  snu    : SID_NAME_USE;
  userSize, domainSize: Cardinal;
  userName: string;
begin
  ptiUser := nil;
  userSize := 0;
  domainSize := 0;
  hasToken := Windows.OpenThreadToken(GetCurrentThread, TOKEN_QUERY, TRUE, hToken);
  if not hasToken and (Windows.GetLastError = ERROR_NO_TOKEN) then
  begin
    hasToken := Windows.OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);
  end;
  if hasToken then
  try
    Windows.GetTokenInformation(hToken, TokenUser, nil, 0, cbti);
    ptiUser := AllocMem(cbti);
    if Windows.GetTokenInformation(hToken, TokenUser, ptiUser, cbti, cbti) then
    begin
      if not Windows.LookupAccountSid(nil, ptiUser.Sid, nil, userSize, nil, domainSize, snu) and
        (Windows.GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        SetLength(userName, userSize - 1);
        SetLength(Result, domainSize - 1);
        Win32Check(Windows.LookupAccountSid(nil, ptiUser.Sid, PChar(userName), userSize,
          PChar(Result), domainSize, snu));
      end;
    end;
  finally
    Windows.CloseHandle(hToken);
    FreeMem(ptiUser);
  end;
end;

class function TEnvironment.GetUserInteractive: Boolean;
begin
  { TODO: UserInteractive }
  Result := True;
end;

class function TEnvironment.GetUserName: string;
var
  size: Cardinal;
begin
  size := 255;
  SetLength(Result, size);
  Win32Check(Windows.GetUserName(PChar(Result), size));
  SetLength(Result, size - 1);
end;

/// http://www.gumpi.com/Blog/2007/10/02/EKON11PromisedEntry3.aspx
/// <author>Daniel Wischnewski</author>
class function TEnvironment.GetIsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_ENABLED = $00000004;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result   := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  if bSuccess then
  begin
    GetTokenInformation(hAccessToken, TokenGroups, nil, 0, dwInfoBufferSize);
    ptgGroups := GetMemory(dwInfoBufferSize);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, dwInfoBufferSize, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
      for x := 0 to ptgGroups.GroupCount - 1 do
      begin
        if (SE_GROUP_ENABLED = (ptgGroups.Groups[x].Attributes and SE_GROUP_ENABLED)) and EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          Break;
        end;
      end;
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

class function TEnvironment.GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;

class procedure TEnvironment.SetCurrentDirectory(const value: string);
begin
  Win32Check(Windows.SetCurrentDirectory(PChar(value)));
end;

{$ENDREGION}


{$REGION 'TServiceController'}

constructor TServiceController.Create(const serviceName: string);
begin
  Create(serviceName, '');
end;

constructor TServiceController.Create(const serviceName, machineName: string);
begin
  Create(serviceName, machineName, fCInvalidServiceHandle);
end;

constructor TServiceController.Create(const serviceName,
  machineName: string; const managerHandle: TValueHolder<THandle>);
begin
  fServiceName := serviceName;
  fMachineName := machineName;
  fManagerHandle := managerHandle;
  fIsLoaded := False;
end;

class procedure TServiceController.GetManagerHandleHolder(
  const machineName: string; out handleHolder: TValueHolder<THandle>);
var
  handle: THandle;
begin
  handle := OpenSCManager(
    PChar(machineName),
    SERVICES_ACTIVE_DATABASE,
    SC_MANAGER_ALL_ACCESS
  );
  Win32Check(handle <> fCInvalidServiceHandle);
  GetHandleHolder(handle, handleHolder);
end;

class procedure TServiceController.GetHandleHolder(handle: THandle;
  out valueHolder: TValueHolder<THandle>);
begin
  valueHolder := TValueHolder<THandle>.Create(
    handle,
    procedure(var serviceHandle: THandle)
    begin
      if serviceHandle <> fCInvalidServiceHandle then
      begin
        Win32Check(CloseServiceHandle(serviceHandle));
      end;
      serviceHandle := fCInvalidServiceHandle;
    end
  );
end;

class procedure TServiceController.InternalEnumServices(const machineName: string;
  serviceKind: TServiceKind; out services: TArray<TServiceController>);
var
  managerHandle: TValueHolder<THandle>;
  bufferSize: DWORD;
  bytesNeeded: DWORD;
  servicesReturned: DWORD;
  resumeHandle: THandle;
  returnValue: BOOL;
  serviceName: string;
  serviceArray, pService: PEnumServiceStatusProcess;
  i: Integer;
const
  serviceTypes: array[TServiceKind] of Cardinal = (
    SERVICE_DRIVER,
    SERVICE_WIN32
  );
begin
  TServiceController.GetManagerHandleHolder(machineName, managerHandle);
  resumeHandle := 0;  // Set resumeHandle to zero the first time
  returnValue := EnumServicesStatusEx(
    managerHandle,
    SC_ENUM_PROCESS_INFO,
    serviceTypes[serviceKind],
    SERVICE_STATE_ALL,
    nil,
    0,
    bytesNeeded,
    servicesReturned,
    resumeHandle,
    nil
  );
  if not returnValue and (GetLastError <> ERROR_MORE_DATA) then
  begin
    RaiseLastOSError;
  end;
  bufferSize := bytesNeeded;
  serviceArray := AllocMem(bytesNeeded);
  try
    returnValue := EnumServicesStatusEx(
      managerHandle,
      SC_ENUM_PROCESS_INFO,
      serviceTypes[serviceKind],
      SERVICE_STATE_ALL,
      PByte(serviceArray),
      bufferSize,
      bytesNeeded,
      servicesReturned,
      resumeHandle,
      nil
    );
    Win32Check(returnValue);
    SetLength(services, servicesReturned);
    pService := serviceArray;
    for i := 0 to servicesReturned - 1 do
    begin
      serviceName := pService.lpServiceName;
      services[i] := TServiceController.Create(serviceName, machineName, managerHandle);
      Inc(pService);
    end;
  finally
    FreeMem(serviceArray);
  end;
end;

procedure TServiceController.ManagerHandleNeeded;
begin
  if fManagerHandle.Value = fCInvalidServiceHandle then
  begin
    GetManagerHandleHolder(fMachineName, fManagerHandle);
    Assert(fManagerHandle.Value <> fCInvalidServiceHandle);
  end;
end;

procedure TServiceController.UpdateStatus;
var
  bytesNeeded: Cardinal;
begin
  Assert(fServiceHandle.Value <> fCInvalidServiceHandle, 'Invalid fServiceHandle');
  Win32Check(QueryServiceStatusEx(
    fServiceHandle,
    SC_STATUS_PROCESS_INFO,
    PByte(@fServiceStatusProcess),
    SizeOf(fServiceStatusProcess),
    bytesNeeded
  ));
  fStatus := ParseStatus(fServiceStatusProcess.dwCurrentState);
  fCanStop := fServiceStatusProcess.dwControlsAccepted and SERVICE_ACCEPT_SHUTDOWN <> 0;
  fCanPauseAndResume := fServiceStatusProcess.dwControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE <> 0;
  fCanShutdown := fServiceStatusProcess.dwControlsAccepted and SERVICE_ACCEPT_SHUTDOWN <> 0;
end;

procedure TServiceController.UpdateConfig;
var
  bufferSize,
  bytesNeeded: Cardinal;
  serviceConfig: WinSvc.PQueryServiceConfig;
  list: TStringDynArray;
  serviceName: string;
  i: Integer;
begin
  Assert(fServiceHandle.Value <> fCInvalidServiceHandle, 'Invalid fServiceHandle');
  if not QueryServiceConfig(fServiceHandle, nil, 0, bytesNeeded) and
    (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
  begin
    RaiseLastOSError;
  end;
  bufferSize := bytesNeeded;
  serviceConfig := AllocMem(bytesNeeded);
  fServiceConfig := TValueHolder<WinSvc.PQueryServiceConfig>.Create(
    serviceConfig,
    procedure(var serviceConfig: WinSvc.PQueryServiceConfig)
    begin
      Dispose(serviceConfig);
    end
  );
  Win32Check(QueryServiceConfig(fServiceHandle, fServiceConfig, bufferSize, bytesNeeded));
  with fServiceConfig.Value^ do
  begin
    fServiceType := ParseServiceTypes(dwServiceType);
    fStartMode := ParseStartMode(dwStartType);
    fDisplayName := lpDisplayName;
    fFileName := lpBinaryPathName;
    list := SplitNullTerminatedStrings(lpDependencies);
  end;
  // TODO: Handle SC_GROUP_IDENTIFIER
  SetLength(fServicesDependedOn, Length(list));
  for i := 0 to High(list) do
  begin
    serviceName := list[i];
    fServicesDependedOn[i] := TServiceController.Create(serviceName);
  end;
end;

procedure TServiceController.UpdateDependentServices;
begin
  // EnumDependentServices
end;

procedure TServiceController.UpdateDescription;
var
  bufferSize,
  bytesNeeded: Cardinal;
  buffer: TBytes;
begin
  Assert(fServiceHandle.Value <> fCInvalidServiceHandle, 'Invalid fServiceHandle');
  if not QueryServiceConfig2(fServiceHandle, SERVICE_CONFIG_DESCRIPTION, nil, 0, bytesNeeded) and
    (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
  begin
    RaiseLastOSError;
  end;
  bufferSize := bytesNeeded;
  SetLength(buffer, bufferSize);
  Win32Check(QueryServiceConfig2(fServiceHandle, SERVICE_CONFIG_DESCRIPTION, PByte(buffer), bufferSize, bytesNeeded));
  fDescription := PServiceDescription(buffer).lpDescription;
end;

procedure TServiceController.Loaded;
begin
  if not fIsLoaded then
  begin
    Refresh;
    fIsLoaded := True;
  end;
end;

procedure TServiceController.Open(desiredAccess: Cardinal);
begin
  Assert(desiredAccess and not SERVICE_ALL_ACCESS = 0, 'Invalid desiredAccess.');
  ManagerHandleNeeded;
  fServiceHandle := OpenService(fManagerHandle, PChar(fServiceName), desiredAccess);
  if fServiceHandle.Value = fCInvalidServiceHandle then
  case GetLastError of
    ERROR_SERVICE_DOES_NOT_EXIST:
    begin
      raise EOSError.CreateFmt(SServiceNotExists, [fServiceName]);
    end;
    else
    begin
      RaiseLastOSError;
    end;
  end;
end;

procedure TServiceController.Close;
begin
  if fServiceHandle.Value <> fCInvalidServiceHandle then
  begin
    CloseServiceHandle(fServiceHandle);
    fServiceHandle := fCInvalidServiceHandle;
  end;
end;

procedure TServiceController.Refresh;
begin
  Open(SERVICE_QUERY_STATUS or SERVICE_QUERY_CONFIG or SERVICE_CONFIG_DESCRIPTION);
  try
    UpdateStatus;
    UpdateConfig;
    UpdateDescription;
    UpdateDependentServices;
  finally
    Close;
  end;
end;

procedure TServiceController.ExecuteCommand(command: Integer);
begin

end;

procedure TServiceController.PerformAction(action: TServiceAction);
var
  serviceStatus: WinSvc.TServiceStatus;
const
  AccessArray: array[TServiceAction] of DWORD = (
    SERVICE_START,              // saStart
    SERVICE_STOP,               // saStop
    SERVICE_PAUSE_CONTINUE,     // saPause
    SERVICE_PAUSE_CONTINUE      // saResume
  );
  ControlArray: array[TServiceAction] of DWORD = (
    0,                          // saStart
    SERVICE_CONTROL_STOP,       // saStop
    SERVICE_CONTROL_PAUSE,      // saPause
    SERVICE_CONTROL_CONTINUE    // saResume
  );
begin
  Assert(action in [saStop, saPause, saResume]);
  Open(AccessArray[action] or SERVICE_QUERY_STATUS);
  try
    UpdateStatus;
    serviceStatus := WinSvc.PServiceStatus(@fServiceStatusProcess)^;
    Win32Check(ControlService(fServiceHandle, ControlArray[action], serviceStatus));
  finally
    Close;
  end;
end;

procedure TServiceController.Start;
begin
  Start([]);
end;

procedure TServiceController.Start(const args: array of string);
var
  pArgs: PChar;
  i: Integer;
  argCount: Integer;
type
  PStrArray = ^TStrArray;
  TStrArray = array[0..32767] of PChar;
begin
  Open(SERVICE_START or SERVICE_QUERY_STATUS);
  try
    if Length(args) = 0 then
    begin
      pArgs := nil;
      argCount := 0;
    end
    else
    begin
      argCount := Length(args) + 1;
      pArgs := AllocMem(argCount * SizeOf(PChar));
      PStrArray(pArgs)^[0] := PChar(fServiceName);
      for i := 0 to High(args) do
      begin
        PStrArray(pArgs)^[i+1] := PChar(args[i]);
      end;
    end;
    Win32Check(StartService(fServiceHandle, argCount, pArgs));
  finally
    FreeMem(pArgs);
    Close;
  end;
end;

procedure TServiceController.Stop;
begin
  PerformAction(saStop);
end;

procedure TServiceController.Pause;
begin
  PerformAction(saPause);
end;

procedure TServiceController.Resume;
begin
  PerformAction(saResume);
end;

procedure TServiceController.Restart;
begin
  Stop;
  WaitForStatus(ssStopped);
  Start;
end;

procedure TServiceController.WaitForStatus(status: TServiceStatus);
begin
  WaitForStatus(status, TTimeSpan.Create(Windows.INFINITE));
end;

procedure TServiceController.WaitForStatus(desiredStatus: TServiceStatus;
  const timeout: TTimeSpan);
var
  oldCheckPoint: DWORD;
  startTickCount: DWORD;
  waitTime: DWORD;
  ticks: DWORD;
  n: Integer;
begin
  TArgument.CheckEnum<TServiceStatus>(desiredStatus, 'desiredStatus');
  Open(SERVICE_QUERY_STATUS);
  try
    UpdateStatus;
    startTickCount := GetTickCount;
    oldCheckPoint := fServiceStatusProcess.dwCheckPoint;
    n := 0;
    while fStatus <> desiredStatus do
    begin
      // FROM <MSDN Library>:
      // Do not wait longer than the wait hint. A good interval is
      // one-tenth the wait hint, but no less than 1 second and no
      // more than 10 seconds.
      waitTime := fServiceStatusProcess.dwWaitHint div 10;
      if waitTime < 1000 then
        waitTime := 1000
      else if waitTime > 10000 then
        waitTime := 10000;
      Sleep(waitTime);
      Inc(n);
      UpdateStatus;
      ticks := GetTickCount - startTickCount;
      if fStatus = desiredStatus then Break;
      if fServiceStatusProcess.dwCheckPoint > oldCheckPoint then
      begin
        // The service is making progress.
        startTickCount := GetTickCount;
        oldCheckPoint := fServiceStatusProcess.dwCheckPoint;
      end
      else if ticks > fServiceStatusProcess.dwWaitHint then
      begin
        // No progress made within the wait hint.
        Break;
      end
      else if ticks > timeout.Ticks  then
      begin
        raise ETimeoutException.Create(STimeoutException);
      end;
    end;
    if fStatus <> desiredStatus then
    begin
      RaiseLastOSError;
    end;
  finally
    Close;
  end;
end;

function TServiceController.ParseServiceTypes(const value: Cardinal): TServiceTypes;
begin
  Result := [];
  if value and SERVICE_ADAPTER <> 0 then
    Include(Result, stAdapter);
  if value and SERVICE_FILE_SYSTEM_DRIVER <> 0 then
    Include(Result, stFileSystemDriver);
  if value and SERVICE_INTERACTIVE_PROCESS <> 0 then
    Include(Result, stInteractiveProcess);
  if value and SERVICE_KERNEL_DRIVER <> 0 then
    Include(Result, stKernelDriver);
  if value and SERVICE_RECOGNIZER_DRIVER <> 0 then
    Include(Result, stRecognizerDriver);
  if value and SERVICE_WIN32_OWN_PROCESS <> 0 then
    Include(Result, stWin32OwnProcess);
  if value and SERVICE_WIN32_SHARE_PROCESS <> 0 then
    Include(Result, stWin32ShareProcess);
end;

function TServiceController.ParseStartMode(const value: Cardinal): TServiceStartMode;
begin
  case value of
    SERVICE_BOOT_START:   Result := smBoot;
    SERVICE_SYSTEM_START: Result := smSystem;
    SERVICE_AUTO_START:   Result := smAutomatic;
    SERVICE_DEMAND_START: Result := smManual;
    SERVICE_DISABLED:     Result := smDisabled;
    else
    begin
      raise EArgumentException.CreateFmt(SArgumentOutOfRangeException, [IntToStr(value)]);
    end;
  end;
end;

function TServiceController.ParseStatus(const value: Cardinal): TServiceStatus;
begin
  case value of
    SERVICE_STOPPED:           Result := ssStopped;
    SERVICE_START_PENDING:     Result := ssStartPending;
    SERVICE_STOP_PENDING:      Result := ssStopPending;
    SERVICE_RUNNING:           Result := ssRunning;
    SERVICE_CONTINUE_PENDING:  Result := ssContinuePending;
    SERVICE_PAUSE_PENDING:     Result := ssPausePending;
    SERVICE_PAUSED:            Result := ssPaused;
    else
    begin
      raise EArgumentException.CreateFmt(SArgumentOutOfRangeException, [IntToStr(value)]);
    end;
  end;
end;

class function TServiceController.GetDevices: TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices('', skDrive, Result);
end;

class function TServiceController.GetDevices(const machineName: string): TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices(machineName, skDrive, Result);
end;

class function TServiceController.GetServices: TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices('', skWin32, Result);
end;

class function TServiceController.GetServices(
  const machineName: string): TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices(machineName, skWin32, Result);
end;

function TServiceController.GetDisplayName: string;
begin
  Loaded;
  Result := fDisplayName;
end;

function TServiceController.GetMachineName: string;
begin
  Loaded;
  Result := fMachineName;
end;

function TServiceController.GetServiceHandle: THandle;
begin
  Loaded;
  Result := fServiceHandle;
end;

function TServiceController.GetServiceName: string;
begin
  Loaded;
  Result := fServiceName;
end;

function TServiceController.GetServiceType: TServiceTypes;
begin
  Loaded;
  Result := fServiceType;
end;

function TServiceController.GetStartMode: TServiceStartMode;
begin
  Loaded;
  Result := fStartMode;
end;

function TServiceController.GetDescription: string;
begin
  Loaded;
  Result := fDescription;
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  Loaded;
  Result := fStatus;
end;

function TServiceController.GetCanStop: Boolean;
begin
  Loaded;
  Result := fCanStop;
end;

function TServiceController.GetCanPauseAndResume: Boolean;
begin
  Loaded;
  Result := fCanPauseAndResume;
end;

function TServiceController.GetCanShutdown: Boolean;
begin
  Loaded;
  Result := fCanShutdown;
end;

function TServiceController.GetExists: Boolean;
var
  serviceHandle: THandle;
begin
  ManagerHandleNeeded;
  serviceHandle := OpenService(fManagerHandle, PChar(fServiceName), SERVICE_QUERY_STATUS);
  Result := serviceHandle <> fCInvalidServiceHandle;
  if Result then
  begin
    CloseServiceHandle(serviceHandle);
  end;
end;

function TServiceController.GetServicesDependedOn: TArray<TServiceController>;
begin
  Loaded;
  Result := fServicesDependedOn;
end;

function TServiceController.GetDependentServices: TArray<TServiceController>;
begin
  Loaded;
  Result := fDependentServices;
end;

{$ENDREGION}


{$REGION 'TLifetimeWatcher'}

constructor TLifetimeWatcher.Create(const proc: TProc);
begin
  inherited Create;
  fProc := proc;
end;

destructor TLifetimeWatcher.Destroy;
begin
  if Assigned(fProc) then
    fProc;
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TCallback'}

type
  PInstruction = ^TInstruction;
  TInstruction = array[1..16] of Byte;

{----------------------------}
{        Code DASM           }
{----------------------------}
{  push  [ESP]               }
{  mov   [ESP+4], ObjectAddr }
{  jmp   MethodAddr          }
{----------------------------}

/// <author>
/// savetime
/// </author>
/// <seealso>http://savetime.delphibbs.com</seealso>
constructor TCallback.Create(objectAddress: TObject; methodAddress: Pointer);
const
  Instruction: TInstruction = (
    $FF,$34,$24,$C7,$44,$24,$04,$00,$00,$00,$00,$E9,$00,$00,$00,$00
  );
var
  p: PInstruction;
begin
  inherited Create;
  New(p);
  Move(Instruction, p^, SizeOf(Instruction));
  PInteger(@p[8])^ := Integer(objectAddress);
  PInteger(@p[13])^ := Longint(methodAddress) - (Longint(p) + SizeOf(Instruction));
  fInstance := p;
end;

destructor TCallback.Destroy;
begin
  Dispose(fInstance);
  inherited Destroy;
end;

function TCallback.Invoke: Pointer;
begin
  Result := fInstance;
end;

{$ENDREGION}


{$REGION 'TDelegate<T>'}

destructor TDelegate<T>.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TDelegate<T>.Add(const delegate: T);
begin
  if fList = nil then
  begin
    fList := TList<T>.Create;
  end;
  fList.Add(delegate);
end;

procedure TDelegate<T>.Remove(const delegate: T);
begin
  if fList <> nil then
  begin
    fList.Remove(delegate);
  end;
end;

procedure TDelegate<T>.Clear;
begin
  if fList <> nil then
  begin
    fList.Clear;
  end;
end;

procedure TDelegate<T>.Invoke(proc: TProc<T>);
var
  delegate: T;
begin
  if fList <> nil then
  begin
    for delegate in fList do
    begin
      proc(delegate);
    end;
  end;
end;

{$ENDREGION}


initialization
  ApplicationPath := ExtractFilePath(ParamStr(0));
  ApplicationVersionInfo := TFileVersionInfo.GetVersionInfo(ParamStr(0));
  ApplicationVersion := ApplicationVersionInfo.FileVersionNumber;
  ApplicationVersionString := ApplicationVersionInfo.FileVersion;

finalization

end.

