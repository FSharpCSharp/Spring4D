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

{TODO -oPaul -cGeneral : Add TArray, TArray<T>}
{TODO -oPaul -cGeneral : Add TArrayBuilder}

///	<summary>Declares the fundamental types for the framework.</summary>
unit Spring;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  DateUtils,
  Types,
  TypInfo,
  Variants,
  TimeSpan,
  Diagnostics,
  Rtti,
  Generics.Defaults,
  Generics.Collections;

type
  ///	<summary>Represents a dynamic array of Byte.</summary>
  TBytes = SysUtils.TBytes;

  ///	<summary>Represents a dynamic array of string.</summary>
  TStringDynArray = Types.TStringDynArray;

  ///	<summary>Represents a time interval.</summary>
  /// <seealso cref="TimeSpan|TTimeSpan" />
  TTimeSpan = TimeSpan.TTimeSpan;

  /// <summary>
  /// Provides a set of methods and properties to accurately measure elapsed time.
  /// </summary>
  /// <seealso cref="Diagnostics|TStopwatch" />
  TStopwatch = Diagnostics.TStopwatch;

  PTypeInfo  = TypInfo.PTypeInfo;
  TTypeKind  = TypInfo.TTypeKind;
  TTypeKinds = TypInfo.TTypeKinds;

  {$REGION 'Documentation'}
  ///	<summary>Represents a logical predicate.</summary>
  ///	<param name="value">the value needs to be determined.</param>
  ///	<returns>Returns True if the value was accepted, otherwise, returns
  ///	false.</returns>
  ///	<remarks>
  ///	  <note type="warning">This type redefined the
  ///	  <see cref="SysUtils|TPredicate&lt;T&gt;" /> type with a const parameter.</note>
  ///	</remarks>
  /// <seealso cref="Spring.DesignPatterns|ISpecification{T}" />
  {$ENDREGION}
  TPredicate<T> = reference to function(const value: T): Boolean;

  ///	<summary>Represents the class type of <c>TCustomAttribute</c>.</summary>
  TAttributeClass = class of TCustomAttribute;

  /// <summary>
  /// Provides a non-reference-counted IInterface implementation.
  /// </summary>
  TInterfaceBase = class abstract(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  ///	<summary>Provides an abstract class base of TThread that implements the
  ///	IInterface.</summary>
  TInterfacedThread = class abstract(TThread, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;


  {$REGION 'Documentation'}
  ///	<summary>Represents a series of bytes in memory.</summary>
  ///	<remarks>
  ///	  The <c>TBuffer</c> structure is actually a wrapper of a value of
  ///	  <c>TBytes</c> while provides some easy-going methods and properties.
  ///	  <note type="warning">This type needs to be reviewed.</note>
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

//    procedure LoadFromStream(stream: TStream; count: Integer);
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


  ///	<summary>Represents a version number in the format of
  ///	"major.minor[.build[.revision]]", which is different from the delphi
  ///	style format "major.minor[.release[.build]]".</summary>
  TVersion = record
  private
    const fCUndefined: Integer = -1;
  strict private
    fMajor: Integer;
    fMinor: Integer;
    fBuild: Integer;      // -1 if undefined.
    fReversion: Integer;  // -1 if undefined.
    function GetMajorReversion: Int16;
    function GetMinorReversion: Int16;
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
    { Operator Overloads }
    class operator Equal(const left, right: TVersion): Boolean;
    class operator NotEqual(const left, right: TVersion): Boolean;
    class operator GreaterThan(const left, right: TVersion): Boolean;
    class operator GreaterThanOrEqual(const left, right: TVersion): Boolean;
    class operator LessThan(const left, right: TVersion): Boolean;
    class operator LessThanOrEqual(const left, right: TVersion): Boolean;
  end;


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
  private
    class procedure DoCheckArrayIndex(const length, index: Integer); static; inline;
    class procedure DoCheckArrayRange(const length, startIndex, count: Integer); static; inline;
    class procedure DoCheckStringIndex(const length, index: Integer); static; inline;
    class procedure DoCheckStringRange(const length, startIndex, count: Integer); static; inline;
  public
    class procedure CheckTrue(condition: Boolean; const msg: string); static; inline;
    class procedure CheckFalse(condition: Boolean; const msg: string); static; inline;

    class procedure CheckInheritsFrom(obj: TObject; const clazz: TClass; const argumentName: string); overload; static; inline;
    class procedure CheckInheritsFrom(const checkclazz, clazz: TClass; const argumentName: string); overload; static; inline;

    class procedure CheckNotNull(obj: TObject; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(p: Pointer; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(const intf: IInterface; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(condition: Boolean; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull<T>(const value: T; const argumentName: string); overload; static; inline;

    class procedure CheckEnum<T{:enum}>(const value: T; const argumentName: string); overload; static; inline;
    class procedure CheckEnum<T{:enum}>(const value: Integer; const argumentName: string); overload; static; inline;

    ///	<exception cref="EArgumentOutOfRangeException">Raised if the
    ///	<paramref name="index" /> is out of range.</exception>
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
    class procedure CheckRange(const length, startIndex, count: Integer; const indexBase: Integer = 0); overload; static; inline;

    class procedure CheckTypeKind(typeInfo: PTypeInfo; const expectedTypeKind: TTypeKind; const argumentName: string); overload; static;
    class procedure CheckTypeKind(typeInfo: PTypeInfo; const expectedTypeKinds: TTypeKinds; const argumentName: string); overload; static;

    class function IsNullReference<T>(const value: T): Boolean; overload; static;
    class function IsNullReference(const value; typeInfo: PTypeInfo): Boolean; overload; static;

    ///	<summary>Raise an EArgumentException exception.</summary>
    class procedure RaiseArgumentException(const msg: string); overload; static; inline;

    ///	<summary>Raise an EArgumentException exception.</summary>
    class procedure RaiseArgumentFormatException(const argumentName: string); overload; static; inline;

    ///	<summary>Raise an EArgumentNullException exception.</summary>
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static; inline;

    ///	<summary>Raise an EArgumentOutOfRangeException exception.</summary>
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static; inline;

    ///	<summary>Raise an EInvalidEnumArgumentException exception.</summary>
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static; inline;
  end;

  /// <summary>
  /// Represents a type alias of the TArgument class.
  /// </summary>
  TArg = TArgument;


  {$REGION 'TNullable<T>'}

  ///	<summary>Represents an "object" whose underlying type is a value type
  ///	that can also be assigned nil like a reference type.</summary>
  ///	<typeparam name="T">The underlying value type of the <see cref="TNullable{T}" />
  ///	generic type.</typeparam>
  /// <remarks>
  /// The <typeparamref name="T" /> must be a value type such as a value of string, Integer.
  /// </remarks>
  TNullable<T> = packed record
  private
    const fCHasValue = '@';  // DO NOT LOCALIZE
  strict private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;
  private
    ///	<summary>Internal use. Clears the value and marks it as null.</summary>
    procedure Clear;
  public
    ///	<summary>Initializes a new instance of the <c>TNullable{T}</c> structure
    ///	to the specified value.</summary>
    constructor Create(const value: T); overload;

    ///	<summary>Initializes a new instance of the <c>TNullable{T}</c> structure
    ///	to the specified value.</summary>
    constructor Create(const value: Variant); overload;

    ///	<summary>Retrieves the value of the current <c>TNullable{T}</c> object,
    ///	or the object's default value.</summary>
    function GetValueOrDefault: T; overload;

    ///	<summary>Retrieves the value of the current <c>TNullable{T}</c> object,
    ///	or the specified default value.</summary>
    ///	<param name="default">the default value</param>
    function GetValueOrDefault(const default: T): T; overload;

    ///	<summary>Gets a value indicating whether the current <c>TNullable{T}</c>
    ///	structure has a value.</summary>
    property HasValue: Boolean read GetHasValue;

    ///	<summary>Gets the value of the current <c>TNullable{T}</c> value.</summary>
    ///	<exception cref="Spring|EInvalidOperation">Raised if the value is
    ///	null.</exception>
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


  {$REGION 'Common TNullable<T> Aliases'}

  ///	<summary>Represents a nullable string.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableString = TNullable<string>;

  ///	<summary>Represents a nullable integer.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableInteger = TNullable<Integer>;

  ///	<summary>Represents a nullable <c>TDateTime</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableDateTime = TNullable<TDateTime>;

  ///	<summary>Represents a nullable <c>Currency</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableCurrency = TNullable<Currency>;

  ///	<summary>Represents a nullable <c>Double</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableDouble = TNullable<Double>;

  ///	<summary>Represents a nullable <c>Boolean</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableBoolean = TNullable<Boolean>;

  ///	<summary>Represents a nullable <c>Int64</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableInt64 = TNullable<Int64>;

  ///	<summary>Represents a nullable <c>TGuid</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableGuid = TNullable<TGUID>;

  {$ENDREGION}


  {$REGION 'TEnum'}

  /// <summary>
  /// Provides static methods to manipulate an enumeration type.
  /// </summary>
  TEnum = record
  private
    class function GetEnumTypeInfo<T>: PTypeInfo; static;
    class function GetEnumTypeData<T>: PTypeData; static;
    { Internal function without range check }
    class function ConvertToInteger<T>(const value: T): Integer; static;
  public
    // Todo: Add Non-Generic methods
//  class procedure ExtractNames<T>(names: TStrings); static;
//  class procedure ExtractValues<T>(values: TStrings); static;
    class function IsValid<T>(const value: T): Boolean; overload; static;
    class function IsValid<T>(const value: Integer): Boolean; overload; static;
    class function GetName<T>(const value: T): string; overload; static;
    class function GetName<T>(const value: Integer): string; overload; static;
    class function GetNames<T>: TStringDynArray; static;
    class function GetValue<T>(const value: T): Integer; overload; static;
    class function GetValue<T>(const value: string): Integer; overload; static;
    class function GetValues<T>: TIntegerDynArray; static;
    class function GetValueStrings<T>: TStringDynArray; static;
    class function TryParse<T>(const value: Integer; out enum: T): Boolean; overload; static;
    class function TryParse<T>(const value: string; out enum: T): Boolean; overload; static;
    class function Parse<T>(const value: Integer): T; overload; static;
    class function Parse<T>(const value: string): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Defines the interface for an abstract value provider. '}
  ///	<summary>Defines the interface for an abstract value provider.</summary>
  ///	<remarks>Use the <see cref="Value" /> property to retrieve the current value from
  ///	the provider. Use the <see cref="SetValue(TValue)">SetValue</see> method to assign a new value to it
  ///	if the value provider is not read only. Otherwise, an
  ///	<c>EInvalidException</c> exception will be raised.</remarks>
  {$ENDREGION}
  IValueProvider = interface
    ['{392A1E2F-CCA1-4CBB-9306-29AA402927D6}']

    {$REGION ' Property Getters and Setters '}
      function GetValue: TValue;
      function GetIsReadOnly: Boolean;
    {$ENDREGION}

    ///	<summary>Sets the value of the provider.</summary>
    ///	<param name="value">the new value.</param>
    ///	<exception cref="EInvalidOperation">Raised if the value provider is
    ///	read only.</exception>
    procedure SetValue(const value: TValue);

    ///	<summary>Gets the value of the provider.</summary>
    property Value: TValue read GetValue;

    {$REGION 'Documentation'}
    ///	<summary>Gets a value that indicates whether the value provider is read
    ///	only.</summary>
    ///	<value>Returns true if the value provider is read only, otherwise,
    ///	returns false.</value>
    ///	<remarks>If the value provider is read only, invoking the
    ///	<see cref="SetValue(TValue)">SetValue</see> method will raise an <c>EInvalidOperation</c>
    ///	exception.</remarks>
    {$ENDREGION}
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;


  {$REGION 'Documentation'}
  ///	<summary>Provides an abstract base class for generic value provider.</summary>
  ///	<remarks>
  ///	  <note type="implement">
  ///	    <para>By default, the <see cref="IsReadOnly" /> property is true.</para>
  ///	    <para>Implementers must override the DoSetValue method if the value
  ///	    provider is not read only.</para>
  ///	  </note>
  ///	</remarks>
  {$ENDREGION}
  TValueProviderBase = class abstract(TInterfacedObject, IValueProvider)
  protected
    function GetValue: TValue; virtual; abstract;
    function GetIsReadOnly: Boolean; virtual;
    procedure DoSetValue(const value: TValue); virtual; abstract;
  public
    procedure SetValue(const value: TValue); virtual;
    property Value: TValue read GetValue;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TValueProvider = TValueProviderBase
    deprecated 'Use the TValueProviderBase class instead.';

  ///	<summary>Represents a multicast delegate interface.</summary>
  ///	<remarks>
  ///	  <note type="warning">This type needs to be reviewed.</note>
  ///	</remarks>
  IDelegate<T> = interface
    function AddHandler(const handler: T): IDelegate<T>;
    function RemoveHandler(const handler: T): IDelegate<T>;
    function Invoke(const callback: TProc<T>): IDelegate<T>;
    procedure Clear;
  end;

  TDelegate<T> = class(TInterfacedObject, IDelegate<T>)
  private
    fHandlers: TList<T>;
  protected
    function GetHandlers: TList<T>; virtual;
    property Handlers: TList<T> read GetHandlers;
  public
    destructor Destroy; override;
    function AddHandler(const handler: T): IDelegate<T>;
    function RemoveHandler(const handler: T): IDelegate<T>;
    function Invoke(const callback: TProc<T>): IDelegate<T>; virtual;
    procedure Clear;
  end;

  /// <summary>
  /// Enforces an ordering constraint on memory operations.
  /// </summary>
  TVolatile<T> = record
  private
    fValue: T;
    function GetValue: T;
    procedure SetValue(const newValue: T);
  public
    property Value: T read GetValue write SetValue;
    { Operator Overloads }
    class operator Implicit(const value: T): TVolatile<T>;
    class operator Implicit(const value: TVolatile<T>): T;
    class operator Equal(const left, right: TVolatile<T>): Boolean;
    class operator NotEqual(const left, right: TVolatile<T>): Boolean;
  end deprecated;// 'Uses the TLazyUtils.GetValue method structure to ensure the thread safety.';

  /// <preliminary />
  /// <threadsafety static="true" />
  TLazyUtils = record
  public
    {$REGION 'Documentation'}
    ///	<summary>Uses the <c>TLazyUtils.GetValue&lt;T&gt;</c> method to
    ///	implement the <b>Lazy-Initialization</b> pattern in
    ///	thread-safe.</summary>
    ///	<param name="field">the field stores the instance.</param>
    ///	<param name="delegate">the delegate that will create a new
    ///	instance.</param>
    ///	<remarks>
    ///	  <para>The following code copied from the <b>SysUtils.TEncoding</b>
    ///	  class illustrates how to use the
    ///	  <c>InterlockedCompareExchangePointer</c> function to ensure the
    ///	  thread-safety.</para>
    ///	  <code lang="Delphi">
    ///	class function TEncoding.GetUTF8: TEncoding;
    ///	var
    ///	  LEncoding: TEncoding;
    ///	begin
    ///	  if FUTF8Encoding = nil then
    ///	  begin
    ///	    LEncoding := TUTF8Encoding.Create;
    ///	    if InterlockedCompareExchangePointer(Pointer(FUTF8Encoding), LEncoding, nil) &lt;&gt; nil then
    ///	      LEncoding.Free;
    ///	  end;
    ///	  Result := FUTF8Encoding;
    ///	end;
    ///	</code>
    ///	  <para>By using the <c>TLazyUtils.GetValue&lt;T&gt;</c> method, the
    ///	  code could be simpilifed:</para>
    ///	  <code lang="Delphi">
    ///	class function TEncoding.GetUTF8: TEncoding;
    ///	begin
    ///	  Result := TLazyUtils.GetValue&lt;TEncoding&gt;(fUtf8Encoding,
    ///	    function: TEncoding
    ///	    begin
    ///	      Result := TUtf8Encoding.Create;
    ///	    end
    ///	  );
    ///	end;
    ///	</code>
    ///	</remarks>
    /// <seealso cref="Windows|InterlockedCompareExchangePointer" />
    {$ENDREGION}
    class function GetValue<T: class>(var field: T; const delegate: TFunc<T>): T; static;
  end;


  {$REGION 'Documentation'}
  ///	<summary>Represents a lifetime watcher. The basic idea is using an
  ///	instance of the <c>IInterface</c> in the host such as a record. The
  ///	anonymous method, which is specified by the <paramref name="proc" />
  ///	parameter in the constructor, will be executed when this interface is
  ///	disposed. Normally, the proc is some kind of clean up code.</summary>
  {$ENDREGION}
  TLifetimeWatcher = class(TInterfacedObject)
  private
    fProc: TProc;
  public
    constructor Create(const proc: TProc);
    destructor Destroy; override;
  end;


  {$REGION 'Documentation'}
  ///	<summary>
  ///	  <para>Provides a simple &amp; flexible implementation of <b>Smart
  ///	  Pointer</b>. This implementation is very skillful and the basic idea
  ///	  comes from a post in Kelly Barry's blog.</para>
  ///	  <para>The point is to use an anonymous method <c>TFunc&lt;T&gt;,</c>
  ///	  which is internally implemented as an interface in Delphi for Win32, to
  ///	  manage the lifetime of an object instance.</para>
  ///	</summary>
  ///	<example>
  ///	  <para>The following example demonstrates how to use the Smart
  ///	  Pointer:</para>
  ///	  <code lang="Delphi">
  ///	procedure TestSmartPointer;
  ///	var
  ///	  person: TFunc&lt;TPerson&gt;;
  ///	begin
  ///	  person := TObjectHolder&lt;TPerson&gt;.Create(TPerson.Create);
  ///	  person.DoSomething;
  ///	end;
  ///	</code>
  ///	</example>
  ///<preliminary />
  {$ENDREGION}
  TObjectHolder<T: class> = class(TInterfacedObject, TFunc<T>)
  private
    fObject: T;
    fLifetimeWatcher: IInterface;
  public
    constructor Create(obj: T); overload;
    constructor Create(obj: T; const lifetimeWatcher: IInterface); overload;
    destructor Destroy; override;
    function Invoke: T;
  end;

  TObjectHolder = TObjectHolder<TObject>;


  {$REGION 'Documentation'}
  ///	<summary>Represents a weak reference, which references a value of managed
  ///	type such as an interface.</summary>
  ///	<example>
  ///	  The following sample demonstrates how to use the
  ///	  <c>TWeakReference&lt;T&gt;</c> type:
  ///	  <code lang="Delphi">
  ///	IHost = interface
  ///	  procedure DoSomething;
  ///	end;
  ///	TSomeClass = class
  ///	private
  ///	  fHost: TWeakReference&lt;IHost&gt;;
  ///	  constructor Create(const host: IHost);
  ///	end;
  ///	constructor TSomeClass.Create(const host: IHost);
  ///	begin
  ///	  fHost := host;
  ///	end;
  ///	// fHost.DoSomething;
  ///	</code>
  ///	</example>
  ///<preliminary />
  {$ENDREGION}
  TWeakReference<T: IInterface> = record
  private
    fValue: Pointer;
    function GetValue: T;
  public
    constructor Create(const value: T);
    ///	<summary>Gets the strong-typed value of the reference.</summary>
    property Value: T read GetValue;
    class operator Implicit(const reference: TWeakReference<T>): T;
    class operator Implicit(const value: T): TWeakReference<T>;
  end;

  ///	<summary>Internal interface. Reserved for future use.</summary>
  ISupportItemType = interface
    ['{FE986DD7-41D5-4312-A2F9-94F7D9E642EE}']
    function GetItemType: PTypeInfo;
  end;

  IItemTypeSupport = ISupportItemType;

  /// <summary>
  /// Provides a workaround to get or set the value of an indexed property.
  /// </summary>
  /// <preliminary />
  ISupportIndexedProperties = interface
    ['{7BF8ED3B-60AB-425E-B678-776FD2EA3437}']
    function GetPropertyValue(const propertyName: string; const index: TValue): TValue;
    procedure SetPropertyValue(const propertyName: string; const index, value: TValue);
  end;


  {$REGION 'Lifecycle Interfaces'}

  {$REGION 'Documentation'}
  ///	<summary>Lifecycle interface. If a component implements this interface,
  ///	the IoC container will invoke the <c>Initialize</c> method when
  ///	initiating an instance of the component.</summary>
  ///	<seealso cref="IStartable"></seealso>
  ///	<seealso cref="IRecyclable"></seealso>
  ///	<seealso cref="IDisposable"></seealso>
  {$ENDREGION}
  IInitializable = interface
    ['{A36BB399-E592-4DFB-A091-EDBA3BE0648B}']
    ///	<summary>Initializes the component.</summary>
    procedure Initialize;
  end;

  ///	<summary>Lifecycle interface. Represents that the component can be
  ///	started and stopped.</summary>
  ///	<seealso cref="IInitializable"></seealso>
  ///	<seealso cref="IRecyclable"></seealso>
  ///	<seealso cref="IDisposable"></seealso>
  IStartable = interface
    ['{8D0252A1-7993-44AA-B0D9-326019B58E78}']
    procedure Start;
    procedure Stop;
  end;

  ///	<summary>Lifecycle interface. Only called for components that belongs to
  ///	a pool when the component comes back to the pool.</summary>
  ///	<seealso cref="IInitializable"></seealso>
  ///	<seealso cref="IStartable"></seealso>
  ///	<seealso cref="IDisposable"></seealso>
  IRecyclable = interface
    ['{85114F41-70E5-4AF4-A375-E445D4619E4D}']
    procedure Recycle;
  end;

  ///	<summary>Lifecycle interface.&#160;If the component implements this
  ///	interface, all resources will be deallocate by calling the <c>dispose</c>
  ///	method.</summary>
  ///	<seealso cref="IInitializable"></seealso>
  ///	<seealso cref="IStartable"></seealso>
  ///	<seealso cref="IRecyclable"></seealso>
  IDisposable = interface
    ['{6708F9BF-0237-462F-AFA2-DF8EF21939EB}']
    procedure Dispose;
  end;

  {$ENDREGION}


  {$REGION 'TLifetimeType & Related Attributes'}

  ///	<summary>Lifetime Type Enumeration.</summary>
  ///	<seealso cref="SingletonAttribute"></seealso>
  ///	<seealso cref="TransientAttribute"></seealso>
  ///	<seealso cref="SingletonPerThreadAttribute"></seealso>
  ///	<seealso cref="PooledAttribute"></seealso>
  TLifetimeType = (
    /// <summary>
    /// Unknown lifetime type.
    /// </summary>
    ltUnknown,
    /// <summary>
    /// Single instance.
    /// </summary>
    ltSingleton,
    /// <summary>
    /// Different instances.
    /// </summary>
    ltTransient,
    /// <summary>
    /// Every thread has a single instance.
    /// </summary>
    ltSingletonPerThread,
    /// <summary>
    /// Instances are transient except that they are recyclable.
    /// </summary>
    ltPooled,
    /// <summary>
    /// Customized lifetime type.
    /// </summary>
    /// <remarks>
    /// Not Implemented yet.
    /// </remarks>
    ltCustom
  );

  ///	<summary>Represents an abstract lifetime attribute class base.</summary>
  LifetimeAttributeBase = class abstract(TCustomAttribute)
  private
    fLifetimeType: TLifetimeType;
  public
    constructor Create(lifetimeType: TLifetimeType);
    property LifetimeType: TLifetimeType read fLifetimeType;
  end;

  ///	<summary>Represents an abstract lifetime attribute class base.</summary>
  TLifetimeAttributeBase = LifetimeAttributeBase
    deprecated 'Use the LifetimeAttributeBase class instead.';

  {$REGION 'Documentation'}
  ///	<summary>Applies this attribute when a component shares the single
  ///	instance.</summary>
  ///	<remarks>When this attribute is applied to a component, the shared
  ///	instance will be returned whenever get the implementation of a
  ///	service.</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	[Singleton]
  ///	TEmailSender = class(TInterfacedObject, IEmailSender)
  ///	//...
  ///	end;
  ///	</code>
  ///	</example>
  ///	<seealso cref="TransientAttribute"></seealso>
  ///	<seealso cref="SingletonPerThreadAttribute"></seealso>
  ///	<seealso cref="PooledAttribute"></seealso>
  ///	<seealso cref="TLifetimeType"></seealso>
  {$ENDREGION}
  SingletonAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Represents that a new instance of the component will be created
  ///	when requested.</summary>
  ///	<remarks>
  ///	  <note type="note">This attribute is the default option.</note>
  ///	</remarks>
  ///	<seealso cref="SingletonAttribute"></seealso>
  ///	<seealso cref="SingletonPerThreadAttribute"></seealso>
  ///	<seealso cref="PooledAttribute"></seealso>
  ///	<seealso cref="TLifetimeType"></seealso>
  {$ENDREGION}
  TransientAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  /// <summary>
  /// Applies this attribute when a component shares the single instance per thread.
  /// </summary>
  ///	<seealso cref="SingletonAttribute"></seealso>
  ///	<seealso cref="TransientAttribute"></seealso>
  ///	<seealso cref="PooledAttribute"></seealso>
  ///	<seealso cref="TLifetimeType"></seealso>
  SingletonPerThreadAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  /// <summary>
  /// Represents that the target component can be pooled.
  /// </summary>
  ///	<seealso cref="SingletonAttribute"></seealso>
  ///	<seealso cref="TransientAttribute"></seealso>
  ///	<seealso cref="SingletonPerThreadAttribute"></seealso>
  ///	<seealso cref="TLifetimeType"></seealso>
  PooledAttribute = class(LifetimeAttributeBase)
  private
    fMinPoolsize: Integer;
    fMaxPoolsize: Integer;
  public
    constructor Create(minPoolSize, maxPoolSize: Integer);
    property MinPoolsize: Integer read fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize;
  end;

//  TCustomLifetimeAttribute = class abstract(LifetimeAttributeBase)
//  end;

  ///	<summary>Applies the <c>InjectionAttribute</c> to injectable instance
  ///	members of a class. e.g. constructors, methods, properties and even
  ///	fields. Also works on parameters of a method.</summary>
  ///	<seealso cref="ImplementsAttribute"></seealso>
  InjectionAttribute = class(TCustomAttribute)
  private
    fValue: string;
    function GetHasValue: Boolean;
  public
    constructor Create; overload;
    constructor Create(const value: string); overload;
    property Value: string read fValue;
    property HasValue: Boolean read GetHasValue;
  end;

 {$REGION 'Documentation'}
 ///	<summary>Applies this attribute to tell the IoC container which service is
 ///	implemented by the target component. In addition, a service name can be
 ///	specified.</summary>
 ///	<remarks>
 ///	  <note type="note">This attribute can be specified more than
 ///	  once.</note>
 ///	</remarks>
 ///	<example>
 ///	  <code lang="Delphi">
 ///	[Implements(TypeInfo(IEmailSender))]
 ///	TRegularEmailSender = class(TInterfacedObject, IEmailSender)
 ///	end;
 ///	[Implements(TypeInfo(IEmailSender), 'mock-email-sender')]
 ///	TMockEmailSender = class(TInterfacedObject, IEmailSender)
 ///	end;
 ///	</code>
 ///	</example>
 ///	<seealso cref="InjectionAttribute"></seealso>
 {$ENDREGION}
 ImplementsAttribute = class(TCustomAttribute)
  private
    fServiceType: PTypeInfo;
    fName: string;
  public
    constructor Create(serviceType: PTypeInfo); overload;
    constructor Create(serviceType: PTypeInfo; const name: string); overload;
    property ServiceType: PTypeInfo read fServiceType;
    property Name: string read fName;
  end;

  {$ENDREGION}


  {$REGION 'Exceptions'}

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

//  EArithmeticException = EMathError;
//  ETimeoutException = class(Exception);

  ERttiException = class(Exception);

  {$ENDREGION}


  {$REGION 'Global Routines'}


  {$REGION 'Documentation'}
  ///	<summary>Retrieves the byte length of a unicode string.</summary>
  ///	<param name="s">the unicode string.</param>
  ///	<returns>The byte length of the unicode string.</returns>
  ///	<remarks>Although there is already a routine
  ///	<c>SysUtils.ByteLength(string)</c> function, it only supports unicode
  ///	strings and doesn't provide overloads for WideStrings and
  ///	AnsiStrings.</remarks>
  ///	<seealso cref="GetByteLength(WideString)"></seealso>
  ///	<seealso cref="GetByteLength(RawByteString)"></seealso>
  {$ENDREGION}
  function GetByteLength(const s: string): Integer; overload; inline;

  {$REGION 'Documentation'}
  ///	<summary>Retrieves the byte length of a WideString.</summary>
  ///	<param name="s">A wide string.</param>
  ///	<returns>The byte length of the wide string.</returns>
  ///	<seealso cref="GetByteLength(string)"></seealso>
  ///	<seealso cref="GetByteLength(RawByteString)"></seealso>
  {$ENDREGION}
  function GetByteLength(const s: WideString): Integer; overload; inline;

  {$REGION 'Documentation'}
  ///	<summary>Retrieves the byte length of a <c>RawByteString</c> (AnsiString
  ///	or UTF8String).</summary>
  ///	<returns>The byte length of the raw byte string.</returns>
  ///	<seealso cref="GetByteLength(string)"></seealso>
  ///	<seealso cref="GetByteLength(WideString)"></seealso>
  {$ENDREGION}
  function GetByteLength(const s: RawByteString): Integer; overload; inline;

  {$REGION 'Documentation'}
  ///	<summary>Determines whether a specified file exists. An
  ///	<see cref="EFileNotFoundException" /> exception will be raised when not
  ///	found.</summary>
  ///	<param name="fileName">the file name.</param>
  ///	<exception cref="EFileNotFoundException">Raised if the target file does
  ///	not exist.</exception>
  ///	<seealso cref="CheckDirectoryExists(string)"></seealso>
  {$ENDREGION}
  procedure CheckFileExists(const fileName: string);

  {$REGION 'Documentation'}
  ///	<summary>Determines whether a specified directory exists. An
  ///	<see cref="EDirectoryNotFoundException" /> exception will be raised when not
  ///	found.</summary>
  ///	<exception cref="EDirectoryNotFoundException">Raised if the directory
  ///	doesn't exist.</exception>
  ///	<seealso cref="CheckFileExists(string)"></seealso>
  {$ENDREGION}
  procedure CheckDirectoryExists(const directory: string);

  /// <summary>
  /// Overloads. SplitString
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

  {$REGION 'Documentation'}
  ///	<summary>Returns a string array that contains the substrings in the
  ///	buffer that are delimited by null char (#0) and ends with an additional
  ///	null char.</summary>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	procedure TestSplitNullTerminatedStrings;
  ///	var
  ///	  buffer: string;
  ///	  strings: TStringDynArray;
  ///	  s: string;
  ///	begin
  ///	  buffer := 'C:'#0'D:'#0'E:'#0#0;
  ///	  strings := SplitString(PChar(buffer));
  ///	  for s in strings do
  ///	  begin
  ///	    Writeln(s);
  ///	  end;
  ///	end;
  ///	</code>
  ///	</example>
  {$ENDREGION}
  function SplitString(const buffer: PChar): TStringDynArray; overload;

  /// <summary>
  /// Returns a string array that contains the substrings in the buffer that are
  /// delimited by null char (#0) and ends with an additional null char.
  /// </summary>
  function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
    deprecated 'Use the SpitString(PChar) function instead.';

  {$REGION 'Documentation'}
  ///	<summary>Executes a method call within the main thread.</summary>
  ///	<param name="threadProc">An anonymous method that will be
  ///	executed.</param>
  ///	<exception cref="EArgumentNullException">Raised if <paramref name=
  ///	"threadProc" /> was not assigned.</exception>
  {$ENDREGION}
  procedure Synchronize(threadProc: TThreadProcedure);

  {$REGION 'Documentation'}
  ///	<summary>Asynchronously executes a method call within the main
  ///	thread.</summary>
  ///	<param name="threadProc">An anonymous method that will be
  ///	executed.</param>
  ///	<exception cref="EArgumentNullException">Raised if threadProc was not
  ///	assigned.</exception>
  {$ENDREGION}
  procedure Queue(threadProc: TThreadProcedure);

  ///	<summary>Try getting property information of an object.</summary>
  ///	<returns>Returns true if the instance has the specified property and the
  ///	property has property information.</returns>
  ///	<exception cref="EArgumentNullException">if instance is nil.</exception>
  function TryGetPropInfo(instance: TObject; const propertyName: string;
    out propInfo: PPropInfo): Boolean;

  {$REGION 'Documentation'}
  ///	<summary>Try parsing a string to a datetime value based on the specified
  ///	format. Returns True if the input string matches the format.</summary>
  ///	<param name="s">the input string</param>
  ///	<param name="format">the format of datetime</param>
  ///	<param name="value">output datetime value</param>
  ///	<returns>Returns True if the input string can be parsed.</returns>
  {$ENDREGION}
  function TryConvertStrToDateTime(const s, format: string; out value: TDateTime): Boolean;

  {$REGION 'Documentation'}
  ///	<summary>Parses a string to a datetime value based on the specified
  ///	format. An EConvertError exception will be raised if failed to parse the
  ///	string.</summary>
  ///	<param name="format">the format of datetime.</param>
  ///	<param name="s">the date time string.</param>
  {$ENDREGION}
  function ConvertStrToDateTime(const s, format: string): TDateTime;

  function TryParseDateTime(const s, format: string; out value: TDateTime): Boolean;
    deprecated 'Use TryConvertStrToDateTime instead.';

  function ParseDateTime(const s, format: string): TDateTime;
    deprecated 'Use ConvertStrToDateTime instead.';

  ///	<summary>Determines if a variant is null or empty. The parameter
  ///"trimWhiteSpace" is an option only for strings.</summary>
  function VarIsNullOrEmpty(const value: Variant; trimWhiteSpace: Boolean = False): Boolean;

  // >>>NOTE<<<
  // Due to the QC #80304, the following methods (with anonymous methods)
  // must not be inlined.

  {$REGION 'Documentation'}
  ///	<summary>Obtains a mutual-exclusion lock for the given object, executes a
  ///	procedure and then releases the lock.</summary>
  ///	<param name="obj">the sync root.</param>
  ///	<param name="proc">the procedure that will be invoked.</param>
  ///	<exception cref="Spring|EArgumentNullException">Raised if <paramref name=
  ///	"obj" /> is nil or <paramref name="proc" /> is unassigned.</exception>
  {$ENDREGION}
  procedure Lock(obj: TObject; const proc: TProc); overload; // inline;
  procedure Lock(const intf: IInterface; const proc: TProc); overload; // inline;

  {$REGION 'Documentation'}
  ///	<summary>Updates an instance of <see cref="Classes|TStrings" /> by calling its
  ///	BeginUpdate and EndUpdate.</summary>
  ///	<param name="strings">an instance of TStrings.</param>
  ///	<exception cref="EArgumentNullException">Raised if <paramref name=
  ///	"strings" /> is nil or <paramref name="proc" /> is not
  ///	assigned.</exception>
  {$ENDREGION}
  procedure UpdateStrings(strings: TStrings; proc: TProc); // inline;


  // TODO: Consider using a interface such as INullableHandler to perform these actions

  {$REGION 'Documentation'}
  ///	<summary>Try getting the underlying type name of a nullable
  ///	type.</summary>
  ///	<remarks>For instance, the underlying type name of the type
  ///	<c>TNullable&lt;System.Integer&gt;</c> is
  ///	<c>System.Integer</c>.</remarks>
  {$ENDREGION}
  function TryGetUnderlyingTypeName(typeInfo: PTypeInfo; out underlyingTypeName: string): Boolean;

  ///	<summary>Try getting the underlying type info of a nullable
  ///	type.</summary>
  function TryGetUnderlyingTypeInfo(typeInfo: PTypeInfo; out underlyingTypeInfo: PTypeInfo): Boolean;

  ///	<summary>Try getting the underlying value of a nullable type.</summary>
  ///	<returns>Returns True if the value is a <c>TNullable&lt;T&gt;</c> and it
  ///	has value.</returns>
  function TryGetUnderlyingValue(const value: TValue; out underlyingValue: TValue): Boolean;

  {$REGION 'Documentation'}
  ///	<summary>Uses this function to get an interface instance from a
  ///	TValue.</summary>
  ///	<remarks>
  ///	  <note type="warning">Rtti bugs: QC #82433 if
  ///	  value.TryAsType&lt;IPropertyNotification&gt;(propertyNotification)
  ///	  then</note>
  ///	</remarks>
  {$ENDREGION}
  function TryGetInterface(const instance: TValue; const guid: TGuid; out intf): Boolean; overload;

  ///	<seealso cref="Spring|TNullable{T}"></seealso>
  function TryGetInterface(const instance: TValue; const guid: TGuid): Boolean; overload;

  {$ENDREGION}


  {$REGION 'Constants'}

const
  ///	<summary>Represents bytes of one KB.</summary>
  COneKB: Int64 = 1024;            // 1KB = 1024 bytes

  ///	<summary>Represents bytes of one MB.</summary>
  COneMB: Int64 = 1048576;         // 1MB = 1024 KB

  ///	<summary>Represents bytes of one GB.</summary>
  COneGB: Int64 = 1073741824;      // 1GB = 1024 MB

  ///	<summary>Represents bytes of one TB.</summary>
  COneTB: Int64 = 1099511627776;   // 1TB = 1024 GB

  ///	<summary>Represents bytes of one KB.</summary>
  OneKB: Int64 = 1024 deprecated 'Use COneKB instead.';

  ///	<summary>Represents bytes of one MB.</summary>
  OneMB: Int64 = 1048576 deprecated 'Use COneMB instead.';

  ///	<summary>Represents bytes of one GB.</summary>
  OneGB: Int64 = 1073741824 deprecated 'Use COneGB instead.';

  ///	<summary>Represents bytes of one TB.</summary>
  OneTB: Int64 = 1099511627776 deprecated 'Use COneTB instead.';

  {$ENDREGION}


implementation

uses
  StrUtils,
  Spring.ResourceStrings;


{$REGION 'Global Routines'}

function GetByteLength(const s: string): Integer;
begin
  Result := Length(s) * SizeOf(Char);
end;

function GetByteLength(const s: WideString): Integer;
begin
  Result := Length(s) * SizeOf(WideChar);
end;

function GetByteLength(const s: RawByteString): Integer;
begin
  Result := Length(s);
end;

procedure CheckFileExists(const fileName: string);
begin
  if not FileExists(fileName) then
  begin
    raise EFileNotFoundException.CreateResFmt(@SFileNotFoundException, [fileName]);
  end;
end;

procedure CheckDirectoryExists(const directory: string);
begin
  if not DirectoryExists(directory) then
  begin
    raise EDirectoryNotFoundException.CreateResFmt(@SDirectoryNotFoundException, [directory]);
  end;
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

function SplitString(const buffer: PChar): TStringDynArray;
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

function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
begin
  Result := SplitString(buffer);
end;

procedure Synchronize(threadProc: TThreadProcedure);
begin
  TArgument.CheckNotNull(Assigned(threadProc), 'threadProc');
  TThread.Synchronize(TThread.CurrentThread, threadProc);
end;

procedure Queue(threadProc: TThreadProcedure);
begin
  TArgument.CheckNotNull(Assigned(threadProc), 'threadProc');
  TThread.Queue(TThread.CurrentThread, threadProc);
end;

function TryGetPropInfo(instance: TObject; const propertyName: string;
  out propInfo: PPropInfo): Boolean;
begin
  TArgument.CheckNotNull(instance, 'instance');
  propInfo := GetPropInfo(instance, propertyName);
  Result := propInfo <> nil;
end;

function TryConvertStrToDateTime(const s, format: string; out value: TDateTime): Boolean;
var
  localString: string;
  stringFormat: string;
  year, month, day: Word;
  hour, minute, second, milliSecond: Word;

  function ExtractElementDef(const element: string; const defaultValue: Integer = 0): Integer;
  var
    position: Integer;
  begin
    position := Pos(element, stringFormat);
    if position > 0 then
    begin
      Result := StrToInt(Copy(localString, position, Length(element)));
    end
    else
    begin
      Result := defaultValue;
    end;
  end;
begin
  localString := Trim(s);
  stringFormat := UpperCase(format);
  Result := Length(localString) = Length(stringFormat);
  if Result then
  try
    year := ExtractElementDef('YYYY', 0);
    if year = 0 then
    begin
      year := ExtractElementDef('YY', 1899);
      if year < 1899 then
      begin
        Inc(year, (DateUtils.YearOf(Today) div 100) * 100);
      end;
    end;
    month := ExtractElementDef('MM', 12);
    day := ExtractElementDef('DD', 30);
    hour := ExtractElementDef('HH');
    minute := ExtractElementDef('NN');
    second := ExtractElementDef('SS');
    milliSecond := ExtractElementDef('ZZZ');
    value := EncodeDateTime(year, month, day, hour, minute, second, milliSecond);
  except
    Result := False;
  end;
end;

function ConvertStrToDateTime(const s, format: string): TDateTime;
begin
  if not TryConvertStrToDateTime(s, format, Result) then
  begin
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [s]);
  end;
end;

function TryParseDateTime(const s, format: string; out value: TDateTime): Boolean;
begin
  Result := TryConvertStrToDateTime(s, format, value);
end;

function ParseDateTime(const s, format: string): TDateTime;
begin
  Result := ConvertStrToDateTime(s, format);
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

procedure Lock(obj: TObject; const proc: TProc);
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

procedure Lock(const intf: IInterface; const proc: TProc);
var
  obj: TObject;
begin
  TArgument.CheckNotNull(intf, 'intf');
  obj := TObject(intf);
  Lock(obj, proc);
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

function TryGetUnderlyingTypeName(typeInfo: PTypeInfo; out underlyingTypeName: string): Boolean;
const
  PrefixString = 'TNullable<';    // DO NOT LOCALIZE
  PrefixStringLength = Length(PrefixString);
var
  typeName: string;
begin
  if (typeInfo = nil) or (typeInfo.Kind <> tkRecord) then
  begin
    Exit(False);
  end;
  typeName := TypInfo.GetTypeName(typeInfo);
  if (Length(typeName) < PrefixStringLength) or
    not SameText(LeftStr(typeName, PrefixStringLength), PrefixString) then
  begin
    Exit(False);
  end;
  Result := True;
  underlyingTypeName := Copy(typeName, PrefixStringLength + 1,
    Length(typeName) - PrefixStringLength - 1);
end;

function TryGetUnderlyingTypeInfo(typeInfo: PTypeInfo; out underlyingTypeInfo: PTypeInfo): Boolean;
var
  underlyingTypeName: string;
  rttiType: TRttiType;
  context: TRttiContext;
begin
  Result := TryGetUnderlyingTypeName(typeInfo, underlyingTypeName);
  if Result then
  begin
    context := TRttiContext.Create;
    rttiType := context.FindType(underlyingTypeName);
    if rttiType <> nil then
      underlyingTypeInfo := rttiType.Handle
    else
      underlyingTypeInfo := nil;
    Result := underlyingTypeInfo <> nil;
  end;
end;

function TryGetUnderlyingValue(const value: TValue; out underlyingValue: TValue): Boolean;
var
  underlyingTypeInfo: PTypeInfo;
  hasValueString: string;
  p: Pointer;
begin
  Result := TryGetUnderlyingTypeInfo(value.TypeInfo, underlyingTypeInfo);
  if not Result then
  begin
    Exit;
  end;
  p := value.GetReferenceToRawData;
  hasValueString := PString(PByte(p) + (value.DataSize - SizeOf(string)))^;
  if hasValueString = '' then
  begin
    Exit(False);
  end;
  TValue.Make(p, underlyingTypeInfo, underlyingValue);
end;

function TryGetInterface(const instance: TValue; const guid: TGuid; out intf): Boolean;
var
  localInterface: IInterface;
begin
  if instance.IsEmpty then Exit(False);
  if instance.IsObject then
  begin
    Result := instance.AsObject.GetInterface(guid, intf);
  end
  else if instance.TryAsType<IInterface>(localInterface) then
  begin
    Result := localInterface.QueryInterface(guid, intf) = S_OK;
  end
  else
  begin
    Exit(False);
  end;
end;

function TryGetInterface(const instance: TValue; const guid: TGuid): Boolean;
var
  localInterface: IInterface;
begin
  if instance.IsEmpty then Exit(False);
  if instance.IsObject then
  begin
    Result := Supports(instance.AsObject, guid);
  end
  else if instance.TryAsType<IInterface>(localInterface) then
  begin
    Result := Supports(localInterface, guid);
  end
  else
  begin
    Exit(False);
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


{$REGION 'TInterfacedThread'}

function TInterfacedThread.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedThread._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfacedThread._Release: Integer;
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
  TArgument.CheckRange(length, startIndex, count, 0);
end;

class procedure TArgument.DoCheckStringIndex(const length, index: Integer);
begin
  DoCheckIndex(length, index, 1);
end;

class procedure TArgument.DoCheckStringRange(const length, startIndex,
  count: Integer);
begin
  TArgument.CheckRange(length, startIndex, count, 1);
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

class procedure TArgument.CheckRange(const length, startIndex,
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
  const msg: string);
begin
  if not condition then
  begin
    raise EArgumentException.Create(msg);
  end;
end;

class procedure TArgument.CheckFalse(condition: Boolean;
  const msg: string);
begin
  if condition then
  begin
    raise EArgumentException.Create(msg);
  end;
end;

class procedure TArgument.CheckInheritsFrom(const checkclazz, clazz: TClass;
  const argumentName: string);
begin
  ASSERT(Assigned(checkclazz));
  ASSERT(Assigned(clazz));

  if (not checkclazz.InheritsFrom(clazz)) then
    raise EArgumentException.CreateResFmt(@SBadObjectInheritance, [argumentName, checkclazz.ClassName, clazz.ClassName]);
end;

class procedure TArgument.CheckInheritsFrom(obj: TObject; const clazz: TClass;
  const argumentName: string);
begin
  if Assigned(obj) then
    CheckInheritsFrom(obj.ClassType, clazz, argumentName);
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

class procedure TArgument.CheckNotNull<T>(const value: T; const argumentName: string);
begin
  if IsNullReference<T>(value) then
  begin
    TArgument.RaiseArgumentNullException(argumentName);
  end;
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
      [argumentName, GetTypeName(TypeInfo(T)), value]
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

class procedure TArgument.CheckTypeKind(typeInfo: PTypeInfo;
  const expectedTypeKind: TTypeKind; const argumentName: string);
begin
  TArgument.CheckNotNull(typeInfo, argumentName);
  if typeInfo.Kind <> expectedTypeKind then
  begin
    raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument, [typeInfo.Name, argumentName]);
  end;
end;

class procedure TArgument.CheckTypeKind(typeInfo: PTypeInfo;
  const expectedTypeKinds: TTypeKinds; const argumentName: string);
begin
  TArgument.CheckNotNull(typeInfo, argumentName);
  if not (typeInfo.Kind in expectedTypeKinds) then
  begin
    raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument, [typeInfo.Name, argumentName]);
  end;
end;

//  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
//    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
//    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString,
//    tkClassRef, tkPointer, tkProcedure);
class function TArgument.IsNullReference(const value; typeInfo: PTypeInfo): Boolean;
begin
  Result := (typeInfo <> nil) and
    (typeInfo.Kind in [tkPointer, tkClass, tkClassRef, tkInterface, tkProcedure, tkMethod]);
  Result := Result and not Assigned(@value);
end;

class function TArgument.IsNullReference<T>(const value: T): Boolean;
var
  localTypeInfo: PTypeInfo;
begin
  localTypeInfo := TypeInfo(T);
  Result := TArgument.IsNullReference(value, localTypeInfo);
end;

class procedure TArgument.RaiseArgumentException(const msg: string);
begin
  raise EArgumentException.Create(msg);
end;

class procedure TArgument.RaiseArgumentNullException(
  const argumentName: string);
begin
  raise EArgumentNullException.CreateResFmt(@SArgumentNullException, [argumentName]);
end;

class procedure TArgument.RaiseArgumentOutOfRangeException(
  const argumentName: string);
begin
  raise EArgumentOutOfRangeException.CreateResFmt(@SArgumentOutOfRangeException, [argumentName]);
end;

class procedure TArgument.RaiseArgumentFormatException(const argumentName: string);
begin
  raise EConvertError.CreateResFmt(@SInvalidArgumentFormat, [argumentName]);
end;

class procedure TArgument.RaiseInvalidEnumArgumentException(
  const argumentName: string);
begin
  raise EInvalidEnumArgumentException.CreateResFmt(@SInvalidEnumArgument, [argumentName]);
end;

{$ENDREGION}


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


{$REGION 'TEnum'}

class function TEnum.GetEnumTypeInfo<T>: PTypeInfo;
begin
  Result := TypeInfo(T);
  TArgument.CheckTypeKind(Result, tkEnumeration, 'T');
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
  typeInfo: PTypeInfo;
  data: PTypeData;
begin
  typeInfo := System.TypeInfo(T);
  TArgument.CheckTypeKind(typeInfo, [tkEnumeration], 'T');
  data := GetTypeData(typeInfo);
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
  typeInfo: PTypeInfo;
begin
  TArgument.CheckEnum<T>(value, 'value');

  typeInfo := GetEnumTypeInfo<T>;
  Result := GetEnumName(typeInfo, value);
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

class function TEnum.GetValueStrings<T>: TStringDynArray;
var
  typeData: PTypeData;
  i: Integer;
begin
  typeData := TEnum.GetEnumTypeData<T>;
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  for i := 0 to High(Result) do
  begin
    Result[i] := IntToStr(i);
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
  typeInfo: PTypeInfo;
  intValue: Integer;
begin
  typeInfo := TEnum.GetEnumTypeInfo<T>;
  intValue := GetEnumValue(typeInfo, value);
  Result := TEnum.TryParse<T>(intValue, enum);
end;

class function TEnum.Parse<T>(const value: Integer): T;
begin
  if not TEnum.TryParse<T>(value, Result) then
    raise EFormatException.CreateResFmt(@SIncorrectFormat, [IntToStr(value)]);
end;

class function TEnum.Parse<T>(const value: string): T;
begin
  if not TEnum.TryParse<T>(value, Result) then
    raise EFormatException.CreateResFmt(@SIncorrectFormat, [value]);
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
  end
  else
  begin
    Clear;
  end;
end;

procedure TNullable<T>.Clear;
begin
  fHasValue := '';
end;

function TNullable<T>.GetHasValue: Boolean;
begin
  Result := Length(fHasValue) > 0;
end;

function TNullable<T>.GetValue: T;
begin
  if not HasValue then
  begin
    raise EInvalidOperation.CreateRes(@SNullableTypeHasNoValue);
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
  Result := value.Value;
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
    Result := TNullable<T>.Create(v.AsType<T>);
  end
  else
  begin
    Result.Clear;
  end;
end;

class operator TNullable<T>.Implicit(value: Pointer): TNullable<T>;
begin
  if value = nil then
  begin
    Result.Clear;
  end
  else
  begin
    raise EInvalidOperation.CreateRes(@SCannotAssignPointerToNullable);
  end;
end;

class operator TNullable<T>.Explicit(const value: TNullable<T>): T;
begin
  Result := value.Value;
end;

{$ENDREGION}


{$REGION 'TValueProvider'}

function TValueProviderBase.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

procedure TValueProviderBase.SetValue(const value: TValue);
begin
  if not GetIsReadOnly then
  begin
    DoSetValue(value);
  end
  else
  begin
    raise EInvalidOperation.CreateRes(@SCannotModifyReadOnlyValue);
  end;
end;

{$ENDREGION}


{$REGION 'TDelegate<T>'}

procedure TDelegate<T>.Clear;
begin
  if fHandlers <> nil then
    fHandlers.Clear;
end;

destructor TDelegate<T>.Destroy;
begin
  fHandlers.Free;
  inherited Destroy;
end;

function TDelegate<T>.GetHandlers: TList<T>;
begin
  if fHandlers = nil then
  begin
    fHandlers := TList<T>.Create;
  end;
  Result := fHandlers;
end;

function TDelegate<T>.AddHandler(
  const handler: T): IDelegate<T>;
begin
  Handlers.Add(handler);
  Result := Self;
end;

function TDelegate<T>.RemoveHandler(
  const handler: T): IDelegate<T>;
begin
  Handlers.Remove(handler);
  Result := Self;
end;

function TDelegate<T>.Invoke(
  const callback: TProc<T>): IDelegate<T>;
var
  delegate: T;
begin
  if fHandlers <> nil then
  begin
    for delegate in fHandlers do
    begin
      callback(delegate);
    end;
  end;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TObjectHolder<T>'}

constructor TObjectHolder<T>.Create(obj: T);
var
  lifetimeWatcher: IInterface;
begin
  TArgument.CheckNotNull(PPointer(@obj)^, 'obj');
  if obj.InheritsFrom(TInterfacedObject) then
  begin
    obj.GetInterface(IInterface, lifetimeWatcher);
  end
  else
  begin
    lifetimeWatcher := nil;
  end;
  Create(obj, lifetimeWatcher);
end;

constructor TObjectHolder<T>.Create(obj: T; const lifetimeWatcher: IInterface);
begin
  inherited Create;
  fObject := obj;
  fLifetimeWatcher := lifetimeWatcher;
end;

destructor TObjectHolder<T>.Destroy;
begin
  if fLifetimeWatcher = nil then
  begin
    fObject.Free;
  end;
  inherited Destroy;
end;

function TObjectHolder<T>.Invoke: T;
begin
  Result := fObject;
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


{$REGION 'TWeakReference<T>'}

constructor TWeakReference<T>.Create(const value: T);
begin
  fValue := Pointer(IInterface(value));
end;

function TWeakReference<T>.GetValue: T;
begin
  IInterface(Result) := IInterface(fValue);
end;

class operator TWeakReference<T>.Implicit(const reference: TWeakReference<T>): T;
begin
  Result := reference.Value;
end;

class operator TWeakReference<T>.Implicit(const value: T): TWeakReference<T>;
begin
  Result := TWeakReference<T>.Create(value);
end;

{$ENDREGION}


{$REGION 'TVolatile<T>'}

{$WARNINGS OFF}

function TVolatile<T>.GetValue: T;
begin
  MemoryBarrier;
  Result := fValue;
end;

procedure TVolatile<T>.SetValue(const newValue: T);
begin
  MemoryBarrier;
  fValue := newValue;
end;

class operator TVolatile<T>.Implicit(const value: T): TVolatile<T>;
begin
  Result.Value := value;
end;

class operator TVolatile<T>.Implicit(const value: TVolatile<T>): T;
begin
  Result := value.Value;
end;

class operator TVolatile<T>.Equal(const left, right: TVolatile<T>): Boolean;
begin
  Result := TEqualityComparer<T>.Default.Equals(left, right);
end;

class operator TVolatile<T>.NotEqual(const left, right: TVolatile<T>): Boolean;
begin
  Result := not TEqualityComparer<T>.Default.Equals(left, right);
end;

{$WARNINGS ON}

{$ENDREGION}


{$REGION 'TLazyUtils'}

class function TLazyUtils.GetValue<T>(var field: T;
  const delegate: TFunc<T>): T;
var
  localValue: T;
begin
  if PPointer(@field)^ = nil then
  begin
    localValue := delegate();
    if InterlockedCompareExchangePointer(PPointer(@field)^, PPointer(@localValue)^, nil) <> nil then
      localValue.Free;
  end;
  Result := field;
end;

{$ENDREGION}


{$REGION 'Attributes'}

{ LifetimeAttributeBase }

constructor LifetimeAttributeBase.Create(lifetimeType: TLifetimeType);
begin
  inherited Create;
  fLifetimeType := lifetimeType;
end;

{ SingletonAttribute }

constructor SingletonAttribute.Create;
begin
  inherited Create(TLifetimeType.ltSingleton);
end;

{ TransientAttribute }

constructor TransientAttribute.Create;
begin
  inherited Create(TLifetimeType.ltTransient);
end;

{ SingletonPerThreadAttribute }

constructor SingletonPerThreadAttribute.Create;
begin
  inherited Create(TLifetimeType.ltSingletonPerThread);
end;

{ InjectionAttribute }

constructor InjectionAttribute.Create;
begin
  Create('');
end;

constructor InjectionAttribute.Create(const value: string);
begin
  inherited Create;
  fValue := value;
end;

function InjectionAttribute.GetHasValue: Boolean;
begin
  Result := fValue <> '';
end;

{ ImplementsAttribute }

constructor ImplementsAttribute.Create(serviceType: PTypeInfo);
begin
  Create(serviceType, '');
end;

constructor ImplementsAttribute.Create(serviceType: PTypeInfo;
  const name: string);
begin
  inherited Create;
  fServiceType := serviceType;
  fName := name;
end;

{ PooledAttribute }

constructor PooledAttribute.Create(minPoolSize, maxPoolSize: Integer);
begin
  inherited Create(ltPooled);
  fMinPoolsize := minPoolSize;
  fMaxPoolsize := maxPoolsize;
end;

{$ENDREGION}

end.


