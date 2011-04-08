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
  Generics.Defaults,
  Generics.Collections,
  Diagnostics,
  TimeSpan,
  Rtti,
  Variants;

type
  ///	<summary>Represents a dynamic array of Byte.</summary>
  TBytes = SysUtils.TBytes;

  ///	<summary>Represents a type information.</summary>
  PTypeInfo = TypInfo.PTypeInfo;

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

  ///	<summary>Represents the class type of <c>TCustomAttribute</c>.</summary>
  TAttributeClass = class of TCustomAttribute;

{$IFNDEF DELPHIXE_UP}
  TThreadID = LongWord;
{$ENDIF}

  ///	<summary>Represents a logical predicate.</summary>
  ///	<param name="value">the value needs to be determined.</param>
  ///	<returns>Returns True if the value was accepted, otherwise, returns
  ///	false.</returns>
  ///	<remarks>
  ///	  <note type="tip">This type redefined the
  ///	  <see cref="SysUtils|TPredicate`1">SysUtils.TPredicate&lt;T&gt;</see> type with a const parameter.</note>
  ///	</remarks>
  /// <seealso cref="Spring.DesignPatterns|ISpecification{T}" />
  TPredicate<T> = reference to function(const value: T): Boolean;

  /// <summary>
  /// Represents a method that has a single parameter and does not return a value.
  /// </summary>
  TAction<T> = reference to procedure(const obj: T);

  /// <summary>
  /// Represents a procedure that has a single parameter and does not return a value.
  /// </summary>
  TActionProc<T> = procedure(const obj: T);

  /// <summary>
  /// Represents a instance method that has a single parameter and does not return a value.
  /// </summary>
  TActionMethod<T> = procedure(const obj: T) of object;

  /// <summary>
  /// Provides a non-reference-counted <see cref="System|IInterface" /> implementation.
  /// </summary>
  TInterfaceBase = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  ///	<summary>
  /// Provides an abstract class base of TThread that implements the
  ///	IInterface.
  /// </summary>
  TInterfacedThread = class(TThread, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Provides static methods to check arguments and raise argument
  ///	exceptions.</summary>
  ///	<remarks>It's recommended that all arguments of public methods, including
  ///	global routines, class and record methods, should be checked.</remarks>
  {$ENDREGION}
  TArgument = class
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
  {$IFNDEF DisableGenerics}
    class procedure CheckNotNull<T>(const value: T; const argumentName: string); overload; static; inline;

    class procedure CheckEnum<T{:enum}>(const value: T; const argumentName: string); overload; static; inline;
    class procedure CheckEnum<T{:enum}>(const value: Integer; const argumentName: string); overload; static; inline;
  {$ENDIF}

    ///	<exception cref="Spring|EArgumentOutOfRangeException">Raised if the
    ///	<paramref name="index" /> is out of range.</exception>
    class procedure CheckRange(const buffer: array of Byte; const index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Byte; const startIndex, count: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; const index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; const startIndex, count: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; const index: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; const startIndex, count: Integer); overload; static;
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

  {$IFNDEF DisableGenerics}
    class function IsNullReference<T>(const value: T): Boolean; overload; static;
  {$ENDIF}
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


  /// <summary>
  /// Provides static methods to manipulate an enumeration type.
  /// </summary>
  TEnum = class
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

  /// <summary>
  /// Provides static methods to manipulate a variant type.
  /// </summary>
  TVariant = class
//    class function IsNullOrEmpty(const value: Variant): Boolean;
  end;

  TMemory = class
  // Comparer, Conversion, Fill
//    class function BinToHex(const buffer: Pointer; count: Integer): string; overload; static;
//    class function BinToHex(const buffer: Pointer; count: Integer;
//      const prefix: string; const delimiter: string = ' '): string; overload; static;
  end;


  /// <summary>
  /// Provides static methods to manipulate an array.
  /// </summary>
  TArray = class(Generics.Collections.TArray)
  public
  (*
    class function Add<T>(var target: array of T; const value: T): Integer;
    class procedure Reverse<T>(var target: array of T);
    class procedure Delete<T>(var target: array of T; const index: Integer);
    class procedure Copy<T>(const source: array of T; var dest: array of T; len: Integer); overload;
    class function Contains<T>(const target: array of T; const value: T): Boolean;
    class function Exists<T>(const match: TPredicate<T>): Boolean;
    class function IndexOf<T>(const target: array of T; const value: T): Integer;
    class function LastIndexOf<T>(const target: array of T; const value: T): Integer;
    class function FindFirst<T>(const target: array of T; const predicate: TPredicate<T>): T;
    class function FindLast<T>(const target: array of T; const predicate: TPredicate<T>): T;
    class function FindAll<T>(const target: array of T; const predicate: TPredicate<T>): TArray<T>;
    ForEach
  //*)
  end;


  ///	<summary>Represents a series of bytes in memory.</summary>
  ///	<remarks>
  ///	  The <c>TBuffer</c> structure is actually a wrapper of a value of
  ///	  <c>TBytes</c> while provides some easy-going methods and properties.
  ///	  <note type="warning">This type needs to be reviewed.</note>
  ///	</remarks>
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


  ///	<summary>Represents an "object" whose underlying type is a value type
  ///	that can also be assigned nil like a reference type.</summary>
  ///	<typeparam name="T">The underlying value type of the <see cref=
  ///	"TNullable`1" /> generic type.</typeparam>
  ///	<remarks>The <typeparamref name="T" /> must be a value type such as a
  ///	value of string, Integer.</remarks>
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

    /// <summary>
    /// Determines whether a variant value is null or empty.
    /// </summary>
    class function VarIsNullOrEmpty(const value: Variant; trimWhiteSpace: Boolean = False): Boolean; static;
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


  {$REGION 'Common TNullable<T> Aliases'}

  ///	<summary>Represents a nullable string.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableString = TNullable<string>;

  ///	<summary>Represents a nullable integer.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableInteger = TNullable<Integer>;

  ///	<summary>Represents a nullable <c>Int64</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableInt64 = TNullable<Int64>;

  ///	<summary>Represents a nullable native integer.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableNativeInt = TNullable<NativeInt>;

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

  ///	<summary>Represents a nullable <c>TGuid</c>.</summary>
  ///	<seealso cref="TNullable{T}"></seealso>
  TNullableGuid = TNullable<TGUID>;

  {$ENDREGION}

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
    ///	<seealso cref="Windows|InterlockedCompareExchangePointer"></seealso>
    {$ENDREGION}
    class function GetValue<T: class>(var field: T; const delegate: TFunc<T>): T; static;
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
  ///	  The following example demonstrates how to use the Smart Pointer:
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
  ///	<summary>Represents a lifetime watcher.</summary>
  ///	<remarks>The basic idea is using an instance of the <c>IInterface</c> in
  ///	the host such as a record. The anonymous method, which is specified by
  ///	the <paramref name="proc" /> parameter in the constructor, will be
  ///	executed when this interface is disposed. Normally, the proc is some kind
  ///	of clean up code.</remarks>
  {$ENDREGION}
  TLifetimeWatcher = class(TInterfacedObject)
  private
    fProc: TProc;
  public
    constructor Create(const proc: TProc);
    destructor Destroy; override;
  end;

  /// <summary>
  /// Provides a workaround to get or set the value of an indexed property.
  /// </summary>
  /// <preliminary />
  ISupportIndexedProperties = interface
    ['{7BF8ED3B-60AB-425E-B678-776FD2EA3437}']
    function GetPropertyValue(const propertyName: string; const index: TValue): TValue;
    procedure SetPropertyValue(const propertyName: string; const index, value: TValue);
  end;

  {$REGION 'Documentation'}
  ///	<summary>Lifetime Type Enumeration.</summary>
  ///	<seealso cref="SingletonAttribute"></seealso>
  ///	<seealso cref="TransientAttribute"></seealso>
  ///	<seealso cref="SingletonPerThreadAttribute"></seealso>
  ///	<seealso cref="PooledAttribute"></seealso>
  {$ENDREGION}
  TLifetimeType = (
    ///	<summary>Unknown lifetime type.</summary>
    ltUnknown,

    ///	<summary>Single instance.</summary>
    ltSingleton,

    ///	<summary>Different instances.</summary>
    ltTransient,

    ///	<summary>Every thread has a single instance.</summary>
    ltSingletonPerThread,

    ///	<summary>Instances are transient except that they are
    ///	recyclable.</summary>
    ltPooled,

    ///	<summary>Customized lifetime type.</summary>
    ltCustom
  );


  {$REGION 'Lifetime Type Attributes'}

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
  SingletonAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>Represents that a new instance of the component will be created
  ///	when requested.</summary>
  ///	<remarks>
  ///	  <note type="note">This attribute is the default option.</note>
  ///	</remarks>
  ///	<seealso cref="SingletonAttribute"></seealso>
  ///	<seealso cref="SingletonPerThreadAttribute"></seealso>
  ///	<seealso cref="PooledAttribute"></seealso>
  ///	<seealso cref="TLifetimeType"></seealso>
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

  {$REGION 'Documentation'}
  ///	<summary>Lifecycle interface. Represents that the component can be
  ///	started and stopped.</summary>
  ///	<seealso cref="IInitializable"></seealso>
  ///	<seealso cref="IRecyclable"></seealso>
  ///	<seealso cref="IDisposable"></seealso>
  {$ENDREGION}
  IStartable = interface
    ['{8D0252A1-7993-44AA-B0D9-326019B58E78}']
    procedure Start;
    procedure Stop;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Lifecycle interface. Only called for components that belongs to
  ///	a pool when the component comes back to the pool.</summary>
  ///	<seealso cref="IInitializable"></seealso>
  ///	<seealso cref="IStartable"></seealso>
  ///	<seealso cref="IDisposable"></seealso>
  {$ENDREGION}
  IRecyclable = interface
    ['{85114F41-70E5-4AF4-A375-E445D4619E4D}']
    procedure Recycle;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Lifecycle interface. If the component implements this interface,
  ///	all resources will be deallocate by calling the <c>Dispose</c>
  ///	method.</summary>
  ///	<seealso cref="IInitializable"></seealso>
  ///	<seealso cref="IStartable"></seealso>
  ///	<seealso cref="IRecyclable"></seealso>
  {$ENDREGION}
  IDisposable = interface
    ['{6708F9BF-0237-462F-AFA2-DF8EF21939EB}']
    procedure Dispose;
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
{$IFDEF DELPHIXE_UP}
  EArgumentNullException        = SysUtils.EArgumentNilException;
{$ELSE}
  EArgumentNullException        = class(EArgumentException);
{$ENDIF}
  EInvalidEnumArgumentException = class(EArgumentException);

  EIOException                  = SysUtils.EInOutError;
  EFileNotFoundException        = SysUtils.EFileNotFoundException;
  EDirectoryNotFoundException   = SysUtils.EDirectoryNotFoundException;
  EDriveNotFoundException       = class(EIOException);

  ERttiException = class(Exception);

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

class procedure TArgument.CheckRange<T>(const buffer: array of T;
  const index: Integer);
begin
  if (index < 0) or (index >= Length(buffer)) then
  begin
    RaiseArgumentOutOfRangeException('index');
  end;
end;

class procedure TArgument.CheckRange<T>(const buffer: array of T;
  const startIndex, count: Integer);
begin
  DoCheckArrayRange(Length(buffer), startIndex, count);
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

{$IFNDEF DisableGenerics}
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
{$ENDIF}

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

{$IFNDEF DisableGenerics}

class function TArgument.IsNullReference<T>(const value: T): Boolean;
var
  localTypeInfo: PTypeInfo;
begin
  localTypeInfo := TypeInfo(T);
  Result := TArgument.IsNullReference(value, localTypeInfo);
end;

{$ENDIF}

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


{$REGION 'TEnum'}

{$IFNDEF DisableGenerics}

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

{$ENDIF}

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

class function TNullable<T>.VarIsNullOrEmpty(const value: Variant;
  trimWhiteSpace: Boolean): Boolean;
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

