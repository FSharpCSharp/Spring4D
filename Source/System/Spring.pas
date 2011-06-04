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
  /// Represents a anonymous method that has a single parameter and does not return a value.
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
  ///	<summary>
  ///	  Provides static methods to check arguments and raise argument
  ///	  exceptions.
  ///	</summary>
  ///	<remarks>
  ///	  It's recommended that all arguments of public methods of global
  ///	  routines, class and record methods should be checked.
  ///	</remarks>
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

    class procedure CheckInheritsFrom(obj: TObject; const clazz: TClass; const parameterName: string); overload; static; inline;
    class procedure CheckInheritsFrom(const checkclazz, clazz: TClass; const parameterName: string); overload; static; inline;

    class procedure CheckNotNull(obj: TObject; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(p: Pointer; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(const intf: IInterface; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(condition: Boolean; const parameterName: string); overload; static; inline;
    class procedure CheckNotNull<T>(const value: T; const argumentName: string); overload; static; inline;

    class procedure CheckEnum<T{:enum}>(const value: T; const argumentName: string); overload; static; inline;
    class procedure CheckEnum<T{:enum}>(const value: Integer; const argumentName: string); overload; static; inline;

    ///	<exception cref="Spring|EArgumentOutOfRangeException">
    ///	  Raised if the <paramref name="index" /> is out of range.
    ///	</exception>
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

    class function IsNullReference(const value; typeInfo: PTypeInfo): Boolean; static;

    ///	<summary>Raises an EArgumentException exception.</summary>
    class procedure RaiseArgumentException(const msg: string); overload; static; inline;

    ///	<summary>Raises an EArgumentException exception.</summary>
    class procedure RaiseArgumentFormatException(const argumentName: string); overload; static; inline;

    ///	<summary>Raises an EArgumentNullException exception.</summary>
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static; inline;

    ///	<summary>Raises an EArgumentOutOfRangeException exception.</summary>
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static; inline;

    ///	<summary>Raises an EInvalidEnumArgumentException exception.</summary>
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static; inline;
  end;

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


  {$REGION 'TNullable<T> & Aliases'}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents an "object" whose underlying type is a value type that can
  ///	  also be assigned nil like a reference type.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The underlying value type of the <see cref="TNullable`1" /> generic
  ///	  type.
  ///	</typeparam>
  ///	<remarks>
  ///	  The <typeparamref name="T" /> must be a value type such as a value of
  ///	  string, Integer.
  ///	</remarks>
  {$ENDREGION}
  TNullable<T> = packed record
  private
    const CHasValueFlag = '@';  // DO NOT LOCALIZE
  strict private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;
  private
    ///	<summary>Internal use. Marks the current instance as null.</summary>
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


  ///	<summary>Represents a nullable string.</summary>
  TNullableString = TNullable<string>;

  ///	<summary>Represents a nullable integer.</summary>
  TNullableInteger = TNullable<Integer>;

  ///	<summary>Represents a nullable <c>Int64</c>.</summary>
  TNullableInt64 = TNullable<Int64>;

  ///	<summary>Represents a nullable native integer.</summary>
  TNullableNativeInt = TNullable<NativeInt>;

  ///	<summary>Represents a nullable <c>TDateTime</c>.</summary>
  TNullableDateTime = TNullable<TDateTime>;

  ///	<summary>Represents a nullable <c>Currency</c>.</summary>
  TNullableCurrency = TNullable<Currency>;

  ///	<summary>Represents a nullable <c>Double</c>.</summary>
  TNullableDouble = TNullable<Double>;

  ///	<summary>Represents a nullable <c>Boolean</c>.</summary>
  TNullableBoolean = TNullable<Boolean>;

  ///	<summary>Represents a nullable <c>TGuid</c>.</summary>
  TNullableGuid = TNullable<TGUID>;

  {$ENDREGION}


  {$REGION 'MulticastEvent'}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents a multicast delegate.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The event handler type.
  ///	</typeparam>
  ///	<remarks>
  ///	  As a multicast delegate, you can add or remove an event handler. The event
  ///	  handler must be an instance method. The calling conversion?should be
  ///	  default or stdcall.
  ///	</remarks>
  ///	<example>
  ///	  The following example demonstrates how to use it:
  ///	  <code lang="Delphi">
  ///	type
  ///	  TClipboardEventHandler = procedure (sender: TObject) of object;
  ///	  TClipboardEvent = TEvent&lt;TClipboardEventHandler&gt;;
  ///	  TClipboardWatcher = class
  ///	  private
  ///	    fOnChanged: TClipboardChangedEvent;
  ///	  public
  ///	    property OnChanged: TClipboardChangedEvent read fOnChanged;
  ///	  end;
  ///	  </code>
  ///	</example>
  {$ENDREGION}
  IDelegate<T> = interface
    {$REGION 'Property Getters'}
      function GetInvoke: T;
      function GetCount: Integer;
      function GetIsEmpty: Boolean;
      function GetIsNotEmpty: Boolean;
    {$ENDREGION}

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Adds an event handler to the list.
    ///	</summary>
    {$ENDREGION}
    procedure Add(const handler: T);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Remove an event handler if it exists.
    ///	</summary>
    {$ENDREGION}
    procedure Remove(const handler: T);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Clears all event handlers.
    ///	</summary>
    {$ENDREGION}
    procedure Clear;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Invokes the event.
    ///	</summary>
    {$ENDREGION}
    property Invoke: T read GetInvoke;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNotEmpty: Boolean read GetIsNotEmpty;
  end;


  PMethod = ^TMethod;

  TMethodInvocations = class
  private
    const
      paEAX = Word(0);
      paEDX = Word(1);
      paECX = Word(2);
      paStack = Word(3);

    type
      PParameterInfos = ^TParameterInfos;
      TParameterInfos = array[0..255] of ^PTypeInfo;

      PParameters = ^TParameters;
      TParameters = packed record
      public
        Registers: array[paEDX..paECX] of Cardinal;
        Stack: array[0..1023] of Byte;
      end;

      PMethodInfo = ^TMethodInfo;
      TMethodInfo = record
        TypeData: PTypeData;
        ParamInfos: PParameterInfos;
        StackSize: Integer;
        CallConversion: TCallConv;
        PStack: PParameters;
        constructor Create(typeInfo: PTypeInfo);
      end;
  private
    fMethodType: PTypeInfo;
    fMethodInfo: TMethodInfo;
    fMethods: TArray<TMethod>;
    fCount: Integer;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  protected
    procedure InternalInvokeHandlers;
    procedure InvokeEventHandlerStub;
  public
    constructor Create(methodTypeInfo: PTypeInfo);
    procedure Add(const method: TMethod);
    procedure Remove(const method: TMethod);
    function  IndexOf(const method: TMethod): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TDelegate<T> = class(TInterfacedObject, IDelegate<T>)
  private
    fInvocations: TMethodInvocations;
    fInvoke: T;
    function GetInvoke: T;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetIsNotEmpty: Boolean;
  protected
    procedure InvocationsNeeded; inline;
    property Invocations: TMethodInvocations read fInvocations;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure Clear;

    property Invoke: T read GetInvoke;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNotEmpty: Boolean read GetIsNotEmpty;
  end;

  TEvent<T> = record
  private
    fInstance: IDelegate<T>;
    function GetInvoke: T;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetIsNotEmpty: Boolean;
  public
    class function Create: IDelegate<T>; static;

    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure Clear;

    function GetInstance: IDelegate<T>; inline;

    property Invoke: T read GetInvoke;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNotEmpty: Boolean read GetIsNotEmpty;

    class operator Implicit(const event: IDelegate<T>): TEvent<T>;
    class operator Implicit(const event: TEvent<T>): IDelegate<T>;
    class operator Implicit(const eventHandler: T): TEvent<T>;
  end;

  {$ENDREGION}


  {$REGION 'Lifetime Type & Attributes'}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Lifetime Type Enumeration.
  ///	</summary>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="PooledAttribute" />
  {$ENDREGION}
  TLifetimeType = (
    ///	<summary>
    ///	  Unknown lifetime type.
    ///	</summary>
    ltUnknown,

    ///	<summary>
    ///	  Single instance.
    ///	</summary>
    ltSingleton,

    ///	<summary>
    ///	  Different instances.
    ///	</summary>
    ltTransient,

    ///	<summary>
    ///	  Every thread has a single instance.
    ///	</summary>
    ltSingletonPerThread,

    ///	<summary>
    ///	  Instances are transient except that they are recyclable.
    ///	</summary>
    ltPooled,

    ///	<summary>
    ///	  Customized lifetime type.
    ///	</summary>
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

  ///	<summary>
  ///	  Applies this attribute when a component shares the single instance.
  ///	</summary>
  ///	<remarks>
  ///	  When this attribute is applied to a component, the shared instance will
  ///	  be returned whenever get the implementation of a service.
  ///	</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	[Singleton]
  ///	TEmailSender = class(TInterfacedObject, IEmailSender)
  ///	//...
  ///	end;
  ///	  </code>
  ///	</example>
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="PooledAttribute" />
  ///	<seealso cref="TLifetimeType" />
  SingletonAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Represents that a new instance of the component will be created when
  ///	  requested.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="note">
  ///	    This attribute is the default option.
  ///	  </note>
  ///	</remarks>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="PooledAttribute" />
  ///	<seealso cref="TLifetimeType" />
  TransientAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Applies this attribute when a component shares the single instance per
  ///	  thread.
  ///	</summary>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="PooledAttribute" />
  ///	<seealso cref="TLifetimeType" />
  SingletonPerThreadAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Represents that the target component can be pooled.
  ///	</summary>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="TLifetimeType" />
  PooledAttribute = class(LifetimeAttributeBase)
  private
    fMinPoolsize: Integer;
    fMaxPoolsize: Integer;
  public
    constructor Create(minPoolSize, maxPoolSize: Integer);
    property MinPoolsize: Integer read fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize;
  end;

  ///	<summary>
  ///	  Applies the <c>InjectionAttribute</c> to injectable instance members of
  ///	  a class. e.g. constructors, methods, properties and even fields. Also
  ///	  works on parameters of a method.
  ///	</summary>
  ///	<seealso cref="ImplementsAttribute" />
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

  ///	<summary>
  ///	  Applies this attribute to tell the IoC container which service is
  ///	  implemented by the target component. In addition, a service name can be
  ///	  specified.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="note">
  ///	    This attribute can be specified more than once.
  ///	  </note>
  ///	</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	[Implements(TypeInfo(IEmailSender))]
  ///	TRegularEmailSender = class(TInterfacedObject, IEmailSender)
  ///	end;
  ///	[Implements(TypeInfo(IEmailSender), 'mock-email-sender')]
  ///	TMockEmailSender = class(TInterfacedObject, IEmailSender)
  ///	end;
  ///	  </code>
  ///	</example>
  ///	<seealso cref="InjectionAttribute" />
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
  ///	<summary>
  ///	  Lifecycle interface. If the component implements this interface, all
  ///	  resources will be deallocate by calling the <c>Dispose</c> method.
  ///	</summary>
  ///	<seealso cref="IInitializable" />
  ///	<seealso cref="IStartable" />
  ///	<seealso cref="IRecyclable" />
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

  EInsufficientMemoryException = EOutOfMemory;

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
  const parameterName: string);
begin
  ASSERT(Assigned(checkclazz));
  ASSERT(Assigned(clazz));

  if (not checkclazz.InheritsFrom(clazz)) then
    raise EArgumentException.CreateResFmt(@SBadObjectInheritance, [parameterName, checkclazz.ClassName, clazz.ClassName]);
end;

class procedure TArgument.CheckInheritsFrom(obj: TObject; const clazz: TClass;
  const parameterName: string);
begin
  if Assigned(obj) then
    CheckInheritsFrom(obj.ClassType, clazz, parameterName);
end;

class procedure TArgument.CheckNotNull(condition: Boolean;
  const parameterName: string);
begin
  if not condition then
  begin
    TArgument.RaiseArgumentNullException(parameterName);
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
  if IsNullReference(value, TypeInfo(T)) then
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

//class function TArgument.IsNullReference<T>(const value: T): Boolean;
//var
//  localTypeInfo: PTypeInfo;
//begin
//  localTypeInfo := TypeInfo(T);
//  Result := TArgument.IsNullReference(value, localTypeInfo);
//end;

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
  fHasValue := CHasValueFlag;
end;

constructor TNullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    fValue := v.AsType<T>;
    fHasValue := CHasValueFlag;
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


{$REGION 'TMethodInfo'}

function AdditionalInfoOf(TypeData: PTypeData): Pointer;
var
  P: PByte;
  I: Integer;
begin
  P := @TypeData^.ParamList;
  // Skip parameter names and types
  for I := 1 to TypeData^.ParamCount do
  begin
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1 );
  end;
  if TypeData^.MethodKind = mkFunction then
    // Skip return type name and info
    Inc(P, P[0] + 1 + 4);
  Result := P;
end;

//  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
//    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
//    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString,
//    tkClassRef, tkPointer, tkProcedure);

function GetTypeSize(typeInfo: PTypeInfo): Integer;
var
  typeData: PTypeData;
const
  COrdinalSizes: array[TOrdType] of Integer = (1, 1, 2, 2, 4, 4);
  CFloatSizes: array[TFloatType] of Integer = (4, 8, SizeOf(Extended), 8, 8);
  CSetSizes: array[TOrdType] of Integer = (1, 1, 2, 2, 4, 4);
begin
  case typeInfo^.Kind of
    tkChar:
      Result := 1;
    tkWChar:
      Result := 2;
    tkInteger, tkEnumeration:
      begin
        typeData := GetTypeData(typeInfo);
        Result := COrdinalSizes[typeData.OrdType];
      end;
    tkFloat:
      begin
        typeData := GetTypeData(typeInfo);
        Result := CFloatSizes[typeData^.FloatType];
      end;
    tkString, tkLString, tkUString, tkWString, tkInterface, tkClass, tkClassRef, tkDynArray:
      Result := SizeOf(Pointer);
    tkMethod, tkInt64:
      Result := 8;
    tkVariant:
      Result := 16;
    tkSet:
      begin
        // big sets have no typeInfo for now
        typeData := GetTypeData(typeInfo);
        Result := CSetSizes[typeData^.OrdType];
      end;
    tkRecord:
      begin
        typeData := GetTypeData(typeInfo);
        Result := typeData.RecSize;
      end;
    tkArray:
      begin
        typeData := GetTypeData(typeInfo);
        Result := typeData.ArrayData.Size;
      end;
    else
      begin
        Assert(False, 'Unsupported type');
        Result := -1;
      end;
  end;
end;

constructor TMethodInvocations.TMethodInfo.Create(typeInfo: PTypeInfo);
var
  typeData: PTypeData;
  P: PByte;
  curReg: Integer;
  I: Integer;
  Size: Integer;
begin
  typeData := GetTypeData(typeInfo);
  Self.TypeData := typeData;
  P := AdditionalInfoOf(typeData);
  CallConversion := TCallConv(PByte(p)^);
  ParamInfos := PParameterInfos(Cardinal(P) + 1);

  if CallConversion = ccReg then
  begin
    curReg := paEDX;
    StackSize := 0;
  end
  else begin
    curReg := paStack;
    StackSize := SizeOf(Pointer); // Self in stack
  end;

  P := @typeData^.ParamList;

  for I := 0 to typeData^.ParamCount - 1 do
  begin
    if TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] <> [] then
      Size := 4
    else
      Size := GetTypeSize(ParamInfos^[I]^);
    if (Size <= 4) and (curReg <= paECX) then
      Inc(curReg)
    else
      Inc(StackSize, Size);
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1);
  end;
end;

{$ENDREGION}


{$REGION 'TMethodInvocations'}

constructor TMethodInvocations.Create(methodTypeInfo: PTypeInfo);
begin
  inherited Create;
  fMethodType := methodTypeInfo;
  fMethodInfo := TMethodInfo.Create(fMethodType);
end;

procedure TMethodInvocations.Add(const method: TMethod);
begin
  if Length(fMethods) = fCount then
  begin
    if fCount = 0 then
      SetLength(fMethods, 1)
    else
      SetLength(fMethods, fCount * 2);
  end;
  fMethods[fCount].Code := PMethod(@method)^.Code;
  fMethods[fCount].Data := PMethod(@method)^.Data;
  Inc(fCount);
end;

procedure TMethodInvocations.Remove(const method: TMethod);
var
  index: Integer;
begin
  index := IndexOf(method);
  if index > -1 then
  begin
    Move(fMethods[index+1], fMethods[index], (fCount - index - 1) * SizeOf(TMethod));
    Dec(fCount);
    fMethods[fCount].Data := nil;
    fMethods[fCount].Code := nil;
  end;
end;

procedure TMethodInvocations.Clear;
begin
  SetLength(fMethods, 0);
  fCount := 0;
end;

function TMethodInvocations.IndexOf(const method: TMethod): Integer;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
  begin
    if (fMethods[i].Code = method.Code) and (fMethods[i].Data = method.Data) then
    begin
      Exit(i);
    end;
  end;
  Result := -1;
end;

function TMethodInvocations.GetCount: Integer;
begin
  Result := fCount;
end;

function TMethodInvocations.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TMethodInvocations.InternalInvokeHandlers;
var
  methods: TArray<TMethod>;
  method: TMethod;
  stackSize: Integer;
  callConversion: TCallConv;
  pStack: PParameters;
  i: Integer;
begin
  methods := fMethods;
  pStack := fMethodInfo.PStack;
  stackSize := fMethodInfo.stackSize;
  callConversion := fMethodInfo.CallConversion;
  for i := 0 to fCount - 1 do
  begin
    method.Data := methods[i].Data;
    method.Code := methods[i].Code;
    // Check to see if there is anything on the stack.
    if StackSize > 0 then
    asm
      // if there are items on the stack, allocate the space there and
      // move that data over.
      MOV ECX,StackSize
      SUB ESP,ECX
      MOV EDX,ESP
      MOV EAX, pStack
      LEA EAX,[EAX].TParameters.Stack[8]
      CALL System.Move
    end;
    asm
      // Now we need to load up the registers. EDX and ECX may have some data
      // so load them on up.
      MOV EAX,pStack
      MOV EDX,[EAX].TParameters.Registers.DWORD[0]
      MOV ECX,[EAX].TParameters.Registers.DWORD[4]
      // EAX is always "Self" and it changes on a per method pointer instance, so
      // grab it out of the method data.
      MOV EAX, method.Data
      CMP callConversion, ccReg
      JZ @BeginCall
      Mov [ESP], EAX // eax -> Self, put it into internal stack
    @BeginCall:
      // Now we call the method. This depends on the fact that the called method
      // will clean up the stack if we did any manipulations above.
      CALL method.Code
    end;
  end;
end;

procedure TMethodInvocations.InvokeEventHandlerStub;
const
  PtrSize = SizeOf(Pointer);
asm
        // is register conversion call ?
        CMP     BYTE PTR Self.fMethodInfo.CallConversion, ccReg
        JZ      @Begin
        Mov     EAX, [esp + 4]
@Begin:
        PUSH    EAX
        PUSH    ECX
        PUSH    EDX
        MOV     Self.fMethodInfo.TMethodInfo.PStack,ESP
        CALL    InternalInvokeHandlers
        // Pop EDX and ECX off the stack while preserving all registers.
        MOV     [ESP+4],EAX
        POP     EAX
        POP     EAX
        POP     ECX		// Self
        Mov     EAX, ECX
        MOV     ECX,[ECX].fMethodInfo.StackSize
        TEST    ECX,ECX
        JZ      @@SimpleRet
        // Jump to the actual return instruction since it is most likely not just a RET
        //JMP     ECX    // Data Exec. Prevention: Jumping into a GetMem allocated memory block

        // stack address alignment
        // In cdecl call conversion, the caller will clear the stack
        CMP     DWORD PTR [EAX].fMethodInfo.CallConversion, ccCdecl
        JZ      @@SimpleRet
        ADD     ECX, PtrSize - 1
        AND     ECX, NOT (PtrSize - 1)
        AND     ECX, $FFFF

        // clean up the stack
        PUSH    EAX                         // we need this register, so save it
        MOV     EAX,[ESP + 4]               // Load the return address
        MOV     [ESP + ECX + 4], EAX        // Just blast it over the first param on the stack
        POP     EAX
        ADD     ESP,ECX                     // This will move the stack back to where the moved
                                            // return address is now located. The next RET
                                            // instruction will do the final stack cleanup
@@SimpleRet:
end;

{$ENDREGION}


{$REGION 'TDelegate<T>'}

constructor TDelegate<T>.Create;
var
  p: PTypeInfo;
begin
  p := TypeInfo(T);
  if p = nil then
    raise EInvalidOperation.CreateRes(@SNoTypeInfo);
  if p.Kind <> tkMethod then
    raise EInvalidOperation.CreateRes(@STypeParameterShouldBeMethod);

  inherited Create;
end;

destructor TDelegate<T>.Destroy;
begin
  fInvocations.Free;
  inherited Destroy;
end;

procedure TDelegate<T>.Add(const handler: T);
begin
  InvocationsNeeded;
  fInvocations.Add(PMethod(@handler)^);
end;

procedure TDelegate<T>.Remove(const handler: T);
begin
  InvocationsNeeded;
  fInvocations.Remove(PMethod(@handler)^);
end;

procedure TDelegate<T>.Clear;
begin
  if fInvocations <> nil then
    fInvocations.Clear;
end;

function TDelegate<T>.GetCount: Integer;
begin
  if fInvocations <> nil then
    Result := fInvocations.Count
  else
    Result := 0;
end;

function TDelegate<T>.GetInvoke: T;
begin
  InvocationsNeeded;
  Result := fInvoke;
end;

function TDelegate<T>.GetIsEmpty: Boolean;
begin
  Result := (fInvocations = nil) or (fInvocations.Count = 0);
end;

function TDelegate<T>.GetIsNotEmpty: Boolean;
begin
  Result := not IsEmpty;
end;

procedure TDelegate<T>.InvocationsNeeded;
begin
  if fInvocations = nil then
  begin
    fInvocations := TMethodInvocations.Create(TypeInfo(T));
    PMethod(@fInvoke)^.Data := fInvocations;
    PMethod(@fInvoke)^.Code := @TMethodInvocations.InvokeEventHandlerStub;
  end;
end;

{$ENDREGION}


{$REGION 'TEvent<T>'}

class function TEvent<T>.Create: IDelegate<T>;
begin
  Result := TDelegate<T>.Create;
end;

procedure TEvent<T>.Add(const handler: T);
begin
  GetInstance.Add(handler);
end;

procedure TEvent<T>.Remove(const handler: T);
begin
  GetInstance.Remove(handler);
end;

procedure TEvent<T>.Clear;
begin
  if fInstance <> nil then
    fInstance.Clear;
end;

function TEvent<T>.GetCount: Integer;
begin
  if fInstance <> nil then
    Exit(fInstance.Count)
  else
    Exit(0);
end;

function TEvent<T>.GetInstance: IDelegate<T>;
begin
  if fInstance = nil then
  begin
    fInstance := TDelegate<T>.Create;
  end;
  Result := fInstance;
end;

function TEvent<T>.GetInvoke: T;
begin
  Result := GetInstance.Invoke;
end;

function TEvent<T>.GetIsEmpty: Boolean;
begin
  Result := (fInstance = nil) or fInstance.IsEmpty;
end;

function TEvent<T>.GetIsNotEmpty: Boolean;
begin
  Result := (fInstance <> nil) and fInstance.IsNotEmpty;
end;

class operator TEvent<T>.Implicit(const eventHandler: T): TEvent<T>;
begin
  Result.Clear;
  Result.Add(eventHandler);
end;

class operator TEvent<T>.Implicit(const event: IDelegate<T>): TEvent<T>;
begin
  Result.fInstance := event;
end;

class operator TEvent<T>.Implicit(const event: TEvent<T>): IDelegate<T>;
begin
  Result := event.GetInstance;
end;

{$ENDREGION}

end.
