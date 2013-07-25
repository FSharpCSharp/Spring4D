{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
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

///	<summary>
///	  Declares the fundamental interfaces for the
///	  <see href="http://spring4d.org">Spring4D</see> Framework.
///	</summary>
unit Spring;

{$I Spring.inc}

interface

uses
  Classes,
  Diagnostics,
  Generics.Collections,
  Generics.Defaults,
  Rtti,
  SyncObjs,
  SysUtils,
  TimeSpan,
  Types,
  TypInfo,
  Variants;

type

  {$REGION 'Type redefinitions'}

  ///	<summary>
  ///	  Represents a dynamic array of Byte.
  ///	</summary>
  TBytes = SysUtils.TBytes;

  ///	<summary>
  ///	  Represents a dynamic array of string.
  ///	</summary>
  TStringDynArray = Types.TStringDynArray;

  ///	<summary>
  ///	  Represents a time interval.
  ///	</summary>
  TTimeSpan = TimeSpan.TTimeSpan;

  ///	<summary>
  ///	  Provides a set of methods and properties to accurately measure elapsed
  ///	  time.
  ///	</summary>
  TStopwatch = Diagnostics.TStopwatch;

  PTypeInfo = TypInfo.PTypeInfo;

  TValue = Rtti.TValue;

  ///	<summary>
  ///	  Represents the class type of <see cref="System|TCustomAttribute" />.
  ///	</summary>
  TAttributeClass = class of TCustomAttribute;

{$IFNDEF DELPHIXE_UP}
  TThreadID = LongWord;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'Procedure types'}

  ///	<summary>
  ///	  Represents a logical predicate.
  ///	</summary>
  ///	<param name="value">
  ///	  the value needs to be determined.
  ///	</param>
  ///	<returns>
  ///	  Returns <c>True</c> if the value was accepted, otherwise, returns
  ///	  <c>False</c>.
  ///	</returns>
  ///	<remarks>
  ///	  <note type="tip">
  ///	    This type redefined the
  ///	    <see cref="SysUtils|TPredicate`1">SysUtils.TPredicate&lt;T&gt;</see> 
  ///	    type with a const parameter.
  ///	  </note>
  ///	</remarks>
  ///	<seealso cref="Spring.DesignPatterns|ISpecification&lt;T&gt;" />
  TPredicate<T> = reference to function(const value: T): Boolean;

  ///	<summary>
  ///	  Represents an anonymous method that has a single parameter and does not
  ///	  return a value.
  ///	</summary>
  ///	<seealso cref="TActionProc&lt;T&gt;" />
  ///	<seealso cref="TActionMethod&lt;T&gt;" />
  TAction<T> = reference to procedure(const obj: T);

  ///	<summary>
  ///	  Represents a procedure that has a single parameter and does not return
  ///	  a value.
  ///	</summary>
  ///	<seealso cref="TAction&lt;T&gt;" />
  ///	<seealso cref="TActionMethod&lt;T&gt;" />
  TActionProc<T> = procedure(const obj: T);

  ///	<summary>
  ///	  Represents a instance method that has a single parameter and does not
  ///	  return a value.
  ///	</summary>
  ///	<seealso cref="TAction&lt;T&gt;" />
  ///	<seealso cref="TActionProc&lt;T&gt;" />
  TActionMethod<T> = procedure(const obj: T) of object;

  {$ENDREGION}


  {$REGION 'TInterfaceBase'}

  ///	<summary>
  ///	  Provides a non-reference-counted <see cref="System|IInterface" /> 
  ///	  implementation.
  ///	</summary>
  TInterfaceBase = class abstract(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {$ENDREGION}


  {$REGION 'TArgument'}

  ///	<summary>
  ///	  Provides static methods to check arguments and raise argument
  ///	  exceptions.
  ///	</summary>
  ///	<remarks>
  ///	  It's recommended that all arguments of public types and members should
  ///	  be checked.
  ///	</remarks>
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

    ///	<summary>
    ///	  Raises an <see cref="EArgumentException" /> exception.
    ///	</summary>
    ///	<param name="msg">
    ///	  The general error message.
    ///	</param>
    class procedure RaiseArgumentException(const msg: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EFormatException" /> exception.
    ///	</summary>
    class procedure RaiseArgumentFormatException(const argumentName: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EArgumentNullException" /> exception.
    ///	</summary>
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EArgumentOutOfRangeException" /> exception.
    ///	</summary>
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EInvalidEnumArgumentException" /> exception.
    ///	</summary>
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static; inline;
  end;

  {$ENDREGION}


  {$REGION 'Nullable Types'}

  ///	<summary>
  ///	  A nullable type can represent the normal range of values for its
  ///	  underlying value type, plus an additional <c>Null</c> value.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The underlying value type of the <see cref="Nullable&lt;T&gt;" /> 
  ///	  generic type.
  ///	</typeparam>
  Nullable<T> = record
  private
    const CHasValueFlag = '@';  // DO NOT LOCALIZE
  strict private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;
  private
    ///	<summary>
    ///	  Internal use. Marks the current instance as null.
    ///	</summary>
    ///	<remarks>
    ///	  The <see cref="Nullable&lt;T&gt;" /> type is immutable so that this
    ///	  method must be private.
    ///	</remarks>
    procedure Clear;

    ///	<summary>
    ///	  Determines whether a variant value is null or empty.
    ///	</summary>
    class function VarIsNullOrEmpty(const value: Variant): Boolean; static;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="Nullable&lt;T&gt;" /> 
    ///	  structure to the specified value.
    ///	</summary>
    constructor Create(const value: T); overload;

    ///	<summary>
    ///	  Initializes a new instance of the <see cref="Nullable&lt;T&gt;" /> 
    ///	  structure to the specified value.
    ///	</summary>
    constructor Create(const value: Variant); overload;

    ///	<summary>
    ///	  Retrieves the value of the current <see cref="Nullable&lt;T&gt;" /> 
    ///	  object, or the object's default value.
    ///	</summary>
    function GetValueOrDefault: T; overload;

    ///	<summary>
    ///	  Retrieves the value of the current <see cref="Nullable&lt;T&gt;" /> 
    ///	  object, or the specified default value.
    ///	</summary>
    ///	<param name="defaultValue">
    ///	  A value to return if the <see cref="HasValue" /> property is
    ///	  <c>False</c>.
    ///	</param>
    ///	<returns>
    ///	  The value of the <see cref="Value" /> property if the
    ///	  <see cref="HasValue" /> property is true; otherwise, the
    ///	  <paramref name="defaultValue" /> parameter.
    ///	</returns>
    ///	<remarks>
    ///	  The <see cref="GetValueOrDefault" /> method returns a value even if
    ///	  the <see cref="HasValue" /> property is false (unlike the
    ///	  <see cref="Value" /> property, which throws an exception).
    ///	</remarks>
    function GetValueOrDefault(const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Determines whether two nullable value are equal.
    ///	</summary>
    ///	<remarks>
    ///	  <p> If both two nullable values are null, return true; </p>
    ///	  <p> If either one is null, return false; </p>
    ///	  <p> else compares their values as usual. </p>
    ///	</remarks>
    function Equals(const other: Nullable<T>): Boolean;

    ///	<summary>
    ///	  Gets a value indicating whether the current
    ///	  <see cref="Nullable&lt;T&gt;" /> structure has a value.
    ///	</summary>
    property HasValue: Boolean read GetHasValue;

    ///	<summary>
    ///	  Gets the value of the current <see cref="Nullable&lt;T&gt;" /> value.
    ///	</summary>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	  Raised if the value is null.
    ///	</exception>
    property Value: T read GetValue;

    { Operator Overloads }
    class operator Implicit(const value: Nullable<T>): T;
    class operator Implicit(const value: T): Nullable<T>;
    class operator Implicit(const value: Nullable<T>): Variant;
    class operator Implicit(const value: Variant): Nullable<T>;
    class operator Implicit(value: Pointer): Nullable<T>;
    class operator Explicit(const value: Nullable<T>): T;
    class operator Equal(const a, b: Nullable<T>) : Boolean;
    class operator NotEqual(const a, b: Nullable<T>) : Boolean;
  end;


  ///	<summary>
  ///	  Represents a nullable unicode string.
  ///	</summary>
  TNullableString = Nullable<string>;

  ///	<summary>
  ///	  Represents a nullable ansi string.
  ///	</summary>
  TNullableAnsiString = Nullable<AnsiString>;

  ///	<summary>
  ///	  Represents a nullable wide string.
  ///	</summary>
  TNullableWideString = Nullable<WideString>;

  ///	<summary>
  ///	  Represents a nullable integer.
  ///	</summary>
  TNullableInteger = Nullable<Integer>;

  ///	<summary>
  ///	  Represents a nullable <c>Int64</c>.
  ///	</summary>
  TNullableInt64 = Nullable<Int64>;

  ///	<summary>
  ///	  Represents a nullable native integer.
  ///	</summary>
  TNullableNativeInt = Nullable<NativeInt>;

  ///	<summary>
  ///	  Represents a nullable <c>TDateTime</c>.
  ///	</summary>
  TNullableDateTime = Nullable<TDateTime>;

  ///	<summary>
  ///	  Represents a nullable <c>Currency</c>.
  ///	</summary>
  TNullableCurrency = Nullable<Currency>;

  ///	<summary>
  ///	  Represents a nullable <c>Double</c>.
  ///	</summary>
  TNullableDouble = Nullable<Double>;

  ///	<summary>
  ///	  Represents a nullable <c>Boolean</c>.
  ///	</summary>
  TNullableBoolean = Nullable<Boolean>;

  ///	<summary>
  ///	  Represents a nullable <c>TGuid</c>.
  ///	</summary>
  TNullableGuid = Nullable<TGUID>;

  {$ENDREGION}


  {$REGION 'Lazy Initialization'}

  ///	<summary>
  ///	  Provides support for lazy initialization.
  ///	</summary>
  ILazy = interface
    ['{40223BA9-0C66-49E7-AA33-BDAEF9F506D6}']
    function GetIsValueCreated: Boolean;
    function GetValue: TValue;

    ///	<summary>
    ///	  Gets a value that indicates whether a value has been created for this
    ///	  <c>ILazy</c> instance.
    ///	</summary>
    property IsValueCreated: Boolean read GetIsValueCreated;

    ///	<summary>
    ///	  Gets the lazily initialized value of the current <c>ILazy</c>
    ///	  instance.
    ///	</summary>
    property Value: TValue read GetValue;
  end;

  ///	<summary>
  ///	  Provides support for lazy initialization by generic.
  ///	</summary>
  ILazy<T> = interface(ILazy)
    function GetValue: T;
    property Value: T read GetValue;
  end;

  TLazy = class(TInterfacedObject, ILazy)
  protected
    fIsValueCreated: Boolean;
    function GetIsValueCreated: Boolean;
    function NonGenericGetValue: TValue; virtual; abstract;
    function ILazy.GetValue = NonGenericGetValue;
  end;

  TLazy<T> = class(TLazy, ILazy<T>, TFunc<T>)
  private
    fValueFactory: TFunc<T>;
    fValue: T;
    procedure EnsureInitialized; inline;
  protected
    function GetValue: T;
    function NonGenericGetValue: TValue; override;
    function TFunc<T>.Invoke = GetValue;
  public
    ///	<summary>
    ///	  Initializes a new instance of <see cref="TLazy&lt;T&gt;" /> with a
    ///	  delegate.
    ///	</summary>
    ///	<param name="valueFactory">
    ///	  The delegate that is invoked to produce the lazily initialized value
    ///	  when it is needed.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  Raised if the <paramref name="valueFactory" /> is null.
    ///	</exception>
    constructor Create(const valueFactory: TFunc<T>);

    ///	<summary>
    ///	  Initializes a new instance of <see cref="TLazy&lt;T&gt;" /> with a
    ///	  specified value.
    ///	</summary>
    ///	<param name="value">
    ///	  The initialized value.
    ///	</param>
    constructor CreateFrom(const value: T);

    property IsValueCreated: Boolean read GetIsValueCreated;
    property Value: T read GetValue;
  end;

  Lazy<T> = record
  private
    fLazy: ILazy<T>;
    function GetIsAssigned: Boolean;
    function GetIsValueCreated: Boolean;
    function GetValue: T;
  public
    constructor Create(const valueFactory: TFunc<T>);
    constructor CreateFrom(const value: T);

    class operator Implicit(const value: Lazy<T>): ILazy<T>;
    class operator Implicit(const value: Lazy<T>): T;
    class operator Implicit(const value: T): Lazy<T>;
    class operator Implicit(const value: TLazy<T>): Lazy<T>;

    property IsAssigned: Boolean read GetIsAssigned;
    property IsValueCreated: Boolean read GetIsValueCreated;
    property Value: T read GetValue;
  end;

  TLazyInitializer = record
  public
    class function InterlockedCompareExchange(var target: Pointer; value, comparand: Pointer): Pointer; static;

    class function EnsureInitialized<T>(var target: T; const factoryMethod: TFunc<T>): T; overload; static;
    class function EnsureInitialized<T: class, constructor>(var target: T): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Multicast Event'}

  IEvent = interface
    ['{CFC14C4D-F559-4A46-A5B1-3145E9B182D8}']
  {$REGION 'Property Accessors'}
    function GetInvoke: TMethod;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetIsEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
  {$ENDREGION}

    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    procedure ForEach(const action: TAction<TMethod>);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property IsEmpty: Boolean read GetIsEmpty;
    property Invoke: TMethod read GetInvoke;
  end;

  ///	<summary>
  ///	  Represents a multicast event.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The event handler type must be an instance procedural type such as
  ///	  TNotifyEvent.
  ///	</typeparam>
  IEvent<T> = interface(IEvent)
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetIsEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
  {$ENDREGION}

    ///	<summary>
    ///	  Adds an event handler to the list.
    ///	</summary>
    procedure Add(handler: T);

    ///	<summary>
    ///	  Removes an event handler if it was added to the event.
    ///	</summary>
    procedure Remove(handler: T);

    ///	<summary>
    ///	  Removes all event handlers which were registered by an instance.
    ///	</summary>
    procedure RemoveAll(instance: Pointer);

    ///	<summary>
    ///	  Clears all event handlers.
    ///	</summary>
    procedure Clear;

    ///	<summary>
    ///	  Iterates all event handlers and perform the specified action on each
    ///	  one.
    ///	</summary>
    procedure ForEach(const action: TAction<T>);

    ///	<summary>
    ///	  Invokes all event handlers.
    ///	</summary>
    property Invoke: T read GetInvoke;

    ///	<summary>
    ///	  Gets the number of all event handlers.
    ///	</summary>
    property Count: Integer read GetCount;

    ///	<summary>
    ///	  Gets the value indicates whether the multicast event is enabled, or
    ///	  sets the value to enable or disable the event.
    ///	</summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;

    ///	<summary>
    ///	  Gets a value indicates whether there is not any event handler.
    ///	</summary>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  PMethod = ^TMethod;

  ///	<summary>
  ///	  Internal Use.
  ///	</summary>
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
{$IFNDEF CPUX64}
        Registers: array[paEDX..paECX] of Cardinal;
        EAXRegister: Cardinal;
        ReturnAddress: Pointer;
{$ENDIF}
        Stack: array[0..1023] of Byte;
      end;

      PMethodInfo = ^TMethodInfo;
      TMethodInfo = record
        TypeData: PTypeData;
        ParamInfos: PParameterInfos;
        StackSize: Integer;
        CallConvention: TCallConv;
{$IFDEF CPUX64}
        RegisterFlag: Word;
{$ENDIF CPUX64}
        constructor Create(typeInfo: PTypeInfo);
      end;
  private
    fMethodType: PTypeInfo;
    fMethodInfo: TMethodInfo;
    fMethods: TList<TMethod>;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  protected
    procedure InternalInvokeHandlers(Params: PParameters);
    procedure InvokeEventHandlerStub;
  public
    constructor Create(methodTypeInfo: PTypeInfo);
    destructor Destroy; override;
    procedure Add(const method: TMethod);
    procedure Remove(const method: TMethod);
    procedure RemoveAll(instance: Pointer);
    function  IndexOf(const method: TMethod): Integer;
    procedure Clear;
    procedure ForEach(const action: TAction<TMethod>);
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TEvent = class(TInterfacedObject, IEvent)
  private
    
    fInvocations: TMethodInvocations;
    fInvoke: TMethod;
    fEnabled: Boolean;
    fTypeInfo: PTypeInfo;
    function GetInvoke: TMethod;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    
    function GetIsEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
  protected
    procedure InvocationsNeeded; inline;
    property Invocations: TMethodInvocations read fInvocations;
  public
    constructor Create(typeInfo: PTypeInfo);
    destructor Destroy; override;

    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod); overload;
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    procedure ForEach(const action: TAction<TMethod>);

    property Invoke: TMethod read GetInvoke;
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TEvent<T> = class(TEvent, IEvent<T>)
  private
    function GetInvoke: T;
  public
    constructor Create;

    procedure Add(handler: T); overload;
    procedure Remove(handler: T); overload;
    procedure ForEach(const action: TAction<T>);

    property Invoke: T read GetInvoke;
  end;

  Event<T> = record
  private
    fInstance: IEvent<T>;
    function GetInvoke: T;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  public
    class function Create: Event<T>; static;

    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;

    function EnsureInitialized: Event<T>; inline;
    function GetInstance: IEvent<T>; inline; deprecated 'Use EnsureInitialized instead.';

    property Invoke: T read GetInvoke;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;

    class operator Implicit(const e: IEvent<T>): Event<T>;
    class operator Implicit(var e: Event<T>): IEvent<T>;
    class operator Implicit(var e: Event<T>): T;
    class operator Implicit(const eventHandler: T): Event<T>;
  end;

  IMulticastNotifyEvent = IEvent<TNotifyEvent>;

  TMulticastNotifyEvent = TEvent<TNotifyEvent>;

  {$ENDREGION}


  {$REGION 'Exceptions'}

  ENotSupportedException    = SysUtils.ENotSupportedException;

{$IFDEF DELPHIXE_UP}
  ENotImplementedException  = SysUtils.ENotImplemented;
{$ELSE}
  ENotImplementedException  = class(Exception);
{$ENDIF}

{$IFDEF DELPHIXE_UP}
  EInvalidOperationException  = SysUtils.EInvalidOpException;
{$ELSE}
  EInvalidOperationException  = class(Exception);
{$ENDIF}

  EInvalidCastException     = SysUtils.EInvalidCast;

  EInsufficientMemoryException = EOutOfMemory;

  EFormatException          = class(Exception);
  EIndexOutOfRangeException = class(Exception);

  EArgumentException            = SysUtils.EArgumentException;
  EArgumentOutOfRangeException  = SysUtils.EArgumentOutOfRangeException;
{$IFDEF DELPHIXE_UP}
  EArgumentNilException        = SysUtils.EArgumentNilException;
{$ELSE}
  EArgumentNilException        = class(EArgumentException);
{$ENDIF}
  EArgumentNullException        = EArgumentNilException;
  EInvalidEnumArgumentException = class(EArgumentException);

  ERttiException = class(Exception);

  {$ENDREGION}


  {$REGION 'Routines'}

procedure PlatformNotImplemented;

///	<summary>
///	  Raises an <see cref="Spring|EArgumentNullException" /> if the
///	  <paramref name="value" /> is nil.
///	</summary>
procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string); overload;

///	<summary>
///	  Raises an <see cref="Spring|EArgumentNullException" /> if the
///	  <paramref name="value" /> is nil.
///	</summary>
procedure CheckArgumentNotNull(value: Pointer; const argumentName: string); overload;

  
function InheritsFrom(sourceType, targetType: PTypeInfo): Boolean;

{$ENDREGION}


implementation

uses
  Spring.ResourceStrings;


{$REGION 'Routines'}

procedure PlatformNotImplemented;
begin
  raise ENotImplementedException.Create('Not implemented in present platform.');
end;

procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string);
begin
  CheckArgumentNotNull(Pointer(value), argumentName);
end;

procedure CheckArgumentNotNull(value: Pointer; const argumentName: string);
begin
  if not Assigned(value) then
  begin
    TArgument.RaiseArgumentNullException(argumentName);
  end;
end;

function InheritsFrom(sourceType, targetType: PTypeInfo): Boolean;
var
  sourceData, targetData: PTypeData;
begin
  Result := sourceType = targetType;

  if (sourceType.Kind = tkClass) and (targetType.Kind = tkClass) then
  begin
    sourceData := GetTypeData(sourceType);
    targetData := GetTypeData(targetType);
    Result := sourceData.ClassType.InheritsFrom(targetData.ClassType);
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
  if (index < indexBase) or (index > length + indexBase - 1) then
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
    (startIndex >= indexBase) and (startIndex < indexBase + length),
    StartIndexArgName);
  TArgument.CheckRange(count >= 0, CountArgName);
  if count > 0 then
  begin
    TArgument.CheckRange(count <= indexBase + length - startIndex, CountArgName);
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
  Assert(Assigned(checkclazz));
  Assert(Assigned(clazz));

  if not checkclazz.InheritsFrom(clazz) then
  begin
    raise EArgumentException.CreateResFmt(@SBadObjectInheritance, [parameterName,
      checkclazz.ClassName, clazz.ClassName]);
  end;
end;

class procedure TArgument.CheckInheritsFrom(obj: TObject; const clazz: TClass;
  const parameterName: string);
begin
  if Assigned(obj) then
  begin
    CheckInheritsFrom(obj.ClassType, clazz, parameterName);
  end;
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
  TArgument.CheckNotNull(Assigned(p), argumentName);
end;

class procedure TArgument.CheckNotNull(const intf: IInterface;
  const argumentName: string);
begin
  TArgument.CheckNotNull(Assigned(intf), argumentName);
end;

class procedure TArgument.CheckNotNull(obj: TObject;
  const argumentName: string);
begin
  TArgument.CheckNotNull(Assigned(obj), argumentName);
end;

class procedure TArgument.CheckNotNull<T>(const value: T;
  const argumentName: string);
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
  intValue := 0;
  Move(value, intValue, SizeOf(T));
  TArgument.CheckEnum<T>(intValue, argumentName);
end;

class procedure TArgument.CheckEnum<T>(const value: Integer;
  const argumentName: string);
var
  typeInfo: PTypeInfo;
  data: PTypeData;
  msg: string;
begin
  typeInfo := System.TypeInfo(T);
  TArgument.CheckTypeKind(typeInfo, [tkEnumeration], 'T');

  data := GetTypeData(typeInfo);
  Assert(Assigned(data), 'data must not be nil.');

  if (value < data.MinValue) or (value > data.MaxValue) then
  begin
    msg := Format(SInvalidEnumArgument, [argumentName,
      GetTypeName(typeInfo), value]);
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
    raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument, [
      typeInfo.Name, argumentName]);
  end;
end;

class procedure TArgument.CheckTypeKind(typeInfo: PTypeInfo;
  const expectedTypeKinds: TTypeKinds; const argumentName: string);
begin
  TArgument.CheckNotNull(typeInfo, argumentName);
  if not (typeInfo.Kind in expectedTypeKinds) then
  begin
    raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument, [
      typeInfo.Name, argumentName]);
  end;
end;

class function TArgument.IsNullReference(const value; typeInfo: PTypeInfo): Boolean;
const
  ReferenceKinds = [
    tkClass, tkMethod, tkInterface, tkClassRef, tkPointer, tkProcedure];
begin
  Result := False;
  if Assigned(typeInfo) and (typeInfo.Kind in ReferenceKinds) then
  begin
    if typeInfo.Kind = tkMethod then
      Result := not Assigned(TMethod(value).Code) and not Assigned(TMethod(value).Data)
    else
      Result := not Assigned(PPointer(@value)^);
  end;
end;

class procedure TArgument.RaiseArgumentException(const msg: string);
begin
  raise EArgumentException.Create(msg);
end;

class procedure TArgument.RaiseArgumentNullException(
  const argumentName: string);
begin
  raise EArgumentNullException.CreateResFmt(
    @SArgumentNullException, [argumentName]);
end;

class procedure TArgument.RaiseArgumentOutOfRangeException(
  const argumentName: string);
begin
  raise EArgumentOutOfRangeException.CreateResFmt(
    @SArgumentOutOfRangeException, [argumentName]);
end;

class procedure TArgument.RaiseArgumentFormatException(const argumentName: string);
begin
  raise EFormatException.CreateResFmt(@SInvalidArgumentFormat, [argumentName]);
end;

class procedure TArgument.RaiseInvalidEnumArgumentException(
  const argumentName: string);
begin
  raise EInvalidEnumArgumentException.CreateResFmt(
    @SInvalidEnumArgument, [argumentName]);
end;

{$ENDREGION}


{$REGION 'Nullable<T>'}

constructor Nullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := CHasValueFlag;
end;

constructor Nullable<T>.Create(const value: Variant);
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

procedure Nullable<T>.Clear;
begin
  fHasValue := '';
  fValue := Default(T);
end;

class function Nullable<T>.VarIsNullOrEmpty(const value: Variant): Boolean;
begin
  Result := VarIsNull(value) or VarIsEmpty(value);
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := Length(fHasValue) > 0;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
  begin
    raise EInvalidOperationException.CreateRes(@SNullableTypeHasNoValue);
  end;
  Result := fValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := Value
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := Value
  else
    Result := defaultValue;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if HasValue and other.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Value, other.Value)
  else
    Result := HasValue = other.HasValue;
end;

class operator Nullable<T>.Implicit(const value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(value);
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): Variant;
var
  v: TValue;
begin
  if value.HasValue then
  begin
    v := TValue.From<T>(value.Value);
    if v.IsType<Boolean> then
      Result := v.AsBoolean
    else
      Result := v.AsVariant;
  end
  else
  begin
    Result := Null;
  end;
end;

class operator Nullable<T>.Implicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result := Nullable<T>.Create(v.AsType<T>);
  end
  else
  begin
    Result.Clear;
  end;
end;

class operator Nullable<T>.Implicit(value: Pointer): Nullable<T>;
begin
  if not Assigned(value) then
  begin
    Result.Clear;
  end
  else
  begin
    raise EInvalidOperationException.CreateRes(@SCannotAssignPointerToNullable);
  end;
end;

class operator Nullable<T>.Explicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Equal(const a, b: Nullable<T>): Boolean;
begin
  Result := a.Equals(b);
end;

class operator Nullable<T>.NotEqual(const a, b: Nullable<T>): Boolean;
begin
  Result := not a.Equals(b);
end;

{$ENDREGION}


{$REGION 'TLazy'}

function TLazy.GetIsValueCreated: Boolean;
begin
  Result := fIsValueCreated;
end;

{$ENDREGION}


{$REGION 'TLazy<T>'}

constructor TLazy<T>.Create(const valueFactory: TFunc<T>);
begin
  CheckArgumentNotNull(Pointer(@valueFactory), 'valueFactory');

  inherited Create;
  fValueFactory := valueFactory;
  fIsValueCreated := False;
end;

constructor TLazy<T>.CreateFrom(const value: T);
begin
  inherited Create;
  fValue := value;
  fIsValueCreated := True;
end;

procedure TLazy<T>.EnsureInitialized;
begin
  if fIsValueCreated then
    Exit;

  fValue := fValueFactory();
  fIsValueCreated := True;
end;

function TLazy<T>.GetValue: T;
begin
  EnsureInitialized;
  Result := fValue;
end;

function TLazy<T>.NonGenericGetValue: TValue;
begin
  Result := TValue.From<T>(Value);
end;

{$ENDREGION}


{$REGION 'Lazy<T>'}

constructor Lazy<T>.Create(const valueFactory: TFunc<T>);
begin
  fLazy := TLazy<T>.Create(valueFactory);
end;

constructor Lazy<T>.CreateFrom(const value: T);
begin
  fLazy := TLazy<T>.CreateFrom(value);
end;

function Lazy<T>.GetValue: T;
begin
  if not Assigned(fLazy) then
    raise EInvalidOperationException.CreateRes(@SNoDelegateAssigned);
  Result := fLazy.Value;
end;

function Lazy<T>.GetIsValueCreated: Boolean;
begin
  Result := Assigned(fLazy) and fLazy.IsValueCreated;
end;

function Lazy<T>.GetIsAssigned: Boolean;
begin
  Result := Assigned(fLazy);
end;

class operator Lazy<T>.Implicit(const value: Lazy<T>): ILazy<T>;
begin
  Result := value.fLazy;
end;

class operator Lazy<T>.Implicit(const value: Lazy<T>): T;
begin
  Result := value.Value;
end;

class operator Lazy<T>.Implicit(const value: T): Lazy<T>;
begin
  Result.fLazy := TLazy<T>.CreateFrom(value);
end;

class operator Lazy<T>.Implicit(const value: TLazy<T>): Lazy<T>;
begin
  Result.fLazy := value;
end;

{$ENDREGION}


{$REGION 'TLazyInitializer'}

class function TLazyInitializer.InterlockedCompareExchange(var target: Pointer;
  value, comparand: Pointer): Pointer;
{$IFNDEF DELPHIXE_UP}
asm
  XCHG EAX, EDX
  XCHG EAX, ECX
  LOCK CMPXCHG [EDX], ECX
end;
{$ELSE}
begin
  Result := TInterlocked.CompareExchange(target, value, comparand);
end;
{$ENDIF}

class function TLazyInitializer.EnsureInitialized<T>(var target: T;
  const factoryMethod: TFunc<T>): T;
var
  localValue: T;
begin
  if PPointer(@target)^ = nil then
  begin
    localValue := factoryMethod();
    if TLazyInitializer.InterlockedCompareExchange(PPointer(@target)^,
      PPointer(@localValue)^, nil) <> nil then
    begin
      if PTypeInfo(TypeInfo(T)).Kind = tkClass then
      begin
        PObject(@localValue)^.Free;
      end;
    end
    else if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
    begin
      PPointer(@localValue)^ := nil;
    end;
  end;
  Result := target;
end;

class function TLazyInitializer.EnsureInitialized<T>(var target: T): T;
var
  localValue: T;
begin
  if PPointer(@target)^ = nil then
  begin
    localValue := T.Create;
    if TLazyInitializer.InterlockedCompareExchange(PPointer(@target)^,
      PPointer(@localValue)^, nil) <> nil then
      localValue.Free;
  end;
  Result := target;
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
    // TODO: Validate tkString
    tkString, tkLString, tkUString, tkWString, tkInterface, tkClass, tkClassRef, tkDynArray:
      Result := SizeOf(Pointer);
    tkMethod:
      Result := SizeOf(TMethod);
    tkInt64:
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

  function PassByRef(P: PByte; ParamInfos: PParameterInfos; I: Integer): Boolean;
  begin
    Result := (TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] <> [])
      and not (ParamInfos^[I]^.Kind in [tkFloat, tkMethod, tkInt64]);
  end;

  function Align4(Value: Integer): Integer;
  begin
    Result := (Value + 3) and not 3;
  end;

var
  typeData: PTypeData;
  P: PByte;
  I: Integer;
{$IFNDEF CPUX64}
  curReg: Integer;
  Size: Integer;
{$ENDIF}
begin
  typeData := GetTypeData(typeInfo);
  Self.TypeData := typeData;
  P := AdditionalInfoOf(typeData);
  CallConvention := TCallConv(PByte(p)^);
  ParamInfos := PParameterInfos(Cardinal(P) + 1);

{$IFNDEF CPUX64}
  curReg := paStack;
  if CallConvention = ccReg then
  begin
    curReg := paEDX;
    StackSize := 0;
  end else
{$ENDIF}
  begin
    StackSize := SizeOf(Pointer); // Self in stack
  end;

  P := @typeData^.ParamList;

  for I := 0 to typeData^.ParamCount - 1 do
  begin
{$IFNDEF CPUX64}
    if PassByRef(P, ParamInfos, I) then
      Size := 4
    else
      Size := GetTypeSize(ParamInfos^[I]^);
    if (Size <= 4) and (curReg <= paECX) and (ParamInfos^[I]^.Kind <> tkFloat) then
      Inc(curReg)
    else
    begin
      if Size < 4 then
        Size := 4;
      Inc(StackSize, Align4(Size));
    end;
{$ELSE}
    if I < 3 then
    begin
      if ParamInfos^[I]^.Kind = tkFloat then
        RegisterFlag := RegisterFlag or (1 shl (I + 1));
    end;
    Inc(StackSize, 8);
{$ENDIF}
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1);
  end;

{$IFDEF CPUX64}
  if StackSize < 32 then
    StackSize := 32;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TMethodInvocations'}

procedure InvokeMethod(const Method: TMethod;
  Parameters: Pointer; StackSize: Integer);
const
  PointerSize = SizeOf(Pointer);
const
  paEDX = Word(1);
  paECX = Word(2);
type
  TParameters = packed record
  public
{$IFNDEF CPUX64}
    Registers: array[paEDX..paECX] of Cardinal;
    EAXRegister: Cardinal;
    ReturnAddress: Pointer;
{$ENDIF}
    Stack: array[0..1023] of Byte;
  end;
{$IFNDEF CPUX64}
asm
  push ebp
  mov ebp,esp
  push eax // ebp-4 = Method
  push ebx
  mov ebx, edx // ebx = Parameters

  // if StackSize > 0
  test ecx,ecx
  jz @@no_stack

  // stack address alignment
  add ecx,PointerSize-1
  and ecx,not(PointerSize-1)
  and ecx,$ffff
  sub esp,ecx

  // put stack address as second parameter
  mov edx,esp

  // put params on stack as first parameter
  lea eax,[ebx].TParameters.Stack

  call Move

@@no_stack:
  mov edx,[ebx].TParameters.Registers.dword[0]
  mov ecx,[ebx].TParameters.Registers.dword[4]
  mov ebx,[ebp-$04]
  mov eax,[ebx].TMethod.Data
  call [ebx].TMethod.Code

  pop ebx
  pop eax
  mov esp,ebp
  pop ebp
end;
{$ELSE}
asm
  .params 60
  mov [rbp+$200],Method
  mov [rbp+$208],Parameters
  test r8,r8
  jz @@no_stack

  // put params on stack as first parameter
  lea rcx,[Parameters].TParameters.Stack

  // put stack address as second parameter
  mov rdx,rsp

  call Move

  mov rdx,[rbp+$208]

@@no_stack:
  mov rcx,[rdx].TParameters.Stack.qword[0]
  mov r8,[rdx].TParameters.Stack.qword[16]
  mov r9,[rdx].TParameters.Stack.qword[24]

  movsd xmm0,[rdx].TParameters.Stack.qword[0]
  movsd xmm1,[rdx].TParameters.Stack.qword[8]
  movsd xmm2,[rdx].TParameters.Stack.qword[16]
  movsd xmm3,[rdx].TParameters.Stack.qword[24]

  mov rdx,[rdx].TParameters.Stack.qword[8]

  mov rax,[rbp+$200]
  lea rax,[rax]
  mov rcx,[rax].TMethod.Data
  call [rax].TMethod.Code
end;
{$ENDIF}

constructor TMethodInvocations.Create(methodTypeInfo: PTypeInfo);
begin
  inherited Create;
  fMethodType := methodTypeInfo;
  fMethodInfo := TMethodInfo.Create(fMethodType);
  fMethods := TList<TMethod>.Create;
end;

destructor TMethodInvocations.Destroy;
begin
  fMethods.Free;
  inherited;
end;

procedure TMethodInvocations.ForEach(const action: TAction<TMethod>);
var
  method: TMethod;
begin
  for method in fMethods do
  begin
    action(method);
  end;
end;

procedure TMethodInvocations.Add(const method: TMethod);
begin
  fMethods.Add(method);
end;

procedure TMethodInvocations.Remove(const method: TMethod);
begin
  fMethods.Remove(method);
end;

procedure TMethodInvocations.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if fMethods[i].Data = instance then
      fMethods.Delete(i);
  end;
end;

procedure TMethodInvocations.Clear;
begin
  fMethods.Clear;
end;

function TMethodInvocations.IndexOf(const method: TMethod): Integer;
begin
  Result := fMethods.IndexOf(method);
end;

function TMethodInvocations.GetCount: Integer;
begin
  Result := fMethods.Count;
end;

function TMethodInvocations.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TMethodInvocations.InternalInvokeHandlers(Params: PParameters);
var
  i: Integer;
begin
  for i := 0 to fMethods.Count - 1 do
    InvokeMethod(fMethods[i], Params, fMethodInfo.StackSize);
end;

procedure TMethodInvocations.InvokeEventHandlerStub;
{$IFNDEF CPUX64}
const
  PtrSize = SizeOf(Pointer);
asm
        // is register conversion call ?
        CMP     BYTE PTR Self.fMethodInfo.CallConvention, ccReg
        JZ      @Begin
        Mov     EAX, [esp + 4]
@Begin:
        PUSH    EAX
        PUSH    ECX
        PUSH    EDX
        MOV     EDX,ESP
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
        CMP     DWORD PTR [EAX].fMethodInfo.CallConvention, ccCdecl
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
{$ELSE}
asm
        MOV     AX, WORD PTR [RCX].TMethodInvocations.fMethodInfo.RegisterFlag
@@FIRST:
        TEST    AX, $01
        JZ      @@SAVE_RCX
@@SAVE_XMM0:
        MOVSD   QWORD PTR [RSP+$08], XMM0
        JMP     @@SECOND
@@SAVE_RCX:
        MOV     QWORD PTR [RSP+$08], RCX

@@SECOND:
        TEST    AX, $02
        JZ      @@SAVE_RDX
@@SAVE_XMM1:
        MOVSD   QWORD PTR [RSP+$10], XMM1
        JMP     @@THIRD
@@SAVE_RDX:
        MOV     QWORD PTR [RSP+$10], RDX

@@THIRD:
        TEST    AX, $04
        JZ      @@SAVE_R8
@@SAVE_XMM2:
        MOVSD   QWORD PTR [RSP+$18], XMM2
        JMP     @@FORTH
@@SAVE_R8:
        MOV     QWORD PTR [RSP+$18], R8

@@FORTH:
        TEST    AX, $08
        JZ      @@SAVE_R9
@@SAVE_XMM3:
        MOVSD   QWORD PTR [RSP+$20], XMM3
        JMP     @@1
@@SAVE_R9:
        MOV     QWORD PTR [RSP+$20], R9

@@1:    LEA     RDX, QWORD PTR [RSP+$08]
        MOV     RAX, RCX
        SUB     RSP, $28
        CALL    InternalInvokeHandlers
        ADD     RSP, $28
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TEvent'}

constructor TEvent.Create(typeInfo: PTypeInfo);
begin
  if not Assigned(typeInfo) then
    raise EInvalidOperationException.CreateRes(@SNoTypeInfo);
  if typeInfo.Kind <> tkMethod then
    raise EInvalidOperationException.CreateRes(@STypeParameterShouldBeMethod);
  inherited Create;
  fEnabled := True;
  fTypeInfo := typeInfo;
end;

destructor TEvent.Destroy;
begin
  fInvocations.Free;
  inherited Destroy;
end;

procedure TEvent.Add(const handler: TMethod);
begin
  InvocationsNeeded;
  fInvocations.Add(handler);
end;

procedure TEvent.Clear;
begin
  if Assigned(fInvocations) then
    fInvocations.Clear;
end;

procedure TEvent.ForEach(const action: TAction<TMethod>);
begin
  InvocationsNeeded;
  fInvocations.ForEach(action);
end;

function TEvent.GetCount: Integer;
begin
  if Assigned(fInvocations) then
    Result := fInvocations.Count
  else
    Result := 0;
end;

function TEvent.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TEvent.GetInvoke: TMethod;
begin
  InvocationsNeeded;
  Result := fInvoke;
end;

function TEvent.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TEvent.InvocationsNeeded;
begin
  if not Assigned(fInvocations) then
  begin
    fInvocations := TMethodInvocations.Create(fTypeInfo);
    PMethod(@fInvoke)^.Data := fInvocations;
    PMethod(@fInvoke)^.Code := @TMethodInvocations.InvokeEventHandlerStub;
  end;
end;

procedure TEvent.Remove(const handler: TMethod);
begin
  InvocationsNeeded;
  fInvocations.Remove(PMethod(@handler)^);
end;

procedure TEvent.RemoveAll(instance: Pointer);
begin
  InvocationsNeeded;
  fInvocations.RemoveAll(instance);
end;

procedure TEvent.SetEnabled(const value: Boolean);
begin
  fEnabled := value;
end;

{$ENDREGION}


{$REGION 'TEvent<T>'}

constructor TEvent<T>.Create;
begin
  inherited Create(TypeInfo(T));
end;

procedure TEvent<T>.ForEach(const action: TAction<T>);
begin
  inherited ForEach(TAction<TMethod>(action));
end;

procedure TEvent<T>.Add(handler: T);
begin
  inherited Add(PMethod(@handler)^);
end;

procedure TEvent<T>.Remove(handler: T);
begin
  inherited Remove(PMethod(@handler)^);
end;

function TEvent<T>.GetInvoke: T;
begin
  PMethod(@Result)^ := inherited Invoke;
end;

{$ENDREGION}


{$REGION 'Event<T>'}

class function Event<T>.Create: Event<T>;
begin
  Result := TEvent<T>.Create;
end;

function Event<T>.EnsureInitialized: Event<T>;
begin
  if not Assigned(fInstance) then
  begin
    fInstance := TEvent<T>.Create;
  end;
  Result.fInstance := fInstance;
end;

procedure Event<T>.Add(const handler: T);
begin
  EnsureInitialized;
  fInstance.Add(handler);
end;

procedure Event<T>.Remove(const handler: T);
begin
  EnsureInitialized;
  fInstance.Remove(handler);
end;

procedure Event<T>.RemoveAll(instance: Pointer);
begin
  if Assigned(fInstance) then
    fInstance.RemoveAll(instance);
end;

procedure Event<T>.Clear;
begin
  if Assigned(fInstance) then
    fInstance.Clear;
end;

function Event<T>.GetCount: Integer;
begin
  if Assigned(fInstance) then
    Exit(fInstance.Count)
  else
    Exit(0);
end;

function Event<T>.GetInstance: IEvent<T>;
begin
  EnsureInitialized;
  Result := fInstance;
end;

function Event<T>.GetInvoke: T;
begin
  EnsureInitialized;
  Result := fInstance.Invoke;
end;

function Event<T>.GetIsEmpty: Boolean;
begin
  Result := not Assigned(fInstance) or fInstance.IsEmpty;
end;

class operator Event<T>.Implicit(const eventHandler: T): Event<T>;
begin
  Result.Clear;
  Result.Add(eventHandler);
end;

class operator Event<T>.Implicit(const e: IEvent<T>): Event<T>;
begin
  Result.fInstance := e;
end;

class operator Event<T>.Implicit(var e: Event<T>): IEvent<T>;
begin
  e.EnsureInitialized;
  Result := e.fInstance;
end;

class operator Event<T>.Implicit(var e: Event<T>): T;
begin
  Result := e.GetInvoke;
end;

{$ENDREGION}


end.
