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

///	<summary>
///	  Declares the fundamental interfaces for the <see href="http://spring4d.com">Spring4D</see> Framework.
///	</summary>
unit Spring;

{$I Spring.inc}

interface

uses
  Classes,
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

  ///	<summary>
  ///	  Represents the class type of <see cref="System|TCustomAttribute" />.
  ///	</summary>
  TAttributeClass = class of TCustomAttribute;

{$IFNDEF DELPHIXE_UP}
  TThreadID = LongWord;
{$ENDIF}

  ///	<summary>
  ///	  Represents a logical predicate.
  ///	</summary>
  ///	<param name="value">
  ///	  the value needs to be determined.
  ///	</param>
  ///	<returns>
  ///	  Returns True if the value was accepted, otherwise, returns false.
  ///	</returns>
  ///	<remarks>
  ///	  <note type="tip">
  ///	    This type redefined the
  ///	    <see cref="SysUtils|TPredicate`1">SysUtils.TPredicate&lt;T&gt;</see>
  ///	    type with a const parameter.
  ///	  </note>
  ///	</remarks>
  ///	<seealso cref="Spring.DesignPatterns|ISpecification{T}" />
  TPredicate<T> = reference to function(const value: T): Boolean;

  ///	<summary>
  ///	  Represents an anonymous method that has a single parameter and does not
  ///	  return a value.
  ///	</summary>
  /// <seealso cref="TActionProc{T}" />
  /// <seealso cref="TActionMethod{T}" />
  TAction<T> = reference to procedure(const obj: T);

  ///	<summary>
  ///	  Represents a procedure that has a single parameter and does not return
  ///	  a value.
  ///	</summary>
  /// <seealso cref="TAction{T}" />
  /// <seealso cref="TActionMethod{T}" />
  TActionProc<T> = procedure(const obj: T);

  ///	<summary>
  ///	  Represents a instance method that has a single parameter and does not
  ///	  return a value.
  ///	</summary>
  /// <seealso cref="TAction{T}" />
  /// <seealso cref="TActionProc{T}" />
  TActionMethod<T> = procedure(const obj: T) of object;

  ///	<summary>
  ///	  Provides a non-reference-counted <see cref="System|IInterface" />
  ///	  implementation.
  ///	</summary>
  TInterfaceBase = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  ///	<summary>
  ///	  Provides an abstract class base of TThread that implements the
  ///	  IInterface.
  ///	</summary>
  TInterfacedThread = class(TThread, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  ///	<summary>
  ///	  Provides static methods to check arguments and raise argument
  ///	  exceptions.
  ///	</summary>
  ///	<remarks>
  ///	  It's recommended that all arguments of public types and members should be checked.
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
    ///	  Raises an EArgumentException exception.
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


  {$REGION 'TNullable<T> & Aliases'}

  ///	<summary>
  ///	  Represents an "object" whose underlying type is a value type that can
  ///	  also be assigned nil like a reference type.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The underlying value type of the <see cref="TNullable`1" /> generic
  ///	  type.
  ///	</typeparam>
  ///	<remarks>
  ///	  The <typeparamref name="T" /> must be a value type such as string, Integer.
  ///	</remarks>
  TNullable<T> = packed record
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
    procedure Clear;

    ///	<summary>
    ///	  Determines whether a variant value is null or empty.
    ///	</summary>
    class function VarIsNullOrEmpty(const value: Variant; trimWhiteSpace: Boolean = False): Boolean; static;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <c>TNullable{T}</c> structure to
    ///	  the specified value.
    ///	</summary>
    constructor Create(const value: T); overload;

    ///	<summary>
    ///	  Initializes a new instance of the <c>TNullable{T}</c> structure to
    ///	  the specified value.
    ///	</summary>
    constructor Create(const value: Variant); overload;

    ///	<summary>
    ///	  Retrieves the value of the current <c>TNullable{T}</c> object, or the
    ///	  object's default value.
    ///	</summary>
    function GetValueOrDefault: T; overload;

    ///	<summary>
    ///	  Retrieves the value of the current <c>TNullable{T}</c> object, or the
    ///	  specified default value.
    ///	</summary>
    ///	<param name="defaultValue">
    ///	  A value to return if the <see cref="HasValue" /> property is
    ///	  <c>false</c>.
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
    ///	  Gets a value indicating whether the current <c>TNullable{T}</c>
    ///	  structure has a value.
    ///	</summary>
    property HasValue: Boolean read GetHasValue;

    ///	<summary>
    ///	  Gets the value of the current <c>TNullable&lt;T&gt;</c> value.
    ///	</summary>
    ///	<exception cref="Spring|EInvalidOperation">
    ///	  Raised if the value is null.
    ///	</exception>
    property Value: T read GetValue;

    { Operator Overloads }
    class operator Implicit(const value: TNullable<T>): T;
    class operator Implicit(const value: T): TNullable<T>;
    class operator Implicit(const value: TNullable<T>): Variant;
    class operator Implicit(const value: Variant): TNullable<T>;
    class operator Implicit(value: Pointer): TNullable<T>;
    class operator Explicit(const value: TNullable<T>): T;
  end;


  ///	<summary>
  ///	  Represents a nullable unicode string.
  ///	</summary>
  TNullableString = TNullable<string>;

  ///	<summary>
  ///	  Represents a nullable ansi string.
  ///	</summary>
  TNullableAnsiString = TNullable<AnsiString>;

  ///	<summary>
  ///	  Represents a nullable wide string.
  ///	</summary>
  TNullableWideString = TNullable<WideString>;

  ///	<summary>
  ///	  Represents a nullable integer.
  ///	</summary>
  TNullableInteger = TNullable<Integer>;

  ///	<summary>
  ///	  Represents a nullable <c>Int64</c>.
  ///	</summary>
  TNullableInt64 = TNullable<Int64>;

  ///	<summary>Represents a nullable native integer.</summary>
  TNullableNativeInt = TNullable<NativeInt>;

  ///	<summary>
  ///	  Represents a nullable <c>TDateTime</c>.
  ///	</summary>
  TNullableDateTime = TNullable<TDateTime>;

  ///	<summary>
  ///	  Represents a nullable <c>Currency</c>.
  ///	</summary>
  TNullableCurrency = TNullable<Currency>;

  ///	<summary>
  ///	  Represents a nullable <c>Double</c>.
  ///	</summary>
  TNullableDouble = TNullable<Double>;

  ///	<summary>
  ///	  Represents a nullable <c>Boolean</c>.
  ///	</summary>
  TNullableBoolean = TNullable<Boolean>;

  ///	<summary>
  ///	  Represents a nullable <c>TGuid</c>.
  ///	</summary>
  TNullableGuid = TNullable<TGUID>;

  {$ENDREGION}


  {$REGION 'MulticastEvent (NOT READY)'}

  ///	<summary>
  ///	  Represents a multicast event.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The event handler type must be an instance method such as TNotifyEvent.
  ///	</typeparam>
  IMulticastEvent<T> = interface
    {$REGION 'Property Accessors'}
      function GetInvoke: T;
      function GetCount: Integer;
      function GetEnabled: Boolean;
      function GetIsEmpty: Boolean;
      function GetIsNotEmpty: Boolean;
      procedure SetEnabled(const value: Boolean);
    {$ENDREGION}

    ///	<summary>
    ///	  Adds an event handler to the list.
    ///	</summary>
    procedure Add(const handler: T);

    ///	<summary>
    ///	  Removes an event handler if it was added to the event.
    ///	</summary>
    procedure Remove(const handler: T); overload;

    ///	<summary>
    ///	  Removes all event handlers which were registered by an instance.
    ///	</summary>
    procedure Remove(instance: Pointer); overload;

    ///	<summary>
    ///	  Clears all event handlers.
    ///	</summary>
    procedure Clear;

    ///	<summary>
    ///	  Invokes all event handlers.
    ///	</summary>
    property Invoke: T read GetInvoke;

    ///	<summary>
    ///	  Gets the number of all event handlers.
    ///	</summary>
    property Count: Integer read GetCount;

    /// <summary>
    ///   Gets the value indicates whether the multicast event is enabled, or sets the value to enable or disable the event.
    /// </summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;

    ///	<summary>
    ///	  Gets a value indicates whether there is not any event handler.
    ///	</summary>
    property IsEmpty: Boolean read GetIsEmpty;

    ///	<summary>
    ///	  Gets a value indicates whether there is any event handler.
    ///	</summary>
    property IsNotEmpty: Boolean read GetIsNotEmpty;
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
        Registers: array[paEDX..paECX] of Cardinal;
        Stack: array[0..1023] of Byte;
      end;

      PMethodInfo = ^TMethodInfo;
      TMethodInfo = record
        TypeData: PTypeData;
        ParamInfos: PParameterInfos;
        StackSize: Integer;
        CallConversion: TCallConv;
        Params: PParameters;
        constructor Create(typeInfo: PTypeInfo);
      end;
  private
    fMethodType: PTypeInfo;
    fMethodInfo: TMethodInfo;
    fMethods: TList<TMethod>;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  protected
    procedure InternalInvokeHandlers;
    procedure InvokeEventHandlerStub;
  public
    constructor Create(methodTypeInfo: PTypeInfo);
    destructor Destroy; override;
    procedure Add(const method: TMethod);
    procedure Remove(const method: TMethod);
    procedure RemoveAll(instance: Pointer);
    function  IndexOf(const method: TMethod): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  // TODO: Implement Enabled property & Fix some bugs
  TMulticastEvent<T> = class(TInterfacedObject, IMulticastEvent<T>)
  private
    fInvocations: TMethodInvocations;
    fInvoke: T;
    fEnabled: Boolean;
    function GetInvoke: T;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetIsEmpty: Boolean;
    function GetIsNotEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
  protected
    procedure InvocationsNeeded; inline;
    property Invocations: TMethodInvocations read fInvocations;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const handler: T);
    procedure Remove(const handler: T); overload;
    procedure Remove(instance: Pointer); overload;
    procedure Clear;

    property Invoke: T read GetInvoke;
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNotEmpty: Boolean read GetIsNotEmpty;
  end;

  TEvent<T> = record
  private
    fInstance: IMulticastEvent<T>;
    function GetInvoke: T;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetIsNotEmpty: Boolean;
  public
    class function Create: IMulticastEvent<T>; static;

    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure Clear;

    function GetInstance: IMulticastEvent<T>; inline;

    property Invoke: T read GetInvoke;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNotEmpty: Boolean read GetIsNotEmpty;

    class operator Implicit(const event: IMulticastEvent<T>): TEvent<T>;
    class operator Implicit(const event: TEvent<T>): IMulticastEvent<T>;
    class operator Implicit(const eventHandler: T): TEvent<T>;
  end;

  IMulticastNotifyEvent = IMulticastEvent<TNotifyEvent>;

  TMulticastNotifyEvent = TMulticastEvent<TNotifyEvent>;

{$WARNINGS OFF}
  IDelegate<T> = interface(IMulticastEvent<T>)
  end deprecated;

  TDelegate<T> = class(TMulticastEvent<T>, IDelegate<T>)
  end deprecated;
{$WARNINGS ON}

  {$ENDREGION}


  {$REGION 'Exceptions'}

  ENotSupportedException    = SysUtils.ENotSupportedException;

{$IFDEF DELPHIXE_UP}
  ENotImplementedException  = SysUtils.ENotImplemented;
{$ELSE}
  ENotImplementedException  = class(Exception);
{$ENDIF}

  EInvalidOperation         = SysUtils.EInvalidOp;
  EInvalidCastException     = SysUtils.EConvertError;

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

  EIOException                  = SysUtils.EInOutError;
  EFileNotFoundException        = SysUtils.EFileNotFoundException;
  EDirectoryNotFoundException   = SysUtils.EDirectoryNotFoundException;
  EDriveNotFoundException       = class(EIOException);

  ERttiException = class(Exception);

  {$ENDREGION}

procedure PlatformNotImplemented;

/// <summary>
/// Raises an <see cref="Spring|EArgumentNullException" /> if the <paramref name="value" /> is nil.
/// </summary>
procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string); overload;

/// <summary>
/// Raises an <see cref="Spring|EArgumentNullException" /> if the <paramref name="value" /> is nil.
/// </summary>
procedure CheckArgumentNotNull(value: Pointer; const argumentName: string); overload;

implementation

uses
  StrUtils,
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
  if value = nil then
  begin
    TArgument.RaiseArgumentNullException(argumentName);
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
  Assert(data <> nil, 'data must not be nil.');

  if (value < data.MinValue) or (value > data.MaxValue) then
  begin
    msg := Format(
      SInvalidEnumArgument,
      [argumentName, GetTypeName(typeInfo), value]
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

function TNullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := value
  else
    Result := defaultValue;
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
  fMethods := TList<TMethod>.Create;
end;

destructor TMethodInvocations.Destroy;
begin
  fMethods.Free;
  inherited;
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

procedure TMethodInvocations.InternalInvokeHandlers;
{$IFNDEF CPUX64}
var
  method: TMethod;
  stackSize: Integer;
  callConversion: TCallConv;
  pStack: PParameters;
  i: Integer;
begin
  pStack := fMethodInfo.Params;
  stackSize := fMethodInfo.stackSize;
  callConversion := fMethodInfo.CallConversion;
  for i := 0 to fMethods.Count - 1 do
  begin
    method := fMethods[i];
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
{$ELSE}
begin
  PlatformNotImplemented;
end;
{$ENDIF}

procedure TMethodInvocations.InvokeEventHandlerStub;
{$IFNDEF CPUX64}
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
        MOV     Self.fMethodInfo.TMethodInfo.Params,ESP
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
{$ELSE}
begin
  PlatformNotImplemented;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TMulticastEvent<T>'}

constructor TMulticastEvent<T>.Create;
var
  p: PTypeInfo;
begin
  p := TypeInfo(T);
  if p = nil then
    raise EInvalidOperation.CreateRes(@SNoTypeInfo);
  if p.Kind <> tkMethod then
    raise EInvalidOperation.CreateRes(@STypeParameterShouldBeMethod);
  inherited Create;
  fEnabled := True;
end;

destructor TMulticastEvent<T>.Destroy;
begin
  fInvocations.Free;
  inherited Destroy;
end;

procedure TMulticastEvent<T>.Add(const handler: T);
begin
  InvocationsNeeded;
  fInvocations.Add(PMethod(@handler)^);
end;

procedure TMulticastEvent<T>.Remove(const handler: T);
begin
  InvocationsNeeded;
  fInvocations.Remove(PMethod(@handler)^);
end;

procedure TMulticastEvent<T>.Remove(instance: Pointer);
begin
  InvocationsNeeded;
  fInvocations.RemoveAll(instance);
end;

procedure TMulticastEvent<T>.Clear;
begin
  if fInvocations <> nil then
    fInvocations.Clear;
end;

function TMulticastEvent<T>.GetCount: Integer;
begin
  if fInvocations <> nil then
    Result := fInvocations.Count
  else
    Result := 0;
end;

function TMulticastEvent<T>.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TMulticastEvent<T>.GetInvoke: T;
begin
  InvocationsNeeded;
  Result := fInvoke;
end;

function TMulticastEvent<T>.GetIsEmpty: Boolean;
begin
  Result := (fInvocations = nil) or (fInvocations.Count = 0);
end;

function TMulticastEvent<T>.GetIsNotEmpty: Boolean;
begin
  Result := not IsEmpty;
end;

procedure TMulticastEvent<T>.SetEnabled(const value: Boolean);
begin
  fEnabled := value;
end;

procedure TMulticastEvent<T>.InvocationsNeeded;
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

class function TEvent<T>.Create: IMulticastEvent<T>;
begin
  Result := TMulticastEvent<T>.Create;
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

function TEvent<T>.GetInstance: IMulticastEvent<T>;
begin
  if fInstance = nil then
  begin
    fInstance := TMulticastEvent<T>.Create;
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

class operator TEvent<T>.Implicit(const event: IMulticastEvent<T>): TEvent<T>;
begin
  Result.fInstance := event;
end;

class operator TEvent<T>.Implicit(const event: TEvent<T>): IMulticastEvent<T>;
begin
  Result := event.GetInstance;
end;

{$ENDREGION}

end.
