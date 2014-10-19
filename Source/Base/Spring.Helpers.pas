{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

{$I Spring.inc}

/// <summary>
///   Provides many easy to use class helpers &amp; record helpers that extend
///   some common classes in the RTL.
/// </summary>
/// <remarks>
///   <para>
///     Classes helpers and record helpers have been introduced since Delphi
///     2007. The initial purpose is to allow developers to extend a class
///     without change the original structure.
///   </para>
///   <note type="note">
///     A class helper type may not declare instance data, but class fields
///     are allowed.
///   </note>
///   <note type="warning">
///     Class helpers and record helpers are not intended to be a design tool
///     in Delphi. It is some kind of "patching" technology.
///   </note>
///   <para>
///     If you want to use these helpers, just uses the <b>Spring.Helpers</b>
///     namespace in the target unit.
///   </para>
/// </remarks>
/// <example>
///   See examples in the <see cref="TGuidHelper" />.
/// </example>
unit Spring.Helpers;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes,
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Reflection;

type
  /// <summary>
  ///   Represents a record helper for the <c>System.TGuid</c> structure to
  ///   make it easy to use.
  /// </summary>
  /// <remarks>
  ///   <note type="tip">
  ///     You can use the equal ("=") or not equal ("&lt;&gt;") operator
  ///     loading in latest Delphi XE.
  ///   </note>
  /// </remarks>
  /// <example>
  ///   The following code demonstrates how to create a new guid and use it in
  ///   OO-style:
  ///   <code lang="Delphi">procedure TestGuidHelper;
  /// var
  ///   guid: TGuid;
  /// begin
  ///   // generates a new guid.
  ///   guid := TGuid.NewGuid;
  ///   // this guid must not be empty.
  ///   Assert(not guid.IsEmpty);
  ///   // print the string representation
  ///   Writeln(guid.ToString);
  ///   // print the quoted string
  ///   Writeln(guid.ToQuotedString);
  ///   // This guid must equal to itself.
  ///   Assert(guid.Equals(guid));
  /// end;</code>
  /// </example>
  TGuidHelper = record helper for TGuid
  private
    class function GetEmpty: TGuid; static;
    function GetIsEmpty: Boolean;
  public
    ///	<summary>
    ///	  Creates a guid structure from the specified guid string.
    ///	</summary>
    ///	<param name="guidString">
    ///	  the guid string.
    ///	</param>
    class function Create(const guidString: string): TGuid; overload; static;
    class function Create(const bytes: TBytes): TGuid; overload; static;
    class function Create(a: Integer; b: SmallInt; c: SmallInt; const d: TBytes): TGuid; overload; static;
    class function Create(a: Integer; b: SmallInt; c: SmallInt; d, e, f, g, h, i, j, k: Byte): TGuid; overload; static;
    class function Create(a: Cardinal; b: Word; c: Word; d, e, f, g, h, i, j, k: Byte): TGuid; overload; static;

    ///	<summary>
    ///	  Generates a new <c>TGuid</c> instance.
    ///	</summary>
    class function NewGuid: TGuid; static;

    function ToBytes: TBytes;

    function ToByteArray: TBytes;

    ///	<summary>
    ///	  Returns a string representation of the guid.
    ///	</summary>
    function ToString: string;

    ///	<summary>
    ///	  Determines whether the guid equals to another TGuid structure.
    ///	</summary>
    function Equals(const guid: TGuid): Boolean;

    ///	<summary>
    ///	  Returns the quoted string representation of the guid.
    ///	</summary>
    function ToQuotedString: string;

    ///	<summary>
    ///	  Gets a value which indicates whether the guid is empty (all zero).
    ///	</summary>
    property IsEmpty: Boolean read GetIsEmpty;

    /// <summary>
    ///   Gets the shared empty guid.
    /// </summary>
    /// <value>
    ///   The value of the empty guid is <c>
    ///   {00000000-0000-0000-0000-000000000000}</c>
    /// </value>
    class property Empty: TGuid read GetEmpty;
  end;

  ///	<summary>
  ///	  Provides a static method to create a TMethod structure with an instance
  ///	  and a methodaddress.
  ///	</summary>
  TMethodHelper = record helper for TMethod
  public
    class function Create(const instance, methodAddress: Pointer): TMethod; static;
  end;

  TStreamHelper = class helper for TStream
  public
    /// <summary>
    ///   Reads a value of a value type, which could be an Integer, record,
    ///   etc., from the stream.
    /// </summary>
    /// <remarks>
    ///   <note type="tip">
    ///     The generic argument could be omitted if the compiler can
    ///     automatically inreference the type.
    ///   </note>
    /// </remarks>
    /// <example>
    ///   <para>
    ///     The following example demonstrates how to use the generic <c>
    ///     ReadBuffer&lt;T&gt;</c> and <c>WriteBuffer&lt;T&gt;</c>methods.
    ///   </para>
    ///   <code lang="Delphi">procedure TestStreamHelper;
    /// var
    ///   stream: TStream;
    ///   value: Integer;
    /// begin
    ///   stream := TMemoryStream.Create;
    ///   try
    ///     value := 2;
    ///     stream.WriteBuffer(value);
    ///     stream.Position := 0;
    ///     stream.ReadBuffer&lt;Integer&gt;(value);
    ///   finally
    ///     stream.Free;
    ///   end;
    /// end;</code>
    /// </example>
    procedure ReadBuffer<T: record>(var value: T); overload;

    ///	<summary>
    ///	  Writes a value of a value type to the stream.
    ///	</summary>
    procedure WriteBuffer<T: record>(const value: T); overload;
  end;

  TStringsHelper = class helper for TStrings
  private
    function GetIsEmpty: Boolean;
  public
    ///	<summary>
    ///	  Add an array of string to the list.
    ///	</summary>
    procedure AddStrings(const strings: array of string); overload;

    ///	<summary>
    ///	  Adds or updates a name-value pair.
    ///	</summary>
    ///	<remarks>
    ///	  <note type="warning">
    ///	    There is a <c>Values[name: string]</c>property in the TStrings
    ///	    class, but the entry will be removed if the value is empty.
    ///	  </note>
    ///	</remarks>
    procedure AddOrUpdate(const name, value: string);

//    procedure Remove(const s: string);

    /// <summary>
    ///   Executes a procedure during batch updating of the list.
    /// </summary>
    /// <exception cref="Spring|EArgumentNullException">
    ///   Raised if the<paramref name="strings" /> is nil or the <paramref name="proc" />
    ///    is not assigned.
    /// </exception>
    procedure ExecuteUpdate(const proc: TProc);

    /// <summary>
    ///   Extract all name entries and add them to the <paramref name="strings" />
    ///    list.
    /// </summary>
    /// <exception cref="Spring|EArgumentNullException">
    ///   Raised if the <paramref name="strings" /> is nil.
    /// </exception>
    /// <seealso cref="ExtractValues(TStrings)">
    ///   ExtractValues
    /// </seealso>
    procedure ExtractNames(const strings: TStrings);

    /// <summary>
    ///   Extract all value entries and add them to the <paramref name="strings" />
    ///    list.
    /// </summary>
    /// <exception cref="Spring|EArgumentNullException">
    ///   Raised if the <paramref name="strings" /> is nil.
    /// </exception>
    /// <seealso cref="ExtractNames(TStrings)" />
    procedure ExtractValues(const strings: TStrings);

    ///	<summary>
    ///	  Returns a string array that contains all the <b>name</b>entries in
    ///	  the string list.
    ///	</summary>
    function GetNames: TStringDynArray;

    ///	<summary>
    ///	  Returns a string array that contains all the <b>value</b>entries in
    ///	  the string list.
    ///	</summary>
    function GetValues: TStringDynArray;

//    function GetValue(const name: string): string; overload;
//    function GetValue(const index: Integer): string; overload;

    ///	<summary>
    ///	  Gets the corresponding value of the name entry if there is such an
    ///	  entry and the value is not empty, otherwise, returns the default
    ///	  value specified by the <paramref name="default" />param.
    ///	</summary>
    function GetValueOrDefault<T>(const name: string; const default: T): T; experimental;

    ///	<summary>
    ///	  Try finding a name entry in the list.
    ///	</summary>
    function TryFindName(const name: string; var index: Integer): Boolean;

    ///	<summary>
    ///	  Try finding a value entry in the list.
    ///	</summary>
    function TryFindValue(const value: string; var index: Integer): Boolean;

    /// <summary>
    ///   Try finding an object in the list.
    /// </summary>
    function TryFindObject(const obj: TObject; var index: Integer): Boolean;

    ///	<summary>
    ///	  Determines whether the list contains the specified name entry.
    ///	</summary>
    function ContainsName(const name: string): Boolean;

    ///	<summary>
    ///	  Determines whether the list contains the specified value entry.
    ///	</summary>
    function ContainsValue(const value: string): Boolean;

    /// <summary>
    ///   Determines whether the list contains the specified object.
    /// </summary>
    function ContainsObject(const obj: TObject): Boolean;

    ///	<summary>
    ///	  Converts the string list to a dynamic string array.
    ///	</summary>
    function ToArray: TStringDynArray;

    ///	<summary>
    ///	  Gets a value indicates whether the strings is empty.
    ///	</summary>
    ///	<value>
    ///	  Returns true if the count of the list is zero, otherwise, returns
    ///	  false.
    ///	</value>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TCollectionHelper = class helper for TCollection
  public
    /// <param name="proc">
    ///   the anonymous method that will be executed within the batch update.
    /// </param>
    procedure ExecuteUpdate(const proc: TProc);
  end;

  // TPointHelper, TSizeHelper, TRectHelper

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetCustomAttributes(attributeClass: TAttributeClass): TArray<TCustomAttribute>; overload;

    ///	<summary>
    ///	  Gets an array which contains all custom attribute types which the
    ///	  type applies.
    ///	</summary>
    function GetCustomAttributes<T: TCustomAttribute>: TArray<T>; overload;

    function GetCustomAttribute(attributeClass: TAttributeClass): TCustomAttribute; overload;

    ///	<summary>
    ///	  Enumerates all applied custom attributes and returns the first one
    ///	  which is/inherits the specified type.
    ///	</summary>
    function GetCustomAttribute<T: TCustomAttribute>: T; overload;

    function TryGetCustomAttribute(attributeClass: TAttributeClass;
      out attribute: TCustomAttribute): Boolean; overload;

    ///	<summary>
    ///	  Try getting a custom attribute class which is applied by the type.
    ///	</summary>
    function TryGetCustomAttribute<T: TCustomAttribute>(out attribute: T): Boolean; overload;

    function HasCustomAttribute(attributeClass: TAttributeClass): Boolean; overload;

    ///	<summary>
    ///	  Determines whether the type applies the specified custom attribute
    ///	  class.
    ///	</summary>
    function HasCustomAttribute<T: TCustomAttribute>: Boolean; overload;
  end;

  TRttiClassType = TRttiInstanceType;

  ///	<preliminary />
  TRttiTypeHelper =  class helper for TRttiType
  private
    function GetAsInterface: TRttiInterfaceType;
    function GetIsClass: Boolean;
    function GetIsInterface: Boolean;
    function GetIsClassOrInterface: Boolean;
    function GetAsClass: TRttiInstanceType;
    function GetIsGenericType: Boolean;
    function GetIsLazyType: Boolean;
    function GetAsDynamicArray: TRttiDynamicArrayType;
    function GetIsDynamicArray: Boolean;
    function GetIsString: Boolean;
    function InternalGetConstructors(enumerateBaseType: Boolean = True): IEnumerable<TRttiMethod>;
    function InternalGetMethods(enumerateBaseType: Boolean = True): IEnumerable<TRttiMethod>;
    function InternalGetProperties(enumerateBaseType: Boolean = True): IEnumerable<TRttiProperty>;
    function InternalGetFields(enumerateBaseType: Boolean = True): IEnumerable<TRttiField>;
    function GetConstructors: IEnumerable<TRttiMethod>;
    function GetMethods: IEnumerable<TRttiMethod>;
    function GetProperties: IEnumerable<TRttiProperty>;
    function GetFields: IEnumerable<TRttiField>;
    function GetDefaultName: string;
    function GetAncestorCount: Integer;
  public
    // function GetMembers: IEnumerable<TRttiMember>;

    ///	<summary>
    ///	  Returns an enumerable collection which contains all the interface
    ///	  Rtti types that the target type implements.
    ///	  <note type="note">
    ///	    Only Guid interfaces will be enumerated.
    ///	  </note>
    ///	</summary>
    ///	<seealso cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;

    /// <summary>
    ///   Gets an array of types which contains all generic arguments.
    /// </summary>
    /// <remarks>
    ///   This method extracts generic arguments from the name of the generic
    ///   type. Invoking the method on the type <c>
    ///   TDictionary&lt;Integer,string&gt;</c> for example will return an
    ///   array which contains two types: <c>System.Integer</c> and <c>
    ///   System.string</c>.
    /// </remarks>
    function GetGenericArguments: TArray<TRttiType>;

    /// <summary>
    ///   Returns a string that represents a generic type definition.
    /// </summary>
    function GetGenericTypeDefinition: string;

    /// <summary>
    ///   Determines whether an instance of the current TRttiType can be
    ///   assigned from an instance of the specified TRttiType.
    /// </summary>
    /// <param name="rttiType">
    ///   The type to compare with the current type.
    /// </param>
    function IsAssignableFrom(const rttiType: TRttiType): Boolean;

    function IsType<T>: Boolean; overload;
    function IsType(typeInfo: PTypeInfo): Boolean; overload; inline;

    ///	<summary>
    ///	  Gets an enumerable collection which contains all constructor methods
    ///	  of the type, including inherited.
    ///	</summary>
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Propoerties" />
    ///	<seealso cref="Fields" />
    property Constructors: IEnumerable<TRttiMethod> read GetConstructors;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all methods that the type
    ///	  contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Propoerties" />
    ///	<seealso cref="Fields" />
    property Methods: IEnumerable<TRttiMethod> read GetMethods;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all properties that the
    ///	  type contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Fields" />
    property Properties: IEnumerable<TRttiProperty> read GetProperties;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all fields that the type
    ///	  contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Propoerties" />
    property Fields: IEnumerable<TRttiField> read GetFields;

    property AsClass: TRttiInstanceType read GetAsClass;
    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property AsDynamicArray: TRttiDynamicArrayType read GetAsDynamicArray;
    property IsClass: Boolean read GetIsClass;
    property IsInterface: Boolean read GetIsInterface;
    property IsClassOrInterface: Boolean read GetIsClassOrInterface;
    property IsDynamicArray: Boolean read GetIsDynamicArray;
    property IsString: Boolean read GetIsString;

    ///	<summary>
    ///	  Gets a value indicates whether the current type is generic.
    ///	</summary>
    property IsGenericType: Boolean read GetIsGenericType;

    property IsLazyType: Boolean read GetIsLazyType;
    property DefaultName: string read GetDefaultName;
    property AncestorCount: Integer read GetAncestorCount;
  end;

  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
    function GetAsMethod: TRttiMethod;
    function GetAsProperty: TRttiProperty;
    function GetAsField: TRttiField;
  public
//    procedure InvokeMember(instance: TValue; const arguments: array of TValue);
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
    property AsMethod: TRttiMethod read GetAsMethod;
    property AsProperty: TRttiProperty read GetAsProperty;
    property AsField: TRttiField read GetAsField;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
  end;

  TRttiMethodHelper = class helper for TRttiMethod
  private
    procedure DispatchValue(const value: TValue; typeInfo: PTypeInfo);
    function GetIsGetter: Boolean;
    function GetIsSetter: Boolean;
    function GetReturnTypeHandle: PTypeInfo;
  public
    function Invoke(Instance: TObject; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TClass; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TValue; const Args: array of TValue): TValue; overload;

    property IsGetter: Boolean read GetIsGetter;
    property IsSetter: Boolean read GetIsSetter;
    property ReturnTypeHandle: PTypeInfo read GetReturnTypeHandle;
  end;

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  TRttiInterfaceTypeHelper = class helper for TRttiInterfaceType
  private
    function GetHasGuid: Boolean;
  public
    /// <summary>
    ///   Determines whether this interface type has a guid.
    /// </summary>
    property HasGuid: Boolean read GetHasGuid;
  end;

  TValueHelper = record helper for TValue
  private
    function TryAsInterface(typeInfo: PTypeInfo; out Intf): Boolean;
  public
    function AsPointer: Pointer;
{$IFDEF DELPHI2010}
    function AsString: string;
{$ENDIF}
    function AsType<T>: T;
    function Cast(typeInfo: PTypeInfo): TValue;
    function Equals(const value: TValue): Boolean;
    function IsInstance: Boolean;
    function IsInterface: Boolean;
    function IsNumeric: Boolean;
    function IsPointer: Boolean;
    function IsString: Boolean;
    function IsVariant: Boolean;
{$IFDEF DELPHI2010}
    function IsType<T>: Boolean; overload;
    function IsType(ATypeInfo: PTypeInfo): Boolean; overload;
{$ENDIF}
    function ToObject: TObject;
  end;

implementation

uses
  Generics.Defaults,
  Math,
  RTLConsts,
  StrUtils,
  SysConst,
  TypInfo,
  Spring.ResourceStrings;

type
  PValueData = ^TValueData;


{$REGION 'TGuidHelper'}

class function TGuidHelper.Create(const guidString: string): TGuid;
begin
  Result := StringToGUID(guidString);
end;

class function TGuidHelper.Create(const bytes: TBytes): TGuid;
begin
  if Length(bytes) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Move(bytes[0], Result, SizeOf(Result));
end;

class function TGuidHelper.Create(a: Integer; b, c: SmallInt;
  const d: TBytes): TGuid;
begin
  if Length(d) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [8]);
  Result.D1 := LongWord(a);
  Result.D2 := Word(b);
  Result.D3 := Word(c);
  Move(d[0], Result.D4, SizeOf(Result.D4));
end;

class function TGuidHelper.Create(a: Cardinal; b, c: Word; d, e, f, g, h, i, j,
  k: Byte): TGuid;
begin
  Result.D1 := LongWord(a);
  Result.D2 := Word(b);
  Result.D3 := Word(c);
  Result.D4[0] := d;
  Result.D4[1] := e;
  Result.D4[2] := f;
  Result.D4[3] := g;
  Result.D4[4] := h;
  Result.D4[5] := i;
  Result.D4[6] := j;
  Result.D4[7] := k;
end;

class function TGuidHelper.Create(a: Integer; b, c: SmallInt; d, e, f, g, h, i,
  j, k: Byte): TGuid;
begin
  Result.D1 := LongWord(a);
  Result.D2 := Word(b);
  Result.D3 := Word(c);
  Result.D4[0] := d;
  Result.D4[1] := e;
  Result.D4[2] := f;
  Result.D4[3] := g;
  Result.D4[4] := h;
  Result.D4[5] := i;
  Result.D4[6] := j;
  Result.D4[7] := k;
end;

class function TGuidHelper.NewGuid: TGuid;
begin
  if CreateGUID(Result) <> S_OK then
    RaiseLastOSError;
end;

function TGuidHelper.Equals(const guid: TGuid): Boolean;
begin
  Result := SysUtils.IsEqualGUID(Self, guid);
end;

function TGuidHelper.GetIsEmpty: Boolean;
begin
  {$WARNINGS OFF}
  Result := Self.Equals(TGuid.Empty);
  {$WARNINGS ON}
end;

function TGuidHelper.ToString: string;
begin
  Result := SysUtils.GUIDToString(Self);
end;

function TGuidHelper.ToByteArray: TBytes;
begin
  SetLength(Result, 16);
  Move(D1, Result[0], SizeOf(Self));
end;

function TGuidHelper.ToBytes: TBytes;
begin
  SetLength(Result, 16);
  Move(D1, Result[0], SizeOf(Self));
end;

function TGuidHelper.ToQuotedString: string;
begin
  Result := QuotedStr(Self.ToString);
end;

class function TGuidHelper.GetEmpty: TGuid;
begin
  FillChar(Result, Sizeof(Result), 0);
end;

//class operator TGuidHelper.Equal(const left, right: TGuid) : Boolean;
//begin
//  Result := left.Equals(right);
//end;

//class operator TGuidHelper.NotEqual(const left, right: TGuid) : Boolean;
//begin
//  Result := not left.Equals(right);
//end;

{$ENDREGION}


{$REGION 'TMethodHelper'}

class function TMethodHelper.Create(const instance,
  methodAddress: Pointer): TMethod;
begin
  Result.Code := methodAddress;
  Result.Data := instance;
end;

{$ENDREGION}


{$REGION 'TStreamHelper'}

procedure TStreamHelper.ReadBuffer<T>(var value: T);
begin
  ReadBuffer(value, SizeOf(T));
end;

procedure TStreamHelper.WriteBuffer<T>(const value: T);
begin
  WriteBuffer(value, SizeOf(T));
end;

{$ENDREGION}


{$REGION 'TStringsHelper'}

procedure TStringsHelper.AddStrings(const strings: array of string);
var
  s: string;
begin
  BeginUpdate;
  try
    for s in strings do
    begin
      Add(s);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.AddOrUpdate(const name, value: string);
var
  index: Integer;
begin
  index := IndexOfName(name);
  if index <> -1 then
    Strings[index] := name + NameValueSeparator + value
  else
    Add(name + NameValueSeparator + value);
end;

procedure TStringsHelper.ExecuteUpdate(const proc: TProc);
begin
  Guard.CheckNotNull(Assigned(proc), 'proc');

  BeginUpdate;
  try
    Clear;
    proc;
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractNames(const strings: TStrings);
var
  i: Integer;
begin
  Guard.CheckNotNull(strings, 'strings');

  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
      strings.Add(Self.Names[i]);
  finally
    strings.EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractValues(const strings: TStrings);
var
  i: Integer;
begin
  Guard.CheckNotNull(strings, 'strings');

  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
      strings.Add(Self.ValueFromIndex[i]);
  finally
    strings.EndUpdate;
  end;
end;

function TStringsHelper.TryFindName(const name: string;
  var index: Integer): Boolean;
begin
  index := IndexOfName(name);
  Result := index > -1;
end;

function TStringsHelper.TryFindValue(const value: string;
  var index: Integer): Boolean;
var
  v: string;
  i: Integer;
begin
  index := -1;
  Result := False;
  for i := 0 to Count - 1 do
  begin
    v := ValueFromIndex[i];
    if SameText(v, value) then
    begin
      index := i;
      Exit(True);
    end;
  end;
end;

function TStringsHelper.TryFindObject(const obj: TObject;
  var index: Integer): Boolean;
begin
  index := IndexOfObject(obj);
  Result := index > -1;
end;

function TStringsHelper.ContainsName(const name: string): Boolean;
begin
  Result := IndexOfName(name) > -1;
end;

function TStringsHelper.ContainsValue(const value: string): Boolean;
var
  index: Integer;
begin
  Result := TryFindValue(value, index);
end;

function TStringsHelper.ContainsObject(const obj: TObject): Boolean;
begin
  Result := IndexOfObject(obj) > -1;
end;

function TStringsHelper.GetValueOrDefault<T>(const name: string;
  const default: T): T;
var
  index: Integer;
  value: string;
begin
  index := IndexOfName(name);
  if index > -1 then
    value := ValueFromIndex[index];
  if value <> '' then
    Result := TValue.From<string>(value).AsType<T>  // TODO: Fix this ASAP because TValue.AsType<T> sucks...
  else
    Result := default;
end;

function TStringsHelper.GetNames: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Names[i];
end;

function TStringsHelper.GetValues: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := ValueFromIndex[i];
end;

function TStringsHelper.ToArray: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Strings[i];
end;

function TStringsHelper.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

{$ENDREGION}


{$REGION 'TCollectionHelper'}

procedure TCollectionHelper.ExecuteUpdate(const proc: TProc);
begin
  Guard.CheckNotNull(Assigned(proc), 'proc');

  BeginUpdate;
  try
    Clear;
    proc;
  finally
    EndUpdate;
  end;
end;

{$ENDREGION}


{$REGION 'TRttiObjectHelper'}

function TRttiObjectHelper.TryGetCustomAttribute(
  attributeClass: TAttributeClass; out attribute: TCustomAttribute): Boolean;
begin
  attribute := GetCustomAttribute(attributeClass);
  Result := Assigned(attribute);
end;

function TRttiObjectHelper.TryGetCustomAttribute<T>(out attribute: T): Boolean;
begin
  attribute := GetCustomAttribute<T>;
  Result := Assigned(attribute);
end;

function TRttiObjectHelper.GetCustomAttribute(
  attributeClass: TAttributeClass): TCustomAttribute;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
      Exit(attribute);
  Result := nil;
end;

function TRttiObjectHelper.GetCustomAttribute<T>: T;
begin
  Result := T(GetCustomAttribute(TAttributeClass(T)));
end;

function TRttiObjectHelper.GetCustomAttributes(
  attributeClass: TAttributeClass): TArray<TCustomAttribute>;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute;
    end;
end;

function TRttiObjectHelper.GetCustomAttributes<T>: TArray<T>;
begin
  TArray<TCustomAttribute>(Result) := GetCustomAttributes(TAttributeClass(T));
end;

function TRttiObjectHelper.HasCustomAttribute(
  attributeClass: TAttributeClass): Boolean;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
      Exit(True);
  Result := False;
end;

function TRttiObjectHelper.HasCustomAttribute<T>: Boolean;
begin
  Result := HasCustomAttribute(TAttributeClass(T));
end;

{$ENDREGION}


{$REGION 'TRttiTypeHelper'}

function TRttiTypeHelper.InternalGetConstructors(
  enumerateBaseType: Boolean): IEnumerable<TRttiMethod>;
begin
  Result := TRttiMemberIterator<TRttiMethod>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end, enumerateBaseType, TMethodFilters.IsConstructor());
end;

function TRttiTypeHelper.InternalGetMethods(
  enumerateBaseType: Boolean): IEnumerable<TRttiMethod>;
begin
  Result := TRttiMemberIterator<TRttiMethod>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end, enumerateBaseType);
end;

function TRttiTypeHelper.InternalGetProperties(
  enumerateBaseType: Boolean): IEnumerable<TRttiProperty>;
begin
  Result := TRttiMemberIterator<TRttiProperty>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiProperty>
    begin
      Result := targetType.GetDeclaredProperties;
    end, enumerateBaseType);
end;

function TRttiTypeHelper.IsAssignableFrom(const rttiType: TRttiType): Boolean;
begin
  Result := Spring.IsAssignableFrom(Self.Handle, rttiType.Handle);
end;

function TRttiTypeHelper.IsType(typeInfo: PTypeInfo): Boolean;
begin
  Result := Handle = typeInfo;
end;

function TRttiTypeHelper.IsType<T>: Boolean;
begin
  Result := Handle = TypeInfo(T);
end;

function TRttiTypeHelper.InternalGetFields(
  enumerateBaseType: Boolean): IEnumerable<TRttiField>;
begin
  Result := TRttiMemberIterator<TRttiField>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiField>
    begin
      Result := targetType.GetDeclaredFields;
    end, enumerateBaseType);
end;

function TRttiTypeHelper.GetConstructors: IEnumerable<TRttiMethod>;
begin
  Result := InternalGetConstructors;
end;

function TRttiTypeHelper.GetDefaultName: string;
begin
  if IsPublicType then
    Result := QualifiedName
  else
    Result := Name;
end;

function TRttiTypeHelper.GetMethods: IEnumerable<TRttiMethod>;
begin
  Result := InternalGetMethods;
end;

function TRttiTypeHelper.GetProperties: IEnumerable<TRttiProperty>;
begin
  Result := InternalGetProperties;
end;

function TRttiTypeHelper.GetFields: IEnumerable<TRttiField>;
begin
  Result := InternalGetFields;
end;

{$IFNDEF DELPHIXE_UP}
function SplitString(const s: string; delimiter: Char): TStringDynArray;
var
  list: TStrings;
  i: Integer;
begin
  list := TStringList.Create;
  try
    list.StrictDelimiter := True;
    list.Delimiter := delimiter;
    list.DelimitedText := s;
    SetLength(Result, list.Count);
    for i := 0 to list.Count - 1 do
      Result[i] := list[i];
  finally
    list.Free;
  end;
end;
{$ENDIF}

// Nullable<TDateTime>
// TDictionary<string, TObject>
// TDictionary<string, IDictionary<string, TObject>>
function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;

  function ScanChar(const s: string; var index: Integer): Boolean;
  var
    level: Integer;
  begin
    Result := False;
    level := 0;
    while index <= Length(s) do
    begin
      case s[index] of
        ',': if level = 0 then Exit(True);
        '<': Inc(level);
        '>': Dec(level);
      end;
      Inc(index);
      Result := level = 0;
    end;
  end;

  function SplitTypes(const s: string): TStringDynArray;
  var
    startPos, index: Integer;
  begin
    startPos := 1;
    index := 1;
    while ScanChar(s, index) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Copy(s, startPos, index - startPos);
      Inc(index);
      startPos := index;
    end;
  end;

var
  i: Integer;
  s: string;
  names: TStringDynArray;
begin
  s := Name;
  i := Pos('<', s);
  if i = 0 then
    Exit(nil);
  s := Copy(s, i + 1, Length(s) - i - 1);
  names := SplitTypes(s);
  SetLength(Result, Length(names));
  for i := Low(names) to High(names) do
    Result[i] := TType.FindType(names[i]);
end;

function TRttiTypeHelper.GetGenericTypeDefinition: string;
var
  s: string;
  i: Integer;
begin
  s := Name;
  i := Pos('<', s);
  if i = 0 then
    raise EInvalidOperationException.CreateResFmt(@SNotGenericType, [Name]);
  Result := Copy(s, 0, i) + '>';
end;

function TRttiTypeHelper.GetAncestorCount: Integer;
var
  baseType: TRttiType;
begin
  Result := 0;
  baseType := Self;
  while Assigned(baseType.BaseType) do
  begin
    Inc(Result);
    baseType := baseType.BaseType;
  end;
end;

function TRttiTypeHelper.GetAsClass: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
end;

function TRttiTypeHelper.GetAsDynamicArray: TRttiDynamicArrayType;
begin
  Result := Self as TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetInterfaces: IEnumerable<TRttiInterfaceType>;
var
  list: IDictionary<TGUID, TRttiInterfaceType>;
  classType: TClass;
  table: PInterfaceTable;
  entry: TInterfaceEntry;
  aType: TRttiInterfaceType;
  i: Integer;
begin
  if Self.IsClass then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    classType := Self.AsInstance.MetaclassType;
    while Assigned(classType) do
    begin
      table := classType.GetInterfaceTable;
      if Assigned(table) then
      begin
        for i := 0 to table.EntryCount - 1 do
        begin
          entry := table.Entries[i];
          if not list.ContainsKey(entry.IID) and
        {$WARNINGS OFF}
            not entry.IID.IsEmpty and
        {$WARNINGS ON}
            TType.TryGetInterfaceType(entry.IID, aType) then
          begin
            list[entry.IID] := aType;
          end;
        end;
      end;
      classType := classType.ClassParent;
    end;
    Result := list.Values;
  end
  else
  if Self.IsInterface then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    aType := Self.AsInterface;
    while Assigned(aType) do
    begin
      if aType.HasGuid and not list.ContainsKey(aType.GUID)
        and not IsEqualGUID(aType.GUID, TGuid.Empty) then
      begin
        list[aType.GUID] := aType;
      end;
      aType := aType.BaseType;
    end;
    Result := list.Values;
  end;
end;

function TRttiTypeHelper.GetIsClass: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiTypeHelper.GetIsClassOrInterface: Boolean;
begin
  Result := Self.IsClass or Self.IsInterface;
end;

function TRttiTypeHelper.GetIsDynamicArray: Boolean;
begin
  Result := Self is TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetIsGenericType: Boolean;
begin
  Result := (Pos('<', Name) > 0) and (Pos('>', Name) > 0);
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

function TRttiTypeHelper.GetIsLazyType: Boolean;
begin
  Result := TType.IsLazy(Handle);
end;

function TRttiTypeHelper.GetIsString: Boolean;
begin
  Result := TypeKind in [tkString, tkLString, tkWString, tkUString, tkChar, tkWChar];
end;

{$ENDREGION}


{$REGION 'TRttiInterfaceTypeHelper'}

function TRttiInterfaceTypeHelper.GetHasGuid: Boolean;
begin
  Result := ifHasGuid in Self.IntfFlags;
end;

{$ENDREGION}


{$REGION 'TRttiMemberHelper'}

function TRttiMemberHelper.GetValue(const instance: TValue): TValue;
begin
  if IsProperty then
    Result := AsProperty.GetValue(instance)
  else if IsField then
    Result := AsField.GetValue(instance)
  else
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_GetValue);
end;

procedure TRttiMemberHelper.SetValue(const instance, value: TValue);
begin
  if IsProperty then
    AsProperty.SetValue(instance, value)
  else if IsField then
    AsField.SetValue(instance, value)
  else
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_SetValue);
end;

function TRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function TRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function TRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function TRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;

function TRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function TRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function TRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function TRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

function TRttiMemberHelper.GetAsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function TRttiMemberHelper.GetAsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function TRttiMemberHelper.GetAsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

{$ENDREGION}


{$REGION 'TRttiMethodHelper'}

procedure TRttiMethodHelper.DispatchValue(const value: TValue;
  typeInfo: PTypeInfo);
begin
  if (value.TypeInfo <> typeInfo) and (value.Kind = tkInterface)
    and (typeInfo.Kind = tkInterface)
    and IsAssignableFrom(typeInfo, value.TypeInfo) then
    PValueData(@value).FTypeInfo := typeInfo;
end;

function GetCodeAddress(const classType: TClass; const proc: Pointer): Pointer;
begin
  if (Integer(proc) and $FF000000) = $FF000000 then
    Exit(nil);
  if (Integer(proc) and $FF000000) = $FE000000 then
    Result := PPointer(Integer(classType) + SmallInt(proc))^
  else
    Result := proc;
end;

function TRttiMethodHelper.GetIsGetter: Boolean;
var
  prop: TRttiProperty;
  code: Pointer;
begin
  for prop in Parent.GetProperties do
    if prop is TRttiInstanceProperty then
    begin
      code := GetCodeAddress(prop.Parent.AsInstance.MetaclassType,
        TRttiInstanceProperty(prop).PropInfo.GetProc);
      if code = CodeAddress then
        Exit(True);
    end;
  Result := False;
end;

function TRttiMethodHelper.GetIsSetter: Boolean;
var
  prop: TRttiProperty;
  code: Pointer;
begin
  for prop in Parent.GetProperties do
    if prop is TRttiInstanceProperty then
    begin
      code := GetCodeAddress(prop.Parent.AsInstance.MetaclassType,
        TRttiInstanceProperty(prop).PropInfo.SetProc);
      if code = CodeAddress then
        Exit(True);
    end;
  Result := False;
end;

function TRttiMethodHelper.GetReturnTypeHandle: PTypeInfo;
var
  returnType: TRttiType;
begin
  returnType := Self.ReturnType;
  if Assigned(returnType) then
    Result := returnType.Handle
  else
    Result := nil;
end;

function TRttiMethodHelper.Invoke(Instance: TObject;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  if MethodKind = mkOperatorOverload then
    Result := Rtti.Invoke(CodeAddress, TArray.Copy<TValue>(Args),
      CallingConvention, ReturnTypeHandle{$IFDEF DELPHIXE2_UP}, IsStatic{$ENDIF})
  else
    Result := Self.DispatchInvoke(Instance, Args);
end;

function TRttiMethodHelper.Invoke(Instance: TClass;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  if MethodKind = mkOperatorOverload then
    Result := Rtti.Invoke(CodeAddress, TArray.Copy<TValue>(Args),
      CallingConvention, ReturnTypeHandle{$IFDEF DELPHIXE2_UP}, IsStatic{$ENDIF})
  else
    Result := Self.DispatchInvoke(Instance, Args);
end;

function TRttiMethodHelper.Invoke(Instance: TValue;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  if MethodKind = mkOperatorOverload then
    Result := Rtti.Invoke(CodeAddress, TArray.Copy<TValue>(Args),
      CallingConvention, ReturnTypeHandle{$IFDEF DELPHIXE2_UP}, IsStatic{$ENDIF})
  else
    Result := Self.DispatchInvoke(Instance, Args);
end;

{$ENDREGION}


{$REGION 'TRttiPropertyHelper'}

function TRttiPropertyHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := GetValue(instance.AsObject)
  else
    Result := GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiPropertyHelper.SetValue(const instance, value: TValue);
var
  temp: TValue;
begin
  temp := value.Cast(PropertyType.Handle);
  if instance.IsObject then
    SetValue(instance.AsObject, temp)
  else
    SetValue(instance.GetReferenceToRawData, temp);
end;

{$ENDREGION}


{$REGION 'TRttiFieldHelper'}

function TRttiFieldHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := AsField.GetValue(instance.AsObject)
  else
    Result := AsField.GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiFieldHelper.SetValue(const instance, value: TValue);
var
  temp: TValue;
begin
  temp := value.Cast(FieldType.Handle);
  if instance.IsObject then
    SetValue(instance.AsObject, temp)
  else
    SetValue(instance.GetReferenceToRawData, temp);
end;

{$ENDREGION}


{$REGION 'TValueHelper'}

{$IFDEF DELPHI2010}
function TValueHelper.AsString: string;
begin
  Result := AsType<string>;
end;
{$ENDIF}

function TValueHelper.AsPointer: Pointer;
begin
  if Kind in [tkClass, tkInterface] then
    Result := ToObject
  else
    Result := PPointer(GetReferenceToRawData)^;
end;

function TValueHelper.AsType<T>: T;
begin
{$IFDEF DELPHI2010}
  if IsEmpty then
    Exit(Default(T));
{$ENDIF}
  if not TryAsInterface(System.TypeInfo(T), Result) then
  if not TryAsType<T>(Result) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValueHelper.Cast(typeInfo: PTypeInfo): TValue;
var
  intf: IInterface;
begin
  if TryAsInterface(typeInfo, intf) then
    TValue.Make(@intf, typeInfo, Result)
  else if not TryCast(typeInfo, Result) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function EqualsFail(const left, right: TValue): Boolean;
begin
  Result := False;
end;

function EqualsInt2Int(const left, right: TValue): Boolean;
begin
  Result := left.AsInteger = left.AsInteger;
end;

function EqualsInt2Float(const left, right: TValue): Boolean;
begin
  if right.IsType<Single> then
    Result := Math.SameValue(left.AsInteger, right.AsType<Single>)
  else if right.IsType<Double> then
    Result := Math.SameValue(left.AsInteger, right.AsType<Double>)
  else
    Result := Math.SameValue(left.AsInteger, right.AsExtended);
end;

function EqualsInt2Int64(const left, right: TValue): Boolean;
begin
  Result := left.AsInteger = left.AsInt64;
end;

function EqualsFloat2Int(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle: Result := Math.SameValue(left.AsType<Single>, right.AsInteger);
    ftDouble: Result := Math.SameValue(left.AsType<Double>, right.AsInteger);
  else
    Result := Math.SameValue(left.AsExtended, right.AsInteger);
  end;
end;

function EqualsFloat2Float(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(left.AsType<Single>, right.AsType<Single>);
        ftDouble: Result := Math.SameValue(left.AsType<Single>, right.AsType<Double>);
      else
        Result := Math.SameValue(left.AsType<Single>, right.AsExtended);
      end;
    ftDouble:
      case right.TypeData.FloatType of
        ftSingle: Result := Math.SameValue(left.AsType<Double>, right.AsType<Single>);
        ftDouble: Result := Math.SameValue(left.AsType<Double>, right.AsType<Double>);
      else
        Result := Math.SameValue(left.AsType<Double>, right.AsExtended);
      end;
  else
    case right.TypeData.FloatType of
      ftSingle: Result := Math.SameValue(left.AsExtended, right.AsType<Single>);
      ftDouble: Result := Math.SameValue(left.AsExtended, right.AsType<Double>);
    else
      Result := Math.SameValue(left.AsExtended, right.AsExtended);
    end;
  end;
end;

function EqualsFloat2Int64(const left, right: TValue): Boolean;
begin
  case left.TypeData.FloatType of
    ftSingle: Result := Math.SameValue(left.AsType<Single>, right.AsInt64);
    ftDouble: Result := Math.SameValue(left.AsType<Double>, right.AsInt64);
  else
    Result := Math.SameValue(left.AsExtended, right.AsInt64);
  end;
end;

function EqualsInt642Int(const left, right: TValue): Boolean;
begin
  Result := left.AsInt64 = left.AsInteger;
end;

function EqualsInt64ToFloat(const left, right: TValue): Boolean;
begin
  if right.IsType<Single> then
    Result := Math.SameValue(left.AsInt64, right.AsType<Single>)
  else if right.IsType<Double> then
    Result := Math.SameValue(left.AsInt64, right.AsType<Double>)
  else
    Result := Math.SameValue(left.AsInt64, right.AsExtended);
end;

function EqualsInt642Int64(const left, right: TValue): Boolean;
begin
  Result := left.AsInt64 = left.AsInt64;
end;

function EqualsStr2Str(const left, right: TValue): Boolean;
begin
  Result := left.AsString = right.AsString;
end;

function EqualsClass2Class(const left, right: TValue): Boolean;
begin
  Result := left.AsObject = right.AsObject;
end;

function EqualsIntf2Intf(const left, right: TValue): Boolean;
begin
  Result := left.AsInterface = right.AsInterface;
end;

function EqualsClassRef2ClassRef(const left, right: TValue): Boolean;
begin
  Result := left.AsClass = right.AsClass;
end;

function EqualsPtr2Ptr(const left, right: TValue): Boolean;
begin
  Result := left.AsPointer = right.AsPointer;
end;

function EqualsVar2Var(const left, right: TValue): Boolean;
begin
  Result := left.AsVariant = right.AsVariant;
end;

function EqualsRec2Rec(const left, right: TValue): Boolean;
var
  comparer: TRttiMethod;
begin
  if TType.GetType(left.TypeInfo).Methods.TryGetSingle(comparer,
    TMethodFilters.IsNamed('&op_Equality') and
    TMethodFilters.HasParameterTypes([left.TypeInfo, right.TypeInfo])) then
    Result := comparer.Invoke(nil, [left, right]).AsBoolean
  else
    Result := CompareMem(left.GetReferenceToRawData, right.GetReferenceToRawData, left.DataSize);
end;

{$REGION 'Comparisons'}
type
  TEqualsFunc = function(const left, right: TValue): Boolean;
const
  Comparisons: array[TTypeKind,TTypeKind] of TEqualsFunc = (
    // tkUnknown
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsPtr2Ptr, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkInteger
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsInt2Int, EqualsFail, EqualsFail, EqualsInt2Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsInt2Int64, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkEnumeration
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsInt2Int, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkFloat
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFloat2Int, EqualsFail, EqualsFail, EqualsFloat2Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFloat2Int64, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkSet
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail, // TODO: tkSet
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkClass
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsClass2Class, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail, // TODO: tkMethod
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkWChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkLString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkWString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsVar2Var, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsRec2Rec,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkInterface
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsIntf2Intf, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkInt64
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsInt642Int, EqualsFail, EqualsFail, EqualsInt64ToFloat,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsInt642Int64, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsPtr2Ptr, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkUString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsStr2Str, EqualsStr2Str, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsStr2Str, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkClassRef
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsClassRef2ClassRef,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    ),
    // tkPointer
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsPtr2Ptr, EqualsFail
    ),
    // tkProcedure
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      EqualsFail, EqualsFail, EqualsFail, EqualsFail, EqualsFail,
      // tkPointer, tkProcedure
      EqualsFail, EqualsFail
    )
  );
{$ENDREGION}

function TValueHelper.Equals(const value: TValue): Boolean;
begin
  Result := Assigned(TypeInfo) and Assigned(value.TypeInfo)
    and Comparisons[Kind,value.Kind](Self, value);
end;

function TValueHelper.IsInstance: Boolean;
begin
  Result := Kind in [tkClass, tkInterface];
end;

function TValueHelper.IsInterface: Boolean;
begin
  Result := Kind = tkInterface;
end;

function TValueHelper.IsNumeric: Boolean;
const
  NumericKinds = [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
begin
  Result := IsEmpty or (Kind in NumericKinds);
end;

function TValueHelper.IsPointer: Boolean;
begin
  Result := Kind = tkPointer;
end;

function TValueHelper.IsString: Boolean;
const
  StringKinds = [tkString, tkLString, tkWString, tkUString, tkChar, tkWChar];
begin
  Result := IsEmpty or (Kind in StringKinds);
end;

function TValueHelper.IsVariant: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Variant);
end;

{$IFDEF DELPHI2010}
function TValueHelper.IsType(ATypeInfo: PTypeInfo): Boolean;
var
  unused: TValue;
begin
  Result := IsEmpty or TryCast(ATypeInfo, unused);
end;

function TValueHelper.IsType<T>: Boolean;
begin
  Result := IsType(System.TypeInfo(T));
end;
{$ENDIF}

function TValueHelper.ToObject: TObject;
begin
  if IsInterface then
    Result := AsInterface as TObject
  else
    Result := AsObject;
end;

function TValueHelper.TryAsInterface(typeInfo: PTypeInfo; out Intf): Boolean;
var
  typeData: PTypeData;
  obj: TObject;
begin
  if not (Kind in [tkClass, tkInterface]) then
    Exit(False);
  if typeInfo.Kind <> tkInterface then
    Exit(False);
  if Self.TypeInfo = typeInfo then
    Result := True
  else
  begin
    typeData := GetTypeData(typeInfo);
    if Kind = tkClass then
    begin
{$IFDEF AUTOREFCOUNT}
      Self.FData.FValueData.ExtractRawData(@obj);
{$ELSE}
      obj := TObject(Self.FData.FAsObject);
{$ENDIF}
      Exit(Supports(obj, typeData.Guid, Intf));
    end;
    Result := False;
    typeData := Self.TypeData;
    while Assigned(typeData) and Assigned(typeData.IntfParent) do
    begin
      if typeData.IntfParent^ = typeInfo then
      begin
        Result := True;
        Break;
      end;
      typeData := GetTypeData(typeData.IntfParent^);
    end;
  end;
  if Result then
    IInterface(Intf) := AsInterface;
end;

{$ENDREGION}


end.
