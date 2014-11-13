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
  SysUtils,
  Types;

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

implementation

uses
  Generics.Defaults,
  RTLConsts,
  Rtti,
  StrUtils,
  SysConst,
  TypInfo,
  Spring.ResourceStrings;


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
  BeginUpdate;
  try
    Clear;
    proc;
  finally
    EndUpdate;
  end;
end;

{$ENDREGION}


end.
