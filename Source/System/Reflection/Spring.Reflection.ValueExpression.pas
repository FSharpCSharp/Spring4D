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

{$REGION 'Documentation'}
///	<summary>Implementation of Exrepssion part of Value.</summary>
/// <preliminary />
{$ENDREGION}
unit Spring.Reflection.ValueExpression;

interface

uses
  Rtti,
  Spring;

type

  {$REGION 'Defines the interface for an abstract value provider. '}
  ///	<summary>Defines the interface for an abstract value provider.</summary>
  ///	<remarks>Use the <see cref="Value" /> property to retrieve the current value from
  ///	the provider. Use the <see cref="SetValue(TValue)" /> method to assign a new value to it
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
  ///	<summary>Provides an abstract base class for generic value
  ///	provider.</summary>
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

  {$REGION 'Value Expression'}

  /// <summary>
  /// Expression part of Value
  /// </summary>
  /// <preliminary />
  IValueExpression = interface(IValueProvider)
    ['{A0EB72F0-06AE-460D-8AA2-A0A95BAC4A86}']
    function Follow(const path: string): IValueExpression;
    function GetInstanceValue: TValue;
    function GetExpression: string;
    property InstanceValue: TValue read GetInstanceValue;
    property Expression: string read GetExpression;
  end;

  /// <summary>
  /// Default implementation of Exrepssion part of Value
  /// </summary>
  /// <preliminary />
  TValueExpression = class(TValueProviderBase, IValueExpression)
  strict private
    fInstance: TValue;
    fParent: IValueExpression;
    fExpression: string;
    type
      PPByte = ^PByte;
    function Follow(const path: string): IValueExpression;
    function GetInstanceValue: TValue;
    function GetExpression: string;
  private
    function IndexRef(index: Integer): IValueExpression;
    function MemberRef(const name: string): IValueExpression;
    function GetMemberValue(const instance: TValue;
      const name: string): TValue;
    procedure SetMemberValue(const instance: TValue;
      const name: string; const value: TValue);
  protected
    function GetValue: TValue; override;
    procedure DoSetValue(const value: TValue); override;
    function GetIsReadOnly: Boolean; override;
  public
    constructor Create(const instance: TValue); overload;
    constructor Create(const instance: TValue;
      const expression: string); overload;
    constructor Create(const parent: IValueExpression;
      const member: string); overload;
    property InstanceValue: TValue read GetInstanceValue;
    property Expression: string read GetExpression;
  end;

  {$ENDREGION}

  {$REGION 'Global Routines'}

  /// <summary>
  /// Easy access to particulary IValueExpression within any Object hierarchy
  /// </summary>
  /// <returns>Returns IValueExpression which is equivalent to Object location due to path.</returns>
  function EvaluateExpression(const path: string;
    root: IValueExpression): IValueExpression;

  {$ENDREGION}

implementation

uses
  StrUtils,
  Character,
  SysUtils,
  Spring.ResourceStrings;

{$REGION 'Global Routines'}

function EvaluateExpression(const path: string;
  root: IValueExpression): IValueExpression;

  function SkipWhite(p: PChar): PChar;
  begin
    while IsWhiteSpace(p^) do
      Inc(p);
    Result := p;
  end;

  function ScanName(p: PChar; out str: string): PChar;
  begin
    Result := p;
    while IsLetterOrDigit(Result^) do
      Inc(Result);
    SetString(str, p, Result - p);
  end;

  function ScanNumber(p: PChar; out n: Integer): PChar;
  var
    v: Integer;
  begin
    v := 0;
    while (p >= '0') and (p <= '9') do
    begin
      v := v * 10 + Ord(p^) - Ord('0');
      Inc(p);
    end;
    n := v;
    Result := p;
  end;

const
  tkEof = #0;
  tkNumber = #1;
  tkName = #2;
  tkDot = '.';
  tkLBracket = '[';
  tkRBracket = ']';

var
  cp: PChar;
  currentToken: Char;
  tokenName: string;
  tokenNumber: Integer;

  function NextToken: Char;
    function SetToken(p: PChar): PChar;
    begin
      currentToken := p^;
      Result := p + 1;
    end;
  var
    p: PChar;
  begin
    p := cp;
    p := SkipWhite(p);
    if p^ = #0 then
    begin
      cp := p;
      currentToken := tkEof;
      Exit(currentToken);
    end;

    case p^ of
      '0'..'9':
      begin
        cp := ScanNumber(p, tokenNumber);
        currentToken := tkNumber;
      end;
      '^', '[', ']', '.': cp := SetToken(p);
    else
      cp := ScanName(p, tokenName);
      if tokenName = '' then
        raise Exception.CreateResFmt(@SInvalidExpressionPath, [tokenName]);
      currentToken := tkName;
    end;

    Result := currentToken;
  end;

  function Describe(token: Char): string;
  begin
    case token of
      tkEof: Result := 'end of string';
      tkNumber: Result := 'number';
      tkName: Result := 'name';
    else
      Result := '''' + token + '''';
    end;
  end;

  procedure Expect(token: Char);
  begin
    if token <> currentToken then
      raise Exception.CreateResFmt(@SUnexpectedToken,
        [Describe(token), Describe(currentToken)]);
  end;

  { Semantic actions are methods on TLocation }
var
  expression: IValueExpression;

  { Driver and parser }

begin
  cp := PChar(path);
  NextToken;

  expression := root;

  // Syntax:
  // path ::= ( '.' <name> | '[' <index> ']' | '^' )+ ;;

  // Semantics:

  // '<name>' are field names, '[]' is array indexing, '^' is pointer
  // indirection.

  // Parser continuously calculates the address of the value in question,
  // starting from the root.

  // When we see a name, we look that up as a field on the current type,
  // then add its offset to our current location if the current location is
  // a value type, or indirect (PPointer(x)^) the current location before
  // adding the offset if the current location is a reference type. If not
  // a record or class type, then it's an error.

  // When we see an indexing, we expect the current location to be an array
  // and we update the location to the address of the element inside the array.
  // All dimensions are flattened (multiplied out) and zero-based.

  // When we see indirection, we expect the current location to be a pointer,
  // and dereference it.

  while True do
  begin
    case currentToken of
      tkEof: Break;
      tkName:
      begin
        expression := (expression as TValueExpression).MemberRef(tokenName);
        NextToken;
      end;
      tkDot:
      begin
        NextToken;
        Expect(tkName);
        expression := (expression as TValueExpression).MemberRef(tokenName);
        NextToken;
      end;
      '[':
      begin
        NextToken;
        Expect(tkNumber);
        expression := (expression as TValueExpression).IndexRef(tokenNumber);
        NextToken;
        Expect(']');
        NextToken;
      end;
      '^':
      begin
        expression := (expression as TValueExpression).MemberRef(tokenName);
        NextToken;
      end;
    else
      raise Exception.CreateResFmt(@SInvalidExpressionSyntax, [currentToken]);
    end;
  end;

  Result := expression;
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

{$REGION 'TValueExpression'}

constructor TValueExpression.Create(const instance: TValue);
begin
  Create(instance, '');
end;

constructor TValueExpression.Create(const instance: TValue;
  const expression: string);
begin
  fParent := nil;
  fInstance := instance;
  fExpression := expression;
end;

constructor TValueExpression.Create(const parent: IValueExpression;
  const member: string);
begin
  fParent := parent;
  fExpression := member;
end;

function TValueExpression.GetInstanceValue: TValue;
begin
  if fParent = nil then
    Exit(fInstance)
  else
    Exit(fParent.Value);
end;

procedure TValueExpression.SetMemberValue(const instance: TValue;
  const name: string; const value: TValue);
var
  rttiCtx: TRttiContext;
  instanceType: TRttiType;
  field: TRttiField;
  prop: TRttiProperty;
  staticArray: TRttiArrayType;
  dynamicArray: TRttiDynamicArrayType;
  localInstance: TValue;
begin
  rttiCtx := TRttiContext.Create;
  instanceType := rttiCtx.GetType(instance.TypeInfo);
  if instanceType is TRttiArrayType then
  begin
    staticArray := instanceType as TRttiArrayType;
    value.Cast(staticArray.ElementType.Handle).ExtractRawData(PByte(instance.GetReferenceToRawData) +
      staticArray.ElementType.TypeSize * StrToInt(fExpression));
  end
  else if instanceType is TRttiDynamicArrayType then
  begin
    dynamicArray := instanceType as TRttiDynamicArrayType;
    value.Cast(dynamicArray.ElementType.Handle).ExtractRawData(PPByte(instance.GetReferenceToRawData)^ +
      dynamicArray.ElementType.TypeSize * StrToInt(fExpression));
  end
  else
  begin
    if instanceType is TRttiInterfaceType then
    begin
      localInstance := TObject(instance.AsInterface);
      instanceType := rttiCtx.GetType(localInstance.TypeInfo);
    end
    else localInstance := instance;

    field := instanceType.GetField(fExpression);
    if Assigned(field) then
    begin
      if localInstance.IsObject then
        field.SetValue(localInstance.AsObject, value)
      else
        value.Cast(field.FieldType.Handle).ExtractRawData(PByte(localInstance.GetReferenceToRawData) +
          field.Offset);
    end
    else
    begin
      prop := instanceType.GetProperty(fExpression);
      if Assigned(prop) then
      begin
        if localInstance.IsObject then
          prop.SetValue(localInstance.AsObject, value)
        else
          prop.SetValue(localInstance.GetReferenceToRawData, value);
      end
      else
        raise Exception.CreateResFmt(@SCouldNotFindPath, [fExpression]);
    end;
  end;
  rttiCtx.Free;
end;

function TValueExpression.GetMemberValue(const instance: TValue; const name: string): TValue;
var
  rttiCtx: TRttiContext;
  instanceType: TRttiType;
  field: TRttiField;
  prop: TRttiProperty;
  staticArray: TRttiArrayType;
  dynamicArray: TRttiDynamicArrayType;
  localInstance: TValue;
begin
  rttiCtx := TRttiContext.Create;
  instanceType := rttiCtx.GetType(instance.TypeInfo);
  if instanceType is TRttiArrayType then
  begin
    staticArray := instanceType as TRttiArrayType;
    TValue.Make(PByte(instance.GetReferenceToRawData) +
      staticArray.ElementType.TypeSize * StrToInt(fExpression),
      staticArray.ElementType.Handle, Result);
  end
  else
  if instanceType is TRttiDynamicArrayType then
  begin
    dynamicArray := instanceType as TRttiDynamicArrayType;
    TValue.Make(PPByte(instance.GetReferenceToRawData)^ +
      dynamicArray.ElementType.TypeSize * StrToInt(fExpression),
      dynamicArray.ElementType.Handle, Result);
  end
  else
  begin
    if instanceType is TRttiInterfaceType then
    begin
      localInstance := TObject(instance.AsInterface);
      instanceType := rttiCtx.GetType(localInstance.TypeInfo);
    end
    else localInstance := instance;

    field := instanceType.GetField(fExpression);
    if Assigned(field) then
    begin
      if localInstance.IsObject then
        Result := field.GetValue(localInstance.AsObject)
      else
        TValue.Make(PByte(localInstance.GetReferenceToRawData) + field.Offset,
          field.FieldType.Handle, Result);
    end
    else
    begin
      prop := instanceType.GetProperty(fExpression);
      if Assigned(prop) then
      begin
        if localInstance.IsObject then
          Result := prop.GetValue(localInstance.AsObject)
        else
          Result := prop.GetValue(localInstance.GetReferenceToRawData);
      end
      else
        raise Exception.CreateResFmt(@SCouldNotFindPath, [fExpression]);
    end;
  end;
  rttiCtx.Free;
end;

function TValueExpression.GetIsReadOnly: Boolean;
var
  prop: TRttiProperty;
  expression: IValueExpression;
  rttiCtx: TRttiContext;
begin
  Result := False;
  expression := Self;
  if fParent = nil then
  begin
    if ContainsStr(fExpression, '.') then
      expression := EvaluateExpression(fExpression,
        TValueExpression.Create(fInstance, ''))
  end;
  rttiCtx := TRttiContext.Create;
  prop := rttiCtx.GetType(expression.InstanceValue.TypeInfo).GetProperty(expression.Expression);
  if Assigned(prop) then
    Result := not prop.IsWritable;
  rttiCtx.Free;
end;

function TValueExpression.GetExpression: string;
begin
  Result := fExpression;
end;

procedure TValueExpression.DoSetValue(const value: TValue);
begin
  inherited;
  if fParent = nil then
  begin
    if ContainsStr(fExpression, '.') then
      EvaluateExpression(fExpression,
        TValueExpression.Create(fInstance, '')).SetValue(value)
    else
      if fExpression <> EmptyStr then
        SetMemberValue(InstanceValue, fExpression, value)
      else
        fInstance := value;
  end
  else
    SetMemberValue(InstanceValue, fExpression, value);
end;

function TValueExpression.GetValue: TValue;
begin
  if fParent = nil then
  begin
    if ContainsStr(fExpression, '.') then
      Result := EvaluateExpression(fExpression,
        TValueExpression.Create(fInstance, '')).Value
    else
      if fExpression <> EmptyStr then
        Result := GetMemberValue(InstanceValue, fExpression)
      else
        Result := fInstance;
  end
  else
    Result := GetMemberValue(InstanceValue, fExpression);
end;

function TValueExpression.IndexRef(index: Integer): IValueExpression;
begin
  Result := TValueExpression.Create(Self, IntToStr(index))
end;

function TValueExpression.MemberRef(const name: string): IValueExpression;
begin
  Result := TValueExpression.Create(Self, name);
end;

function TValueExpression.Follow(const path: string): IValueExpression;
begin
  Result := EvaluateExpression(path, Self);
end;

{$ENDREGION}

end.
