{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.Reflection.Filters;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  TypInfo,
  Rtti,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  /// <summary>
  /// Provides static methods to create specifications to filter TRttiMember objects.
  /// </summary>
  TFiltersBase<T: TRttiMember> = class
  public
    class function HasAttribute(attributeClass: TAttributeClass): TSpecification<T>;
    class function HasParameterTypes(const types: array of PTypeInfo): TSpecification<T>;
    class function HasParameterFlags(const flags: TParamFlags): TSpecification<T>;
    class function IsNamed(const name: string): TSpecification<T>;
    class function IsTypeOf<TType>: TSpecification<T>; overload;
    class function IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>; overload;
    class function IsConstructor: TSpecification<T>;
    class function IsInstanceMethod: TSpecification<T>;
    class function IsInvokable: TSpecification<T>;
  end;

  TMemberFilters = class(TFiltersBase<TRttiMember>);

  TMethodFilters = class(TFiltersBase<TRttiMethod>);

  TPropertyFilters = class(TFiltersBase<TRttiProperty>);

  TFieldFilters = class(TFiltersBase<TRttiField>);


  TMemberSpecificationBase<T: TRttiMember> = class abstract(TSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; virtual; abstract;
  public
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  TNameFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fName: string;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  TInvokableFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  THasAttributeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass);
  end;

  TTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  THasParameterTypesFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  TRttiMemberClass = class of TRttiMember;

  TMemberTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  TConstructorFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  TInstanceMethodFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  THasParameterFlagsFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

implementation

uses
  Generics.Collections,
  Spring.Helpers;


{ TMemberSpecificationBase }

function TMemberSpecificationBase<T>.IsSatisfiedBy(
  const member: T): Boolean;
begin
//  TArgument.CheckNotNull(member, 'member');
  Result := Accept(member);
end;

{ THasAttributeSpec }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass);
begin
  inherited Create;
  fAttributeClass := attributeClass;
end;

function THasAttributeFilter<T>.Accept(const member: T): Boolean;
var
  attribute: TCustomAttribute;
begin
  Result := False;
  for attribute in member.GetAttributes do
  begin
    if attribute.InheritsFrom(fAttributeClass) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TNameFilter<T> }

constructor TNameFilter<T>.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

function TNameFilter<T>.Accept(const member: T): Boolean;
begin
  Result := SameText(member.Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.Accept(const member: T): Boolean;
begin
  if member.IsProperty then
  begin
    Result := member.AsProperty.PropertyType.Handle = fTypeInfo;
  end
  else if member.IsField then
  begin
    Result := member.AsField.FieldType.Handle = fTypeInfo;
  end
  else
  begin
    Result := False;
  end;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
begin
  inherited Create;
  fTypes := TArray.CreateArray<PTypeInfo>(types);
end;

function THasParameterTypesFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := member.AsMethod.GetParameters;
  Result := Length(parameters) = Length(fTypes);
  if Result then
  for i := 0 to Length(parameters) - 1 do
  begin
    if parameters[i].ParamType.Handle <> fTypes[i] then  // IsAssignableFrom
    begin
      Result := False;
      Break;
    end;
  end;
end;

{ THasParameterFlagsFilter }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
    begin
      if parameter.Flags * fFlags <> [] then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.Accept(const member: T): Boolean;
begin
  if member.IsProperty then
  begin
    Result := member.AsProperty.IsWritable;
  end
  else if member.IsMethod then
  begin
    Result := not (member.AsMethod.MethodKind in [mkClassConstructor, mkClassDestructor]);
  end
  else
  begin
    Result := True;
  end;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
  inherited Create;
  fMemberClass := memberClass;
end;

function TMemberTypeFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.InheritsFrom(fMemberClass);
end;

{ TConstructorFilter }

function TConstructorFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsConstructor;
end;

{ TInstanceMethodFilter }

function TInstanceMethodFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsMethod and not member.AsMethod.IsClassMethod;
end;

{$REGION 'TFiltersBase<T>'}

class function TFiltersBase<T>.HasAttribute(
  attributeClass: TAttributeClass): TSpecification<T>;
begin
  Result := THasAttributeFilter<T>.Create(attributeClass);
end;

class function TFiltersBase<T>.HasParameterTypes(
  const types: array of PTypeInfo): TSpecification<T>;
begin
  Result := THasParameterTypesFilter<T>.Create(types);
end;

class function TFiltersBase<T>.HasParameterFlags(
  const flags: TParamFlags): TSpecification<T>;
begin
  Result := THasParameterFlagsFilter<T>.Create(flags);
end;

class function TFiltersBase<T>.IsNamed(const name: string): TSpecification<T>;
begin
  Result := TNameFilter<T>.Create(name);
end;

class function TFiltersBase<T>.IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.IsTypeOf<TType>: TSpecification<T>;
begin
  Result := IsTypeOf(TypeInfo(TType));
end;

class function TFiltersBase<T>.IsConstructor: TSpecification<T>;
begin
  Result := TConstructorFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInstanceMethod: TSpecification<T>;
begin
  Result := TInstanceMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInvokable: TSpecification<T>;
begin
  Result := TInvokableFilter<T>.Create;
end;

{$ENDREGION}

end.
