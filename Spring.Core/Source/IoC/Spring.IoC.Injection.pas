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

unit Spring.IoC.Injection;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.IoC.Core;

type
  TInjectionBase = class abstract(TInterfacedObject, IInjection, IInterface)
  private
    fModel: TComponentModel;
    fMember: TRttiMember;
    fDependencies: TArray<TRttiType>;
    function GetDependencyCount: Integer;
    function GetMemberType: TRttiMember;
    function GetModel: TComponentModel;
  protected
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); virtual; abstract;
    procedure DoInject(instance: TObject; const arguments: array of TValue); virtual; abstract;
  public
    constructor Create(model: TComponentModel; member: TRttiMember);
    procedure Inject(instance: TObject; const arguments: array of TValue);
    function GetDependencies: TArray<TRttiType>;
    property DependencyCount: Integer read GetDependencyCount;
    property MemberType: TRttiMember read GetMemberType;
    property Model: TComponentModel read GetModel;
  end;

  TConstructorInjection = class(TInjectionBase)
  protected
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  public
    constructor Create(model: TComponentModel; constructorMethod: TRttiMethod);
  end;

  TPropertyInjection = class(TInjectionBase)
  protected
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  public
    constructor Create(model: TComponentModel; propertyMember: TRttiProperty);
  end;

  TMethodInjection = class(TInjectionBase)
  protected
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  public
    constructor Create(model: TComponentModel; method: TRttiMethod);
  end;

  TFieldInjection = class(TInjectionBase)
  protected
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  public
    constructor Create(model: TComponentModel; field: TRttiField);
  end;

implementation

uses
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings,
  Spring.Helpers;


{$REGION 'TInjectionBase'}

constructor TInjectionBase.Create(model: TComponentModel;
  member: TRttiMember);
begin
  TArgument.CheckNotNull(model, 'model');
  TArgument.CheckNotNull(member, 'member');

  inherited Create;
  fModel := model;
  fMember := member;
  InitializeDependencies(fDependencies);
end;

procedure TInjectionBase.Inject(instance: TObject;
  const arguments: array of TValue);
begin
  TArgument.CheckNotNull(instance, 'instance');
  DoInject(instance, arguments);
end;

function TInjectionBase.GetDependencies: TArray<TRttiType>;
begin
  Result := fDependencies;
end;

function TInjectionBase.GetDependencyCount: Integer;
begin
  Result := Length(fDependencies);
end;

function TInjectionBase.GetMemberType: TRttiMember;
begin
  Result := fMember;
end;

function TInjectionBase.GetModel: TComponentModel;
begin
  Result := fModel;
end;

{$ENDREGION}


{$REGION 'TConstructorInjection'}

constructor TConstructorInjection.Create(model: TComponentModel;
  constructorMethod: TRttiMethod);
begin
  TArgument.CheckNotNull(constructorMethod, 'constructorMethod');
  TArgument.CheckTrue(constructorMethod.IsConstructor, SMethodMustBeConstructor);

  inherited Create(model, constructorMethod);
end;

procedure TConstructorInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := MemberType.AsMethod.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(parameters) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
end;

procedure TConstructorInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  MemberType.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TPropertyInjection'}

constructor TPropertyInjection.Create(model: TComponentModel;
  propertyMember: TRttiProperty);
begin
  inherited Create(model, propertyMember);
end;

procedure TPropertyInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
begin
  dependencies := TArray<TRttiType>.Create(MemberType.AsProperty.PropertyType);
end;

procedure TPropertyInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  TArgument.CheckTrue(Length(arguments) = 1, SUnexpectedArgumentLength);
  MemberType.AsProperty.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TMethodInjection'}

constructor TMethodInjection.Create(model: TComponentModel;
  method: TRttiMethod);
begin
  inherited Create(model, method);
end;

procedure TMethodInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := MemberType.AsMethod.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(parameters) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
end;

procedure TMethodInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  MemberType.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TFieldInjection'}

constructor TFieldInjection.Create(model: TComponentModel;
  field: TRttiField);
begin
  inherited Create(model, field);
end;

procedure TFieldInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
begin
  dependencies := TArray<TRttiType>.Create(MemberType.AsField.FieldType);
end;

procedure TFieldInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  TArgument.CheckTrue(Length(arguments) = 1, SUnexpectedArgumentLength);
  MemberType.AsField.SetValue(instance, arguments[0]);
end;

{$ENDREGION}

end.
