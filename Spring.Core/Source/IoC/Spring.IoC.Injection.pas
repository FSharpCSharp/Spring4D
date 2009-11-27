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
    fModel: TComponentModel;  // Consider remove the dependency of TComponentModel
    fTarget: TRttiMember;
    fDependencies: TArray<TRttiType>;
    function GetDependencyCount: Integer;
    function GetTarget: TRttiMember;
    function GetModel: TComponentModel;
    function GetHasTarget: Boolean;
  protected
    procedure Validate(target: TRttiMember); virtual;
    procedure DoInject(instance: TObject; const arguments: array of TValue); virtual; abstract;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); virtual; abstract;
  public
    constructor Create(model: TComponentModel);
    procedure Initialize(target: TRttiMember); virtual;
    procedure Inject(instance: TObject; const arguments: array of TValue);
    function GetDependencies: TArray<TRttiType>;
    property Target: TRttiMember read GetTarget;
    property HasTarget: Boolean read GetHasTarget;
    property DependencyCount: Integer read GetDependencyCount;
    property Model: TComponentModel read GetModel;
  end;

  TMemberInjectionBase = class abstract(TInjectionBase)
  protected
//    function GetMember: TRttiMember;
  end;

  TConstructorInjection = class(TMemberInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  end;

  TPropertyInjection = class(TMemberInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  end;

  TMethodInjection = class(TMemberInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  end;

  TFieldInjection = class(TMemberInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  end;

  TInjectionFactory = class(TInterfacedObject, IInjectionFactory)
  public
    function CreateConstructorInjection(model: TComponentModel): IInjection;
    function CreatePropertyInjection(model: TComponentModel): IInjection;
    function CreateMethodInjection(model: TComponentModel): IInjection;
    function CreateFieldInjection(model: TComponentModel): IInjection;
  end;

implementation

uses
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings,
  Spring.Helpers;


{$REGION 'TInjectionBase'}

constructor TInjectionBase.Create(model: TComponentModel);
begin
  TArgument.CheckNotNull(model, 'model');
  inherited Create;
  fModel := model;
end;

procedure TInjectionBase.Initialize(target: TRttiMember);
begin
  TArgument.CheckNotNull(target, 'target');
  Validate(target);
  fTarget := target;
  InitializeDependencies(fDependencies);
end;

procedure TInjectionBase.Validate(target: TRttiMember);
begin
end;

procedure TInjectionBase.Inject(instance: TObject;
  const arguments: array of TValue);
begin
  TArgument.CheckNotNull(instance, 'instance');
  if fTarget = nil then
  begin
    raise EInjectionException.CreateRes(@SInjectionTargetNeeded);
  end;
  DoInject(instance, arguments);
end;

function TInjectionBase.GetDependencies: TArray<TRttiType>;
begin
  Result := fDependencies;
end;

function TInjectionBase.GetTarget: TRttiMember;
begin
  Result := fTarget;
end;

function TInjectionBase.GetHasTarget: Boolean;
begin
  Result := fTarget <> nil;
end;

function TInjectionBase.GetDependencyCount: Integer;
begin
  Result := Length(fDependencies);
end;

function TInjectionBase.GetModel: TComponentModel;
begin
  Result := fModel;
end;

{$ENDREGION}


{$REGION 'TConstructorInjection'}

procedure TConstructorInjection.Validate(target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsConstructor then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TConstructorInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := Target.AsMethod.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(parameters) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
end;

procedure TConstructorInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  Target.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TPropertyInjection'}

procedure TPropertyInjection.Validate(target: TRttiMember);
begin
  inherited Validate(target);
  if not target.IsProperty then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TPropertyInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
begin
  dependencies := TArray<TRttiType>.Create(Target.AsProperty.PropertyType);
end;

procedure TPropertyInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  TArgument.CheckTrue(Length(arguments) = 1, SUnexpectedArgumentLength);
  Target.AsProperty.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TMethodInjection'}

procedure TMethodInjection.Validate(target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsMethod then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TMethodInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := Target.AsMethod.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(parameters) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
end;

procedure TMethodInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  Target.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TFieldInjection'}

procedure TFieldInjection.Validate(target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsField then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TFieldInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
begin
  dependencies := TArray<TRttiType>.Create(Target.AsField.FieldType);
end;

procedure TFieldInjection.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  TArgument.CheckTrue(Length(arguments) = 1, SUnexpectedArgumentLength);
  Target.AsField.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TInjectionFactory'}

function TInjectionFactory.CreateConstructorInjection(
  model: TComponentModel): IInjection;
begin
  Result := TConstructorInjection.Create(model);
end;

function TInjectionFactory.CreatePropertyInjection(
  model: TComponentModel): IInjection;
begin
  Result := TPropertyInjection.Create(model);
end;

function TInjectionFactory.CreateMethodInjection(
  model: TComponentModel): IInjection;
begin
  Result := TMethodInjection.Create(model);
end;

function TInjectionFactory.CreateFieldInjection(
  model: TComponentModel): IInjection;
begin
  Result := TFieldInjection.Create(model);
end;

{$ENDREGION}

end.
