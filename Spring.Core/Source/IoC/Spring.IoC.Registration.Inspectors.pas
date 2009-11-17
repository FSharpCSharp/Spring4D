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

unit Spring.IoC.Registration.Inspectors;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Spring.System,
  Spring.Collections,
  Spring.IoC.Core,
  Spring.IoC.Registration;

type
  TRegistrationInspectorBase = class abstract(TInterfacedObject, IRegistrationInspector)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); virtual; abstract;
  public
    procedure ProcessModel(const context: IContainerContext; model: TComponentModel);
  end;

  TLifetimeInspector = class(TRegistrationInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TComponentActivatorInspector = class(TRegistrationInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TConstructorsInspector = class(TRegistrationInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TPropertiesInspector = class(TRegistrationInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TMethodsInspector = class(TRegistrationInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TFieldsInspector = class(TRegistrationInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;


implementation

uses
  Spring.IoC.Injection,
  Spring.IoC.ComponentActivator,
  Spring.Helpers;

{$REGION 'TInspectorBase'}

procedure TRegistrationInspectorBase.ProcessModel(
  const context: IContainerContext; model: TComponentModel);
begin
  TArgument.CheckNotNull(context, 'context');
  TArgument.CheckNotNull(model, 'model');
  DoProcessModel(context, model);
end;

{$ENDREGION}


{$REGION 'TLifetimeInspector'}

procedure TLifetimeInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  attribute: TLifetimeAttributeBase;
begin
  if model.LifetimeManager <> nil then
  begin
    model.LifetimeType := ltCustom;
    Exit;
  end;
  if model.LifetimeType = ltUnknown then
  begin
    if model.ComponentType.TryGetCustomAttribute<TLifetimeAttributeBase>(attribute) then
    begin
      model.LifetimeType := attribute.LifetimeType;
    end
    else
    begin
      model.LifetimeType := ltTransient;
    end;
  end;
  model.LifetimeManager := context.CreateLifetimeManager(model);
end;

{$ENDREGION}


{$REGION 'TConstructorsInspector'}

function CanInject(const parameters: TArray<TRttiParameter>): Boolean;
var
  parameter: TRttiParameter;
begin
  Result := True;
  for parameter in parameters do
  begin
    if not parameter.ParamType.IsClassOrInterface or
      (parameter.Flags * [pfVar, pfOut] <> []) or (parameter.ParamType.Handle = nil) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TConstructorsInspector.DoProcessModel(
  const context: IContainerContext; model: TComponentModel);
var
  constructorCandidate: IInjection;
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
begin
  for method in model.ComponentType.GetMethods do
  begin
    if method.IsConstructor and method.IsPublic then
    begin
      parameters := method.GetParameters;
      if not CanInject(parameters) then
      begin
        Continue;
      end;
      constructorCandidate := TConstructorInjection.Create(model, method);
      model.Constructors.Add(constructorCandidate);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TPropertiesInspector'}

procedure TPropertiesInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  propertyMember: TRttiProperty;
  injectableMember: IInjection;
begin
  for propertyMember in model.ComponentType.GetProperties do
  begin
    if propertyMember.IsWritable and propertyMember.PropertyType.IsClassOrInterface and
      propertyMember.HasCustomAttribute<InjectionAttribute> then
    begin
      injectableMember := TPropertyInjection.Create(model, propertyMember);
      model.Properties.Add(injectableMember);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TMethodsInspector'}

procedure TMethodsInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
  injection: IInjection;
begin
  for method in model.ComponentType.GetMethods do
  begin
    if not method.IsClassMethod and method.HasCustomAttribute<InjectionAttribute> then
    begin
      parameters := method.GetParameters;
      if not CanInject(parameters) then
      begin
        Continue;
      end;
      injection := TMethodInjection.Create(model, method);
      model.Methods.Add(injection);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TFieldsInspector'}

procedure TFieldsInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  field: TRttiField;
  injection: IInjection;
begin
  for field in model.ComponentType.GetFields do
  begin
    if field.FieldType.IsClassOrInterface and
      field.HasCustomAttribute<InjectionAttribute> then
    begin
      injection := TFieldInjection.Create(model, field);
      model.Fields.Add(injection);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TComponentActivatorInspector'}

procedure TComponentActivatorInspector.DoProcessModel(
  const context: IContainerContext; model: TComponentModel);
begin
  if model.ComponentActivator = nil then
  begin
    model.ComponentActivator := TDefaultComponentActivator.Create(context.DependencyResolver);
  end;
end;

{$ENDREGION}

end.
