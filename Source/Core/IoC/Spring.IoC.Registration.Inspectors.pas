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
  TInspectorBase = class abstract(TInterfacedObject, IServiceInspector)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); virtual; abstract;
  public
    procedure ProcessModel(const context: IContainerContext; model: TComponentModel);
  end;

  TLifetimeInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TComponentActivatorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TConstructorsInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TPropertiesInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TMethodsInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TFieldsInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;


implementation

uses
  Spring.IoC.Injection,
  Spring.IoC.ComponentActivator,
  Spring.Helpers;

{$REGION 'TInspectorBase'}

procedure TInspectorBase.ProcessModel(
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
  parameter: TRttiParameter;
  parameters: TArray<TRttiParameter>;
  dependencies: TArray<TDependency>;
  dependency: TDependency;
  i: Integer;
begin
  for method in model.ComponentType.GetMethods do
  begin
//    if fInjectionFactory.CanInject(method) then
    if method.IsConstructor and method.IsPublic then
    begin
      parameters := method.GetParameters;
      if not CanInject(parameters) then
      begin
        Continue;
      end;
      SetLength(dependencies, Length(parameters));
      for i := 0 to High(parameters) do
      begin
        parameter := parameters[i];
        if parameter.ParamType.IsClassOrInterface then
        begin
          dependency := TDependency.Create(dtService, parameter.ParamType);
        end
        else
        begin
          dependency := TDependency.Create(dtParameter, parameter.ParamType);
        end;
        dependencies[i] := dependency;
      end;
      constructorCandidate := TConstructorInjection.Create(model, method, dependencies);
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
  dependency: TDependency;
  injectableMember: IInjection;
begin
  for propertyMember in model.ComponentType.GetProperties do
  begin
    if propertyMember.IsWritable and propertyMember.PropertyType.IsClassOrInterface and
      propertyMember.HasCustomAttribute<InjectionAttribute> then
    begin
      dependency := TDependency.Create(dtService, propertyMember.PropertyType);
      injectableMember := TPropertyInjection.Create(model, propertyMember, 
        TArray<TDependency>.Create(dependency));
//      injectableMember := fInjectionFactory.CreateProperty(
//        model,
//        propertyMember,
//        TArray<TDependency>.Create(dependency)
//      );
      model.Properties.Add(injectableMember);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TMethodsInspector'}

procedure TMethodsInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  member: IInjection;
  method: TRttiMethod;
  parameter: TRttiParameter;
  parameters: TArray<TRttiParameter>;
  dependencies: TArray<TDependency>;
  dependency: TDependency;
  i: Integer;
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
      SetLength(dependencies, Length(parameters));
      for i := 0 to High(parameters) do
      begin
        parameter := parameters[i];
        if parameter.ParamType.IsClassOrInterface then
        begin
          dependency := TDependency.Create(dtService, parameter.ParamType);
        end
        else
        begin
          dependency := TDependency.Create(dtParameter, parameter.ParamType);
        end;
        dependencies[i] := dependency;
      end;
      member := TMethodInjection.Create(model, method, dependencies);
//      member := fInjectionFactory.CreateMethod(model, method, dependencies);
      model.Methods.Add(member);
    end;
  end;
end;
{$ENDREGION}


{$REGION 'TFieldsInspector'}

procedure TFieldsInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  field: TRttiField;
  dependency: TDependency;
  injection: IInjection;
begin
  for field in model.ComponentType.GetFields do
  begin
    if field.FieldType.IsClassOrInterface and
      field.HasCustomAttribute<InjectionAttribute> then
    begin
      dependency := TDependency.Create(dtService, field.FieldType);
      injection := TFieldInjection.Create(
        model,
        field,
        TArray<TDependency>.Create(dependency)
      );
//      injection := fInjectionFactory.CreateField(
//        model,
//        field,
//        TArray<TDependency>.Create(dependency)
//      );
      model.Properties.Add(injection);
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
