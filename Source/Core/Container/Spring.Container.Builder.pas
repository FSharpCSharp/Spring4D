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

unit Spring.Container.Builder;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TComponentBuilder = class(TInterfacedObject, IComponentBuilder)
  private
    fKernel: IKernel;
    fInspectors: IList<IBuilderInspector>;
  public
    constructor Create(const kernel: IKernel);
    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    procedure Build(const model: TComponentModel);
    procedure BuildAll;
  end;

  TInspectorBase = class abstract(TInterfacedObject, IBuilderInspector)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); virtual; abstract;
  public
    procedure ProcessModel(const kernel: IKernel; const model: TComponentModel);
  end;

  TInterfaceInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TLifetimeInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TComponentActivatorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TConstructorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TPropertyInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TMethodInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TFieldInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TInjectionTargetInspector = class(TInspectorBase)
  private
    class var
      fHasNoTargetCondition: TPredicate<IInjection>;
    class constructor Create;
  protected
    procedure CheckConstructorInjections(const kernel: IKernel; const model: TComponentModel);
    procedure CheckMethodInjections(const kernel: IKernel; const model: TComponentModel);
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

implementation

uses
  Rtti,
  TypInfo,
  Spring.Container.Common,
  Spring.Container.ComponentActivator,
  Spring.Container.Injection,
  Spring.Container.LifetimeManager,
  Spring.Container.ResourceStrings,
  Spring.Helpers,
  Spring.Reflection;


{$REGION 'TComponentBuilder'}

constructor TComponentBuilder.Create(const kernel: IKernel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  inherited Create;
  fKernel := kernel;
  fInspectors := TCollections.CreateInterfaceList<IBuilderInspector>;
end;

procedure TComponentBuilder.AddInspector(const inspector: IBuilderInspector);
begin
  Guard.CheckNotNull(inspector, 'inspector');
  fInspectors.Add(inspector);
end;

procedure TComponentBuilder.RemoveInspector(const inspector: IBuilderInspector);
begin
  Guard.CheckNotNull(inspector, 'inspector');
  fInspectors.Remove(inspector);
end;

procedure TComponentBuilder.ClearInspectors;
begin
  fInspectors.Clear;
end;

procedure TComponentBuilder.Build(const model: TComponentModel);
var
  inspector: IBuilderInspector;
begin
  for inspector in fInspectors do
    inspector.ProcessModel(fKernel, model);
end;

procedure TComponentBuilder.BuildAll;
var
  model: TComponentModel;
begin
  for model in fKernel.ComponentRegistry.FindAll do
    Build(model);
end;

{$ENDREGION}


{$REGION 'TInspectorBase'}

procedure TInspectorBase.ProcessModel(
  const kernel: IKernel; const model: TComponentModel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  Guard.CheckNotNull(model, 'model');
  DoProcessModel(kernel, model);
end;

{$ENDREGION}


{$REGION 'TLifetimeInspector'}

procedure TLifetimeInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);

  function CreateLifetimeManager(const model: TComponentModel): ILifetimeManager;
  const
    LifetimeManagerClasses: array[TLifetimeType] of TLifetimeManagerClass = (
      nil,
      TSingletonLifetimeManager,
      TTransientLifetimeManager,
      TSingletonPerThreadLifetimeManager,
      TPooledLifetimeManager,
      nil
    );
  begin
    if Assigned(LifetimeManagerClasses[model.LifetimeType]) then
      Result := LifetimeManagerClasses[model.LifetimeType].Create(model)
    else
      raise ERegistrationException.CreateRes(@SUnexpectedLifetimeType);
  end;

var
  attribute: LifetimeAttributeBase;
begin
  if Assigned(model.LifetimeManager) then
  begin
    model.LifetimeType := TLifetimeType.Custom;
    Exit;
  end;
  if model.LifetimeType = TLifetimeType.Unknown then
  begin
    if model.ComponentType.TryGetCustomAttribute<LifetimeAttributeBase>(attribute) then
    begin
      model.LifetimeType := attribute.LifetimeType;
{$WARN SYMBOL_EXPERIMENTAL OFF}
      if attribute is PooledAttribute then
      begin
        model.MinPoolsize := PooledAttribute(attribute).MinPoolsize;
        model.MaxPoolsize := PooledAttribute(attribute).MaxPoolsize;
      end;
{$WARN SYMBOL_EXPERIMENTAL ON}
    end
    else
      model.LifetimeType := TLifetimeType.Transient;
  end;
  model.LifetimeManager := CreateLifetimeManager(model);
end;

{$ENDREGION}


{$REGION 'TConstructorInspector'}

procedure TConstructorInspector.DoProcessModel(
  const kernel: IKernel; const model: TComponentModel);
var
  predicate: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
  arguments: TArray<TValue>;
  attribute: InjectAttribute;
  i: Integer;
begin
  if not model.ConstructorInjections.IsEmpty then Exit;  // TEMP
  predicate := TMethodFilters.IsConstructor and
    not TMethodFilters.HasParameterFlags([pfVar, pfOut]);
  for method in model.ComponentType.Methods.Where(predicate) do
  begin
    injection := kernel.InjectionFactory.CreateConstructorInjection;
    injection.Initialize(method);
    parameters := method.GetParameters;
    SetLength(arguments, Length(parameters));
    for i := 0 to High(parameters) do
    begin
      parameter := parameters[i];
      if parameter.TryGetCustomAttribute<InjectAttribute>(attribute) and attribute.HasValue then
        arguments[i] := attribute.Value
      else
        arguments[i] := nil;
    end;
    injection.InitializeArguments(arguments);
    model.ConstructorInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TMethodInspector'}

procedure TMethodInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
var
  condition: TPredicate<TRttiMethod>;
  method: TRttiMethod;
  injection: IInjection;
  injectionExists: Boolean;
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
  arguments: TArray<TValue>;
  attribute: InjectAttribute;
  i: Integer;
begin
  condition := TMethodFilters.IsInstanceMethod and
    TMethodFilters.HasAttribute(InjectAttribute) and
    not TMethodFilters.HasParameterFlags([pfOut, pfVar]) and
    not TMethodFilters.IsConstructor;
  for method in model.ComponentType.Methods.Where(condition) do
  begin
    injectionExists := model.MethodInjections.TryGetFirst(injection,
      TInjectionFilters.ContainsMember(method));
    if not injectionExists then
      injection := kernel.InjectionFactory.CreateMethodInjection(method.Name);
    injection.Initialize(method);
    parameters := method.GetParameters;
    SetLength(arguments, Length(parameters));
    for i := 0 to High(parameters) do
    begin
      parameter := parameters[i];
      if parameter.TryGetCustomAttribute<InjectAttribute>(attribute) and attribute.HasValue then
        arguments[i] := attribute.Value
      else
        arguments[i] := nil;
    end;
    injection.InitializeArguments(arguments);
    if not injectionExists then
      model.MethodInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TPropertyInspector'}

procedure TPropertyInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
var
  condition: TPredicate<TRttiProperty>;
  propertyMember: TRttiProperty;
  injection: IInjection;
  injectionExists: Boolean;
  attribute: InjectAttribute;
begin
  condition := TPropertyFilters.IsInvokable and
    TPropertyFilters.HasAttribute(InjectAttribute);
  for propertyMember in model.ComponentType.Properties.Where(condition) do
  begin
    injectionExists := model.PropertyInjections.TryGetFirst(injection,
      TInjectionFilters.ContainsMember(propertyMember));
    if not injectionExists then
      injection := kernel.InjectionFactory.CreatePropertyInjection(propertyMember.Name);
    injection.Initialize(propertyMember);
    if propertyMember.TryGetCustomAttribute<InjectAttribute>(attribute)
      and attribute.HasValue then
      injection.InitializeArguments([attribute.Value]);
    if not injectionExists then
      model.PropertyInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TFieldInspector'}

procedure TFieldInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
var
  condition: TPredicate<TRttiField>;
  field: TRttiField;
  injection: IInjection;
  injectionExists: Boolean;
  attribute: InjectAttribute;
begin
  condition := TFieldFilters.HasAttribute(InjectAttribute);
  for field in model.ComponentType.Fields.Where(condition) do
  begin
    injectionExists := model.FieldInjections.TryGetFirst(injection,
      TInjectionFilters.ContainsMember(field));
    if not injectionExists then
      injection := kernel.InjectionFactory.CreateFieldInjection(field.Name);
    injection.Initialize(field);
    if field.TryGetCustomAttribute<InjectAttribute>(attribute) and attribute.HasValue then
      injection.InitializeArguments([attribute.Value]);
    if not injectionExists then
      model.FieldInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TComponentActivatorInspector'}

procedure TComponentActivatorInspector.DoProcessModel(
  const kernel: IKernel; const model: TComponentModel);
begin
  if not Assigned(model.ComponentActivator) then
    if not Assigned(model.ActivatorDelegate) then
      model.ComponentActivator := TReflectionComponentActivator.Create(kernel, model)
    else
      model.ComponentActivator := TDelegateComponentActivator.Create(kernel, model);
end;

{$ENDREGION}


{$REGION 'TInjectionTargetInspector'}

class constructor TInjectionTargetInspector.Create;
begin
  fHasNoTargetCondition :=
    function(const value: IInjection): Boolean
    begin
      Result := not value.HasTarget;
    end;
end;

procedure TInjectionTargetInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
begin
  CheckConstructorInjections(kernel, model);
  CheckMethodInjections(kernel, model);
end;

procedure TInjectionTargetInspector.CheckConstructorInjections(
  const kernel: IKernel; const model: TComponentModel);
var
  filter: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  for injection in model.ConstructorInjections.Where(fHasNoTargetCondition) do
  begin
    filter := TMethodFilters.IsConstructor and
      TInjectionFilters.IsInjectableMethod(kernel, model, injection);
    method := model.ComponentType.Methods.FirstOrDefault(filter);
    if not Assigned(method) then
      raise EBuilderException.CreateRes(@SUnresovableInjection);
    injection.Initialize(method);
  end;
end;

procedure TInjectionTargetInspector.CheckMethodInjections(
  const kernel: IKernel; const model: TComponentModel);
var
  filter: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  for injection in model.MethodInjections.Where(fHasNoTargetCondition) do
  begin
    filter := TMethodFilters.IsInstanceMethod and
      TMethodFilters.IsNamed(injection.TargetName) and
      TInjectionFilters.IsInjectableMethod(kernel, model, injection);
    method := model.ComponentType.Methods.FirstOrDefault(filter);
    if not Assigned(method) then
      raise EBuilderException.CreateRes(@SUnresovableInjection);
    injection.Initialize(method);
  end;
end;

{$ENDREGION}


{$REGION 'TInterfaceInspector'}

procedure TInterfaceInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
var
  attributes: TArray<ImplementsAttribute>;
  attribute: ImplementsAttribute;
  services: IEnumerable<TRttiInterfaceType>;
  service: TRttiInterfaceType;
begin
  if not model.Services.IsEmpty then Exit;
  if model.ComponentType.IsRecord and not model.HasService(model.ComponentTypeInfo) then
    kernel.ComponentRegistry.RegisterService(model, model.ComponentTypeInfo)
  else
  begin
    attributes := model.ComponentType.GetCustomAttributes<ImplementsAttribute>;
    for attribute in attributes do
      kernel.ComponentRegistry.RegisterService(model, attribute.ServiceType, attribute.Name);

    services := model.ComponentType.GetInterfaces;
    if Assigned(services) then
      for service in services do
        if Assigned(service.BaseType) and not model.HasService(service.Handle) then
        begin
          kernel.ComponentRegistry.RegisterService(model, service.Handle,
            service.DefaultName + '@' + model.ComponentType.DefaultName);
          kernel.ComponentRegistry.RegisterDefault(model, service.Handle);
        end;
    if TType.IsDelegate(model.ComponentTypeInfo)
      and not model.HasService(model.ComponentType.Handle) then
      kernel.ComponentRegistry.RegisterService(model, model.ComponentType.Handle);

    if model.Services.IsEmpty then
      kernel.ComponentRegistry.RegisterService(model, model.ComponentType.Handle);
  end;
end;

{$ENDREGION}


end.
