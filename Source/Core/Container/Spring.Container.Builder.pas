{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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

unit Spring.Container.Builder;

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TComponentBuilder = class(TInterfacedObject, IComponentBuilder)
  private
    fKernel: TKernel;
    fOnBuild: IInvokableNotifyEvent<TComponentModel>;
    fInspectors: IList<IBuilderInspector>;
    function GetOnBuild: INotifyEvent<TComponentModel>;
  public
    constructor Create(const kernel: TKernel);

    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    procedure Build(const model: TComponentModel);
    procedure BuildAll;

    property OnBuild: INotifyEvent<TComponentModel> read GetOnBuild;
  end;

  TInterfaceInspector = class(TInterfacedObject, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TLifetimeInspector = class(TInterfacedObject, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TProviderInspector = class(TInterfacedObject, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TMemberInspector = class(TInterfacedObject)
  protected
    procedure HandleInjectAttribute(const target: TRttiNamedObject;
      var dependency: ITarget; out argument: TValue);
  end;

  TConstructorInspector = class(TMemberInspector, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TPropertyInspector = class(TMemberInspector, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TMethodInspector = class(TMemberInspector, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TFieldInspector = class(TMemberInspector, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TInjectionTargetInspector = class(TInterfacedObject, IBuilderInspector)
  private
    class var
      fHasNoTargetCondition: Predicate<IInjection>;
    class constructor Create;
  protected
    procedure CheckConstructorInjections(const kernel: TKernel; const model: TComponentModel);
    procedure CheckMethodInjections(const kernel: TKernel; const model: TComponentModel);
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TInterceptorInspector = class(TInterfacedObject, IBuilderInspector)
  public
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

  TAbstractMethodInspector = class(TInterfacedObject, IBuilderInspector)
  protected
    procedure ProcessModel(const kernel: TKernel; const model: TComponentModel);
  end;

implementation

uses
  Classes,
  TypInfo,
  Spring.Container.Common,
  Spring.Container.Injection,
  Spring.Container.LifetimeManager,
  Spring.Container.Providers,
  Spring.Container.ResourceStrings,
  Spring.Events,
  Spring.Reflection;


{$REGION 'TComponentBuilder'}

constructor TComponentBuilder.Create(const kernel: TKernel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  inherited Create;
  fKernel := kernel;
  fInspectors := TCollections.CreateInterfaceList<IBuilderInspector>;
  fOnBuild := TNotifyEventImpl<TComponentModel>.Create;
end;

function TComponentBuilder.GetOnBuild: INotifyEvent<TComponentModel>;
begin
  Result := fOnBuild;
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
  fOnBuild.Invoke(Self, model);
end;

procedure TComponentBuilder.BuildAll;
var
  model: TComponentModel;
begin
  for model in fKernel.Registry.FindAll do
    Build(model);
end;

{$ENDREGION}


{$REGION 'TLifetimeInspector'}

procedure TLifetimeInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);

  function CreateLifetimeManager(const model: TComponentModel): ILifetimeManager;
  const
    LifetimeManagerClasses: array[TLifetimeType] of TLifetimeManagerClass = (
      nil,
      TSingletonLifetimeManager,
      TTransientLifetimeManager,
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
    if model.LifetimeType = TLifetimeType.Unknown then
      model.LifetimeType := TLifetimeType.Custom;
    Exit;
  end;
  if model.LifetimeType = TLifetimeType.Unknown then
  begin
    if model.ComponentType.TryGetCustomAttribute<LifetimeAttributeBase>(attribute) then
    begin
      model.LifetimeType := attribute.LifetimeType;
      if attribute is SingletonAttributeBase then
        model.RefCounting := SingletonAttributeBase(attribute).RefCounting;
      if attribute is PooledAttribute then
      begin
        model.MinPoolsize := PooledAttribute(attribute).MinPoolsize;
        model.MaxPoolsize := PooledAttribute(attribute).MaxPoolsize;
      end;
    end
    else
      model.LifetimeType := TLifetimeType.Transient;
  end;
  model.LifetimeManager := CreateLifetimeManager(model);
end;

{$ENDREGION}


{$REGION 'TMemberInspector'}

procedure TMemberInspector.HandleInjectAttribute(const target: TRttiNamedObject;
  var dependency: ITarget; out argument: TValue);
var
  attribute: InjectAttribute;
  targetType: TRttiType;
begin
  if target.TryGetCustomAttribute<InjectAttribute>(attribute) then
  begin
    argument := attribute.Value;
    if attribute.ServiceType <> nil then
    begin
      if target is TRttiProperty then
        targetType := TRttiProperty(target).PropertyType
      else if target is TRttiField then
        targetType := TRttiField(target).FieldType
      else if target is TRttiParameter then
        targetType := TRttiParameter(target).ParamType
      else
        raise EBuilderException.CreateResFmt(@SUnresovableInjection, [
          dependency.TargetType.TypeName]);
      if TType.IsAssignable(attribute.ServiceType, targetType.Handle) then
      begin
        if attribute.ServiceType <> targetType.Handle then
          targetType := attribute.ServiceType.RttiType;
        dependency := TTarget.Create(target, targetType);
      end
      else
        raise EBuilderException.CreateResFmt(@SUnresovableInjection, [
          dependency.TargetType.TypeName]);
    end;
  end
  else
    argument := TValue.Empty;
end;

{$ENDREGION}


{$REGION 'TConstructorInspector'}

procedure TConstructorInspector.ProcessModel(
  const kernel: TKernel; const model: TComponentModel);
var
  predicate: Predicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
  arguments: TArray<TValue>;
  i: Integer;
begin
  if model.ConstructorInjections.Any then Exit;  // TEMP
  if not model.ComponentType.IsInstance then Exit;
  predicate := TMethodFilters.IsConstructor
    and not TMethodFilters.HasParameterFlags([pfVar, pfArray, pfOut]);
  for method in model.ComponentType.Methods.Where(predicate) do
  begin
    injection := kernel.InjectionBuilder.InjectConstructor(model);
    injection.Initialize(method);
    parameters := method.GetParameters;
    SetLength(arguments, Length(parameters));
    for i := Low(parameters) to High(parameters) do
      HandleInjectAttribute(parameters[i], injection.Dependencies[i], arguments[i]);
    injection.InitializeArguments(arguments);
  end;
end;

{$ENDREGION}


{$REGION 'TMethodInspector'}

procedure TMethodInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);
var
  condition: Predicate<TRttiMethod>;
  method: TRttiMethod;
  injection: IInjection;
  parameters: TArray<TRttiParameter>;
  arguments: TArray<TValue>;
  i: Integer;
begin
  condition := TMethodFilters.IsInstanceMethod
    and TMethodFilters.HasAttribute(InjectAttribute)
    and not TMethodFilters.HasParameterFlags([pfVar, pfArray, pfOut])
    and not TMethodFilters.IsConstructor;
  for method in model.ComponentType.Methods.Where(condition) do
  begin
    if not model.MethodInjections.TryGetFirst(injection,
      TInjectionFilters.ContainsMember(method)) then
      injection := kernel.InjectionBuilder.InjectMethod(model, method.Name);
    injection.Initialize(method);
    parameters := method.GetParameters;
    SetLength(arguments, Length(parameters));
    for i := Low(parameters) to High(parameters) do
      HandleInjectAttribute(parameters[i], injection.Dependencies[i], arguments[i]);
    injection.InitializeArguments(arguments);
  end;
end;

{$ENDREGION}


{$REGION 'TPropertyInspector'}

procedure TPropertyInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);
var
  condition: Predicate<TRttiProperty>;
  prop: TRttiProperty;
  injection: IInjection;
  argument: TValue;
begin
  condition := TPropertyFilters.IsInvokable
    and TPropertyFilters.HasAttribute(InjectAttribute);
  for prop in model.ComponentType.Properties.Where(condition) do
  begin
    if not model.PropertyInjections.TryGetFirst(injection,
      TInjectionFilters.ContainsMember(prop)) then
      injection := kernel.InjectionBuilder.InjectProperty(model, prop.Name);
    injection.Initialize(prop);
    HandleInjectAttribute(prop, injection.Dependencies[0], argument);
    injection.InitializeArguments([argument]);
  end;
end;

{$ENDREGION}


{$REGION 'TFieldInspector'}

procedure TFieldInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);
var
  condition: Predicate<TRttiField>;
  field: TRttiField;
  injection: IInjection;
  argument: TValue;
begin
  condition := TFieldFilters.HasAttribute(InjectAttribute);
  for field in model.ComponentType.Fields.Where(condition) do
  begin
    if not model.FieldInjections.TryGetFirst(injection,
      TInjectionFilters.ContainsMember(field)) then
      injection := kernel.InjectionBuilder.InjectField(model, field.Name);
    injection.Initialize(field);
    HandleInjectAttribute(field, injection.Dependencies[0], argument);
    injection.InitializeArguments([argument]);
  end;
end;

{$ENDREGION}


{$REGION 'TProviderInspector'}

procedure TProviderInspector.ProcessModel(
  const kernel: TKernel; const model: TComponentModel);
begin
  if not Assigned(model.Provider) then
    if model.ComponentType.TypeKind = tkClass then
      model.Provider := TReflectionProvider.Create(kernel, model)
    else
      raise EBuilderException.CreateResFmt(@SRegistrationIncomplete, [model.ComponentType.Name]);
end;

{$ENDREGION}


{$REGION 'TInjectionTargetInspector'}

class constructor TInjectionTargetInspector.Create;
begin
  fHasNoTargetCondition :=
    function(const value: IInjection): Boolean
    begin
      Result := value.Target = nil;
    end;
end;

procedure TInjectionTargetInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);
begin
  CheckConstructorInjections(kernel, model);
  CheckMethodInjections(kernel, model);
end;

procedure TInjectionTargetInspector.CheckConstructorInjections(
  const kernel: TKernel; const model: TComponentModel);
var
  filter: Predicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  for injection in model.ConstructorInjections.Where(fHasNoTargetCondition) do
  begin
    filter := TMethodFilters.IsConstructor
      and TInjectionFilters.IsInjectableMethod(kernel, Model, injection.Arguments);
    method := model.ComponentType.Methods.FirstOrDefault(filter);
    if not Assigned(method) then
      raise EBuilderException.CreateResFmt(@SUnresovableInjection, [
        model.ComponentType.DefaultName]);
    injection.Initialize(method);
  end;
end;

procedure TInjectionTargetInspector.CheckMethodInjections(
  const kernel: TKernel; const model: TComponentModel);
var
  filter: Predicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  for injection in model.MethodInjections.Where(fHasNoTargetCondition) do
  begin
    filter := TMethodFilters.IsInstanceMethod
      and TMethodFilters.IsNamed(injection.TargetName)
      and TInjectionFilters.IsInjectableMethod(kernel, Model, injection.Arguments);
    method := model.ComponentType.Methods.FirstOrDefault(filter);
    if not Assigned(method) then
      raise EBuilderException.CreateResFmt(@SUnresovableInjection, [
        model.ComponentType.DefaultName]);
    injection.Initialize(method);
  end;
end;

{$ENDREGION}


{$REGION 'TInterfaceInspector'}

procedure TInterfaceInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);
var
  attributes: TArray<ImplementsAttribute>;
  attribute: ImplementsAttribute;
  services: IEnumerable<TRttiInterfaceType>;
  service: TRttiInterfaceType;
begin
  if model.Services.Any then Exit;
  if model.ComponentType.IsRecord and not model.HasService(model.ComponentType.Handle) then
    kernel.Registry.RegisterService(model, model.ComponentType.Handle)
  else
  begin
    attributes := model.ComponentType.GetCustomAttributes<ImplementsAttribute>;
    for attribute in attributes do
      kernel.Registry.RegisterService(model, attribute.ServiceType, attribute.ServiceName);

    services := model.ComponentType.GetInterfaces.Where(
      function(const interfaceType: TRttiInterfaceType): Boolean
      begin
        Result := (interfaceType.Handle <> TypeInfo(IInterface))
          and (interfaceType.Handle <> TypeInfo(IInterfaceComponentReference));
      end);
    if Assigned(services) then
      for service in services do
        if Assigned(service.BaseType) and not model.HasService(service.Handle) then
        begin
          kernel.Registry.RegisterService(model, service.Handle,
            service.DefaultName + '@' + model.ComponentType.DefaultName);
          kernel.Registry.RegisterDefault(model, service.Handle);
        end;
    if IsMethodReference(model.ComponentType.Handle)
      and not model.HasService(model.ComponentType.Handle) then
      kernel.Registry.RegisterService(model, model.ComponentType.Handle);

    if not model.Services.Any then
      kernel.Registry.RegisterService(model, model.ComponentType.Handle);
  end;
end;

{$ENDREGION}


{$REGION 'TInterceptorInspector'}

procedure TInterceptorInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);
var
  attributes: TArray<InterceptorAttribute>;
  attribute: InterceptorAttribute;
  interceptorRef: TInterceptorReference;
begin
  attributes := model.ComponentType.GetCustomAttributes<InterceptorAttribute>;
  for attribute in attributes do
  begin
    if Assigned(attribute.InterceptorType) then
      interceptorRef := TInterceptorReference.Create(attribute.InterceptorType)
    else
      interceptorRef := TInterceptorReference.Create(attribute.Name);
    if not model.Interceptors.Contains(interceptorRef,
      function(const left, right: TInterceptorReference): Boolean
      begin
        Result := (left.TypeInfo = right.TypeInfo) and (left.Name = right.Name);
      end) then
      model.Interceptors.Add(interceptorRef);
  end;
end;

{$ENDREGION}


{$REGION 'TAbstractMethodInspector'}

procedure TAbstractMethodInspector.ProcessModel(const kernel: TKernel;
  const model: TComponentModel);

  function HasVirtualAbstractMethod(const rttiType: TRttiType): Boolean;
  var
    virtualMethods: IEnumerable<TRttiMethod>;
    virtualMethodsGrouped: IEnumerable<IGrouping<SmallInt,TRttiMethod>>;
  begin
    virtualMethods := rttiType.Methods.Where(
      function(const method: TRttiMethod): Boolean
      begin
        Result := (method.DispatchKind = dkVtable) and (method.VirtualIndex >= 0);
      end);
    virtualMethodsGrouped := TEnumerable.GroupBy<TRttiMethod,SmallInt>(
      virtualMethods,
      function(const method: TRttiMethod): SmallInt
      begin
        Result := method.VirtualIndex;
      end);
    virtualMethods := TEnumerable.Select<IGrouping<SmallInt,TRttiMethod>, TRttiMethod>(
      virtualMethodsGrouped,
      function(const group: IGrouping<SmallInt,TRttiMethod>): TRttiMethod
      begin
        Result := group.First;
      end);
    Result := virtualMethods.Any(
      function(const method: TRttiMethod): Boolean
      begin
        Result := method.IsAbstract;
      end);
  end;

begin
  if model.ComponentType.IsClass
    and HasVirtualAbstractMethod(model.ComponentType) then
    kernel.Logger.Warn(Format('component type %s contains abstract methods', [model.ComponentType.DefaultName]));
end;

{$ENDREGION}


end.
