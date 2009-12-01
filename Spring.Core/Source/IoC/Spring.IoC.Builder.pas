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

unit Spring.IoC.Builder;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Generics.Collections,
  Spring.System,
  Spring.DesignPatterns,
  Spring.IoC.Core;

type
  TComponentBuilder = class(TInterfacedObject, IComponentBuilder)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
    fInspectors: TList<IBuilderInspector>;
  public
    constructor Create(context: IContainerContext; registry: IComponentRegistry);
    destructor Destroy; override;
    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    procedure Build(model: TComponentModel);
    procedure BuildAll;
  end;

  TInspectorBase = class abstract(TInterfacedObject, IBuilderInspector)
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

  // InjectionAttributeInspector
  TConstructorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TPropertyInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TMethodInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TFieldInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TInjectionTargetInspector = class(TInspectorBase)
  private
    class var
      fCHasNoTargetCondition: TPredicate<IInjection>;
    class constructor Create;
  protected
    procedure CheckConstructorInjections(const context: IContainerContext; model: TComponentModel);
    procedure CheckMethodInjections(const context: IContainerContext; model: TComponentModel);
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TInjectableMethodFilter = class(TSpecificationBase<TRttiMethod>)
  private
    fContext: IContainerContext;
    fModel: TComponentModel;
    fInjection: IInjection;
    fArguments: TArray<TValue>;
  public
    constructor Create(const context: IContainerContext; model: TComponentModel;
      const injection: IInjection);
    function IsSatisfiedBy(const method: TRttiMethod): Boolean; override;
  end;

  TInjectionFilters = class
  public
    class function IsInjectableMethod(const context: IContainerContext;
      model: TComponentModel; const injection: IInjection): TSpecification<TRttiMethod>;
  end;

implementation

uses
  Spring.Collections,
  Spring.Reflection,
  Spring.IoC.Injection,
  Spring.IoC.ComponentActivator,
  Spring.Helpers,
  Spring.IoC.ResourceStrings;


{$REGION 'TComponentBuilder'}

constructor TComponentBuilder.Create(
  context: IContainerContext; registry: IComponentRegistry);
begin
  TArgument.CheckNotNull(context, 'context');
  TArgument.CheckNotNull(registry, 'registry');
  inherited Create;
  fContext := context;
  fRegistry := registry;
  fInspectors := TList<IBuilderInspector>.Create;
end;

destructor TComponentBuilder.Destroy;
begin
  fInspectors.Free;
  inherited Destroy;
end;

procedure TComponentBuilder.AddInspector(const inspector: IBuilderInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Add(inspector);
end;

procedure TComponentBuilder.RemoveInspector(const inspector: IBuilderInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Remove(inspector);
end;

procedure TComponentBuilder.ClearInspectors;
begin
  fInspectors.Clear;
end;

procedure TComponentBuilder.Build(model: TComponentModel);
var
  inspector: IBuilderInspector;
begin
  for inspector in fInspectors do
  begin
    inspector.ProcessModel(fContext, model);
  end;
end;

procedure TComponentBuilder.BuildAll;
var
  model: TComponentModel;
begin
  for model in fRegistry.FindAll do
  begin
    Build(model);
  end;
end;

{$ENDREGION}


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


{$REGION 'TConstructorInspector'}

procedure TConstructorInspector.DoProcessModel(
  const context: IContainerContext; model: TComponentModel);
var
  predicate: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
  arguments: TArray<TValue>;
  attribute: InjectionAttribute;
  i: Integer;
begin
  if not model.ConstructorInjections.IsEmpty then Exit;  // TEMP
  predicate := TMethodFilters.IsConstructor and
    not TMethodFilters.HasParameterFlags([pfVar, pfOut]);
  for method in model.ComponentType.GetMethods.Where(predicate) do
  begin
    injection := context.InjectionFactory.CreateConstructorInjection(model);
    injection.Initialize(method);
    parameters := method.GetParameters;
    SetLength(arguments, Length(parameters));
    for i := 0 to High(parameters) do
    begin
      parameter := parameters[i];
      if parameter.TryGetCustomAttribute<InjectionAttribute>(attribute) and attribute.HasValue then
      begin
        arguments[i] := attribute.Value;
      end
      else
      begin
        arguments[i] := TValue.Empty;
      end;
    end;
    model.UpdateInjectionArguments(injection, arguments);
    model.ConstructorInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TMethodInspector'}

procedure TMethodInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  condition: TPredicate<TRttiMethod>;
  method: TRttiMethod;
  injection: IInjection;
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
  arguments: TArray<TValue>;
  attribute: InjectionAttribute;
  i: Integer;
begin
  condition := TMethodFilters.IsInstanceMethod and
    TMethodFilters.HasAttribute(InjectionAttribute) and
    not TMethodFilters.HasParameterFlags([pfOut, pfVar]);
  for method in model.ComponentType.GetMethods.Where(condition) do
  begin
    injection := context.InjectionFactory.CreateMethodInjection(model, method.Name);
    injection.Initialize(method);
    parameters := method.GetParameters;
    SetLength(arguments, Length(parameters));
    for i := 0 to High(parameters) do
    begin
      parameter := parameters[i];
      if parameter.TryGetCustomAttribute<InjectionAttribute>(attribute) and attribute.HasValue then
      begin
        arguments[i] := attribute.Value;
      end
      else
      begin
        arguments[i] := TValue.Empty;
      end;
    end;
    model.UpdateInjectionArguments(injection, arguments);
    model.MethodInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TPropertyInspector'}

procedure TPropertyInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  condition: TPredicate<TRttiProperty>;
  propertyMember: TRttiProperty;
  injection: IInjection;
  attribute: InjectionAttribute;
begin
  condition := TPropertyFilters.IsInvokable and
    TPropertyFilters.HasAttribute(InjectionAttribute);
  for propertyMember in model.ComponentType.GetProperties.Where(condition) do
  begin
    injection := context.InjectionFactory.CreatePropertyInjection(model, propertyMember.Name);
    injection.Initialize(propertyMember);
    if propertyMember.TryGetCustomAttribute<InjectionAttribute>(attribute) and attribute.HasValue then
    begin
      model.UpdateInjectionArguments(injection, [attribute.Value]);
    end;
    model.PropertyInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TFieldInspector'}

procedure TFieldInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  condition: TPredicate<TRttiField>;
  field: TRttiField;
  injection: IInjection;
  attribute: InjectionAttribute;
begin
  condition := TFieldFilters.HasAttribute(InjectionAttribute);
  for field in model.ComponentType.GetFields.Where(condition) do
  begin
    injection := context.InjectionFactory.CreateFieldInjection(model, field.Name);
    injection.Initialize(field);
    if field.TryGetCustomAttribute<InjectionAttribute>(attribute) and attribute.HasValue then
    begin
      model.UpdateInjectionArguments(injection, [attribute.Value]);
    end;
    model.FieldInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TComponentActivatorInspector'}

procedure TComponentActivatorInspector.DoProcessModel(
  const context: IContainerContext; model: TComponentModel);
begin
  if model.ComponentActivator = nil then
  begin
    if not Assigned(model.ActivatorDelegate) then
    begin
      model.ComponentActivator := TReflectionComponentActivator.Create(model, context.DependencyResolver);
    end
    else
    begin
      model.ComponentActivator := TDelegateComponentActivator.Create(model);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TInjectionTargetInspector'}

class constructor TInjectionTargetInspector.Create;
begin
  fCHasNoTargetCondition :=
    function(value: IInjection): Boolean
    begin
      Result := not value.HasTarget;
    end;
end;

procedure TInjectionTargetInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
begin
  CheckConstructorInjections(context, model);
  CheckMethodInjections(context, model);
end;

procedure TInjectionTargetInspector.CheckConstructorInjections(
  const context: IContainerContext; model: TComponentModel);
var
  filter: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  for injection in model.ConstructorInjections.Where(fCHasNoTargetCondition) do
  begin
    filter := TMethodFilters.IsConstructor and
      TInjectionFilters.IsInjectableMethod(context, model, injection);
    method := model.ComponentType.GetMethods.FirstOrDefault(filter);
    if method = nil then
    begin
      raise EBuilderException.CreateRes(@SUnresovableInjection);
    end;
    injection.Initialize(method);
  end;
end;

procedure TInjectionTargetInspector.CheckMethodInjections(
  const context: IContainerContext; model: TComponentModel);
var
  filter: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  for injection in model.MethodInjections.Where(fCHasNoTargetCondition) do
  begin
    filter := TMethodFilters.IsInstanceMethod and
      TMethodFilters.IsNamed(injection.TargetName) and
      TInjectionFilters.IsInjectableMethod(context, model, injection);
    method := model.ComponentType.GetMethods.FirstOrDefault(filter);
    if method = nil then
    begin
      raise EBuilderException.CreateRes(@SUnresovableInjection);
    end;
    injection.Initialize(method);
  end;
end;

{$ENDREGION}


{$REGION 'TInjectionFilters'}

class function TInjectionFilters.IsInjectableMethod(
  const context: IContainerContext; model: TComponentModel;
  const injection: IInjection): TSpecification<TRttiMethod>;
begin
  Result := TInjectableMethodFilter.Create(context, model, injection);
end;

{$ENDREGION}


{$REGION 'TInjectableMethodFilter'}

constructor TInjectableMethodFilter.Create(const context: IContainerContext;
  model: TComponentModel; const injection: IInjection);
begin
  inherited Create;
  fContext := context;
  fModel := model;
  fInjection := injection;
  fArguments := model.GetInjectionArguments(fInjection);
end;

function TInjectableMethodFilter.IsSatisfiedBy(
  const method: TRttiMethod): Boolean;
var
  dependencies: TArray<TRttiType>;
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := method.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(dependencies) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
  Result := fContext.DependencyResolver.CanResolveDependencies(dependencies, fArguments);
end;

{$ENDREGION}

end.
