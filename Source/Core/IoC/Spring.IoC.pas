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

unit Spring.IoC;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.IoC.Core,
  Spring.IoC.Registration;

type
  TValue = Rtti.TValue;

  /// <summary>
  /// Represents an Inversion of Control (IoC) container.
  /// </summary>
  /// <remarks>
  /// Neither normal interfaces nor generic interfaces have a guid identifier,
  /// so we can not determine whether an object supports a non-guid interface,
  /// furthermore, we can not safely cast an object to a non-guid interface
  /// variable. For these reasons, it's a pity that the two kinds of
  /// interfaces are not yet supported by the Spring IoC container.
  /// If you have a good idea, please drop me an email:
  /// baoquan.zuo[at]gmail.com (Paul). Thanks!
  /// </remarks>
  TContainer = class(TInterfaceBase, IContainerContext, IInterface)
  private
    fRegistry: IComponentRegistry;
    fBuilder: IComponentBuilder;
    fServiceResolver: IServiceResolver;
    fDependencyResolver: IDependencyResolver;
    fInjectionFactory: IInjectionFactory;
    fRegistrationManager: TRegistrationManager;
    function GetComponentRegistry: IComponentRegistry;
  protected
    { Implements IContainerContext }
    function GetDependencyResolver: IDependencyResolver;
    function GetInjectionFactory: IInjectionFactory;
    function CreateLifetimeManager(model: TComponentModel): ILifetimeManager;
    property ComponentRegistry: IComponentRegistry read GetComponentRegistry;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
  protected
    procedure InitializeInspectors; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterComponent<TComponentType: class>: TRegistration<TComponentType>; overload;
    function RegisterComponent(componentType: PTypeInfo): TRegistration; overload;

//    function RegisterInstance<T>(instance: T): TContainer;
//    function RegisterDecorations<TServiceType>(const decorationClasses: array of TClass): TContainer;

    procedure Build;

    function Resolve<T>: T; overload;
    function Resolve<T>(const name: string): T; overload;
    function Resolve(typeInfo: PTypeInfo): TValue; overload;
    function Resolve(const name: string): TValue; overload;

    function ResolveAll<TServiceType>: TArray<TServiceType>; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;

    { Experimental Release Methods }
    procedure Release(instance: TObject); overload;
    procedure Release(instance: IInterface); overload;
  end;

  EContainerException = Spring.IoC.Core.EContainerException;
  ERegistrationException = Spring.IoC.Core.ERegistrationException;
  EResolveException = Spring.IoC.Core.EResolveException;
  EUnsatisfiedDependencyException = Spring.IoC.Core.EUnsatisfiedDependencyException;
  ECircularDependencyException = Spring.IoC.Core.ECircularDependencyException;
  EActivatorException = Spring.IoC.Core.EActivatorException;

implementation

uses
  Spring.ResourceStrings,
  Spring.IoC.Builder,
  Spring.IoC.LifetimeManager,
  Spring.IoC.Injection,
  Spring.IoC.Resolvers,
  Spring.Core.ResourceStrings;

{$REGION 'TContainer'}

constructor TContainer.Create;
begin
  inherited Create;
  fRegistry := TComponentRegistry.Create(Self);
  fBuilder := TComponentBuilder.Create(Self, fRegistry);
  fServiceResolver := TServiceResolver.Create(Self, fRegistry);
  fDependencyResolver := TDependencyResolver.Create(Self, fRegistry);
  fInjectionFactory := TInjectionFactory.Create;
  fRegistrationManager := TRegistrationManager.Create(fRegistry);
  InitializeInspectors;
end;

destructor TContainer.Destroy;
begin
  fRegistrationManager.Free;
  fBuilder.ClearInspectors;
  fRegistry.UnregisterAll;
  inherited Destroy;
end;

procedure TContainer.Build;
begin
  fBuilder.BuildAll;
end;

procedure TContainer.InitializeInspectors;
var
  inspectors: TArray<IBuilderInspector>;
  inspector: IBuilderInspector;
begin
  inspectors := TArray<IBuilderInspector>.Create(
    TLifetimeInspector.Create,
    TComponentActivatorInspector.Create,
//    TImplementsAttributeInspector.Create,
    TInjectionTargetInspector.Create,
    TConstructorInspector.Create,
    TPropertyInspector.Create,
    TMethodInspector.Create,
    TFieldInspector.Create
  );
  for inspector in inspectors do
  begin
    fBuilder.AddInspector(inspector);
  end;
end;

function TContainer.CreateLifetimeManager(
  model: TComponentModel): ILifetimeManager;
begin
  TArgument.CheckNotNull(model, 'model');
  case model.LifetimeType of
    ltSingleton:
    begin
      Result := TSingletonLifetimeManager.Create(model);
    end;
    ltTransient:
    begin
      Result := TTransientLifetimeManager.Create(model);
    end;
//    ltPerThread:
//    begin
//
//    end;
//    ltPooled:
//    begin
//
//    end;
    else
    begin
      raise ERegistrationException.CreateRes(@SUnexpectedLifetimeType);
    end;
  end;
end;

function TContainer.GetComponentRegistry: IComponentRegistry;
begin
  Result := fRegistry;
end;

function TContainer.GetDependencyResolver: IDependencyResolver;
begin
  Result := fDependencyResolver;
end;

function TContainer.GetInjectionFactory: IInjectionFactory;
begin
  Result := fInjectionFactory;
end;

function TContainer.RegisterComponent<TComponentType>: TRegistration<TComponentType>;
begin
  Result := fRegistrationManager.RegisterComponent<TComponentType>;
end;

function TContainer.RegisterComponent(componentType: PTypeInfo): TRegistration;
begin
  Result := fRegistrationManager.RegisterComponent(componentType);
end;

function TContainer.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := fRegistry.HasService(serviceType);
end;

function TContainer.HasService(const name: string): Boolean;
begin
  Result := fRegistry.HasService(name);
end;

function TContainer.Resolve<T>: T;
var
  value: TValue;
begin
  value := Resolve(TypeInfo(T));
  Result := value.AsType<T>;
end;

function TContainer.Resolve<T>(const name: string): T;
var
  value: TValue;
begin
  value := Resolve(name);
  Result := value.AsType<T>;
end;

function TContainer.Resolve(typeInfo: PTypeInfo): TValue;
var
  model: TComponentModel;
begin
  // TODO: How to support bootstrap
  if not fRegistry.HasService(typeInfo) and (typeInfo.Kind = tkClass) then
  begin
    RegisterComponent(typeInfo).Implements(typeInfo);
    model := fRegistry.FindOne(typeInfo);
    fBuilder.Build(model);
    Result := fServiceResolver.Resolve(model.GetServiceName(typeInfo));
  end
  else
  begin
    Result := fServiceResolver.Resolve(typeInfo);
  end;
end;

function TContainer.Resolve(const name: string): TValue;
begin
  Result := fServiceResolver.Resolve(name);
end;

function TContainer.ResolveAll<TServiceType>: TArray<TServiceType>;
var
  serviceType: PTypeInfo;
  models: IList<TComponentModel>;
  model: TComponentModel;
  value: TValue;
  i: Integer;
begin
  serviceType := TypeInfo(TServiceType);
  models := fRegistry.FindAll(serviceType).ToList;
  SetLength(Result, models.Count);
  for i := 0 to models.Count - 1 do
  begin
    model := models[i];
    value := Resolve(model.GetServiceName(serviceType));
    Result[i] := value.AsType<TServiceType>;
  end;
end;

function TContainer.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
begin
  Result := fServiceResolver.ResolveAll(serviceType);
end;

procedure TContainer.Release(instance: TObject);
var
  model: TComponentModel;
begin
  TArgument.CheckNotNull(instance, 'instance');
  model := fRegistry.FindOne(instance.ClassInfo);
  if model = nil then
  begin
    raise EContainerException.CreateRes(@SComponentNotFound);
  end;
  model.LifetimeManager.ReleaseInstance(instance);
end;

procedure TContainer.Release(instance: IInterface);
begin
  TArgument.CheckNotNull(instance, 'instance');
  { TODO: -oOwner -cGeneral : Release instance of IInterface }
end;

{$ENDREGION}


end.
