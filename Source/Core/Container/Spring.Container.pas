{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

{TODO -oOwner -cGeneral : Thread Safety}

unit Spring.Container;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Services,
  Spring.Container.Core,
  Spring.Container.Registration;

type
  TValue = Rtti.TValue;

  /// <summary>
  /// Represents a Dependency Inject Container.
  /// </summary>
  TContainer = class(TInterfaceBase, IContainerContext, IInterface)
  private
    fRegistry: IComponentRegistry;
    fBuilder: IComponentBuilder;
    fServiceResolver: IServiceResolver;
    fDependencyResolver: IDependencyResolver;
    fInjectionFactory: IInjectionFactory;
    fRegistrationManager: TRegistrationManager;
  protected
    { Implements IContainerContext }
    function GetDependencyResolver: IDependencyResolver;
    function GetInjectionFactory: IInjectionFactory;
    function GetComponentRegistry: IComponentRegistry;
    function CreateLifetimeManager(model: TComponentModel): ILifetimeManager;
    property ComponentRegistry: IComponentRegistry read GetComponentRegistry;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
  protected
    procedure InitializeInspectors; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterType<TComponentType: class>: TRegistration<TComponentType>; overload;
    function RegisterType(componentType: PTypeInfo): TRegistration; overload;

    function RegisterComponent<TComponentType: class>: TRegistration<TComponentType>; overload; deprecated 'Use RegisterType';
    function RegisterComponent(componentType: PTypeInfo): TRegistration; overload; deprecated 'Use RegisterType';

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

  TServiceLocatorAdapter = class(TInterfacedObject, IServiceLocator)
  private
    fContainer: TContainer;
  public
    constructor Create(container: TContainer);

    function GetService(serviceType: PTypeInfo): TValue; overload;
    function GetService(serviceType: PTypeInfo; const name: string): TValue; overload;

    function GetAllServices(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;
  end;

{$REGION 'Exceptions'}

  EContainerException = Spring.Container.Core.EContainerException;
  ERegistrationException = Spring.Container.Core.ERegistrationException;
  EResolveException = Spring.Container.Core.EResolveException;
  EUnsatisfiedDependencyException = Spring.Container.Core.EUnsatisfiedDependencyException;
  ECircularDependencyException = Spring.Container.Core.ECircularDependencyException;
  EActivatorException = Spring.Container.Core.EActivatorException;

{$ENDREGION}

function GlobalContainer: TContainer;

implementation

uses
  Spring.Container.Builder,
  Spring.Container.LifetimeManager,
  Spring.Container.Injection,
  Spring.Container.Resolvers,
  Spring.Container.ResourceStrings;

var
  _GlobalContainer: TContainer;
  _GlobalServiceLocator: IServiceLocator;

function GlobalContainer: TContainer;
begin
  Result := _GlobalContainer;
end;


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
    TInterfaceInspector.Create,
    TComponentActivatorInspector.Create,
    TLifetimeInspector.Create,
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
    TLifetimeType.Singleton:
      begin
        Result := TSingletonLifetimeManager.Create(model);
      end;
    TLifetimeType.Transient:
      begin
        Result := TTransientLifetimeManager.Create(model);
      end;
    TLifetimeType.SingletonPerThread:
      begin
        Result := TSingletonPerThreadLifetimeManager.Create(model);
      end;
    TLifetimeType.Pooled:
      begin
        Result := TPooledLifetimeManager.Create(model);
      end;
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

function TContainer.RegisterComponent(componentType: PTypeInfo): TRegistration;
begin
  Result := fRegistrationManager.RegisterComponent(componentType);
end;

function TContainer.RegisterComponent<TComponentType>: TRegistration<TComponentType>;
begin
  Result := fRegistrationManager.RegisterComponent<TComponentType>;
end;

function TContainer.RegisterType<TComponentType>: TRegistration<TComponentType>;
begin
  Result := fRegistrationManager.RegisterComponent<TComponentType>;
end;

function TContainer.RegisterType(componentType: PTypeInfo): TRegistration;
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
    RegisterType(typeInfo).Implements(typeInfo);
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


{$REGION 'TServiceLocatorAdapter'}

constructor TServiceLocatorAdapter.Create(container: TContainer);
begin
  inherited Create;
  fContainer := container;
end;

function TServiceLocatorAdapter.GetService(serviceType: PTypeInfo): TValue;
begin
  Result := fContainer.Resolve(serviceType);
end;

function TServiceLocatorAdapter.GetService(serviceType: PTypeInfo; const name: string): TValue;
begin
  Result := fContainer.Resolve({serviceType, }name);
end;

function TServiceLocatorAdapter.GetAllServices(serviceType: PTypeInfo): TArray<TValue>;
begin
  Result := fContainer.ResolveAll(serviceType);
end;

function TServiceLocatorAdapter.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := fContainer.HasService(serviceType);
end;

function TServiceLocatorAdapter.HasService(serviceType: PTypeInfo; const name: string): Boolean;
begin
  Result := fContainer.HasService({serviceType, }name);
end;

{$ENDREGION}

procedure InitializeGlobalContainer;
begin
  _GlobalContainer := TContainer.Create;
  _GlobalServiceLocator := TServiceLocatorAdapter.Create(_GlobalContainer);
  ServiceLocator.Initialize(
    function: IServiceLocator
    begin
      Result := _GlobalServiceLocator;
    end
  );
end;

initialization
  InitializeGlobalContainer;

finalization
  _GlobalServiceLocator := nil;
  FreeAndNil(_GlobalContainer);

end.
