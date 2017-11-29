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

unit Spring.Container;

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Common,
  Spring.Container.Core,
  Spring.Container.Registration,
  Spring.Logging,
  Spring.Services;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}{$ENDIF}

type
  /// <summary>
  ///   Represents a Dependency Injection Container.
  /// </summary>
  TContainer = class(TKernel, IKernelInternal)
  private
    fChangedModels: ISet<TComponentModel>;
    procedure CheckBuildRequired;
    procedure HandleBuild(Sender: TObject; const model: TComponentModel);
    procedure HandleRegistryChanged(Sender: TObject;
      const model: TComponentModel; action: TCollectionChangedAction);

    procedure ResolveInternal(var result; serviceType: PTypeInfo);
    function RegisterInstanceInternal(serviceType: PTypeInfo; const instance;
      const serviceName: string): TRegistration;

    function CreateProvider<T>(const delegate: TProviderDelegate<T>; const model: TComponentModel): IProvider; overload;
  {$IFDEF DELPHIXE7_UP}
    function CreateProviderObj(const delegate: IInterface; typeInfo: Pointer; const model: TComponentModel): IProvider;
    function CreateProviderIntf(const delegate: IInterface; typeInfo: Pointer; const model: TComponentModel): IProvider;
  {$ENDIF}
    function CreateProvider(const instance: TValue; const model: TComponentModel): IProvider; overload;
    function CreateProvider(const instance; instanceType: PTypeInfo; const model: TComponentModel): IProvider; overload;

    class var GlobalInstance: TContainer;
    type TValueArray = array of TValue;
  protected
    class constructor Create;
    class destructor Destroy;
    procedure InitializeInspectors; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddExtension<T: IContainerExtension, constructor>; overload;

    function RegisterDecorator<TService; TDecorator: TService>: TRegistration; overload; inline;
    function RegisterDecorator<TService; TDecorator: TService>(
      const condition: Predicate<TComponentModel>): TRegistration; overload; inline;

    function RegisterFactory<TFactoryType: IInterface>: TRegistration; overload;
    function RegisterFactory<TFactoryType: IInterface>(
      paramResolution: TParamResolution): TRegistration; overload;
    function RegisterFactory<TFactoryType: IInterface>(const serviceName: string): TRegistration; overload;
    function RegisterFactory<TFactoryType: IInterface>(const serviceName: string;
      paramResolution: TParamResolution): TRegistration; overload;
    function RegisterFactory<TFactoryType: IInterface>(const serviceName: string;
      const resolvedServiceName: string): TRegistration; overload;
    function RegisterFactory<TFactoryType: IInterface>(const serviceName: string;
      const resolvedServiceName: string;
      paramResolution: TParamResolution): TRegistration; overload;

    function RegisterInstance<TServiceType>(const instance: TServiceType;
      const serviceName: string = ''): TRegistration; overload; inline;
    function RegisterInstance(serviceType: PTypeInfo; const instance: TValue;
      const serviceName: string = ''): TRegistration; overload;

    function RegisterType<TComponentType>: TRegistration; overload; inline;
    function RegisterType(componentType: PTypeInfo): TRegistration; overload;
    function RegisterType<TServiceType>(
      const serviceName: string): TRegistration; overload; inline;
    function RegisterType<TServiceType, TComponentType>(
      const serviceName: string = ''): TRegistration; overload; inline;
    function RegisterType(serviceType, componentType: PTypeInfo;
      const serviceName: string = ''): TRegistration; overload;

    function RegisterType<TComponentType>(
      const delegate: TProviderDelegate<TComponentType>): TRegistration; overload; inline;
    function RegisterType<TServiceType>(
      const delegate: TProviderDelegate<TServiceType>;
      const serviceName: string): TRegistration; overload; inline;
    function RegisterType<TServiceType, TComponentType>(
      const delegate: TProviderDelegate<TComponentType>;
      const serviceName: string = ''): TRegistration; overload; inline;

    procedure Build;

    function Resolve<T>: T; overload;
    function Resolve<T>(const arguments: array of TValue): T; overload;
    function Resolve<T>(const serviceName: string): T; overload;
    function Resolve<T>(const serviceName: string;
      const arguments: array of TValue): T; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo;
      const arguments: array of TValue): TValue; overload;
    function Resolve(const serviceName: string): TValue; overload;
    function Resolve(const serviceName: string;
      const arguments: array of TValue): TValue; overload;

    function ResolveAll<TServiceType>: TArray<TServiceType>; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>; overload;
  end;

  /// <summary>
  ///   Adapter to get access to a <see cref="TContainer" /> instance over the <see cref="Spring.Services|IServiceLocator" />
  ///    interface.
  /// </summary>
  TServiceLocatorAdapter = class(TInterfacedObject, IServiceLocator)
  private
    fContainer: TContainer;
    class var GlobalInstance: IServiceLocator;
    class procedure Init; static;
  public
    constructor Create(const container: TContainer);

    function GetService(serviceType: PTypeInfo): TValue; overload;
    function GetService(serviceType: PTypeInfo; const serviceName: string): TValue; overload;
    function GetService(serviceType: PTypeInfo; const args: array of TValue): TValue; overload;
    function GetService(serviceType: PTypeInfo; const serviceName: string; const args: array of TValue): TValue; overload;

    function GetAllServices(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const serviceName: string): Boolean; overload;
  end;


{$REGION 'Exceptions'}

  EContainerException = Spring.Container.Core.EContainerException;
  ERegistrationException = Spring.Container.Core.ERegistrationException;
  EResolveException = Spring.Container.Core.EResolveException;
  ECircularDependencyException = Spring.Container.Core.ECircularDependencyException;
  EActivatorException = Spring.Container.Core.EActivatorException;

{$ENDREGION}

procedure CleanupGlobalContainer;

/// <summary>
///   Returns global instance of the container.
/// </summary>
function GlobalContainer: TContainer; inline;

implementation

uses
  TypInfo,
  Spring.Container.Builder,
  Spring.Container.Context,
  Spring.Container.Providers,
  Spring.Container.Resolvers,
  Spring.Container.ResourceStrings,
  Spring.Reflection;

function GlobalContainer: TContainer;
begin
  Result := TContainer.GlobalInstance;
end;


{$REGION 'TContainer'}

class constructor TContainer.Create;
begin
  GlobalInstance := TContainer.Create;
end;

class destructor TContainer.Destroy;
begin
  GlobalInstance.Free;
end;

procedure TContainer.CheckBuildRequired;
begin
  if fChangedModels.Any then
    // TODO just call Build instead of exception?
    raise EContainerException.CreateRes(@SContainerRequiresBuild);
end;

constructor TContainer.Create;
begin
  inherited Create;
  fChangedModels := TCollections.CreateSet<TComponentModel>;
  Registry.OnChanged.Add(HandleRegistryChanged);
  Builder.OnBuild.Add(HandleBuild);
  InitializeInspectors;

  Resolver.AddResolver(TLazyResolver.Create(Self));
  Resolver.AddResolver(TDynamicArrayResolver.Create(Self));
  Resolver.AddResolver(TCollectionResolver.Create(Self));
  Resolver.AddResolver(TComponentOwnerResolver.Create(Self));
end;

destructor TContainer.Destroy;
begin
  fChangedModels.Clear;
  Builder.ClearInspectors;
  Registry.UnregisterAll;
  inherited Destroy;
end;

procedure TContainer.AddExtension<T>;
var
  extension: IContainerExtension;
begin
  extension := T.Create;
  AddExtension(extension);
end;

procedure TContainer.Build;
begin
  Builder.BuildAll;
  fChangedModels.Clear;
end;

procedure TContainer.InitializeInspectors;
var
  inspectors: TArray<IBuilderInspector>;
  inspector: IBuilderInspector;
begin
  inspectors := TArray<IBuilderInspector>.Create(
    TInterfaceInspector.Create,
    TProviderInspector.Create,
    TLifetimeInspector.Create,
    TInjectionTargetInspector.Create,
    TConstructorInspector.Create,
    TPropertyInspector.Create,
    TMethodInspector.Create,
    TFieldInspector.Create,
    TInterceptorInspector.Create,
    TAbstractMethodInspector.Create
  );
  for inspector in inspectors do
    Builder.AddInspector(inspector);
end;

function TContainer.CreateProvider(
  const instance: TValue; const model: TComponentModel): IProvider;
var
  value: TValue;
begin
  value := instance;
  Result := TDelegateProvider.Create(Self, model,
    function: TValue
    begin
      Result := value;
    end);
end;

function TContainer.CreateProvider(const instance;
  instanceType: PTypeInfo; const model: TComponentModel): IProvider;
var
  value: TValue;
begin
  TValue.Make(@instance, instanceType, value);
  Result := TDelegateProvider.Create(Self, model,
    function: TValue
    begin
      Result := value;
    end);
end;

function TContainer.CreateProvider<T>(
  const delegate: TProviderDelegate<T>;
  const model: TComponentModel): IProvider;
begin
  Result := TDelegateProvider.Create(Self, model,
    function: TValue
    var
      instance: T;
    begin
      instance := delegate();
      Result := TValue.From(@instance, TypeInfo(T));
    end);
end;

{$IFDEF DELPHIXE7_UP}
function TContainer.CreateProviderIntf(const delegate: IInterface;
  typeInfo: Pointer; const model: TComponentModel): IProvider;
begin
  Result := TDelegateProvider.Create(Self, model,
    function: TValue
    var
      instance: IInterface;
    begin
      instance := TProviderDelegate<IInterface>(delegate)();
      Result := TValue.From(@instance, typeInfo);
    end);
end;

function TContainer.CreateProviderObj(const delegate: IInterface;
  typeInfo: Pointer; const model: TComponentModel): IProvider;
begin
  Result := TDelegateProvider.Create(Self, model,
    function: TValue
    var
      instance: TObject;
    begin
      instance := TProviderDelegate<TObject>(delegate)();
      Result := TValue.From(@instance, typeInfo);
    end);
end;
{$ENDIF}

procedure TContainer.HandleBuild(Sender: TObject; const model: TComponentModel);
begin
  fChangedModels.Remove(model);
end;

procedure TContainer.HandleRegistryChanged(Sender: TObject;
  const model: TComponentModel; action: TCollectionChangedAction);
begin
  fChangedModels.Add(model);
end;

function TContainer.RegisterType(componentType: PTypeInfo): TRegistration;
begin
  Result := TRegistration(Registry.RegisterComponent(componentType));
end;

function TContainer.RegisterType(serviceType, componentType: PTypeInfo;
  const serviceName: string): TRegistration;
begin
  Result := RegisterType(componentType);
  Result.Implements(serviceType, serviceName);
end;

function TContainer.RegisterDecorator<TService, TDecorator>: TRegistration;
begin
  Result := RegisterType(TypeInfo(TDecorator), TypeInfo(TDecorator));
  DecoratorResolver.AddDecorator(TypeInfo(TService), Result.Model, nil);
end;

function TContainer.RegisterDecorator<TService, TDecorator>(
  const condition: Predicate<TComponentModel>): TRegistration;
begin
  Result := RegisterType(TypeInfo(TDecorator), TypeInfo(TDecorator));
  DecoratorResolver.AddDecorator(TypeInfo(TService), Result.Model, condition);
end;

function TContainer.RegisterFactory<TFactoryType>: TRegistration;
begin
  Result := RegisterType(TypeInfo(TFactoryType), TypeInfo(TFactoryType));
  Registry.RegisterFactory(Result.Model);
end;

function TContainer.RegisterFactory<TFactoryType>(
  paramResolution: TParamResolution): TRegistration;
begin
  Result := RegisterType(TypeInfo(TFactoryType), TypeInfo(TFactoryType));
  Registry.RegisterFactory(Result.Model, paramResolution);
end;

function TContainer.RegisterFactory<TFactoryType>(
  const serviceName: string): TRegistration;
begin
  Result := RegisterType(TypeInfo(TFactoryType), TypeInfo(TFactoryType), serviceName);
  Registry.RegisterFactory(Result.Model);
end;

function TContainer.RegisterFactory<TFactoryType>(
  const serviceName: string;
  paramResolution: TParamResolution): TRegistration;
begin
  Result := RegisterType(TypeInfo(TFactoryType), TypeInfo(TFactoryType), serviceName);
  Registry.RegisterFactory(Result.Model, paramResolution);
end;

function TContainer.RegisterFactory<TFactoryType>(const serviceName,
  resolvedServiceName: string): TRegistration;
begin
  Result := RegisterType(TypeInfo(TFactoryType), TypeInfo(TFactoryType), serviceName);
  Registry.RegisterFactory(Result.Model, resolvedServiceName);
end;

function TContainer.RegisterFactory<TFactoryType>(const serviceName,
  resolvedServiceName: string;
  paramResolution: TParamResolution): TRegistration;
begin
  Result := RegisterType(TypeInfo(TFactoryType), TypeInfo(TFactoryType), serviceName);
  Registry.RegisterFactory(Result.Model, resolvedServiceName, paramResolution);
end;

function TContainer.RegisterInstanceInternal(serviceType: PTypeInfo;
  const instance; const serviceName: string): TRegistration;
begin
  Result := RegisterType(serviceType, serviceType, serviceName);
  Result.Model.Provider := CreateProvider(instance, serviceType, Result.Model);
end;

function TContainer.RegisterInstance<TServiceType>(const instance: TServiceType;
  const serviceName: string): TRegistration;
begin
  Result := RegisterInstanceInternal(TypeInfo(TServiceType), instance, serviceName);
end;

function TContainer.RegisterInstance(serviceType: PTypeInfo;
  const instance: TValue; const serviceName: string): TRegistration;
begin
  Result := RegisterType(serviceType, serviceType, serviceName);
  Result.Model.Provider := CreateProvider(instance, Result.Model);
end;

function TContainer.RegisterType<TComponentType>: TRegistration;
begin
  Result := RegisterType(TypeInfo(TComponentType));
end;

function TContainer.RegisterType<TServiceType>(
  const serviceName: string): TRegistration;
begin
  Result := RegisterType(TypeInfo(TServiceType), TypeInfo(TServiceType), serviceName);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const serviceName: string): TRegistration;
begin
  Result := RegisterType(TypeInfo(TServiceType), TypeInfo(TComponentType), serviceName);
end;

function TContainer.RegisterType<TComponentType>(
  const delegate: TProviderDelegate<TComponentType>): TRegistration;
begin
  Result := RegisterType(TypeInfo(TComponentType));
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TComponentType) of
    tkClass: Result.Model.Provider := CreateProviderObj(PInterface(@delegate)^, TypeInfo(TComponentType), Result.Model);
    tkInterface: Result.Model.Provider := CreateProviderIntf(PInterface(@delegate)^, TypeInfo(TComponentType), Result.Model);
  else{$ELSE}begin{$ENDIF}
    Result.Model.Provider := CreateProvider<TComponentType>(delegate, Result.Model);
  end;
end;

function TContainer.RegisterType<TServiceType>(
  const delegate: TProviderDelegate<TServiceType>;
  const serviceName: string): TRegistration;
begin
  Result := RegisterType(TypeInfo(TServiceType), TypeInfo(TServiceType), serviceName);
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TServiceType) of
    tkClass: Result.Model.Provider := CreateProviderObj(PInterface(@delegate)^, TypeInfo(TServiceType), Result.Model);
    tkInterface: Result.Model.Provider := CreateProviderIntf(PInterface(@delegate)^, TypeInfo(TServiceType), Result.Model);
  else{$ELSE}begin{$ENDIF}
    Result.Model.Provider := CreateProvider<TServiceType>(delegate, Result.Model);
  end;
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const delegate: TProviderDelegate<TComponentType>;
  const serviceName: string): TRegistration;
begin
  Result := RegisterType(TypeInfo(TServiceType), TypeInfo(TComponentType), serviceName);
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TComponentType) of
    tkClass: Result.Model.Provider := CreateProviderObj(PInterface(@delegate)^, TypeInfo(TComponentType), Result.Model);
    tkInterface: Result.Model.Provider := CreateProviderIntf(PInterface(@delegate)^, TypeInfo(TComponentType), Result.Model);
  else{$ELSE}begin{$ENDIF}
    Result.Model.Provider := CreateProvider<TComponentType>(delegate, Result.Model);
  end;
end;

procedure TContainer.ResolveInternal(var result; serviceType: PTypeInfo);
var
  value: TValue;
begin
  value := Resolve(serviceType, []);
  value.ExtractRawData(@Result);
end;

function TContainer.Resolve<T>: T;
begin
  ResolveInternal(Result, TypeInfo(T));
end;

function TContainer.Resolve<T>(const arguments: array of TValue): T;
var
  value: TValue;
begin
  value := Resolve(TypeInfo(T), arguments);
  value.AsType(TypeInfo(T), Result);
end;

function TContainer.Resolve<T>(const serviceName: string): T;
var
  value: TValue;
begin
  value := Resolve(serviceName, []);
  value.AsType(TypeInfo(T), Result);
end;

function TContainer.Resolve<T>(const serviceName: string;
  const arguments: array of TValue): T;
var
  value: TValue;
begin
  value := Resolve(serviceName, arguments);
  value.AsType(TypeInfo(T), Result);
end;

function TContainer.Resolve(serviceType: PTypeInfo): TValue;
begin
  Result := Resolve(serviceType, []);
end;

function TContainer.Resolve(serviceType: PTypeInfo;
  const arguments: array of TValue): TValue;
var
  context: IContext;
  request: IRequest;
begin
  CheckBuildRequired;
  context := TContext.Create(nil, arguments);
  request := TRequest.Create(serviceType, context, nil, nil);
  Result := Resolver.Resolve(request);
end;

function TContainer.Resolve(const serviceName: string): TValue;
begin
  Result := Resolve(serviceName, []);
end;

function TContainer.Resolve(const serviceName: string;
  const arguments: array of TValue): TValue;
var
  componentModel: TComponentModel;
  context: IContext;
  serviceType: PTypeInfo;
  request: IRequest;
begin
  CheckBuildRequired;
  componentModel := Registry.FindOne(serviceName);
  if not Assigned(componentModel) then
    raise EResolveException.CreateResFmt(@SServiceNotFound, [serviceName]);
  context := TContext.Create(componentModel, arguments);
  serviceType := componentModel.GetServiceType(serviceName);
  request := TRequest.Create(serviceType, context, nil, serviceName);
  Result := Resolver.Resolve(request);
end;

function TContainer.ResolveAll<TServiceType>: TArray<TServiceType>;
var
  values: TArray<TValue>;
  i: Integer;
begin
  values := ResolveAll(TypeInfo(TServiceType));
  SetLength(Result, Length(values));
  for i := Low(values) to High(values) do
    TValueArray(values)[i].AsType(TypeInfo(TServiceType), Result[i]);
end;

function TContainer.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
var
  targetType: TRttiType;
  models: TArray<TComponentModel>;
  i: Integer;
  context: IContext;
  serviceName: string;
  request: IRequest;
begin
  CheckBuildRequired;
  targetType := serviceType.RttiType;
  // TODO: remove dependency on lazy type
  if IsLazyType(serviceType) then
    serviceType := GetLazyType(serviceType);
  models := Registry.FindAll(serviceType).ToArray;
  SetLength(Result, Length(models));
  for i := Low(models) to High(models) do
  begin
    context := TContext.Create(models[i], []);
    serviceName := models[i].GetServiceName(serviceType);
    request := TRequest.Create(targetType.Handle, context, nil, serviceName);
    Result[i] := Resolver.Resolve(request);
  end;
end;

{$ENDREGION}


{$REGION 'TServiceLocatorAdapter'}

class procedure TServiceLocatorAdapter.Init;
begin
  GlobalInstance := TServiceLocatorAdapter.Create(GlobalContainer);
  ServiceLocator.Initialize(
    function: IServiceLocator
    begin
      Result := GlobalInstance;
    end);
end;

constructor TServiceLocatorAdapter.Create(const container: TContainer);
begin
  inherited Create;
  fContainer := container;
end;

function TServiceLocatorAdapter.GetService(serviceType: PTypeInfo): TValue;
begin
  Result := fContainer.Resolve(serviceType);
end;

function TServiceLocatorAdapter.GetService(serviceType: PTypeInfo; //FI:O804
  const serviceName: string): TValue;
begin
  Result := fContainer.Resolve({serviceType, }serviceName);
end;

function TServiceLocatorAdapter.GetService(serviceType: PTypeInfo;
  const args: array of TValue): TValue;
begin
  Result := fContainer.Resolve(serviceType, args);
end;

function TServiceLocatorAdapter.GetService(serviceType: PTypeInfo; //FI:O804
  const serviceName: string; const args: array of TValue): TValue;
begin
  Result := fContainer.Resolve({serviceType, }serviceName, args);
end;

function TServiceLocatorAdapter.GetAllServices(serviceType: PTypeInfo): TArray<TValue>;
begin
  Result := fContainer.ResolveAll(serviceType);
end;

function TServiceLocatorAdapter.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := fContainer.Registry.HasService(serviceType);
end;

function TServiceLocatorAdapter.HasService(serviceType: PTypeInfo; const serviceName: string): Boolean;
begin
  Result := fContainer.Registry.HasService(serviceType, serviceName);
end;

{$ENDREGION}


procedure CleanupGlobalContainer;
begin
  TServiceLocatorAdapter.GlobalInstance := nil;
  FreeAndNil(TContainer.GlobalInstance);
end;

initialization
  TServiceLocatorAdapter.Init;

end.
