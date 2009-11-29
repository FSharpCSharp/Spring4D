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
//  Spring.DesignPatterns,
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
  protected
    { Implements IContainerContext }
    function GetDependencyResolver: IDependencyResolver;
    function GetInjectionFactory: IInjectionFactory;
    function CreateLifetimeManager(model: TComponentModel): ILifetimeManager;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
  protected
    procedure InitializeServiceInspectors; virtual;
    function DoRegisterType(const name: string; serviceType, componentType: PTypeInfo;
      lifetimeType: TLifetimeType; activatorDelegate: TActivatorDelegate): TContainer; overload;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterComponent<TComponentType: class>: TRegistration<TComponentType>; overload;
    function RegisterComponent(componentType: PTypeInfo): TRegistration; overload;

//    function RegisterDecorations<TServiceType>(const decorationClasses: array of TClass): TContainer;
//    function RegisterInstance<T>(instance: T): TContainer;

    procedure Build; // SetUp or BuildUp

    function Resolve<T>: T; overload;
    function Resolve<T>(const name: string): T; overload;
    function Resolve(typeInfo: PTypeInfo): TValue; overload;
    function Resolve(typeInfo: PTypeInfo; const name: string): TValue; overload;

    function ResolveAll<TServiceType>: TArray<TServiceType>; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;

    { Experimental Release Methods }
    procedure Release(instance: TObject); overload;
    procedure Release(instance: IInterface); overload;

    {$REGION 'Deprecated RegisterType Methods'}

    function RegisterType<TServiceType; TComponentType: TServiceType>: TContainer; overload; deprecated 'Use RegisterComponent instead.';
    function RegisterType<TServiceType; TComponentType: TServiceType>(
      activatorDelegate: TActivatorDelegate<TComponentType>): TContainer; overload; deprecated 'Use RegisterComponent instead.';

    function RegisterType<TServiceType; TComponentType: TServiceType>(lifetimeType: TLifetimeType): TContainer; overload;
    function RegisterType<TServiceType; TComponentType: TServiceType>(lifetimeType: TLifetimeType;
      activatorDelegate: TActivatorDelegate<TComponentType>): TContainer; overload; deprecated 'Use RegisterComponent instead.';

    function RegisterType<TServiceType; TComponentType: TServiceType>(const name: string): TContainer; overload; deprecated 'Use RegisterComponent instead.';
    function RegisterType<TServiceType; TComponentType: TServiceType>(
      const name: string; activatorDelegate: TActivatorDelegate<TComponentType>): TContainer; overload; deprecated 'Use RegisterComponent instead.';

    function RegisterType<TServiceType; TComponentType: TServiceType>(const name: string;
      lifetimeType: TLifetimeType): TContainer; overload; deprecated 'Use RegisterComponent instead.';
    function RegisterType<TServiceType; TComponentType: TServiceType>(
      const name: string; lifetimeType: TLifetimeType;
      activatorDelegate: TActivatorDelegate<TComponentType>): TContainer; overload; deprecated 'Use RegisterComponent instead.';

    {$ENDREGION}
  end;

  // TEMP
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
  Spring.IoC.ResourceStrings;

{$REGION 'TContainer'}

constructor TContainer.Create;
begin
  inherited Create;
  fRegistry := TComponentRegistry.Create(Self);
  fBuilder := TComponentBuilder.Create(Self, fRegistry);
  fServiceResolver := TServiceResolver.Create(Self, fRegistry);
  fDependencyResolver := TDependencyResolver.Create(fRegistry);
  fInjectionFactory := TInjectionFactory.Create;
  fRegistrationManager := TRegistrationManager.Create(fRegistry);
  InitializeServiceInspectors;
end;

destructor TContainer.Destroy;
begin
  fRegistrationManager.Free;
  fBuilder.ClearInspectors;
  fRegistry.Clear;
  inherited Destroy;
end;

procedure TContainer.Build;
begin
  fBuilder.BuildAll;
end;

procedure TContainer.InitializeServiceInspectors;
var
  inspectors: TArray<IBuilderInspector>;
  inspector: IBuilderInspector;
begin
  inspectors := TArray<IBuilderInspector>.Create(
    TLifetimeInspector.Create,
    TComponentActivatorInspector.Create,
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

function TContainer.GetDependencyResolver: IDependencyResolver;
begin
  Result := fDependencyResolver;
end;

function TContainer.GetInjectionFactory: IInjectionFactory;
begin
  Result := fInjectionFactory;
end;

function TContainer.RegisterType<TServiceType, TComponentType>: TContainer;
begin
  Result := DoRegisterType('', TypeInfo(TServiceType), TypeInfo(TComponentType),
    ltUnknown, nil);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  activatorDelegate: TActivatorDelegate<TComponentType>): TContainer;
begin
  Result := DoRegisterType('', TypeInfo(TServiceType), TypeInfo(TComponentType),
    ltUnknown, TActivatorDelegate(activatorDelegate));
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  lifetimeType: TLifetimeType): TContainer;
begin
  Result := DoRegisterType('', TypeInfo(TServiceType), TypeInfo(TComponentType),
    lifetimeType, nil);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  lifetimeType: TLifetimeType;
  activatorDelegate: TActivatorDelegate<TComponentType>): TContainer;
begin
  Result := DoRegisterType('', TypeInfo(TServiceType), TypeInfo(TComponentType),
    lifetimeType, TActivatorDelegate(activatorDelegate));
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const name: string): TContainer;
begin
  Result := DoRegisterType(name, TypeInfo(TServiceType), TypeInfo(TComponentType), ltUnknown, nil);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const name: string; activatorDelegate: TActivatorDelegate<TComponentType>): TContainer;
begin
  Result := DoRegisterType(name, TypeInfo(TServiceType), TypeInfo(TComponentType),
    ltUnknown, TActivatorDelegate(activatorDelegate));
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const name: string; lifetimeType: TLifetimeType): TContainer;
begin
  Result := DoRegisterType(name, TypeInfo(TServiceType), TypeInfo(TComponentType),
    lifetimeType, nil);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const name: string; lifetimeType: TLifetimeType;
  activatorDelegate: TActivatorDelegate<TComponentType>): TContainer;
begin
  Result := DoRegisterType(name, TypeInfo(TServiceType), TypeInfo(TComponentType),
    lifetimeType, TActivatorDelegate(activatorDelegate));
end;

function TContainer.DoRegisterType(const name: string; serviceType,
  componentType: PTypeInfo; lifetimeType: TLifetimeType;
  activatorDelegate: TActivatorDelegate): TContainer;
var
  model: TComponentModel;
begin
  model := fRegistry.GetComponentModel(componentType);
  model.LifetimeType := lifetimeType;
  model.ActivatorDelegate := activatorDelegate;
  fRegistry.AddServiceType(componentType, serviceType);
  fBuilder.Build(model);
  Result := Self;
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

function TContainer.HasService(serviceType: PTypeInfo; const name: string): Boolean;
begin
  Result := fRegistry.HasService(serviceType, name);
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
  value := Resolve(TypeInfo(T), name);
  Result := value.AsType<T>;
end;

function TContainer.Resolve(typeInfo: PTypeInfo): TValue;
begin
  Result := Resolve(typeInfo, '');
end;

function TContainer.Resolve(typeInfo: PTypeInfo; const name: string): TValue;
var
  model: TComponentModel;
begin
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');
  if not fRegistry.HasService(typeInfo, name) and (typeInfo.Kind = tkClass) then
  begin
    RegisterComponent(typeInfo).Implements(typeInfo, name);
    model := fRegistry.FindOne(typeInfo);
    fBuilder.Build(model);
  end;
  Result := fServiceResolver.Resolve(typeInfo, name);
end;

//// [Internal Error URW1111]
//function TContainer.ResolveAll<TServiceType>: TArray<TServiceType>;
//var
//  serviceType: PTypeInfo;
//  instances: TArray<TValue>;
//  i: Integer;
//begin
//  serviceType := TypeInfo(TServiceType);
//  instances := fServiceResolver.ResolveAll(serviceType);
//  SetLength(Result, Length(Instances));
//  for i := 0 to High(instances) do
//  begin
//    Result[i] := instances[i].AsType<TServiceType>;
//  end;
//end;

function TContainer.ResolveAll<TServiceType>: TArray<TServiceType>;
var
  serviceType: PTypeInfo;
  models: IList<TComponentModel>;
  model: TComponentModel;
  value: TValue;
  i: Integer;
begin
  serviceType := TypeInfo(TServiceType);
  TArgument.CheckTypeKind(serviceType, [tkClass, tkInterface], 'serviceType');
  models := fRegistry.FindAll(serviceType).ToList;
  SetLength(Result, models.Count);
  for i := 0 to models.Count - 1 do
  begin
    model := models[i];
    value := Resolve(serviceType, model.GetServiceName(serviceType));
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
