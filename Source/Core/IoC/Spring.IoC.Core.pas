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

unit Spring.IoC.Core;

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
  Spring.DesignPatterns,
  Spring.Reflection;

type
  { Forward Declarations }
  TComponentModel = class;
  IComponentRegistry = interface;
  IBuilderInspector = interface;
  IServiceResolver = interface;
  IDependencyResolver = interface;
  IInjection = interface;
  IInjectionFactory = interface;
  ILifetimeManager = interface;

  TActivatorDelegate = reference to function: TObject;
  TActivatorDelegate<T: class> = reference to function: T;

  /// <summary>
  /// IContainerContext
  /// </summary>
  IContainerContext = interface
    ['{9E90EADB-A720-4394-A5E0-5DF0550C1E92}']
  {$REGION 'Property Getters & Setters'}
    function GetComponentRegistry: IComponentRegistry;
    function GetDependencyResolver: IDependencyResolver;
    function GetInjectionFactory: IInjectionFactory;
  {$ENDREGION}
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;
//    function GetServiceType(const name: string): PTypeInfo;
    function CreateLifetimeManager(model: TComponentModel): ILifetimeManager;
    property ComponentRegistry: IComponentRegistry read GetComponentRegistry;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
  end;

  /// <summary>
  /// Manages the registration of components. (IComponentBuilder)
  /// </summary>
  IComponentRegistry = interface
    ['{CBCA1D0F-1244-4AB4-AB07-091053932166}']

    procedure RegisterService(componentType, serviceType: PTypeInfo); overload;
    procedure RegisterService(componentType, serviceType: PTypeInfo; const name: string); overload;
    procedure UnregisterAll;

    function GetComponent(componentType: PTypeInfo): TComponentModel;
    function HasComponent(componentType: PTypeInfo): Boolean;
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;
//    function GetServiceType(const name: string): PTypeInfo;

    function FindOne(componentType: PTypeInfo): TComponentModel; overload;
    function FindOne(const name: string): TComponentModel; overload;
    function FindAll: IEnumerableEx<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): IEnumerableEx<TComponentModel>; overload;
  end;

  IComponentBuilder = interface
    ['{8309EBC7-9699-47CF-B177-4BC9B787EBE0}']
    // Inspectors (Policies)
    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    // Build
    procedure Build(model: TComponentModel);
    procedure BuildAll;
  end;

  /// <summary>
  /// IBuilderInspector (IBuilderPolicy)
  /// </summary>
  IBuilderInspector = interface
    ['{3E2F36D1-2C0D-4D6A-91B3-49B09BD31318}']
    procedure ProcessModel(const context: IContainerContext; model: TComponentModel);
  end;

  /// <summary>
  /// ILifetimeManager
  /// </summary>
  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance: TObject;
    procedure ReleaseInstance(instance: TObject);
  end;

  /// <summary>
  /// Component Activator
  /// </summary>
  IComponentActivator = IObjectActivator;

//  IComponentActivator = interface
//    ['{752F2CDE-222C-4D8B-B344-BB7BCA9EAB9E}']
//    function CreateInstance: TObject;
//  end;

  /// <summary>
  /// Represents an injection of a member.
  /// e.g. constructor, method, property and even field injection.
  /// </summary>
  IInjection = interface
    ['{864AAA38-4F93-4BB9-AD8A-B796FCD2EFE0}']
    {$REGION 'Property Getters & Setters'}

    function GetDependencyCount: Integer;
    function GetTarget: TRttiMember;
    function GetTargetName: string;
    function GetHasTarget: Boolean;
    function GetModel: TComponentModel;

    {$ENDREGION}
    procedure Initialize(target: TRttiMember);
    procedure Inject(instance: TObject; const arguments: array of TValue);
    function GetDependencies: TArray<TRttiType>;
    property DependencyCount: Integer read GetDependencyCount;
    property Target: TRttiMember read GetTarget;
    property TargetName: string read GetTargetName;
    property HasTarget: Boolean read GetHasTarget;
    property Model: TComponentModel read GetModel;
  end;

  /// <summary>
  /// Injection Factory
  /// </summary>
  IInjectionFactory = interface
    ['{EA75E648-C3EB-4CE7-912A-AB82B12BBD87}']
    function CreateConstructorInjection(model: TComponentModel): IInjection;
    function CreateMethodInjection(model: TComponentModel; const methodName: string): IInjection;
    function CreatePropertyInjection(model: TComponentModel; const propertyName: string): IInjection;
    function CreateFieldInjection(model: TComponentModel; const fieldName: string): IInjection;
  end;

//  IConstructorSelector = interface
//    ['{1CB36E23-84B6-462C-A21D-0EE8BEDBDE1D}']
//    function SelectEligibleConstructor(const context: IContainerContext; componentType: TRttiInstanceType);
//  end;

//  ITypeConverter = interface
//    ['{0FC28D39-C191-4BA4-B56E-410932018A1E}']
//    function CanConvert(const value: TValue; targetType: TRttiType): Boolean;
//    function TryConvert(const value: TValue; targetType: TRttiType; out targetValue: TValue): Boolean;
//  end;

  IDependencyResolver = interface
    ['{15ADEA1D-7C3F-48D5-8E85-84B4332AFF5F}']
//    function CanResolveDependency(dependency: TRttiType): Boolean; overload;
//    function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; overload;
//    function ResolveDependency(dependency: TRttiType): TValue; overload;
//    function ResolveDependency(dependency: TRttiType; const argument: TValue): TValue; overload;

    function CanResolveDependencies(dependencies: TArray<TRttiType>): Boolean; overload;
    function CanResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean; overload;
    function ResolveDependencies(dependencies: TArray<TRttiType>): TArray<TValue>; overload;
    function ResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): TArray<TValue>; overload;

    function CanResolveDependencies(const injection: IInjection): Boolean; overload;
    function CanResolveDependencies(const injection: IInjection; const arguments: TArray<TValue>): Boolean; overload;
    function ResolveDependencies(const injection: IInjection): TArray<TValue>; overload;
    function ResolveDependencies(const injection: IInjection; const arguments: TArray<TValue>): TArray<TValue>; overload;
  end;

  /// <summary>
  /// Resolves services.
  /// </summary>
  IServiceResolver = interface // (IDependencyResolver)
    ['{14669EBA-4E57-4DF4-919D-377D8E90144C}']
    function CanResolve(serviceType: PTypeInfo): Boolean; overload;
    function CanResolve(const name: string): Boolean; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  /// <summary>
  /// TComponentModel
  /// </summary>
  TComponentModel = class
  private
    fContext: IContainerContext;
    fComponentType: TRttiInstanceType;
    fLifetimeType: TLifetimeType;
    fLifetimeManager: ILifetimeManager;
    fComponentActivator: IComponentActivator;
    fActivatorDelegate: TActivatorDelegate;
    fMinPoolsize: Integer;
    fMaxPoolsize: Integer;
    fServices: IDictionary<string, PTypeInfo>;
    fConstructorInjections: IList<IInjection>;
    fMethodInjections: IList<IInjection>;
    fPropertyInjections: IList<IInjection>;
    fFieldInjections: IList<IInjection>;
    fInjectionArguments: IDictionary<IInjection, TArray<TValue>>;
    function GetComponentTypeInfo: PTypeInfo;
    function GetInjectionFactory: IInjectionFactory;
  protected
    function GetServices: IDictionary<string, PTypeInfo>;
    function GetConstructorInjections: IList<IInjection>;
    function GetMethodInjections: IList<IInjection>;
    function GetPropertyInjections: IList<IInjection>;
    function GetFieldInjections: IList<IInjection>;
    function GetInjections: IDictionary<IInjection, TArray<TValue>>;
    property Injections: IDictionary<IInjection, TArray<TValue>> read GetInjections;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
  public
    constructor Create(context: IContainerContext; componentType: TRttiInstanceType);

    {$REGION 'Typed Injections'}

    function InjectConstructor(const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectMethod(const methodName: string): IInjection; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectProperty(const propertyName: string): IInjection; overload;
    function InjectField(const fieldName: string): IInjection; overload;

    {$ENDREGION}

    {$REGION 'Named/Valued Injections'}

    procedure InjectConstructor(const arguments: array of TValue); overload;
    procedure InjectMethod(const methodName: string; const arguments: array of TValue); overload;
    procedure InjectProperty(const propertyName: string; const value: TValue); overload;
    procedure InjectField(const fieldName: string; const value: TValue); overload;

    {$ENDREGION}

    function HasService(serviceType: PTypeInfo): Boolean;
    function GetServiceName(serviceType: PTypeInfo): string;
    function GetServiceType(const name: string): PTypeInfo;
    function GetInjectionArguments(const injection: IInjection): TArray<TValue>;
    procedure UpdateInjectionArguments(const injection: IInjection; const arguments: array of TValue);

    property ComponentType: TRttiInstanceType read fComponentType;
    property ComponentTypeInfo: PTypeInfo read GetComponentTypeInfo;
    property Services: IDictionary<string, PTypeInfo> read GetServices;
    property MinPoolsize: Integer read fMinPoolsize write fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize write fMaxPoolsize;

    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
    property ComponentActivator: IComponentActivator read fComponentActivator write fComponentActivator;
    property ActivatorDelegate: TActivatorDelegate read fActivatorDelegate write fActivatorDelegate;

    property ConstructorInjections: IList<IInjection> read GetConstructorInjections;
    property MethodInjections: IList<IInjection> read GetMethodInjections;
    property PropertyInjections: IList<IInjection> read GetPropertyInjections;
    property FieldInjections: IList<IInjection> read GetFieldInjections;
  end;

  EContainerException = class(SysUtils.Exception);

  ERegistrationException = class(EContainerException);
  EBuilderException = class(EContainerException);
  EInjectionException = class(EContainerException);

  EResolveException = class(EContainerException);
  ECircularDependencyException = class(EResolveException);
  EUnsatisfiedDependencyException = class(EResolveException);

  EActivatorException = class(EContainerException);

implementation

uses
  Spring.ResourceStrings,
  Spring.Helpers,
  Spring.Core.ResourceStrings;

{$REGION 'TComponentModel'}

constructor TComponentModel.Create(context: IContainerContext;
  componentType: TRttiInstanceType);
begin
  inherited Create;
  fContext := context;
  fComponentType := componentType;
end;

function TComponentModel.InjectConstructor(
  const parameterTypes: array of PTypeInfo): IInjection;
var
  predicate: TPredicate<TRttiMethod>;
  method: TRttiMethod;
begin
  predicate := TMethodFilters.IsConstructor and
    TMethodFilters.HasParameterTypes(parameterTypes);
  method := ComponentType.GetMethods.FirstOrDefault(predicate);
  if method = nil then
  begin
    raise ERegistrationException.CreateRes(@SUnsatisfiedConstructorParameters);
  end;
  Result := InjectionFactory.CreateConstructorInjection(Self);
  Result.Initialize(method);
  ConstructorInjections.Add(Result);
end;

function TComponentModel.InjectMethod(const methodName: string): IInjection;
var
  method: TRttiMethod;
begin
  method := ComponentType.GetMethod(methodName);
  if method = nil then
  begin
    raise ERegistrationException.CreateResFmt(@SNoSuchMethod, [methodName]);
  end;
  Result := InjectionFactory.CreateMethodInjection(Self, methodName);
  Result.Initialize(method);
  MethodInjections.Add(Result);
end;

function TComponentModel.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): IInjection;
var
  predicate: TPredicate<TRttiMethod>;
  method: TRttiMethod;
begin
  predicate := TMethodFilters.IsNamed(methodName) and
    TMethodFilters.IsInstanceMethod and
    TMethodFilters.HasParameterTypes(parameterTypes);
  method := ComponentType.GetMethods.FirstOrDefault(predicate);
  if method = nil then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedMethodParameterTypes, [methodName]);
  end;
  Result := InjectionFactory.CreateMethodInjection(Self, methodName);
  Result.Initialize(method);
  MethodInjections.Add(Result);
end;

function TComponentModel.InjectProperty(const propertyName: string): IInjection;
var
  propertyMember: TRttiProperty;
begin
  propertyMember := ComponentType.GetProperty(propertyName);
  if propertyMember = nil then
  begin
    raise ERegistrationException.CreateResFmt(@SNoSuchProperty, [propertyName]);
  end;
  Result := InjectionFactory.CreatePropertyInjection(Self, propertyName);
  Result.Initialize(propertyMember);
  PropertyInjections.Add(Result);
end;

function TComponentModel.InjectField(const fieldName: string): IInjection;
var
  field: TRttiField;
begin
  field := ComponentType.GetField(fieldName);
  if field = nil then
  begin
    raise ERegistrationException.CreateResFmt(@SNoSuchField, [fieldName]);
  end;
  Result := InjectionFactory.CreateFieldInjection(Self, fieldName);
  Result.Initialize(field);
  FieldInjections.Add(Result);
end;

procedure TComponentModel.UpdateInjectionArguments(const injection: IInjection;
  const arguments: array of TValue);
begin
  TArgument.CheckNotNull(injection, 'injection');
  Injections.Add(injection, TArray.CreateArray<TValue>(arguments));
end;

procedure TComponentModel.InjectConstructor(const arguments: array of TValue);
var
  injection: IInjection;
begin
  injection := InjectionFactory.CreateConstructorInjection(Self);
  ConstructorInjections.Add(injection);
  UpdateInjectionArguments(injection, arguments);
end;

procedure TComponentModel.InjectMethod(const methodName: string;
  const arguments: array of TValue);
var
  injection: IInjection;
begin
  injection := InjectionFactory.CreateMethodInjection(Self, methodName);
  MethodInjections.Add(injection);
  UpdateInjectionArguments(injection, arguments);
end;

procedure TComponentModel.InjectProperty(const propertyName: string;
  const value: TValue);
var
  injection: IInjection;
begin
  injection := InjectProperty(propertyName);
  UpdateInjectionArguments(injection, value);
end;

procedure TComponentModel.InjectField(const fieldName: string;
  const value: TValue);
var
  injection: IInjection;
begin
  injection := InjectField(fieldName);
  UpdateInjectionArguments(injection, value);
end;

function TComponentModel.GetInjectionArguments(
  const injection: IInjection): TArray<TValue>;
begin
  TArgument.CheckNotNull(injection, 'injection');
  Injections.TryGetValue(injection, Result);
end;

function TComponentModel.GetComponentTypeInfo: PTypeInfo;
begin
  Result := ComponentType.Handle;
end;

function TComponentModel.GetInjectionFactory: IInjectionFactory;
begin
  Result := fContext.InjectionFactory;
end;

function TComponentModel.GetConstructorInjections: IList<IInjection>;
begin
  if fConstructorInjections = nil then
  begin
    fConstructorInjections := TCollections.CreateList<IInjection>;
  end;
  Result := fConstructorInjections;
end;

function TComponentModel.GetMethodInjections: IList<IInjection>;
begin
  if fMethodInjections = nil then
  begin
    fMethodInjections := TCollections.CreateList<IInjection>;
  end;
  Result := fMethodInjections;
end;

function TComponentModel.GetPropertyInjections: IList<IInjection>;
begin
  if fPropertyInjections = nil then
  begin
    fPropertyInjections := TCollections.CreateList<IInjection>;
  end;
  Result := fPropertyInjections;
end;

function TComponentModel.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := fServices.Values.Contains(serviceType);
end;

function TComponentModel.GetServiceName(serviceType: PTypeInfo): string;
var
  item: TPair<string, PTypeInfo>;
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');
  Result := '';
  for item in Services do
  begin
    if item.Value = serviceType then
    begin
      Exit(item.Key);
    end;
  end;
end;

function TComponentModel.GetServiceType(const name: string): PTypeInfo;
begin
  Result := fServices[name];
end;

function TComponentModel.GetServices: IDictionary<string, PTypeInfo>;
begin
  if fServices = nil then
  begin
    fServices := TCollections.CreateDictionary<string, PTypeInfo>;
  end;
  Result := fServices;
end;

function TComponentModel.GetFieldInjections: IList<IInjection>;
begin
  if fFieldInjections = nil then
  begin
    fFieldInjections := TCollections.CreateList<IInjection>;
  end;
  Result := fFieldInjections;
end;

function TComponentModel.GetInjections: IDictionary<IInjection, TArray<TValue>>;
begin
  if fInjectionArguments = nil then
  begin
    fInjectionArguments := TCollections.CreateDictionary<IInjection, TArray<TValue>>;
  end;
  Result := fInjectionArguments;
end;

{$ENDREGION}


end.
