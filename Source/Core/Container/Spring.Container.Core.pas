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

unit Spring.Container.Core;

{$I Spring.inc}

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Common,
  Spring.DesignPatterns;

type
  { Forward Declarations }
  TComponentModel = class;
  IComponentBuilder = interface;
  IComponentRegistry = interface;
  IBuilderInspector = interface;
  IServiceResolver = interface;
  IDependencyResolver = interface;
  IInjection = interface;
  IInjectionFactory = interface;
  ILifetimeManager = interface;
  IContainerExtension = interface;

  TActivatorDelegate = reference to function: TValue;
  TActivatorDelegate<T> = reference to function: T;

  ///	<summary>
  ///	  IContainerContext
  ///	</summary>
  IContainerContext = interface
    ['{9E90EADB-A720-4394-A5E0-5DF0550C1E92}']
  {$REGION 'Property Accessors'}
    function GetComponentBuilder: IComponentBuilder;
    function GetComponentRegistry: IComponentRegistry;
    function GetDependencyResolver: IDependencyResolver;
    function GetInjectionFactory: IInjectionFactory;
    function GetServiceResolver: IServiceResolver;
  {$ENDREGION}
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;
    function CreateLifetimeManager(const model: TComponentModel): ILifetimeManager;
    procedure AddExtension(const extension: IContainerExtension);
    property ComponentBuilder: IComponentBuilder read GetComponentBuilder;
    property ComponentRegistry: IComponentRegistry read GetComponentRegistry;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
    property ServiceResolver: IServiceResolver read GetServiceResolver;
    property ComponentBuilder: IComponentBuilder read GetComponentBuilder;
  end;

  ///	<summary>
  ///	  Extends the container
  ///	</summary>
  IContainerExtension = interface
    ['{E78748FB-D75C-447C-B984-9782A8F26C20}']
    procedure Initialize;
    procedure InitializeExtension(const context: IContainerContext);
  end;

  ///	<summary>
  ///	  Manages the registration of components. (IComponentBuilder)
  ///	</summary>
  IComponentRegistry = interface
    ['{CBCA1D0F-1244-4AB4-AB07-091053932166}']
    function RegisterComponent(componentType: PTypeInfo): TComponentModel;
    procedure RegisterService(const model: TComponentModel; serviceType: PTypeInfo); overload;
    procedure RegisterService(const model: TComponentModel; serviceType: PTypeInfo; const name: string); overload;
    procedure RegisterDefault(const model: TComponentModel; serviceType: PTypeInfo);
{$IFDEF DELPHIXE_UP}
    procedure RegisterFactory(const model: TComponentModel); overload;
    procedure RegisterFactory(const model: TComponentModel; const name: string); overload;
{$ENDIF}
    procedure UnregisterAll;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;
    function HasDefault(serviceType: PTypeInfo): Boolean;

    function FindOne(componentType: PTypeInfo): TComponentModel; overload;
    function FindOne(const name: string): TComponentModel; overload;
    function FindDefault(serviceType: PTypeInfo): TComponentModel;
    function FindAll: IEnumerable<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): IEnumerable<TComponentModel>; overload;
  end;

  IComponentBuilder = interface
    ['{8309EBC7-9699-47CF-B177-4BC9B787EBE0}']
    // Inspectors (Policies)
    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    // Build
    procedure Build(const model: TComponentModel);
    procedure BuildAll;
  end;

  ///	<summary>
  ///	  IBuilderInspector (IBuilderPolicy)
  ///	</summary>
  IBuilderInspector = interface
    ['{3E2F36D1-2C0D-4D6A-91B3-49B09BD31318}']
    procedure ProcessModel(const context: IContainerContext; const model: TComponentModel);
  end;

  ///	<summary>
  ///	  ILifetimeManager
  ///	</summary>
  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance(const resolver: IDependencyResolver): TValue;
    procedure ReleaseInstance(const instance: TValue);
  end;

  ///	<summary>
  ///	  Component Activator
  ///	</summary>
  IComponentActivator = interface
    ['{18E6DF78-C947-484F-A0A8-D9A5B0BEC887}']
    function CreateInstance(const resolver: IDependencyResolver): TValue;
  end;

  ///	<summary>
  ///	  Represents an Inject of a member. e.g. constructor, method, property
  ///	  and even field Inject.
  ///	</summary>
  IInjection = interface
    ['{864AAA38-4F93-4BB9-AD8A-B796FCD2EFE0}']
  {$REGION 'Property Accessors'}
    function GetDependencyCount: Integer;
    function GetTarget: TRttiMember;
    function GetTargetName: string;
    function GetHasTarget: Boolean;
    function GetArguments: TArray<TValue>;
    function GetDependencies: TArray<TRttiType>;
  {$ENDREGION}

    procedure Initialize(target: TRttiMember);
    procedure InitializeArguments(const arguments: array of TValue);
    procedure Inject(const instance: TValue; const arguments: array of TValue);

    property DependencyCount: Integer read GetDependencyCount;
    property Target: TRttiMember read GetTarget;
    property TargetName: string read GetTargetName;
    property HasTarget: Boolean read GetHasTarget;
    property Arguments: TArray<TValue> read GetArguments;
    property Dependencies: TArray<TRttiType> read GetDependencies;
  end;

  IInjectionList = IList<IInjection>;

  ///	<summary>
  ///	  Inject Factory
  ///	</summary>
  IInjectionFactory = interface
    ['{EA75E648-C3EB-4CE7-912A-AB82B12BBD87}']
    function CreateConstructorInjection: IInjection;
    function CreateMethodInjection(const methodName: string): IInjection;
    function CreatePropertyInjection(const propertyName: string): IInjection;
    function CreateFieldInjection(const fieldName: string): IInjection;
  end;

  TResolveEvent = procedure(Sender: TObject; var instance: TValue) of object;

  IResolver = interface
    ['{EA0ABA0F-BED0-4897-9E50-133184E105B7}']
    function GetOnResolve: IEvent<TResolveEvent>;
    property OnResolve: IEvent<TResolveEvent> read GetOnResolve;
  end;

  IDependencyResolver = interface(IResolver)
    ['{15ADEA1D-7C3F-48D5-8E85-84B4332AFF5F}']
    function CanResolveDependency(const dependency: TRttiType; const argument: TValue): Boolean;
    function ResolveDependency(const dependency: TRttiType; const argument: TValue): TValue;

    function CanResolveDependencies(const dependencies: TArray<TRttiType>;
      const arguments: TArray<TValue>; const target: TRttiMember): Boolean;
    function ResolveDependencies(const dependencies: TArray<TRttiType>;
      const arguments: TArray<TValue>; const target: TRttiMember): TArray<TValue>;
  end;

  ///	<summary>
  ///	  Overrides the resolver.
  ///	</summary>
  IResolverOverride = interface
    ['{2DA386A3-949C-451F-BF22-017668689591}']
    function GetResolver(const context: IContainerContext): IDependencyResolver;
  end;

  ///	<summary>
  ///	  Resolves services.
  ///	</summary>
  IServiceResolver = interface(IResolver)
    ['{14669EBA-4E57-4DF4-919D-377D8E90144C}']
    function CanResolve(serviceType: PTypeInfo): Boolean; overload;
    function CanResolve(const name: string): Boolean; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo; const resolverOverride: IResolverOverride): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function Resolve(const name: string; const resolverOverride: IResolverOverride): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  ///	<summary>
  ///	  TComponentModel
  ///	</summary>
  TComponentModel = class
  private
    fContext: IContainerContext;
    fComponentType: TRttiType;
    fLifetimeType: TLifetimeType;
    fLifetimeManager: ILifetimeManager;
    fComponentActivator: IComponentActivator;
    fActivatorDelegate: TActivatorDelegate;
    fMinPoolsize: Integer;
    fMaxPoolsize: Integer;
    fRefCounting: TRefCounting;
    fServices: IDictionary<string, PTypeInfo>;
    fConstructorInjections: IInjectionList;
    fMethodInjections: IInjectionList;
    fPropertyInjections: IInjectionList;
    fFieldInjections: IInjectionList;
    function GetComponentTypeInfo: PTypeInfo;
    function GetInjectionFactory: IInjectionFactory;
  protected
    function GetServices: IDictionary<string, PTypeInfo>;
    function GetConstructorInjections: IInjectionList;
    function GetMethodInjections: IInjectionList;
    function GetPropertyInjections: IInjectionList;
    function GetFieldInjections: IInjectionList;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
  public
    constructor Create(const context: IContainerContext; componentType: TRttiType);

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

    property ComponentType: TRttiType read fComponentType;
    property ComponentTypeInfo: PTypeInfo read GetComponentTypeInfo;
    property Services: IDictionary<string, PTypeInfo> read GetServices;
    property MinPoolsize: Integer read fMinPoolsize write fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize write fMaxPoolsize;
    property RefCounting: TRefCounting read fRefCounting write fRefCounting;

    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
    property ComponentActivator: IComponentActivator read fComponentActivator write fComponentActivator;
    property ActivatorDelegate: TActivatorDelegate read fActivatorDelegate write fActivatorDelegate;

    property ConstructorInjections: IInjectionList read GetConstructorInjections;
    property MethodInjections: IInjectionList read GetMethodInjections;
    property PropertyInjections: IInjectionList read GetPropertyInjections;
    property FieldInjections: IInjectionList read GetFieldInjections;
  end;

  TValueHolder = class(TInterfacedObject, TFunc<TValue>)
  private
    fValue: TValue;
    fLifetimeWatcher: IInterface;
  public
    constructor Create(const value: TValue; refCounting: TRefCounting); overload;
    constructor Create(const value: TValue; const lifetimeWatcher: IInterface); overload;
    destructor Destroy; override;
    function Invoke: TValue;
  end;

  EContainerException = class(Exception);

  ERegistrationException = class(EContainerException);
  EBuilderException = class(EContainerException);
  EInjectionException = class(EContainerException);

  EResolveException = class(EContainerException);
  ECircularDependencyException = class(EResolveException);
  EUnsatisfiedDependencyException = class(EResolveException);

  EActivatorException = class(EContainerException);

  TInjectableMethodFilter = class(TSpecificationBase<TRttiMethod>)
  private
    fContext: IContainerContext;
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fModel: TComponentModel;
    fInjection: IInjection;
    fArguments: TArray<TValue>;
  public
    constructor Create(const context: IContainerContext;
      const model: TComponentModel; const injection: IInjection);
    function IsSatisfiedBy(const method: TRttiMethod): Boolean; override;
  end;

  TContainsMemberFilter = class(TSpecificationBase<IInjection>)
  private
    fMember: TRttiMember;
  public
    constructor Create(const member: TRttiMember);
    function IsSatisfiedBy(const injection: IInjection): Boolean; override;
  end;

  TInjectionFilters = class
  public
    class function ContainsMember(const member: TRttiMember): TSpecification<IInjection>;
    class function IsInjectableMethod(const context: IContainerContext;
      const model: TComponentModel; const injection: IInjection): TSpecification<TRttiMethod>;
  end;

implementation

uses
  Generics.Collections,
  TypInfo,
  Spring.Container.ResourceStrings,
  Spring.Helpers,
  Spring.Reflection;


{$REGION 'TComponentModel'}

constructor TComponentModel.Create(const context: IContainerContext;
  componentType: TRttiType);
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
  predicate := TMethodFilters.IsConstructor
    and TMethodFilters.HasParameterTypes(parameterTypes);
  method := ComponentType.Methods.FirstOrDefault(predicate);
  if not Assigned(method) then
    raise ERegistrationException.CreateResFmt(
      @SUnsatisfiedConstructorParameters, [ComponentType.Name]);
  Result := InjectionFactory.CreateConstructorInjection;
  Result.Initialize(method);
  ConstructorInjections.Add(Result);
end;

function TComponentModel.InjectMethod(const methodName: string): IInjection;
var
  method: TRttiMethod;
  injectionExists: Boolean;
begin
  method := ComponentType.GetMethod(methodName);
  if not Assigned(method) then
    raise ERegistrationException.CreateResFmt(@SNoSuchMethod, [methodName]);
  injectionExists := MethodInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(method));
  if not injectionExists then
    Result := InjectionFactory.CreateMethodInjection(methodName);
  Result.Initialize(method);
  if not injectionExists then
    MethodInjections.Add(Result);
end;

function TComponentModel.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): IInjection;
var
  predicate: TPredicate<TRttiMethod>;
  method: TRttiMethod;
  injectionExists: Boolean;
begin
  predicate := TMethodFilters.IsNamed(methodName)
    and TMethodFilters.IsInstanceMethod
    and TMethodFilters.HasParameterTypes(parameterTypes);
  method := ComponentType.Methods.FirstOrDefault(predicate);
  if not Assigned(method) then
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedMethodParameterTypes, [methodName]);
  injectionExists := MethodInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(method));
  if not injectionExists then
    Result := InjectionFactory.CreateMethodInjection(methodName);
  Result.Initialize(method);
  if not injectionExists then
    MethodInjections.Add(Result);
end;

function TComponentModel.InjectProperty(const propertyName: string): IInjection;
var
  propertyMember: TRttiProperty;
  injectionExists: Boolean;
begin
  propertyMember := ComponentType.GetProperty(propertyName);
  if not Assigned(propertyMember) then
    raise ERegistrationException.CreateResFmt(@SNoSuchProperty, [propertyName]);
  injectionExists := PropertyInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(propertyMember));
  if not injectionExists then
    Result := InjectionFactory.CreatePropertyInjection(propertyName);
  Result.Initialize(propertyMember);
  if not injectionExists then
    PropertyInjections.Add(Result);
end;

function TComponentModel.InjectField(const fieldName: string): IInjection;
var
  field: TRttiField;
  injectionExists: Boolean;
begin
  field := ComponentType.GetField(fieldName);
  if not Assigned(field) then
    raise ERegistrationException.CreateResFmt(@SNoSuchField, [fieldName]);
  injectionExists := FieldInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(field));
  if not injectionExists then
    Result := InjectionFactory.CreateFieldInjection(fieldName);
  Result.Initialize(field);
  if not injectionExists then
    FieldInjections.Add(Result);
end;

procedure TComponentModel.InjectConstructor(const arguments: array of TValue);
var
  Inject: IInjection;
begin
  Inject := InjectionFactory.CreateConstructorInjection;
  ConstructorInjections.Add(Inject);
  Inject.InitializeArguments(arguments);
end;

procedure TComponentModel.InjectMethod(const methodName: string;
  const arguments: array of TValue);
var
  Inject: IInjection;
begin
  Inject := InjectionFactory.CreateMethodInjection(methodName);
  MethodInjections.Add(Inject);
  Inject.InitializeArguments(arguments);
end;

procedure TComponentModel.InjectProperty(const propertyName: string;
  const value: TValue);
var
  Inject: IInjection;
begin
  Inject := InjectProperty(propertyName);
  Inject.InitializeArguments(value);
end;

procedure TComponentModel.InjectField(const fieldName: string;
  const value: TValue);
var
  Inject: IInjection;
begin
  Inject := InjectField(fieldName);
  Inject.InitializeArguments(value);
end;

function TComponentModel.GetComponentTypeInfo: PTypeInfo;
begin
  Result := ComponentType.Handle;
end;

function TComponentModel.GetInjectionFactory: IInjectionFactory;
begin
  Result := fContext.InjectionFactory;
end;

function TComponentModel.GetConstructorInjections: IInjectionList;
begin
  if not Assigned(fConstructorInjections) then
    fConstructorInjections := TCollections.CreateInterfaceList<IInjection>;
  Result := fConstructorInjections;
end;

function TComponentModel.GetMethodInjections: IInjectionList;
begin
  if not Assigned(fMethodInjections) then
    fMethodInjections := TCollections.CreateInterfaceList<IInjection>;
  Result := fMethodInjections;
end;

function TComponentModel.GetPropertyInjections: IInjectionList;
begin
  if not Assigned(fPropertyInjections) then
    fPropertyInjections := TCollections.CreateInterfaceList<IInjection>;
  Result := fPropertyInjections;
end;

function TComponentModel.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := Assigned(fServices) and fServices.Values.Contains(serviceType);
end;

function TComponentModel.GetServiceName(serviceType: PTypeInfo): string;
var
  item: TPair<string, PTypeInfo>;
begin
  Guard.CheckNotNull(serviceType, 'serviceType');
  for item in Services do
    if item.Value = serviceType then
      Exit(item.Key);
  Result := '';
end;

function TComponentModel.GetServiceType(const name: string): PTypeInfo;
begin
  Result := fServices[name];
end;

function TComponentModel.GetServices: IDictionary<string, PTypeInfo>;
begin
  if not Assigned(fServices) then
    fServices := TCollections.CreateDictionary<string, PTypeInfo>;
  Result := fServices;
end;

function TComponentModel.GetFieldInjections: IInjectionList;
begin
  if not Assigned(fFieldInjections) then
    fFieldInjections := TCollections.CreateInterfaceList<IInjection>;
  Result := fFieldInjections;
end;

{$ENDREGION}


{$REGION 'TValueHolder'}

constructor TValueHolder.Create(const value: TValue; refCounting: TRefCounting);
var
  lifetimeWatcher: IInterface;
begin
  Guard.CheckNotNull(not value.IsEmpty, 'value');

  if ((refCounting = TRefCounting.Unknown) and value.IsObject
    and value.AsObject.InheritsFrom(TInterfacedObject))
    or (refCounting = TRefCounting.True) then
  begin
    value.AsObject.GetInterface(IInterface, lifetimeWatcher);
  end
  else
  begin
    if value.Kind = tkInterface then
      lifetimeWatcher := value.AsInterface
    else
      lifetimeWatcher := nil;
  end;
  Create(value, lifetimeWatcher);
end;

constructor TValueHolder.Create(const value: TValue;
  const lifetimeWatcher: IInterface);
begin
  inherited Create();
  fValue := value;
  fLifetimeWatcher := lifetimeWatcher;
end;

destructor TValueHolder.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  if not Assigned(fLifetimeWatcher) and fValue.IsObject then
    fValue.AsObject.Free;
{$ELSE}
  fValue := nil;
{$ENDIF}
  inherited Destroy;
end;

function TValueHolder.Invoke: TValue;
begin
  Result := fValue;
end;

{$ENDREGION}


{$REGION 'TInjectableMethodFilter'}

constructor TInjectableMethodFilter.Create(const context: IContainerContext;
  const model: TComponentModel; const injection: IInjection);
begin
  inherited Create;
  fContext := context;
  fModel := model;
  fInjection := injection;
  fArguments := fInjection.Arguments;
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
    dependencies[i] := parameters[i].ParamType;
  Result := fContext.DependencyResolver.CanResolveDependencies(
    dependencies, fArguments, method);
end;

{$ENDREGION}


{$REGION 'TContainsMemberFilter'}

constructor TContainsMemberFilter.Create(const member: TRttiMember);
begin
  inherited Create;
  fMember := member;
end;

function TContainsMemberFilter.IsSatisfiedBy(
  const injection: IInjection): Boolean;
begin
  Result := injection.Target = fmember;
end;

{$ENDREGION}


{$REGION 'TInjectionFilters'}

class function TInjectionFilters.ContainsMember(
  const member: TRttiMember): TSpecification<IInjection>;
begin
  Result := TContainsMemberFilter.Create(member);
end;

class function TInjectionFilters.IsInjectableMethod(
  const context: IContainerContext; const model: TComponentModel;
  const injection: IInjection): TSpecification<TRttiMethod>;
begin
  Result := TInjectableMethodFilter.Create(context, model, injection);
end;

{$ENDREGION}


end.
