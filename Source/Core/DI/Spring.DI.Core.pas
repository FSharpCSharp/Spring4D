{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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

unit Spring.DI.Core;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Spring,
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

    function FindOne(componentType: PTypeInfo): TComponentModel; overload;
    function FindOne(const name: string): TComponentModel; overload;
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

  IDependencyResolver = interface
    ['{15ADEA1D-7C3F-48D5-8E85-84B4332AFF5F}']
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
  IServiceResolver = interface
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


  {$REGION 'Documentation'}
  ///	<summary>
  ///	  <para>Provides a simple &amp; flexible implementation of <b>Smart
  ///	  Pointer</b>. This implementation is very skillful and the basic idea
  ///	  comes from a post in Kelly Barry's blog.</para>
  ///	  <para>The point is to use an anonymous method <c>TFunc&lt;T&gt;,</c>
  ///	  which is internally implemented as an interface in Delphi for Win32, to
  ///	  manage the lifetime of an object instance.</para>
  ///	</summary>
  ///	<example>
  ///	  The following example demonstrates how to use the Smart Pointer:
  ///	  <code lang="Delphi">
  ///	procedure TestSmartPointer;
  ///	var
  ///	  person: TFunc&lt;TPerson&gt;;
  ///	begin
  ///	  person := TObjectHolder&lt;TPerson&gt;.Create(TPerson.Create);
  ///	  person.DoSomething;
  ///	end;
  ///	</code>
  ///	</example>
  {$ENDREGION}
  TObjectHolder<T: class> = class(TInterfacedObject, TFunc<T>)
  private
    fObject: T;
    fLifetimeWatcher: IInterface;
  public
    constructor Create(obj: T); overload;
    constructor Create(obj: T; const lifetimeWatcher: IInterface); overload;
    destructor Destroy; override;
    function Invoke: T;
  end;

  TObjectHolder = TObjectHolder<TObject>;

  EContainerException = class(Exception);

  ERegistrationException = class(EContainerException);
  EBuilderException = class(EContainerException);
  EInjectionException = class(EContainerException);

  EResolveException = class(EContainerException);
  ECircularDependencyException = class(EResolveException);
  EUnsatisfiedDependencyException = class(EResolveException);

  EActivatorException = class(EContainerException);

implementation

uses
  Spring.Helpers,
  Generics.Collections,
  Spring.ResourceStrings,
  Spring.DI.ResourceStrings;

{$REGION 'TArrayHelper'}

type
  TArrayHelper = class helper for TArray
  public
    class function CreateArray<T>(const values: array of T): TArray<T>; // deprecated;
  end;

class function TArrayHelper.CreateArray<T>(const values: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := 0 to High(values) do
  begin
    Result[i] := values[i];
  end;
end;

{$ENDREGION}


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
  method := ComponentType.Methods.FirstOrDefault(predicate);
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
  method := ComponentType.Methods.FirstOrDefault(predicate);
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


{$REGION 'TObjectHolder<T>'}

constructor TObjectHolder<T>.Create(obj: T);
var
  lifetimeWatcher: IInterface;
begin
  TArgument.CheckNotNull(PPointer(@obj)^, 'obj');

  if obj.InheritsFrom(TInterfacedObject) then
  begin
    obj.GetInterface(IInterface, lifetimeWatcher);
  end
  else
  begin
    lifetimeWatcher := nil;
  end;
  Create(obj, lifetimeWatcher);
end;

constructor TObjectHolder<T>.Create(obj: T; const lifetimeWatcher: IInterface);
begin
  inherited Create;
  fObject := obj;
  fLifetimeWatcher := lifetimeWatcher;
end;

destructor TObjectHolder<T>.Destroy;
begin
  if fLifetimeWatcher = nil then
  begin
    fObject.Free;
  end;
  inherited Destroy;
end;

function TObjectHolder<T>.Invoke: T;
begin
  Result := fObject;
end;

{$ENDREGION}

end.
