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
  ISubDependencyResolver = interface;
  IDependencyResolver = interface;
  IInjection = interface;
  IInjector = interface;
  ILifetimeManager = interface;
  IComponentActivator = interface;
  IContainerExtension = interface;
  ICreationContext = interface;

  TActivatorDelegate = reference to function: TValue;
  TActivatorDelegate<T> = reference to function: T;

  /// <summary>
  ///   The <c>IKernel</c> interface exposes all the functionality the
  ///   container implements.
  /// </summary>
  IKernel = interface
    ['{9E90EADB-A720-4394-A5E0-5DF0550C1E92}']
  {$REGION 'Property Accessors'}
    function GetBuilder: IComponentBuilder;
    function GetInjector: IInjector;
    function GetRegistry: IComponentRegistry;
    function GetResolver: IDependencyResolver;
  {$ENDREGION}
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;

    procedure AddExtension(const extension: IContainerExtension);

    property Builder: IComponentBuilder read GetBuilder;
    property Injector: IInjector read GetInjector;
    property Registry: IComponentRegistry read GetRegistry;
    property Resolver: IDependencyResolver read GetResolver;
  end;

  IKernelInternal = interface
    ['{14669EBA-4E57-4DF4-919D-377D8E90144C}']
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo;
      const arguments: array of TValue): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function Resolve(const name: string;
      const arguments: array of TValue): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  /// <summary>
  ///   Extends the container.
  /// </summary>
  IContainerExtension = interface
    ['{E78748FB-D75C-447C-B984-9782A8F26C20}']
    procedure Initialize;
    procedure InitializeExtension(const kernel: IKernel);
  end;

  /// <summary>
  ///   Manages the registration of components.
  /// </summary>
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
    function FindOne(serviceType: PTypeInfo; const argument: TValue): TComponentModel; overload;
    function FindDefault(serviceType: PTypeInfo): TComponentModel;
    function FindAll: IEnumerable<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): IEnumerable<TComponentModel>; overload;
  end;

  /// <summary>
  ///   Component Builder
  /// </summary>
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

  /// <summary>
  ///   Builder Inspector
  /// </summary>
  IBuilderInspector = interface
    ['{3E2F36D1-2C0D-4D6A-91B3-49B09BD31318}']
    procedure ProcessModel(const kernel: IKernel; const model: TComponentModel);
  end;

  /// <summary>
  ///   Lifetime Manager
  /// </summary>
  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function Resolve(const context: ICreationContext): TValue;
    procedure Release(const instance: TValue);
  end;

  /// <summary>
  ///   Component Activator
  /// </summary>
  IComponentActivator = interface
    ['{18E6DF78-C947-484F-A0A8-D9A5B0BEC887}']
    function CreateInstance(const context: ICreationContext): TValue;
  end;

  /// <summary>
  ///   Represents an injection of a member. e.g. constructor, method, property
  ///   and even field.
  /// </summary>
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

  IInjector = interface
    ['{4627FFB4-F8D9-4A70-A281-8D323FAAA4CB}']
  {$REGION 'Typed Injections'}
    function InjectConstructor(const model: TComponentModel;
      const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectMethod(const model: TComponentModel;
      const methodName: string): IInjection; overload;
    function InjectMethod(const model: TComponentModel; const methodName: string;
      const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectProperty(const model: TComponentModel;
      const propertyName: string): IInjection; overload;
    function InjectField(const model: TComponentModel;
      const fieldName: string): IInjection; overload;
  {$ENDREGION}

  {$REGION 'Named/Valued Injections'}
    function InjectConstructor(const model: TComponentModel;
      const arguments: array of TValue): IInjection; overload;
    function InjectMethod(const model: TComponentModel; const methodName: string;
      const arguments: array of TValue): IInjection; overload;
    function InjectProperty(const model: TComponentModel;
      const propertyName: string; const value: TValue): IInjection; overload;
    function InjectField(const model: TComponentModel;
      const fieldName: string; const value: TValue): IInjection; overload;
  {$ENDREGION}
  end;

  TResolveEvent = procedure(Sender: TObject; var instance: TValue) of object;

  ISubDependencyResolver = interface
    ['{E360FFAD-2235-49D1-9A4F-50945877E337}']
    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean;
    function Resolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): TValue;
  end;

  /// <summary>
  ///   Used during a component request, passed along to the whole process.
  ///   This will allow some data to be passed along the process, which is used
  ///   to detect cycled dependency graphs and also being used to provide
  ///   arguments to components.
  /// </summary>
  ICreationContext = interface(ISubDependencyResolver)
    ['{0E788A94-AD9B-4951-85C1-40F877BB8A24}']
    procedure EnterResolution(const model: TComponentModel);
    procedure LeaveResolution(const model: TComponentModel);
    function IsInResolution(const model: TComponentModel): Boolean;

    procedure AddArgument(const argument: TValue);
    function TryHandle(const injection: IInjection;
      out handled: IInjection): Boolean;
  end;

  IDependencyResolver = interface(ISubDependencyResolver)
    ['{15ADEA1D-7C3F-48D5-8E85-84B4332AFF5F}']
    function CanResolve(const context: ICreationContext;
      const dependencies: TArray<TRttiType>;
      const arguments: TArray<TValue>): Boolean; overload;
    function Resolve(const context: ICreationContext;
      const dependencies: TArray<TRttiType>;
      const arguments: TArray<TValue>): TArray<TValue>; overload;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  /// <summary>
  ///   TComponentModel
  /// </summary>
  TComponentModel = class
  private
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
  public
    constructor Create(const componentType: TRttiType);

    function HasService(serviceType: PTypeInfo): Boolean;
    function GetServiceName(serviceType: PTypeInfo): string;
    function GetServiceType(const name: string): PTypeInfo;

    property ComponentType: TRttiType read fComponentType;
    property ComponentTypeInfo: PTypeInfo read GetComponentTypeInfo;
    property Services: IDictionary<string, PTypeInfo> read fServices;
    property MinPoolsize: Integer read fMinPoolsize write fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize write fMaxPoolsize;
    property RefCounting: TRefCounting read fRefCounting write fRefCounting;

    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
    property ComponentActivator: IComponentActivator read fComponentActivator write fComponentActivator;
    property ActivatorDelegate: TActivatorDelegate read fActivatorDelegate write fActivatorDelegate;

    property ConstructorInjections: IInjectionList read fConstructorInjections;
    property MethodInjections: IInjectionList read fMethodInjections;
    property PropertyInjections: IInjectionList read fPropertyInjections;
    property FieldInjections: IInjectionList read fFieldInjections;
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
    fKernel: IKernel;
    fArguments: TArray<TValue>;
  public
    constructor Create(const kernel: IKernel; const arguments: TArray<TValue>);
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
    class function IsInjectableMethod(const kernel: IKernel;
      const arguments: TArray<TValue>): TSpecification<TRttiMethod>;
  end;

implementation

uses
  Generics.Collections,
  TypInfo,
  Spring.Container.ResourceStrings,
  Spring.Helpers;


{$REGION 'TComponentModel'}

constructor TComponentModel.Create(const componentType: TRttiType);
begin
  inherited Create;
  fComponentType := componentType;
  fServices := TCollections.CreateDictionary<string, PTypeInfo>;
  fConstructorInjections := TCollections.CreateInterfaceList<IInjection>;
  fMethodInjections := TCollections.CreateInterfaceList<IInjection>;
  fPropertyInjections := TCollections.CreateInterfaceList<IInjection>;
  fFieldInjections := TCollections.CreateInterfaceList<IInjection>;
end;

function TComponentModel.GetComponentTypeInfo: PTypeInfo;
begin
  Result := fComponentType.Handle;
end;

function TComponentModel.GetServiceName(serviceType: PTypeInfo): string;
begin
  Guard.CheckNotNull(serviceType, 'serviceType');
  Result := fServices.FirstOrDefault(
    function(const item: TPair<string, PTypeInfo>): Boolean
    begin
      Result := item.Value = serviceType;
    end).Key;
end;

function TComponentModel.GetServiceType(const name: string): PTypeInfo;
begin
  Result := fServices[name];
end;

function TComponentModel.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := fServices.ContainsValue(serviceType);
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

constructor TInjectableMethodFilter.Create(const kernel: IKernel;
  const arguments: TArray<TValue>);
begin
  inherited Create;
  fKernel := kernel;
  fArguments := arguments;
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
  Result := fKernel.Resolver.CanResolve(nil, dependencies, fArguments);
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
  Result := injection.Target = fMember;
end;

{$ENDREGION}


{$REGION 'TInjectionFilters'}

class function TInjectionFilters.ContainsMember(
  const member: TRttiMember): TSpecification<IInjection>;
begin
  Result := TContainsMemberFilter.Create(member);
end;

class function TInjectionFilters.IsInjectableMethod(const kernel: IKernel;
  const arguments: TArray<TValue>): TSpecification<TRttiMethod>;
begin
  Result := TInjectableMethodFilter.Create(kernel, arguments);
end;

{$ENDREGION}


end.
