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
  Spring.DesignPatterns;

type
  { Forward Declarations }
  TComponentModel = class;
  IServiceRegistry = interface;
  IRegistrationInspector = interface;
  IServiceResolver = interface;
  IDependencyResolver = interface;
  IInjection = interface;
  ILifetimeManager = interface;
  IComponentActivator = interface;

  TActivatorDelegate = reference to function: TObject;
  TActivatorDelegate<T> = reference to function: T;

  /// <summary>
  /// Injection Type Enumeration
  /// </summary>
  TInjectionType = (
    itUnknown,
    itConstructor,
    itProperty,
    itMethod,
    itField
  );

  /// <summary>
  /// IContainerContext
  /// </summary>
  IContainerContext = interface
    ['{9E90EADB-A720-4394-A5E0-5DF0550C1E92}']
    function GetDependencyResolver: IDependencyResolver;
//    function CanInject(model: TComponentModel; member: TRttiMember): Boolean;
//    function CreateInjection(model: TComponentModel; member: TRttiMember): IInjection;
    function CreateLifetimeManager(model: TComponentModel): ILifetimeManager;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver; 
//    property ServiceRegistry: IServiceRegistry;
//    property InjectionFactory: IInjectionFactory;
  end;

  /// <summary>
  /// Manages the registration of services.
  /// </summary>
  IServiceRegistry = interface
    ['{CBCA1D0F-1244-4AB4-AB07-091053932166}']

    // Inspectors
    procedure AddInspector(const inspector: IRegistrationInspector);
    procedure RemoveInspector(const inspector: IRegistrationInspector);
    procedure ClearInspectors;

    // Registration
    procedure RegisterType(const name: string; serviceType, componentType: PTypeInfo;
      lifetimeType: TLifetimeType; activatorDelegate: TActivatorDelegate); // lifetimeManager: ILifetimeManager;
    procedure UnregisterAll;

    // Query
    function FindOne(serviceType: PTypeInfo): TComponentModel; overload;
    function FindOne(serviceType: PTypeInfo; const name: string): TComponentModel; overload;
    function FindOneByComponentType(componentType: PTypeInfo): TComponentModel;
    function FindAll(serviceType: PTypeInfo): TArray<TComponentModel>;
    function HasServiceType(serviceType: PTypeInfo): Boolean;
  end;

  /// <summary>
  /// IRegistrationInspector
  /// </summary>
  IRegistrationInspector = interface
    ['{3E2F36D1-2C0D-4D6A-91B3-49B09BD31318}']
    procedure ProcessModel(const context: IContainerContext; model: TComponentModel);
  end;

  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance: TObject;
    procedure ReleaseInstance(instance: TObject);
  end;

  (*
  IObjectActivator = interface
    ['{F827E7B0-FBAA-48CA-9DAE-A21E68AF5F22}']
    function CreateInstance: TObject;
    procedure DestroyInstance(instance: TObject);
  end;

  // MinPoolSize, MaxPoolSize, Timeout
  IObjectPool<T> = interface
    ['{C3318FC1-8674-4733-A5F8-4DB660751915}']
    function GetInstance: T;
    procedure Release(instance: T);
  end;
  //*)

  IComponentActivator = interface
    ['{752F2CDE-222C-4D8B-B344-BB7BCA9EAB9E}']
    function CreateInstance: TObject;
  end;

  /// <summary>
  /// Represents a member injection. e.g. constructor, method, property and
  /// even field injection.
  /// </summary>
  // (IInjectable)
  IInjection = interface
    ['{864AAA38-4F93-4BB9-AD8A-B796FCD2EFE0}']
    {$REGION 'Property Getters & Setters'}
      function GetDependencyCount: Integer;
      function GetMemberType: TRttiMember;
      function GetModel: TComponentModel;
    {$ENDREGION}
    procedure Inject(instance: TObject; const arguments: array of TValue);
    function GetDependencies: TArray<TRttiType>;
    property DependencyCount: Integer read GetDependencyCount;
    property MemberType: TRttiMember read GetMemberType;
    property Model: TComponentModel read GetModel;
//    property IsOptional: Boolean read GetIsOptional;
  end;

  IInjectionManager = interface
    ['{9C7013B8-B10F-4BAE-BD94-44B979D58639}']
    function CanInject(model: TComponentModel; member: TRttiMember): Boolean;
    function CreateInjection(model: TComponentModel; member: TRttiMember): IInjection;
  end;

  /// <summary>
  /// Resolves services.
  /// </summary>
  IServiceResolver = interface
    ['{14669EBA-4E57-4DF4-919D-377D8E90144C}']
    function CanResolve(typeInfo: PTypeInfo): Boolean;
    function Resolve(typeInfo: PTypeInfo; const name: string): TValue;
  end;

  IDependencyResolver = interface
    ['{15ADEA1D-7C3F-48D5-8E85-84B4332AFF5F}']
    function CanResolve(const member: IInjection): Boolean;
    function ResolveDependencies(const member: IInjection): TArray<TValue>;
  end;

  TComponentModel = class
  private
    fName: string;
    fServiceType: TRttiType;
    fComponentType: TRttiInstanceType;
    fComponentActivator: IComponentActivator;
    fLifetimeType: TLifetimeType;
    fLifetimeManager: ILifetimeManager;
    fInjectionArguments: TDictionary<TRttiMember, TArray<TValue>>;
    fConstructors: IList<IInjection>;
    fProperties: IList<IInjection>;
    fMethods: IList<IInjection>;
    fFields: IList<IInjection>;
    fActivatorDelegate: TActivatorDelegate;
    function GetConstructors: IList<IInjection>;
    function GetProperties: IList<IInjection>;
    function GetMethods: IList<IInjection>;
    function GetFields: IList<IInjection>;
    function GetInjectionArguments: TDictionary<TRttiMember, TArray<TValue>>;
    function GetComponentTypeInfo: PTypeInfo;
    function GetServiceTypeInfo: PTypeInfo;
  public
    constructor Create(const name: string; serviceType: TRttiType; componentType: TRttiInstanceType);
    procedure AddOrUpdateInjectionArguments(member: TRttiMember; const arguments: array of TValue);
    property Name: string read fName;
    property ServiceType: TRttiType read fServiceType;
    property ServiceTypeInfo: PTypeInfo read GetServiceTypeInfo;
    property ComponentType: TRttiInstanceType read fComponentType;
    property ComponentTypeInfo: PTypeInfo read GetComponentTypeInfo;
    property ComponentActivator: IComponentActivator read fComponentActivator write fComponentActivator;
    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
    property Constructors: IList<IInjection> read GetConstructors;
    property Properties: IList<IInjection> read GetProperties;
    property Methods: IList<IInjection> read GetMethods;
    property Fields: IList<IInjection> read GetFields;
    property ActivatorDelegate: TActivatorDelegate read fActivatorDelegate write fActivatorDelegate;
    property InjectionArguments: TDictionary<TRttiMember, TArray<TValue>> read GetInjectionArguments;
  end;

  EContainerException = class(SysUtils.Exception);
  ERegistrationException = class(EContainerException);
  EResolveException = class(EContainerException);
  ECircularDependencyException = class(EResolveException);
  EActivatorException = class(EContainerException);

implementation

uses
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings;


{$REGION 'TComponentModel'}

constructor TComponentModel.Create(const name: string;
  serviceType: TRttiType; componentType: TRttiInstanceType);
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');
  TArgument.CheckNotNull(componentType, 'componentType');

  inherited Create;
  fName := name;
  fServiceType := serviceType;
  fComponentType := componentType;
end;

procedure TComponentModel.AddOrUpdateInjectionArguments(member: TRttiMember;
  const arguments: array of TValue);
var
  i: Integer;
  values: TArray<TValue>;
begin
  TArgument.CheckNotNull(member, 'member');

  for i := 0 to High(arguments) do
  begin
    values[i] := arguments[i];
  end;
  fInjectionArguments.AddOrSetValue(member, values);
end;

function TComponentModel.GetConstructors: IList<IInjection>;
begin
  if fConstructors = nil then
  begin
    fConstructors := TCollections.CreateList<IInjection>;
  end;
  Result := fConstructors;
end;

function TComponentModel.GetProperties: IList<IInjection>;
begin
  if fProperties = nil then
  begin
    fProperties := TCollections.CreateList<IInjection>;
  end;
  Result := fProperties;
end;

function TComponentModel.GetMethods: IList<IInjection>;
begin
  if fMethods = nil then
  begin
    fMethods := TCollections.CreateList<IInjection>;
  end;
  Result := fMethods;
end;

function TComponentModel.GetFields: IList<IInjection>;
begin
  if fFields = nil then
  begin
    fFields := TCollections.CreateList<IInjection>;
  end;
  Result := fFields;
end;

function TComponentModel.GetInjectionArguments: TDictionary<TRttiMember, TArray<TValue>>;
begin
  if fInjectionArguments = nil then
  begin
    fInjectionArguments := TDictionary<TRttiMember, TArray<TValue>>.Create;
  end;
  Result := fInjectionArguments;
end;

function TComponentModel.GetServiceTypeInfo: PTypeInfo;
begin
  Result := ServiceType.Handle;
end;

function TComponentModel.GetComponentTypeInfo: PTypeInfo;
begin
  Result := ComponentType.Handle;
end;

{$ENDREGION}

end.
