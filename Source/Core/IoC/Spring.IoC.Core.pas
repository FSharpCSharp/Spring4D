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
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  { Forward Declarations }
  TComponentModel = class;
  TDependency = class;
  
  IServiceRegistry = interface;
  IServiceInspector = interface;
  IServiceResolver = interface;
  IDependencyResolver = interface;
  IInjection = interface;
  ILifetimeManager = interface;
  IComponentActivator = interface;

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
    procedure AddInspector(const inspector: IServiceInspector);
    procedure RemoveInspector(const inspector: IServiceInspector);
    procedure ClearInspectors;

    // Registration
    procedure RegisterType(const name: string; serviceType, componentType: PTypeInfo;
      lifetimeType: TLifetimeType); // lifetimeManager: ILifetimeManager;
    procedure UnregisterAll;

    // Query
    function FindComponentModel(serviceType: PTypeInfo): TComponentModel;
//    function FindComponentModel(componentType: PTypeInfo): TComponentModel;
    function HasServiceType(serviceType: PTypeInfo): Boolean;
//    function FindOne(serviceType: PTypeInfo; const componentName: string): TComponentModel;
//    function FindAll(serviceType: PTypeInfo): TArray<TComponentModel>;
//    function HasModel(const componentName: string): Boolean;
  end;

  /// <summary>
  /// IServiceInspector
  /// </summary>
  IServiceInspector = interface
    ['{3E2F36D1-2C0D-4D6A-91B3-49B09BD31318}']
    procedure ProcessModel(const context: IContainerContext; model: TComponentModel);
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

  /// <summary>
  /// Manages the lifetime of components.
  /// </summary>
  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance: TObject;
    procedure Release(instance: TObject);
  end;

  IComponentActivator = interface
    ['{752F2CDE-222C-4D8B-B344-BB7BCA9EAB9E}']
    function CreateInstance(model: TComponentModel): TObject;
    procedure DestroyInstance(instance: TObject);
  end;

  /// <summary>
  /// Represents an injection member. e.g. constructor, method, property and
  /// even field injection.
  /// </summary>
  IInjection = interface
    ['{864AAA38-4F93-4BB9-AD8A-B796FCD2EFE0}']
    {$REGION 'Property Getters & Setters'}
      function GetMemberType: TRttiMember;
      function GetDependencyCount: Integer;
//      function GetName: string;
//      function GetModel: TComponentModel;
//    property Name: string read GetName;
//    property Model: TComponentModel read GetModel;
    {$ENDREGION}
//    procedure Inject(instance: TObject);
    procedure Inject(instance: TObject; const arguments: TArray<TValue>);
    function GetDependencies: TArray<TDependency>;  // TArray<TRttiType>
    property MemberType: TRttiMember read GetMemberType;
    property DependencyCount: Integer read GetDependencyCount;
//    property IsOptional: Boolean read GetIsOptional;
  end;

  IInjectionFactory = interface
    ['{D03DA351-51EF-4EAF-B190-03F42349AE86}']
//    function Accept(model: TComponentModel; member: TRttiMember): Boolean;
//    function CreateInjection(model: TComponentModel; member: TRttiMember): IInjection;
    function CreateConstructor(model: TComponentModel; constructorMethod: TRttiMethod;
      const dependencies: TArray<TDependency>): IInjection;
    function CreateProperty(model: TComponentModel; propertyMember: TRttiProperty;
      const dependencies: TArray<TDependency>): IInjection;
    function CreateMethod(model: TComponentModel; method: TRttiMethod;
      const dependencies: TArray<TDependency>): IInjection;
    function CreateField(model: TComponentModel; field: TRttiField;
      const dependencies: TArray<TDependency>): IInjection;
  end;

  TComponentModel = class
  private
    fName: string;
    fServiceType: TRttiType;
    fComponentType: TRttiType;
    fComponentActivator: IComponentActivator;
    fLifetimeType: TLifetimeType;
    fLifetimeManager: ILifetimeManager;
    fConstructors: IList<IInjection>;
    fProperties: IList<IInjection>;
    fMethods: IList<IInjection>;
    fFields: IList<IInjection>;
    function GetConstructors: IList<IInjection>;
    function GetProperties: IList<IInjection>;
    function GetMethods: IList<IInjection>;
    function GetFields: IList<IInjection>;
  public
    constructor Create(const name: string; serviceType, componentType: TRttiType);
    property Name: string read fName;
    property ServiceType: TRttiType read fServiceType;
    property ComponentType: TRttiType read fComponentType;
    property ComponentActivator: IComponentActivator read fComponentActivator write fComponentActivator;
    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
    property Constructors: IList<IInjection> read GetConstructors;
    property Properties: IList<IInjection> read GetProperties;
    property Methods: IList<IInjection> read GetMethods;
    property Fields: IList<IInjection> read GetFields;
  end;

  TDependencyType = (
    dtUnknown,
    dtService,
    dtParameter   // Primitive Types
  );

  // TDependencyModel
  TDependency = class
  private
    fComponentName: string;
    fDependencyType: TDependencyType;
    fTargetType: TRttiType;
    fDependencyValue: TValue;
    function GetTargetTypeInfo: PTypeInfo;
    function GetIsService: Boolean;
  public
    constructor Create(dependencyType: TDependencyType; targetType: TRttiType);
    property ComponentName: string read fComponentName write fComponentName;
    property DependencyType: TDependencyType read fDependencyType;
    property Value: TValue read fDependencyValue write fDependencyValue;
    property TargetType: TRttiType read fTargetType;
    property TargetTypeInfo: PTypeInfo read GetTargetTypeInfo;
    property IsService: Boolean read GetIsService;
  end;

  EContainerException = class(SysUtils.Exception);
  ERegistrationException = class(EContainerException);
  EResolveException = class(EContainerException);
  ECircularDependencyException = class(EResolveException);

implementation

uses
  Spring.ResourceStrings;


{$REGION 'TComponentModel'}

constructor TComponentModel.Create(const name: string;
  serviceType, componentType: TRttiType);
begin
  inherited Create;
  fName := name;
  fServiceType := serviceType;
  fComponentType := componentType;
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

{$ENDREGION}


{$REGION 'TDependency'}

constructor TDependency.Create(dependencyType: TDependencyType;
  targetType: TRttiType);
begin
  TArgument.CheckEnum<TDependencyType>(dependencyType, 'dependencyType');
  TArgument.CheckNotNull(targetType, 'targetType');

  inherited Create;
  fDependencyType := dependencyType;
  fTargetType := targetType;
end;

function TDependency.GetIsService: Boolean;
begin
  Result := DependencyType = dtService;
end;

function TDependency.GetTargetTypeInfo: PTypeInfo;
begin
  Result := TargetType.Handle;
end;

{$ENDREGION}

end.
