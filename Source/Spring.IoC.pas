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
  Windows,
  SysUtils,
  TypInfo,
  Rtti,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  TComponentModel = class;
  TContainer = class;

  IDependency = interface

  end;

  IInjectionMember = interface

  end;

  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance: TObject;
    procedure Release(instance: TObject);
  end;

  IComponentBuilder = interface
    function CreateInstance(model: TComponentModel): TObject;
  end;

//  TConstructorDependency = class(TInterfacedObject, IDependency, IInterface)
//
//  end;
//
//  TPropertyDependency = class(TInterfacedObject, IDependency, IInterface)
//
//  end;
//
//  TMethodDependency = class(TInterfacedObject, IDependency, IInterface)
//
//  end;
//
//  TFieldDependency = class(TInterfacedObject, IDependency, IInterface)
//
//  end;

  TLifetimeManagerBase = class abstract(TInterfacedObject, ILifetimeManager, IInterface)
  private
    fBuilder: IComponentBuilder;
    fModel: TComponentModel;
  public
    constructor Create(const builder: IComponentBuilder; model: TComponentModel);
    function GetInstance: TObject; virtual; abstract;
    procedure Release(instance: TObject); virtual; abstract;
    property Builder: IComponentBuilder read fBuilder;
    property Model: TComponentModel read fModel;
  end;

  TSingletonLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstance: TObject;
  public
    destructor Destroy; override;
    function GetInstance: TObject; override;
    procedure Release(instance: TObject); override;
  end;

  TTransientLifetimeManager = class(TLifetimeManagerBase)
  public
    function GetInstance: TObject; override;
    procedure Release(instance: TObject); override;
  end;

//  TParameterBase = class abstract
//
//  end;
//
//  TPrimaryParameter = class(TParameterBase)
//
//  end;
//
//  TDependencyParameter = class(TParameterBase)
//
//  end;

  TConstructorCandidate = class;

  TComponentModel = class
  private
    fName: string;
    fServiceType: TRttiType;
    fComponentType: TRttiType;
    fLifetimeType: TLifetimeType;
    fLifetimeManager: ILifetimeManager;
    fConstructors: IList<TConstructorCandidate>;
    fProperties: IList<TRttiProperty>;
    function GetConstructors: IList<TConstructorCandidate>;
    function GetProperties: IList<TRttiProperty>;
  public
    constructor Create(const name: string; serviceType, componentType: TRttiType);
    property Name: string read fName;
    property ServiceType: TRttiType read fServiceType;
    property ComponentType: TRttiType read fComponentType;
    property Constructors: IList<TConstructorCandidate> read GetConstructors;
    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property LifetimeManager: ILifetimeManager read fLifetimeManager write fLifetimeManager;
//    property Properties: IList<TRttiProperty> read GetProperties;
//    property Methods: IList<TRttiProperty> read GetMethods;
//    property Fields: IList<TRttiProperty> read GetFields;
  end;

  TDependencyType = (
    dtService,
    dtParameter
  );

  TDependencyModel = class
  private
    fDependencyType: TDependencyType;
    fTargetType: TRttiType;
    fIsOptional: Boolean;
  public
    constructor Create(dependencyType: TDependencyType; targetType: TRttiType; isOptional: Boolean);
    property TargetType: TRttiType read fTargetType;
    property DependencyType: TDependencyType read fDependencyType;
    property IsOptional: Boolean read fIsOptional write fIsOptional;
  end;

  TConstructorCandidate = class
  private
    fConstructorMethod: TRttiMethod;
    fDependencies: TArray<TDependencyModel>;
  public
    constructor Create(constructorMethod: TRttiMethod; const dependencies: TArray<TDependencyModel>);
    destructor Destroy; override;                              
    property ConstructorMethod: TRttiMethod read fConstructorMethod;
    property Dependencies: TArray<TDependencyModel> read fDependencies;
  end;

  TObjectNotification = (
    onAdded,
    onExtracted,
    onRemoved
  );

  TComponentModelManager = class(TInterfaceBase, IComponentBuilder, IInterface)
  private
    fContext: TRttiContext;
    fModels: TList<TComponentModel>;
  protected
    procedure ProcessModel(model: TComponentModel); virtual;
    procedure InspectConstructors(model: TComponentModel); virtual;
    procedure InspectLifetime(model: TComponentModel); virtual;
//    procedure InspectProperties(model: TComponentModel); virtual;
    function GetEligibleConstructor(model: TComponentModel): TConstructorCandidate;
    function GetConstructorParameters(const dependencies: TArray<TDependencyModel>): TArray<TValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function AddComponentModel(const name: string; serviceTypeInfo, componentTypeInfo: PTypeInfo;
      lifetimeType: TLifetimeType): TComponentModel;
    function CanResolve(typeInfo: PTypeInfo): Boolean;
    function CreateInstance(componentModel: TComponentModel): TObject;
    function Resolve(typeInfo: PTypeInfo): Pointer;
    function BuildInstance(classType: TClass; constructorMethod: TRttiMethod;
      const parameters: TArray<TValue>): TObject;
  end;

  /// <summary>
  /// Represents an IoC (Inversion of Control) container.
  /// </summary>
  /// <remarks>
  /// </remarks>
  TContainer = class
  private
    fManager: TComponentModelManager;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterComponent<TServiceType; TComponentType: TServiceType>: TContainer; overload;
    function RegisterComponent<TServiceType; TComponentType: TServiceType>(lifetime: TLifetimeType): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const dependencies: array of IDependency): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(lifetime: TLifetimeType; const dependencies: array of IDependency): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string; lifetime: TLifetimeType): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string; lifetime: TLifetimeType; const dependencies: array of IDependency): TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(const name: string): TContainer; overload;
//    function RegisterComponent(serviceType, componentType: PTypeInfo): TContainer; overload;
    function RegisterComponent(const name: string; serviceType, componentType: PTypeInfo;
      lifetime: TLifetimeType; const dependencies: array of IDependency): TContainer; overload;
//    function RegisterDependencies(const name: string; componentType: PTypeInfo;
//      const dependencies: array of IDependency): TContainer; overload; virtual; abstract;
    function Resolve<T>: T; overload;
//    function Resolve<T>(const name: string; const dependencies: array of IDependency): T; overload;
//    function Resolve<T>(const name: string): T; overload;
    function Resolve(typeInfo: PTypeInfo): Pointer; overload;
//    function ResolveAll<TServiceType>: IEnumerable<TServiceType>;
  end;

  EContainerException = class(Exception);

//  function Container: TContainer;

implementation


{$REGION 'TComponentModel'}

constructor TComponentModel.Create(const name: string;
  serviceType, componentType: TRttiType);
begin
  inherited Create;
  fName := name;
  fServiceType := serviceType;
  fComponentType := componentType;
end;

function TComponentModel.GetConstructors: IList<TConstructorCandidate>;
begin
  if fConstructors = nil then
  begin
    fConstructors := TContainers.CreateList<TConstructorCandidate>(True);
  end;
  Result := fConstructors;
end;

function TComponentModel.GetProperties: IList<TRttiProperty>;
begin
  if fProperties = nil then
  begin
    fProperties := TContainers.CreateList<TRttiProperty>;
  end;
  Result := fProperties;
end;

{$ENDREGION}


{$REGION 'TDependencyModel'}

constructor TDependencyModel.Create(dependencyType: TDependencyType;
  targetType: TRttiType; isOptional: Boolean);
begin
  inherited Create;
  fTargetType := targetType;
  fDependencyType := dependencyType;
  fIsOptional := isOptional;
end;

{$ENDREGION}


{$REGION 'TConstructorCandidate'}

constructor TConstructorCandidate.Create(constructorMethod: TRttiMethod;
  const dependencies: TArray<TDependencyModel>);
begin
  inherited Create;
  fConstructorMethod := constructorMethod;
  fDependencies := dependencies;
end;

destructor TConstructorCandidate.Destroy;
var
  model: TDependencyModel;
begin
  for model in fDependencies do
  begin
    model.Free;
  end;
//  FreeArrayItems<TDependencyModel>(fDependencies);
  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TComponentModelManager'}

constructor TComponentModelManager.Create;
begin
  inherited Create;
  fContext := TRttiContext.Create;
  fModels := TObjectList<TComponentModel>.Create;
end;

destructor TComponentModelManager.Destroy;
begin
  fModels.Free;
  fContext.Free;
  inherited Destroy;
end;

function TComponentModelManager.BuildInstance(classType: TClass; 
  constructorMethod: TRttiMethod; const parameters: TArray<TValue>): TObject;
begin
  Result := classType.NewInstance;
  try
    constructorMethod.Invoke(Result, parameters);
  except 
    on e: Exception do
    begin
      Result.Destroy;
      Result.FreeInstance; // necessary?
      raise;
    end;
  end;
  Result.AfterConstruction;
end;

function TComponentModelManager.CanResolve(typeInfo: PTypeInfo): Boolean;
begin
  Result := True;
end;

function TComponentModelManager.AddComponentModel(const name: string;
  serviceTypeInfo, componentTypeInfo: PTypeInfo;
  lifetimeType: TLifetimeType): TComponentModel;
var
  serviceType: TRttiType;
  componentType: TRttiType;
begin
  TArgument.CheckNotNull(serviceTypeInfo, 'serviceTypeInfo');
  TArgument.CheckNotNull(componentTypeInfo, 'componentTypeInfo');
  serviceType := fContext.GetType(serviceTypeInfo);
  componentType := fContext.GetType(componentTypeInfo);
  if (serviceType is TRttiInterfaceType) and not (ifHasGuid in TRttiInterfaceType(serviceType).IntfFlags) then
  begin
    raise ENotSupportedException.Create('Non-Guid Interfaces are not supported.');
  end;
  Result := TComponentModel.Create(name, serviceType, componentType);
  Result.LifetimeType := lifetimeType;
  fModels.Add(Result);
//  Notify(Result, onAdded);
  ProcessModel(Result);
end;

function TComponentModelManager.GetEligibleConstructor(
  model: TComponentModel): TConstructorCandidate;
var
  candidate: TConstructorCandidate;
begin
  Result := nil;
  for candidate in model.Constructors do
  begin
    Result := candidate; // TEMP
    Break;
  end;
end;

function TComponentModelManager.GetConstructorParameters(
  const dependencies: TArray<TDependencyModel>): TArray<TValue>;
var
  dependency: TDependencyModel;
  instance: Pointer;
  i: Integer;
begin
  SetLength(Result, Length(dependencies));
  for i := 0 to High(dependencies) do
  begin
    dependency := dependencies[i];
    case dependency.DependencyType of
      dtService:
      begin
        instance := Resolve(dependency.TargetType.Handle);
        if dependency.TargetType is TRttiInstanceType then
        begin
          Result[i] := TObject(instance);
        end
        else if dependency.fTargetType is TRttiInterfaceType then
        begin
          TValue.MakeWithoutCopy(@instance, dependency.TargetType.Handle, Result[i]);
        end
        else
        begin
          raise EContainerException.Create('Unexpected dependency parameterType.');
        end;
      end;
      dtParameter:
      begin
        raise ENotImplementedException.Create('Parameter');
      end;
    end;
    Assert(not Result[i].IsEmpty);
  end;
end;

function TComponentModelManager.CreateInstance(
  componentModel: TComponentModel): TObject;
var
  componentType: TRttiInstanceType;
  candidate: TConstructorCandidate;
  constructorMethod: TRttiMethod;
  constructorParameters: TArray<TValue>;
begin
  TArgument.CheckNotNull(componentModel, 'componentModel');
  candidate := GetEligibleConstructor(componentModel);
  constructorParameters := GetConstructorParameters(candidate.Dependencies);
  constructorMethod := candidate.ConstructorMethod;
  componentType := componentModel.ComponentType as TRttiInstanceType;
  Result := BuildInstance(componentType.MetaclassType, constructorMethod, constructorParameters);
end;

procedure TComponentModelManager.ProcessModel(model: TComponentModel);
begin
  InspectConstructors(model);
  InspectLifetime(model);
//  InspectProperties(model);
//  InspectMethods(model);
//  InspectFields(model);
end;

function TComponentModelManager.Resolve(typeInfo: PTypeInfo): Pointer;
var
  model: TComponentModel;
  instance: TObject;
begin
  Result := nil;
  for model in fModels do
  begin
    if model.ServiceType.Handle = typeInfo then
    begin
      instance := model.LifetimeManager.GetInstance;
      case typeInfo.Kind of
        tkInterface:
        begin
          instance.GetInterface(GetTypeData(typeInfo).Guid, Result);
        end;
        tkClass:
        begin
          Result := instance;
        end;
      end;
      Break;
    end;
  end;
  if Result = nil then
  begin
    raise EContainerException.Create('Cannot resolve the type.');
  end;
end;

procedure TComponentModelManager.InspectConstructors(model: TComponentModel);
var
  constructorCandidate: TConstructorCandidate;
  method: TRttiMethod;
  parameter: TRttiParameter;
  parameters: TArray<TRttiParameter>;
  dependencies: TArray<TDependencyModel>;
  dependencyModel: TDependencyModel;
  i: Integer;
begin
  for method in model.ComponentType.GetMethods do
  begin
    if method.IsConstructor and (method.Visibility = mvPublic) then
    begin
      parameters := method.GetParameters;
      SetLength(dependencies, Length(parameters));
      for i := 0 to High(parameters) do
      begin
        parameter := parameters[i];
//        if parameter.Flags in [pfVar, pfOut] then
//          Continue;
        if parameter.ParamType.TypeKind in [tkClass, tkInterface] then
        begin
          dependencyModel := TDependencyModel.Create(dtService, parameter.ParamType, True);
        end
        else
        begin
          dependencyModel := TDependencyModel.Create(dtParameter, parameter.ParamType, True);
        end;
        dependencies[i] := dependencyModel;
      end;
      constructorCandidate := TConstructorCandidate.Create(method, dependencies);
      model.Constructors.Add(constructorCandidate);
    end;
  end;
end;

procedure TComponentModelManager.InspectLifetime(model: TComponentModel);
begin
  if model.LifetimeType = ltUnknown then
    model.LifetimeType := ltSingleton;
  case model.LifetimeType of
    ltSingleton:
    begin
      model.LifetimeManager := TSingletonLifetimeManager.Create(Self, model);
    end;
    ltTransient:
    begin
      model.LifetimeManager := TTransientLifetimeManager.Create(Self, model);
    end;
    else
    begin
      raise EContainerException.Create('Unsupported lifetimeType.');
    end;
//    ltPerThread: ;
//    ltPooled: ;
//    ltCustom: ;
  end;
end;

//procedure TComponentModelManager.InspectProperties(model: TComponentModel);
//var
//  propertyMember: TRttiProperty;
//begin
//  for propertyMember in model.ComponentType.GetProperties do
//  begin
//    if propertyMember.IsWritable and (propertyMember.PropertyType.TypeKind in [tkClass, tkInterface]) then
//    begin
//      model.Properties.Add(propertyMember);
//    end;
//  end;
//end;

{$ENDREGION}


{$REGION 'TLifetimeManagerBase'}

constructor TLifetimeManagerBase.Create(const builder: IComponentBuilder;
  model: TComponentModel);
begin
  inherited Create;
  fBuilder := builder;
  fModel := model;
end;

{$ENDREGION}


{$REGION 'TSingletonLifetimeManager'}

destructor TSingletonLifetimeManager.Destroy;
begin
  if not (fInstance is TInterfacedObject) then
  begin
    fInstance.Free;
  end;
  inherited Destroy;
end;

function TSingletonLifetimeManager.GetInstance: TObject;
var
  localInstance: TObject;
begin
  if fInstance = nil then
  begin
    localInstance := fBuilder.CreateInstance(fModel);
    if InterlockedCompareExchangePointer(Pointer(fInstance), localInstance, nil) <> nil then
      localInstance.Free;
  end;
  Result := fInstance;
end;

procedure TSingletonLifetimeManager.Release(instance: TObject);
begin
end;

{$ENDREGION}


{$REGION 'TTransientLifetimeManager'}

function TTransientLifetimeManager.GetInstance: TObject;
begin
  Result := Builder.CreateInstance(Model);
end;

procedure TTransientLifetimeManager.Release(instance: TObject);
begin
  instance.Free;
end;

{$ENDREGION}


{$REGION 'TContainer'}

constructor TContainer.Create;
begin
  inherited Create;
  fManager := TComponentModelManager.Create;
end;

destructor TContainer.Destroy;
begin
  fManager.Free;
  inherited Destroy;
end;

function TContainer.RegisterComponent<TServiceType, TComponentType>: TContainer;
begin
  Result := RegisterComponent('', TypeInfo(TServiceType), TypeInfo(TComponentType), ltSingleton, []);
end;

function TContainer.RegisterComponent<TServiceType, TComponentType>(lifetime: TLifetimeType): TContainer;
begin
  Result := RegisterComponent('', TypeInfo(TServiceType), TypeInfo(TComponentType), lifetime, []);
end;

function TContainer.RegisterComponent(const name: string; serviceType,
  componentType: PTypeInfo; lifetime: TLifetimeType;
  const dependencies: array of IDependency): TContainer;
begin
  fManager.AddComponentModel(name, serviceType, componentType, lifetime);
  Result := Self;
end;

function TContainer.Resolve(typeInfo: PTypeInfo): Pointer;
begin
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');
  Result := fManager.Resolve(typeInfo);
end;

function TContainer.Resolve<T>: T;
begin
  TRtti.CheckTypeKind<T>([tkClass, tkInterface]);
  PPointer(@Result)^ := Resolve(TypeInfo(T));
end;

{$ENDREGION}

end.
