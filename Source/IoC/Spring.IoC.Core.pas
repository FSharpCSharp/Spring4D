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
  Spring.Collections;

type
  TComponentModel = class;
  TConstructorCandidate = class;

  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance: TObject;
    procedure Release(instance: TObject);
  end;

  IComponentActivator = interface
    ['{752F2CDE-222C-4D8B-B344-BB7BCA9EAB9E}']
    function CreateInstance(model: TComponentModel): TObject;
  end;

  IDependency = interface

  end;

  IInjectionMember = interface

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

  TObjectNotification = (
    onAdded,
    onExtracted,
    onRemoved
  );

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
    property DependencyType: TDependencyType read fDependencyType;
    property TargetType: TRttiType read fTargetType;
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

  TComponentModelManager = class(TInterfaceBase, IComponentActivator, IInterface)
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
    procedure Release(instance: TObject);
    function AddComponentModel(const name: string; serviceTypeInfo, componentTypeInfo: PTypeInfo;
      lifetimeType: TLifetimeType): TComponentModel;
    function CanResolve(typeInfo: PTypeInfo): Boolean;
    function CreateInstance(componentModel: TComponentModel): TObject;
    function Resolve(typeInfo: PTypeInfo): Pointer;
    function BuildInstance(classType: TClass; constructorMethod: TRttiMethod;
      const parameters: TArray<TValue>): TObject;
  end;

  EContainerException = class(Exception);

implementation

uses
  Spring.ResourceStrings,
  Spring.IoC.LifetimeManager;


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
    fConstructors := TCollections.CreateList<TConstructorCandidate>(True);
  end;
  Result := fConstructors;
end;

function TComponentModel.GetProperties: IList<TRttiProperty>;
begin
  if fProperties = nil then
  begin
    fProperties := TCollections.CreateList<TRttiProperty>;
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
    Result.AfterConstruction;
  except
    on e: Exception do
    begin
      Result.Destroy;
      raise;
    end;
  end;
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
        else if dependency.TargetType is TRttiInterfaceType then
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

procedure TComponentModelManager.Release(instance: TObject);
begin

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


end.
