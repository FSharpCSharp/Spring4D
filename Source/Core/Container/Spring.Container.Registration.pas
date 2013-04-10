{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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

{TODO -oOwner -cGeneral : Add DelegateTo(TClass)}
unit Spring.Container.Registration;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Services,
  Spring.Container.Core;

type
  ///	<summary>
  ///	  TComponentRegistry
  ///	</summary>
  TComponentRegistry = class(TInterfacedObject, IComponentRegistry, IInterface)
  private
    fContainerContext: IContainerContext;
    fRttiContext: TRttiContext;
    fModels: IList<TComponentModel>;
    fServiceTypeMappings: IDictionary<PTypeInfo, IList<TComponentModel>>;
    fServiceNameMappings: IDictionary<string, TComponentModel>;
  protected
    procedure OnComponentModelAdded(model: TComponentModel);
    procedure CheckIsNonGuidInterface(serviceType: TRttiType);
    procedure Validate(componentType, serviceType: PTypeInfo; var serviceName: string);
    function GetDefaultTypeName(serviceType: TRttiType): string;
  public
    constructor Create(const context: IContainerContext);
    destructor Destroy; override;
    function RegisterComponent(componentTypeInfo: PTypeInfo): TComponentModel;
    procedure RegisterService(model: TComponentModel; serviceType: PTypeInfo); overload;
    procedure RegisterService(model: TComponentModel; serviceType: PTypeInfo; const name: string); overload;
    procedure UnregisterAll;
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;
    function FindOne(componentType: PTypeInfo): TComponentModel; overload;
    function FindOne(const name: string): TComponentModel; overload;
    function FindAll: IEnumerable<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): IEnumerable<TComponentModel>; overload;
  end;

  ///	<summary>
  ///	  Internal helper class for non-generic fluent style registration of a
  ///	  component.
  ///	</summary>
  TRegistration = record
  private
{$IFNDEF DELPHIXE_UP}
    fRegistry: TComponentRegistry;
{$ELSE}
    fRegistry: IComponentRegistry;
{$ENDIF}
    fModel: TComponentModel;
    constructor Create(const registry: IComponentRegistry; componentType: PTypeInfo);
  public
    function Implements(serviceType: PTypeInfo): TRegistration; overload;
    function Implements(serviceType: PTypeInfo; const name: string): TRegistration; overload;

    function DelegateTo(const delegate: TActivatorDelegate): TRegistration; overload;

    {$REGION 'Typed Injections'}

    function InjectConstructor(const parameterTypes: array of PTypeInfo): TRegistration; overload;
    function InjectProperty(const propertyName: string): TRegistration; overload;
    function InjectMethod(const methodName: string): TRegistration; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): TRegistration; overload;
    function InjectField(const fieldName: string): TRegistration; overload;

    {$ENDREGION}

    {$REGION 'Named/Valued Injections'}

    function InjectConstructor(const arguments: array of TValue): TRegistration; overload;
    function InjectProperty(const propertyName: string; const value: TValue): TRegistration; overload;
    function InjectMethod(const methodName: string; const arguments: array of TValue): TRegistration; overload;
    function InjectField(const fieldName: string; const value: TValue): TRegistration; overload;

    {$ENDREGION}

    function AsSingleton: TRegistration; overload;
    function AsSingleton(refCounting: TRefCounting): TRegistration; overload;
    function AsSingletonPerThread: TRegistration;
    function AsTransient: TRegistration;
    function AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration;

    function AsDefault(serviceType: PTypeInfo): TRegistration;
  end;

  ///	<summary>
  ///	  Internal helper class for generic fluent style registration of a
  ///	  component.
  ///	</summary>
  TRegistration<T> = record
  private
    fRegistration: TRegistration;
    constructor Create(const registry: IComponentRegistry);
  public
    function Implements(serviceType: PTypeInfo): TRegistration<T>; overload;
    function Implements(serviceType: PTypeInfo; const name: string): TRegistration<T>; overload;
    function Implements<TServiceType>: TRegistration<T>; overload;
    function Implements<TServiceType>(const name: string): TRegistration<T>; overload;

    function DelegateTo(const delegate: TActivatorDelegate<T>): TRegistration<T>; overload;

  {$REGION 'Typed Injections'}

    function InjectConstructor(const parameterTypes: array of PTypeInfo): TRegistration<T>; overload;
    function InjectProperty(const propertyName: string): TRegistration<T>; overload;
    function InjectMethod(const methodName: string): TRegistration<T>; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): TRegistration<T>; overload;
    function InjectField(const fieldName: string): TRegistration<T>; overload;

  {$ENDREGION}

  {$REGION 'Named/Valued Injections'}

    function InjectConstructor(const arguments: array of TValue): TRegistration<T>; overload;
    function InjectProperty(const propertyName: string; const value: TValue): TRegistration<T>; overload;
    function InjectMethod(const methodName: string; const arguments: array of TValue): TRegistration<T>; overload;
    function InjectField(const fieldName: string; const value: TValue): TRegistration<T>; overload;

  {$ENDREGION}

    function AsSingleton: TRegistration<T>; overload;
    function AsSingleton(refCounting: TRefCounting): TRegistration<T>; overload;
    function AsSingletonPerThread: TRegistration<T>;
    function AsTransient: TRegistration<T>;
    function AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration<T>;

    function AsDefault(serviceType: PTypeInfo): TRegistration<T>; overload;
    function AsDefault<TServiceType>: TRegistration<T>; overload;
  end;

  ///	<summary>
  ///	  Provides both generic and non-generic fluent-style registration
  ///	  methods.
  ///	</summary>
  ///	<remarks>
  ///	  Why both TRegistration and TRegistration(T) are defined as record and
  ///	  their constructors are private, is to provide generic and non-generic
  ///	  fluent-style registration with only necessary methods.
  ///	</remarks>
  TRegistrationManager = class
  private
    fRegistry: IComponentRegistry;
  public
    constructor Create(const registry: IComponentRegistry);
    function RegisterComponent<TComponentType>: TRegistration<TComponentType>; overload;
    function RegisterComponent(componentType: PTypeInfo): TRegistration; overload;
  end;


implementation

uses
  Spring.Reflection,
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.Container.ResourceStrings;

{$REGION 'TComponentRegistry'}

constructor TComponentRegistry.Create(const context: IContainerContext);
begin
  TArgument.CheckNotNull(context, 'context');

  inherited Create;
  fContainerContext := context;
  fRttiContext := TRttiContext.Create;
  fModels := TCollections.CreateObjectList<TComponentModel>(True);
  fServiceTypeMappings := TCollections.CreateDictionary<PTypeInfo, IList<TComponentModel>>;
  fServiceNameMappings := TCollections.CreateDictionary<string, TComponentModel>;
end;

destructor TComponentRegistry.Destroy;
begin
  fRttiContext.Free;
  inherited Destroy;
end;

procedure TComponentRegistry.CheckIsNonGuidInterface(serviceType: TRttiType);
begin
  if serviceType.IsInterface and not TRttiInterfaceType(serviceType).HasGuid then
  begin
    raise ERegistrationException.CreateRes(@SNonGuidInterfaceServicesAreNotSupported);
  end;
end;

procedure TComponentRegistry.Validate(componentType, serviceType: PTypeInfo;
  var serviceName: string);
var
  componentTypeObject: TRttiType;
  serviceTypeObject: TRttiType;
begin
  componentTypeObject := fRttiContext.GetType(componentType);
  serviceTypeObject := fRttiContext.GetType(serviceType);
  CheckIsNonGuidInterface(serviceTypeObject);
  if not TType.IsAssignable(componentType, serviceType) 
    and not componentTypeObject.IsInterface then
  begin
    raise ERegistrationException.CreateResFmt(@SIncompatibleTypes, [
      GetTypeName(componentType), GetTypeName(serviceType)]);
  end;
  if serviceName = '' then
  begin
    serviceName := GetDefaultTypeName(componentTypeObject) + '.' + serviceTypeObject.Name;
  end;
  if HasService(serviceName) then
  begin
    raise ERegistrationException.CreateResFmt(@SDuplicatedName, [serviceName]);
  end;
end;

procedure TComponentRegistry.UnregisterAll;
begin
  fServiceNameMappings.Clear;
  fServiceTypeMappings.Clear;
  fModels.Clear;
end;

procedure TComponentRegistry.RegisterService(model: TComponentModel;
  serviceType: PTypeInfo);
begin
  RegisterService(model, serviceType, '');
end;

procedure TComponentRegistry.RegisterService(model: TComponentModel;
  serviceType: PTypeInfo; const name: string);
var
  models: IList<TComponentModel>;
  serviceName: string;
begin
  TArgument.CheckNotNull(model, 'model');
  TArgument.CheckNotNull(serviceType, 'serviceType');

  serviceName := name;
  Validate(model.ComponentTypeInfo, serviceType, serviceName);
  model.Services[serviceName] := serviceType;
  if not fServiceTypeMappings.TryGetValue(serviceType, models) then
  begin
    models := TCollections.CreateList<TComponentModel>;
    fServiceTypeMappings.AddOrSetValue(serviceType, models);
  end;
  models.Add(model);
  fServiceNameMappings.Add(serviceName, model);
end;

function TComponentRegistry.RegisterComponent(
  componentTypeInfo: PTypeInfo): TComponentModel;
var
  componentType: TRttiType;
begin
  TArgument.CheckNotNull(componentTypeInfo, 'componentTypeInfo');

  componentType := fRttiContext.GetType(componentTypeInfo);
  Result := TComponentModel.Create(fContainerContext, componentType);
  fModels.Add(Result);
  OnComponentModelAdded(Result);
end;

function TComponentRegistry.FindOne(const name: string): TComponentModel;
begin
  fServiceNameMappings.TryGetValue(name, Result);
end;

function TComponentRegistry.FindOne(componentType: PTypeInfo): TComponentModel;
begin
  TArgument.CheckNotNull(componentType, 'componentType');

  Result := fModels.FirstOrDefault(
    function(const model: TComponentModel): Boolean
    begin
      Result := model.ComponentTypeInfo = componentType;
    end);
end;

function TComponentRegistry.FindAll: IEnumerable<TComponentModel>;
begin
  Result := fModels;
end;

function TComponentRegistry.FindAll(
  serviceType: PTypeInfo): IEnumerable<TComponentModel>;
var
  models: IList<TComponentModel>;
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');

  if fServiceTypeMappings.TryGetValue(serviceType, models) then
  begin
    Result := models;
  end
  else
  begin
    Result := TCollections.CreateList<TComponentModel>;;
  end;
end;

function TComponentRegistry.GetDefaultTypeName(serviceType: TRttiType): string;
begin
  Assert(serviceType <> nil, 'serviceType should not be nil');

  if serviceType.IsPublicType then
  begin
    Result := serviceType.QualifiedName;
  end
  else
  begin
    Result := serviceType.Name;
  end;
end;

function TComponentRegistry.HasService(serviceType: PTypeInfo): Boolean;
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');

  Result := fServiceTypeMappings.ContainsKey(serviceType);
end;

function TComponentRegistry.HasService(const name: string): Boolean;
begin
  Result := fServiceNameMappings.ContainsKey(name);
end;

function TComponentRegistry.HasService(serviceType: PTypeInfo;
  const name: string): Boolean;
var
  model: TComponentModel;
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');

  Result := fServiceNameMappings.TryGetValue(name, model) and
    model.HasService(serviceType);
end;

procedure TComponentRegistry.OnComponentModelAdded(model: TComponentModel);
var
  attributes: TArray<ImplementsAttribute>;
  attribute: ImplementsAttribute;
begin
  attributes := model.ComponentType.GetCustomAttributes<ImplementsAttribute>;
  for attribute in attributes do
  begin
    RegisterService(model, attribute.ServiceType, attribute.Name);
  end;
end;

{$ENDREGION}


{$REGION 'TRegistration'}

constructor TRegistration.Create(const registry: IComponentRegistry;
  componentType: PTypeInfo);
begin
  TArgument.CheckNotNull(registry, 'registry');
  TArgument.CheckNotNull(componentType, 'componentType');
{$IFNDEF DELPHIXE_UP}
  fRegistry := registry as TComponentRegistry;
{$ELSE}
  fRegistry := registry;
{$ENDIF}
  fModel := fRegistry.RegisterComponent(componentType);
end;

function TRegistration.Implements(serviceType: PTypeInfo): TRegistration;
begin
  fRegistry.RegisterService(fModel, serviceType);
  Result := Self;
end;

function TRegistration.Implements(serviceType: PTypeInfo;
  const name: string): TRegistration;
begin
  fRegistry.RegisterService(fModel, serviceType, name);
  Result := Self;
end;

function TRegistration.DelegateTo(const delegate: TActivatorDelegate): TRegistration;
begin
  fModel.ActivatorDelegate := delegate;
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  fModel.InjectConstructor(parameterTypes);
  Result := Self;
end;

function TRegistration.InjectProperty(
  const propertyName: string): TRegistration;
begin
  fModel.InjectProperty(propertyName);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  fModel.InjectMethod(methodName, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string): TRegistration;
begin
  fModel.InjectMethod(methodName);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string): TRegistration;
begin
  fModel.InjectField(fieldName);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const arguments: array of TValue): TRegistration;
begin
  fModel.InjectConstructor(arguments);
  Result := Self;
end;

function TRegistration.InjectProperty(const propertyName: string;
  const value: TValue): TRegistration;
begin
  fModel.InjectProperty(propertyName, value);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const arguments: array of TValue): TRegistration;
begin
  fModel.InjectMethod(methodName, arguments);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string;
  const value: TValue): TRegistration;
begin
  fModel.InjectField(fieldName, value);
  Result := Self;
end;

function TRegistration.AsSingleton: TRegistration;
begin
  Result := AsSingleton(TRefCounting.Unknown);
end;

function TRegistration.AsSingleton(refCounting: TRefCounting): TRegistration;
begin
  fModel.LifetimeType := TLifetimeType.Singleton;
  fModel.RefCounting := refCounting;
  Result := Self;
end;

function TRegistration.AsSingletonPerThread: TRegistration;
begin
  fModel.LifetimeType := TLifetimeType.SingletonPerThread;
  Result := Self;
end;

function TRegistration.AsTransient: TRegistration;
begin
  fModel.LifetimeType := TLifetimeType.Transient;
  Result := Self;
end;

function TRegistration.AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration;
begin
  fModel.LifetimeType := TLifetimeType.Pooled;
  fModel.MinPoolsize := minPoolSize;
  fModel.MaxPoolsize := maxPoolSize;
  Result := Self;
end;

function TRegistration.AsDefault(serviceType: PTypeInfo): TRegistration;
var
  model: TComponentModel;
begin
  for model in fRegistry.FindAll(serviceType) do
  begin
    model.DefaultServices.Remove(serviceType);
  end;
  fModel.DefaultServices.Add(serviceType);
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TRegistration<T>'}

constructor TRegistration<T>.Create(
  const registry: IComponentRegistry);
begin
  fRegistration := TRegistration.Create(registry, TypeInfo(T));
end;

function TRegistration<T>.Implements(serviceType: PTypeInfo): TRegistration<T>;
begin
  fRegistration.Implements(serviceType);
  Result := Self;
end;

function TRegistration<T>.Implements(serviceType: PTypeInfo;
  const name: string): TRegistration<T>;
begin
  fRegistration.Implements(serviceType, name);
  Result := Self;
end;

function TRegistration<T>.Implements<TServiceType>: TRegistration<T>;
begin
  Result := Implements(TypeInfo(TServiceType));
end;

function TRegistration<T>.Implements<TServiceType>(
  const name: string): TRegistration<T>;
begin
  Result := Implements(TypeInfo(TServiceType), name);
end;

function TRegistration<T>.DelegateTo(
  const delegate: TActivatorDelegate<T>): TRegistration<T>;
begin
  fRegistration.DelegateTo(
    function: TValue
    begin
      Result := TValue.From<T>(delegate());
    end);
  Result := Self;
end;

function TRegistration<T>.InjectConstructor(
  const parameterTypes: array of PTypeInfo): TRegistration<T>;
begin
  fRegistration.InjectConstructor(parameterTypes);
  Result := Self;
end;

function TRegistration<T>.InjectProperty(
  const propertyName: string): TRegistration<T>;
begin
  fRegistration.InjectProperty(propertyName);
  Result := Self;
end;

function TRegistration<T>.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): TRegistration<T>;
begin
  fRegistration.InjectMethod(methodName, parameterTypes);
  Result := Self;
end;

function TRegistration<T>.InjectMethod(
  const methodName: string): TRegistration<T>;
begin
  fRegistration.InjectMethod(methodName);
  Result := Self;
end;

function TRegistration<T>.InjectField(
  const fieldName: string): TRegistration<T>;
begin
  fRegistration.InjectField(fieldName);
  Result := Self;
end;

function TRegistration<T>.InjectConstructor(
  const arguments: array of TValue): TRegistration<T>;
begin
  fRegistration.InjectConstructor(arguments);
  Result := Self;
end;

function TRegistration<T>.InjectProperty(const propertyName: string;
  const value: TValue): TRegistration<T>;
begin
  fRegistration.InjectProperty(propertyName, value);
  Result := Self;
end;

function TRegistration<T>.InjectMethod(const methodName: string;
  const arguments: array of TValue): TRegistration<T>;
begin
  fRegistration.InjectMethod(methodName, arguments);
  Result := Self;
end;

function TRegistration<T>.InjectField(const fieldName: string;
  const value: TValue): TRegistration<T>;
begin
  fRegistration.InjectField(fieldName, value);
  Result := Self;
end;

function TRegistration<T>.AsSingleton: TRegistration<T>;
begin
  fRegistration.AsSingleton;
  Result := Self;
end;

function TRegistration<T>.AsSingleton(refCounting: TRefCounting): TRegistration<T>;
begin
  fRegistration.AsSingleton(refCounting);
  Result := Self;
end;

function TRegistration<T>.AsSingletonPerThread: TRegistration<T>;
begin
  fRegistration.AsSingletonPerThread;
  Result := Self;
end;

function TRegistration<T>.AsTransient: TRegistration<T>;
begin
  fRegistration.AsTransient;
  Result := Self;
end;

function TRegistration<T>.AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration<T>;
begin
  fRegistration.AsPooled(minPoolSize, maxPoolSize);
  Result := Self;
end;

function TRegistration<T>.AsDefault(serviceType: PTypeInfo): TRegistration<T>;
begin
  fRegistration.AsDefault(serviceType);
  Result := Self;
end;

function TRegistration<T>.AsDefault<TServiceType>: TRegistration<T>;
begin
  Result := AsDefault(TypeInfo(TServiceType));
end;

{$ENDREGION}


{$REGION 'TRegistrationManager'}

constructor TRegistrationManager.Create(
  const registry: IComponentRegistry);
begin
  inherited Create;
  fRegistry := registry;
end;

function TRegistrationManager.RegisterComponent(
  componentType: PTypeInfo): TRegistration;
begin
  TArgument.CheckNotNull(componentType, 'componentType');
  Result := TRegistration.Create(fRegistry, componentType);
end;

function TRegistrationManager.RegisterComponent<TComponentType>: TRegistration<TComponentType>;
begin
  Result := TRegistration<TComponentType>.Create(fRegistry);
end;

{$ENDREGION}

end.
