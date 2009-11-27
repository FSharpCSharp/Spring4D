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

unit Spring.IoC.Registration;

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
  Spring.IoC.Core;

type
  /// <summary>
  /// TComponentRegistry
  /// </summary>
  TComponentRegistry = class(TInterfacedObject, IComponentRegistry, IInterface)
  private
    fContainerContext: IContainerContext;
    fRttiContext: TRttiContext;
    fModels: IDictionary<PTypeInfo, TComponentModel>;
    fServiceTypeMappings: TDictionary<PTypeInfo, TArray<TComponentModel>>;
//    fSyncRoot: IReadWriteSync;
  protected
    procedure CheckServiceType(serviceType: TRttiType);
    procedure AddComponentModel(model: TComponentModel);
    function GetFullTypeName(typeInfo: PTypeInfo): string;
  public
    constructor Create(const context: IContainerContext);
    destructor Destroy; override;
    procedure AddServiceType(componentType, serviceType: PTypeInfo); overload;
    procedure AddServiceType(componentType, serviceType: PTypeInfo; const name: string); overload;
    procedure Clear;
    function GetComponentModel(componentTypeInfo: PTypeInfo): TComponentModel;
    function FindOneByServiceType(serviceType: PTypeInfo): TComponentModel; overload;
    function FindOneByServiceType(serviceType: PTypeInfo; const name: string): TComponentModel; overload;
    function FindOne(componentType: PTypeInfo): TComponentModel;
    function FindAll: TArray<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): TArray<TComponentModel>; overload;
    function HasServiceType(serviceType: PTypeInfo): Boolean;
  end;

  /// <summary>
  /// Internal non-generic helper class for fluent style registration of a component.
  /// </summary>
  TRegistration = record
  private
    fRegistry: IComponentRegistry;
    fComponentType: PTypeInfo;
    function GetComponentModel: TComponentModel;
    constructor Create(const registry: IComponentRegistry; componentType: PTypeInfo);
  public
    function Implements(serviceType: PTypeInfo): TRegistration; overload;
    function Implements(serviceType: PTypeInfo; const name: string): TRegistration; overload;

    function DelegateTo(delegate: TActivatorDelegate): TRegistration; overload;

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

    function AsSingleton: TRegistration;
    function AsTransient: TRegistration;
  end;

  /// <summary>
  /// Internal generic helper class for fluent style registration of a component.
  /// </summary>
  TRegistration<T: class> = record
  private
    fRegistration: TRegistration;
    constructor Create(const registry: IComponentRegistry);
  public
    function Implements(serviceType: PTypeInfo): TRegistration<T>; overload;
    function Implements(serviceType: PTypeInfo; const name: string): TRegistration<T>; overload;
    function Implements<TServiceType>: TRegistration<T>; overload;
    function Implements<TServiceType>(const name: string): TRegistration<T>; overload;

    function DelegateTo(delegate: TActivatorDelegate<T>): TRegistration<T>; overload;

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

    function AsSingleton: TRegistration<T>;
    function AsTransient: TRegistration<T>;
  end;

//  PRegistration = ^TRegistration;
//  PRegistration<T: class> = ^TRegistration<T>;

  /// <summary>
  /// Provides generic and non-generic fluent-style registration methods.
  /// </summary>
  /// <remarks>
  /// Why both TRegistration and TRegistration<T> are defined as record and
  /// their constructors are private, is to provide generic and non-generic
  /// fluent-style registration with only necessary methods.
  /// </remarks>
  TRegistrationManager = class
  private
    fRegistry: IComponentRegistry;
  public
    constructor Create(const registry: IComponentRegistry);
    function RegisterComponent<TComponentType: class>: TRegistration<TComponentType>; overload;
    function RegisterComponent(componentType: PTypeInfo): TRegistration; overload;
  end;


implementation

uses
  Spring.Reflection,
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings;

{$REGION 'TComponentRegistry'}

constructor TComponentRegistry.Create(const context: IContainerContext);
begin
  TArgument.CheckNotNull(context, 'context');
  inherited Create;
  fContainerContext := context;
  fRttiContext := TRttiContext.Create;
  fModels := TCollections.CreateDictionary<PTypeInfo, TComponentModel>([doOwnsValues]);
  fServiceTypeMappings := TDictionary<PTypeInfo, TArray<TComponentModel>>.Create;
end;

destructor TComponentRegistry.Destroy;
begin
  fServiceTypeMappings.Free;
  fRttiContext.Free;
  inherited Destroy;
end;

procedure TComponentRegistry.CheckServiceType(serviceType: TRttiType);
begin
  if serviceType.IsInterface and not TRttiInterfaceType(serviceType).HasGuid then
  begin
    raise ERegistrationException.CreateRes(@SNonGuidInterfaceServicesAreNotSupported);
  end;
end;

procedure TComponentRegistry.Clear;
begin
  fServiceTypeMappings.Clear;
  fModels.Clear;
end;

procedure TComponentRegistry.AddServiceType(componentType,
  serviceType: PTypeInfo);
begin
  AddServiceType(componentType, serviceType, '');
end;

procedure TComponentRegistry.AddServiceType(componentType,
  serviceType: PTypeInfo; const name: string);
var
  model: TComponentModel;
  models: TArray<TComponentModel>;
  serviceTypeObject: TRttiType;
begin
  TArgument.CheckNotNull(componentType, 'componentType');
  TArgument.CheckNotNull(serviceType, 'serviceType');
  serviceTypeObject := fRttiContext.GetType(serviceType);
  Assert(serviceTypeObject <> nil, 'serviceTypeObject should not be nil.');
  if serviceTypeObject.IsInterface and
    not serviceTypeObject.AsInterface.HasGuid then
  begin
    raise ERegistrationException.CreateRes(@SNonGuidInterfaceServicesAreNotSupported);
  end;
  if not TRtti.IsAssignable(componentType, serviceType) then
  begin
    raise ERegistrationException.CreateResFmt(@SIncompatibleTypes, [
      GetTypeName(componentType), GetTypeName(serviceType)]);
  end;
  model := GetComponentModel(componentType);
  model.ServiceType := serviceTypeObject;
  model.Name := name;
  if not fServiceTypeMappings.TryGetValue(model.ServiceTypeInfo, models) then
  begin
    models := TArray<TComponentModel>.Create(model);
  end
  else
  begin
    SetLength(models, Length(models) + 1);
    models[High(models)] := model;
  end;
  fServiceTypeMappings.AddOrSetValue(serviceType, models);
end;

procedure TComponentRegistry.AddComponentModel(model: TComponentModel);
begin
  Assert(model <> nil, 'model should not be nil.');
  fModels.Add(model.ComponentTypeInfo, model);
  if model.ServiceType <> nil then
  begin
    AddServiceType(model.ComponentTypeInfo, model.ServiceTypeInfo, model.Name);
  end;
end;

function TComponentRegistry.GetComponentModel(
  componentTypeInfo: PTypeInfo): TComponentModel;
var
  componentType: TRttiInstanceType;
begin
  TArgument.CheckNotNull(componentTypeInfo, 'componentTypeInfo');
  if not fModels.TryGetValue(componentTypeInfo, Result) then
  begin
    componentType := fRttiContext.GetType(componentTypeInfo).AsInstance;
    Result := TComponentModel.Create(fContainerContext, componentType);
    AddComponentModel(Result);
  end;
end;

function TComponentRegistry.FindOneByServiceType(serviceType: PTypeInfo): TComponentModel;
begin
  Result := FindOneByServiceType(serviceType, '');
end;

function TComponentRegistry.FindOneByServiceType(serviceType: PTypeInfo;
  const name: string): TComponentModel;
var
  models: TArray<TComponentModel>;
  model: TComponentModel;
begin
  Result := nil;
  if fServiceTypeMappings.TryGetValue(serviceType, models) then
  begin
    if Length(models) = 0 then Exit;
    if name = '' then Exit(models[0]);
    for model in models do
    begin
      if SameText(model.Name, name) then
      begin
        Exit(model);
      end;
    end;
  end;
end;

function TComponentRegistry.FindOne(
  componentType: PTypeInfo): TComponentModel;
begin
  TArgument.CheckNotNull(componentType, 'componentType');
  fModels.TryGetValue(componentType, Result);
end;

function TComponentRegistry.FindAll: TArray<TComponentModel>;
begin
  Result := fModels.Values.ToArray;  // TEMP
end;

function TComponentRegistry.FindAll(
  serviceType: PTypeInfo): TArray<TComponentModel>;
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');
  fServiceTypeMappings.TryGetValue(serviceType, Result);
end;

function TComponentRegistry.GetFullTypeName(typeInfo: PTypeInfo): string;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  Result := fRttiContext.GetType(typeInfo).QualifiedName;
end;

function TComponentRegistry.HasServiceType(serviceType: PTypeInfo): Boolean;
begin
  Result := FindOneByServiceType(serviceType) <> nil;
end;

{$ENDREGION}


{$REGION 'TRegistration'}

constructor TRegistration.Create(const registry: IComponentRegistry;
  componentType: PTypeInfo);
begin
  TArgument.CheckNotNull(registry, 'registry');
  TArgument.CheckNotNull(componentType, 'componentType');
  fRegistry := registry;
  fComponentType := componentType;
end;

function TRegistration.GetComponentModel: TComponentModel;
begin
  Assert(fRegistry <> nil, 'fRegistry should not be nil');
  Result := fRegistry.GetComponentModel(fComponentType);
  Assert(Result <> nil, 'Result should not be nil');
end;

function TRegistration.Implements(serviceType: PTypeInfo): TRegistration;
begin
  fRegistry.AddServiceType(fComponentType, serviceType);
  Result := Self;
end;

function TRegistration.Implements(serviceType: PTypeInfo;
  const name: string): TRegistration;
begin
  fRegistry.AddServiceType(fComponentType, serviceType, name);
  Result := Self;
end;

function TRegistration.DelegateTo(delegate: TActivatorDelegate): TRegistration;
begin
  GetComponentModel.ActivatorDelegate := delegate;
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  GetComponentModel.InjectConstructor(parameterTypes);
  Result := Self;
end;

function TRegistration.InjectProperty(
  const propertyName: string): TRegistration;
begin
  GetComponentModel.InjectProperty(propertyName);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): TRegistration;
begin
  GetComponentModel.InjectMethod(methodName, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string): TRegistration;
begin
  GetComponentModel.InjectMethod(methodName);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string): TRegistration;
begin
  GetComponentModel.InjectField(fieldName);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const arguments: array of TValue): TRegistration;
begin
  GetComponentModel.InjectConstructor(arguments);
  Result := Self;
end;

function TRegistration.InjectProperty(const propertyName: string;
  const value: TValue): TRegistration;
begin
  GetComponentModel.InjectProperty(propertyName, value);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const arguments: array of TValue): TRegistration;
begin
  GetComponentModel.InjectMethod(methodName, arguments);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string;
  const value: TValue): TRegistration;
begin
  GetComponentModel.InjectField(fieldName, value);
  Result := Self;
end;

function TRegistration.AsSingleton: TRegistration;
begin
  GetComponentModel.LifetimeType := ltSingleton;
  Result := Self;
end;

function TRegistration.AsTransient: TRegistration;
begin
  GetComponentModel.LifetimeType := ltTransient;
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
  delegate: TActivatorDelegate<T>): TRegistration<T>;
begin
  fRegistration.DelegateTo(TActivatorDelegate(delegate));
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

function TRegistration<T>.AsTransient: TRegistration<T>;
begin
  fRegistration.AsTransient;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TRegistrationManager'}

constructor TRegistrationManager.Create(
  const registry: IComponentRegistry);
begin
  TArgument.CheckNotNull(registry, 'registry');
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
