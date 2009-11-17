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
  /// TServiceRegistry
  /// </summary>
  TServiceRegistry = class(TInterfacedObject, IServiceRegistry, IInterface)
  private
    fContainerContext: IContainerContext;
    fRttiContext: TRttiContext;
    fInspectors: TList<IRegistrationInspector>;
    fModels: TList<TComponentModel>;
  protected
    procedure CheckServiceType(serviceType: TRttiType);
    function AddComponentModel(const name: string; serviceTypeInfo,
      componentTypeInfo: PTypeInfo; lifetimeType: TLifetimeType): TComponentModel; virtual;
    procedure ProcessModel(model: TComponentModel); virtual;
  public
    constructor Create(const context: IContainerContext);
    destructor Destroy; override;
    procedure AddInspector(const inspector: IRegistrationInspector);
    procedure RemoveInspector(const inspector: IRegistrationInspector);
    procedure ClearInspectors;
    procedure RegisterType(const name: string; serviceTypeInfo, componentTypeInfo: PTypeInfo; lifetimeType: TLifetimeType);
    procedure UnregisterAll;
    function FindOne(serviceType: PTypeInfo): TComponentModel; overload;
    function FindOne(serviceType: PTypeInfo; const name: string): TComponentModel; overload;
    function FindOneByComponentType(componentType: PTypeInfo): TComponentModel;
    function GetFullTypeName(typeInfo: PTypeInfo): string;
    function HasServiceType(serviceType: PTypeInfo): Boolean;
  end;

implementation

uses
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings;

{$REGION 'TServiceRegistry'}

constructor TServiceRegistry.Create(const context: IContainerContext);
begin
  TArgument.CheckNotNull(context, 'context');
  inherited Create;
  fContainerContext := context;
  fRttiContext := TRttiContext.Create;
  fModels := TObjectList<TComponentModel>.Create;
  fInspectors := TList<IRegistrationInspector>.Create;
end;

destructor TServiceRegistry.Destroy;
begin
  fInspectors.Free;
  fModels.Free;
  fRttiContext.Free;
  inherited Destroy;
end;

procedure TServiceRegistry.AddInspector(const inspector: IRegistrationInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Add(inspector);
end;

procedure TServiceRegistry.RemoveInspector(const inspector: IRegistrationInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Remove(inspector);
end;

procedure TServiceRegistry.ClearInspectors;
begin
  fInspectors.Clear;
end;

procedure TServiceRegistry.CheckServiceType(serviceType: TRttiType);
begin
  if serviceType.IsInterface and not TRttiInterfaceType(serviceType).HasGuid then
  begin
    raise ERegistrationException.CreateRes(@SNonGuidInterfaceServicesAreNotSupported);
  end;
end;

procedure TServiceRegistry.RegisterType(const name: string; serviceTypeInfo,
  componentTypeInfo: PTypeInfo; lifetimeType: TLifetimeType);
var
  model: TComponentModel;
begin
  TArgument.CheckNotNull(serviceTypeInfo, 'serviceTypeInfo');
  TArgument.CheckNotNull(componentTypeInfo, 'componentTypeInfo');
  TArgument.CheckTypeKind(componentTypeInfo, [tkClass], 'componentTypeInfo');
  TArgument.CheckEnum<TLifetimeType>(lifetimeType, 'lifetimeType');
  { TODO: Determine whether componentTypeInfo is assignable to serviceTypeInfo }

  model := AddComponentModel(name, serviceTypeInfo, componentTypeInfo, lifetimeType);
  ProcessModel(model);
end;

procedure TServiceRegistry.UnregisterAll;
begin
  fModels.Clear;
end;

function TServiceRegistry.AddComponentModel(const name: string;
  serviceTypeInfo, componentTypeInfo: PTypeInfo; lifetimeType: TLifetimeType): TComponentModel;
var
  serviceType: TRttiType;
  componentType: TRttiInstanceType;
  componentName: string;
begin
  serviceType := fRttiContext.GetType(serviceTypeInfo);
  componentType := fRttiContext.GetType(componentTypeInfo).AsInstance;
  Assert(serviceType <> nil, 'serviceType should not be nil.');
  Assert(componentType <> nil, 'componentType should not be nil.');
  CheckServiceType(serviceType);
  if name = '' then
  begin
    if componentType.IsPublicType then
      componentName := componentType.QualifiedName
    else
      componentName := componentType.Name;
  end
  else
  begin
    componentName := name;
  end;
  // TODO: Handle Duplication (AddOrUpdate)
  Result := TComponentModel.Create(name, serviceType, componentType);
  Result.LifetimeType := lifetimeType;
  fModels.Add(Result);
end;

procedure TServiceRegistry.ProcessModel(model: TComponentModel);
var
  inspector: IRegistrationInspector;
begin
  for inspector in fInspectors do
  begin
    inspector.ProcessModel(fContainerContext, model);
  end;
end;

function TServiceRegistry.FindOne(serviceType: PTypeInfo): TComponentModel;
var
  model: TComponentModel;
begin
  Result := nil;
  for model in fModels do
  begin
    if model.ServiceType.Handle = serviceType then
    begin
      Result := model;
      Break;
    end;
  end;
end;

function TServiceRegistry.FindOne(serviceType: PTypeInfo;
  const name: string): TComponentModel;
begin
  Result := FindOne(serviceType);
end;

function TServiceRegistry.FindOneByComponentType(
  componentType: PTypeInfo): TComponentModel;
var
  model: TComponentModel;
begin
  TArgument.CheckNotNull(componentType, 'componentType');
  Result := nil;
  for model in fModels do
  begin
    if model.ComponentType.Handle = componentType then
    begin
      Result := model;
      Break;
    end;
  end;
end;

function TServiceRegistry.GetFullTypeName(typeInfo: PTypeInfo): string;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  Result := fRttiContext.GetType(typeInfo).QualifiedName;
end;

function TServiceRegistry.HasServiceType(serviceType: PTypeInfo): Boolean;
begin
  Result := FindOne(serviceType) <> nil;
end;

{$ENDREGION}


end.
