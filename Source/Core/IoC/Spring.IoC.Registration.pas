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
    fInspectors: TList<IServiceInspector>;
    fModels: TList<TComponentModel>;
  protected
    procedure CheckServiceType(serviceType: TRttiType);
    function AddComponentModel(const name: string; serviceType,
      componentType: PTypeInfo; lifetimeType: TLifetimeType): TComponentModel; virtual;
    procedure ProcessModel(model: TComponentModel); virtual;
  public
    constructor Create(const context: IContainerContext);
    destructor Destroy; override;
    procedure AddInspector(const inspector: IServiceInspector);
    procedure RemoveInspector(const inspector: IServiceInspector);
    procedure ClearInspectors;
    procedure RegisterType(const name: string; serviceTypeInfo, componentTypeInfo: PTypeInfo; lifetimeType: TLifetimeType);
    procedure UnregisterAll;
    function FindComponentModel(serviceType: PTypeInfo): TComponentModel;
    function HasServiceType(serviceType: PTypeInfo): Boolean;
  end;

implementation

uses
  Spring.Helpers,
  Spring.ResourceStrings;

{$REGION 'TServiceRegistry'}

constructor TServiceRegistry.Create(const context: IContainerContext);
begin
  TArgument.CheckNotNull(context, 'context');
  inherited Create;
  fContainerContext := context;
  fRttiContext := TRttiContext.Create;
  fModels := TObjectList<TComponentModel>.Create;
  fInspectors := TList<IServiceInspector>.Create;
end;

destructor TServiceRegistry.Destroy;
begin
  fInspectors.Free;
  fModels.Free;
  fRttiContext.Free;
  inherited Destroy;
end;

procedure TServiceRegistry.AddInspector(const inspector: IServiceInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Add(inspector);
end;

procedure TServiceRegistry.RemoveInspector(const inspector: IServiceInspector);
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
  serviceType, componentType: PTypeInfo; lifetimeType: TLifetimeType): TComponentModel;
var
  serviceTypeObject: TRttiType;
  componentTypeObject: TRttiType;
begin
  serviceTypeObject := fRttiContext.GetType(serviceType);
  componentTypeObject := fRttiContext.GetType(componentType);
  Assert(serviceTypeObject <> nil, 'serviceTypeObject should not be nil.');
  Assert(componentTypeObject <> nil, 'componentTypeObject should not be nil.');
  CheckServiceType(serviceTypeObject);
  // TODO: Handle Duplication (AddOrUpdate)
  Result := TComponentModel.Create(name, serviceTypeObject, componentTypeObject);
  Result.LifetimeType := lifetimeType;
  fModels.Add(Result);
end;

procedure TServiceRegistry.ProcessModel(model: TComponentModel);
var
  inspector: IServiceInspector;
begin
  for inspector in fInspectors do
  begin
    inspector.ProcessModel(fContainerContext, model);
  end;
end;

function TServiceRegistry.FindComponentModel(serviceType: PTypeInfo): TComponentModel;
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

function TServiceRegistry.HasServiceType(serviceType: PTypeInfo): Boolean;
begin
  Result := FindComponentModel(serviceType) <> nil;
end;

{$ENDREGION}


end.
