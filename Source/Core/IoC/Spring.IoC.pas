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
  SysUtils,
  TypInfo,
  Rtti,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.IoC.Core;

type
  TValue = Rtti.TValue;

  /// <summary>
  /// Represents an Inversion of Control (IoC) container.
  /// </summary>
  /// <remarks>
  /// </remarks>
  TContainer = class(TInterfaceBase, IContainerContext, IInterface)
  private
    fServiceRegistry: IServiceRegistry;
    fServiceResolver: IServiceResolver;
    fDependencyResolver: IDependencyResolver;
  protected
    { Implements IContainerContext }
    function CreateLifetimeManager(model: TComponentModel): ILifetimeManager;
    function GetDependencyResolver: IDependencyResolver;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
  protected
    procedure InitializeServiceInspectors; virtual;
    function RegisterType(const name: string; serviceType, componentType: PTypeInfo;
      lifetimeType: TLifetimeType): TContainer; overload;
    function Resolve(typeInfo: PTypeInfo; const name: string): TValue; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function RegisterType<TServiceType; TComponentType: TServiceType>: TContainer; overload;
    function RegisterType<TServiceType; TComponentType: TServiceType>(lifetimeType: TLifetimeType): TContainer; overload;
//    function RegisterType<TServiceType; TComponentType: TServiceType>(const lifetimeManager: ILifetimeManager): TContainer; overload;
    function RegisterType<TServiceType; TComponentType: TServiceType>(const name: string): TContainer; overload;
    function RegisterType<TServiceType; TComponentType: TServiceType>(const name: string; lifetimeType: TLifetimeType): TContainer; overload;
//    function RegisterType<TServiceType; TComponentType: TServiceType>(const name: string;const lifetimeManager: ILifetimeManager): TContainer; overload;

//    function RegisterInstance<T: class>(instance: T): TContainer;

    { Experimental Injection Methods }
    function InjectConstructor<TComponentType: class>(const parameters: TArray<TValue>): TContainer;
    function InjectProperty<TComponentType: class>(const propertyName: string; const value: TValue): TContainer;
    function InjectMethod<TComponentType: class>(const methodName: string; const parameters: TArray<TValue>): TContainer;
    function InjectField<TComponentType: class>(const fieldName: string; const value: TValue): TContainer;

    function Resolve<T>: T; overload;
    function Resolve<T>(const name: string): T; overload;

//    function ResolveAll<TServiceType>: TArray<TServiceType>;

    { Experimental Release Methods }
    procedure Release(instance: TObject); overload;
    procedure Release(instance: IInterface); overload;
  end;

  EContainerException = Spring.IoC.Core.EContainerException;

implementation

uses
  Spring.IoC.Registration,
  Spring.IoC.Registration.Inspectors,
  Spring.IoC.LifetimeManager,
  Spring.IoC.Injection,
  Spring.IoC.Resolvers,
  Spring.ResourceStrings;

{$REGION 'TContainer'}

constructor TContainer.Create;
begin
  inherited Create;
  fServiceRegistry := TServiceRegistry.Create(Self);
  fServiceResolver := TServiceResolver.Create(fServiceRegistry);
  fDependencyResolver := TDependencyResolver.Create(fServiceRegistry);
end;

destructor TContainer.Destroy;
begin
  fServiceRegistry.UnregisterAll;
  fServiceRegistry.ClearInspectors;
  inherited Destroy;
end;

procedure TContainer.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeServiceInspectors;
end;

procedure TContainer.InitializeServiceInspectors;
var
  inspectors: TArray<IServiceInspector>;
  inspector: IServiceInspector;
begin
  inspectors := TArray<IServiceInspector>.Create(
    TLifetimeInspector.Create,
    TComponentActivatorInspector.Create,
    TConstructorsInspector.Create,
    TPropertiesInspector.Create,
    TMethodsInspector.Create,
    TFieldsInspector.Create
  );
  for inspector in inspectors do
  begin
    fServiceRegistry.AddInspector(inspector);
  end;
end;

function TContainer.CreateLifetimeManager(
  model: TComponentModel): ILifetimeManager;
begin
  TArgument.CheckNotNull(model, 'model');
  case model.LifetimeType of
    ltSingleton:
    begin
      Result := TSingletonLifetimeManager.Create(model);
    end;
    ltTransient:
    begin
      Result := TTransientLifetimeManager.Create(model);
    end;
//    ltPerThread:
//    begin
//
//    end;
//    ltPooled:
//    begin
//
//    end;
    else
    begin
      raise ERegistrationException.Create('Unsupported lifetimeType.');
    end;
  end;
end;

function TContainer.GetDependencyResolver: IDependencyResolver;
begin
  Result := fDependencyResolver;
end;

function TContainer.RegisterType<TServiceType, TComponentType>: TContainer;
begin
  Result := RegisterType('', TypeInfo(TServiceType), TypeInfo(TComponentType), ltUnknown);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(lifetimeType: TLifetimeType): TContainer;
begin
  Result := RegisterType('', TypeInfo(TServiceType), TypeInfo(TComponentType), lifetimeType);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const name: string): TContainer;
begin
  Result := RegisterType(name, TypeInfo(TServiceType), TypeInfo(TComponentType), ltUnknown);
end;

function TContainer.RegisterType<TServiceType, TComponentType>(
  const name: string; lifetimeType: TLifetimeType): TContainer;
begin
  Result := RegisterType(name, TypeInfo(TServiceType), TypeInfo(TComponentType), lifetimeType);
end;

function TContainer.RegisterType(const name: string; serviceType,
  componentType: PTypeInfo; lifetimeType: TLifetimeType): TContainer;
begin
  TArgument.CheckNotNull(serviceType, 'serviceType');
  TArgument.CheckNotNull(componentType, 'componentType');
  TArgument.CheckEnum<TLifetimeType>(lifetimeType, 'lifetimeType');
  TArgument.CheckTypeKind(componentType, [tkClass], 'componentType');
  fServiceRegistry.RegisterType(name, serviceType, componentType, lifetimeType);
  Result := Self;
end;

function TContainer.InjectConstructor<TComponentType>(
  const parameters: TArray<TValue>): TContainer;
begin

  Result := Self;
end;

function TContainer.InjectProperty<TComponentType>(const propertyName: string;
  const value: TValue): TContainer;
begin

  Result := Self;
end;

function TContainer.InjectMethod<TComponentType>(const methodName: string;
  const parameters: TArray<TValue>): TContainer;
begin

  Result := Self;
end;

function TContainer.InjectField<TComponentType>(const fieldName: string;
  const value: TValue): TContainer;
begin

  Result := Self;
end;

function TContainer.Resolve<T>: T;
//var
//  name: string;
begin
//  name := TRtti.GetFullName(TypeInfo(T));
  Result := Resolve<T>('');
end;

function TContainer.Resolve<T>(const name: string): T;
var
  value: TValue;
begin
  TRtti.CheckTypeKind<T>([tkClass, tkInterface]);
  value := Resolve(TypeInfo(T), name);
  Result := value.AsType<T>;
end;

function TContainer.Resolve(typeInfo: PTypeInfo; const name: string): TValue;
begin
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');
  Result := fServiceResolver.Resolve(typeInfo, name);
end;

procedure TContainer.Release(instance: TObject);
//var
//  model: TComponentModel;
begin
  TArgument.CheckNotNull(instance, 'instance');
end;

procedure TContainer.Release(instance: IInterface);
begin
  TArgument.CheckNotNull(instance, 'instance');

end;

{$ENDREGION}

end.
