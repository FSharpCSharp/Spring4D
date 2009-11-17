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
  /// <p>Neither normal interfaces nor generic interfaces have a guid identifier,
  /// so we can not determine whether an object supports a non-guid interface,
  /// further more, we can not safely cast an object to a non-guid interface
  /// variable. For these reasons, it's a pity that the two kinds of
  /// interfaces are not yet supported by the Spring IoC container.
  /// If you have a good idea, please drop me an email:
  /// baoquan.zuo[at]gmail.com (Paul). Thanks!</p>
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
    function FindEligibleMember(componentType: TRttiInstanceType; injectionType: TInjectionType;
      const memberName: string; argumentCount: Integer): TRttiMember;
    procedure InitializeServiceInspectors; virtual;
    procedure InjectMember(componentType: PTypeInfo; injectionType: TInjectionType;
      const memberName: string; const arguments: array of TValue); virtual;
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

//    function RegisterInstance<T>(instance: T): TContainer;

    { Experimental Injection Methods }
    function InjectConstructor<TComponentType: class>(const arguments: array of TValue): TContainer;
    function InjectProperty<TComponentType: class>(const propertyName: string; const value: TValue): TContainer;
    function InjectMethod<TComponentType: class>(const methodName: string; const arguments: array of TValue): TContainer;
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
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings;

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
  inspectors: TArray<IRegistrationInspector>;
  inspector: IRegistrationInspector;
begin
  inspectors := TArray<IRegistrationInspector>.Create(
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

function TContainer.RegisterType<TServiceType, TComponentType>(
  lifetimeType: TLifetimeType): TContainer;
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
  fServiceRegistry.RegisterType(name, serviceType, componentType, lifetimeType);
  Result := Self;
end;

function TContainer.FindEligibleMember(componentType: TRttiInstanceType;
  injectionType: TInjectionType; const memberName: string;
  argumentCount: Integer): TRttiMember;
var
  method: TRttiMethod;
begin
  Assert(componentType <> nil, 'componentType should not be nil.');
  case injectionType of
    itConstructor:
    begin
      Result := nil;
      for method in componentType.GetMethods do
      begin
        // TODO: Smart Match Constructor
        if method.IsConstructor and (Length(method.GetParameters) = argumentCount) then
        begin
          Result := method;
          Break;
        end;
      end;
    end;
    itProperty:
    begin
      Result := componentType.GetProperty(memberName);
    end;
    itMethod:
    begin
      Result := componentType.GetMethod(memberName);
    end;
    itField:
    begin
      Result := componentType.GetField(memberName);
    end;
    else
    begin
      raise EContainerException.Create('Unexpected injectionType.');
    end;
  end;
end;

procedure TContainer.InjectMember(componentType: PTypeInfo;
  injectionType: TInjectionType; const memberName: string;
  const arguments: array of TValue);
var
  model: TComponentModel;
  member: TRttiMember;
begin
  TArgument.CheckNotNull(componentType, 'componentType');
  model := fServiceRegistry.FindOneByComponentType(componentType);
  if model = nil then
  begin
    raise EContainerException.Create('No component was found');
  end;
  member := FindEligibleMember(model.ComponentType, injectionType, memberName, Length(arguments));
  if member = nil then
  begin
    raise EContainerException.Create('No member was matched');
  end;
  model.AddOrUpdateInjectionArguments(member, arguments);
end;

function TContainer.InjectConstructor<TComponentType>(
  const arguments: array of TValue): TContainer;
begin
  InjectMember(TypeInfo(TComponentType), itConstructor, '', arguments);
  Result := Self;
end;

function TContainer.InjectProperty<TComponentType>(const propertyName: string;
  const value: TValue): TContainer;
begin
  InjectMember(TypeInfo(TComponentType), itProperty, propertyName, value);
  Result := Self;
end;

function TContainer.InjectMethod<TComponentType>(const methodName: string;
  const arguments: array of TValue): TContainer;
begin
  InjectMember(TypeInfo(TComponentType), itMethod, methodName, arguments);
  Result := Self;
end;

function TContainer.InjectField<TComponentType>(const fieldName: string;
  const value: TValue): TContainer;
begin
  InjectMember(TypeInfo(TComponentType), itField, fieldName, value);
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
var
  model: TComponentModel;
begin
  TArgument.CheckNotNull(instance, 'instance');
  model := fServiceRegistry.FindOneByComponentType(instance.ClassInfo);
  model.LifetimeManager.Release(instance);
end;

procedure TContainer.Release(instance: IInterface);
begin
  TArgument.CheckNotNull(instance, 'instance');
end;

{$ENDREGION}

end.
