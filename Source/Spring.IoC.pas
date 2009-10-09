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
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  TContainer = class;

  IDependency = interface

  end;

  TConstructorDependency = class(TInterfacedObject, IDependency, IInterface)

  end;

  TPropertyDependency = class(TInterfacedObject, IDependency, IInterface)

  end;

  TMethodDependency = class(TInterfacedObject, IDependency, IInterface)

  end;

  TFieldDependency = class(TInterfacedObject, IDependency, IInterface)

  end;

  TDependencyParameter = class

  end;

  TLifetimeType = (
    ltUnknown,
    ltSingleton,
    ltTransient,
    ltPerThread,   // Reserved
    ltPooled,      // Reserved
    ltCustom       // Reserved
  );

  ILifetimeManager = interface
    ['{7DF9A902-B07A-468B-B201-B4561A921CF5}']
    function GetInstance: TObject;
    procedure Release(instance: TObject);
  end;

  {$REGION 'Attributes'}

  DependencyAttribute = class(TCustomAttribute)
  end;

  TLifetimeAttribute = class abstract(TCustomAttribute)
  private
    fLifetimeType: TLifetimeType;
  public
    constructor Create(lifetimeType: TLifetimeType);
    property LifetimeType: TLifetimeType read fLifetimeType;
  end;

  SingletonAttribute = class(TLifetimeAttribute)
  public
    constructor Create;
  end;

  TransientAttribute = class(TLifetimeAttribute)
  public
    constructor Create;
  end;

//  PerThreadAttribute = class(TLifetimeAttribute)
//  public
//    constructor Create;
//  end;

//  PooledAttribute = class(TLifetimeAttribute)
//  end;

//  CustomLifetimeAttribute = class(TLifetimeAttribute)
//  private
//    fLifetimeManagerType: PTypeInfo;
//  end;

  {$ENDREGION}


  TLifetimeManagerBase = class abstract(TInterfacedObject, ILifetimeManager, IInterface)
  public
    function GetInstance: TObject; virtual; abstract;
    procedure Release(instance: TObject); virtual; abstract;
  end;

  TComponentModel = class
  private
    fName: string;
    fServiceType: PTypeInfo;
    fComponentType: PTypeInfo;
    fLifetimeManager: ILifetimeManager;
    fConstructors: IList<TRttiMethod>;
    fProperties: IList<TRttiProperty>;
    fLifetimeType: TLifetimeType;
    function GetConstructors: IList<TRttiMethod>;
    function GetProperties: IList<TRttiProperty>;
  public
    constructor Create(const name: string; serviceType, componentType: PTypeInfo);
    property Name: string read fName;
    property ServiceType: PTypeInfo read fServiceType;
    property ComponentType: PTypeInfo read fComponentType;
    property LifetimeType: TLifetimeType read fLifetimeType write fLifetimeType;
    property Constructors: IList<TRttiMethod> read GetConstructors;
    property Properties: IList<TRttiProperty> read GetProperties;
//    property Methods: IList<TRttiProperty> read GetMethods;
//    property Fields: IList<TRttiProperty> read GetFields;
  end;

  TConstructorCandidate = class
  private
    fConstructorMethod: TRttiMethod;
    fParameters: IList<TObject>;
  end;

  TComponentModelBuilder = class
  private
    fContext: TRttiContext;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateComponentModel(const name: string; serviceTypeInfo, componentTypeInfo: PTypeInfo;
      lifetimeType: TLifetimeType): TComponentModel;
  end;

  TComponentModelManager = class
  private
    fModels: TList<TComponentModel>;
  end;

  /// <summary>
  /// Represents an abstract IoC (Inversion of Control) container.
  /// </summary>
  /// <remarks>
  /// </remarks>
  TContainer = class//(TInterfacedObject, IContainer)
  private
    fTypes: TDictionary<PTypeInfo, PTypeInfo>;
    fBuilder: TComponentModelBuilder;
    fContext: TRttiContext;
    fItems: TList<TObject>;
  protected
    { IContainer (Temp) }
    function Invoke: TContainer;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterComponent<TServiceType; TComponentType: TServiceType>: TContainer; overload;
//    function RegisterComponent<TServiceType; TComponentType: TServiceType>(lifetime: TLifetimeType): TContainer; overload;
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

implementation

{ TLifetimeAttribute }

constructor TLifetimeAttribute.Create(lifetimeType: TLifetimeType);
begin
  inherited Create;
  fLifetimeType := lifetimeType;
end;

{ SingletonAttribute }

constructor SingletonAttribute.Create;
begin
  inherited Create(TLifetimeType.ltSingleton);
end;

{ TransientAttribute }

constructor TransientAttribute.Create;
begin
  inherited Create(TLifetimeType.ltTransient);
end;

{ TContainer }

constructor TContainer.Create;
begin
  inherited Create;
  fBuilder := TComponentModelBuilder.Create;
  fTypes := TDictionary<PTypeInfo, PTypeInfo>.Create;
  fContext := TRttiContext.Create;
  fItems := TObjectList<TObject>.Create;
end;

destructor TContainer.Destroy;
begin
  fContext.Free;
  fTypes.Free;
  fItems.Free;
  fBuilder.Free;
  inherited Destroy;
end;

function TContainer.Invoke: TContainer;
begin
  Result := Self as TContainer;
end;

function TContainer.RegisterComponent<TServiceType, TComponentType>: TContainer;
begin
  Result := RegisterComponent('', TypeInfo(TServiceType), TypeInfo(TComponentType), ltSingleton, []);
end;

function TContainer.Resolve(typeInfo: PTypeInfo): Pointer;
var
  componentTypeInfo: PTypeInfo;
  instance: TObject;
  componentType: TRttiInstanceType;
  method: TRttiMethod;
  parameter: TRttiParameter;
  params: TArray<TRttiParameter>;
  constructorMethod: TRttiMethod;
  constructorparameters: TArray<TValue>;
  value: TValue;
  i: Integer;
  obj: TObject;
  intf: IInterface;
begin
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');

//  Result := fBuilder.Resolve(typeInfo);

  if not fTypes.TryGetValue(typeInfo, componentTypeInfo) then
  begin
    componentTypeInfo := typeInfo;
  end;
  componentType := fContext.GetType(componentTypeInfo) as TRttiInstanceType;

  constructorMethod := nil;
  for method in componentType.GetMethods do
  begin
    if method.IsConstructor then
    begin
      constructorMethod := method;
      Break;
    end;
  end;
  if constructorMethod = nil then
  begin
    raise EContainerException.Create('No constructor was found.');
  end;
  instance := componentType.MetaclassType.NewInstance;
  params := constructorMethod.GetParameters;
  SetLength(constructorparameters, Length(params));
  for i := 0 to High(params) do
  begin
    parameter := params[i];
    if parameter.ParamType is TRttiInstanceType then
    begin
      obj := TObject(Resolve(parameter.ParamType.Handle));
    end
    else if parameter.ParamType is TRttiInterfaceType then
    begin
      intf := IInterface(Resolve(parameter.ParamType.Handle));
      TValue.MakeWithoutCopy(@intf, parameter.ParamType.Handle, value);
      constructorparameters[i] := value;
    end
    else
    begin
      raise EContainerException.Create('Unsupported parameter type.');
    end;
  end;
  constructorMethod.Invoke(instance, constructorparameters);
  instance.AfterConstruction; // TEMP
  case typeInfo.Kind of
    TTypeKind.tkClass:
    begin
      fItems.Add(instance);
      PObject(@Result)^ := instance;
    end;
    TTypeKind.tkInterface:
    begin
      instance.GetInterface(GetTypeData(typeInfo).Guid, Result);
    end;
    else
    begin
      raise EContainerException.Create('Unsupported type kind.');
    end;
  end;
end;

function TContainer.Resolve<T>: T;
begin
  TRtti.CheckTypeKind<T>([tkClass, tkInterface]);
  PPointer(@Result)^ := Resolve(TypeInfo(T));
end;

function TContainer.RegisterComponent(const name: string; serviceType,
  componentType: PTypeInfo; lifetime: TLifetimeType;
  const dependencies: array of IDependency): TContainer;
begin
  fTypes.AddOrSetValue(serviceType, componentType);
end;

{ TComponentModel }

constructor TComponentModel.Create(const name: string;
  serviceType, componentType: PTypeInfo);
begin
  inherited Create;
  fName := name;
  fServiceType := serviceType;
  fComponentType := componentType;
end;

function TComponentModel.GetConstructors: IList<TRttiMethod>;
begin
  if fConstructors = nil then
  begin
    fConstructors := TContainers.CreateList<TRttiMethod>;
  end;
end;

function TComponentModel.GetProperties: IList<TRttiProperty>;
begin
  if fProperties = nil then
  begin
    fProperties := TContainers.CreateList<TRttiProperty>;
  end;
end;

{ TComponentModelBuilder }

constructor TComponentModelBuilder.Create;
begin
  inherited Create;
  fContext := TRttiContext.Create;
end;

destructor TComponentModelBuilder.Destroy;
begin
  fContext.Free;
  inherited Destroy;
end;

function TComponentModelBuilder.CreateComponentModel(const name: string;
  serviceTypeInfo, componentTypeInfo: PTypeInfo;
  lifetimeType: TLifetimeType): TComponentModel;
var
  serviceType: TRttiStructuredType;
  componentType: TRttiInstanceType;
  method: TRttiMethod;
  parameter: TRttiParameter;
  params: TArray<TRttiParameter>;
  constructorMethod: TRttiMethod;
  constructorparameters: TArray<TValue>;
  propertyMember: TRttiProperty;
begin
  Result := TComponentModel.Create(name, serviceTypeInfo, componentTypeInfo);
  serviceType := fContext.GetType(serviceTypeInfo) as TRttiStructuredType;
  componentType := fContext.GetType(componentTypeInfo) as TRttiInstanceType;
  constructorMethod := nil;
  for method in componentType.GetMethods do
  begin
    if method.IsConstructor then
    begin
      Result.Constructors.Add(method);
    end;
  end;
  for propertyMember in componentType.GetProperties do
  begin
    if propertyMember.IsWritable and (propertyMember.PropertyType.TypeKind in [tkClass, tkInterface]) then
    begin
      Result.Properties.Add(propertyMember);
    end;
  end;
end;

end.
