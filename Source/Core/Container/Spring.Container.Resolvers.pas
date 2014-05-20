{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Container.Resolvers;

{$I Spring.inc}

interface

uses
  Rtti,
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver)
  private
    fKernel: IKernel;
    fLock: TCriticalSection;
    fDependencies: IList<TComponentModel>;
    fSubResolvers: IList<ISubDependencyResolver>;
  protected
    procedure CheckCircularDependency(const model: TComponentModel);
    function GetEligibleModel(
      const dependency: TRttiType; const argument: TValue): TComponentModel;

    function ResolveLazyDependency(
      const dependency: TRttiType; const argument: TValue): TValue;
    function ResolveManyDependency(
      const dependency: TRttiType; const argument: TValue): TValue;
    property Kernel: IKernel read fKernel;
  public
    constructor Create(const kernel: IKernel);
    destructor Destroy; override;

    function CanResolve(
      const dependency: TRttiType; const argument: TValue): Boolean; overload; virtual;
    function CanResolve(
      const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
      const target: TRttiMember): Boolean; overload; virtual;

    function Resolve(
      const dependency: TRttiType; const argument: TValue): TValue; overload; virtual;
    function Resolve(
      const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
      const target: TRttiMember): TArray<TValue>; overload; virtual;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  TServiceResolver = class(TInterfacedObject, IServiceResolver)
  private
    fKernel: IKernel;
    fRegistry: IComponentRegistry;
  protected
    function InternalResolve(const model: TComponentModel; serviceType: PTypeInfo;
      const resolver: IDependencyResolver): TValue;
    function InternalResolveLazy(const model: TComponentModel; serviceType: PTypeInfo;
      const resolver: IDependencyResolver): TValue;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(serviceType: PTypeInfo): Boolean; overload;
    function CanResolve(const name: string): Boolean; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo; const resolverOverride: IResolverOverride): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function Resolve(const name: string; const resolverOverride: IResolverOverride): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  TResolverOverride = class(TInterfacedObject, IResolverOverride)
  public
    function GetResolver(const kernel: IKernel): IDependencyResolver; virtual; abstract;
  end;

  TParameterOverride = class(TResolverOverride)
  private
    fName: string;
    fValue: TValue;

    type
      TResolver = class(TDependencyResolver)
      private
        fName: string;
        fValue: TValue;
      public
        constructor Create(const kernel: IKernel;
          const name: string; const value: TValue);

        function CanResolve(
          const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
          const target: TRttiMember): Boolean; override;
        function Resolve(
          const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
          const target: TRttiMember): TArray<TValue>; override;
      end;
  public
    constructor Create(const name: string; const value: TValue);

    function GetResolver(const kernel: IKernel): IDependencyResolver; override;
  end;

  TOrderedParametersOverride = class(TResolverOverride)
  private
    fArguments: TArray<TValue>;

    type
      TResolver = class(TDependencyResolver)
      private
        fArguments: TArray<TValue>;
      public
        constructor Create(const kernel: IKernel; const arguments: TArray<TValue>);

        function CanResolve(
          const dependency: TRttiType; const argument: TValue): Boolean; override;

        function CanResolve(
          const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
          const target: TRttiMember): Boolean; override;
        function Resolve(
          const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
          const target: TRttiMember): TArray<TValue>; override;
      end;
  public
    constructor Create(const arguments: array of TValue);

    function GetResolver(const kernel: IKernel): IDependencyResolver; override;
  end;

  TDependencyOverride = class(TResolverOverride)
  private
    fTypeInfo: PTypeInfo;
    fValue: TValue;

    type
      TResolver = class(TDependencyResolver)
      private
        fTypeInfo: PTypeInfo;
        fValue: TValue;
      public
        constructor Create(const kernel: IKernel;
          typeInfo: PTypeInfo; const value: TValue);

        function CanResolve(
          const dependency: TRttiType; const argument: TValue): Boolean; override;
        function Resolve(
          const dependency: TRttiType; const argument: TValue): TValue; override;
      end;
  public
    constructor Create(typeInfo: PTypeInfo; const value: TValue);

    function GetResolver(const kernel: IKernel): IDependencyResolver; override;

    property TypeInfo: PTypeInfo read fTypeInfo;
    property Value: TValue read fValue;
  end;

  TDependencyOverride<T> = class(TDependencyOverride)
  public
    constructor Create(const value: TValue);
  end;

  TDependencyOverrides = class(TResolverOverride)
  private
    fDependencyOverrides: IList<TDependencyOverride>;

    type
      TResolver = class(TDependencyResolver)
      private
        fDependencyOverrides: IList<TDependencyOverride>;
      public
        constructor Create(const kernel: IKernel;
          const dependencyOverrides: IList<TDependencyOverride>);

        function CanResolve(
          const dependency: TRttiType; const argument: TValue): Boolean; override;
        function Resolve(
          const dependency: TRttiType; const argument: TValue): TValue; override;
      end;
  public
    constructor Create(const dependencyOverrides: array of TDependencyOverride);

    function GetResolver(const kernel: IKernel): IDependencyResolver; override;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring.Container.ResourceStrings,
  Spring.Helpers,
  Spring.Reflection;


function ResolveValue(typeInfo: PTypeInfo; const instance: TValue): TValue;
var
  localInterface: Pointer;
begin
  Guard.CheckFalse(instance.IsEmpty, 'instance should not be empty.');

  case typeInfo.Kind of
    tkClass, tkRecord:
      Result := instance;
    tkInterface:
    begin
      if instance.IsObject then
        instance.AsObject.GetInterface(GetTypeData(typeInfo).Guid, localInterface)
      else
      begin
        if TType.IsDelegate(typeInfo) then
        begin
          localInterface := nil;
          IInterface(localInterface) := instance.AsInterface;
        end
        else
          instance.AsInterface.QueryInterface(GetTypeData(typeInfo).Guid, localInterface);
      end;
      TValue.MakeWithoutCopy(@localInterface, typeInfo, Result);
    end;
  else
    Result := nil;
  end;
end;


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const kernel: IKernel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  inherited Create;
  fKernel := kernel;
  fDependencies := TCollections.CreateList<TComponentModel>(False);
  fLock := TCriticalSection.Create;
  fSubResolvers := TCollections.CreateInterfaceList<ISubDependencyResolver>;
end;

destructor TDependencyResolver.Destroy;
begin
  fLock.Free;
  inherited;
end;

procedure TDependencyResolver.AddSubResolver(const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Add(subResolver);
end;

procedure TDependencyResolver.RemoveSubResolver(const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Remove(subResolver);
end;

function TDependencyResolver.GetEligibleModel(
  const dependency: TRttiType; const argument: TValue): TComponentModel;
var
  name: string;
begin
  if argument.IsEmpty then
  begin
    if not Kernel.ComponentRegistry.HasService(dependency.Handle) then
    begin
      if dependency.IsClassOrInterface and not TType.IsLazy(dependency.Handle) then
        raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
      Result := nil;
    end
    else
    begin
      Result := Kernel.ComponentRegistry.FindDefault(dependency.Handle);
      if not Assigned(Result) then
        raise EUnsatisfiedDependencyException.CreateResFmt(
          @SUnsatisfiedDependency, [dependency.Name]);
    end;
  end
  else
  begin
    name := argument.AsString;
    Result := Kernel.ComponentRegistry.FindOne(name);
    if not Assigned(Result) then
    begin
      if TType.IsLazy(dependency.Handle) then
        Exit;
      raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
    end;
    if not Result.HasService(dependency.Handle) then
    begin
      if not TType.IsLazy(dependency.Handle) then
        raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
      Result := nil;
    end;
  end;
end;

procedure TDependencyResolver.CheckCircularDependency(const model: TComponentModel);
begin
  Guard.CheckNotNull(model, 'model');

  if fDependencies.Contains(model) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [model.ComponentType.Name]);
end;

function TDependencyResolver.CanResolve(
  const dependency: TRttiType; const argument: TValue): Boolean;
var
  serviceName: string;
  serviceType: PTypeInfo;
  lazyType: TRttiType;
  model: TComponentModel;
begin
  if dependency.IsClassOrInterface or dependency.IsRecord then
  begin
    if argument.IsEmpty then
      Result := Kernel.ComponentRegistry.HasDefault(dependency.Handle)
        or TType.IsLazy(dependency.Handle)
    else
    begin
      Result := argument.IsType<string>;
      if Result then
      begin
        serviceName := argument.AsString;
        model := Kernel.ComponentRegistry.FindOne(serviceName);
        Result := Assigned(model);
        if Result then
        begin
          serviceType := model.Services[serviceName];
          Result := serviceType = dependency.Handle;
          if not Result and TType.IsLazy(dependency.Handle) then
          begin
            lazyType := TType.FindType(TType.GetLazyTypeName(dependency.Handle));
            Result := Assigned(lazyType) and (serviceType = lazyType.Handle);
          end;
        end;
      end;
    end;
  end
  else
    Result := argument.IsEmpty or argument.IsType(dependency.Handle);
end;

function TDependencyResolver.Resolve(
  const dependency: TRttiType; const argument: TValue): TValue;
var
  model: TComponentModel;
  instance: TValue;
begin
  Guard.CheckNotNull(dependency, 'dependency');

  if dependency.IsDynamicArray then
  begin
    Result := ResolveManyDependency(dependency, argument);
    Exit;
  end;

  if not (dependency.IsClassOrInterface or dependency.IsRecord)
    or (argument.Kind in [tkClass, tkInterface, tkRecord]) then
  begin
    Result := argument;
{$IFDEF DELPHI2010}
    if Result.IsEmpty then
      TValue.Make(nil, dependency.Handle, Result);
{$ENDIF}
    Exit;
  end;

  fLock.Enter;
  try
    model := GetEligibleModel(dependency, argument);
    if not Assigned(model) then
    begin
      Result := ResolveLazyDependency(dependency, argument);
      Exit;
    end;
    CheckCircularDependency(model);
    fDependencies.Add(model);
    try
      instance := model.LifetimeManager.Resolve(Self);
    finally
      fDependencies.Remove(model);
    end;
  finally
    fLock.Leave;
  end;

  Result := ResolveValue(dependency.Handle, instance);
end;

function TDependencyResolver.ResolveLazyDependency(
  const dependency: TRttiType; const argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  name: string;
  lazyType: TRttiType;
  model: TComponentModel;
  value: TValue;
  valueFactoryObj: TFunc<TObject>;
  valueFactoryIntf: TFunc<IInterface>;
begin
  lazyKind := TType.GetLazyKind(dependency.Handle);
  if lazyKind = lkNone then
    Exit(argument);

  name := TType.GetLazyTypeName(dependency.Handle);
  lazyType := TType.FindType(name);
  if not Assigned(lazyType) or not CanResolve(lazyType, argument) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);

  model := GetEligibleModel(lazyType, argument);
  CheckCircularDependency(model);

  case lazyType.TypeKind of
    tkClass:
    begin
      value := argument;
      valueFactoryObj :=
        function: TObject
        begin
          Result := Resolve(lazyType, value).AsObject;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<TObject>>(valueFactoryObj);
        lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(valueFactoryObj));
        lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(valueFactoryObj));
      end;
    end;
    tkInterface:
    begin
      value := argument;
      valueFactoryIntf :=
        function: IInterface
        begin
          Result := Resolve(lazyType, value).AsInterface;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<IInterface>>(valueFactoryIntf);
        lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(valueFactoryIntf));
        lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(valueFactoryIntf));
      end;
    end;
  else
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
  end;

  TValueData(Result).FTypeInfo := dependency.Handle;
end;

function TDependencyResolver.ResolveManyDependency(
  const dependency: TRttiType; const argument: TValue): TValue;
var
  dependencyType: TRttiType;
  serviceType: PTypeInfo;
  lazyTypeName: string;
  models: IEnumerable<TComponentModel>;
  values: array of TValue;
  i: Integer;
  model: TComponentModel;
begin
  if not dependency.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);

  dependencyType := dependency.AsDynamicArray.ElementType;
  if TType.IsLazy(dependencyType.Handle) then
  begin
    lazyTypeName := TType.GetLazyTypeName(dependencyType.Handle);
    serviceType := TType.FindType(lazyTypeName).Handle;
  end
  else
    serviceType := dependencyType.Handle;
  models := Kernel.ComponentRegistry.FindAll(serviceType);
  SetLength(values, models.Count);
  i := 0;
  for model in models do
  begin
    values[i] := Resolve(dependencyType, model.GetServiceName(serviceType));
    Inc(i);
  end;
  Result := TValue.FromArray(dependency.Handle, values);
end;

function TDependencyResolver.CanResolve(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
  const target: TRttiMember): Boolean;
var
  dependency: TRttiType;
  i: Integer;
begin
  Result := True;
  if Length(dependencies) = Length(arguments) then
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      if not CanResolve(dependency, arguments[i]) then
        Exit(False);
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
      if not CanResolve(dependency, nil) then
        Exit(False);
  end
  else
    Exit(False);
end;

function TDependencyResolver.Resolve(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
  const target: TRttiMember): TArray<TValue>;
var
  dependency: TRttiType;
  hasArgument: Boolean;
  i: Integer;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  SetLength(Result, Length(dependencies));
  if hasArgument then
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := Resolve(dependency, arguments[i]);
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := Resolve(dependency, nil);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TServiceResolver'}

constructor TServiceResolver.Create(const kernel: IKernel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  inherited Create;
  fKernel := kernel;
  fRegistry := fkernel.ComponentRegistry;
end;

function TServiceResolver.CanResolve(serviceType: PTypeInfo): Boolean;
begin
  Result := fRegistry.HasService(serviceType);
end;

function TServiceResolver.CanResolve(const name: string): Boolean;
begin
  Result := fRegistry.HasService(name);
end;

function TServiceResolver.InternalResolve(const model: TComponentModel;
  serviceType: PTypeInfo; const resolver: IDependencyResolver): TValue;
var
  instance: TValue;
begin
  Guard.CheckNotNull(model, 'model');
  Guard.CheckNotNull(serviceType, 'serviceType');
  Guard.CheckNotNull(model.LifetimeManager, 'model.LifetimeManager');

  instance := model.LifetimeManager.Resolve(resolver);
  Result := ResolveValue(serviceType, instance);
end;

function TServiceResolver.InternalResolveLazy(const model: TComponentModel;
  serviceType: PTypeInfo; const resolver: IDependencyResolver): TValue;
var
  lazyKind: TLazyKind;
  name: string;
  lazyType: PTypeInfo;
  valueFactoryObj: TFunc<TObject>;
  valueFactoryIntf: TFunc<IInterface>;
begin
  Guard.CheckNotNull(model, 'model');
  Guard.CheckNotNull(serviceType, 'serviceType');
  Guard.CheckNotNull(model.LifetimeManager, 'model.LifetimeManager');

  lazyKind := TType.GetLazyKind(serviceType);
  if lazyKind <> lkNone then
  begin
    name := TType.GetLazyTypeName(serviceType);
    lazyType := TType.FindType(TType.GetLazyTypeName(serviceType)).Handle;
  end;

  case lazyType.Kind of
    tkClass:
    begin
      valueFactoryObj :=
        function: TObject
        begin
          Result := InternalResolve(model, lazyType, resolver).AsObject;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<TObject>>(valueFactoryObj);
        lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(valueFactoryObj));
        lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(valueFactoryObj));
      end;
    end;
    tkInterface:
    begin
      valueFactoryIntf :=
        function: IInterface
        begin
          Result := InternalResolve(model, lazyType, resolver).AsInterface;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<IInterface>>(valueFactoryIntf);
        lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(valueFactoryIntf));
        lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(valueFactoryIntf));
      end;
    end;
  end;

  TValueData(Result).FTypeInfo := serviceType;
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo): TValue;
begin
  Result := Resolve(serviceType, nil);
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo;
  const resolverOverride: IResolverOverride): TValue;
var
  serviceName: string;
  resolveLazy: Boolean;
  rttiType: TRttiType;
  lazyType: PTypeInfo;
  model: TComponentModel;
  resolver: IDependencyResolver;
begin
  resolveLazy := not fRegistry.HasService(serviceType) and TType.IsLazy(serviceType);
  lazyType := serviceType;
  if resolveLazy then
  begin
    rttiType := TType.FindType(TType.GetLazyTypeName(serviceType));
    if Assigned(rttiType) then
      serviceType := rttiType.Handle;
  end;

  serviceName := GetTypeName(serviceType);

  if not fRegistry.HasService(serviceType) then
    raise EResolveException.CreateResFmt(@SNoComponentRegistered, [serviceName])
  else
  begin
    model := fRegistry.FindDefault(serviceType);
    if not Assigned(model) then
      raise EUnsatisfiedDependencyException.CreateResFmt(
        @SUnsatisfiedDependency, [serviceName]);
  end;
  if Assigned(resolverOverride) then
    resolver := resolverOverride.GetResolver(fKernel)
  else
    resolver := fKernel.DependencyResolver;

  if resolveLazy then
    Result := InternalResolveLazy(model, lazyType, resolver)
  else
    Result := InternalResolve(model, serviceType, resolver);
end;

function TServiceResolver.Resolve(const name: string): TValue;
begin
  Result := Resolve(name, nil);
end;

function TServiceResolver.Resolve(const name: string;
  const resolverOverride: IResolverOverride): TValue;
var
  model: TComponentModel;
  serviceType: PTypeInfo;
  resolver: IDependencyResolver;
begin
  model := fRegistry.FindOne(name);
  if not Assigned(model) then
    raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
  serviceType := model.GetServiceType(name);
  if Assigned(resolverOverride) then
    resolver := resolverOverride.GetResolver(fKernel)
  else
    resolver := fKernel.DependencyResolver;
  Result := InternalResolve(model, serviceType, resolver);
end;

function TServiceResolver.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
var
  isLazy: Boolean;
  models: IEnumerable<TComponentModel>;
  model: TComponentModel;
  i: Integer;
  modelType: PTypeInfo;
begin
  isLazy := TType.IsLazy(serviceType);
  if isLazy then
    modelType := TType.FindType(TType.GetLazyTypeName(serviceType)).Handle
  else
    modelType := serviceType;
  models := fRegistry.FindAll(modelType);
  SetLength(Result, models.Count);
  i := 0;
  for model in models do
  begin
    if isLazy then
      Result[i] := InternalResolveLazy(model, serviceType, fKernel.DependencyResolver)
    else
      Result[i] := InternalResolve(model, serviceType, fKernel.DependencyResolver);
    Inc(i);
  end;
end;

{$ENDREGION}


{$REGION 'TOrderedParametersOverride'}

constructor TOrderedParametersOverride.Create(const arguments: array of TValue);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fArguments, Length(arguments));
  for i := Low(arguments) to High(arguments) do
    fArguments[i] := arguments[i];
end;

function TOrderedParametersOverride.GetResolver(const kernel: IKernel): IDependencyResolver;
begin
  Result := TResolver.Create(kernel, fArguments);
end;

{$ENDREGION}


{$REGION 'TOrderedParametersOverride.TResolver'}

constructor TOrderedParametersOverride.TResolver.Create(const kernel: IKernel;
  const arguments: TArray<TValue>);
begin
  inherited Create(kernel);
  fArguments := arguments;
end;

function TOrderedParametersOverride.TResolver.CanResolve(
  const dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := argument.IsType(dependency.Handle);
end;

function TOrderedParametersOverride.TResolver.CanResolve(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
  const target: TRttiMember): Boolean;
begin
  Result := inherited CanResolve(dependencies, fArguments, target);
end;

function TOrderedParametersOverride.TResolver.Resolve(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
  const target: TRttiMember): TArray<TValue>;
begin
  if target.IsConstructor then
    Result := fArguments
  else
    Result := Kernel.DependencyResolver.Resolve(
      dependencies, arguments, target);
end;

{$ENDREGION}


{$REGION 'TParameterOverride'}

constructor TParameterOverride.Create(const name: string; const value: TValue);
begin
  inherited Create;
  fName := name;
  fValue := value;
end;

function TParameterOverride.GetResolver(
  const kernel: IKernel): IDependencyResolver;
begin
  Result := TResolver.Create(kernel, fName, fValue);
end;

{$ENDREGION}


{$REGION 'TParameterOverride.TResolver'}

constructor TParameterOverride.TResolver.Create(const kernel: IKernel;
  const name: string; const value: TValue);
begin
  inherited Create(kernel);
  fName := name;
  fValue := value;
end;

function TParameterOverride.TResolver.CanResolve(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
  const target: TRttiMember): Boolean;
var
  dependency: TRttiType;
  i: Integer;
  parameters: TArray<TRttiParameter>;
begin
  Result := True;
  if Length(dependencies) = Length(arguments) then
  begin
    parameters := target.AsMethod.GetParameters;
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      if SameText(parameters[i].Name, fName) then
        Continue;
      if not CanResolve(dependency, arguments[i]) then
        Exit(False);
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
      if not CanResolve(dependency, nil) then
        Exit(False);
  end
  else
    Exit(False);
end;

function TParameterOverride.TResolver.Resolve(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>;
  const target: TRttiMember): TArray<TValue>;
var
  dependency: TRttiType;
  hasArgument: Boolean;
  i: Integer;
  parameters: TArray<TRttiParameter>;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  SetLength(Result, Length(dependencies));
  if hasArgument then
  begin
    parameters := target.AsMethod.GetParameters;
    for i := 0 to High(dependencies) do
    begin
      if SameText(parameters[i].Name, fName) then
        Result[i] := fValue
      else
      begin
        dependency := dependencies[i];
        Result[i] := Resolve(dependency, arguments[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := Resolve(dependency, nil);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TDependencyOverride'}

constructor TDependencyOverride.Create(typeInfo: PTypeInfo;
  const value: TValue);
begin
  inherited Create;
  fTypeInfo := typeInfo;
  fValue := value;
end;

function TDependencyOverride.GetResolver(
  const kernel: IKernel): IDependencyResolver;
begin
  Result := TResolver.Create(kernel, fTypeInfo, fValue);
end;

{$ENDREGION}


{$REGION 'TDependencyOverride.TResolver'}

constructor TDependencyOverride.TResolver.Create(const kernel: IKernel;
  typeInfo: PTypeInfo; const value: TValue);
begin
  inherited Create(kernel);
  fTypeInfo := typeInfo;
  fValue := value;
end;

function TDependencyOverride.TResolver.CanResolve(
  const dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := dependency.Handle = fTypeInfo;
  if not Result then
    Result := Kernel.DependencyResolver.CanResolve(dependency, argument);
end;

function TDependencyOverride.TResolver.Resolve(
  const dependency: TRttiType; const argument: TValue): TValue;
begin
  if dependency.Handle = fTypeInfo then
    Result := fValue
  else
    Result := Kernel.DependencyResolver.Resolve(dependency, argument);
end;

{$ENDREGION}


{$REGION 'TDependencyOverride<T>'}

constructor TDependencyOverride<T>.Create(const value: TValue);
begin
  inherited Create(System.TypeInfo(T), value);
end;

{$ENDREGION}


{$REGION 'TDependencyOverrides'}

constructor TDependencyOverrides.Create(
  const dependencyOverrides: array of TDependencyOverride);
begin
  inherited Create;
  fDependencyOverrides := TCollections.CreateObjectList<TDependencyOverride>;
  fDependencyOverrides.AddRange(dependencyOverrides);
end;

function TDependencyOverrides.GetResolver(
  const kernel: IKernel): IDependencyResolver;
begin
  Result := TResolver.Create(kernel, fDependencyOverrides);
end;

{$ENDREGION}


{$REGION 'TDependencyOverrides.TResolver'}

constructor TDependencyOverrides.TResolver.Create(const kernel: IKernel;
  const dependencyOverrides: IList<TDependencyOverride>);
begin
  inherited Create(kernel);
  fDependencyOverrides := dependencyOverrides;
end;

function TDependencyOverrides.TResolver.CanResolve(
  const dependency: TRttiType; const argument: TValue): Boolean;
var
  dependencyOverride: TDependencyOverride;
begin
  for dependencyOverride in fDependencyOverrides do
    if dependencyOverride.TypeInfo = dependency.Handle then
      Exit(True);

  Result := Kernel.DependencyResolver.CanResolve(dependency, argument);
end;

function TDependencyOverrides.TResolver.Resolve(
  const dependency: TRttiType; const argument: TValue): TValue;
var
  dependencyOverride: TDependencyOverride;
begin
  for dependencyOverride in fDependencyOverrides do
    if dependencyOverride.TypeInfo = dependency.Handle then
      Exit(dependencyOverride.Value);

  Result := Kernel.DependencyResolver.Resolve(dependency, argument);
end;

{$ENDREGION}


end.
