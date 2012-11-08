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

unit Spring.Container.Resolvers;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TResolver = class(TInterfacedObject, IResolver)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
    fOnResolve: IList<TOnResolveEvent>;
    procedure DoResolve(var instance: TValue);
    function GetOnResolve: IList<TOnResolveEvent>;
  protected
    procedure ConstructValue(typeInfo: PTypeInfo; const instance: TValue; out value: TValue);

    property Context: IContainerContext read fContext;
    property Registry: IComponentRegistry read fRegistry;
  public
    constructor Create(const context: IContainerContext; const registry: IComponentRegistry);
  end;

  TDependencyResolver = class(TResolver, IDependencyResolver, IInterface)
  private
    fDependencyTypes: IList<TRttiType>;
  protected
    procedure CheckCircularDependency(dependency: TRttiType);
    function GetEligibleModel(dependency: TRttiType; const argument: TValue): TComponentModel;
  public
    constructor Create(const context: IContainerContext; const registry: IComponentRegistry);

    function CanResolveDependency(dependency: TRttiType): Boolean; overload; virtual;
    function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; overload; virtual;
    function ResolveDependency(dependency: TRttiType): TValue; overload; virtual;
    function ResolveDependency(dependency: TRttiType; const argument: TValue): TValue; overload; virtual;

    function CanResolveDependencies(dependencies: TArray<TRttiType>): Boolean; overload; virtual;
    function CanResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean; overload; virtual;
    function ResolveDependencies(dependencies: TArray<TRttiType>): TArray<TValue>; overload; virtual;
    function ResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): TArray<TValue>; overload; virtual;

    function CanResolveDependencies(const Inject: IInjection): Boolean; overload; virtual;
    function CanResolveDependencies(const Inject: IInjection; const arguments: TArray<TValue>): Boolean; overload; virtual;
    function ResolveDependencies(const Inject: IInjection): TArray<TValue>; overload; virtual;
    function ResolveDependencies(const Inject: IInjection; const arguments: TArray<TValue>): TArray<TValue>; overload; virtual;
  end;

  TServiceResolver = class(TResolver, IServiceResolver, IInterface)
  protected
    function DoResolve(model: TComponentModel; serviceType: PTypeInfo;
      resolver: IDependencyResolver): TValue;
  public
    function CanResolve(serviceType: PTypeInfo): Boolean; overload;
    function CanResolve(const name: string): Boolean; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo; resolverOverride: IResolverOverride): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function Resolve(const name: string; resolverOverride: IResolverOverride): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  TResolverOverride = class(TInterfacedObject, IResolverOverride, IInterface)
  public
    function GetResolver(context: IContainerContext): IDependencyResolver; virtual; abstract;
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
        fInject: IInjection;
      public
        constructor Create(const context: IContainerContext; const registry: IComponentRegistry;
          const name: string; const value: TValue);

        function CanResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean; override;
        function ResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): TArray<TValue>; override;

        function CanResolveDependencies(const Inject: IInjection): Boolean; override;
        function ResolveDependencies(const Inject: IInjection): TArray<TValue>; override;
      end;
  public
    constructor Create(const name: string; const value: TValue);

    function GetResolver(context: IContainerContext): IDependencyResolver; override;
  end;

  TOrderedParametersOverride = class(TResolverOverride)
  private
    fArguments: TArray<TValue>;

    type
      TResolver = class(TDependencyResolver)
      private
        fArguments: TArray<TValue>;
      public
        constructor Create(const context: IContainerContext; const registry: IComponentRegistry;
          arguments: TArray<TValue>);

        function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; override;

        function CanResolveDependencies(const Inject: IInjection): Boolean; override;
        function ResolveDependencies(const Inject: IInjection): TArray<TValue>; override;
      end;
  public
    constructor Create(const arguments: array of TValue);

    function GetResolver(context: IContainerContext): IDependencyResolver; override;
  end;

implementation

uses
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.Container.ComponentActivator,
  Spring.Container.LifetimeManager,
  Spring.Container.ResourceStrings;

{$REGION 'TResolver'}

constructor TResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry);
begin
  inherited Create;
  fContext := context;
  fRegistry := registry;
  fOnResolve := TCollections.CreateList<TOnResolveEvent>;
end;

procedure TResolver.DoResolve(var instance: TValue);
var
  event: TOnResolveEvent;
begin
  for event in fOnResolve do
    event(Self, instance);
end;

function TResolver.GetOnResolve: IList<TOnResolveEvent>;
begin
  Result := fOnResolve;
end;

procedure TResolver.ConstructValue(typeInfo: PTypeInfo; const instance: TValue;
  out value: TValue);
var
  localInterface: Pointer;
begin
  Assert(not instance.IsEmpty, 'instance should not be empty.');
  case typeInfo.Kind of
    tkClass:
    begin
      value := instance;
    end;
    tkInterface:
    begin
      if instance.IsObject then
      begin
        instance.AsObject.GetInterface(GetTypeData(typeInfo).Guid, localInterface);
      end
      else
      begin
        instance.AsInterface.QueryInterface(GetTypeData(typeInfo).Guid, localInterface);
      end;
      TValue.MakeWithoutCopy(@localInterface, typeInfo, value);
    end;
  else
    value := TValue.Empty;
  end;

  DoResolve(value);
end;

{$ENDREGION}


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry);
begin
  inherited Create(context, registry);
  fDependencyTypes := TCollections.CreateList<TRttiType>;
end;

function TDependencyResolver.GetEligibleModel(dependency: TRttiType;
  const argument: TValue): TComponentModel;
var
  models: IEnumerable<TComponentModel>;
  name: string;
begin
  if argument.IsEmpty then
  begin
    models := Registry.FindAll(dependency.Handle);
    if models.Count > 1 then
    begin
      raise EUnsatisfiedDependencyException.CreateResFmt(@SUnsatisfiedDependency,
        [dependency.Name]);
    end;
    Result := models.FirstOrDefault;
  end
  else
  begin
    name := argument.AsString;
    Result := Registry.FindOne(name);
    if Result = nil then
    begin
      raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
    end;
    if not Result.HasService(dependency.Handle) then
    begin
      raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
    end;
  end;
end;

procedure TDependencyResolver.CheckCircularDependency(dependency: TRttiType);
begin
  Assert(dependency <> nil, 'dependency should not be nil.');
  if fDependencyTypes.Contains(dependency) then
  begin
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected,
      [dependency.Name]
    );
  end;
end;

function TDependencyResolver.CanResolveDependency(
  dependency: TRttiType): Boolean;
begin
  Result := CanResolveDependency(dependency, TValue.Empty);
end;

function TDependencyResolver.CanResolveDependency(dependency: TRttiType;
  const argument: TValue): Boolean;
//var
//  predicate: TPredicate<TComponentModel>;
begin
  if dependency.IsClassOrInterface then
  begin
    if argument.IsEmpty then
    begin
      Result := Registry.FindAll(dependency.Handle).Count = 1;
    end
    else
    begin
//      predicate := 
//        function (c: TComponentModel): Boolean
//        begin
//          Result := c.HasService(dependency.Handle);
//        end;
//      Result := argument.IsType<string> and
//        fRegistry.FindAll(dependency.Handle).Any(predicate);
      Result := argument.IsType<string> and
        Registry.HasService(argument.AsString);
//        Registry.HasService(dependency.Handle, argument.AsString);
    end;
  end
  else
  begin
    Result := argument.IsType(dependency.Handle);
  end;
end;

function TDependencyResolver.ResolveDependency(dependency: TRttiType): TValue;
begin
  Result := ResolveDependency(dependency, TValue.Empty);
end;

function TDependencyResolver.ResolveDependency(dependency: TRttiType;
  const argument: TValue): TValue;
var
  model: TComponentModel;
  instance: TValue;
begin
  TArgument.CheckNotNull(dependency, 'dependency');
  if not dependency.IsClassOrInterface
    or (argument.Kind in [tkClass, tkInterface]) then
  begin
    Exit(argument);
  end;
  CheckCircularDependency(dependency);
  model := GetEligibleModel(dependency, argument);
  if model = nil then Exit(argument);
  fDependencyTypes.Add(dependency);
  try
    instance := model.LifetimeManager.GetInstance;
  finally
    fDependencyTypes.Remove(dependency);
  end;
  ConstructValue(dependency.Handle, instance, Result);
end;

function TDependencyResolver.CanResolveDependencies(
  dependencies: TArray<TRttiType>): Boolean;
begin
  Result := CanResolveDependencies(dependencies, nil);
end;

function TDependencyResolver.CanResolveDependencies(
  dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean;
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
      if not CanResolveDependency(dependency, arguments[i]) then
      begin
        Exit(False);
      end;
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
    begin
      if not CanResolveDependency(dependency, TValue.Empty) then
      begin
        Exit(False);
      end;
    end;
  end
  else
  begin
    Exit(False);
  end;
end;

function TDependencyResolver.ResolveDependencies(
  dependencies: TArray<TRttiType>): TArray<TValue>;
begin
  Result := ResolveDependencies(dependencies, nil);
end;

function TDependencyResolver.ResolveDependencies(
  dependencies: TArray<TRttiType>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependency: TRttiType;
  hasArgument: Boolean;
  i: Integer;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
  begin
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  end;
  SetLength(Result, Length(dependencies));
  if hasArgument then
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := ResolveDependency(dependency, arguments[i]);
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := ResolveDependency(dependency, TValue.Empty);
    end;
  end;
end;

function TDependencyResolver.CanResolveDependencies(
  const Inject: IInjection): Boolean;
var
  arguments: TArray<TValue>;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  arguments := Inject.Model.GetInjectionArguments(Inject);
  Result := CanResolveDependencies(Inject, arguments);
end;

function TDependencyResolver.CanResolveDependencies(const Inject: IInjection;
  const arguments: TArray<TValue>): Boolean;
var
  dependencyTypes: TArray<TRttiType>;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  dependencyTypes := Inject.GetDependencies;
  Result := CanResolveDependencies(dependencyTypes, arguments);
end;

function TDependencyResolver.ResolveDependencies(
  const Inject: IInjection): TArray<TValue>;
var
  dependencyArguments: TArray<TValue>;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  dependencyArguments := Inject.Model.GetInjectionArguments(Inject);
  Result := ResolveDependencies(Inject, dependencyArguments);
end;

function TDependencyResolver.ResolveDependencies(const Inject: IInjection;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependencyTypes: TArray<TRttiType>;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  dependencyTypes := Inject.GetDependencies;
  Result := ResolveDependencies(dependencyTypes, arguments);
end;

{$ENDREGION}


{$REGION 'TServiceResolver'}

function TServiceResolver.CanResolve(serviceType: PTypeInfo): Boolean;
begin
  Result := Registry.HasService(serviceType);
end;

function TServiceResolver.CanResolve(const name: string): Boolean;
begin
  Result := Registry.HasService(name);
end;

function TServiceResolver.DoResolve(model: TComponentModel;
  serviceType: PTypeInfo; resolver: IDependencyResolver): TValue;
var
  instance: TValue;
begin
  Assert(model <> nil, 'model should not be nil.');
  Assert(serviceType <> nil, 'serviceType should not be nil.');
  if model.LifetimeManager = nil then
  begin
    raise EResolveException.CreateRes(@SLifetimeManagerMissing);
  end;
  instance := model.LifetimeManager.GetInstance(resolver);
  ConstructValue(serviceType, instance, Result);
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo): TValue;
begin
  Result := Resolve(serviceType, nil);
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo;
  resolverOverride: IResolverOverride): TValue;
var
  serviceName: string;
  models: IEnumerable<TComponentModel>;
  model: TComponentModel;
  resolver: IDependencyResolver;
begin
  serviceName := GetTypeName(serviceType);
  models := Registry.FindAll(serviceType);
  if models.IsEmpty then
  begin
    raise EResolveException.CreateResFmt(@SNoComponentRegistered, [serviceName]);
  end;
  if models.Count > 1 then
  begin
    models := models.Where(
      function(const model: TComponentModel): Boolean
      begin
        Result := model.DefaultServices.Contains(serviceType);
      end);
    if models.Count <> 1 then
    begin
      raise EUnsatisfiedDependencyException.CreateResFmt(@SUnsatisfiedDependency,
        [serviceName]);
    end;
  end;
  model := models.First;
  if Assigned(resolverOverride) then
    resolver := resolverOverride.GetResolver(Context)
  else
    resolver := nil;
  Result := DoResolve(model, serviceType, resolver);
end;

function TServiceResolver.Resolve(const name: string): TValue;
begin
  Result := Resolve(name, nil);
end;

function TServiceResolver.Resolve(const name: string;
  resolverOverride: IResolverOverride): TValue;
var
  model: TComponentModel;
  serviceType: PTypeInfo;
  resolver: IDependencyResolver;
begin
  model := Registry.FindOne(name);
  if model = nil then
  begin
    raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
  end;
  serviceType := model.GetServiceType(name);
  if Assigned(resolverOverride) then
    resolver := resolverOverride.GetResolver(Context)
  else
    resolver := nil;
  Result := DoResolve(model, serviceType, resolver);
end;

function TServiceResolver.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
var
  models: IList<TComponentModel>;
  model: TComponentModel;
  i: Integer;
begin
  models := Registry.FindAll(serviceType).ToList;
  SetLength(Result, models.Count);
  for i := 0 to models.Count - 1 do
  begin
    model := models[i];
    Result[i] := DoResolve(model, serviceType, nil);
  end;
end;

{$ENDREGION}


{$REGION 'TOrderedParametersOverride'}

constructor TOrderedParametersOverride.Create(const arguments: array of TValue);
var
  i: Integer;
begin
  SetLength(fArguments, Length(arguments));
  for i := Low(arguments) to High(arguments) do
    fArguments[i] := arguments[i];
end;

function TOrderedParametersOverride.GetResolver(context: IContainerContext): IDependencyResolver;
begin
  Result := TResolver.Create(context, context.ComponentRegistry, fArguments);
end;

{$ENDREGION}


{$REGION 'TOrderedParametersOverride.TResolver'}

constructor TOrderedParametersOverride.TResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry; arguments: TArray<TValue>);
begin
  inherited Create(context, registry);
  fArguments := arguments;
end;

function TOrderedParametersOverride.TResolver.CanResolveDependency(
  dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := argument.IsType(dependency.Handle);
end;

function TOrderedParametersOverride.TResolver.CanResolveDependencies(
  const Inject: IInjection): Boolean;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  Result := CanResolveDependencies(Inject, fArguments);
end;

function TOrderedParametersOverride.TResolver.ResolveDependencies(
  const Inject: IInjection): TArray<TValue>;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  Result := fArguments;
end;

{$ENDREGION}


{$REGION 'TParameterOverride'}

constructor TParameterOverride.Create(const name: string; const value: TValue);
begin
  fName := name;
  fValue := value;
end;

function TParameterOverride.GetResolver(
  context: IContainerContext): IDependencyResolver;
begin
  Result := TResolver.Create(context, context.ComponentRegistry, fName, fValue);
end;

{$ENDREGION}


{$REGION 'TParameterOverride.TResolver'}

constructor TParameterOverride.TResolver.Create(
  const context: IContainerContext; const registry: IComponentRegistry;
  const name: string; const value: TValue);
begin
  inherited Create(context, registry);
  fName := name;
  fValue := value;
end;

function TParameterOverride.TResolver.CanResolveDependencies(
  dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean;
var
  dependency: TRttiType;
  i: Integer;
  parameters: TArray<TRttiParameter>;
begin
  Result := True;
  if Length(dependencies) = Length(arguments) then
  begin
    parameters := fInject.Target.AsMethod.GetParameters;
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      if SameText(parameters[i].Name, fName) then
        Continue;
      if not CanResolveDependency(dependency, arguments[i]) then
      begin
        Exit(False);
      end;
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
    begin
      if not CanResolveDependency(dependency, TValue.Empty) then
      begin
        Exit(False);
      end;
    end;
  end
  else
  begin
    Exit(False);
  end;
end;

function TParameterOverride.TResolver.CanResolveDependencies(
  const Inject: IInjection): Boolean;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  fInject := Inject;
  Result := inherited;
end;

function TParameterOverride.TResolver.ResolveDependencies(
  dependencies: TArray<TRttiType>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependency: TRttiType;
  hasArgument: Boolean;
  i: Integer;
  parameters: TArray<TRttiParameter>;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
  begin
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  end;
  SetLength(Result, Length(dependencies));
  if hasArgument then
  begin
    parameters := fInject.Target.AsMethod.GetParameters;
    for i := 0 to High(dependencies) do
    begin
      if SameText(parameters[i].Name, fName) then
        Result[i] := fValue
      else
      begin
        dependency := dependencies[i];
        Result[i] := ResolveDependency(dependency, arguments[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := ResolveDependency(dependency, TValue.Empty);
    end;
  end;
end;

function TParameterOverride.TResolver.ResolveDependencies(
  const Inject: IInjection): TArray<TValue>;
begin
  TArgument.CheckNotNull(Inject, 'Inject');
  fInject := Inject;
  Result := inherited;
end;

{$ENDREGION}

end.
