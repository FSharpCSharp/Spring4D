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

unit Spring.IoC.Resolvers;

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
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver, IInterface)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
    fDependencyTypes: TList<TRttiType>;
  protected
    procedure CheckCircularDependency(dependency: TRttiType);
    procedure ConstructValue(dependency: TRttiType; instance: TObject; out value: TValue);
    function GetEligibleModel(dependency: TRttiType; const argument: TValue): TComponentModel;
  public
    constructor Create(const context: IContainerContext; const registry: IComponentRegistry);
    destructor Destroy; override;

    function CanResolveDependency(dependency: TRttiType): Boolean; overload;
    function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; overload;
    function ResolveDependency(dependency: TRttiType): TValue; overload;
    function ResolveDependency(dependency: TRttiType; const argument: TValue): TValue; overload;

    function CanResolveDependencies(dependencies: TArray<TRttiType>): Boolean; overload;
    function CanResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean; overload;
    function ResolveDependencies(dependencies: TArray<TRttiType>): TArray<TValue>; overload;
    function ResolveDependencies(dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): TArray<TValue>; overload;

    function CanResolveDependencies(const injection: IInjection): Boolean; overload;
    function CanResolveDependencies(const injection: IInjection; const arguments: TArray<TValue>): Boolean; overload;
    function ResolveDependencies(const injection: IInjection): TArray<TValue>; overload;
    function ResolveDependencies(const injection: IInjection; const arguments: TArray<TValue>): TArray<TValue>; overload;
  end;

  TServiceResolver = class(TInterfacedObject, IServiceResolver, IInterface)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
  protected
    function DoResolve(model: TComponentModel; serviceType: PTypeInfo): TValue;
  public
    constructor Create(context: IContainerContext; const registry: IComponentRegistry);
    function CanResolve(serviceType: PTypeInfo): Boolean; overload;
    function CanResolve(const name: string): Boolean; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

implementation

uses
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.IoC.ComponentActivator,
  Spring.IoC.LifetimeManager,
  Spring.IoC.ResourceStrings;

{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry);
begin
  inherited Create;
  fContext := context;
  fRegistry := registry;
  fDependencyTypes := TList<TRttiType>.Create;
end;

destructor TDependencyResolver.Destroy;
begin
  fDependencyTypes.Free;
  inherited Destroy;
end;

function TDependencyResolver.GetEligibleModel(dependency: TRttiType;
  const argument: TValue): TComponentModel;
var
  models: IEnumerableEx<TComponentModel>;
  name: string;
begin
  if argument.IsEmpty then
  begin
    models := fRegistry.FindAll(dependency.Handle);
    if models.Count > 1 then
    begin
      raise EUnsatisfiedDependencyException.CreateResFmt(@SUnsatisfiedDependency,
        [dependency.Name]);
    end;
    Result := models.First;
  end
  else
  begin
    name := argument.AsString;
    Result := fRegistry.FindOne(name);
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

procedure TDependencyResolver.ConstructValue(dependency: TRttiType;
  instance: TObject; out value: TValue);
var
  localInterface: Pointer;
begin
  Assert(dependency <> nil, 'dependency should not be nil.');
  Assert(instance <> nil, 'instance should not be nil.');
  if dependency.IsClass then
  begin
    value := instance;
  end
  else if dependency.IsInterface then
  begin
    instance.GetInterface(GetTypeData(dependency.Handle).Guid, localInterface);
    TValue.MakeWithoutCopy(@localInterface, dependency.Handle, value);
  end
  else
  begin
    value := TValue.Empty;
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
      Result := fRegistry.FindAll(dependency.Handle).Count = 1;
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
        fRegistry.HasService(argument.AsString);
//        fRegistry.HasService(dependency.Handle, argument.AsString);
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
  instance: TObject;
begin
  TArgument.CheckNotNull(dependency, 'dependency');
  if not dependency.IsClassOrInterface then
  begin
    Exit(argument);
  end;
  CheckCircularDependency(dependency);
  model := GetEligibleModel(dependency, argument);
  fDependencyTypes.Add(dependency);
  try
    instance := model.LifetimeManager.GetInstance;
  finally
    fDependencyTypes.Remove(dependency);
  end;
  ConstructValue(dependency, instance, Result);
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
  const injection: IInjection): Boolean;
var
  arguments: TArray<TValue>;
begin
  TArgument.CheckNotNull(injection, 'injection');
  arguments := injection.Model.GetInjectionArguments(injection);
  Result := CanResolveDependencies(injection, arguments);
end;

function TDependencyResolver.CanResolveDependencies(const injection: IInjection;
  const arguments: TArray<TValue>): Boolean;
var
  dependencyTypes: TArray<TRttiType>;
begin
  TArgument.CheckNotNull(injection, 'injection');
  dependencyTypes := injection.GetDependencies;
  Result := CanResolveDependencies(dependencyTypes, arguments);
end;

function TDependencyResolver.ResolveDependencies(
  const injection: IInjection): TArray<TValue>;
var
  dependencyArguments: TArray<TValue>;
begin
  TArgument.CheckNotNull(injection, 'injection');
  dependencyArguments := injection.Model.GetInjectionArguments(injection);
  Result := ResolveDependencies(injection, dependencyArguments);
end;

function TDependencyResolver.ResolveDependencies(const injection: IInjection;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependencyTypes: TArray<TRttiType>;
begin
  TArgument.CheckNotNull(injection, 'injection');
  dependencyTypes := injection.GetDependencies;
  Result := ResolveDependencies(dependencyTypes, arguments);
end;

{$ENDREGION}


{$REGION 'TServiceResolver'}

constructor TServiceResolver.Create(context: IContainerContext;
  const registry: IComponentRegistry);
begin
  inherited Create;
  fContext := context;
  fRegistry := registry;
end;

function TServiceResolver.CanResolve(serviceType: PTypeInfo): Boolean;
begin
  Result := fRegistry.HasService(serviceType);
end;

function TServiceResolver.CanResolve(const name: string): Boolean;
begin
  Result := fRegistry.HasService(name);
end;

function TServiceResolver.DoResolve(model: TComponentModel;
  serviceType: PTypeInfo): TValue;
var
  instance: TObject;
  localInterface: Pointer;
begin
  Assert(model <> nil, 'model should not be nil.');
  Assert(serviceType <> nil, 'serviceType should not be nil.');
  if model.LifetimeManager = nil then
  begin
    raise EResolveException.CreateRes(@SLifetimeManagerMissing);
  end;
  instance := model.LifetimeManager.GetInstance;
  Assert(instance <> nil, 'instance should not be nil.');
  case serviceType.Kind of
    tkClass:
    begin
      Result := instance;
    end;
    tkInterface:
    begin
      instance.GetInterface(GetTypeData(serviceType).Guid, localInterface);
      TValue.MakeWithoutCopy(@localInterface, serviceType, Result);
    end;
  end;
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo): TValue;
var
  serviceName: string;
  models: IEnumerableEx<TComponentModel>;
  model: TComponentModel;
begin
  serviceName := GetTypeName(serviceType);
  models := fRegistry.FindAll(serviceType);
  if models.IsEmpty then
  begin
    raise EResolveException.CreateResFmt(@SNoComponentRegistered, [serviceName]);
  end;
  if models.Count > 1 then
  begin
    raise EUnsatisfiedDependencyException.CreateResFmt(@SUnsatisfiedDependency,
      [serviceName]);
  end;
  model := models.First;
  Result := DoResolve(model, serviceType);
end;

function TServiceResolver.Resolve(const name: string): TValue;
var
  model: TComponentModel;
  serviceType: PTypeInfo;
begin
  model := fRegistry.FindOne(name);
  if model = nil then
  begin
    raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
  end;
  serviceType := model.GetServiceType(name);
  Result := DoResolve(model, serviceType);
end;

function TServiceResolver.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
var
  models: IList<TComponentModel>;
  model: TComponentModel;
  i: Integer;
begin
  models := fRegistry.FindAll(serviceType).ToList;
  SetLength(Result, models.Count);
  for i := 0 to models.Count - 1 do
  begin
    model := models[i];
    Result[i] := DoResolve(model, serviceType);
  end;
end;

{$ENDREGION}

end.
