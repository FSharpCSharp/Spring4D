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
    fRegistry: IComponentRegistry;
    fDependencies: TList<PTypeInfo>;
    function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean;
  public
    constructor Create(const registry: IComponentRegistry);
    destructor Destroy; override;
    function CanResolve(const injection: IInjection): Boolean; overload;
    function ResolveDependencies(const injection: IInjection): TArray<TValue>; overload;
  end;

  TServiceResolver = class(TInterfacedObject, IServiceResolver, IInterface)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
  public
    constructor Create(context: IContainerContext; const registry: IComponentRegistry);
    function CanResolve(serviceType: PTypeInfo): Boolean;
    function Resolve(serviceType: PTypeInfo; const name: string): TValue;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

implementation

uses
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.IoC.ComponentActivator,
  Spring.IoC.LifetimeManager,
  Spring.IoC.ResourceStrings;

{ TDependencyResolver }

constructor TDependencyResolver.Create(const registry: IComponentRegistry);
begin
  TArgument.CheckNotNull(registry, 'registry');
  inherited Create;
  fRegistry := registry;
  fDependencies := TList<PTypeInfo>.Create;
end;

destructor TDependencyResolver.Destroy;
begin
  fDependencies.Free;
  inherited Destroy;
end;

function TDependencyResolver.CanResolveDependency(dependency: TRttiType;
  const argument: TValue): Boolean;
begin
  if dependency.IsClassOrInterface then
  begin
    if argument.IsEmpty then
    begin
      Result := fRegistry.HasService(dependency.Handle);
    end
    else
    begin
      Result := argument.IsType<string> and
        fRegistry.HasService(dependency.Handle, argument.AsString);
    end;
  end
  else
  begin
    Result := argument.IsType(dependency.Handle);
  end;
end;

function TDependencyResolver.CanResolve(
  const injection: IInjection): Boolean;
var
  dependencies: TArray<TRttiType>;
  dependency: TRttiType;
  arguments: TArray<TValue>;
  i: Integer;
begin
  TArgument.CheckNotNull(injection, 'injection');
  dependencies := injection.GetDependencies;
  arguments := injection.Model.GetInjectionArguments(injection);
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
  const injection: IInjection): TArray<TValue>;
var
  dependency: TRttiType;
  model: TComponentModel;
  arguments: TArray<TValue>;
  instance: TObject;
  localInterface: Pointer;
  index: Integer;
  name: string;
begin
  TArgument.CheckNotNull(injection, 'injection');
  SetLength(Result, injection.DependencyCount);
  arguments := injection.Model.GetInjectionArguments(injection);
  if (Length(arguments) > 0) and (Length(arguments) <> injection.DependencyCount) then
  begin
    raise EResolveException.Create('Unsatisified arguments.');
  end;
  index := 0;
  for dependency in injection.GetDependencies do
  begin
    if fDependencies.Contains(dependency.Handle) then
    begin
      raise ECircularDependencyException.CreateResFmt(
        @SCircularDependencyDetected,
        [dependency.Name]
      );
    end;
    if (Length(arguments) > 0) then // Use arguments
    begin
      if dependency.IsClassOrInterface then
      begin
        name := arguments[index].AsString;
      end
      else
      begin
        Result[index] := arguments[index];
        Inc(index);
        Continue;
      end;
    end;
    model := fRegistry.FindOne(dependency.Handle, name);
    if model = nil then
    begin
      raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
    end;
    fDependencies.Add(dependency.Handle);
    try
      instance := model.LifetimeManager.GetInstance;
    finally
      fDependencies.Remove(dependency.Handle);
    end;
    if dependency.IsClass then
    begin
      Result[index] := instance;
    end
    else if dependency.IsInterface then
    begin
      instance.GetInterface(GetTypeData(dependency.Handle).Guid, localInterface);
      TValue.MakeWithoutCopy(@localInterface, dependency.Handle, Result[index]);
    end
    else
    begin
      if Length(arguments) = 0 then
      begin
        raise EResolveException.Create('No arguments');
      end;
      Result[index] := arguments[index];
    end;
    Assert(not Result[index].IsEmpty);
    Inc(index);
  end;
end;

{ TServiceResolver }

constructor TServiceResolver.Create(context: IContainerContext;
  const registry: IComponentRegistry);
begin
  TArgument.CheckNotNull(context, 'context');
  TArgument.CheckNotNull(registry, 'registry');
  inherited Create;
  fContext := context;
  fRegistry := registry;
end;

function TServiceResolver.CanResolve(serviceType: PTypeInfo): Boolean;
begin
  // TODO: CanResolve
  Result := True;
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo;
  const name: string): TValue;
var
  models: IEnumerableEx<TComponentModel>;
  model: TComponentModel;
  instance: TObject;
  localInterface: Pointer;
begin
  TArgument.CheckTypeKind(serviceType, [tkClass, tkInterface], 'serviceType');

  models := fRegistry.FindAll(serviceType);
  if models.IsEmpty then
  begin
    raise EResolveException.CreateResFmt(@SNoComponentRegistered, [GetTypeName(serviceType)]);
  end;
  if (models.Count = 1) and (name = '') then
  begin
    model := models.First;
  end
  else
  begin
    model := models.Where(
      function(item: TComponentModel): Boolean
      begin
        Result := item.Services.ContainsKey(name);
      end
    ).FirstOrDefault;
    if model = nil then
    begin
      raise EUnsatisfiedDependencyException.CreateResFmt(@SUnsatisfiedDependency,
        [GetTypeName(serviceType), name]);
    end;
  end;
  if model.LifetimeManager = nil then
  begin
    raise EResolveException.CreateRes(@SLifetimeManagerNeeded);
  end;
  instance := model.LifetimeManager.GetInstance;
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

function TServiceResolver.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
var
  models: IList<TComponentModel>;
  model: TComponentModel;
  i: Integer;
begin
  TArgument.CheckTypeKind(serviceType, [tkClass, tkInterface], 'serviceType');
  models := fRegistry.FindAll(serviceType).ToList;
  SetLength(Result, models.Count);
  for i := 0 to models.Count - 1 do
  begin
    model := models[i];
    Result[i] := Resolve(serviceType, model.GetServiceName(serviceType));
  end;
end;

end.
