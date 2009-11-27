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
    function CanResolveDependency(dependency: TRttiType; const arguments: TArray<TValue>): Boolean;
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
    function CanResolve(typeInfo: PTypeInfo): Boolean;
    function Resolve(typeInfo: PTypeInfo; const name: string): TValue;
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
  const arguments: TArray<TValue>): Boolean;
begin
  Result := dependency.IsClassOrInterface and
    fRegistry.HasServiceType(dependency.Handle);
end;

function TDependencyResolver.CanResolve(
  const injection: IInjection): Boolean;
var
  dependency: TRttiType;
  injectionInfo: TInjectionInfo;
//  injectionArguments: TArray<TValue>;
begin
  TArgument.CheckNotNull(injection, 'injection');
  Result := True;
  injection.Model.InjectionArguments.TryGetValue(injection, injectionInfo);
  for dependency in injection.GetDependencies do
  begin
    if not CanResolveDependency(dependency, injectionInfo.Arguments) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TDependencyResolver.ResolveDependencies(
  const injection: IInjection): TArray<TValue>;
var
  dependency: TRttiType;
  model: TComponentModel;
  injectionInfo: TInjectionInfo;
//  arguments: TArray<TValue>;
  instance: TObject;
  localInterface: Pointer;
  index: Integer;
  name: string;
begin
  TArgument.CheckNotNull(injection, 'injection');
  SetLength(Result, injection.DependencyCount);
  injection.Model.InjectionArguments.TryGetValue(injection, injectionInfo);
  if (Length(injectionInfo.Arguments) > 0) and (Length(injectionInfo.Arguments) <> injection.DependencyCount) then
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
    if (Length(injectionInfo.Arguments) > 0) then // Use arguments
    begin
      if dependency.IsClassOrInterface then
      begin
        name := injectionInfo.Arguments[index].AsString;
      end
      else
      begin
        Result[index] := injectionInfo.Arguments[index];
        Inc(index);
        Continue;
      end;
    end;
    model := fRegistry.FindOneByServiceType(dependency.Handle, name);
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
      if Length(injectionInfo.Arguments) = 0 then
      begin
        raise EResolveException.Create('No arguments');
      end;
      Result[index] := injectionInfo.Arguments[index];
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

function TServiceResolver.CanResolve(typeInfo: PTypeInfo): Boolean;
begin
  // TODO: CanResolve
  Result := True;
end;

function TServiceResolver.Resolve(typeInfo: PTypeInfo;
  const name: string): TValue;
var
  models: TArray<TComponentModel>;
  model: TComponentModel;
  item: TComponentModel;
  instance: TObject;
  localInterface: Pointer;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');

  models := fRegistry.FindAll(typeInfo);
  if Length(models) = 0 then
  begin
    raise EResolveException.CreateResFmt(@SNoComponentRegistered, [GetTypeName(typeInfo)]);
  end;
  if Length(models) = 1 then
  begin
    model := models[0];
  end
  else
  begin
    model := nil;
    for item in models do
    begin
      if SameText(item.Name, name) then
      begin
        model := item;
      end;
    end;
    if model = nil then
    begin
      raise EUnsatisfiedDependencyException.CreateResFmt(@SUnsatisfiedDependency,
        [GetTypeName(typeInfo), name]);
    end;
  end;
  if model.LifetimeManager = nil then
  begin
    raise EResolveException.CreateRes(@SLifetimeManagerNeeded);
  end;
  instance := model.LifetimeManager.GetInstance;
  case typeInfo.Kind of
    tkClass:
    begin
      Result := instance;
    end;
    tkInterface:
    begin
      instance.GetInterface(GetTypeData(typeInfo).Guid, localInterface);
      TValue.MakeWithoutCopy(@localInterface, typeInfo, Result);
    end;
  end;
end;

end.
