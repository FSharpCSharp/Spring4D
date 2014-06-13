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
  Spring.Container.Core,
  Spring.Reflection;

type
  TSubDependencyResolverBase = class abstract(TInterfacedObject, ISubDependencyResolver)
  private
    fKernel: IKernel;
  protected
    property Kernel: IKernel read fKernel;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; virtual; abstract;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; virtual; abstract;
  end;

  TDependencyResolver = class(TSubDependencyResolverBase, IDependencyResolver)
  private
    fSubResolvers: IList<ISubDependencyResolver>;
  protected
    function CanResolveFromContext(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function CanResolveFromSubResolvers(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function InternalResolveValue(typeInfo: PTypeInfo;
      const instance: TValue): TValue;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; overload; override;
    function CanResolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): Boolean; reintroduce; overload; virtual;

    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; overload; override;
    function Resolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): TArray<TValue>; reintroduce; overload; virtual;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  TLazyResolver = class(TSubDependencyResolverBase)
  private
    function InternalResolveClass(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue;
      lazyKind: TLazyKind): TValue;
    function InternalResolveInterface(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue;
      lazyKind: TLazyKind): TValue;
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue; override;
  end;

  TDynamicArrayResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue; override;
  end;

  TListResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue; override;
  end;

  TPrimitivesResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Container.CreationContext,
  Spring.Container.ResourceStrings,
  Spring.Helpers;


{$REGION 'TSubDependencyResolverBase'}

constructor TSubDependencyResolverBase.Create(const kernel: IKernel);
begin
{$IFNDEF DISABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
end;

{$ENDREGION}


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const kernel: IKernel);
begin
  inherited Create(kernel);
  fSubResolvers := TCollections.CreateInterfaceList<ISubDependencyResolver>;
end;

procedure TDependencyResolver.AddSubResolver(
  const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Add(subResolver);
end;

procedure TDependencyResolver.RemoveSubResolver(
  const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Remove(subResolver);
end;

function TDependencyResolver.InternalResolveValue(
  typeInfo: PTypeInfo; const instance: TValue): TValue;
var
  intf: Pointer;
begin
  Guard.CheckNotNull(typeInfo, 'typeInfo');
  Guard.CheckNotNull(not instance.IsEmpty, 'instance');

  if typeInfo.Kind = tkInterface then
  begin
    if instance.IsObject then
      instance.AsObject.GetInterface(GetTypeData(typeInfo).Guid, intf)
    else
    begin
      if TType.IsDelegate(typeInfo) then
      begin
        intf := nil;
        IInterface(intf) := instance.AsInterface;
      end
      else
        instance.AsInterface.QueryInterface(GetTypeData(typeInfo).Guid, intf);
    end;
    TValue.MakeWithoutCopy(@intf, typeInfo, Result);
  end
  else
    Result := instance;
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  kind: TTypeKind;
  serviceName: string;
  serviceType: PTypeInfo;
  componentModel: TComponentModel;
begin
  if CanResolveFromContext(context, dependency, argument) then
    Exit(True);

  if CanResolveFromSubResolvers(context, dependency, argument) then
    Exit(True);

  if argument.IsEmpty then
    Result := Kernel.Registry.HasDefault(dependency.TargetTypeInfo)
  else if argument.TryAsType<TTypeKind>(Kind) and (kind = tkDynArray) then
    Result := Kernel.Registry.HasService(dependency.TargetTypeInfo)
  else
  begin
    Result := argument.IsString;
    if Result then
    begin
      serviceName := argument.AsString;
      componentModel := Kernel.Registry.FindOne(serviceName);
      Result := Assigned(componentModel);
      if Result then
      begin
        serviceType := componentModel.Services[serviceName];
        Result := TType.IsAssignable(dependency.TargetTypeInfo, serviceType);
      end;
    end;
  end;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  i: Integer;
  componentModel: TComponentModel;
  instance: TValue;
begin
  Guard.CheckNotNull(dependency, 'dependency');

  if CanResolveFromContext(context, dependency, argument) then
    Exit(context.Resolve(context, dependency, argument));

  for i := fSubResolvers.Count - 1 downto 0 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(fSubResolvers[i].Resolve(context, dependency, argument));

  componentModel := Kernel.Registry.FindOne(dependency.TargetTypeInfo, argument);
  if not Assigned(componentModel) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.TargetTypeName]);
  if context.IsInResolution(componentModel) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [componentModel.ComponentTypeName]);

  context.EnterResolution(componentModel);
  try
    instance := componentModel.LifetimeManager.Resolve(context);
  finally
    context.LeaveResolution(componentModel);
  end;

  Result := InternalResolveValue(dependency.TargetTypeInfo, instance);
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependencies: TArray<TDependencyModel>;
  const arguments: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  if Length(dependencies) = Length(arguments) then
  begin
    for i := 0 to High(dependencies) do
    begin
      if not arguments[i].IsEmpty
        and arguments[i].IsType(dependencies[i].TargetTypeInfo) then
        Continue;
      if not CanResolve(context, dependencies[i], arguments[i]) then
        Exit(False);
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for i := 0 to High(dependencies) do
      if not CanResolve(context, dependencies[i], nil) then
        Exit(False);
  end
  else
    Exit(False);
  Result := True;
end;

function TDependencyResolver.CanResolveFromContext(
  const context: ICreationContext; const dependency: TDependencyModel;
  const argument: TValue): Boolean;
begin
  Result := Assigned(context) and context.CanResolve(context, dependency, argument);
end;

function TDependencyResolver.CanResolveFromSubResolvers(
  const context: ICreationContext; const dependency: TDependencyModel;
  const argument: TValue): Boolean;
var
  i: Integer;
begin
  for i := fSubResolvers.Count - 1 downto 0 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(True);
  Result := False;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependencies: TArray<TDependencyModel>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
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
      if not arguments[i].IsEmpty
        and arguments[i].IsType(dependencies[i].TargetTypeInfo) then
      begin
        Result[i] := arguments[i];
        Continue;
      end;
      Result[i] := Resolve(context, dependencies[i], arguments[i]);
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
      Result[i] := Resolve(context, dependencies[i], nil);
  end;
end;

{$ENDREGION}


{$REGION 'TLazyResolver'}

function TLazyResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
begin
  Result := TType.IsLazy(dependency.TargetTypeInfo) and Kernel.Resolver.CanResolve(
    context, TDependencyModel.Create(
    dependency.TargetType.GetGenericArguments[0], dependency.Target), argument);
end;

function TLazyResolver.InternalResolveClass(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue; lazyKind: TLazyKind): TValue;
var
  model: TDependencyModel;
  value: TValue;
  factory: TFunc<TObject>;
begin
  model := dependency;
  value := argument;
  factory :=
    function: TObject
    begin
      Result := Kernel.Resolver.Resolve(context, model, value).AsObject;
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<TObject>>(factory);
    lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(factory));
  end;
end;

function TLazyResolver.InternalResolveInterface(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue; lazyKind: TLazyKind): TValue;
var
  model: TDependencyModel;
  value: TValue;
  factory: TFunc<IInterface>;
begin
  model := dependency;
  value := argument;
  factory :=
    function: IInterface
    begin
      Result := Kernel.Resolver.Resolve(context, model, value).AsInterface;
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<IInterface>>(factory);
    lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(factory));
  end;
end;

function TLazyResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  dependencyType: TRttiType;
  componentModel: TComponentModel;
  dependencyModel: TDependencyModel;
begin
  if not TType.IsLazy(dependency.TargetTypeInfo) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.TargetTypeName]);

  lazyKind := TType.GetLazyKind(dependency.TargetTypeInfo);
  dependencyType := dependency.TargetType.GetGenericArguments[0];

  componentModel := Kernel.Registry.FindOne(dependencyType.Handle, argument);
  if not Assigned(componentModel) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.TargetTypeName]);
  if context.IsInResolution(componentModel) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [componentModel.ComponentTypeName]);

  dependencyModel := TDependencyModel.Create(dependencyType, dependency.Target);
  case dependencyType.TypeKind of
    tkClass: Result := InternalResolveClass(context, dependencyModel, argument, lazyKind);
    tkInterface: Result := InternalResolveInterface(context, dependencyModel, argument, lazyKind);
  else
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.TargetTypeName]);
  end;

  TValueData(Result).FTypeInfo := dependency.TargetTypeInfo;
end;

{$ENDREGION}


{$REGION 'TDynamicArrayResolver'}

function TDynamicArrayResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
begin
  Result := dependency.TargetType.IsDynamicArray and Kernel.Resolver.CanResolve(
    context, TDependencyModel.Create(dependency.TargetType.AsDynamicArray.ElementType,
    dependency.Target), TValue.From(tkDynArray));
end;

function TDynamicArrayResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
  serviceType: PTypeInfo;
  models: TArray<TComponentModel>;
  values: TArray<TValue>;
  i: Integer;
  serviceName: string;
begin
  if not dependency.TargetType.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.TargetTypeName]);
  targetType := dependency.TargetType.AsDynamicArray.ElementType;
  dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
  // TODO: remove dependency on lazy type
  if TType.IsLazy(targetType.Handle) then
    serviceType := targetType.GetGenericArguments[0].Handle
  else
    serviceType := targetType.Handle;
  models := Kernel.Registry.FindAll(serviceType).ToArray;
  SetLength(values, Length(models));
  for i := 0 to High(models) do
  begin
    serviceName := models[i].GetServiceName(serviceType);
    values[i] := Kernel.Resolver.Resolve(context, dependencyModel, serviceName);
  end;
  Result := TValue.FromArray(dependency.TargetTypeInfo, values);
end;

{$ENDREGION}


{$REGION 'TListResolver'}

function TListResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
const
  SupportedTypes: array[0..3] of string = (
    'IList<>', 'IReadOnlyList<>', 'ICollection<>', 'IEnumerable<>');
var
  itemType: TRttiType;
begin
  Result := dependency.TargetType.IsGenericType
    and MatchText(dependency.TargetType.GetGenericTypeDefinition, SupportedTypes);
  if Result then
  begin
    itemType := dependency.TargetType.GetGenericArguments[0];
    Result := itemType.IsClassOrInterface
      and Kernel.Resolver.CanResolve(context, TDependencyModel.Create(
      itemType, dependency.Target), TValue.From(tkDynArray));
  end;
end;

function TListResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  itemType: TRttiType;
  arrayType: TRttiType;
  values: TValue;
begin
  itemType := dependency.TargetType.GetGenericArguments[0];
  arrayType := dependency.TargetType.GetMethod('ToArray').ReturnType;
  values := (Kernel as IKernelInternal).Resolve(arrayType.Handle);
  case itemType.TypeKind of
    tkClass:
    begin
      TValueData(values).FTypeInfo := TypeInfo(TArray<TObject>);
      Result := TValue.From(TList<TObject>.Create(values.AsType<TArray<TObject>>())).Cast(dependency.TargetTypeInfo);
    end;
    tkInterface:
    begin
      TValueData(values).FTypeInfo := TypeInfo(TArray<IInterface>);
      Result := TValue.From(TList<IInterface>.Create(values.AsType<TArray<IInterface>>())).Cast(dependency.TargetTypeInfo);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TPrimitivesResolver'}

function TPrimitivesResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
begin
  Result := not Kernel.HasService(dependency.TargetTypeInfo)
    and (not (dependency.TargetType.TypeKind in [tkClass, tkInterface, tkRecord])
    or (argument.Kind in [tkClass, tkInterface, tkRecord]));
end;

function TPrimitivesResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
begin
  Result := argument;
{$IFDEF DELPHI2010}
  if Result.IsEmpty then
    TValue.Make(nil, dependency.TargetTypeInfo, Result);
{$ENDIF}
end;

{$ENDREGION}


end.
