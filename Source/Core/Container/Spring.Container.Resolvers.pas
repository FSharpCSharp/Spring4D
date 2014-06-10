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
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver)
  private
    fKernel: IKernel;
    fSubResolvers: IList<ISubDependencyResolver>;
  protected
    function CanResolveFromContext(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean;
    function CanResolveFromSubResolvers(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean;
    function InternalResolveValue(typeInfo: PTypeInfo;
      const instance: TValue): TValue;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType;
      const argument: TValue): Boolean; overload; virtual;
    function CanResolve(const context: ICreationContext;
      const dependencies: TArray<TRttiType>;
      const arguments: TArray<TValue>): Boolean; overload; virtual;

    function Resolve(const context: ICreationContext;
      const dependency: TRttiType;
      const argument: TValue): TValue; overload; virtual;
    function Resolve(const context: ICreationContext;
      const dependencies: TArray<TRttiType>;
      const arguments: TArray<TValue>): TArray<TValue>; overload; virtual;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  TSubDependencyResolver = class abstract(TInterfacedObject, ISubDependencyResolver)
  strict private
    fKernel: IKernel;
  protected
    property Kernel: IKernel read fKernel;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean; virtual; abstract;
    function Resolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): TValue; virtual; abstract;
  end;

  TLazyResolver = class(TSubDependencyResolver)
  private
    function InternalResolveClass(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue;
      lazyKind: TLazyKind): TValue;
    function InternalResolveInterface(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue;
      lazyKind: TLazyKind): TValue;
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): TValue; override;
  end;

  TDynamicArrayResolver = class(TSubDependencyResolver)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): TValue; override;
  end;

  TListResolver = class(TSubDependencyResolver)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): TValue; override;
  end;

  TPrimitivesResolver = class(TSubDependencyResolver)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TRttiType; const argument: TValue): TValue; override;
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


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const kernel: IKernel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  inherited Create;
  fKernel := kernel;
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
  const dependency: TRttiType; const argument: TValue): Boolean;
var
  kind: TTypeKind;
  serviceName: string;
  serviceType: PTypeInfo;
  model: TComponentModel;
begin
  if CanResolveFromContext(context, dependency, argument) then
    Exit(True);

  if CanResolveFromSubResolvers(context, dependency, argument) then
    Exit(True);

  if dependency.TypeKind in [tkClass, tkInterface, tkRecord] then
  begin
    if argument.IsEmpty then
      Result := fKernel.Registry.HasDefault(dependency.Handle)
    else if argument.TryAsType<TTypeKind>(Kind) and (kind = tkDynArray) then
      Result := fKernel.Registry.HasService(dependency.Handle)
    else
    begin
      Result := argument.IsType<string>;
      if Result then
      begin
        serviceName := argument.AsString;
        model := fKernel.Registry.FindOne(serviceName);
        Result := Assigned(model);
        if Result then
        begin
          serviceType := model.Services[serviceName];
          Result := serviceType = dependency.Handle;
        end;
      end;
    end;
  end
  else
    Result := argument.IsEmpty or argument.IsType(dependency.Handle);
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): TValue;
var
  subResolver: ISubDependencyResolver;
  model: TComponentModel;
  instance: TValue;
begin
  Guard.CheckNotNull(dependency, 'dependency');

  if CanResolveFromContext(context, dependency, argument) then
    Exit(context.Resolve(context, dependency, argument));

  for subResolver in fSubResolvers do
    if subResolver.CanResolve(context, dependency, argument) then
      Exit(subResolver.Resolve(context, dependency, argument));

  model := fKernel.Registry.FindOne(dependency.Handle, argument);
  if not Assigned(model) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
  if context.IsInResolution(model) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [model.ComponentType.Name]);

  context.EnterResolution(model);
  try
    instance := model.LifetimeManager.Resolve(context);
  finally
    context.LeaveResolution(model);
  end;

  Result := InternalResolveValue(dependency.Handle, instance);
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependencies: TArray<TRttiType>;
  const arguments: TArray<TValue>): Boolean;
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
      if not arguments[i].IsEmpty and arguments[i].IsType(dependency.Handle) then
        Continue;
      if not CanResolve(context, dependency, arguments[i]) then
        Exit(False);
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
      if not CanResolve(context, dependency, nil) then
        Exit(False);
  end
  else
    Exit(False);
end;

function TDependencyResolver.CanResolveFromContext(
  const context: ICreationContext; const dependency: TRttiType;
  const argument: TValue): Boolean;
begin
  Result := Assigned(context) and context.CanResolve(context, dependency, argument)
end;

function TDependencyResolver.CanResolveFromSubResolvers(
  const context: ICreationContext; const dependency: TRttiType;
  const argument: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to fSubResolvers.Count - 1 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(True);
  Result := False;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependencies: TArray<TRttiType>;
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
      if not arguments[i].IsEmpty and arguments[i].IsType(dependencies[i].Handle) then
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


{$REGION 'TSubDependencyResolver'}

constructor TSubDependencyResolver.Create(const kernel: IKernel);
begin
{$IFNDEF DISABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
end;

{$ENDREGION}


{$REGION 'TLazyResolver'}

function TLazyResolver.CanResolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := TType.IsLazy(dependency.Handle) and Kernel.Resolver.CanResolve(
    context, dependency.GetGenericArguments[0], argument);
end;

function TLazyResolver.InternalResolveClass(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue; lazyKind: TLazyKind): TValue;
var
  value: TValue;
  factory: TFunc<TObject>;
begin
  value := argument;
  factory :=
    function: TObject
    begin
      Result := Kernel.Resolver.Resolve(context, dependency, value).AsObject;
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<TObject>>(factory);
    lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(factory));
  end;
end;

function TLazyResolver.InternalResolveInterface(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue; lazyKind: TLazyKind): TValue;
var
  value: TValue;
  factory: TFunc<IInterface>;
begin
  value := argument;
  factory :=
    function: IInterface
    begin
      Result := Kernel.Resolver.Resolve(context, dependency, value).AsInterface;
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<IInterface>>(factory);
    lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(factory));
  end;
end;

function TLazyResolver.Resolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  dependencyType: TRttiType;
  model: TComponentModel;
begin
  if not TType.IsLazy(dependency.Handle) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);

  lazyKind := TType.GetLazyKind(dependency.Handle);
  dependencyType := dependency.GetGenericArguments[0];

  model := Kernel.Registry.FindOne(dependencyType.Handle, argument);
  if not Assigned(model) then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
  if context.IsInResolution(model) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [model.ComponentType.Name]);

  case dependencyType.TypeKind of
    tkClass: Result := InternalResolveClass(context, dependencyType, argument, lazyKind);
    tkInterface: Result := InternalResolveInterface(context, dependencyType, argument, lazyKind);
  else
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
  end;

  TValueData(Result).FTypeInfo := dependency.Handle;
end;

{$ENDREGION}


{$REGION 'TDynamicArrayResolver'}

function TDynamicArrayResolver.CanResolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := dependency.IsDynamicArray and Kernel.Resolver.CanResolve(
    context, dependency.AsDynamicArray.ElementType, TValue.From(tkDynArray));
end;

function TDynamicArrayResolver.Resolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): TValue;
var
  dependencyType: TRttiType;
  modelType: PTypeInfo;
  models: IEnumerable<TComponentModel>;
  values: TArray<TValue>;
  i: Integer;
  model: TComponentModel;
begin
  if not dependency.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);

  dependencyType := dependency.AsDynamicArray.ElementType;
  // TODO: remove dependency on lazy type
  if TType.IsLazy(dependencyType.Handle) then
    modelType := dependencyType.GetGenericArguments[0].Handle
  else
    modelType := dependencyType.Handle;
  models := Kernel.Registry.FindAll(modelType);
  SetLength(values, models.Count);
  i := 0;
  for model in models do
  begin
    values[i] := Kernel.Resolver.Resolve(
      context, dependencyType, model.GetServiceName(modelType));
    Inc(i);
  end;
  Result := TValue.FromArray(dependency.Handle, values);
end;

{$ENDREGION}


{$REGION 'TListResolver'}

function TListResolver.CanResolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): Boolean;
const
  SupportedTypes: array[0..2] of string = (
    'IList<>', 'ICollection<>', 'IEnumerable<>');
begin
  Result := dependency.IsGenericType
    and MatchText(dependency.GetGenericTypeDefinition, SupportedTypes)
    and (dependency.GetGenericArguments[0].TypeKind in [tkClass, tkInterface]);
end;

function TListResolver.Resolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): TValue;
var
  itemType: TRttiType;
  arrayType: TRttiType;
  values: TValue;
begin
  itemType := dependency.GetGenericArguments[0];
  arrayType := TType.FindType('TArray<' + itemType.DefaultName + '>');
  values := (Kernel as IKernelInternal).Resolve(arrayType.Handle);
  case itemType.TypeKind of
    tkClass:
    begin
      TValueData(values).FTypeInfo := TypeInfo(TArray<TObject>);
      Result := TValue.From(TList<TObject>.Create(values.AsType<TArray<TObject>>())).Cast(dependency.Handle);
    end;
    tkInterface:
    begin
      TValueData(values).FTypeInfo := TypeInfo(TArray<IInterface>);
      Result := TValue.From(TList<IInterface>.Create(values.AsType<TArray<IInterface>>())).Cast(dependency.Handle);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TPrimitivesResolver'}

function TPrimitivesResolver.CanResolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := not Kernel.HasService(dependency.Handle)
    and (not (dependency.TypeKind in [tkClass, tkInterface, tkRecord])
    or (argument.Kind in [tkClass, tkInterface, tkRecord]));
end;

function TPrimitivesResolver.Resolve(const context: ICreationContext;
  const dependency: TRttiType; const argument: TValue): TValue;
begin
  Result := argument;
{$IFDEF DELPHI2010}
  if Result.IsEmpty then
    TValue.Make(nil, dependency.Handle, Result);
{$ENDIF}
end;

{$ENDREGION}


end.
