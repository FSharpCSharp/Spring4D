{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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

{$I Spring.inc}

unit Spring.Container.Resolvers;

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TResolverBase = class abstract(TInterfacedObject, IResolver)
  private
    fKernel: TKernel;
  protected
    property Kernel: TKernel read fKernel;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean; virtual;
    function Resolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): TValue; virtual; abstract;
  end;

  TDependencyResolver = class(TResolverBase, IDependencyResolver)
  private
    fResolvers: IList<IResolver>;
  protected
    function CanResolveFromArgument(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean;
    function CanResolveFromContext(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean;
    function CanResolveFromResolvers(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean;
    function InternalResolveValue(const context: ICreationContext;
      const model: TComponentModel; const target: ITarget;
      const instance: TValue): TValue;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean; overload; override;
    function CanResolve(const context: ICreationContext;
      const targets: TArray<ITarget>;
      const arguments: TArray<TValue>): Boolean; reintroduce; overload; virtual;

    function Resolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): TValue; overload; override;
    function Resolve(const context: ICreationContext;
      const targets: TArray<ITarget>;
      const arguments: TArray<TValue>): TArray<TValue>; reintroduce; overload; virtual;

    procedure AddResolver(const resolver: IResolver);
    procedure RemoveResolver(const resolver: IResolver);
  end;

  TLazyResolver = class(TResolverBase)
  private
    function InternalResolve<T>(const context: ICreationContext;
      const target: ITarget; const argument: TValue; lazyKind: TLazyKind): TValue;

  public
    function CanResolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): TValue; override;
  end;

  TDynamicArrayResolver = class(TResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): TValue; override;
  end;

  TCollectionResolver = class(TResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): TValue; override;
  end;

  TComponentOwnerResolver = class(TResolverBase)
  private
    fVirtualIndex: SmallInt;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const target: ITarget; const argument: TValue): TValue; override;
  end;

  TDecoratorResolver = class(TInterfacedObject, IDecoratorResolver)
  private
    type
      TDecoratorEntry = record
        DecoratorModel: TComponentModel;
        Condition: Predicate<TComponentModel>;
      end;
  private
    fDecorators: IMultiMap<PTypeInfo, TDecoratorEntry>;
    function GetDecorators(decoratedType: PTypeInfo;
      const decoratedModel: TComponentModel): IEnumerable<TComponentModel>;
  public
    constructor Create;

    procedure AddDecorator(decoratedType: PTypeInfo;
      const decoratorModel: TComponentModel;
      const condition: Predicate<TComponentModel>);
    function Resolve(const target: ITarget; const model: TComponentModel;
      const context: ICreationContext; const decoratee: TValue): TValue;
  end;

implementation

uses
  Classes,
  StrUtils,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Container.CreationContext,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TResolverBase'}

constructor TResolverBase.Create(const kernel: TKernel);
begin
{$IFNDEF DISABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
end;

function TResolverBase.CanResolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): Boolean;
begin
  if not argument.IsEmpty and argument.IsString then
    Result := not Kernel.Registry.HasService(target.TypeInfo, argument.AsString)
      and (target.TypeInfo <> argument.TypeInfo)
  else
    Result := not Kernel.Registry.HasService(target.TypeInfo)
      and (target.TypeInfo <> argument.TypeInfo);
end;

{$ENDREGION}


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const kernel: TKernel);
begin
  inherited Create(kernel);
  fResolvers := TCollections.CreateInterfaceList<IResolver>;
end;

procedure TDependencyResolver.AddResolver(
  const resolver: IResolver);
begin
  fResolvers.Add(resolver);
end;

procedure TDependencyResolver.RemoveResolver(
  const resolver: IResolver);
begin
  fResolvers.Remove(resolver);
end;

function TDependencyResolver.InternalResolveValue(
  const context: ICreationContext; const model: TComponentModel;
  const target: ITarget; const instance: TValue): TValue;
var
  intf: Pointer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(context, 'context');
  Guard.CheckNotNull(target, 'target');
  Guard.CheckNotNull(not instance.IsEmpty, 'instance');
{$ENDIF}

  if target.TypeInfo.Kind = tkInterface then
  begin
    if instance.IsObject then
      instance.AsObject.GetInterface(target.TypeInfo.TypeData.Guid, intf)
    else
    begin
      if IsMethodReference(target.TypeInfo) then
      begin
        intf := nil;
        IInterface(intf) := instance.AsInterface;
      end
      else
        instance.AsInterface.QueryInterface(target.TypeInfo.TypeData.Guid, intf);
    end;
    TValue.MakeWithoutCopy(@intf, target.TypeInfo, Result);
    Result := Kernel.ProxyFactory.CreateInstance(context, Result, model, []);
  end
  else
    Result := instance;

  Result := Kernel.DecoratorResolver.Resolve(target, model, context, Result);
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): Boolean;
var
  kind: TTypeKind;
  serviceName: string;
  serviceType: PTypeInfo;
  componentModel: TComponentModel;
begin
  if target.TypeInfo = nil then
    Exit(True);

  if CanResolveFromContext(context, target, argument) then
    Exit(True);

  if CanResolveFromResolvers(context, target, argument) then
    Exit(True);

  if argument.IsEmpty then
    Result := Kernel.Registry.HasDefault(target.TypeInfo)
  else if CanResolveFromArgument(context, target, argument) then
    Result := True
  else if argument.TryAsType(TypeInfo(TTypeKind), kind) and (kind = tkDynArray) then
    Result := Kernel.Registry.HasService(target.TypeInfo)
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
        Result := IsAssignableFrom(target.TypeInfo, serviceType);
      end;
    end;
  end;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): TValue;
var
  i: Integer;
  componentModel: TComponentModel;
  instance: TValue;
begin
  if target.TypeInfo = nil then
    Exit(TValue.Empty);

  if CanResolveFromContext(context, target, argument) then
    Exit(context.Resolve(context, target, argument));

  for i := fResolvers.Count - 1 downto 0 do
    if fResolvers[i].CanResolve(context, target, argument) then
      Exit(fResolvers[i].Resolve(context, target, argument));

  if CanResolveFromArgument(context, target, argument) then
    Exit(argument);

  componentModel := Kernel.Registry.FindOne(target.TypeInfo, argument);

  if context.EnterResolution(componentModel, instance) then
  try
    instance := componentModel.LifetimeManager.Resolve(context, componentModel);
  finally
    context.LeaveResolution(componentModel);
  end;
  Result := InternalResolveValue(context, componentModel, target, instance);
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const targets: TArray<ITarget>; const arguments: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  if Length(targets) = Length(arguments) then
  begin
    for i := Low(targets) to High(targets) do
      if not CanResolve(context, targets[i], arguments[i]) then
        Exit(False);
  end
  else if arguments = nil then
  begin
    for i := Low(targets) to High(targets) do
      if not CanResolve(context, targets[i], nil) then
        Exit(False);
  end
  else
    Exit(False);
  Result := True;
end;

function TDependencyResolver.CanResolveFromArgument(
  const context: ICreationContext; const target: ITarget;
  const argument: TValue): Boolean;
begin
  Result := Assigned(argument.TypeInfo) and argument.IsType(target.TypeInfo);
  if not Result and (argument.Kind in [tkInteger, tkFloat, tkInt64]) then
    Result := argument.Kind = target.TypeInfo.Kind;
  if Result and argument.IsString then
    Result := not Kernel.Registry.HasService(target.TypeInfo, argument.AsString);
end;

function TDependencyResolver.CanResolveFromContext(
  const context: ICreationContext;  const target: ITarget;
  const argument: TValue): Boolean;
begin
  Result := Assigned(context)
    and context.CanResolve(context, target, argument);
end;

function TDependencyResolver.CanResolveFromResolvers(
  const context: ICreationContext; const target: ITarget;
  const argument: TValue): Boolean;
var
  i: Integer;
begin
  for i := fResolvers.Count - 1 downto 0 do
    if fResolvers[i].CanResolve(context, target, argument) then
      Exit(True);
  Result := False;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const targets: TArray<ITarget>; const arguments: TArray<TValue>): TArray<TValue>;
var
  i: Integer;
begin
  if Assigned(arguments) and (Length(arguments) <> Length(targets)) then
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  SetLength(Result, Length(targets));
  if Assigned(arguments) then
    for i := Low(targets) to High(targets) do
      Result[i] := Resolve(context, targets[i], arguments[i])
  else
    for i := Low(targets) to High(targets) do
      Result[i] := Resolve(context, targets[i], nil);
end;

{$ENDREGION}


{$REGION 'TLazyResolver'}

function TLazyResolver.CanResolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): Boolean;
var
  targetType: TRttiType;
  newTarget: ITarget;
begin
  Result := IsLazyType(target.TypeInfo)
    and inherited CanResolve(context, target, argument);
  if Result then
  begin
    targetType := GetLazyType(target.TypeInfo).RttiType;
    newTarget := TTarget.Create(targetType, target.Target);
    Result := Kernel.Resolver.CanResolve(context, newTarget, argument);
  end;
end;

function TLazyResolver.InternalResolve<T>(const context: ICreationContext;
  const target: ITarget; const argument: TValue; lazyKind: TLazyKind): TValue;
var
  value: TValue;
  factory: Func<T>;
  intf: IInterface;
begin
  value := argument;
  factory :=
    function: T
    begin
      Kernel.Resolver.Resolve(context, target, value).AsType(TypeInfo(T), Result);
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<Func<T>>(factory);
    lkRecord: Result := TValue.From<Lazy<T>>(Lazy<T>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<T>>(Lazy<T>.Create(factory));
  end;
end;

function TLazyResolver.Resolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  targetType: TRttiType;
  newTarget: ITarget;
  componentModel: TComponentModel;
  hasEntered: Boolean;
begin
  if not IsLazyType(target.TypeInfo) then
    raise EResolveException.CreateResFmt(@SCannotResolveType, [target.TypeInfo.TypeName]);

  lazyKind := GetLazyKind(target.TypeInfo);
  targetType := GetLazyType(target.TypeInfo).RttiType;
  newTarget := TTarget.Create(targetType, target.Target);
  if Kernel.Registry.HasService(targetType.Handle) then
  begin
    componentModel := Kernel.Registry.FindOne(targetType.Handle, argument);
    hasEntered := context.EnterResolution(componentModel, Result);
  end
  else
  begin
    componentModel := nil;
    hasEntered := False;
  end;
  try
    case targetType.TypeKind of
      tkClass: Result := InternalResolve<TObject>(
        context, newTarget, argument, lazyKind);
      tkInterface: Result := InternalResolve<IInterface>(
        context, newTarget, argument, lazyKind);
      tkUString: Result := InternalResolve<string>(
        context, newTarget, argument, lazyKind);
      tkInteger: Result := InternalResolve<Integer>(
        context, newTarget, argument, lazyKind);
      tkInt64: Result := InternalResolve<Int64>(
        context, newTarget, argument, lazyKind);
    else
      raise EResolveException.CreateResFmt(@SCannotResolveType, [target.TypeInfo.TypeName]);
    end;
    TValueData(Result).FTypeInfo := target.TypeInfo;
  finally
    if hasEntered then
      context.LeaveResolution(componentModel);
  end;
end;

{$ENDREGION}


{$REGION 'TDynamicArrayResolver'}

function ResolveDynamicArray(const kernel: TKernel; const context: ICreationContext;
  const target: ITarget; const targetType: TRttiType): TArray<TValue>;
var
  serviceType: PTypeInfo;
  models: TArray<TComponentModel>;
  i: Integer;
  serviceName: string;
begin
  // TODO: remove dependency on lazy type
  serviceType := targetType.Handle;
  if IsLazyType(serviceType) then
    serviceType := GetLazyType(serviceType);
  models := kernel.Registry.FindAll(serviceType).ToArray;

  SetLength(Result, Length(models));
  for i := Low(models) to High(models) do
  begin
    serviceName := models[i].GetServiceName(serviceType);
    Result[i] := Kernel.Resolver.Resolve(context, target, serviceName);
  end;
end;

function TDynamicArrayResolver.CanResolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): Boolean;
var
  targetType: TRttiType;
  newTarget: ITarget;
begin
  targetType := target.TypeInfo.RttiType;
  Result := targetType.IsDynamicArray

    and inherited CanResolve(context, target, argument);
  if Result then
  begin
    targetType := targetType.AsDynamicArray.ElementType;
    newTarget := TTarget.Create(targetType, nil);
    Result := Kernel.Resolver.CanResolve(context, newTarget, TValue.From(tkDynArray));
  end;
end;

function TDynamicArrayResolver.Resolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): TValue;
var
  targetType: TRttiType;
  newTarget: ITarget;
  values: TArray<TValue>;
begin
  targetType := target.TypeInfo.RttiType;
  if not targetType.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveType, [target.TypeInfo.TypeName]);
  targetType := targetType.AsDynamicArray.ElementType;
  newTarget := TTarget.Create(targetType, nil);

  values := ResolveDynamicArray(Kernel, context, newTarget, targetType);
  Result := TValue.FromArray(target.TypeInfo, values);
end;

{$ENDREGION}


{$REGION 'TCollectionResolver'}

function TCollectionResolver.CanResolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): Boolean;
const
  SupportedTypes: array[0..4] of string = (
    'IList<>', 'IReadOnlyList<>', 'ICollection<>', 'IReadOnlyCollection<T>', 'IEnumerable<>');
var
  targetType: TRttiType;
  newTarget: ITarget;
begin
  targetType := target.TypeInfo.RttiType;
  Result := targetType.IsGenericType
    and MatchText(targetType.GetGenericTypeDefinition, SupportedTypes)
    and inherited CanResolve(context, target, argument);
  if Result then
  begin
    targetType := GetElementType(targetType.Handle).RttiType;
    newTarget := TTarget.Create(targetType, nil);
    Result := targetType.IsClassOrInterface
      and Kernel.Resolver.CanResolve(context, newTarget, TValue.From(tkDynArray));
  end;
end;

function TCollectionResolver.Resolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): TValue;
var
  itemType: TRttiType;
  newTarget: ITarget;
  values: TArray<TValue>;
  list: IInterface;
  i: Integer;
begin
  itemType := GetElementType(target.TypeInfo).RttiType;
  newTarget := TTarget.Create(itemType, nil);
  values := ResolveDynamicArray(Kernel, context, newTarget, itemType);
  case itemType.TypeKind of
    tkClass:
    begin
      list := TCollections.CreateObjectList(itemType.Handle, False);
      for i := Low(values) to High(values) do
        IObjectList(list).Add(values[i].AsObject);
      TValue.Make(@list, target.TypeInfo, Result);
    end;
    tkInterface:
    begin
      list := TCollections.CreateInterfaceList(itemType.Handle);
      for i := Low(values) to High(values) do
        Spring.Collections.IInterfaceList(list).Add(values[i].AsInterface);
      TValue.Make(@list, target.TypeInfo, Result);
    end;
  else
    raise EResolveException.CreateResFmt(@SCannotResolveType, [target.TypeInfo.TypeName]);
  end;
end;

{$ENDREGION}


{$REGION 'TComponentOwnerResolver'}

constructor TComponentOwnerResolver.Create(const kernel: TKernel);
begin
  inherited Create(kernel);
  fVirtualIndex := TType.GetType(TComponent).Constructors.First.VirtualIndex;
end;

function TComponentOwnerResolver.CanResolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): Boolean;
var
  method: TRttiMethod;
begin
  if target.TypeInfo <> TypeInfo(TComponent) then
    Exit(False);
  if Kernel.Registry.HasService(target.TypeInfo) then
    Exit(False);
  if not argument.IsEmpty then
    Exit(False);
  if not (target.Target is TRttiParameter) then
    Exit(False);

  method := TRttiMethod(target.Target.Parent);
  Result := (method.VirtualIndex = fVirtualIndex)
    and (method.Parent.AsInstance.MetaclassType.InheritsFrom(TComponent));
end;

function TComponentOwnerResolver.Resolve(const context: ICreationContext;
  const target: ITarget; const argument: TValue): TValue;
begin
  TValue.Make(nil, TComponent.ClassInfo, Result);
end;

{$ENDREGION}


{$REGION 'TDecoratorResolver'}

constructor TDecoratorResolver.Create;
begin
  inherited Create;
  fDecorators := TCollections.CreateMultiMap<PTypeInfo, TDecoratorEntry>;
end;

procedure TDecoratorResolver.AddDecorator(decoratedType: PTypeInfo;
  const decoratorModel: TComponentModel;
  const condition: Predicate<TComponentModel>);
var
  entry: TDecoratorEntry;
begin
  entry.DecoratorModel := decoratorModel;
  entry.Condition := condition;
  fDecorators.Add(decoratedType, entry);
end;

function TDecoratorResolver.GetDecorators(decoratedType: PTypeInfo;
  const decoratedModel: TComponentModel): IEnumerable<TComponentModel>;
begin
  Result := TEnumerable.Select<TDecoratorEntry,TComponentModel>(
    fDecorators[decoratedType].Where(
      function(const entry: TDecoratorEntry): Boolean
      begin
        Result := not Assigned(entry.Condition) or entry.Condition(decoratedModel);
      end),
    function(const entry: TDecoratorEntry): TComponentModel
    begin
      Result := entry.DecoratorModel;
    end);
end;

function TDecoratorResolver.Resolve(const target: ITarget;
  const model: TComponentModel; const context: ICreationContext;
  const decoratee: TValue): TValue;
var
  decoratorModel: TComponentModel;
  index: Integer;
begin
  Result := decoratee;
  for decoratorModel in GetDecorators(target.TypeInfo, model) do
  begin
    // TODO: make this more explicit to just inject on the decorator constructor
    index := context.AddArgument(TTypedValue.Create(Result, target.TypeInfo));
    try
      Result := decoratorModel.LifetimeManager.Resolve(context, decoratorModel).Cast(target.TypeInfo);
    finally
      context.RemoveTypedArgument(index);
    end;
  end;
end;

{$ENDREGION}


end.
