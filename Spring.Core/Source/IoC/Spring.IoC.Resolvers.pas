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
  Spring.IoC.Core;

type
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver, IInterface)
  private
    fRegistry: IServiceRegistry;
    fDependencies: TList<PTypeInfo>;
  public
    constructor Create(const serviceRegistry: IServiceRegistry);
    destructor Destroy; override;
    function CanResolve(const member: IInjection): Boolean; overload;
    function ResolveDependencies(const member: IInjection): TArray<TValue>; overload;
  end;

  TServiceResolver = class(TInterfacedObject, IServiceResolver, IInterface)
  private
    fRegistry: IServiceRegistry;
  public
    constructor Create(const serviceRegistry: IServiceRegistry);
    function CanResolve(typeInfo: PTypeInfo): Boolean;
    function Resolve(typeInfo: PTypeInfo; const name: string): TValue;
  end;

implementation

uses
  Spring.Helpers,
  Spring.ResourceStrings,
  Spring.IoC.ResourceStrings;

{ TDependencyResolver }

constructor TDependencyResolver.Create(const serviceRegistry: IServiceRegistry);
begin
  inherited Create;
  fRegistry := serviceRegistry;
  fDependencies := TList<PTypeInfo>.Create;
end;

destructor TDependencyResolver.Destroy;
begin
  fDependencies.Free;
  inherited Destroy;
end;

function TDependencyResolver.CanResolve(
  const member: IInjection): Boolean;
var
  dependency: TRttiType;
begin
  TArgument.CheckNotNull(member, 'member');
  Result := True;
  for dependency in member.GetDependencies do
  begin
    if not dependency.IsClassOrInterface or
      not fRegistry.HasServiceType(dependency.Handle) then  // TODO: CanResolve (TEMP)
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TDependencyResolver.ResolveDependencies(
  const member: IInjection): TArray<TValue>;
var
  dependency: TRttiType;
  model: TComponentModel;
  instance: TObject;
  localInterface: Pointer;
  index: Integer;
begin
  TArgument.CheckNotNull(member, 'member');
  SetLength(Result, member.DependencyCount);
  index := 0;
  for dependency in member.GetDependencies do
  begin
    if fDependencies.Contains(dependency.Handle) then
    begin
      raise ECircularDependencyException.CreateResFmt(
        @SCircularDependencyDetected, 
        [dependency.Name]
      );
    end;
    model := fRegistry.FindOne(dependency.Handle);
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
      raise EResolveException.CreateRes(@SUnexpectedDependencyParameterType);
    end;
    Assert(not Result[index].IsEmpty);
    Inc(index);
  end;
end;

{ TServiceResolver }

constructor TServiceResolver.Create(const serviceRegistry: IServiceRegistry);
begin
  TArgument.CheckNotNull(serviceRegistry, 'serviceRegistry');
  inherited Create;
  fRegistry := serviceRegistry;
end;

function TServiceResolver.CanResolve(typeInfo: PTypeInfo): Boolean;
begin
  // TODO: CanResolve
  Result := True;
end;

function TServiceResolver.Resolve(typeInfo: PTypeInfo;
  const name: string): TValue;
var
  model: TComponentModel;
  instance: TObject;
  localInterface: Pointer;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');
  TArgument.CheckTypeKind(typeInfo, [tkClass, tkInterface], 'typeInfo');

  Result := nil;
  model := fRegistry.FindOne(typeInfo);
  if model = nil then
  begin
    if typeInfo.Kind = tkClass then
    begin
      fRegistry.RegisterType('', typeInfo, typeInfo, ltTransient);
      Result := Resolve(typeInfo, '');
      Exit;
    end
    else
    begin
      raise EResolveException.CreateResFmt(@SNoComponentFound, [GetTypeName(typeInfo)]);
    end;
  end;
  if model.LifetimeManager = nil then
  begin
    raise EResolveException.CreateRes(@SLifetimeManagerWasExpected);
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
