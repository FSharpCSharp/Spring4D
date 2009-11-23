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

unit Spring.IoC.ComponentActivator;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Spring.System,
  Spring.Collections,
  Spring.IoC.Core;

type
  TComponentActivatorBase = class abstract(TInterfacedObject, IComponentActivator, IInterface)
  private
    fComponentModel: TComponentModel;
  protected
    property ComponentModel: TComponentModel read fComponentModel;
  public
    constructor Create(componentModel: TComponentModel);
    function CreateInstance: TObject; virtual; abstract;
  end;

  /// <summary>
  /// Activates an instance by reflection.
  /// </summary>
  TReflectionComponentActivator = class(TComponentActivatorBase)
  private
    fResolver: IDependencyResolver;
    function GetEligibleConstructor(model: TComponentModel): IInjection; virtual;
    function InternalCreateInstance(classType: TClass; constructorMethod: TRttiMethod;
      const arguments: TArray<TValue>): TObject;
    procedure ExecuteInjections(instance: TObject; const injections: IList<IInjection>);
  public
    constructor Create(componentModel: TComponentModel; const resolver: IDependencyResolver);
    function CreateInstance: TObject; override;
  end;

  /// <summary>
  /// Activates an instance by a TActivatorDelegate delegate.
  /// </summary>
  TDelegateComponentActivator = class(TComponentActivatorBase)
  public
    function CreateInstance: TObject; override;
  end;

implementation

uses
  Spring.Helpers,
  Spring.IoC.ResourceStrings;


{$REGION 'TComponentActivatorBase'}

constructor TComponentActivatorBase.Create(componentModel: TComponentModel);
begin
  TArgument.CheckNotNull(componentModel, 'componentModel');
  inherited Create;
  fComponentModel := componentModel;
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator'}

constructor TReflectionComponentActivator.Create(componentModel: TComponentModel;
  const resolver: IDependencyResolver);
begin
  TArgument.CheckNotNull(resolver, 'resolver');
  inherited Create(componentModel);
  fResolver := resolver;
end;

function TReflectionComponentActivator.CreateInstance: TObject;
var
  componentType: TRttiInstanceType;
  constructorInjection: IInjection;
  constructorArguments: TArray<TValue>;
begin
  constructorInjection := GetEligibleConstructor(fComponentModel);
  constructorArguments := fResolver.ResolveDependencies(constructorInjection);
  componentType := fComponentModel.ComponentType as TRttiInstanceType;
  Result := InternalCreateInstance(
    componentType.MetaclassType,
    (constructorInjection.MemberType as TRttiMethod),
    constructorArguments
  );
  ExecuteInjections(Result, fComponentModel.Properties);
  ExecuteInjections(Result, fComponentModel.Methods);
  ExecuteInjections(Result, fComponentModel.Fields);
end;

procedure TReflectionComponentActivator.ExecuteInjections(instance: TObject;
  const injections: IList<IInjection>);
var
  member: IInjection;
  arguments: TArray<TValue>;
begin
  for member in injections do
  begin
    if fResolver.CanResolve(member) then
    begin
      arguments := fResolver.ResolveDependencies(member);
      member.Inject(instance, arguments);
    end;
  end;
end;

function TReflectionComponentActivator.GetEligibleConstructor(
  model: TComponentModel): IInjection;
var
  candidate: IInjection;
  winner: IInjection;
  maxCount: Integer;
begin
  winner := nil;
  maxCount := -1;

  for candidate in model.Constructors do
  begin
    if candidate.MemberType.HasCustomAttribute<InjectionAttribute> then
    begin
      winner := candidate;
      Break;
    end
    else if fResolver.CanResolve(candidate) then
    begin
      if candidate.DependencyCount > maxCount then
      begin
        winner := candidate;
        maxCount := candidate.DependencyCount;
      end;
    end;
  end;
  Result := winner;
end;

function TReflectionComponentActivator.InternalCreateInstance(classType: TClass;
  constructorMethod: TRttiMethod; const arguments: TArray<TValue>): TObject;
begin
  Result := classType.NewInstance;
  try
    constructorMethod.Invoke(Result, arguments);
  except
    on Exception do
    begin
      Result.Destroy;
      raise;
    end;
  end;
  try
    Result.AfterConstruction;
  except
    on Exception do
    begin
      Result.BeforeDestruction;
      Result.Destroy;
      raise;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TDelegateComponentActivator'}

function TDelegateComponentActivator.CreateInstance: TObject;
begin
  if not Assigned(fComponentModel.ActivatorDelegate) then
  begin
    raise EActivatorException.CreateRes(@SActivatorDelegateExpected);
  end;
  Result := fComponentModel.ActivatorDelegate.Invoke;
end;

{$ENDREGION}


end.
