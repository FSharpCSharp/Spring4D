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
  /// <summary>
  /// Abstract ComponentActivator
  /// </summary>
  TComponentActivatorBase = class abstract(TInterfacedObject, IComponentActivator, IInterface)
  private
    fModel: TComponentModel;
  protected
    property Model: TComponentModel read fModel;
  public
    constructor Create(model: TComponentModel);
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
    constructor Create(model: TComponentModel; const resolver: IDependencyResolver);
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
  Spring.Core.ResourceStrings;


{$REGION 'TComponentActivatorBase'}

constructor TComponentActivatorBase.Create(model: TComponentModel);
begin
  inherited Create;
  fModel := model;
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator'}

constructor TReflectionComponentActivator.Create(model: TComponentModel;
  const resolver: IDependencyResolver);
begin
  inherited Create(model);
  fResolver := resolver;
end;

function TReflectionComponentActivator.CreateInstance: TObject;
var
  componentType: TRttiInstanceType;
  constructorInjection: IInjection;
  constructorArguments: TArray<TValue>;
begin
  constructorInjection := GetEligibleConstructor(fModel);
  if constructorInjection = nil then
  begin
    raise EActivatorException.CreateRes(@SUnsatisfiedConstructor);
  end;
  constructorArguments := fResolver.ResolveDependencies(constructorInjection);
  componentType := fModel.ComponentType as TRttiInstanceType;
  Result := InternalCreateInstance(
    componentType.MetaclassType,
    (constructorInjection.Target as TRttiMethod),
    constructorArguments
  );
  ExecuteInjections(Result, fModel.PropertyInjections);
  ExecuteInjections(Result, fModel.MethodInjections);
  ExecuteInjections(Result, fModel.FieldInjections);
end;

procedure TReflectionComponentActivator.ExecuteInjections(instance: TObject;
  const injections: IList<IInjection>);
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  for injection in injections do
  begin
    arguments := fResolver.ResolveDependencies(injection);
    injection.Inject(instance, arguments);
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

  for candidate in model.ConstructorInjections do
  begin
    if candidate.Target.HasCustomAttribute<InjectionAttribute> then
    begin
      winner := candidate;
      Break;
    end;
    if fResolver.CanResolveDependencies(candidate) then
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

type
  TInterfacedObjectHack = class(TInterfacedObject);

function TReflectionComponentActivator.InternalCreateInstance(classType: TClass;
  constructorMethod: TRttiMethod; const arguments: TArray<TValue>): TObject;
begin
  Result := classType.NewInstance;
  try
    constructorMethod.Invoke(Result, arguments);
  except
    on Exception do
    begin
      if Result is TInterfacedObject then
      begin
        Dec(TInterfacedObjectHack(Result).FRefCount);
      end;
      Result.Free;
      raise;
    end;
  end;
  try
    Result.AfterConstruction;
  except
    on Exception do
    begin
      Result.Free;
      raise;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TDelegateComponentActivator'}

function TDelegateComponentActivator.CreateInstance: TObject;
begin
  if not Assigned(fModel.ActivatorDelegate) then
  begin
    raise EActivatorException.CreateRes(@SActivatorDelegateExpected);
  end;
  Result := fModel.ActivatorDelegate.Invoke;
end;

{$ENDREGION}

end.
