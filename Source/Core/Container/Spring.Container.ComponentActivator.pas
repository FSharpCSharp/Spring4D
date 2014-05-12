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

unit Spring.Container.ComponentActivator;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  ///	<summary>
  ///	  Abstract ComponentActivator
  ///	</summary>
  TComponentActivatorBase = class abstract(TInterfacedObject, IComponentActivator)
  private
    fKernel: IKernel;
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fModel: TComponentModel;
  protected
    procedure ExecuteInjections(const instance: TValue;
      const injections: IList<IInjection>; const resolver: IDependencyResolver);
    property Kernel: IKernel read fKernel;
    property Model: TComponentModel read fModel;
  public
    constructor Create(const kernel: IKernel; const model: TComponentModel); virtual;
    function CreateInstance(const resolver: IDependencyResolver): TValue; overload; virtual; abstract;
  end;

  ///	<summary>
  ///	  Activates an instance by reflection.
  ///	</summary>
  TReflectionComponentActivator = class(TComponentActivatorBase)
  protected
    function GetEligibleConstructor(const model: TComponentModel;
      const resolver: IDependencyResolver): IInjection; virtual;
  public
    function CreateInstance(const resolver: IDependencyResolver): TValue; override;
  end;

  ///	<summary>
  ///	  Activates an instance by a TActivatorDelegate delegate.
  ///	</summary>
  TDelegateComponentActivator = class(TComponentActivatorBase)
  public
    function CreateInstance(const resolver: IDependencyResolver): TValue; override;
  end;

implementation

uses
  Spring.Container.Common,
  Spring.Container.ResourceStrings,
  Spring.Helpers,
  Spring.Reflection;


{$REGION 'TComponentActivatorBase'}

constructor TComponentActivatorBase.Create(const kernel: IKernel;
  const model: TComponentModel);
begin
  Guard.CheckNotNull(kernel, 'kernel');
  Guard.CheckNotNull(model, 'model');
  inherited Create;
  fKernel := kernel;
  fModel := model;
end;

procedure TComponentActivatorBase.ExecuteInjections(const instance: TValue;
  const injections: IList<IInjection>; const resolver: IDependencyResolver);
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  for injection in injections do
  begin
    arguments := resolver.Resolve(fKernel, injection.Dependencies,
      injection.Arguments, injection.Target);
    injection.Inject(instance, arguments);
  end;
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator'}

function TReflectionComponentActivator.CreateInstance(
  const resolver: IDependencyResolver): TValue;
var
  constructorInjection: IInjection;
  constructorArguments: TArray<TValue>;
begin
  constructorInjection := GetEligibleConstructor(Model, resolver);
  if constructorInjection = nil then
    raise EActivatorException.CreateRes(@SUnsatisfiedConstructor);
  constructorArguments := resolver.Resolve(
    Kernel,
    constructorInjection.Dependencies,
    constructorInjection.Arguments,
    constructorInjection.Target);
  Result := TActivator.CreateInstance(
    Model.ComponentType.AsInstance,
    constructorInjection.Target.AsMethod,
    constructorArguments);
  try
    ExecuteInjections(Result, Model.FieldInjections, resolver);
    ExecuteInjections(Result, Model.PropertyInjections, resolver);
    ExecuteInjections(Result, Model.MethodInjections, resolver);
  except
    if not Result.IsEmpty and Result.IsObject then
    begin
{$IFNDEF AUTOREFCOUNT}
      Result.AsObject.Free;
{$ENDIF}
      Result := nil;
    end;
    raise;
  end;
end;

function TReflectionComponentActivator.GetEligibleConstructor(
  const model: TComponentModel; const resolver: IDependencyResolver): IInjection;
var
  candidate: IInjection;
  winner: IInjection;
  maxCount: Integer;
begin
  winner := nil;
  maxCount := -1;

  for candidate in model.ConstructorInjections do
  begin
    if candidate.Target.HasCustomAttribute<InjectAttribute> then
    begin
      winner := candidate;
      Break;
    end;
    if resolver.CanResolve(Kernel, candidate.Dependencies,
      candidate.Arguments, candidate.Target) then
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

{$ENDREGION}


{$REGION 'TDelegateComponentActivator'}

function TDelegateComponentActivator.CreateInstance(
  const resolver: IDependencyResolver): TValue;
begin
  if not Assigned(Model.ActivatorDelegate) then
    raise EActivatorException.CreateRes(@SActivatorDelegateExpected);
  Result := Model.ActivatorDelegate.Invoke;
  try
    ExecuteInjections(Result, Model.FieldInjections, resolver);
    ExecuteInjections(Result, Model.PropertyInjections, resolver);
    ExecuteInjections(Result, Model.MethodInjections, resolver);
  except
    if not Result.IsEmpty and Result.IsObject then
    begin
{$IFNDEF AUTOREFCOUNT}
      Result.AsObject.Free;
{$ENDIF}
      Result := nil;
    end;
    raise;
  end;
end;

{$ENDREGION}


end.
