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
      const injections: IList<IInjection>; const context: ICreationContext);
    property Kernel: IKernel read fKernel;
    property Model: TComponentModel read fModel;
  public
    constructor Create(const kernel: IKernel; const model: TComponentModel);
    function CreateInstance(const context: ICreationContext): TValue; overload; virtual; abstract;
  end;

  ///	<summary>
  ///	  Activates an instance by reflection.
  ///	</summary>
  TReflectionComponentActivator = class(TComponentActivatorBase)
  protected
    function CheckConstructorCandidate(const candidate: IInjection;
      const context: ICreationContext): Boolean; virtual;
    function CreateConstructorArguments(const injection: IInjection;
      const context: ICreationContext): TArray<TValue>; virtual;
    function SelectEligibleConstructor(
      const context: ICreationContext): IInjection; virtual;
  public
    function CreateInstance(const context: ICreationContext): TValue; override;
  end;

  ///	<summary>
  ///	  Activates an instance by a TActivatorDelegate delegate.
  ///	</summary>
  TDelegateComponentActivator = class(TComponentActivatorBase)
  public
    function CreateInstance(const context: ICreationContext): TValue; override;
  end;

implementation

uses
  Rtti,
  SysUtils,
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
  const injections: IList<IInjection>; const context: ICreationContext);
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  for injection in injections do
  begin
    arguments := Kernel.DependencyResolver.Resolve(
      context, injection.Dependencies, injection.Arguments);
    injection.Inject(instance, arguments);
  end;
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator'}

function TReflectionComponentActivator.CheckConstructorCandidate(
  const candidate: IInjection; const context: ICreationContext): Boolean;
var
  arguments: TArray<TValue>;
begin
  Result := context.CheckConstructorCandidate(candidate);
  if Result then
  begin
    arguments := context.CreateConstructorArguments(candidate);
    Result := Kernel.DependencyResolver.CanResolve(
      context, candidate.Dependencies, arguments);
  end;
end;

function TReflectionComponentActivator.CreateConstructorArguments(
  const injection: IInjection; const context: ICreationContext): TArray<TValue>;
var
  arguments: TArray<TValue>;
begin
  arguments := context.CreateConstructorArguments(injection);
  Result := Kernel.DependencyResolver.Resolve(
    context, injection.Dependencies, arguments);
end;

function TReflectionComponentActivator.CreateInstance(
  const context: ICreationContext): TValue;
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  injection := SelectEligibleConstructor(context);
  if injection = nil then
    raise EActivatorException.CreateRes(@SUnsatisfiedConstructor);
  arguments := CreateConstructorArguments(injection, context);
  Result := TActivator.CreateInstance(
    Model.ComponentType.AsInstance, injection.Target.AsMethod, arguments);
  try
    ExecuteInjections(Result, Model.FieldInjections, context);
    ExecuteInjections(Result, Model.PropertyInjections, context);
    ExecuteInjections(Result, Model.MethodInjections, context);
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

function TReflectionComponentActivator.SelectEligibleConstructor(
  const context: ICreationContext): IInjection;
var
  candidate: IInjection;
  winner: IInjection;
  maxCount: Integer;
begin
  winner := nil;
  maxCount := -1;

  for candidate in Model.ConstructorInjections do
  begin
    if candidate.Target.HasCustomAttribute<InjectAttribute> then
    begin
      winner := candidate;
      Break;
    end;
    if candidate.DependencyCount > maxCount then
      if CheckConstructorCandidate(candidate, context) then
      begin
        winner := candidate;
        maxCount := candidate.DependencyCount;
      end;
  end;
  Result := winner;
end;

{$ENDREGION}


{$REGION 'TDelegateComponentActivator'}

function TDelegateComponentActivator.CreateInstance(
  const context: ICreationContext): TValue;
begin
  if not Assigned(Model.ActivatorDelegate) then
    raise EActivatorException.CreateRes(@SActivatorDelegateExpected);
  Result := Model.ActivatorDelegate.Invoke;
  try
    ExecuteInjections(Result, Model.FieldInjections, context);
    ExecuteInjections(Result, Model.PropertyInjections, context);
    ExecuteInjections(Result, Model.MethodInjections, context);
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
