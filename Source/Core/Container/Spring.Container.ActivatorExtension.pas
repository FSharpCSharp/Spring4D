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

unit Spring.Container.ActivatorExtension;

{$I Spring.inc}

interface

uses
  Spring.Container,
  Spring.Container.Builder,
  Spring.Container.ComponentActivator,
  Spring.Container.Core,
  Spring.Container.Extensions;

type
  TActivatorContainerExtension = class(TContainerExtension)
  protected
    procedure Initialize; override;
  end;

  TActivatorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel;
      const model: TComponentModel); override;
  end;

  TReflectionComponentActivator2 = class(TReflectionComponentActivator)
  protected
    function GetEligibleConstructor(const model: TComponentModel;
      const resolver: IDependencyResolver): IInjection; override;
  end;

resourcestring
  SAmbiguousConstructor = 'Ambiguous constructor.';


implementation

uses
  Spring.Container.Common,
  Spring.Container.ResourceStrings,
  Spring.Helpers;


{$REGION 'TActivatorContainerExtension'}

procedure TActivatorContainerExtension.Initialize;
begin
  Kernel.ComponentBuilder.AddInspector(TActivatorInspector.Create);
end;

{$ENDREGION}


{$REGION 'TActivatorInspector'}

procedure TActivatorInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
begin
  if not Assigned(model.ActivatorDelegate) then
    model.ComponentActivator := TReflectionComponentActivator2.Create(kernel, model);
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator2'}

function TReflectionComponentActivator2.GetEligibleConstructor(
  const model: TComponentModel;
  const resolver: IDependencyResolver): IInjection;
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
    if candidate.DependencyCount = maxCount then
      raise EResolveException.CreateRes(@SAmbiguousConstructor);
    if candidate.DependencyCount > maxCount then
    begin
      winner := candidate;
      maxCount := candidate.DependencyCount;
    end;
  end;
  Result := winner;
  if Assigned(Result) and not resolver.CanResolve(
    Result.Dependencies, Result.Arguments, Result.Target) then
    raise EResolveException.CreateRes(@SUnsatisfiedConstructorParameters);
end;

{$ENDREGION}


end.
