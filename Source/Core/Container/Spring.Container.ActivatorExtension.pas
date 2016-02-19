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

unit Spring.Container.ActivatorExtension;

interface

uses
  Spring.Container.Core;

type
  TActivatorContainerExtension = class(TInterfacedObject, IContainerExtension)
  public
    procedure Initialize(const kernel: TKernel);
  end;

implementation

uses
  Rtti,
  Spring.Collections,
  Spring.Container.Common,
  Spring.Container.ResourceStrings,
  Spring.Reflection;

type
  TStrictConstructorSelector = class(TInterfacedObject, IConstructorSelector)
  private
    fKernel: TKernel;
    function TryHandle(const context: ICreationContext;
      const candidate: IInjection; var winner: IInjection): Boolean;
    property Kernel: TKernel read fKernel;
  public
    constructor Create(const kernel: TKernel);
    function Find(const context: ICreationContext;
      const model: TComponentModel): IInjection;
  end;


{$REGION 'TActivatorContainerExtension'}

procedure TActivatorContainerExtension.Initialize(const kernel: TKernel);
begin
  kernel.ConstructorSelector := TStrictConstructorSelector.Create(kernel);
end;

{$ENDREGION}


{$REGION 'TStrictConstructorSelector'}

constructor TStrictConstructorSelector.Create(const kernel: TKernel);
begin
  inherited Create;
  fKernel := kernel;
end;

function TStrictConstructorSelector.Find(const context: ICreationContext;
  const model: TComponentModel): IInjection;
var
  maxCount: Integer;
  targetType: TRttiType;
  candidates: IEnumerable<IInjection>;
  candidate: IInjection;
begin
  Result := Model.ConstructorInjections.FirstOrDefault(
    function(const injection: IInjection): Boolean
    begin
      Result := injection.Target.AsMethod.HasCustomAttribute(InjectAttribute);
    end);
  if Assigned(Result) then
    Exit;

  maxCount := -1;
  targetType := nil;

  candidates := Model.ConstructorInjections.Ordered(
    function(const left, right: IInjection): Integer
    begin
      Result := right.Target.Parent.AncestorCount - left.Target.Parent.AncestorCount;
      if Result = 0 then
        Result := right.DependencyCount - left.DependencyCount;
    end).TakeWhile(
    function(const injection: IInjection): Boolean
    begin
      if maxCount = -1 then
        maxCount := injection.DependencyCount;
      if targetType = nil then
        targetType := injection.Target.Parent;
      Result := (injection.DependencyCount = maxCount)
        and (targetType = injection.Target.Parent);
    end).Where(
    function(const injection: IInjection): Boolean
    begin
      Result := TryHandle(context, injection, candidate);
    end);
  if candidates.Count > 1 then
    raise EResolveException.CreateResFmt(@SAmbiguousConstructor, [targetType.DefaultName]);
  Result := candidate;
end;

function TStrictConstructorSelector.TryHandle(const context: ICreationContext;
  const candidate: IInjection; var winner: IInjection): Boolean;
var
  injection: IInjection;
begin
  Result := context.TryHandle(candidate, injection)
    and Kernel.Resolver.CanResolve(
    context, injection.Dependencies, injection.Arguments);
  if Result then
    winner := injection;
end;

{$ENDREGION}


end.
