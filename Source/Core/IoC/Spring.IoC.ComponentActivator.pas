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
  TDefaultComponentActivator = class(TInterfacedObject, IComponentActivator, IInterface)
  private
    fResolver: IDependencyResolver;
    function GetEligibleConstructor(model: TComponentModel): IInjection; virtual;
    function InternalCreateInstance(classType: TClass; constructorMethod: TRttiMethod;
      const arguments: TArray<TValue>): TObject;
    procedure ExecuteInjections(instance: TObject; const injections: IList<IInjection>);
  public
    constructor Create(const resolver: IDependencyResolver);
    function CreateInstance(componentModel: TComponentModel): TObject;
    procedure DestroyInstance(instance: TObject);
  end;

implementation

uses
  Spring.Helpers;

{$REGION 'TDefaultComponentActivator'}

constructor TDefaultComponentActivator.Create(const resolver: IDependencyResolver);
begin
  TArgument.CheckNotNull(resolver, 'resolver');
  inherited Create;
  fResolver := resolver;
end;

function TDefaultComponentActivator.CreateInstance(
  componentModel: TComponentModel): TObject;
var
  componentType: TRttiInstanceType;
  constructorInjection: IInjection;
  constructorArguments: TArray<TValue>;
begin
  TArgument.CheckNotNull(componentModel, 'componentModel');
  constructorInjection := GetEligibleConstructor(componentModel);
  constructorArguments := fResolver.ResolveDependencies(constructorInjection);
  componentType := componentModel.ComponentType as TRttiInstanceType;
  Result := InternalCreateInstance(
    componentType.MetaclassType,
    (constructorInjection.MemberType as TRttiMethod),
    constructorArguments
  );
  ExecuteInjections(Result, componentModel.Properties);
  ExecuteInjections(Result, componentModel.Methods);
  ExecuteInjections(Result, componentModel.Fields);
end;

procedure TDefaultComponentActivator.DestroyInstance(instance: TObject);
begin

end;

procedure TDefaultComponentActivator.ExecuteInjections(instance: TObject;
  const injections: IList<IInjection>);
var
  member: IInjection;
  parameters: TArray<TValue>;
begin
  for member in injections do
  begin
    if fResolver.CanResolve(member) then
    begin
      parameters := fResolver.ResolveDependencies(member);
      member.Inject(instance, parameters);
    end;
  end;
end;

function TDefaultComponentActivator.GetEligibleConstructor(
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

function TDefaultComponentActivator.InternalCreateInstance(classType: TClass;
  constructorMethod: TRttiMethod; const arguments: TArray<TValue>): TObject;
begin
  Result := classType.NewInstance;
  try
    constructorMethod.Invoke(Result, arguments);
    Result.AfterConstruction;
  except
    on Exception do
    begin
      Result.Destroy;
      raise;
    end;
  end;
end;

{$ENDREGION}

end.
