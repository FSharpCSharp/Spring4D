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

{$I Spring.inc}

unit Spring.Container.ProxyFactory;

interface

uses
  Spring,
  Spring.Container.Core,
  Spring.Interception;

type
  TProxyFactory = class(TInterfacedObject, IProxyFactory)
  private
    fKernel: IKernel;
    fGenerator: TProxyGenerator;
  public
    constructor Create(const kernel: IKernel);
    destructor Destroy; override;

    function CreateInstance(const context: ICreationContext;
      const instance: TValue; const model: TComponentModel;
      const constructorArguments: array of TValue): TValue;
  end;

implementation


{$REGION 'TProxyFactory'}

constructor TProxyFactory.Create(const kernel: IKernel);
begin
  inherited Create;
  fKernel := kernel;
  fGenerator := TProxyGenerator.Create;
end;

destructor TProxyFactory.Destroy;
begin
  fGenerator.Free;
  inherited;
end;

function TProxyFactory.CreateInstance(const context: ICreationContext;
  const instance: TValue; const model: TComponentModel;
  const constructorArguments: array of TValue): TValue;
var
  interceptors: TArray<IInterceptor>;
  i: Integer;
  interceptorRef: TInterceptorReference;
  interceptor: TValue;
begin
  if model.Interceptors.Any then
  begin
    SetLength(interceptors, model.Interceptors.Count);
    for i := Low(interceptors) to High(interceptors) do
    begin
      interceptorRef := model.Interceptors[i];
      if Assigned(interceptorRef.Key) then
        interceptor := (fKernel as IKernelInternal).Resolve(interceptorRef.Key)
      else
        interceptor := (fKernel as IKernelInternal).Resolve(interceptorRef.Value);
      interceptors[i] := interceptor.AsInterface as IInterceptor;
    end;
    Result := fGenerator.CreateInterfaceProxyWithTarget(instance.TypeInfo,
      instance.AsInterface, interceptors);
  end
  else
    Result := instance;
end;

{$ENDREGION}


end.
