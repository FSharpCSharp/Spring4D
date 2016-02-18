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

unit Spring.Container.AutoMockExtension;

interface

uses
  Spring.Container.Core;

type
  TAutoMockExtension = class(TInterfacedObject, IContainerExtension)
  public
    procedure Initialize(const kernel: TKernel);
  end;

implementation

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Container.Common,
  Spring.Container.ComponentActivator,
  Spring.Reflection,
  Spring.Mocking,
  Spring.Mocking.Core;

type
  TAutoMockResolver = class(TInterfacedObject, IResolver)
  private
    fKernel: TKernel;
    procedure EnsureMockRegistered(const mockedType: TRttiType);
    class function TryGetMockedType(const targetType: TRttiType;
      out mockedType: TRttiType): Boolean; static;
  public
    constructor Create(const kernel: TKernel);

    function CanResolve(const request: IRequest): Boolean;
    function Resolve(const request: IRequest): TValue;
  end;


{$REGION 'TAutoMockExtension'}

procedure TAutoMockExtension.Initialize(const kernel: TKernel);
begin
  kernel.Resolver.AddResolver(TAutoMockResolver.Create(kernel));
end;

{$ENDREGION}


{$REGION 'TAutoMockResolver'}

constructor TAutoMockResolver.Create(const kernel: TKernel);
begin
  inherited Create;
  fKernel := kernel;
end;

function TAutoMockResolver.CanResolve(const request: IRequest): Boolean;
var
  serviceType: TRttiType;
  mockedType: TRttiType;
  argument: TValue;
begin
  serviceType := request.Service.RttiType;
  if serviceType.IsGenericType
    and TryGetMockedType(serviceType, mockedType)
    and mockedType.IsInterface and not mockedType.IsType(TypeInfo(IInterface)) then
    Exit(True);

  argument := request.Parameter;
  if serviceType.IsInterface and not IsLazyType(serviceType.Handle) then
    if argument.IsEmpty then
      Exit(not fKernel.Registry.HasService(serviceType.Handle))
    else
      if argument.IsString then
        Exit(not fKernel.Registry.HasService(serviceType.Handle, argument.AsString));

  Result := False;
end;

procedure TAutoMockResolver.EnsureMockRegistered(const mockedType: TRttiType);
var
  mockName: string;
  mockModel: TComponentModel;
begin
  mockName := 'IMock<' + mockedType.DefaultName + '>';
  if not fKernel.Registry.HasService(mockName) then
  begin
    // only for interfaces
    mockModel := fKernel.Registry.RegisterComponent(TMock<IInterface>.ClassInfo);
    fKernel.Registry.RegisterService(mockModel, TypeInfo(IMock<IInterface>), mockName);
    mockModel.ComponentActivator := TDelegateComponentActivator.Create(fKernel, mockModel,
      function: TValue
      var
        mock: TMock;
      begin
        mock := TMock<IInterface>.NewInstance as TMock;
        mock.Create(mockedType.Handle);
        Result := mock;
      end);
    mockModel.LifetimeType := TLifetimeType.Singleton;
    fKernel.Builder.Build(mockModel);
  end;
end;

function TAutoMockResolver.Resolve(const request: IRequest): TValue;
var
  serviceType: TRttiType;
  mockDirectly: Boolean;
  mockedType: TRttiType;
  mockName: string;
begin
  serviceType := request.Service.RttiType;
  mockDirectly := serviceType.IsGenericType
    and TryGetMockedType(serviceType, mockedType);
  if not mockDirectly then
    mockedType := serviceType;
  mockName := 'IMock<' + mockedType.DefaultName + '>';
  EnsureMockRegistered(mockedType);
  Result := (fKernel as IKernelInternal).Resolve(mockName);
  if mockDirectly then
  begin
    TValueData(Result).FTypeInfo := serviceType.Handle;
    Exit;
  end
  else
    Result := (Result.AsType<IMock<IInterface>> as IMock).Instance;
end;

class function TAutoMockResolver.TryGetMockedType(const targetType: TRttiType;
  out mockedType: TRttiType): Boolean;
begin
  if SameText(targetType.GetGenericTypeDefinition, 'IMock<>') then
    mockedType := targetType.GetMethod('GetInstance').ReturnType
  else if SameText(targetType.GetGenericTypeDefinition, 'Mock<>') then
    mockedType := targetType.GetField('fMock').FieldType.GetMethod('GetInstance').ReturnType
  else
    Exit(False);
  Result := True;
end;

{$ENDREGION}


end.
