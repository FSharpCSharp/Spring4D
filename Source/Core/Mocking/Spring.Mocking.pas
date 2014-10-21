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

unit Spring.Mocking;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Interception;

{$SCOPEDENUMS ON}

type
  TMockState = (Act, Arrange, Assert);
  TMockBehavior = (Dynamic, Strict);

  TValue = Rtti.TValue;

  TCallInfo = record
  private
    fInvocation: IInvocation;
    fCallCount: Integer;
    function GetMethod: TRttiMethod;
    function GetArguments: TArray<TValue>;
    function GetCallCount: Integer;
  public
    constructor Create(const invocation: IInvocation; callCount: Integer);
    property Arguments: TArray<TValue> read GetArguments;
    property CallCount: Integer read GetCallCount;
    property Method: TRttiMethod read GetMethod;
  end;

  TMockAction = reference to function(const callInfo: TCallInfo): TValue;

  IWhen = interface(IInvokable)
    ['{DAD0B65B-8CEF-4513-ADE8-38E8F9AAFA9A}']
    procedure When;
    procedure WhenForAnyArgs;
  end;

  ISetup = interface(IInvokable)
    ['{0BC12D48-41FF-46D0-93B3-773EE19D75ED}']
    function Executes: IWhen; overload;
    function Executes(const action: TProc): IWhen; overload;
    function Executes(const action: TAction<TCallInfo>): IWhen; overload;

    function Raises(const exceptionClass: ExceptClass;
      const msg: string = ''): IWhen; overload;
    function Raises(const exceptionClass: ExceptClass;
      const msg: string; const args: array of const): IWhen; overload;

    function Returns(const value: TValue): IWhen; overload;
    function Returns(const values: array of TValue): IWhen; overload;
    function Returns(const action: TFunc<TValue>): IWhen; overload;
    function Returns(const actions: array of TFunc<TValue>): IWhen; overload;
    function Returns(const action: TMockAction): IWhen; overload;
    function Returns(const actions: array of TMockAction): IWhen; overload;
  end;

  IMock = interface(IInvokable)
    ['{7D386664-22CF-4555-B03E-61319C39BC12}']
    function GetInstance: TValue;
    function GetTypeInfo: PTypeInfo;

    function Setup: ISetup;

    procedure Received; overload;
    procedure Received(const times: Times); overload;
    procedure ReceivedWithAnyArgs; overload;
    procedure ReceivedWithAnyArgs(const times: Times); overload;

    property Instance: TValue read GetInstance;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  IWhen<T> = interface(IInvokable)
    ['{4162918E-4DE6-47D6-B609-D5A17F3FBE2B}']
    function When: T;
    function WhenForAnyArgs: T;
  end;

  ISetup<T> = interface(IInvokable)
    ['{CD661866-EB29-400C-ABC8-19FC8D59FFAD}']
    function Executes: IWhen<T>; overload;
    function Executes(const action: TProc): IWhen<T>; overload;
    function Executes(const action: TAction<TCallInfo>): IWhen<T>; overload;

    function Raises(const exceptionClass: ExceptClass;
      const msg: string = ''): IWhen<T>; overload;
    function Raises(const exceptionClass: ExceptClass;
      const msg: string; const args: array of const): IWhen<T>; overload;

    function Returns(const value: TValue): IWhen<T>; overload;
    function Returns(const values: array of TValue): IWhen<T>; overload;
    function Returns(const action: TFunc<TValue>): IWhen<T>; overload;
    function Returns(const actions: array of TFunc<TValue>): IWhen<T>; overload;
    function Returns(const action: TMockAction): IWhen<T>; overload;
    function Returns(const actions: array of TMockAction): IWhen<T>; overload;
  end;

  IMock<T> = interface(IInvokable)
    ['{67AD5AD2-1C23-41BA-8F5D-5C28B3C7ABF7}']
    function GetInstance: T;
    function GetTypeInfo: PTypeInfo;

    function Setup: ISetup<T>;

    function Received: T; overload;
    function Received(const times: Times): T; overload;
    function ReceivedWithAnyArgs: T; overload;
    function ReceivedWithAnyArgs(const times: Times): T; overload;

    property Instance: T read GetInstance;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  EMockException = class(Exception);

  TMethodCall = class
  private
    fAction: TMockAction;
    fArguments: Nullable<TArray<TValue>>;
    fCallCount: Integer;
  public
    constructor Create(const action: TMockAction;
      const arguments: Nullable<TArray<TValue>>);

    function Invoke(const invocation: IInvocation): TValue;

    property Arguments: Nullable<TArray<TValue>> read fArguments;
    property CallCount: Integer read fCallCount;
  end;

  TMocks = record
  public
    class function Create<T>(
      behavior: TMockBehavior = TMockBehavior.Dynamic): IMock<T>; static;
    class function From<T: IInterface>(const value: T): IMock<T>; static;
  end;

implementation

uses
  Spring.Helpers,
  Spring.Mocking.Core,
  Spring.Mocking.Interceptor;


{$REGION 'TCallInfo'}

constructor TCallInfo.Create(const invocation: IInvocation; callCount: Integer);
begin
  fInvocation := invocation;
  fCallCount := callCount;
end;

function TCallInfo.GetArguments: TArray<TValue>;
begin
  Result := fInvocation.Arguments;
end;

function TCallInfo.GetCallCount: Integer;
begin
  Result := fCallCount;
end;

function TCallInfo.GetMethod: TRttiMethod;
begin
  Result := fInvocation.Method;
end;

{$ENDREGION}


{$REGION 'TMethodCall'}

constructor TMethodCall.Create(const action: TMockAction;
  const arguments: Nullable<TArray<TValue>>);
begin
  inherited Create;
  fAction := action;
  fArguments := arguments;
end;

function TMethodCall.Invoke(const invocation: IInvocation): TValue;
begin
  Inc(fCallCount);
  Result := fAction(TCallInfo.Create(invocation, fCallCount));
  if invocation.Method.MethodKind = mkFunction then
    Result := Result.Cast(invocation.Method.ReturnType.Handle);
end;

{$ENDREGION}


{$REGION 'TMocks'}

class function TMocks.Create<T>(behavior: TMockBehavior): IMock<T>;
begin
  Result := TMock<T>.Create(behavior);
end;

class function TMocks.From<T>(const value: T): IMock<T>;
var
  accessor: IProxyTargetAccessor;
  proxy: TValue;
  mock: TMock;
begin
  Assert(Supports(value, IProxyTargetAccessor, accessor));
  TValue.Make(@value, System.TypeInfo(T), proxy);
  mock := TMock<T>.NewInstance as TMock;
  mock.Create(TypeInfo(T), accessor.GetInterceptors.First(
    function(const interceptor: IInterceptor): Boolean
    begin
      Result := (interceptor as TObject) is TMockInterceptor
    end) as TMockInterceptor, proxy);
  Result := mock as IMock<T>;
end;

{$ENDREGION}


end.
