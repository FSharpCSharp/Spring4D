{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2015 Spring4D Team                           }
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

unit TestAdaptersADO;

interface

uses
  SysUtils,
  DB,
  ADOInt,
  ADODB,
  ComObj,
  TestFramework,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Mocking,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Adapters.ADO,
  Spring.Persistence.SQL.Interfaces,
  Spring.TestUtils,
  TestMockADOConnection;

type
  IADOExecuteCall = interface
  end;

  TBaseADOAdapterTest = class(TTestCase)
  protected
    fMockConnectionObject: Mock<TMockADOConnection>;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure SetupOpen;
    function SetupExecute(const queries: array of string): IADOExecuteCall;
  end;

  TBaseADOConnectionAdapterTest = class(TBaseADOAdapterTest)
  protected
    fAdapter: TADOConnectionAdapter;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TADOExceptionHandlerAccess = class(TADOExceptionHandler);
  TADOExceptionHandlerTest = class(TTestCase<TADOExceptionHandlerAccess>)
  published
    procedure TestGetAdapterException_EDatabaseError;
    procedure TestGetAdapterException_ESafecallException;
    procedure TestGetAdapterException_EOleSysError;
    procedure TestGetAdapterException_Others_Return_Nil;
  end;

  TADOConnectionAdapterTest = class(TBaseADOConnectionAdapterTest)
  published
    procedure TestIsConnected;
    procedure TestConnect;
    procedure TestDisconnect;
    procedure TestCreateStatement;
    procedure TestBeginTransaction;
  end;

  TADOTransactionAdapterTest = class(TBaseADOConnectionAdapterTest)
  protected
    procedure SetUp; override;
  published
    procedure TestCommit;
    procedure TestRollback;
  end;

implementation

uses
  TestEntities,
  TestExceptions;


{$REGION 'TADOExecuteCall'}

type
  TADOExecuteCall = class(TInterfacedObject, IADOExecuteCall)
  private
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    fTestCase: TTestCase;
    fQueries: IList<string>;
    fCurrentCallIndex: Integer;
  public
    constructor Create(const testCase: TTestCase;
      const queries: array of string);
    function Execute(const callInfo: TCallInfo): TValue;
  end;

constructor TADOExecuteCall.Create(const testCase: TTestCase;
  const queries: array of string);
begin
  inherited Create;
  fTestCase := testCase;
  fQueries := TCollections.CreateList<string>(queries);
  fCurrentCallIndex := -1
end;

function TADOExecuteCall.Execute(const callInfo: TCallInfo): TValue;
var
  query: string;
begin
  Inc(fCurrentCallIndex);
  query := callInfo.Arguments[0].ToString;
  try
    if fCurrentCallIndex < fQueries.Count then
      fTestCase.CheckEquals(fQueries[fCurrentCallIndex], query,
        Format('Invalid query, index: %d', [fCurrentCallIndex]))
    else
      fTestCase.Fail(Format('Invalid query, no query expected index: %d query: %s',
        [fCurrentCallIndex, query]));
  except
    // Save status in case EOleException gets raised with undescriptive error
    on E: ETestFailure do
    begin
      fTestCase.Status(E.Message);
      raise;
    end
    else raise;
  end;
end;

{$ENDREGION}


{$REGION 'TBaseADOAdapterTest'}

procedure TBaseADOAdapterTest.SetUp;
begin
  inherited;
  fMockConnectionObject := Mock<TMockADOConnection>.Create(TMockBehavior.Strict);
end;

function TBaseADOAdapterTest.SetupExecute(const queries: array of string): IADOExecuteCall;
var
  lResult: TADOExecuteCall;
  o: OleVariant;
begin
  lResult := TADOExecuteCall.Create(Self, queries);
  Result := lResult;
  fMockConnectionObject.Setup.Executes(lResult.Execute).WhenForAnyArgs
    .Execute('', o, 0);
end;

procedure TBaseADOAdapterTest.SetupOpen;
begin
  with fMockConnectionObject.Setup.Executes do
  begin
    WhenForAnyArgs.Get_ConnectionString;
    WhenForAnyArgs.Get_State;
    WhenForAnyArgs.Open('', '', '', 0);
  end;
end;

procedure TBaseADOAdapterTest.TearDown;
begin
  inherited;
  fMockConnectionObject.Free;
end;

{$ENDREGION}


{$REGION 'TBaseADOConnectionAdapterTest'}

procedure TBaseADOConnectionAdapterTest.SetUp;
begin
  inherited;
  fAdapter := TADOConnectionAdapter.Create(TADOConnection.Create(nil));
  fAdapter.AutoFreeConnection := True;
end;

procedure TBaseADOConnectionAdapterTest.TearDown;
begin
  // Ensure the mock alows the adapter to free properly
  SetupOpen;
  fAdapter.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TADOConnectionAdapterTest'}

procedure TADOConnectionAdapterTest.TestBeginTransaction;
var
  transaction: IDBTransaction;
begin
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;

  // Test connect exception
  CheckException(EADOAdapterException,
    procedure begin fAdapter.BeginTransaction end);

  SetupOpen;
  // Test BeginTrans exception
  CheckException(EADOAdapterException,
    procedure begin fAdapter.BeginTransaction end);

  fMockConnectionObject.Setup.Executes.WhenForAnyArgs.BeginTrans;
  transaction := fAdapter.BeginTransaction;
  CheckNotNull(transaction);
end;

procedure TADOConnectionAdapterTest.TestConnect;
begin
  CheckException(EADOAdapterException,
    procedure begin fAdapter.Connect end);

  SetupOpen;
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;
  fAdapter.Connect;

  Pass;
end;

procedure TADOConnectionAdapterTest.TestCreateStatement;
var
  statement: IDBStatement;
begin
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;

  statement := fAdapter.CreateStatement;
  CheckNotNull(statement);
end;

procedure TADOConnectionAdapterTest.TestDisconnect;
begin
  fMockConnectionObject.Setup.Returns([adStateConnecting, adStateConnecting,
    adStateOpen]).WhenForAnyArgs.Get_State;
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;

  CheckException(EADOAdapterException,
    procedure begin fAdapter.Disconnect end);

  SetupOpen;
  fAdapter.Disconnect;

  Pass;
end;

procedure TADOConnectionAdapterTest.TestIsConnected;
begin
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;
  CheckException(EADOAdapterException,
    procedure begin fAdapter.IsConnected end);

  fMockConnectionObject.Setup.Returns([adStateConnecting, adStateConnecting,
    adStateOpen, adStateOpen]).WhenForAnyArgs.Get_State;
  CheckTrue(fAdapter.IsConnected);

  fMockConnectionObject.Setup.Returns([adStateConnecting, adStateConnecting,
    adStateClosed, adStateClosed]).WhenForAnyArgs.Get_State;
  CheckFalse(fAdapter.IsConnected);
end;

{$ENDREGION}


{$REGION 'TADOTransactionAdapterTest'}

procedure TADOTransactionAdapterTest.SetUp;
begin
  inherited;
  fAdapter.Connection.ConnectionObject := fMockConnectionObject;
end;

procedure TADOTransactionAdapterTest.TestCommit;
begin
  SetupOpen;
  with fMockConnectionObject.Setup.Executes do
  begin
    When.BeginTrans;
    When.CommitTrans;
  end;
  with fAdapter.BeginTransaction do
  begin
    Commit;
    fMockConnectionObject.Setup.Raises<EOleException>.When.CommitTrans;
    ExpectedException := EADOAdapterException;
    Commit;
  end;
end;

procedure TADOTransactionAdapterTest.TestRollback;
begin
  SetupOpen;
  with fMockConnectionObject.Setup.Executes do
  begin
    When.BeginTrans;
    When.RollbackTrans;
  end;
  with fAdapter.BeginTransaction do
  begin
    Rollback;
    fMockConnectionObject.Setup.Raises<EOleException>.When.RollbackTrans;
    ExpectedException := EADOAdapterException;
    Rollback;
  end;
end;

{$ENDREGION}


{$REGION 'TADOExceptionHandlerTest'}

procedure TADOExceptionHandlerTest.TestGetAdapterException_EDatabaseError;
var
  exc, result: Managed<Exception>;
begin
  exc := EDatabaseError.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EADOAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EADOAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TADOExceptionHandlerTest.TestGetAdapterException_EOleSysError;
var
  exc, result: Managed<Exception>;
begin
  exc := EOleException.Create('', -1, '', '', 0);
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EADOAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(-1, EADOAdapterException(result.Value).ErrorCode);
end;

procedure TADOExceptionHandlerTest.TestGetAdapterException_ESafecallException;
var
  exc, result: Managed<Exception>;
begin
  exc := ESafecallException.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EADOAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EADOAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TADOExceptionHandlerTest.TestGetAdapterException_Others_Return_Nil;
var
  exc, result: Managed<Exception>;
begin
  exc := Exception.Create('');
  result := SUT.GetAdapterException(exc, '');
  CheckNull(result);
end;

{$ENDREGION}


initialization
  RegisterTests('Spring.Persistence.Adapters', [
    TADOConnectionAdapterTest.Suite,
    TADOTransactionAdapterTest.Suite,
    TADOExceptionHandlerTest.Suite]);

end.
