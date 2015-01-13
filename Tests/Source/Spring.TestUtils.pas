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

unit Spring.TestUtils;

{$I Spring.Tests.inc}

interface

uses
  Classes,
  IniFiles,
  SysUtils,
  TestFramework;

type
  TAbstractTestHelper = class helper for TAbstractTest
  public
    procedure CheckException(expected: ExceptionClass; method: TProc; const msg: string = '');
  end;

procedure ProcessTestResult(const ATestResult: TTestResult);

implementation

procedure ProcessTestResult(const ATestResult: TTestResult);
begin
{$IFNDEF AUTOREFCOUNT}
  ATestResult.Free;
{$ENDIF}
end;

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer; inline;
begin
  Result := CallerAddr;
end;
{$ENDIF}

{$REGION 'TAbstractTestHelper'}

procedure TAbstractTestHelper.CheckException(
  expected: ExceptionClass; method: TProc; const msg: string);
begin
  FCheckCalled := True;
  try
    method;
  except
    on E: Exception do
    begin
      if not Assigned(expected) then
        raise
      else if not E.InheritsFrom(expected) then
        FailNotEquals(expected.ClassName, E.ClassName, msg, ReturnAddress)
      else
        expected := nil;
    end;
  end;
  if Assigned(expected) then
    FailNotEquals(expected.ClassName, 'nothing', msg, ReturnAddress);
end;

{$ENDREGION}


end.
