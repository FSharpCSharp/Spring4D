{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

unit Spring.Reactive.Tests.AsyncLock;

interface

uses
  TestFramework,
  Spring.TestUtils,
  Spring.Reactive.Concurrency.AsyncLock;

type
  AsyncLockTest = class(TTestCase)
  private
    lock: TAsyncLock;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Wait_ArgumentChecking;
    procedure Wait_Graceful;
    procedure Wait_Fail;
    procedure Wait_QueuesWork;

    procedure Dispose;
  end;

implementation

uses
  Spring,
  SysUtils;

{ AsyncLockTest }

procedure AsyncLockTest.SetUp;
begin
  lock := TAsyncLock.Create;
end;

procedure AsyncLockTest.TearDown;
begin
  lock.Free;
end;

procedure AsyncLockTest.Wait_ArgumentChecking;
begin
  CheckException(EArgumentNullException, procedure begin lock.Wait(nil) end);
end;

procedure AsyncLockTest.Wait_Fail;
var
  ok: Boolean;
begin
  ok := False;
  lock.Wait(procedure begin ok := True end);
  CheckTrue(ok);
end;

procedure AsyncLockTest.Wait_Graceful;
var
  ex: Exception;
begin
  ex := Exception.Create('');
  try
    lock.Wait(procedure begin raise ex end);
    CheckTrue(False);
  except
    on e: Exception do
      CheckSame(ex, e);
  end;

  lock.Wait(procedure begin CheckTrue(False) end);
end;

procedure AsyncLockTest.Wait_QueuesWork;
var
  l1, l2: Boolean;
begin
  l1 := False;
  l2 := False;

  lock.Wait(procedure begin lock.Wait(procedure begin CheckTrue(l1); l2 := True end); l1 := True end);
  CheckTrue(l2);
end;

procedure AsyncLockTest.Dispose;
var
  l1, l2, l3, l4: Boolean;
begin
  l1 := False;
  l2 := False;
  l3 := False;
  l4 := False;

  lock.Wait(
    procedure
    begin
      lock.Wait(
        procedure
        begin
          lock.Wait(
            procedure
            begin
               l3 := True;
            end);

          l2 := True;

          lock.Dispose;

          lock.Wait(
            procedure
            begin
              l4 := True;
            end);
        end);

      l1 := True;
    end);

  CheckTrue(l1);
  CheckTrue(l2);
  CheckFalse(l3);
  CheckFalse(l4);
end;

initialization
  RegisterTest(AsyncLockTest.Suite);

end.
