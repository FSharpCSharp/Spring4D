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

program SpringFrameworkTests;

{.$DEFINE CONSOLE_TESTRUNNER}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  Classes,
  Forms,
  Windows,
  SysUtils,
  TestFramework,
  TestExtensions,
  GUITestRunner,
  TextTestRunner,
  Spring.System,
  Spring.Tests.System in 'Tests\Spring.Tests.System.pas',
  Spring.Tests.Helpers in 'Tests\Spring.Tests.Helpers.pas',
  Spring.Tests.IoC in 'Tests\Spring.Tests.IoC.pas',
  Spring.Tests.DesignPatterns in 'Tests\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.IoC.LifetimeManager in 'Tests\Spring.Tests.IoC.LifetimeManager.pas';

{$R *.RES}

procedure RegisterTestCases;
begin
  RegisterTests('Spring System Tests', [
    TTestSplitString.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestVersion.Suite,
    TTestEnum.Suite,
    TTestBuffer.Suite,
    TTestEmptyBuffer.Suite,
    TTestFiveBytesBuffer.Suite,
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3)
  ]);

//  RegisterTests('Spring Cryptography Tests', [
//    TTestMD5.Suite,
//    TTestDES.Suite,
//    TTestTripleDES.Suite
//  ]);

  RegisterTests('Spring DesignPatterns Tests', [
    TTestSingleton.Suite
  ]);

  RegisterTests('Spring Helpers Tests', [
    TTestGuidHelper.Suite
  ]);

  RegisterTests('Spring IoC Tests', [
    TTestSingletonLifetimeManager.Suite,
    TTestTransientLifetimeManager.Suite,
    TTestContainer.Suite
  ]);
end;

begin
  Application.Initialize;
  RegisterTestCases;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

