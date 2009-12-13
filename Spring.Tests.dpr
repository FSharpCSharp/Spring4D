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

program Spring.Tests;

{.$DEFINE CONSOLE_TESTRUNNER}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  Classes,
  SysUtils,
  Forms,
  Windows,
  TestFramework,
  TestExtensions,
  GUITestRunner,
  TextTestRunner,
  Spring.Tests.DesignPatterns in 'Tests\Base\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in 'Tests\Base\Spring.Tests.Helpers.pas',
  Spring.Tests.System in 'Tests\Base\Spring.Tests.System.pas',
  Spring.Tests.Numbering in 'Tests\Core\Spring.Tests.Numbering.pas',
  Spring.Tests.IoC in 'Tests\Core\Spring.Tests.IoC.pas',
  Spring.Tests.IoC.LifetimeManager in 'Tests\Core\Spring.Tests.IoC.LifetimeManager.pas',
  Spring.Tests.IoC.Components in 'Tests\Core\Spring.Tests.IoC.Components.pas',
  Spring.Tests.Utils in 'Tests\Base\Spring.Tests.Utils.pas',
  Spring.Tests.Pool in 'Tests\Core\Spring.Tests.Pool.pas';

{$R *.RES}

procedure RegisterTestCases;
begin
  RegisterTests('Base.System', [
    TTestSplitString.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestTryParseDateTime.Suite,
    TTestVersion.Suite,
    TTestEnum.Suite,
    TTestBuffer.Suite,
    TTestEmptyBuffer.Suite,
    TTestFiveByteBuffer.Suite,
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3)
  ]);

  RegisterTests('Base.DesignPatterns', [
    TTestSingleton.Suite
  ]);

  RegisterTests('Base.Utils', [
    TTestDecimalCalculator.Suite,
    TTestHexCalculator.Suite,
    TTestBaseNineCalculator.Suite
  ]);

  RegisterTests('Base.Helpers', [
    TTestGuidHelper.Suite
  ]);

//  RegisterTests('Base.Cryptography', [
//    TTestMD5.Suite,
//    TTestDES.Suite,
//    TTestTripleDES.Suite
//  ]);

  RegisterTests('Core.Pool', [
    TTestObjectPool.Suite
  ]);

  RegisterTests('Core.IoC', [
    TTestSingletonLifetimeManager.Suite,
    TTestTransientLifetimeManager.Suite,
    TTestEmptyContainer.Suite,
    TTestSimpleContainer.Suite,
    TTestDifferentServiceImplementations.Suite,
    TTestImplementsDifferentServices.Suite,
    TTestRegisterInterfaces.Suite,
    TTestActivatorDelegate.Suite,
    TTestTypedInjectionByCoding.Suite,
    TTestTypedInjectionsByAttribute.Suite,
    TTestNamedInjectionsByCoding.Suite,
    TTestNamedInjectionsByAttribute.Suite,
    TTestDirectCircularDependency.Suite,
    TTestCrossedCircularDependency.Suite,
    TTestImplementsAttribute.Suite
  ]);

  RegisterTests('Core.Numbering', [
    TTestNumberRuleBuilder.Suite
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

