{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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

program Spring.Tests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Classes,
  Forms,
  Windows,
  TestFramework,
  TestExtensions,
  GUITestRunner,
  TextTestRunner,
  Spring.Tests.Collections in '..\..\Tests\System\Spring.Tests.Collections.pas',
  Spring.Tests.DesignPatterns in '..\..\Tests\System\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in '..\..\Tests\System\Spring.Tests.Helpers.pas',
  Spring.Tests.Reflection.ValueConverters in '..\..\Tests\System\Spring.Tests.Reflection.ValueConverters.pas',
  Spring.Tests.System in '..\..\Tests\System\Spring.Tests.System.pas',
  Spring.Tests.SysUtils in '..\..\Tests\System\Spring.Tests.SysUtils.pas',
  Spring.Tests.Container.Components in '..\..\Tests\Core\Spring.Tests.Container.Components.pas',
  Spring.Tests.Container.LifetimeManager in '..\..\Tests\Core\Spring.Tests.Container.LifetimeManager.pas',
  Spring.Tests.Container in '..\..\Tests\Core\Spring.Tests.Container.pas',
  Spring.Tests.Pool in '..\..\Tests\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in '..\..\Tests\Extensions\Spring.Tests.Cryptography.pas',
  Spring.Tests.Utils in '..\..\Tests\Extensions\Spring.Tests.Utils.pas';

procedure RegisterTestCases;
begin
  RegisterTests('Spring.System', [
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3),
    TTestLazy.Suite,
    TTestEmptyMulticastEvent.Suite
  ]);

  RegisterTests('Spring.System.SysUtils', [
    TTestSplitString.Suite,
    TTestTryConvertStrToDateTime.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestEnum.Suite
  ]);

  RegisterTests('Spring.System.DesignPatterns', [
    TTestSingleton.Suite
  ]);

  RegisterTests('Spring.System.Helpers', [
    TTestGuidHelper.Suite
  ]);

  RegisterTests('Spring.System.Reflection.ValueConverters', [
    TTestFromString.Suite,
    TTestFromWideString.Suite,
    TTestFromInteger.Suite,
    TTestFromCardinal.Suite,
    TTestFromSmallInt.Suite,
    TTestFromShortInt.Suite,
    TTestFromBoolean.Suite,
    TTestFromEnum.Suite,
    TTestFromFloat.Suite,
    TTestFromColor.Suite,
    TTestFromCurrency.Suite,
    TTestFromDateTime.Suite,
    TTestFromObject.Suite,
    TTestFromNullable.Suite,
    TTestFromInterface.Suite,
    TTestCustomTypes.Suite
  ]);

//  RegisterTests('Spring.System.Reflection.ValueExpression', [
//    TTestValueExpression.Suite
//  ]);

  RegisterTests('Spring.Core.Container', [
    TTestEmptyContainer.Suite,
    TTestSimpleContainer.Suite,
    TTestDifferentServiceImplementations.Suite,
    TTestImplementsDifferentServices.Suite,
    TTestActivatorDelegate.Suite,
    TTestTypedInjectionByCoding.Suite,
    TTestTypedInjectionsByAttribute.Suite,
    TTestNamedInjectionsByCoding.Suite,
    TTestNamedInjectionsByAttribute.Suite,
    TTestDirectCircularDependency.Suite,
    TTestCrossedCircularDependency.Suite,
    TTestImplementsAttribute.Suite,
    TTestRegisterInterfaces.Suite,
    TTestSingletonLifetimeManager.Suite,
    TTestTransientLifetimeManager.Suite,
    TTestDefaultResolve.Suite,
    TTestInjectionByValue.Suite,
    TTestObjectPool.Suite
  ]);

  RegisterTests('Spring.Extensions.Utils', [
    TTestVersion.Suite
  ]);

  RegisterTests('Spring.Extensions.Cryptography', [
//    TTestBuffer.Suite,
//    TTestEmptyBuffer.Suite,
//    TTestFiveByteBuffer.Suite,
    TTestCRC16.Suite,
    TTestCRC32.Suite,
    TTestMD5.Suite,
    TTestSHA1.Suite,
    TTestSHA256.Suite,
    TTestSHA384.Suite,
    TTestSHA512.Suite,
    TTestPaddingModeIsNone.Suite,
    TTestPaddingModeIsPKCS7.Suite,
    TTestPaddingModeIsZeros.Suite,
    TTestPaddingModeIsANSIX923.Suite,
    TTestPaddingModeIsISO10126.Suite,
    TTestDES.Suite,
    TTestTripleDES.Suite
  ]);

// Stefan Glienke - 2011/11/20:
// removed configuration and logging tests because they break other tests in Delphi 2010
// due to some bug in Rtti.TRttiPackage.MakeTypeLookupTable
// see https://forums.embarcadero.com/thread.jspa?threadID=54471
//
//  RegisterTests('Spring.Core.Configuration', [
//    TTestConfiguration.Suite
//  ]);
//
//  RegisterTests('Spring.Core.Logging', [
//     TTestLoggingConfig.Suite
//  ]);
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;

  RegisterTestCases;

  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

