{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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
  Classes,
  SysUtils,
  Forms,
  Windows,
  TestFramework,
  TestExtensions,
  GUITestRunner,
  TextTestRunner,
  Spring.Tests.DI.Components in 'Source\Core\Spring.Tests.DI.Components.pas',
  Spring.Tests.DI.LifetimeManager in 'Source\Core\Spring.Tests.DI.LifetimeManager.pas',
  Spring.Tests.DI in 'Source\Core\Spring.Tests.DI.pas',
  Spring.Tests.Pool in 'Source\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in 'Source\System\Spring.Tests.Cryptography.pas',
  Spring.Tests.DesignPatterns in 'Source\System\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in 'Source\System\Spring.Tests.Helpers.pas',
  Spring.Tests.System in 'Source\System\Spring.Tests.System.pas',
  Spring.Tests.Utils in 'Source\System\Spring.Tests.Utils.pas',
  Spring.Tests.ValueConverters in 'Source\System\Spring.Tests.ValueConverters.pas';

{$R *.RES}

procedure RegisterTestCases;
begin
  RegisterTests('Base.System', [
    TTestSplitString.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestTryConvertStrToDateTime.Suite,
    TTestVersion.Suite,
    TTestEnum.Suite,
    TTestBuffer.Suite,
    TTestEmptyBuffer.Suite,
    TTestFiveByteBuffer.Suite,
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3),
    TTestDelegate.Suite
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

//  RegisterTests('Base.Cloning', [
//    TTestCloning.Suite,
//    TTestCloneableObject.Suite
//  ]);

  RegisterTests('Base.ValueConverters', [
    TTestFromString.Suite,
    TTestFromWideString.Suite,
    TTestFromInteger.Suite,
    TTestFromSmallInt.Suite,
    TTestFromShortInt.Suite,
    TTestFromLongInt.Suite,
    TTestFromBoolean.Suite,
    TTestFromEnum.Suite,
    TTestFromFloat.Suite,
    TTestFromColor.Suite,
    TTestFromCurrency.Suite,
    TTestFromDateTime.Suite,
    TTestFromObject.Suite,
    TTestFromNullable.Suite,
    TTestFromInterface.Suite
  ]);

  RegisterTests('Core.Pool', [
    TTestObjectPool.Suite
  ]);

  RegisterTests('Core.Cryptography', [
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
//    TTestMACTripleDES.Suite
  ]);

  RegisterTests('Core.DI', [
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

//  RegisterTests('Core.Numerics', [
//    TTestHalf.Suite,
//    TTestBigCardinal.Suite,
//    TTestBigInteger.Suite,
//    TTestBigDecimal.Suite
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

