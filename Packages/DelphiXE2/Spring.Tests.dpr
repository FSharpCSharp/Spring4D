{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2012 Spring4D Team                           }
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

/// <summary>
/// Represents the Unit Tests of Spring Enterprise Library for Delphi.
/// </summary>
/// <remarks>
/// The compiler directive {$STRONGLINKTYPES ON} has been introduced since Delphi 2010. By enable this option,
/// We use the Extended RTTI to register all test cases instead of register them manually.
/// </remarks>
program Spring.Tests;


(*
  Copied from the RAD Studio Documentation:
  {$STRONGLINKTYPES ON} links in with strong fixups (rather than weak fixups) all types
  in the EXE or DLL's root type table. The root type table is the index that lets the RTTI unit map qualified names to types.
  The smart linker eliminates symbols (including the RTTI associated with types) that only have weak fixups referencing them.
  If one or more strong fixups references the symbol, then it is included in the final binary,
  and the weak references do not get set to @PointerToNil.
*)
{$STRONGLINKTYPES OFF}

{.$DEFINE CONSOLE_TESTRUNNER}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Classes,
  SysUtils,
  Forms,
  Windows,
  Rtti,
  TestFramework,
  TestExtensions,
  GUITestRunner,
  TextTestRunner,
  Spring.Tests.Container.Components in '..\..\Tests\Core\Spring.Tests.Container.Components.pas',
  Spring.Tests.Container.LifetimeManager in '..\..\Tests\Core\Spring.Tests.Container.LifetimeManager.pas',
  Spring.Tests.Container in '..\..\Tests\Core\Spring.Tests.Container.pas',
  Spring.Tests.Pool in '..\..\Tests\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in '..\..\Tests\Extensions\Spring.Tests.Cryptography.pas',
  Spring.Tests.Utils in '..\..\Tests\Extensions\Spring.Tests.Utils.pas',
  Spring.Tests.Collections in '..\..\Tests\System\Spring.Tests.Collections.pas',
  Spring.Tests.DesignPatterns in '..\..\Tests\System\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in '..\..\Tests\System\Spring.Tests.Helpers.pas',
  Spring.Tests.Reflection.ValueConverters in '..\..\Tests\System\Spring.Tests.Reflection.ValueConverters.pas',
  Spring.Tests.System in '..\..\Tests\System\Spring.Tests.System.pas',
  Spring.UnitTests in '..\..\Tests\Spring.UnitTests.pas',
  Spring.Tests.SysUtils in '..\..\Tests\System\Spring.Tests.SysUtils.pas';

{$REGION 'Deprecated (The classical way)'}

procedure RegisterTestCases;
begin
  RegisterTests('Spring.System', [
//    TTestBuffer.Suite,
//    TTestEmptyBuffer.Suite,
//    TTestFiveByteBuffer.Suite,
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3),
    TTestLazy.Suite,
    TTestEmptyMulticastEvent.Suite
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

  RegisterTests('Spring.Core.Pool', [
    TTestObjectPool.Suite
  ]);

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
    TTestInjectionByValue.Suite
  ]);

  RegisterTests('Spring.Extensions.Utils', [
    TTestSplitString.Suite,
    TTestTryConvertStrToDateTime.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestVersion.Suite,
    TTestEnum.Suite
  ]);

  RegisterTests('Spring.Extensions.Cryptography', [
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

{$ENDREGION}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;

  RegisterTestCases;
//  RegisterAllTestCasesByRTTI;

  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

