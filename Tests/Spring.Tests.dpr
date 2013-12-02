{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
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

{$I Spring.Tests.inc}

uses
  FinalBuilder.XMLTestRunner in 'Source\FinalBuilder.XMLTestRunner.pas',
  Spring.RunTestsUsingConsole in 'Source\Spring.RunTestsUsingConsole.pas',
  Spring.RunTestsUsingVclGui in 'Source\Spring.RunTestsUsingVclGui.pas',
  Spring.RunTestsUsingXmlOutput in 'Source\Spring.RunTestsUsingXmlOutput.pas',
  Spring.TestUtils in 'Source\Spring.TestUtils.pas',
  Spring.Tests.Base in 'Source\Base\Spring.Tests.Base.pas',
  Spring.Tests.Collections in 'Source\Base\Spring.Tests.Collections.pas',
  Spring.Tests.DesignPatterns in 'Source\Base\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in 'Source\Base\Spring.Tests.Helpers.pas',
  Spring.Tests.Reflection.ValueConverters in 'Source\Base\Spring.Tests.Reflection.ValueConverters.pas',
  Spring.Tests.SysUtils in 'Source\Base\Spring.Tests.SysUtils.pas',
  Spring.Tests.Container.Components in 'Source\Core\Spring.Tests.Container.Components.pas',
  Spring.Tests.Container.Interfaces in 'Source\Core\Spring.Tests.Container.Interfaces.pas',
  Spring.Tests.Container.LifetimeManager in 'Source\Core\Spring.Tests.Container.LifetimeManager.pas',
  Spring.Tests.Container in 'Source\Core\Spring.Tests.Container.pas',
  Spring.Tests.Pool in 'Source\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in 'Source\Extensions\Spring.Tests.Cryptography.pas',
  Spring.Tests.Utils in 'Source\Extensions\Spring.Tests.Utils.pas',
  Spring.TestRegistration in 'Source\Spring.TestRegistration.pas';

begin
  RegisterTestCases();
  ReportMemoryLeaksOnShutdown := True;
  RunRegisteredTestCases(); // all RunTestsUsing* units should support this method
end.
