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

unit Spring.RunTestsUsingConsole;

interface

{$i Spring.Tests.inc}

{$ifdef CONSOLE_TESTRUNNER}
procedure RunRegisteredTestCases();
{$endif CONSOLE_TESTRUNNER}

implementation

{$ifdef CONSOLE_TESTRUNNER}
uses
  SysUtils,
  TextTestRunner,
  Spring.TestUtils;

var
  ExitBehavior: TRunnerExitBehavior = rxbContinue;

procedure RunRegisteredTestCases();
begin
  WriteLn('To run with rxbPause, use -p switch');
  WriteLn('To run with rxbHaltOnFailures, use -h switch');
  WriteLn('No switch runs as rxbContinue');

  if FindCmdLineSwitch('p', ['-', '/'], true) then
    ExitBehavior := rxbPause
  else if FindCmdLineSwitch('h', ['-', '/'], true) then
    ExitBehavior := rxbHaltOnFailures;
  ProcessTestResult(TextTestRunner.RunRegisteredTests(ExitBehavior));
{$ifdef MACOS}
  {$ifdef DEBUG}
  Write('Press <Enter>');
  Readln;
  {$endif DEBUG}
{$endif MACOS}
end;
{$endif CONSOLE_TESTRUNNER}

end.
