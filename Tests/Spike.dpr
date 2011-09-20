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

/// <summary>
/// This program is only for experimental use. Please add it to Ignored list.
/// </summary>
program Spike;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  Windows,
  StrUtils,
  Spring,
  Spring.Collections,
  Spring.Utils,
  Spring.Services.Logging,
  Spring.Logging,
  Spring.Logging.Appenders,
  Spring.Logging.Layouts;

var
  t: IAppender;
  e: TLoggingEvent;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    GlobalContainer.RegisterComponent<TFileAppender>.Implements<IAppender>('file').AsSingleton;
    GlobalContainer.RegisterComponent<TConsoleAppender>.Implements<IAppender>('console').AsSingleton;
    GlobalContainer.RegisterComponent<TColoredConsoleAppender>.Implements<IAppender>('coloredconsole').AsSingleton;
    GlobalContainer.RegisterComponent<TPatternLayout>.Implements<ILayout>('patternlayout');
    GlobalContainer.Build;

    t := ServiceLocator.Resolve<IAppender>('coloredconsole');
    e.Message := 'test';
    t.Append(e);
    t := nil;
    ServiceLocator.Initialize(nil);
    //    Readln(s);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
