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

uses
  Classes,
  SysUtils,
  Rtti,
  Spring,
  Spring.Utils,
  Spring.Collections,
  Spring.Logging,
  Spring.Logging.Core,
  Spring.Logging.Appenders,
  Spring.Logging.Layouts;

type
  TStringAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

var
  logger: ILogger;
  Logs: string;

{ TStringAppender }

procedure TStringAppender.DoAppend(const event: TLoggingEvent);
begin
  Logs := Logs + event.Message + #13#10;
end;

var
  S: string;
  appender: TAppenderBase;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    logger := LoggingManager.GetLogger('Spring');
    appender := TConsoleAppender.Create('s');
    appender := TOutputDebugStringAppender.Create('s');

    appender.Layout := TPatternLayout.Create('%message  %5date %newline %appversion');
    (logger as IAppenderAttachable).AddAppender(appender as IAppender);
    logger.Debug('Debug');
    logger.Info('Info...');
    logger.Warn('WARN');
    logger.Error('ERROR');
    logger := LoggingManager.GetLogger('Spring.Base');
    logger.Debug('XX');
    logger.Info('YY');
    Readln(s);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
