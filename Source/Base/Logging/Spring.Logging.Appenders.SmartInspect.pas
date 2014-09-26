{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Logging.Appenders.SmartInspect;

{$I Spring.inc}

interface

uses
  Spring.Logging,
  Spring.Logging.Appenders.Base;

type
  TSmartInspectAppender = class(TLogAppenderBase)
  protected
    procedure DoSend(const entry: TLogEntry); override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  SiAuto;


{$REGION 'TCodeSiteAppender'}

constructor TSmartInspectAppender.Create;
begin
  inherited;
  Si.Enabled := True;
end;

procedure TSmartInspectAppender.DoSend(const entry: TLogEntry);
begin
  case entry.EntryType of
    TLogEntryType.Text:
      if Assigned(entry.Exception) then
        SiMain.LogException(entry.Exception, TLogAppenderBase.FormatMsg(entry))
      else
        case entry.Level of
          TLogLevel.Unknown: ;

          TLogLevel.Trace:
            SiMain.LogVerbose(entry.Msg);

          TLogLevel.Debug:
            SiMain.LogDebug(entry.Msg);

          TLogLevel.Text,
          TLogLevel.Info:
            SiMain.LogMessage(entry.Msg);

          TLogLevel.Warn:
            SiMain.LogWarning(entry.Msg);

          TLogLevel.Error:
            SiMain.LogError(entry.Msg);

          TLogLevel.Fatal:
            SiMain.LogFatal(entry.Msg);
        end;

    TLogEntryType.Value:
      case entry.Data.Kind of
        tkClass: SiMain.LogObject(entry.Msg, entry.Data.AsObject);
      else
        SiMain.LogValue(entry.Msg, entry.Data.ToString);
      end;

    TLogEntryType.Entering:
      SiMain.EnterMethod(
        TLogAppenderBase.FormatMethodName(entry.ClassType, entry.Msg));

    TLogEntryType.Leaving:
      SiMain.LeaveMethod(
        TLogAppenderBase.FormatMethodName(entry.ClassType, entry.Msg));

    TLogEntryType.CallStack,
    TLogEntryType.SerializedData:
      SiMain.LogMessage(entry.Msg);
  end;
end;

{$ENDREGION}


end.
