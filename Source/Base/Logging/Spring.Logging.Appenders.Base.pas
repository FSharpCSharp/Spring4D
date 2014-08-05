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

unit Spring.Logging.Appenders.Base;

{$I Spring.inc}

interface

uses
  Spring.Logging;

type
  {$REGION 'TLogAppenderBase'}
  TLogAppenderBase = class(TInterfacedObject, ILogAppender, ILoggerProperties)
  private
    fEnabled: Boolean;
    fLevels: TLogLevels;

    function GetLevels: TLogLevels;
    function GetEnabled: Boolean;

    procedure SetLevels(value: TLogLevels);
    procedure SetEnabled(value: Boolean);
  protected const
    {$REGION 'Helper constants and functions'}
    //May or may not be used by descendants, its here just for convenience
    LEVEL : array[TLogLevel] of string = (
      '[UNKNOWN]',
      '[VERBOSE]',
      '', //CallStack
      '', //SerializedData
      '[DEBUG]',
      '[TEXT]',
      '[INFO]',
      '[WARN]',
      '[ERROR]',
      '[FATAL]'
    );
    LEVEL_FIXED : array[TLogLevel] of string = (
      '[UNK  ]',
      '[VERB ]',
      '', //CallStack
      '', //SerializedData
      '[DEBUG]',
      '[TEXT ]',
      '[INFO ]',
      '[WARN ]',
      '[ERROR]',
      '[FATAL]'
    );
    class function FormatMsg(const entry: TLogEntry): string; static; inline;
    {$ENDREGION}
  protected
    function IsEnabled(level: TLogLevel): Boolean; inline;
    procedure DoSend(const entry: TLogEntry); virtual; abstract;
  public
    constructor Create;

    procedure Send(const entry: TLogEntry);

    property Enabled: Boolean read fEnabled write fEnabled;
    property Levels: TLogLevels read fLevels write fLevels;
  end;
  {$ENDREGION}
implementation

{$REGION 'TLogAppenderBase'}
{ TLogAppenderBase }

constructor TLogAppenderBase.Create;
begin
  inherited;
  fEnabled := true;
  fLevels := LOG_BASIC_LEVELS;
end;

class function TLogAppenderBase.FormatMsg(const entry: TLogEntry): string;
begin
  if (entry.Exc = nil) then
    Result := entry.Msg
  else
  begin
    if (entry.Msg <> '') then
      Result := entry.Msg + ', ' + entry.Exc.ClassName
    else Result := entry.Exc.ClassName;
    if (entry.Exc.Message <> '') then
      Result := Result + ': ' + entry.Exc.Message;
  end;
end;

function TLogAppenderBase.GetEnabled: Boolean;
begin
  Result := Enabled;
end;

function TLogAppenderBase.GetLevels: TLogLevels;
begin
  Result := Levels;
end;

function TLogAppenderBase.IsEnabled(level: TLogLevel): Boolean;
begin
  Result:=fEnabled and (level in fLevels);
end;

procedure TLogAppenderBase.Send(const entry: TLogEntry);
begin
  if (IsEnabled(entry.Level)) then
    DoSend(entry);
end;
procedure TLogAppenderBase.SetEnabled(value: Boolean);
begin
  Enabled := value;
end;

procedure TLogAppenderBase.SetLevels(value: TLogLevels);
begin
  Levels := value;
end;
{$ENDREGION}

end.
