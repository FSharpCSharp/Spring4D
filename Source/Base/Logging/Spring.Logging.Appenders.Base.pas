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
{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  SysUtils,
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
  {$REGION 'Helper constants and functions'}
  protected const
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
  public
    class function FormatText(const entry: TLogEntry): string; static; inline;
    class function FormatMsg(const entry: TLogEntry): string; static; inline;
    class function FormatException(const ext: Exception): string; static; //noinline
    class function FormatMethodName(const classType: TClass;
      const methodName: string): string; static; inline;
    class function FormatEntering(const classType: TClass;
      const methodName: string): string; static; inline;
    class function FormatLeaving(const classType: TClass;
      const methodName: string): string; static; inline;
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

uses
  Spring.Logging.ResourceStrings;

{$REGION 'TLogAppenderBase'}
{ TLogAppenderBase }

constructor TLogAppenderBase.Create;
begin
  inherited;
  fEnabled := true;
  fLevels := LOG_BASIC_LEVELS;
end;

class function TLogAppenderBase.FormatEntering(const classType: TClass;
  const methodName: string): string;
begin
  Result := SLogEntering + FormatMethodName(classType, methodName);
end;

class function TLogAppenderBase.FormatException(const ext: Exception): string;
var
  len: Integer;
begin
  SetLength(Result, 1024);
  len := ExceptionErrorMessage(ext, ExceptAddr, @Result[1], Length(Result));
  SetLength(Result, len);
end;

class function TLogAppenderBase.FormatLeaving(const classType: TClass;
  const methodName: string): string;
begin
  Result := SLogLeaving + FormatMethodName(classType, methodName);
end;

class function TLogAppenderBase.FormatMethodName(const classType: TClass;
  const methodName: string): string;
begin
  if (classType <> nil) then
    Result := classType.QualifiedClassName + '.' + methodName
  else Result := methodName;
end;

class function TLogAppenderBase.FormatMsg(const entry: TLogEntry): string;
begin
  if (entry.Exc = nil) then
    Result := FormatText(entry)
  else
  begin
    if (entry.Msg <> '') then
      Result := FormatText(entry) + ': ' + FormatException(entry.Exc)
    else Result := FormatException(entry.Exc);
  end;
end;

class function TLogAppenderBase.FormatText(const entry: TLogEntry): string;
begin
  case entry.EntryType of
    TLogEntryType.Text:
      Result := entry.Msg;

    TLogEntryType.Entering:
      Result := FormatEntering(entry.ClassType, entry.Msg);

    TLogEntryType.Leaving:
      Result := FormatLeaving(entry.ClassType, entry.Msg);
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
