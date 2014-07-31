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

unit Spring.Logging.Appenders;

interface

uses
  SysUtils,
  SyncObjs,
  Classes,
{$IFDEF FMX}
  FMX.Platform,
{$ENDIF}
  Spring.Logging;

type
  {$REGION 'TLogAppenderBase'}
  TLogAppenderBase = class(TInterfacedObject, ILogAppender)
  private
    fEnabled: Boolean;
    fLevels: TLogLevels;
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
    procedure DoWrite(const entry: TLogEntry); virtual; abstract;
  public
    constructor Create;

    procedure Write(const entry: TLogEntry);

    property Enabled: Boolean read fEnabled write fEnabled;
    property Levels: TLogLevels read fLevels write fLevels;
  end;
  {$ENDREGION}

  {$REGION 'TLogAppenderWithTimeStampFormat'}
  TLogAppenderWithTimeStampFormat = class abstract(TLogAppenderBase)
  private
    FFormat: string;
  protected
    function FormatTimeStamp(const timeStamp: TDateTime): string; inline;
  public
    constructor Create;

    property Format: string read FFormat write FFormat;
  end;
  {$ENDREGION}

  {$REGION 'TTextLogAppender'}
  /// <summary>
  ///   Simple appender that outputs the text to given Delphi Text file
  ///   (or pipe)
  /// </summary>
  TTextLogAppender = class(TLogAppenderWithTimeStampFormat)
  public type
    PTextFile = ^TextFile;
  protected
    fFile: PTextFile;
    procedure DoWrite(const entry: TLogEntry); override;
  public
    /// <summary>
    ///   Make sure that the pointer doesn;t get out of scope!
    /// </summary>
    constructor Create(output: PTextFile); overload;
    /// <summary>
    ///   Uses stderr
    /// </summary>
    constructor Create; overload;
  end;
  {$ENDREGION}

  {$REGION 'TStreamLogAppender'}
  TStreamLogAppender = class(TLogAppenderWithTimeStampFormat)
  private
    fStream: TStream;
{$IFNDEF AUTOREFCOUNT}
    fOwnsStream: Boolean;
{$ENDIF}
    fEncoding: TEncoding;
    fLock: TCriticalSection;
  protected
    procedure DoWrite(const entry: TLogEntry); override;
  public
    constructor Create(const stream: TStream; ownsStream: Boolean = true;
      const encoding: TEncoding = nil);
    destructor Destroy; override;
  end;
  {$ENDREGION}

  {$REGION 'TTraceLogAppender'}
{$IFDEF MSWINDOWS}
  TTraceLogAppender = class(TLogAppenderWithTimeStampFormat)
  protected
    procedure DoWrite(const entry: TLogEntry); override;
  end;
{$ENDIF}
 {$ENDREGION}

  {$REGION 'TFMXLogAppender'}
{$IFDEF FMX}
  TFMXLogAppender = class(TLogAppenderWithTimeStampFormat)
  private
    fService: IFMXLoggingService;
  protected
    procedure DoWrite(const entry: TLogEntry); override;
  public
    constructor Create;
  end;
{$ENDIF}
  {$ENDREGION}

  {$REGION 'TAndroidLogAppender'}
{$IFDEF ANDROID}
  TAndroidLogAppender = class(TLogAppenderWithTimeStampFormat)
  private
  	fTagMarshaller: TMarshaller;
	  fTag: MarshaledAString; //fTag is valid as long as marshaller is referenced
  protected
    procedure DoWrite(const entry: TLogEntry); override;
  public
    /// <summary>
    ///   Creates Android logcat log appender, the tag can be used to
    ///   differentiate applications in the adb logcat viewer.
    /// </summary>
    constructor Create(const tag: string = 'delphiapp');
  end;
{$ENDIF}
  {$ENDREGION}

  {$REGION 'Default log appender assignment'}
{$IFDEF MSWINDOWS}
 {$IFDEF CONSOLE}
  TDefaultLogAppender = TConsoleLogAppender;
 {$ELSE !CONSOLE}
	TDefaultLogAppender = TTraceLogAppender;
 {$ENDIF CONSOLE}
{$ELSE !MSWINDOWS}
 {$IFDEF FMX}
  {$IFDEF ANDROID}
	TDefaultLogAppender = TAndroidLogAppender;
  {$ELSE !ANDROID}
	TDefaultLogAppender = TFMXLogAppender
  {$ENDIF ANDROID}
 {$ELSE !FMX}
	TDefaultLogAppender = TConsoleLogAppender;
 {$ENDIF FMX}
{$ENDIF MSWINDOWS}
  {$ENDREGION}

implementation

uses
  Spring
{$IFDEF MSWINDOWS}
  ,Windows
{$ENDIF}
{$IFDEF ANDROID}
  ,Androidapi.Log
{$ENDIF}
  ;


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

function TLogAppenderBase.IsEnabled(level: TLogLevel): Boolean;
begin
  Result:=fEnabled and (level in fLevels);
end;

procedure TLogAppenderBase.Write(const entry: TLogEntry);
begin
  if (IsEnabled(entry.Level)) then
    DoWrite(entry);
end;
{$ENDREGION}

{$REGION 'TLogAppenderWithTimeStampFormat'}
{ TLogAppenderWithTimeStampFormat }

constructor TLogAppenderWithTimeStampFormat.Create;
begin
  inherited;
  Format := 'hh:nn:ss:zzz';
end;

function TLogAppenderWithTimeStampFormat.FormatTimeStamp(
  const timeStamp: TDateTime): string;
begin
  Result := FormatDateTime(Format, timeStamp);
end;
{$ENDREGION}

{$REGION 'TTextLogAppender'}
{ TTextLogAppender }

constructor TTextLogAppender.Create(output: PTextFile);
begin
  inherited Create;
  fFile := output;
end;

constructor TTextLogAppender.Create;
begin
  Create(@ErrOutput);
end;

procedure TTextLogAppender.DoWrite(const entry: TLogEntry);
begin
  Writeln(fFile^, FormatTimeStamp(entry.TimeStamp), ': ',
    LEVEL_FIXED[entry.Level], ' ', FormatMsg(entry));
  Flush(fFile^);
end;
{$ENDREGION}

{$REGION 'TStreamLogAppender'}
{ TStreamLogAppender }

constructor TStreamLogAppender.Create(const stream: TStream;
  ownsStream: Boolean; const encoding: TEncoding);
begin
  inherited Create;

  Guard.CheckNotNull(stream, 'stream');
  fStream := stream;
{$IFNDEF AUTOREFCOUNT}
  fOwnsStream := ownsStream;
{$ENDIF}
  if (encoding <> nil) then
    fEncoding := encoding
  else fEncoding := TEncoding.UTF8;
  fLock := TCriticalSection.Create;
end;

destructor TStreamLogAppender.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  if (fOwnsStream) then
    fStream.Free;
{$ENDIF}
  fLock.Free;
  inherited;
end;

procedure TStreamLogAppender.DoWrite(const entry: TLogEntry);
var
  buff: TBytes;
begin
  fLock.Enter;
  try
    buff := fEncoding.GetBytes(FormatTimeStamp(entry.TimeStamp) + ': ' +
      LEVEL_FIXED[entry.Level] + ' ' + FormatMsg(entry));
    fStream.WriteBuffer(buff, Length(buff));
  finally
    fLock.Leave;
  end;
end;
{$ENDREGION}

{$REGION 'TTraceLogAppender'}
{ TTraceLogAppender }

{$IFDEF MSWINDOWS}
procedure TTraceLogAppender.DoWrite(const entry: TLogEntry);
var buff: string;
begin
  buff := FormatTimeStamp(entry.TimeStamp) + ': ' + LEVEL[entry.Level] + ' ' +
    FormatMsg(entry);
  OutputDebugString(PChar(buff));
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'TFMXLogAppender'}
{ TFMXLogAppender }

{$IFDEF FMX}
constructor TFMXLogAppender.Create;
begin
  fService:=TPlatformServices.Current.GetPlatformService(IFMXLoggingService) as IFMXLoggingService;
end;

procedure TFMXLogAppender.DoWrite(const entry: TLogEntry);
begin
  fService.Log(FormatTimeStamp(entry.TimeStamp) + ': ' + LEVEL[entry.Level] + ' ' +
    FormatMsg(entry), []);
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'TAndroidLogAppender'}
{ TAndroidLogAppender }

{$IFDEF ANDROID}
constructor TAndroidLogAppender.Create(const tag: string);
begin
  inherited Create;
  fTag := FTagMarshaller.AsAnsi(Tag).ToPointer;
end;

procedure TAndroidLogAppender.DoWrite(const entry: TLogEntry);
const
  LEVEL : array[TLogLevel] of android_LogPriority = (
    ANDROID_LOG_UNKNOWN,
    ANDROID_LOG_VERBOSE,
    ANDROID_LOG_DEBUG,  //CallStack
    ANDROID_LOG_DEBUG,  //SerializedData
    ANDROID_LOG_DEBUG,  //Debug
    ANDROID_LOG_DEBUG,  //Text
    ANDROID_LOG_INFO,
    ANDROID_LOG_WARN,
    ANDROID_LOG_ERROR,
    ANDROID_LOG_FATAL
  );
var
  m: TMarshaller;
  buff: string;
begin
  buff:=FormatTimeStamp(entry.TimeStamp) + ': ' + FormatMsg(entry);
	__android_log_write(LEVEL[entry.Level], fTag, M.AsAnsi(buff).ToPointer);
end;
{$ENDIF}
{$ENDREGION}

end.
