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

unit Spring.Logging;

{$I Spring.inc}

interface

uses
  SysUtils,
  Rtti
{$IFDEF LOG_EXTENDED_FROMAT}
{$IFDEF DELPHIXE2_UP}
  ,System.UITypes //Has minimum dependencies
{$ELSE}
  ,Graphics //Has (unfortunately) VCL dependencies
{$ENDIF}
{$ENDIF}
  ;

{$REGION 'Shadowed Delphi types'}
{$IFDEF LOG_EXTENDED_FROMAT}
{$IFDEF DELPHIXE2_UP}
type
  TColor = System.UITypes.TColor;
  TColors = System.UITypes.TColors;
  TFontStyle = System.UITypes.TFontStyle;
  TFontStyles = System.UITypes.TFontStyles;

const
  clDefault = TColors.SysDefault;
{$ELSE}
type
  TColor = Graphics.TColor;
  TFontStyle = Graphics.TFontStyle;
  TFontStyles = Graphics.TFontStyles;

 const
  clDefault = Graphics.clDefault;
{$ENDIF}
{$ENDIF}
{$ENDREGION}

{$SCOPEDENUMS ON}
{$REGION 'Log Level definitions and constants'}
type
  TLogLevel = (
    /// <summary>
    ///   Excluded from all states and should never be used!
    /// </summary>
    Unknown,
    Verbose,
    /// <summary>
    ///   Should only be called if stack is sent to the appender. The appender
    ///   may treat it in a specific way. No one else should use this level.
    ///   If this level is not set, callstack logging will be disabled
    ///   completely, this may have significant performance impact on some
    ///   platforms.
    /// </summary>
    CallStack,
    /// <summary>
    ///   Should only be called if serialized data (object, record, etc.) is
    ///   sent to the appender. The appender may treat it in a specific way.
    ///   No one else should use this level. If this level is not set,
    ///   data serialization logging will be disabled completely.
    /// </summary>
    SerializedData,
    Debug,
    Text,
    Info,
    Warning,
    Error,
    Fatal
  );
  TLogLevels = set of TLogLevel;

const
  LOG_ALL_LEVELS = [Low(TLogLevel)..High(TLogLevel)] - [TLogLevel.Unknown];
  LOG_BASIC_LEVELS = [
    TLogLevel.Info,
    TLogLevel.Warning,
    TLogLevel.Error,
    TLogLevel.Fatal
  ];
  {$ENDREGION}

type
  {$REGION 'TLogEntry'}
  TLogEntry = record
  private
    fLevel: TLogLevel;
    fMsg: string;
    fTimeStamp: TDateTime;
    fExc: Exception;
{$IFDEF LOG_EXTENDED_FROMAT}
    /// <summary>
    ///   Leave as default to instruct the appender/viewer to choose the default
    ///   color based on the level or entry contents or prescribe the color of
    ///   your choosing (not that some appenders may ignore the color).
    /// </summary>
    fColor: TColor;
    /// <summary>
    ///   Similar to Color but defines a font style.
    /// </summary>
    fFontStyle: TFontStyles;
{$ENDIF}
    fAddStack: Boolean;
    /// <summary>
    ///   An arbitrary data that the logger may output to the appenders
    /// </summary>
    fData: TValue;
    /// <summary>
    ///   Additional data anyone can use to extend behavior of their appenders
    /// </summary>
    fTag: NativeInt;
  public
    constructor Create(level : TLogLevel; const msg : string); overload;
    constructor Create(level : TLogLevel; const msg : string;
      const exc: Exception); overload;
    {constructor Create(level : TLogLevel; const msg : string;
      color : TColor = clDefault; fontStyle : TFontStyles = []; )}

    function SetException(const exc: Exception) : TLogEntry;
{$IFDEF LOG_EXTENDED_FROMAT}
    function SetColor(color: TColor): TLogEntry;
    function SetFontStyle(fontStyle: TFontStyles) : TLogEntry;
{$ENDIF}
    function AddStack: TLogEntry;
    function SetData(const Data: TValue): TLogEntry;
    function SetTag(tag: NativeInt): TLogEntry;

    property Level: TLogLevel read fLevel;
    property Msg: string read fMsg;
    property TimeStamp: TDateTime read fTimeStamp;
    property Exc: Exception read fExc;
{$IFDEF LOG_EXTENDED_FROMAT}
    property Color: TColor read fColor;
    property FontStyle: TFontStyles read fFontStyle;
{$ENDIF}
    property AddStackValue: Boolean read fAddStack;
    property Data: TValue read fData;
    property Tag: NativeInt read fTag;
  end;
  {$ENDREGION}

  {$REGION 'ILogger'}
  ILogger = interface
    ['{8655E906-C12D-4EB3-8291-30CEAB769B26}']
    procedure Log(const entry: TLogEntry); overload;

    procedure Log(level: TLogLevel; const msg : string); overload;
    procedure Log(level: TLogLevel; const msg : string;
      const exc: Exception); overload;
    procedure Log(level: TLogLevel; const fmt : string;
      const args : array of const); overload;
    procedure Log(level: TLogLevel; const fmt : string;
      const args : array of const; const exc: Exception); overload;

    procedure Fatal(const msg : string); overload;
    procedure Fatal(const msg : string; const exc: Exception); overload;
    procedure Fatal(const fmt : string; const args : array of const); overload;
    procedure Fatal(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    procedure Error(const msg : string); overload;
    procedure Error(const msg : string; const exc: Exception); overload;
    procedure Error(const fmt : string; const args : array of const); overload;
    procedure Error(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    procedure Warn(const msg : string); overload;
    procedure Warn(const msg : string; const exc: Exception); overload;
    procedure Warn(const fmt : string; const args : array of const); overload;
    procedure Warn(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    procedure Info(const msg : string); overload;
    procedure Info(const msg : string; const exc: Exception); overload;
    procedure Info(const fmt : string; const args : array of const); overload;
    procedure Info(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    procedure Text(const msg : string); overload;
    procedure Text(const msg : string; const exc: Exception); overload;
    procedure Text(const fmt : string; const args : array of const); overload;
    procedure Text(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    procedure Debug(const msg : string); overload;
    procedure Debug(const msg : string; const exc: Exception); overload;
    procedure Debug(const fmt : string; const args : array of const); overload;
    procedure Debug(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    procedure Verbose(const msg : string); overload;
    procedure Verbose(const msg : string; const exc: Exception); overload;
    procedure Verbose(const fmt : string; const args : array of const); overload;
    procedure Verbose(const fmt : string; const args : array of const;
      const exc: Exception); overload;

    function IsEnabled(level: TLogLevel): Boolean;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsVerboseEnabled: Boolean;
  end;
  {$ENDREGION}

  {$REGION 'ILogAppender'}
  ILogAppender = interface
    ['{70DDEB60-3D01-48FB-92CF-A738A8C4BC85}']
    procedure Send(const entry: TLogEntry);
  end;
  {$ENDREGION}

  {$REGION 'ILoggerController'}
  ILoggerController = interface(ILogAppender)
    ['{6556A795-6F1B-4392-92FC-8E3391E3CB07}']
    procedure AddAppender(const appedner: ILogAppender);
  end;
  {$ENDREGION}

  {$REGION 'ILoggerProperties'}
  /// <summary>
  ///   Interface that can be used to change logger/appender settings during
  ///   runtime. It is hidden within this interface so that it is not widely
  ///   exposed to the consumer.
  /// </summary>
  ILoggerProperties = interface
    ['{6514ADA8-A0A0-4234-A5EE-FBAFE34B58F2}']
    function GetLevels: TLogLevels;
    function GetEnabled: Boolean;

    procedure SetLevels(value: TLogLevels);
    procedure SetEnabled(value: Boolean);

    property Levels: TLogLevels read GetLevels write SetLevels;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;
  {$ENDREGION}

implementation

{$REGION 'TLogEntry'}
{ TLogEntry }

function TLogEntry.AddStack: TLogEntry;
begin
  Result := Self;
  Result.fAddStack := true;
end;

constructor TLogEntry.Create(level: TLogLevel; const msg: string);
begin
  fTimeStamp:=Now; //Do this ASAP
  fLevel := level;
  fMsg := msg;

{$IFDEF LOG_EXTENDED_FROMAT}
  //Set default values
  fColor := clDefault;
  fFontStyle := [];
{$ENDIF}

  //Reset non-managed fields
  fExc := nil;
  fAddStack := false;
  fTag := 0;

  Assert(fData.IsEmpty);
end;

constructor TLogEntry.Create(level: TLogLevel; const msg: string;
  const exc: Exception);
begin
  Create(level, msg);
  fExc := exc;
end;

{$IFDEF LOG_EXTENDED_FROMAT}
function TLogEntry.SetColor(color: TColor): TLogEntry;
begin
  Result := Self;
  Result.fColor := color;
end;
{$ENDIF}

function TLogEntry.SetData(const Data: TValue): TLogEntry;
begin
  Result := Self;
  Result.fData := Data;
end;

function TLogEntry.SetException(const exc: Exception): TLogEntry;
begin
  Result := Self;
  Result.fExc := exc;
end;

{$IFDEF LOG_EXTENDED_FROMAT}
function TLogEntry.SetFontStyle(fontStyle: TFontStyles): TLogEntry;
begin
  Result := Self;
  Result.fFontStyle := fontStyle;
end;
{$ENDIF}

function TLogEntry.SetTag(tag: NativeInt): TLogEntry;
begin
  Result := Self;
  Result.fTag := tag;
end;
{$ENDREGION}

end.
