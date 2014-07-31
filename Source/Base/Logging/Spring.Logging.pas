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
  Rtti,
{$IFDEF DELPHIXE2_UP}
  System.UITypes;
{$ELSE}
  Graphics;
{$ENDIF}

{$REGION 'Shadowed Delphi types'}
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
    function SetColor(color: TColor): TLogEntry;
    function SetFontStyle(fontStyle: TFontStyles) : TLogEntry;
    function AddStack: TLogEntry;
    function SetData(const Data: TValue): TLogEntry;
    function SetTag(tag: NativeInt): TLogEntry;

    property Level: TLogLevel read fLevel;
    property Msg: string read fMsg;
    property TimeStamp: TDateTime read fTimeStamp;
    property Exc: Exception read fExc;
    property Color: TColor read fColor;
    property FontStyle: TFontStyles read fFontStyle;
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
  end;
  {$ENDREGION}

  {$REGION 'ILogAppender'}
  ILogAppender = interface
    ['{70DDEB60-3D01-48FB-92CF-A738A8C4BC85}']
    procedure Write(const entry: TLogEntry);
  end;
  {$ENDREGION}

  {$REGION 'ILoggerController'}
  ILoggerController = interface
    ['{6556A795-6F1B-4392-92FC-8E3391E3CB07}']
    procedure Send(const entry: TLogEntry);
    procedure AddAppender(const appedner: ILogAppender);
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

  //Set default values
  fColor := clDefault;
  fFontStyle := [];

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

function TLogEntry.SetColor(color: TColor): TLogEntry;
begin
  Result := Self;
  Result.fColor := color;
end;

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

function TLogEntry.SetFontStyle(fontStyle: TFontStyles): TLogEntry;
begin
  Result := Self;
  Result.fFontStyle := fontStyle;
end;

function TLogEntry.SetTag(tag: NativeInt): TLogEntry;
begin
  Result := Self;
  Result.fTag := tag;
end;
{$ENDREGION}

end.
