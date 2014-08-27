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

unit Spring.Logging.Loggers;

{$I Spring.inc}

interface

uses
  SysUtils,
  Rtti,
  Spring.Logging;

type
  {$REGION 'TNullLogger'}
  /// <summary>
  ///   Logger that does nothing and does it in fastes way possible.
  /// </summary>
  TNullLogger = class(TInterfacedObject, ILogger)
  public
    function GetEnabled: Boolean;
    function GetLevels: TLogLevels;

    function IsEnabled(level: TLogLevel): Boolean; inline;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;

    procedure Log(const entry: TLogEntry); overload;

    procedure LogValue(const name: string; const value: TValue); overload;
    procedure LogValue(level: TLogLevel; const name: string;
      const value: TValue); overload;

    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const e: Exception); overload;
    procedure Log(const fmt: string; const args: array of const); overload;
    procedure Log(const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Log(level: TLogLevel; const msg: string); overload;
    procedure Log(level: TLogLevel; const msg: string;
      const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt: string;
      const args: array of const); overload;
    procedure Log(level: TLogLevel; const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;
    procedure Fatal(const fmt: string; const args: array of const); overload;
    procedure Fatal(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;
    procedure Error(const fmt: string; const args: array of const); overload;
    procedure Error(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; const e: Exception); overload;
    procedure Warn(const fmt: string; const args: array of const); overload;
    procedure Warn(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; const e: Exception); overload;
    procedure Info(const fmt: string; const args: array of const); overload;
    procedure Info(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Text(const msg: string); overload;
    procedure Text(const msg: string; const e: Exception); overload;
    procedure Text(const fmt: string; const args: array of const); overload;
    procedure Text(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; const e: Exception); overload;
    procedure Debug(const fmt: string; const args: array of const); overload;
    procedure Debug(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Trace(const msg: string); overload;
    procedure Trace(const msg: string; const e: Exception); overload;
    procedure Trace(const fmt: string; const args: array of const); overload;
    procedure Trace(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Entering(const classType: TClass;
      const methodName: string); overload;
    procedure Entering(const instance: TObject;
      const methodName: string); overload;
    procedure Entering(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    procedure Leaving(const classType: TClass;
      const methodName: string); overload;
    procedure Leaving(const instance: TObject;
      const methodName: string); overload;
    procedure Leaving(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    function Track(const classType: TClass;
      const methodName: string): IInterface; overload;
    function Track(const instance: TObject;
      const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string): IInterface; overload;
  end;
  {$ENDREGION}


  {$REGION 'TLoggerBase'}
  {$M+}
  TLoggerBase = class abstract(TInterfacedObject, ILogger, ILogAppender,
    ILoggerProperties)
  private
    type
      TLogTracking = class(TInterfacedObject, IInterface)
      private
        fLogger: ILogger;
        fLevel: TLogLevel;
        fClassType: TClass;
        fMethodName: string;
      public
        constructor Create(const logger: ILogger; level: TLogLevel;
          const classType: TClass; const methodName: string);
        destructor Destroy; override;
      end;
  private
    fDefaultLevel: TLogLevel;
    fEnabled: Boolean;
    fLevels: TLogLevels;

    function GetDefaultLevel: TLogLevel;
    function GetEnabled: Boolean;
    function GetLevels: TLogLevels;

    procedure SetDefaultLevel(value: TLogLevel);
    procedure SetEnabled(value: Boolean);
    procedure SetLevels(value: TLogLevels);
  protected
    procedure DoLog(const entry: TLogEntry); virtual; abstract;

    procedure ILogAppender.Send = Log;
  public
    constructor Create;

    function IsEnabled(level: TLogLevel): Boolean; inline;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;

    procedure Log(const entry: TLogEntry); overload;

    procedure LogValue(const name: string; const value: TValue); overload;
    procedure LogValue(level: TLogLevel; const name: string;
      const value: TValue); overload;

    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const e: Exception); overload;
    procedure Log(const fmt: string; const args: array of const); overload;
    procedure Log(const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Log(level: TLogLevel; const msg : string); overload;
    procedure Log(level: TLogLevel; const msg : string;
      const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt : string;
      const args : array of const); overload;
    procedure Log(level: TLogLevel; const fmt : string;
      const args : array of const; const e: Exception); overload;

    procedure Fatal(const msg : string); overload;
    procedure Fatal(const msg : string; const e: Exception); overload;
    procedure Fatal(const fmt : string; const args : array of const); overload;
    procedure Fatal(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Error(const msg : string); overload;
    procedure Error(const msg : string; const e: Exception); overload;
    procedure Error(const fmt : string; const args : array of const); overload;
    procedure Error(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Warn(const msg : string); overload;
    procedure Warn(const msg : string; const e: Exception); overload;
    procedure Warn(const fmt : string; const args : array of const); overload;
    procedure Warn(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Info(const msg : string); overload;
    procedure Info(const msg : string; const e: Exception); overload;
    procedure Info(const fmt : string; const args : array of const); overload;
    procedure Info(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Text(const msg : string); overload;
    procedure Text(const msg : string; const e: Exception); overload;
    procedure Text(const fmt : string; const args : array of const); overload;
    procedure Text(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Debug(const msg : string); overload;
    procedure Debug(const msg : string; const e: Exception); overload;
    procedure Debug(const fmt : string; const args : array of const); overload;
    procedure Debug(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Trace(const msg : string); overload;
    procedure Trace(const msg : string; const e: Exception); overload;
    procedure Trace(const fmt : string; const args : array of const); overload;
    procedure Trace(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Entering(const classType: TClass;
      const methodName: string); overload;
    procedure Entering(const instance: TObject;
      const methodName: string); overload;
    procedure Entering(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    procedure Leaving(const classType: TClass;
      const methodName: string); overload;
    procedure Leaving(const instance: TObject;
      const methodName: string); overload;
    procedure Leaving(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    function Track(const classType: TClass;
      const methodName: string): IInterface; overload;
    function Track(const instance: TObject;
      const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string): IInterface; overload;
  published
    property DefaultLevel: TLogLevel read GetDefaultLevel write SetDefaultLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TLogLevels read GetLevels write SetLevels;
  end;
  {$M-}
  {$ENDREGION}


  {$REGION 'TLogger'}
  TLogger = class(TLoggerBase)
  private
    fController: ILoggerController;
  protected
    procedure DoLog(const entry: TLogEntry); override;
  public
    constructor Create(const controller: ILoggerController);
  end;
  {$ENDREGION}


implementation

uses
  Spring,
  Spring.Collections;


{$REGION 'TNullLogger'}

procedure TNullLogger.Log(const entry: TLogEntry);
begin
end;

procedure TNullLogger.Log(level: TLogLevel; const msg: string);
begin
end;

procedure TNullLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLogger.Log(level: TLogLevel; const msg: string;
  const e: Exception);
begin
end;

procedure TNullLogger.Debug(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Debug(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLogger.Debug(const msg: string);
begin
end;

procedure TNullLogger.Debug(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Error(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Entering(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLogger.Entering(const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLogger.Entering(const instance: TObject;
  const methodName: string);
begin
end;

procedure TNullLogger.Error(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLogger.Error(const msg: string);
begin
end;

procedure TNullLogger.Error(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Fatal(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Fatal(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

function TNullLogger.GetEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetLevels: TLogLevels;
begin
  Result := [];
end;

procedure TNullLogger.Fatal(const msg: string);
begin
end;

procedure TNullLogger.Fatal(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Info(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Info(const msg: string);
begin
end;

procedure TNullLogger.Info(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

function TNullLogger.IsDebugEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsEnabled(level: TLogLevel): Boolean;
begin
  Result := False;
end;

function TNullLogger.IsErrorEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsFatalEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsInfoEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsTextEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsTraceEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.IsWarnEnabled: Boolean;
begin
  Result := False;
end;

procedure TNullLogger.Info(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Leaving(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLogger.Log(const msg: string);
begin
end;

procedure TNullLogger.Log(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Log(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Log(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLogger.Leaving(const instance: TObject;
  const methodName: string);
begin
end;

procedure TNullLogger.Leaving(const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const e: Exception);
begin
end;

procedure TNullLogger.LogValue(level: TLogLevel; const name: string;
  const value: TValue);
begin
end;

procedure TNullLogger.LogValue(const name: string; const value: TValue);
begin
end;

procedure TNullLogger.Text(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Text(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

function TNullLogger.Track(const instance: TObject;
  const methodName: string): IInterface;
begin
end;

function TNullLogger.Track(const classType: TClass;
  const methodName: string): IInterface;
begin
end;

function TNullLogger.Track(level: TLogLevel; const classType: TClass;
  const methodName: string): IInterface;
begin
  Result := nil;
end;

procedure TNullLogger.Text(const msg: string);
begin
end;

procedure TNullLogger.Text(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Trace(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Trace(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLogger.Trace(const msg: string);
begin
end;

procedure TNullLogger.Trace(const msg: string; const e: Exception);
begin
end;

procedure TNullLogger.Warn(const fmt: string; const args: array of const);
begin
end;

procedure TNullLogger.Warn(const fmt: string; const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLogger.Warn(const msg: string);
begin
end;

procedure TNullLogger.Warn(const msg: string; const e: Exception);
begin
end;

{$ENDREGION}


{$REGION 'TLoggerBase'}

constructor TLoggerBase.Create;
begin
  inherited;
  fDefaultLevel := TLogLevel.Info;
  fEnabled := True;
  fLevels := LOG_BASIC_LEVELS;
end;

procedure TLoggerBase.Debug(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, Format(fmt, args)));
end;

procedure TLoggerBase.Debug(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, Format(fmt, args), e));
end;

procedure TLoggerBase.Debug(const msg: string);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, msg));
end;

procedure TLoggerBase.Debug(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, msg, e));
end;

procedure TLoggerBase.Error(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, Format(fmt, args)));
end;

procedure TLoggerBase.Entering(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Entering, methodName, classType));
end;

procedure TLoggerBase.Entering(const classType: TClass;
  const methodName: string);
begin
  Entering(classType, methodName);
end;

procedure TLoggerBase.Entering(const instance: TObject;
  const methodName: string);
begin
  Entering(instance, methodName);
end;

procedure TLoggerBase.Error(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, Format(fmt, args), e));
end;

procedure TLoggerBase.Error(const msg: string);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, msg));
end;

procedure TLoggerBase.Error(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, msg, e));
end;

procedure TLoggerBase.Fatal(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, Format(fmt, args)));
end;

procedure TLoggerBase.Fatal(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, Format(fmt, args), e));
end;

function TLoggerBase.GetDefaultLevel: TLogLevel;
begin
  Result := fDefaultLevel;
end;

function TLoggerBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TLoggerBase.GetLevels: TLogLevels;
begin
  Result := fLevels;
end;

procedure TLoggerBase.Fatal(const msg: string);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, msg));
end;

procedure TLoggerBase.Fatal(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, msg, e));
end;

procedure TLoggerBase.Info(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, msg, e));
end;

procedure TLoggerBase.Info(const msg: string);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, msg));
end;

procedure TLoggerBase.Info(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, Format(fmt, args), e));
end;

procedure TLoggerBase.Info(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, Format(fmt, args)));
end;

function TLoggerBase.IsDebugEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Debug);
end;

function TLoggerBase.IsEnabled(level: TLogLevel): Boolean;
begin
  Result := fEnabled and (level in fLevels);
end;

function TLoggerBase.IsErrorEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Error);
end;

function TLoggerBase.IsFatalEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Fatal);
end;

function TLoggerBase.IsInfoEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Info);
end;

function TLoggerBase.IsTextEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Text);
end;

function TLoggerBase.IsTraceEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Trace);
end;

function TLoggerBase.IsWarnEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Warn);
end;

procedure TLoggerBase.Leaving(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Leaving, methodName, classType));
end;

procedure TLoggerBase.Leaving(const instance: TObject;
  const methodName: string);
begin
  Leaving(instance.ClassType, methodName);
end;

procedure TLoggerBase.Leaving(const classType: TClass;
  const methodName: string);
begin
  Leaving(classType, methodName);
end;

procedure TLoggerBase.Log(const entry: TLogEntry);
begin
  if IsEnabled(entry.Level) then
    DoLog(entry);
end;

procedure TLoggerBase.Log(level: TLogLevel; const msg: string;
  const e: Exception);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, msg, e));
end;

procedure TLoggerBase.Log(level: TLogLevel; const msg: string);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, msg));
end;

procedure TLoggerBase.Log(level: TLogLevel; const fmt: string;
  const args: array of const);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, Format(fmt, args)));
end;

procedure TLoggerBase.Log(const msg: string);
begin
  Log(fDefaultLevel, msg);
end;

procedure TLoggerBase.Log(const msg: string; const e: Exception);
begin
  Log(fDefaultLevel, msg, e);
end;

procedure TLoggerBase.Log(const fmt: string; const args: array of const);
begin
  Log(fDefaultLevel, fmt, args);
end;

procedure TLoggerBase.Log(const fmt: string; const args: array of const;
  const e: Exception);
begin
  Log(fDefaultLevel, fmt, args, e);
end;

procedure TLoggerBase.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const e: Exception);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, Format(fmt, args), e));
end;

procedure TLoggerBase.LogValue(level: TLogLevel; const name: string;
  const value: TValue);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Value, name, nil, value));
end;

procedure TLoggerBase.LogValue(const name: string; const value: TValue);
begin
  LogValue(fDefaultLevel, name, value);
end;

procedure TLoggerBase.SetDefaultLevel(value: TLogLevel);
begin
  fDefaultLevel := value;
end;

procedure TLoggerBase.SetEnabled(value: Boolean);
begin
  fEnabled := value;
end;

procedure TLoggerBase.SetLevels(value: TLogLevels);
begin
  fLevels := value;
end;

procedure TLoggerBase.Text(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, Format(fmt, args)));
end;

procedure TLoggerBase.Text(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, Format(fmt, args), e));
end;

procedure TLoggerBase.Text(const msg: string);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, msg));
end;

procedure TLoggerBase.Text(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, msg, e));
end;

function TLoggerBase.Track(level: TLogLevel; const classType: TClass;
  const methodName: string): IInterface;
begin
  if IsEnabled(level) then
    Result := TLogTracking.Create(Self, level, classType, methodName)
  else
    Result := nil;
end;

function TLoggerBase.Track(const instance: TObject;
  const methodName: string): IInterface;
begin
  Result := Track(instance.ClassType, methodName);
end;

function TLoggerBase.Track(const classType: TClass;
  const methodName: string): IInterface;
begin
  Result := Track(classType, methodName);
end;

procedure TLoggerBase.Trace(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, Format(fmt, args)));
end;

procedure TLoggerBase.Trace(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, Format(fmt, args), e));
end;

procedure TLoggerBase.Trace(const msg: string);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, msg));
end;

procedure TLoggerBase.Trace(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, msg, e));
end;

procedure TLoggerBase.Warn(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, Format(fmt, args)));
end;

procedure TLoggerBase.Warn(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, Format(fmt, args), e));
end;

procedure TLoggerBase.Warn(const msg: string);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, msg));
end;

procedure TLoggerBase.Warn(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, msg, e));
end;

{$ENDREGION}


{$REGION 'TLogger'}

constructor TLogger.Create(const controller: ILoggerController);
begin
  Guard.CheckNotNull(controller, 'controller');
  inherited Create;

  fController := controller;
end;

procedure TLogger.DoLog(const entry: TLogEntry);
begin
  fController.Send(entry);
end;

{$ENDREGION}


{$REGION 'TLoggerBase.TLogTracking'}

constructor TLoggerBase.TLogTracking.Create(const logger: ILogger;
  level: TLogLevel; const classType: TClass; const methodName: string);
begin
  inherited Create;
  fLogger := logger;
  fLevel := level;
  fClassType := classType;
  fMethodName := methodName;
  fLogger.Entering(fLevel, fClassType, fMethodName);
end;

destructor TLoggerBase.TLogTracking.Destroy;
begin
  fLogger.Leaving(fLevel, fClassType, fMethodName);
  inherited;
end;

{$ENDREGION}


end.
