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
    function IsVerboseEnabled: Boolean;

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

    procedure Entering(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;
    procedure Entering(level: TLogLevel; const classType: TClass;
      const methodName: string; const arguments: array of TValue); overload;

    procedure Leaving(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string; const arguments: array of TValue): IInterface; overload;
  end;
  {$ENDREGION}

  {$REGION 'TLoggerBase'}
  TLoggerBase = class abstract(TInterfacedObject, ILogger, ILogAppender,
    ILoggerProperties)
  private
    fEnabled: Boolean;
    fLevels: TLogLevels;

    function GetLevels: TLogLevels;
    function GetEnabled: Boolean;

    procedure SetLevels(value: TLogLevels);
    procedure SetEnabled(value: Boolean);
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
    function IsVerboseEnabled: Boolean;

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

    procedure Entering(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;
    procedure Entering(level: TLogLevel; const classType: TClass;
      const methodName: string; const arguments: array of TValue); overload;

    procedure Leaving(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string; const arguments: array of TValue): IInterface; overload;

    property Enabled: Boolean read fEnabled write fEnabled;
    property Levels: TLogLevels read fLevels write fLevels;
  end;
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

type
  TTrackingImpl = class(TInterfacedObject, IInterface)
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

{$REGION 'TNullLogger'}
{ TNullLogger }

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
  const exc: Exception);
begin

end;

procedure TNullLogger.Debug(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Debug(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

procedure TNullLogger.Debug(const msg: string);
begin

end;

procedure TNullLogger.Debug(const msg: string; const exc: Exception);
begin

end;

procedure TNullLogger.Error(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Entering(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin

end;

procedure TNullLogger.Entering(level: TLogLevel; const classType: TClass;
  const methodName: string; const arguments: array of TValue);
begin

end;

procedure TNullLogger.Error(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

procedure TNullLogger.Error(const msg: string);
begin

end;

procedure TNullLogger.Error(const msg: string; const exc: Exception);
begin

end;

procedure TNullLogger.Fatal(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Fatal(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

function TNullLogger.GetEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.GetLevels: TLogLevels;
begin
  Result := [];
end;

procedure TNullLogger.Fatal(const msg: string);
begin

end;

procedure TNullLogger.Fatal(const msg: string; const exc: Exception);
begin

end;

procedure TNullLogger.Info(const msg: string; const exc: Exception);
begin

end;

procedure TNullLogger.Info(const msg: string);
begin

end;

procedure TNullLogger.Info(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

function TNullLogger.IsDebugEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.IsEnabled(level: TLogLevel): Boolean;
begin
  Result := false;
end;

function TNullLogger.IsErrorEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.IsFatalEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.IsInfoEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.IsTextEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.IsVerboseEnabled: Boolean;
begin
  Result := false;
end;

function TNullLogger.IsWarnEnabled: Boolean;
begin
  Result := false;
end;

procedure TNullLogger.Info(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Leaving(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin

end;

procedure TNullLogger.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const exc: Exception);
begin

end;

procedure TNullLogger.Text(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Text(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

function TNullLogger.Track(level: TLogLevel; const classType: TClass;
  const methodName: string): IInterface;
begin
  Result := nil;
end;

function TNullLogger.Track(level: TLogLevel; const classType: TClass;
  const methodName: string; const arguments: array of TValue): IInterface;
begin
  Result := nil;
end;

procedure TNullLogger.Text(const msg: string);
begin

end;

procedure TNullLogger.Text(const msg: string; const exc: Exception);
begin

end;

procedure TNullLogger.Verbose(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Verbose(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

procedure TNullLogger.Verbose(const msg: string);
begin

end;

procedure TNullLogger.Verbose(const msg: string; const exc: Exception);
begin

end;

procedure TNullLogger.Warn(const fmt: string; const args: array of const);
begin

end;

procedure TNullLogger.Warn(const fmt: string; const args: array of const;
  const exc: Exception);
begin

end;

procedure TNullLogger.Warn(const msg: string);
begin

end;

procedure TNullLogger.Warn(const msg: string; const exc: Exception);
begin

end;
{$ENDREGION}

{$REGION 'TLoggerBase'}
{ TLoggerBase }

constructor TLoggerBase.Create;
begin
  inherited;

  fEnabled := true;
  fLevels := LOG_BASIC_LEVELS;
end;

procedure TLoggerBase.Debug(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Debug)) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, Format(fmt, args)));
end;

procedure TLoggerBase.Debug(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Debug)) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, Format(fmt, args), exc));
end;

procedure TLoggerBase.Debug(const msg: string);
begin
  if (IsEnabled(TLogLevel.Debug)) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, msg));
end;

procedure TLoggerBase.Debug(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Debug)) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, msg, exc));
end;

procedure TLoggerBase.Error(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Error)) then
    DoLog(TLogEntry.Create(TLogLevel.Error, Format(fmt, args)));
end;

procedure TLoggerBase.Entering(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Entering, methodName,
      classType));
end;

procedure TLoggerBase.Entering(level: TLogLevel; const classType: TClass;
  const methodName: string; const arguments: array of TValue);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Entering, methodName,
      classType, TValue.From<TArray<TValue>>(TArray.Copy<TValue>(arguments))));
end;

procedure TLoggerBase.Error(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Error)) then
    DoLog(TLogEntry.Create(TLogLevel.Error, Format(fmt, args), exc));
end;

procedure TLoggerBase.Error(const msg: string);
begin
  if (IsEnabled(TLogLevel.Error)) then
    DoLog(TLogEntry.Create(TLogLevel.Error, msg));
end;

procedure TLoggerBase.Error(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Error)) then
    DoLog(TLogEntry.Create(TLogLevel.Error, msg, exc));
end;

procedure TLoggerBase.Fatal(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Fatal)) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, Format(fmt, args)));
end;

procedure TLoggerBase.Fatal(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Fatal)) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, Format(fmt, args), exc));
end;

function TLoggerBase.GetEnabled: Boolean;
begin
  Result := Enabled;
end;

function TLoggerBase.GetLevels: TLogLevels;
begin
  Result := Levels;
end;

procedure TLoggerBase.Fatal(const msg: string);
begin
  if (IsEnabled(TLogLevel.Fatal)) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, msg));
end;

procedure TLoggerBase.Fatal(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Fatal)) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, msg, exc));
end;

procedure TLoggerBase.Info(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Info)) then
    DoLog(TLogEntry.Create(TLogLevel.Info, msg, exc));
end;

procedure TLoggerBase.Info(const msg: string);
begin
  if (IsEnabled(TLogLevel.Info)) then
    DoLog(TLogEntry.Create(TLogLevel.Info, msg));
end;

procedure TLoggerBase.Info(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Info)) then
    DoLog(TLogEntry.Create(TLogLevel.Info, Format(fmt, args), exc));
end;

procedure TLoggerBase.Info(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Info)) then
    DoLog(TLogEntry.Create(TLogLevel.Info, Format(fmt, args)));
end;

function TLoggerBase.IsDebugEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Debug);
end;

function TLoggerBase.IsEnabled(level: TLogLevel): Boolean;
begin
  Result:=fEnabled and (level in fLevels);
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

function TLoggerBase.IsVerboseEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Verbose);
end;

function TLoggerBase.IsWarnEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Warning);
end;

procedure TLoggerBase.Log(const entry: TLogEntry);
begin
  if (IsEnabled(entry.Level)) then
    DoLog(entry);
end;

procedure TLoggerBase.Log(level: TLogLevel; const msg: string;
  const exc: Exception);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, msg, exc));
end;

procedure TLoggerBase.Log(level: TLogLevel; const msg: string);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, msg));
end;

procedure TLoggerBase.Log(level: TLogLevel; const fmt: string;
  const args: array of const);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, Format(fmt, args)));
end;

procedure TLoggerBase.Leaving(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Leaving, methodName,
      classType));
end;

procedure TLoggerBase.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const exc: Exception);
begin
  if (IsEnabled(level)) then
    DoLog(TLogEntry.Create(level, Format(fmt, args), exc));
end;

procedure TLoggerBase.SetEnabled(value: Boolean);
begin
  Enabled := value;
end;

procedure TLoggerBase.SetLevels(value: TLogLevels);
begin
  Levels := value;
end;

procedure TLoggerBase.Text(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Text)) then
    DoLog(TLogEntry.Create(TLogLevel.Text, Format(fmt, args)));
end;

procedure TLoggerBase.Text(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Text)) then
    DoLog(TLogEntry.Create(TLogLevel.Text, Format(fmt, args), exc));
end;

function TLoggerBase.Track(level: TLogLevel; const classType: TClass;
  const methodName: string): IInterface;
begin
  if (IsEnabled(level)) then
  begin
    Entering(level, classType, methodName);
    Result := TTrackingImpl.Create(Self, level, classType, methodName);
  end
  else Result := nil;
end;

function TLoggerBase.Track(level: TLogLevel; const classType: TClass;
  const methodName: string; const arguments: array of TValue): IInterface;
begin
  if (IsEnabled(level)) then
  begin
    Entering(level, classType, methodName, arguments);
    Result := TTrackingImpl.Create(Self, level, classType, methodName);
  end
  else Result := nil;
end;

procedure TLoggerBase.Text(const msg: string);
begin
  if (IsEnabled(TLogLevel.Text)) then
    DoLog(TLogEntry.Create(TLogLevel.Text, msg));
end;

procedure TLoggerBase.Text(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Text)) then
    DoLog(TLogEntry.Create(TLogLevel.Text, msg, exc));
end;

procedure TLoggerBase.Verbose(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Verbose)) then
    DoLog(TLogEntry.Create(TLogLevel.Verbose, Format(fmt, args)));
end;

procedure TLoggerBase.Verbose(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Verbose)) then
    DoLog(TLogEntry.Create(TLogLevel.Verbose, Format(fmt, args), exc));
end;

procedure TLoggerBase.Verbose(const msg: string);
begin
  if (IsEnabled(TLogLevel.Verbose)) then
    DoLog(TLogEntry.Create(TLogLevel.Verbose, msg));
end;

procedure TLoggerBase.Verbose(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Verbose)) then
    DoLog(TLogEntry.Create(TLogLevel.Verbose, msg, exc));
end;

procedure TLoggerBase.Warn(const fmt: string; const args: array of const);
begin
  if (IsEnabled(TLogLevel.Warning)) then
    DoLog(TLogEntry.Create(TLogLevel.Warning, Format(fmt, args)));
end;

procedure TLoggerBase.Warn(const fmt: string; const args: array of const;
  const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Warning)) then
    DoLog(TLogEntry.Create(TLogLevel.Warning, Format(fmt, args), exc));
end;

procedure TLoggerBase.Warn(const msg: string);
begin
  if (IsEnabled(TLogLevel.Warning)) then
    DoLog(TLogEntry.Create(TLogLevel.Warning, msg));
end;

procedure TLoggerBase.Warn(const msg: string; const exc: Exception);
begin
  if (IsEnabled(TLogLevel.Warning)) then
    DoLog(TLogEntry.Create(TLogLevel.Warning, msg, exc));
end;
{$ENDREGION}

{$REGION 'TLogger'}
{ TLogger }

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

{ TTrackingImpl }

constructor TTrackingImpl.Create(const logger: ILogger; level: TLogLevel;
  const classType: TClass; const methodName: string);
begin
  inherited Create;
  fLogger := logger;
  fLevel := level;
  fClassType := classType;
  fMethodName := methodName;
end;

destructor TTrackingImpl.Destroy;
begin
  fLogger.Leaving(fLevel, fClassType, fMethodName);
  inherited;
end;

end.
