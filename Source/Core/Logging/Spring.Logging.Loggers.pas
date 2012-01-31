{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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
  Windows,
  SysUtils,
  Spring.Services.Logging,
  Spring.Logging.Core;

type
  TLoggerBase = class abstract(TInterfacedObject, ILogger)
  private
    fName: string;
    fLevel: TLevel;
    function GetName: string;
    function GetIsDebugEnabled: Boolean;
    function GetIsInfoEnabled: Boolean;
    function GetIsWarnEnabled: Boolean;
    function GetIsErrorEnabled: Boolean;
    function GetIsFatalEnabled: Boolean;
  protected
    function GetLevel: TLevel; virtual;
    procedure SetLevel(const value: TLevel); virtual;
  protected
    function ShouldLog(const level: TLevel): Boolean; virtual;
    procedure CreateLoggingEvent(const level: TLevel; const msg: string; e: Exception; out event: TLoggingEvent);
    procedure Log(const level: TLevel; const msg: string; ex: Exception); overload; virtual;
    procedure InternalLog(const event: TLoggingEvent); overload; virtual;
    procedure HandleException(e: Exception); virtual;
  public
    constructor Create(const name: string);

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const format: string; const args: array of const); overload;
    procedure DebugFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const format: string; const args: array of const); overload;
    procedure InfoFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const format: string; const args: array of const); overload;
    procedure WarnFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const format: string; const args: array of const); overload;
    procedure ErrorFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const format: string; const args: array of const); overload;
    procedure FatalFormat(const format: string; const args: array of const; e: Exception); overload;

    property Name: string read GetName;
    property Level: TLevel read GetLevel write SetLevel;

    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  ///	<summary>
  ///	  Provides the default implementation of ILogger which supports hierarchy.
  ///	</summary>
  TLogger = class(TLoggerBase, IHierarchicalLogger)
  strict private
    fRepository: Pointer;
    fParent: IHierarchicalLogger;
    fAdditivity: Boolean;
  protected
    function GetAdditivity: Boolean; virtual;
    function GetParent: IHierarchicalLogger; virtual;
    procedure SetAdditivity(const value: Boolean); virtual;
    procedure SetParent(const value: IHierarchicalLogger); virtual;
  private
    function GetRepository: ILoggerManager;
  protected
    function ShouldLog(const level: TLevel): Boolean; override;
    procedure InternalLog(const event: TLoggingEvent); override;
  public
    constructor Create(const repository: ILoggerManager; const name: string);
    function GetEffectiveLevel: TLevel; virtual;
    property Additivity: Boolean read GetAdditivity write SetAdditivity;
    property Parent: IHierarchicalLogger read GetParent;
    property Repository: ILoggerManager read GetRepository;
  end;

  /// <summary>
  /// Internal implementation for the unique root logger.
  /// </summary>
  TRootLogger = class sealed(TLogger)
  protected
    procedure SetLevel(const value: TLevel); override;
  public
    constructor Create(const repository: ILoggerManager);
    function GetEffectiveLevel: TLevel; override;
  end;

  TNullLogger = class(TInterfacedObject, ILogger)
  protected
    function GetName: string;
    function GetIsDebugEnabled: Boolean;
    function GetIsInfoEnabled: Boolean;
    function GetIsWarnEnabled: Boolean;
    function GetIsErrorEnabled: Boolean;
    function GetIsFatalEnabled: Boolean;
  public
    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const format: string; const args: array of const); overload;
    procedure DebugFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const format: string; const args: array of const); overload;
    procedure InfoFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const format: string; const args: array of const); overload;
    procedure WarnFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const format: string; const args: array of const); overload;
    procedure ErrorFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const format: string; const args: array of const); overload;
    procedure FatalFormat(const format: string; const args: array of const; e: Exception); overload;

    property Name: string read GetName;

    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

implementation

uses
  Spring.Logging.Utils;

{$REGION 'TLoggerBase'}

constructor TLoggerBase.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

procedure TLoggerBase.CreateLoggingEvent(const level: TLevel; const msg: string; e: Exception; out event: TLoggingEvent);
begin
  event.LoggerName := Self.Name;
  event.LoggerLevel := level;
  event.TimeStamp := Now;
  event.ThreadID := GetCurrentThreadId;
  event.Message := msg;
  if e <> nil then
  begin
    event.ErrorMessage := e.ClassName + ': ' + e.Message;
  end;
end;

procedure TLoggerBase.Log(const level: TLevel; const msg: string;
  ex: Exception);
var
  event: TLoggingEvent;
begin
  try
    CreateLoggingEvent(level, msg, ex, event);
    InternalLog(event);
  except on e: Exception do
    HandleException(e);
  end;
end;

function TLoggerBase.ShouldLog(const level: TLevel): Boolean;
begin
  Result := (level <> nil) and
    level.IsGreaterThanOrEqualTo(fLevel);
end;

procedure TLoggerBase.InternalLog(const event: TLoggingEvent);
begin
end;

procedure TLoggerBase.HandleException(e: Exception);
begin
  InternalLogger.Debug('Failed to log event.', e);
end;

procedure TLoggerBase.Debug(const msg: string);
begin
  if IsDebugEnabled then
  begin
    Log(TLevel.Debug, msg, nil);
  end;
end;

procedure TLoggerBase.Debug(const msg: string; e: Exception);
begin
  if IsDebugEnabled then
  begin
    Log(TLevel.Debug, msg, e);
  end;
end;

procedure TLoggerBase.DebugFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if IsDebugEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Debug, msg, nil);
  end;
end;

procedure TLoggerBase.DebugFormat(const format: string;
  const args: array of const; e: Exception);
var
  msg: string;
begin
  if IsDebugEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Debug, msg, e);
  end;
end;

procedure TLoggerBase.Info(const msg: string);
begin
  if IsInfoEnabled then
  begin
    Log(TLevel.Info, msg, nil);
  end;
end;

procedure TLoggerBase.Info(const msg: string; e: Exception);
begin
  if IsInfoEnabled then
  begin
    Log(TLevel.Info, msg, e);
  end;
end;

procedure TLoggerBase.InfoFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if IsInfoEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Info, msg, nil);
  end;
end;

procedure TLoggerBase.InfoFormat(const format: string;
  const args: array of const; e: Exception);
var
  msg: string;
begin
  if IsInfoEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Info, msg, e);
  end;
end;

procedure TLoggerBase.Warn(const msg: string);
begin
  if IsWarnEnabled then
  begin
    Log(TLevel.Warn, msg, nil);
  end;
end;

procedure TLoggerBase.Warn(const msg: string; e: Exception);
begin
  if IsWarnEnabled then
  begin
    Log(TLevel.Warn, msg, e);
  end;
end;

procedure TLoggerBase.WarnFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if IsWarnEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Warn, msg, nil);
  end;
end;

procedure TLoggerBase.WarnFormat(const format: string;
  const args: array of const; e: Exception);
var
  msg: string;
begin
  if IsWarnEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Warn, msg, e);
  end;
end;

procedure TLoggerBase.Error(const msg: string);
begin
  if IsErrorEnabled then
  begin
    Log(TLevel.Error, msg, nil);
  end;
end;

procedure TLoggerBase.Error(const msg: string; e: Exception);
begin
  if IsErrorEnabled then
  begin
    Log(TLevel.Error, msg, e);
  end;
end;

procedure TLoggerBase.ErrorFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if IsErrorEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Error, msg, nil);
  end;
end;

procedure TLoggerBase.ErrorFormat(const format: string;
  const args: array of const; e: Exception);
var
  msg: string;
begin
  if IsErrorEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Error, msg, e);
  end;
end;

procedure TLoggerBase.Fatal(const msg: string);
begin
  if IsFatalEnabled then
  begin
    Log(TLevel.Fatal, msg, nil);
  end;
end;

procedure TLoggerBase.Fatal(const msg: string; e: Exception);
begin
  if IsFatalEnabled then
  begin
    Log(TLevel.Fatal, msg, e);
  end;
end;

procedure TLoggerBase.FatalFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if IsFatalEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Fatal, msg, nil);
  end;
end;

procedure TLoggerBase.FatalFormat(const format: string;
  const args: array of const; e: Exception);
var
  msg: string;
begin
  if IsFatalEnabled then
  begin
    msg := SysUtils.Format(format, args);
    Log(TLevel.Fatal, msg, e);
  end;
end;

function TLoggerBase.GetName: string;
begin
  Result := fName;
end;

function TLoggerBase.GetLevel: TLevel;
begin
  Result := fLevel;
end;

function TLoggerBase.GetIsDebugEnabled: Boolean;
begin
  Result := ShouldLog(TLevel.Debug);
end;

function TLoggerBase.GetIsInfoEnabled: Boolean;
begin
  Result := ShouldLog(TLevel.Info);
end;

function TLoggerBase.GetIsWarnEnabled: Boolean;
begin
  Result := ShouldLog(TLevel.Warn);
end;

function TLoggerBase.GetIsErrorEnabled: Boolean;
begin
  Result := ShouldLog(TLevel.Error);
end;

function TLoggerBase.GetIsFatalEnabled: Boolean;
begin
  Result := ShouldLog(TLevel.Fatal);
end;

procedure TLoggerBase.SetLevel(const value: TLevel);
begin
  fLevel := value;
end;

{$ENDREGION}


{$REGION 'TLogger'}

constructor TLogger.Create(const repository: ILoggerManager;
  const name: string);
begin
  inherited Create(name);
  fRepository := Pointer(repository);
  fAdditivity := True;
end;

procedure TLogger.InternalLog(const event: TLoggingEvent);
begin
  Repository.SendLoggingEvent(Self, event);
end;

function TLogger.GetAdditivity: Boolean;
begin
  Result := fAdditivity;
end;

function TLogger.GetParent: IHierarchicalLogger;
begin
  Result := fParent;
end;

function TLogger.GetRepository: ILoggerManager;
begin
  Result := ILoggerManager(fRepository);
end;

function TLogger.ShouldLog(const level: TLevel): Boolean;
begin
  Result := (level <> nil) and
    level.IsGreaterThanOrEqualTo(Repository.Threshold) and
    level.IsGreaterThanOrEqualTo(GetEffectiveLevel);
end;

function TLogger.GetEffectiveLevel: TLevel;
var
  logger: IHierarchicalLogger;
begin
  Result := fLevel;
  if Result = nil then
  begin
    logger := Parent;
    while (Result = nil) and (logger <> nil) do
    begin
      Result := logger.Level;
      logger := logger.Parent;
    end;
  end;
end;

procedure TLogger.SetParent(const value: IHierarchicalLogger);
begin
  fParent := value;
end;

procedure TLogger.SetAdditivity(const value: Boolean);
begin
  fAdditivity := value;
end;

{$ENDREGION}


{$REGION 'TRootLogger'}

constructor TRootLogger.Create(const repository: ILoggerManager);
begin
  inherited Create(repository, 'root');
  SetLevel(TLevel.Debug);
end;

function TRootLogger.GetEffectiveLevel: TLevel;
begin
  Result := Self.Level;
end;

procedure TRootLogger.SetLevel(const value: TLevel);
begin
  if value = nil then
  begin
    InternalLogger.Debug('RootLogger: You have tried to set a null level to root.');
  end
  else
  begin
    inherited SetLevel(value);
  end;
end;

{$ENDREGION}


{$REGION 'TNullLogger'}

procedure TNullLogger.Debug(const msg: string; e: Exception);
begin

end;

procedure TNullLogger.Debug(const msg: string);
begin

end;

procedure TNullLogger.DebugFormat(const format: string; const args: array of const; e: Exception);
begin

end;

procedure TNullLogger.DebugFormat(const format: string; const args: array of const);
begin

end;

procedure TNullLogger.Error(const msg: string; e: Exception);
begin

end;

procedure TNullLogger.Error(const msg: string);
begin

end;

procedure TNullLogger.ErrorFormat(const format: string; const args: array of const);
begin

end;

procedure TNullLogger.ErrorFormat(const format: string; const args: array of const; e: Exception);
begin

end;

procedure TNullLogger.Fatal(const msg: string; e: Exception);
begin

end;

procedure TNullLogger.Fatal(const msg: string);
begin

end;

procedure TNullLogger.FatalFormat(const format: string; const args: array of const; e: Exception);
begin

end;

procedure TNullLogger.FatalFormat(const format: string; const args: array of const);
begin

end;

procedure TNullLogger.Info(const msg: string);
begin

end;

procedure TNullLogger.Info(const msg: string; e: Exception);
begin

end;

procedure TNullLogger.InfoFormat(const format: string; const args: array of const; e: Exception);
begin

end;

procedure TNullLogger.InfoFormat(const format: string; const args: array of const);
begin

end;

procedure TNullLogger.Warn(const msg: string);
begin

end;

procedure TNullLogger.Warn(const msg: string; e: Exception);
begin

end;

procedure TNullLogger.WarnFormat(const format: string; const args: array of const; e: Exception);
begin

end;

procedure TNullLogger.WarnFormat(const format: string; const args: array of const);
begin

end;

function TNullLogger.GetIsDebugEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetIsErrorEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetIsFatalEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetIsInfoEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetIsWarnEnabled: Boolean;
begin
  Result := False;
end;

function TNullLogger.GetName: string;
begin
  Result := 'NULL';
end;

{$ENDREGION}

end.
