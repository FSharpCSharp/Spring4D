{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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
{                                                                           }
{                        KEEP IT SIMPLE & FAST!!!                           }
{                                                                           }
{***************************************************************************}

unit Spring.Logging.Core.Logger;

{$I Spring.inc}

{$SCOPEDENUMS ON}  // Enable Scoped Enumerations Synax

interface

uses
  Classes,
  SysUtils,
  Spring.System,
  Spring.Collections,
  Spring.Resources,
  Spring.Logging,
  Spring.Logging.Core;

type
  TLogger = class(TInterfaceBase, ILogger, IAppenderAttachable, IInterface)
  private
    type
      TLoggerWalkProc = reference to function(logger: TLogger): Boolean;
  strict private
    fName: string;
    fAdditivity: Boolean;
    fLevel: TLevel;
    fParent: TLogger;
    fAppenderAttachable: IAppenderAttachable;
    fIsDebugEnabled: Boolean;
    fIsInfoEnabled: Boolean;
    fIsWarnEnabled: Boolean;
    fIsErrorEnabled: Boolean;
    fIsFatalEnabled: Boolean;
    function GetIsDebugEnabled: Boolean;
    function GetIsInfoEnabled: Boolean;
    function GetIsWarnEnabled: Boolean;
    function GetIsErrorEnabled: Boolean;
    function GetIsFatalEnabled: Boolean;
  protected
    function GetAdditivity: Boolean; virtual;
    function GetEffectiveLevel: TLevel; virtual;
    function GetLevel: TLevel; virtual;
    function GetParent: TLogger; virtual;
    procedure SetAdditivity(const value: Boolean); virtual;
    procedure SetParent(const value: TLogger); virtual;
    procedure SetLevel(const value: TLevel); virtual;
  protected
    fHierarchy: ILoggerRepository;
    fAppenderAttachableLock: IReadWriteSync;
    function GetAppenderAttachable: IAppenderAttachable;
    function GetAppenders: ICollection<IAppender>;
    procedure CallAppenders(const loggingEvent: TLoggingEvent); virtual;
    procedure DoLog(const level: TLevel; const msg: string; e: Exception); virtual;
    procedure WalkThroughLoggers(logger: TLogger; callback: TLoggerWalkProc);
//    function IsEnabledFor(const level: TLevel): Boolean; virtual;
//    property AppenderManager: IAppenderAttachable read fAppenderManager;
  public
    constructor Create(const name: string);
    procedure CloseNestedAppenders;
    { IAppenderAttachable }
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAllAppenders;
    function GetAppender(const name: string): IAppender;
    function RemoveAppender(const name: string): IAppender; overload;
    function RemoveAppender(const appender: IAppender): IAppender; overload;
    property Appenders: ICollection<IAppender> read GetAppenders;
    { ILogger }
    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const format: string; const args: array of const);
    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const format: string; const args: array of const);
    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const format: string; const args: array of const);
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const format: string; const args: array of const);
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const format: string; const args: array of const);
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
    { Properties }
    property Name: string read fName;
    property Additivity: Boolean read GetAdditivity write SetAdditivity;
    property EffectiveLevel: TLevel read GetEffectiveLevel;
    property Level: TLevel read GetLevel write SetLevel;
    property Parent: TLogger read GetParent write SetParent;
    property Hierarchy: ILoggerRepository read fHierarchy write fHierarchy;
  end;


implementation

uses Spring.Logging.Appenders;


{$IFDEF SUPPORTS_REGION} {$REGION 'TLogger'} {$ENDIF}

constructor TLogger.Create(const name: string);
begin
  inherited Create;
  fName := name;
  fAdditivity := True;
  fAppenderAttachableLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure TLogger.CallAppenders(const loggingEvent: TLoggingEvent);
var
  logger: TLogger;
  collection: ICollection<IAppender>;
  appender: IAppender;
begin
  Assert(loggingEvent <> nil, 'loggingEvent');
  logger := Self;
  while logger <> nil do
  begin
    logger.fAppenderAttachableLock.BeginRead;
    try
      if logger.fAppenderAttachable <> nil then
      begin
        collection := GetAppenderAttachable.Appenders;
        for appender in collection do
        begin
          appender.Append(loggingEvent);
        end;
      end;
    finally
      logger.fAppenderAttachableLock.EndWrite;
    end;
    if not logger.Additivity then Break;
    logger := logger.Parent;
  end;
end;

procedure TLogger.CloseNestedAppenders;
var
  appenders: ICollection<IAppender>;
  appender: IAppender;
begin
  fAppenderAttachableLock.BeginWrite;
  try
    if fAppenderAttachable <> nil then
    begin
      appenders := fAppenderAttachable.GetAppenders;
      for appender in appenders do
      begin
        if Supports(appender, IAppenderAttachable) then
        begin
          appender.Close;
        end;
      end;
    end;
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

procedure TLogger.DoLog(const level: TLevel; const msg: string; e: Exception);
var
  loggingEvent: TLoggingEvent;
begin
  // TODO: Create loggingEvent
  loggingEvent := TLoggingEvent.Create;
  try
    CallAppenders(loggingEvent);
  finally
    loggingEvent.Free;
  end;
end;

function TLogger.GetAppenderAttachable: IAppenderAttachable;
begin
  if fAppenderAttachable = nil then
  begin
    fAppenderAttachable := TAppenderAttachable.Create;
  end;
  Result := fAppenderAttachable;
end;

procedure TLogger.AddAppender(const appender: IAppender);
begin
  TArgument.CheckNotNull(appender, 'appender');
  fAppenderAttachableLock.BeginWrite;
  try
    GetAppenderAttachable.AddAppender(appender);
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

procedure TLogger.RemoveAllAppenders;
begin
  fAppenderAttachableLock.BeginWrite;
  try
    GetAppenderAttachable.RemoveAllAppenders;
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

function TLogger.GetAppenders: ICollection<IAppender>;
begin
  fAppenderAttachableLock.BeginRead;
  try
    Result := GetAppenderAttachable.GetAppenders;
  finally
    fAppenderAttachableLock.EndRead;
  end;
end;

function TLogger.GetAppender(const name: string): IAppender;
begin
  fAppenderAttachableLock.BeginRead;
  try
    Result := GetAppenderAttachable.GetAppender(name);
  finally
    fAppenderAttachableLock.EndRead;
  end;
end;

function TLogger.RemoveAppender(const name: string): IAppender;
begin
  fAppenderAttachableLock.BeginWrite;
  try
    Result := GetAppenderAttachable.RemoveAppender(name);
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

function TLogger.RemoveAppender(const appender: IAppender): IAppender;
begin
  TArgument.CheckNotNull(appender, 'appender');
  fAppenderAttachableLock.BeginWrite;
  try
    Result := GetAppenderAttachable.RemoveAppender(appender);
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

function TLogger.GetAdditivity: Boolean;
begin
  Result := fAdditivity;
end;

function TLogger.GetLevel: TLevel;
begin
  Result := fLevel;
end;

function TLogger.GetParent: TLogger;
begin
  Result := fParent;
end;

function TLogger.GetEffectiveLevel: TLevel;
var
  logger: TLogger;
begin
  Result := fLevel;
  logger := Self;
  while Result.IsNull do
  begin
    logger := logger.Parent;
    Result := logger.Level;
  end;
  Assert(not Result.IsNull, 'EffectiveLevel should never be null.');
end;

procedure TLogger.SetAdditivity(const value: Boolean);
begin
  fAdditivity := value;
end;

procedure TLogger.SetLevel(const value: TLevel);
begin
  fLevel := value;
  fIsDebugEnabled := fLevel >= TLevel.Debug;
  fIsInfoEnabled:= fLevel >= TLevel.Info;
  fIsWarnEnabled:= fLevel >= TLevel.Warn;
  fIsErrorEnabled:= fLevel >= TLevel.Error;
  fIsFatalEnabled:= fLevel >= TLevel.Fatal;
end;

procedure TLogger.SetParent(const value: TLogger);
begin
  fParent := value;
end;

procedure TLogger.Debug(const msg: string);
begin
  if fIsDebugEnabled then
  begin
    DoLog(TLevel.Debug, msg, nil);
  end;
end;

procedure TLogger.Debug(const msg: string; e: Exception);
begin
  if fIsDebugEnabled then
  begin
    DoLog(TLevel.Debug, msg, e);
  end;
end;

procedure TLogger.DebugFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if fIsDebugEnabled then
  begin
    msg := SysUtils.Format(format, args);
    DoLog(TLevel.Debug, msg, nil);
  end;
end;

procedure TLogger.Info(const msg: string);
begin
  if fIsInfoEnabled then
  begin
    DoLog(TLevel.Info, msg, nil);
  end;
end;

procedure TLogger.Info(const msg: string; e: Exception);
begin
  if fIsInfoEnabled then
  begin
    DoLog(TLevel.Info, msg, e);
  end;
end;

procedure TLogger.InfoFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if fIsInfoEnabled then
  begin
    msg := SysUtils.Format(format, args);
    DoLog(TLevel.Info, msg, nil);
  end;
end;

procedure TLogger.Warn(const msg: string);
begin
  if fIsWarnEnabled then
  begin
    DoLog(TLevel.Warn, msg, nil);
  end;
end;

procedure TLogger.WalkThroughLoggers(logger: TLogger; callback: TLoggerWalkProc);
begin
  while logger <> nil do
  begin
    callback(logger);
    logger := logger.Parent;
  end;
end;

procedure TLogger.Warn(const msg: string; e: Exception);
begin
  if fIsWarnEnabled then
  begin
    DoLog(TLevel.Warn, msg, nil);
  end;
end;

procedure TLogger.WarnFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if fIsWarnEnabled then
  begin
    msg := SysUtils.Format(format, args);
    DoLog(TLevel.Warn, msg, nil);
  end;
end;

procedure TLogger.Error(const msg: string);
begin
  if fIsErrorEnabled then
  begin
    DoLog(TLevel.Error, msg, nil);
  end;
end;

procedure TLogger.Error(const msg: string; e: Exception);
begin
  if fIsErrorEnabled then
  begin
    DoLog(TLevel.Error, msg, nil);
  end;
end;

procedure TLogger.ErrorFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if fIsErrorEnabled then
  begin
    msg := SysUtils.Format(format, args);
    DoLog(TLevel.Error, msg, nil);
  end;
end;

procedure TLogger.Fatal(const msg: string);
begin
  if fIsFatalEnabled then
  begin
    DoLog(TLevel.Fatal, msg, nil);
  end;
end;

procedure TLogger.Fatal(const msg: string; e: Exception);
begin
  if fIsFatalEnabled then
  begin
    DoLog(TLevel.Fatal, msg, nil);
  end;
end;

procedure TLogger.FatalFormat(const format: string; const args: array of const);
var
  msg: string;
begin
  if fIsFatalEnabled then
  begin
    msg := SysUtils.Format(format, args);
    DoLog(TLevel.Fatal, msg, nil);
  end;
end;

function TLogger.GetIsDebugEnabled: Boolean;
begin
  Result := fIsDebugEnabled;
end;

function TLogger.GetIsInfoEnabled: Boolean;
begin
  Result := fIsInfoEnabled;
end;

function TLogger.GetIsWarnEnabled: Boolean;
begin
  Result := fIsWarnEnabled;
end;

function TLogger.GetIsErrorEnabled: Boolean;
begin
  Result := fIsErrorEnabled;
end;

function TLogger.GetIsFatalEnabled: Boolean;
begin
  Result := fIsFatalEnabled;
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}

end.
