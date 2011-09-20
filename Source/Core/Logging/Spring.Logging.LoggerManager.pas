{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Logging.LoggerManager;

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  SyncObjs,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Services.Logging,
  Spring.Logging.Core;

type
  /// <summary>
  /// Hierarchical organization of loggers.
  /// </summary>
  TLoggerManager = class(TInterfacedObject, ILoggerManager, ILoggerFactory)
  private
    fLoggers: IDictionary<string, ILogger>;
    fAppenders: IList<IAppender>;
    fAppenderRefs: IDictionary<string, IList<IAppender>>;
    fLevels: IDictionary<string, TLevel>;
    fRoot: IHierarchicalLogger;
    fName: string;
    fThreshold: TLevel;
    fLock: TCriticalSection;
    fEmittedNoAppenderWarning: Boolean;
  private
    function GetName: string;
    function GetThreshold: TLevel;
    function GetRoot: IHierarchicalLogger;
    function GetAppenders: IList<IAppender>;
    procedure SetName(const value: string);
    procedure SetThreshold(const value: TLevel);
  protected
    function AddLogger(const name: string): IHierarchicalLogger;
    procedure Lock;
    procedure Unlock;
    procedure InitializeLevels;
    procedure UpdateParent(const logger: IHierarchicalLogger);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendLoggingEvent(const sender: ILogger; const event: TLoggingEvent);
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAppender(const appender: IAppender);
    procedure AddAppenderRef(const logger: ILogger; const appender: IAppender);
    procedure RemoveAppenderRef(const logger: ILogger; const appender: IAppender);
    function CreateAppender(const typeName: string): IAppender;
    function CreateLayout(const typeName: string): ILayout;
    function IsDisabled(const level: TLevel): Boolean;
    function FindLogger(const name: string): ILogger;
    function FindLevel(const name: string): TLevel;
    function FindAppender(const name: string): IAppender;
    function GetLoggers: IEnumerable<ILogger>;
    function GetDefaultLogger: ILogger;
    function GetLogger(const name: string): ILogger; overload;
    function GetLogger(typeInfo: PTypeInfo): ILogger; overload;
    property EmittedNoAppenderWarning: Boolean read fEmittedNoAppenderWarning write fEmittedNoAppenderWarning;
    property Name: string read GetName write SetName;
    property Threshold: TLevel read GetThreshold write SetThreshold;
    property Root: IHierarchicalLogger read GetRoot;
    property Appenders: IList<IAppender> read GetAppenders;
  end;

const
  CDefaultAppenderNamespace = 'Spring.Logging.Appenders';
  CDefaultLayoutNamespace = 'Spring.Logging.Layouts';

implementation

uses
  StrUtils,
  Spring.Reflection,
  Spring.Logging.Utils,
  Spring.Logging.ResourceStrings,
  Spring.Logging.Loggers,
  Spring.Logging.Appenders,
  Spring.Logging.Layouts;


{$REGION 'TLoggerManager'}

constructor TLoggerManager.Create;
begin
  inherited Create;
  fLevels := TDictionary<string, TLevel>.Create;
  fAppenders := TList<IAppender>.Create;
  fLoggers := TDictionary<string, ILogger>.Create;
  fAppenderRefs := TDictionary<string, IList<IAppender>>.Create;
  fRoot := TRootLogger.Create(Self);
  fLock := TCriticalSection.Create;
  fThreshold := TLevel.All;
  InitializeLevels;
end;

destructor TLoggerManager.Destroy;
begin
  fLoggers.Clear;
  fAppenders.Clear;
  fAppenderRefs.Clear;
  fLock.Free;
  inherited Destroy;
end;

procedure TLoggerManager.InitializeLevels;
  procedure AddLevel(level: TLevel);
  begin
    fLevels.Add(level.Name, level);
  end;
begin
  AddLevel(TLevel.All);
  AddLevel(TLevel.Debug);
  AddLevel(TLevel.Info);
  AddLevel(TLevel.Warn);
  AddLevel(TLevel.Error);
  AddLevel(TLevel.Fatal);
  AddLevel(TLevel.Off);
end;

function GetFullName(const typeName, defaultNamespace: string): string;
begin
  if not ContainsStr(typeName, '.') then
  begin
    Result := defaultNamespace + '.' + typeName;
  end
  else
  begin
    Result := typeName;
  end;
end;

function TLoggerManager.CreateAppender(const typeName: string): IAppender;
var
  fullName: string;
  instance: TObject;
begin
  fullName := GetFullName(typeName, CDefaultAppenderNamespace);
  instance := TActivator.CreateInstance(fullName);
  if not Supports(instance, IAppender, Result) then
  begin
    instance.Free;
  end;
end;

function TLoggerManager.CreateLayout(const typeName: string): ILayout;
var
  fullName: string;
  instance: TObject;
begin
  fullName := GetFullName(typeName, CDefaultLayoutNamespace);
  instance := TActivator.CreateInstance(fullName);
  if not Supports(instance, ILayout, Result) then
  begin
    instance.Free;
  end;
end;

function TLoggerManager.FindAppender(const name: string): IAppender;
var
  appender: IAppender;
begin
  Result := nil;
  Lock;
  try
    for appender in GetAppenders do
    begin
      if SameText(appender.Name, name) then
      begin
        Result := appender;
        Exit;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TLoggerManager.FindLevel(const name: string): TLevel;
begin
  Lock;
  try
    fLevels.TryGetValue(UpperCase(name), Result);
  finally
    Unlock;
  end;
end;

function TLoggerManager.FindLogger(const name: string): ILogger;
begin
  Lock;
  try
    fLoggers.TryGetValue(name, Result);
  finally
    Unlock;
  end;
end;

procedure TLoggerManager.AddAppender(const appender: IAppender);
begin
  Lock;
  try
    Appenders.Add(appender);
  finally
    Unlock;
  end;
end;

procedure TLoggerManager.RemoveAppender(const appender: IAppender);
begin
  Lock;
  try
    Appenders.Remove(appender);
  finally
    Unlock;
  end;
end;

procedure TLoggerManager.AddAppenderRef(const logger: ILogger; const appender: IAppender);
var
  appenders: IList<IAppender>;
begin
  CheckArgumentNotNull(logger, 'logger');
  CheckArgumentNotNull(appender, 'appender');

  Lock;
  try
    if not fAppenderRefs.TryGetValue(logger.Name, appenders) then
    begin
      appenders := TList<IAppender>.Create;
      fAppenderRefs[logger.Name] := appenders;
    end;
    appenders.Add(appender);
  finally
    Unlock;
  end;
end;

procedure TLoggerManager.RemoveAppenderRef(const logger: ILogger; const appender: IAppender);
var
  appenders: IList<IAppender>;
begin
  CheckArgumentNotNull(logger, 'logger');
  CheckArgumentNotNull(appender, 'appender');

  Lock;
  try
    appenders.Remove(appender);
  finally
    Unlock;
  end;
end;

function TLoggerManager.AddLogger(const name: string): IHierarchicalLogger;
begin
  Result := TLogger.Create(Self, name);
  fLoggers[name] := Result;
end;

function TLoggerManager.GetAppenders: IList<IAppender>;
begin
  Result := fAppenders;
end;

function TLoggerManager.GetDefaultLogger: ILogger;
begin
  Result := Root;
end;

function TLoggerManager.GetLoggers: IEnumerable<ILogger>;
begin
  Lock;
  try
    Result := fLoggers.Values;
  finally
    Unlock;
  end;
end;

function TLoggerManager.GetLogger(const name: string): ILogger;
var
  logger: ILogger;
begin
  Lock;
  try
    if not fLoggers.TryGetValue(name, logger) then
    begin
      logger := AddLogger(name);
      UpdateParent(logger as IHierarchicalLogger);
    end;
  finally
    Unlock;
  end;
  Result := logger;
end;

function TLoggerManager.GetLogger(typeInfo: PTypeInfo): ILogger;
var
  context: TRttiContext;
  typeObj: TRttiType;
begin
  TArgument.CheckNotNull(typeInfo, 'typeInfo');

  context := TRttiContext.Create;
  try
    typeObj := context.GetType(typeInfo);
    if typeObj = nil then
    begin
      raise EArgumentException.CreateRes(@SNoTypeInformation);
    end;
    if typeObj.IsPublicType then
      Result := GetLogger(typeObj.QualifiedName)
    else
      Result := GetLogger(typeObj.Name);
  finally
    context.Free;
  end;
end;

// Loop through parents of the specified logger.
// if name = 'a.b.c.d', then loop through 'a.b.c', 'a.b' and 'a'
procedure TLoggerManager.UpdateParent(const logger: IHierarchicalLogger);
var
  name: string;
  parent: IHierarchicalLogger;
  index: Integer;
begin
  name := logger.Name;
  index := LastIndexOf('.', name);
  if index > 0 then
  begin
    Delete(name, index, Length(name) - index + 1);
    parent := GetLogger(name) as IHierarchicalLogger;
  end
  else
  begin
    parent := Root;
  end;
  logger.SetParent(parent);
end;

function TLoggerManager.IsDisabled(const level: TLevel): Boolean;
begin
  CheckArgumentNotNull(level, 'level');

  Lock;
  try
    Result := not Threshold.IsGreaterThanOrEqualTo(level);
  finally
    Unlock;
  end;
end;

procedure TLoggerManager.SendLoggingEvent(const sender: ILogger; const event: TLoggingEvent);
var
  appenders: IList<IAppender>;
  appender: IAppender;
begin
  CheckArgumentNotNull(sender, 'sender');

  Lock;
  try
    if fAppenderRefs.TryGetValue(sender.Name, appenders) and (appenders <> nil) then
    begin
      for appender in appenders do
      begin
        appender.Append(event);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TLoggerManager.Lock;
begin
  fLock.Enter;
end;

procedure TLoggerManager.Unlock;
begin
  fLock.Leave;
end;

function TLoggerManager.GetName: string;
begin
  Result := fName;
end;

function TLoggerManager.GetRoot: IHierarchicalLogger;
begin
  Result := fRoot;
end;

function TLoggerManager.GetThreshold: TLevel;
begin
  Result := fThreshold;
end;

procedure TLoggerManager.SetName(const value: string);
begin
  fName := value;
end;

procedure TLoggerManager.SetThreshold(const value: TLevel);
begin
  Lock;
  try
    if value <> nil then
    begin
      fThreshold := value;
    end
    else
    begin
      InternalLogger.ErrorFormat('%s: Threshold cannot be set to null. Setting to ALL.', [ClassName]);
      fThreshold := TLevel.All;
    end;
  finally
    Unlock;
  end;
end;

{$ENDREGION}

end.
