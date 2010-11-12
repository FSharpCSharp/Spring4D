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

unit Spring.Logging.Repositories;

interface

uses
  Classes,
  SysUtils,
  XmlIntf,
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Configuration,
  Spring.Logging,
  Spring.Logging.Core,
  Spring.Logging.Utils;

type
  /// <summary>
  /// Internal implementation for the unique root logger.
  /// </summary>
  TRootLogger = class sealed(TLogger)
  protected
    procedure SetLevel(const value: TLevel); override;
  public
    constructor Create(const repository: ILoggerRepository);
    function GetEffectiveLevel: TLevel; override;
  end;

  /// <summary>
  /// Hierarchical organization of loggers.
  /// </summary>
  TLoggerRepository = class(TInterfacedObject, ILoggerRepository, IConfigurable)
  private
    fLoggers: IDictionary<string, ILogger>;
    fAppenders: IList<IAppender>;
    fLevels: IDictionary<string, TLevel>;
    fRoot: IHierarchyLogger;
    fName: string;
    fThreshold: TLevel;
    fLock: TCriticalSection;
    fEmittedNoAppenderWarning: Boolean;
  private
    function GetName: string;
    function GetThreshold: TLevel;
    function GetRoot: IHierarchyLogger;
    procedure SetName(const value: string);
    procedure SetThreshold(const value: TLevel);
  protected
    function AddLogger(const name: string): IHierarchyLogger;
    procedure Lock;
    procedure Unlock;
    procedure InitializeLevels;
    procedure ConfigurationChanged;
    procedure UpdateParent(const logger: IHierarchyLogger);
  protected
    { IConfigurable }
    procedure Configure(const configuration: IConfigurationNode); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateAppender(const typeName: string): IAppender;
    function CreateLayout(const typeName: string): ILayout;
    function IsDisabled(const level: TLevel): Boolean;
    function FindLogger(const name: string): ILogger;
    function FindLevel(const name: string): TLevel;
    function FindAppender(const name: string): IAppender;
    function GetAppenders: ICollection<IAppender>;
    function GetCurrentLoggers: ICollection<ILogger>;
    function GetLogger(const name: string): ILogger;
    property EmittedNoAppenderWarning: Boolean read fEmittedNoAppenderWarning write fEmittedNoAppenderWarning;
    property Name: string read GetName write SetName;
    property Threshold: TLevel read GetThreshold write SetThreshold;
    property Root: IHierarchyLogger read GetRoot;
  end;

const
  CDefaultAppenderNamespace = 'Spring.Logging.Appenders';
  CDefaultLayoutNamespace = 'Spring.Logging.Layouts';

implementation

uses
  StrUtils,
  Rtti,
  Spring.Reflection,
  Spring.Logging.Appenders,
  Spring.Logging.Layouts;

function LastIndexOf(const value: Char; const s: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
  begin
    if s[i] = value then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{$REGION 'TRootLogger'}

constructor TRootLogger.Create(const repository: ILoggerRepository);
begin
  inherited Create(repository, '');
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
    TInternalLogger.Debug('RootLogger: You have tried to set a null level to root.');
  end
  else
  begin
    inherited SetLevel(value);
  end;
end;

{$ENDREGION}


{$REGION 'TLoggerRepository'}

constructor TLoggerRepository.Create;
begin
  inherited Create;
  fLevels := TCollections.CreateDictionary<string, TLevel>;
  fAppenders := TCollections.CreateList<IAppender>;
  fLoggers := TCollections.CreateDictionary<string, ILogger>;
  fRoot := TRootLogger.Create(Self);
  fLock := TCriticalSection.Create;
  fThreshold := TLevel.All;
  InitializeLevels;
end;

destructor TLoggerRepository.Destroy;
begin
  fLock.Free;
  inherited Destroy;
end;

procedure TLoggerRepository.InitializeLevels;
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
    Result := CDefaultAppenderNamespace + '.' + typeName;
  end
  else
  begin
    Result := typeName;
  end;
end;

function TLoggerRepository.CreateAppender(const typeName: string): IAppender;
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

function TLoggerRepository.CreateLayout(const typeName: string): ILayout;
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

function TLoggerRepository.FindAppender(const name: string): IAppender;
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

function TLoggerRepository.FindLevel(const name: string): TLevel;
begin
  fLevels.TryGetValue(UpperCase(name), Result);
end;

function TLoggerRepository.FindLogger(const name: string): ILogger;
begin
  Lock;
  try
    fLoggers.TryGetValue(name, Result);
  finally
    Unlock;
  end;
end;

function TLoggerRepository.AddLogger(const name: string): IHierarchyLogger;
begin
  Result := TLogger.Create(Self, name);
  fLoggers[name] := Result;
end;

function TLoggerRepository.GetAppenders: ICollection<IAppender>;
begin
  Result := fAppenders;
end;

function TLoggerRepository.GetCurrentLoggers: ICollection<ILogger>;
begin
  Result := fLoggers.Values;
end;

function TLoggerRepository.GetLogger(const name: string): ILogger;
var
  logger: ILogger;
begin
  Lock;
  try
    if not fLoggers.TryGetValue(name, logger) then
    begin
      logger := AddLogger(name);
      UpdateParent(logger as IHierarchyLogger);
    end;
  finally
    Unlock;
  end;
  Result := logger;
end;

// Loop through parents of the specified logger.
// if name = 'a.b.c.d', then loop through 'a.b.c', 'a.b' and 'a'
procedure TLoggerRepository.UpdateParent(const logger: IHierarchyLogger);
var
  name: string;
  parent: IHierarchyLogger;
  index: Integer;
begin
  name := logger.Name;
  index := LastIndexOf('.', name);
  if index > 0 then
  begin
    Delete(name, index, Length(name) - index + 1);
    parent := GetLogger(name) as IHierarchyLogger;
  end
  else
  begin
    parent := Root;
  end;
  logger.SetParent(parent);
end;

function TLoggerRepository.IsDisabled(const level: TLevel): Boolean;
begin
  TArgument.CheckNotNull(level <> nil, 'level');
  Result := not Threshold.IsGreaterThanOrEqualTo(level);
end;

procedure TLoggerRepository.Lock;
begin
  fLock.Enter;
end;

procedure TLoggerRepository.Unlock;
begin
  fLock.Leave;
end;

//procedure TLoggerRepository.ResetConfiguration;
//begin
//  TInternalLogger.Debug('THierarchy: ResetConfiguration called on Hierarchy [' + Name + ']');
//  Root.CloseNestedAppenders;
//  Lock(fLoggers,
//    procedure
//    var
//      collection: ICollection<ILogger>;
//      logger: ILogger;
//    begin
//      Shutdown;
//      collection := Self.GetCurrentLoggers;
//      for logger in collection do
//      begin
//        TLogger(logger).Level := nil;
//        TLogger(logger).Additivity := True;
//      end;
//    end
//  );
//  inherited ResetConfiguration;
//end;

//procedure TLoggerRepository.Shutdown;
//begin
//  TInternalLogger.Debug('THierarchy: Shutdown called on Hierarchy [' + Name + ']');
//  Root.CloseNestedAppenders;
//  Lock(fLoggers,
//    procedure
//    var
//      collection: ICollection<ILogger>;
//      logger: ILogger;
//    begin
//      collection := Self.GetCurrentLoggers;
//      for logger in collection do
//      begin
//        TLogger(logger).CloseNestedAppenders;
//      end;
//      Root.RemoveAllAppenders;
//      for logger in collection do
//      begin
//        TLogger(logger).RemoveAllAppenders;
//      end;
//    end
//  );
//  inherited Shutdown;
//end;

procedure TLoggerRepository.ConfigurationChanged;
begin

end;

procedure TLoggerRepository.Configure(
  const configuration: IConfigurationNode);
var
  nodes: IConfigurationNodes;
  node: IConfigurationNode;
  name: string;
  thresholdValue: string;
  logger: ILogger;
  appenderType: string;
  appender: IAppender;
begin
  TArgument.CheckNotNull(configuration, 'configuration');
  if configuration.TryGetAttribute('threshold', thresholdValue) then
  begin
    Threshold := FindLevel(thresholdValue);
  end;
  nodes := configuration.FindNodes('appender');
  for node in nodes do
  begin
    appenderType := node.Attributes['type'];
    appender := CreateAppender(appenderType);
    if appender = nil then
    begin
      // TODO: Internal log
      Continue;
    end;
    GetAppenders.Add(appender);
    (appender as ILoggerRepositoryInit).InitializeRepository(Self);
    TryConfigure(appender, node);
  end;

  node := configuration.FindNode('root');
  if node <> nil then
  begin
    TryConfigure(Root, node);
  end;

  nodes := configuration.FindNodes('logger');
  for node in nodes do
  begin
    name := node.Attributes['name'];
    logger := GetLogger(name);
    TryConfigure(logger, node);
  end;
end;

function TLoggerRepository.GetName: string;
begin
  Result := fName;
end;

function TLoggerRepository.GetRoot: IHierarchyLogger;
begin
  Result := fRoot;
end;

function TLoggerRepository.GetThreshold: TLevel;
begin
  Result := fThreshold;
end;

procedure TLoggerRepository.SetName(const value: string);
begin
  fName := value;
end;

procedure TLoggerRepository.SetThreshold(const value: TLevel);
begin
  if value <> nil then
  begin
    fThreshold := value;
  end
  else
  begin
    TInternalLogger.ErrorFmt('%s: Threshold cannot be set to null. Setting to ALL', [ClassName]);
    fThreshold := TLevel.All;
  end;
end;

{$ENDREGION}

end.

