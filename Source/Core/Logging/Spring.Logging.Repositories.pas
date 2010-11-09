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
  StrUtils,
  XmlIntf,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.ResourceStrings,
  Spring.Logging,
  Spring.Logging.Core,
  Spring.Logging.Utils;

type
  /// <summary>
  /// Internal root logger.
  /// </summary>
  TRootLogger = class sealed(TLogger)
  protected
    function GetEffectiveLevel: TLevel; override;
    procedure SetLevel(const value: TLevel); override;
  public
    constructor Create;
  end;

  /// <summary>
  /// TLoggerRepositoryBase
  /// </summary>
  TLoggerRepositoryBase = class abstract(TInterfacedObject, ILoggerRepository)
  private
    fName: string;
    fConfigured: Boolean;
    fThreshold: TLevel;
    fProperties: TStrings;
    function GetName: string;
    function GetConfigured: Boolean;
    function GetProperties: TStrings;
    function GetThreshold: TLevel;
    procedure SetName(const value: string);
    procedure SetConfigured(const value: Boolean);
    procedure SetThreshold(const value: TLevel);
  protected
    procedure CallOnConfigurationChanged;
    procedure CallOnConfigurationReset;
    procedure CallOnShutdown;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const event: TLoggingEvent); virtual; abstract;
    procedure ResetConfiguration; virtual;
    procedure Shutdown; virtual;
    function FindLogger(const name: string): ILogger; virtual; abstract;
    function GetAppenders: ICollection<IAppender>; virtual; abstract;
    function GetCurrentLoggers: ICollection<ILogger>; virtual; abstract;
    function GetLogger(const name: string): ILogger; virtual; abstract;
    property Name: string read GetName write SetName;
    property Configured: Boolean read GetConfigured write SetConfigured;
    property Properties: TStrings read GetProperties;
    property Threshold: TLevel read GetThreshold write SetThreshold;
  end;

  /// <summary>
  /// Hierarchical organization of loggers.
  /// </summary>
  TLoggerRepository = class(TLoggerRepositoryBase)
  private
    type
      /// <summary>
      /// Provision nodes are used where no logger instance has been specified
      /// </summary>
      TProvisionNode = class(TList<TLogger>)
      end;
  private
    fRoot: TLogger;
    fEmittedNoAppenderWarning: Boolean;
    fLoggers: TDictionary<string, TObject>;
    procedure UpdateParents(logger: TLogger);
    procedure UpdateChildren(provisionNode: TProvisionNode; logger: TLogger);
  protected
    function CreateProvisionNode(const logger: TLogger): TProvisionNode;
    function CreateLogger(const name: string): TLogger;
  protected
    procedure ConfigurationChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetConfiguration; override;
    procedure Shutdown; override;
    function IsDisabled(const level: TLevel): Boolean;
    function GetAppenders: ICollection<IAppender>; override;
    function GetCurrentLoggers: ICollection<ILogger>; override;
    function GetLogger(const name: string): ILogger; override;
    property EmittedNoAppenderWarning: Boolean read fEmittedNoAppenderWarning write fEmittedNoAppenderWarning;
    property Root: TLogger read fRoot;
  end;

implementation


{$REGION 'TRootLogger'}

constructor TRootLogger.Create;
begin
  inherited Create('');
  SetLevel(TLevel.All);
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
  fLoggers := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  fRoot := TRootLogger.Create;
  fRoot.Repository := Self;
  fThreshold := TLevel.All;
end;

destructor TLoggerRepository.Destroy;
begin
  fRoot.Free;
  fLoggers.Free;
  inherited Destroy;
end;

function TLoggerRepository.CreateLogger(const name: string): TLogger;
begin
  Result := TLogger.Create(name);
  Result.Repository := Self;
  fLoggers.AddOrSetValue(name, Result);
end;

function TLoggerRepository.CreateProvisionNode(const logger: TLogger): TProvisionNode;
begin
  Result := TProvisionNode.Create;
  Result.Add(logger);
end;

function TLoggerRepository.GetAppenders: ICollection<IAppender>;
begin
  Result := TCollections.CreateList<IAppender>;
end;

function TLoggerRepository.GetCurrentLoggers: ICollection<ILogger>;
var
  node: TObject;
begin
  Result := TCollections.CreateList<ILogger>;
  for node in fLoggers.Values do
  begin
    if node is TLogger then
    begin
      Result.Add(TLogger(node));
    end;
  end;
end;

function TLoggerRepository.GetLogger(const name: string): ILogger;
var
  logger: TLogger;
  node: TObject;
  provisionNode: TProvisionNode;
begin
  logger := nil;
  MonitorEnter(fLoggers);
  try
    fLoggers.TryGetValue(name, node);
    if node = nil then
    begin
      logger := CreateLogger(name);
      UpdateParents(logger);
    end
    else if node is TLogger then
    begin
      logger := TLogger(node);
    end
    else if node is TProvisionNode then
    begin
      provisionNode := TProvisionNode(node);
      try
        logger := CreateLogger(name);
        UpdateChildren(provisionNode, logger);
        UpdateParents(logger);
      finally
        provisionNode.Free;
      end;
    end
    else
    begin
      TInternalLogger.ErrorFmt('Hierarchy: Unexpected object type %s', [node.ClassName]);
    end;
  finally
    MonitorExit(fLoggers);
  end;
  Result := logger;
end;

procedure TLoggerRepository.UpdateParents(logger: TLogger);
var
  name: string;
  parentFound: Boolean;
  index: Integer;
  node: TObject;

  function LastIndexOf(const value: Char; const s: string): Integer; inline;
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
begin
  name := logger.Name;
  parentFound := False;
  // if name = "w.x.y.z", loop through "w.x.y", "w.x" and "w", but not "w.x.y.z"
  index := LastIndexOf('.', name);
  while index > 0 do
  begin
    Delete(name, index, Length(name) - index + 1);
    fLoggers.TryGetValue(name, node);
    if node = nil then
    begin
      node := CreateProvisionNode(logger);
      fLoggers[name] := node;
    end
    else if node is TLogger then
    begin
      logger.Parent := TLogger(node);
      parentFound := True;
      Break;
    end
    else if node is TProvisionNode then
    begin
      TProvisionNode(node).Add(logger);
    end
    else
    begin
      TInternalLogger.ErrorFmt('Hierarchy: Unexpected object type %s', [node.ClassName]);
    end;
    index := LastIndexOf('.', name);
  end;
  if not parentFound then
  begin
    logger.Parent := Self.Root;
  end;
end;

procedure TLoggerRepository.UpdateChildren(provisionNode: TProvisionNode;
  logger: TLogger);
var
  childLogger: TLogger;
begin
  for childLogger in provisionNode do
  begin
    // Unless this child already points to a correct (lower) parent,
    // make logger.Parent point to childLogger.Parent and childLogger.Parent to logger.
    if not StartsText(logger.Name, childLogger.Parent.Name) then
    begin
      logger.Parent := childLogger.Parent;
      childLogger.Parent := logger;
    end;
  end;
end;

function TLoggerRepository.IsDisabled(const level: TLevel): Boolean;
begin
  TArgument.CheckNotNull(level <> nil, 'level');
  Result := not Configured or (Threshold.Value > level.Value);
end;

procedure TLoggerRepository.ResetConfiguration;
begin
  TInternalLogger.Debug('THierarchy: ResetConfiguration called on Hierarchy [' + Name + ']');
  Root.CloseNestedAppenders;
  Lock(fLoggers,
    procedure
    var
      collection: ICollection<ILogger>;
      logger: ILogger;
    begin
      Shutdown;
      collection := Self.GetCurrentLoggers;
      for logger in collection do
      begin
        TLogger(logger).Level := nil;
        TLogger(logger).Additivity := True;
      end;
    end
  );
  inherited ResetConfiguration;
end;

procedure TLoggerRepository.Shutdown;
begin
  TInternalLogger.Debug('THierarchy: Shutdown called on Hierarchy [' + Name + ']');
  Root.CloseNestedAppenders;
  Lock(fLoggers,
    procedure
    var
      collection: ICollection<ILogger>;
      logger: ILogger;
    begin
      collection := Self.GetCurrentLoggers;
      for logger in collection do
      begin
        TLogger(logger).CloseNestedAppenders;
      end;
      Root.RemoveAllAppenders;
      for logger in collection do
      begin
        TLogger(logger).RemoveAllAppenders;
      end;
    end
  );
  inherited Shutdown;
end;

procedure TLoggerRepository.ConfigurationChanged;
begin
  CallOnConfigurationChanged;
end;

{$ENDREGION}


{$REGION 'TLoggerRepositoryBase'}

constructor TLoggerRepositoryBase.Create;
begin
  inherited Create;
  fProperties := TStringList.Create;
  fThreshold := TLevel.All;
end;

destructor TLoggerRepositoryBase.Destroy;
begin
  fProperties.Free;
  inherited Destroy;
end;

procedure TLoggerRepositoryBase.CallOnConfigurationChanged;
begin
//  NotifyListeners(
//    procedure(listener: ILoggerRepositoryListener)
//    begin
//      listener.OnConfigurationChanged;
//    end
//  );
end;

procedure TLoggerRepositoryBase.CallOnConfigurationReset;
begin
//  NotifyListeners(
//    procedure(listener: ILoggerRepositoryListener)
//    begin
//      listener.OnConfigurationReset;
//    end
//  );
end;

procedure TLoggerRepositoryBase.CallOnShutdown;
begin
//  NotifyListeners(
//    procedure(listener: ILoggerRepositoryListener)
//    begin
//      listener.OnShutdown;
//    end
//  );
end;

procedure TLoggerRepositoryBase.ResetConfiguration;
begin
  Configured := False;
  CallOnConfigurationReset;
end;

procedure TLoggerRepositoryBase.Shutdown;
begin
  CallOnShutdown;
end;

function TLoggerRepositoryBase.GetConfigured: Boolean;
begin
  Result := fConfigured;
end;

function TLoggerRepositoryBase.GetName: string;
begin
  Result := fName;
end;

function TLoggerRepositoryBase.GetProperties: TStrings;
begin
  Result := fProperties;
end;

function TLoggerRepositoryBase.GetThreshold: TLevel;
begin
  Result := fThreshold;
end;

procedure TLoggerRepositoryBase.SetConfigured(const value: Boolean);
begin
  fConfigured := value;
end;

procedure TLoggerRepositoryBase.SetName(const value: string);
begin
  fName := value;
end;

procedure TLoggerRepositoryBase.SetThreshold(const value: TLevel);
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
