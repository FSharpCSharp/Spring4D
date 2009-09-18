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

unit Spring.Logging.Repositories;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  XmlIntf,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Patterns,
  Spring.ResourceStrings,
  Spring.Logging,
  Spring.Logging.Core,
  Spring.Logging.Core.Logger,
  Spring.Logging.Utils;

type
  /// <summary>
  /// Basic Configurator interface for repositories
  /// </summary>
  IBasicRepositoryConfigurator = interface
    ['{294C8AE8-91B8-4295-95D3-684EFF92A3B0}']
    procedure Configure(const appender: IAppender);
  end;

  /// <summary>
  /// Configure repository using XML
  /// </summary>
  IXmlRepositoryConfigurator = interface
    ['{5CABE70D-DC46-49BB-852B-93129E95B84D}']
    procedure Configure(const xmlElement: IXMLNode);
  end;

  TRootLogger = class(TLogger)
  protected
    function GetEffectiveLevel: TLevel; override;
    procedure SetLevel(const value: TLevel); override;
  end;

  /// <summary>
  /// TLoggerRepositoryBase
  /// </summary>
  TLoggerRepositoryBase = class(TObservable<ILoggerRepositoryListener>, ILoggerRepository)
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
    function Exists(const name: string): ILogger; virtual; abstract;
    procedure Log(const loggingEvent: TLoggingEvent); virtual; abstract;
    procedure ResetConfiguration; virtual;
    procedure Shutdown; virtual;
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
  THierarchy = class(TLoggerRepositoryBase, IBasicRepositoryConfigurator,
    IXmlRepositoryConfigurator, ILoggerRepository, IInterface)
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
    function AddLogger(const name: string): TLogger;
  protected
    procedure ConfigurationChanged;
    procedure BasicRepositoryConfigure(const appender: IAppender);
    procedure XmlRepositoryConfigure(const xmlElement: IXMLNode);
    procedure IBasicRepositoryConfigurator.Configure = BasicRepositoryConfigure;
    procedure IXmlRepositoryConfigurator.Configure = XmlRepositoryConfigure;
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

  TXmlHierarchyConfigurator = class
  private
    fHierarchy: THierarchy;
  public
    constructor Create(hierarchy: THierarchy);
    procedure Configure(const xmlNode: IXmlNode);
  end;

  TAppenderList = TListAdapter<IAppender>;
  TLoggerList   = TListAdapter<ILogger>;

implementation

{ TXmlHierarchyConfigurator }

constructor TXmlHierarchyConfigurator.Create(hierarchy: THierarchy);
begin
  inherited Create;
  fHierarchy := hierarchy;
end;

procedure TXmlHierarchyConfigurator.Configure(const xmlNode: IXmlNode);
begin
end;


{$IFDEF SUPPORTS_REGION} {$REGION 'TRootLogger'} {$ENDIF}

function TRootLogger.GetEffectiveLevel: TLevel;
begin
  Result := Self.Level;
end;

procedure TRootLogger.SetLevel(const value: TLevel);
begin
  if value.IsNull then
  begin
    TInternalLogger.Debug('RootLogger: You have tried to set a null level to root.');
  end
  else
  begin
    inherited SetLevel(value);
  end;
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$REGION 'THierarchy'}

constructor THierarchy.Create;
begin
  inherited Create;
  fLoggers := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  fRoot := TRootLogger.Create('root');
  fRoot.Hierarchy := Self;
  fThreshold := TLevel.All;
end;

destructor THierarchy.Destroy;
begin
  fRoot.Free;
  fLoggers.Free;
  inherited Destroy;
end;

function THierarchy.AddLogger(const name: string): TLogger;
begin
  Result := TLogger.Create(name);
  Result.Hierarchy := Self;
  fLoggers[name] := Result;
end;

function THierarchy.CreateProvisionNode(const logger: TLogger): TProvisionNode;
begin
  Result := TProvisionNode.Create;
  Result.Add(logger);
end;

function THierarchy.GetAppenders: ICollection<IAppender>;
begin
  Result := TContainer.CreateList<IAppender>;
end;

function THierarchy.GetCurrentLoggers: ICollection<ILogger>;
var
  node: TObject;
begin
  Result := TContainer.CreateList<ILogger>;
  for node in fLoggers.Values do
  begin
    if node is TLogger then
    begin
      Result.Add(TLogger(node));
    end;
  end;
end;

function THierarchy.GetLogger(const name: string): ILogger;
var
  logger: TLogger;
begin
  logger := nil;
  Lock(fLoggers,
    procedure
    var
      node: TObject;
      provisionNode: TProvisionNode;
    begin
      fLoggers.TryGetValue(name, node);
      if node = nil then
      begin
        logger := AddLogger(name);
        UpdateParents(logger);
      end
      else if node is TLogger then
      begin
        logger := TLogger(node);
      end
      else if node is TProvisionNode then
      begin
        provisionNode := TProvisionNode(fLoggers.ExtractPair(name).Value);
        try
          logger := AddLogger(name);
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
    end
  );
  Result := logger;
end;

procedure THierarchy.UpdateParents(logger: TLogger);
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

procedure THierarchy.UpdateChildren(provisionNode: TProvisionNode;
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

function THierarchy.IsDisabled(const level: TLevel): Boolean;
begin
  TArgument.CheckNotNull(not level.IsNull, 'level');
  Result := not Configured or (Threshold > level);
end;

procedure THierarchy.ResetConfiguration;
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
        TLogger(logger).Level := TLevel.Null;
        TLogger(logger).Additivity := True;
      end;
    end
  );
  inherited ResetConfiguration;
end;

procedure THierarchy.Shutdown;
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

procedure THierarchy.ConfigurationChanged;
begin
  CallOnConfigurationChanged;
end;

procedure THierarchy.BasicRepositoryConfigure(const appender: IAppender);
begin
  Root.AddAppender(appender);
  fConfigured := True;
  ConfigurationChanged;
end;

procedure THierarchy.XmlRepositoryConfigure(const xmlElement: IXMLNode);
var
  configurator: TXmlHierarchyConfigurator;
begin
  configurator := TXmlHierarchyConfigurator.Create(Self);
  try
    configurator.Configure(xmlElement);
  finally
    configurator.Free;
  end;
  fConfigured := True;
  ConfigurationChanged;
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
  NotifyObservers(
    procedure(listener: ILoggerRepositoryListener)
    begin
      listener.OnConfigurationChanged;
    end
  );
end;

procedure TLoggerRepositoryBase.CallOnConfigurationReset;
begin
  NotifyObservers(
    procedure(listener: ILoggerRepositoryListener)
    begin
      listener.OnConfigurationReset;
    end
  );
end;

procedure TLoggerRepositoryBase.CallOnShutdown;
begin
  NotifyObservers(
    procedure(listener: ILoggerRepositoryListener)
    begin
      listener.OnShutdown;
    end
  );
end;

procedure TLoggerRepositoryBase.ResetConfiguration;
begin
  Configured := False;
  CallOnConfigurationReset;
end;

procedure TLoggerRepositoryBase.Shutdown;
begin
  // TODO: Close Plugins
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
  if not value.IsNull then
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
