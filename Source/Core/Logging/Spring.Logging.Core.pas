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

unit Spring.Logging.Core;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Configuration;

type

  {$REGION 'Documentation'}
  ///	<summary>Defines the default set of levels recognized by the logging
  ///	service.</summary>
  ///	<remarks>
  ///	  There are 7 predefined logging level in the class:
  ///	  <list type="bullet">
  ///	    <item><see cref="TLevel.All">All</see></item>
  ///	    <item><see cref="TLevel.Debug">Debug</see></item>
  ///	    <item><see cref="TLevel.Info">Info</see></item>
  ///	    <item><see cref="TLevel.Warn">Warn</see></item>
  ///	    <item><see cref="TLevel.Error">Error</see></item>
  ///	    <item><see cref="TLevel.Fatal">Fatal</see></item>
  ///	    <item><see cref="TLevel.Off">Off</see></item>
  ///	  </list>
  ///	</remarks>
  {$ENDREGION}
  TLevel = class(TObject)
  strict private
    const
      fCAllValue    = Low(Integer);
      fCDebugValue  = 10000;
      fCInfoValue   = 20000;
      fCWarnValue   = 30000;
      fCErrorValue  = 40000;
      fCFatalValue  = 50000;
      fCOffValue    = High(Integer);
    var
      fName: string;
      fValue: Integer;
    class var
      fAll:   TLevel;
      fDebug: TLevel;
      fInfo:  TLevel;
      fWarn:  TLevel;
      fError: TLevel;
      fFatal: TLevel;
      fOff:   TLevel;
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create(const levelValue: Integer; const levelName: string);
    function Equals(obj: TObject): Boolean; override;
    function ToString: string; override;
    function IsGreaterThan(level: TLevel): Boolean; // inline;
    function IsGreaterThanOrEqualTo(level: TLevel): Boolean; // inline;
//    property DisplayName: string read fDisplayName;
    property Name: string read fName;
    property Value: Integer read fValue;
    class property All: TLevel read fAll;
    class property Debug: TLevel read fDebug;
    class property Info: TLevel read fInfo;
    class property Warn: TLevel read fWarn;
    class property Error: TLevel read fError;
    class property Fatal: TLevel read fFatal;
    class property Off: TLevel read fOff;
  end;

  /// <summary>
  /// The ILogger interface is used by application to log messages.
  /// </summary>
  ILogger = interface
    ['{803DC36C-03FE-4C5C-B8BE-9CB79076DCB2}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
      function GetLevel: TLevel;
      function GetIsDebugEnabled: Boolean;
      function GetIsInfoEnabled: Boolean;
      function GetIsWarnEnabled: Boolean;
      function GetIsErrorEnabled: Boolean;
      function GetIsFatalEnabled: Boolean;
      procedure SetLevel(const value: TLevel);
    {$ENDREGION}
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
    property Name: string read GetName;
    property Level: TLevel read GetLevel write SetLevel;
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  IHierarchyLogger = interface(ILogger)
    ['{4A229971-00AB-47E8-9BD4-4F01C6E47017}']
    {$REGION 'Property Getters & Setters'}
      function GetParent: IHierarchyLogger;
      function GetAdditivity: Boolean;
      procedure SetParent(const value: IHierarchyLogger);
      procedure SetAdditivity(const value: Boolean);
    {$ENDREGION}
    function GetEffectiveLevel: TLevel;
    property Parent: IHierarchyLogger read GetParent;
    property Additivity: Boolean read GetAdditivity write SetAdditivity;
  end;

  {$REGION 'Documentation'}
  ///	<summary>The internal representation of logging events.</summary>
  ///	<remarks>When an affirmative decision is made to log then a LoggingEvent
  ///	instance is created. This instance is passed around to the different
  ///	logging components.</remarks>
  {$ENDREGION}
  TLoggingEvent = record
    LoggerName: string;
    LoggerLevel: TLevel;
    TimeStamp: TDateTime;
    ThreadID: TThreadID;
    Message: string;
    ExceptionString: string;
  end;

  /// <summary>
  /// IAppender
  /// </summary>
  IAppender = interface
    ['{76735B1E-5AF3-4DC3-A19C-A35FFC13DB55}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
      procedure SetName(const value: string);
    {$ENDREGION}
    procedure Close;
    procedure Append(const event: TLoggingEvent);
    property Name: string read GetName write SetName;
  end;

  ///	<summary>IBulkAppender</summary>
  IBulkAppender = interface(IAppender)
    ['{E5EADA47-4ED2-4097-8BCC-25B94716F1E3}']
    procedure Append(const events: IEnumerable<TLoggingEvent>);
  end;

  /// <summary>
  /// Interface for attaching, removing and retrieving appenders.
  /// </summary>
  IAppenderAttachable = interface
    ['{25687F5E-642E-48D6-9DD9-C76C7443CE71}']
    {$REGION 'Property Getters & Setters'}
      function GetAppenders: IEnumerableEx<IAppender>;
    {$ENDREGION}
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAppender(const appender: IAppender);
    procedure ClearAppenders;
    function FindAppender(const name: string): IAppender;
    property Appenders: IEnumerableEx<IAppender> read GetAppenders;
  end;

  {$REGION 'Documentation'}
  ///	<summary>An ILayout object is used to format a LoggingEvent as text. The
  ///	Format method is called by an appender to transform the LoggingEvent into
  ///	a string.</summary>
  ///	<remarks>The layout can also supply Header and Footer text that is
  ///	appender before any events and after all the events
  ///	respectively.</remarks>
  {$ENDREGION}
  ILayout = interface
    ['{96A344B1-6104-4C3D-A41F-0F5336BE2800}']
    {$REGION 'Property Getters & Setters'}
      function GetContentType: string;
      function GetHeader: string;
      function GetFooter: string;
      function GetIgnoresException: Boolean;
    {$ENDREGION}
    function Format(const event: TLoggingEvent): string;
    property ContentType: string read GetContentType;
    property Header: string read GetHeader;
    property Footer: string read GetFooter;
    property IgnoresException: Boolean read GetIgnoresException;
  end;

  ILoggerRepository = interface(IConfigurable)
    ['{C44BAA97-C52A-49DE-9E4D-B1721A4F5405}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
      function GetThreshold: TLevel;
      procedure SetName(const value: string);
      procedure SetThreshold(const value: TLevel);
    {$ENDREGION}
    function CreateAppender(const typeName: string): IAppender;
    function CreateLayout(const typeName: string): ILayout;
    function FindLogger(const name: string): ILogger;
    function FindLevel(const name: string): TLevel;
    function FindAppender(const name: string): IAppender;
    function GetAppenders: ICollection<IAppender>;
    function GetCurrentLoggers: ICollection<ILogger>;
    function GetLogger(const name: string): ILogger;
    property Name: string read GetName write SetName;
    property Threshold: TLevel read GetThreshold write SetThreshold;
  end;

  ILoggerRepositoryInit = interface
    ['{1F851710-0FD3-49A8-B918-5C5BBA160131}']
    procedure InitializeRepository(const repository: ILoggerRepository);
  end;

  /// <summary>
  /// Represents an abstract class of a logger.
  /// </summary>
  TLoggerBase = class abstract(TInterfacedObject, ILogger)
  private
    fName: string;
    function GetName: string;
    function GetIsDebugEnabled: Boolean; // inline;
    function GetIsInfoEnabled: Boolean; // inline;
    function GetIsWarnEnabled: Boolean; // inline;
    function GetIsErrorEnabled: Boolean; // inline;
    function GetIsFatalEnabled: Boolean; // inline;
  protected
    fLevel: TLevel;
    function GetLevel: TLevel; virtual;
    procedure SetLevel(const value: TLevel); virtual;
  protected
    procedure Log(const level: TLevel; const msg: string; e: Exception); overload; virtual;
    procedure DoLog(const event: TLoggingEvent); overload; virtual;
    procedure HandleException(e: Exception); virtual;
    function IsEnabledFor(const level: TLevel): Boolean; virtual;
  public
    constructor Create(const name: string);
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
    property Name: string read GetName;
    property Level: TLevel read GetLevel write SetLevel;
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  /// <summary>
  /// Represents a logger which supports hierarchy and appenders.
  /// </summary>
  TLogger = class(TLoggerBase, IHierarchyLogger, IAppenderAttachable, IConfigurable)
  strict private
    fRepository: ILoggerRepository;
    fParent: IHierarchyLogger;
    fAdditivity: Boolean;
    fAppenderAttachable: IAppenderAttachable;
  protected
    function GetAdditivity: Boolean; virtual;
    function GetParent: IHierarchyLogger; virtual;
    procedure SetAdditivity(const value: Boolean); virtual;
    procedure SetParent(const value: IHierarchyLogger); virtual;
  protected
    fAppenderAttachableLock: IReadWriteSync;
    function GetAppenderAttachable: IAppenderAttachable;
    function GetAppenders: IEnumerableEx<IAppender>;
    procedure CallAppenders(const event: TLoggingEvent); virtual;
  protected
    function IsEnabledFor(const level: TLevel): Boolean; override;
    procedure DoLog(const event: TLoggingEvent); override;
  protected
    { IConfigurable }
    procedure Configure(const configuration: IConfigurationNode); virtual;
  public
    constructor Create(const repository: ILoggerRepository; const name: string);
    procedure CloseNestedAppenders;
    { IAppenderAttachable }
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAppender(const appender: IAppender);
    procedure ClearAppenders;
    function FindAppender(const name: string): IAppender;
    property Appenders: IEnumerableEx<IAppender> read GetAppenders;
    { Properties }
    function GetEffectiveLevel: TLevel; virtual;
    property Additivity: Boolean read GetAdditivity write SetAdditivity;
    property Parent: IHierarchyLogger read GetParent;
    property Repository: ILoggerRepository read fRepository;
  end;

  TAppenderAttachable = class(TInterfacedObject, IAppenderAttachable)
  private
    fList: ICollection<IAppender>;
    function GetAppenders: IEnumerableEx<IAppender>;
  public
    constructor Create;
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAppender(const appender: IAppender);
    procedure ClearAppenders;
    function FindAppender(const name: string): IAppender;
    property Appenders: IEnumerableEx<IAppender> read GetAppenders;
  end;
  
  ELoggingException = class(Exception);


implementation

uses
  Spring.Logging.ResourceStrings;


{$REGION 'TLevel'}

class constructor TLevel.Create;
begin
  fAll := TLevel.Create(fCAllValue, SAllDescription);
  fDebug := TLevel.Create(fCDebugValue, SDebugDescription);
  fInfo := TLevel.Create(fCInfoValue, SInfoDescription);
  fWarn := TLevel.Create(fCWarnValue, SWarnDescription);
  fError := TLevel.Create(fCErrorValue, SErrorDescription);
  fFatal := TLevel.Create(fCFatalValue, SFatalDescription);
  fOff := TLevel.Create(fCOffValue, SOffDescription);
end;

class destructor TLevel.Destroy;
begin
  fAll.Free;
  fDebug.Free;
  fInfo.Free;
  fWarn.Free;
  fError.Free;
  fFatal.Free;
  fOff.Free;
end;

constructor TLevel.Create(const levelValue: Integer; const levelName: string);
begin
  fValue := levelValue;
  fName := levelName;
end;

function TLevel.Equals(obj: TObject): Boolean;
begin
  if obj = nil then
    Exit(False)
  else if obj = Self then
    Exit(True)
  else
    Exit(False);
end;

function TLevel.ToString: string;
begin
  Result := Name;
end;

function TLevel.IsGreaterThan(level: TLevel): Boolean;
begin
  Result := (level <> nil) and (Self.Value > level.Value);
end;

function TLevel.IsGreaterThanOrEqualTo(level: TLevel): Boolean;
begin
  Result := (level <> nil) and (Self.Value >= level.Value);
end;

{$ENDREGION}


{$REGION 'TLoggerBase'}

constructor TLoggerBase.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

// TODO: Add try-except block
function TLoggerBase.IsEnabledFor(const level: TLevel): Boolean;
begin
  Result := False;
end;

procedure TLoggerBase.Log(const level: TLevel; const msg: string;
  e: Exception);
var
  event: TLoggingEvent;
begin
  event.LoggerName := Self.Name;
  event.LoggerLevel := level;
  event.TimeStamp := Now;
  event.ThreadID := GetCurrentThreadId;
  event.Message := msg;
  if e <> nil then
  begin
    event.ExceptionString := e.ClassName + ': ' + e.Message;
  end;
  try
    DoLog(event);
  except on e: Exception do
    HandleException(e);
  end;
end;

procedure TLoggerBase.DoLog(const event: TLoggingEvent);
begin
end;

procedure TLoggerBase.HandleException(e: Exception);
begin
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
    Log(TLevel.Warn, msg, nil);
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
    Log(TLevel.Error, msg, nil);
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
    Log(TLevel.Fatal, msg, nil);
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
  Result := IsEnabledFor(TLevel.Debug);
end;

function TLoggerBase.GetIsInfoEnabled: Boolean;
begin
  Result := IsEnabledFor(TLevel.Info);
end;

function TLoggerBase.GetIsWarnEnabled: Boolean;
begin
  Result := IsEnabledFor(TLevel.Warn);
end;

function TLoggerBase.GetIsErrorEnabled: Boolean;
begin
  Result := IsEnabledFor(TLevel.Error);
end;

function TLoggerBase.GetIsFatalEnabled: Boolean;
begin
  Result := IsEnabledFor(TLevel.Fatal);
end;

procedure TLoggerBase.SetLevel(const value: TLevel);
begin
  fLevel := value;
end;

{$ENDREGION}


{$REGION 'TLogger'}

constructor TLogger.Create(const repository: ILoggerRepository;
  const name: string);
begin
  inherited Create(name);
  fRepository := repository;
  fAdditivity := True;
  fAppenderAttachableLock := TSimpleRWSync.Create;
end;

procedure TLogger.DoLog(const event: TLoggingEvent);
begin
  CallAppenders(event);
end;

procedure TLogger.CallAppenders(const event: TLoggingEvent);
var
  logger: TLogger;
  collection: IEnumerableEx<IAppender>;
  appender: IAppender;
begin
  logger := Self;
  while logger <> nil do
  begin
    logger.fAppenderAttachableLock.BeginRead;
    try
      if logger.fAppenderAttachable <> nil then
      begin
        collection := logger.GetAppenderAttachable.Appenders;
        for appender in collection do
        begin
          appender.Append(event);
        end;
      end;
    finally
      logger.fAppenderAttachableLock.EndWrite;
    end;
    if not logger.Additivity then Break;
    logger := TLogger(logger.Parent);
  end;
end;

procedure TLogger.CloseNestedAppenders;
var
  appenders: IEnumerableEx<IAppender>;
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

// TODO: Considering IInitializable
procedure TLogger.Configure(const configuration: IConfigurationNode);
var
  additivity: string;
  loggerLevel: string;
  nodes: IConfigurationNodes;
  node: IConfigurationNode;
  appenderName: string;
  appender: IAppender;
begin
  TArgument.CheckNotNull(configuration, 'configuration');
  node := configuration.FindNode('level');
  if (node <> nil) and node.TryGetAttribute('value', loggerLevel) then
  begin
    Self.Level := fRepository.FindLevel(loggerLevel);
  end;
  if configuration.TryGetAttribute('additivity', additivity) then
  begin
    Self.Additivity := StrToBoolDef(additivity, True);
  end;
  nodes := configuration.FindNodes('appender-ref');
  if not nodes.IsEmpty then
  begin
    for node in nodes do
    begin
      appenderName := node.Attributes['ref'];
      appender := fRepository.FindAppender(appenderName);
      if appender = nil then
      begin
        // TODO: Log error message.
        Continue;
      end;
      AddAppender(appender);
    end;
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

procedure TLogger.RemoveAppender(const appender: IAppender);
begin
  TArgument.CheckNotNull(appender, 'appender');
  fAppenderAttachableLock.BeginWrite;
  try
    GetAppenderAttachable.RemoveAppender(appender);
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

procedure TLogger.ClearAppenders;
begin
  fAppenderAttachableLock.BeginWrite;
  try
    GetAppenderAttachable.ClearAppenders;
  finally
    fAppenderAttachableLock.EndWrite;
  end;
end;

function TLogger.FindAppender(const name: string): IAppender;
begin
  fAppenderAttachableLock.BeginRead;
  try
    Result := GetAppenderAttachable.FindAppender(name);
  finally
    fAppenderAttachableLock.EndRead;
  end;
end;

function TLogger.GetAppenders: IEnumerableEx<IAppender>;
begin
  fAppenderAttachableLock.BeginRead;
  try
    Result := GetAppenderAttachable.GetAppenders;
  finally
    fAppenderAttachableLock.EndRead;
  end;
end;

function TLogger.GetAdditivity: Boolean;
begin
  Result := fAdditivity;
end;

function TLogger.GetParent: IHierarchyLogger;
begin
  Result := fParent;
end;

// TODO: Optimization (Returns false if this level is disabled globally)
function TLogger.IsEnabledFor(const level: TLevel): Boolean;
begin
  Result := (level <> nil) and level.IsGreaterThanOrEqualTo(GetEffectiveLevel);
end;

function TLogger.GetEffectiveLevel: TLevel;
var
  logger: IHierarchyLogger;
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

procedure TLogger.SetParent(const value: IHierarchyLogger);
begin
  fParent := value;
end;

procedure TLogger.SetAdditivity(const value: Boolean);
begin
  fAdditivity := value;
end;

{$ENDREGION}


{$REGION 'TAppenderAttachable'}

constructor TAppenderAttachable.Create;
begin
  inherited Create;
  fList := TCollections.CreateList<IAppender>;
end;

procedure TAppenderAttachable.AddAppender(const appender: IAppender);
begin
  TArgument.CheckNotNull(appender, 'appender');
  fList.Add(appender);
end;

procedure TAppenderAttachable.RemoveAppender(const appender: IAppender);
begin
  TArgument.CheckNotNull(appender, 'appender');
  fList.Remove(appender);
end;

procedure TAppenderAttachable.ClearAppenders;
begin
  fList.Clear;
end;

function TAppenderAttachable.FindAppender(const name: string): IAppender;
var
  appender: IAppender;
begin
  Result := nil;
  for appender in fList do
  begin
    if SameText(appender.Name, name) then
    begin
      Result := appender;
      Break;
    end;
  end;
end;

function TAppenderAttachable.GetAppenders: IEnumerableEx<IAppender>;
begin
  Result := fList;
end;

{$ENDREGION}

end.
