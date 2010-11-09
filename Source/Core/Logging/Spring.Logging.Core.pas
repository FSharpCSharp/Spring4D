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
  StrUtils,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns;

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
    function IsGreaterThan(level: TLevel): Boolean; inline;
    function IsGreaterThanOrEqualTo(level: TLevel): Boolean; inline;
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
  /// The ILogger interface is used by application to log messages.
  /// </summary>
  ILogger = interface
    ['{803DC36C-03FE-4C5C-B8BE-9CB79076DCB2}']
  {$REGION 'Property Getters & Setters'}
    function GetName: string;
    function GetIsDebugEnabled: Boolean;
    function GetIsInfoEnabled: Boolean;
    function GetIsWarnEnabled: Boolean;
    function GetIsErrorEnabled: Boolean;
    function GetIsFatalEnabled: Boolean;
//    function GetIsTraceEnabled: Boolean;
//    procedure Trace(const msg: string); overload;
//    procedure Trace(const msg: string; e: Exception); overload;
//    procedure TraceFormat(const format: string; const args: array of const);
//    procedure Trace<TValue>(const instance: TValue); overload;
//    procedure Trace<TValue>(const instance: TValue; e: Exception); overload;
//    procedure Debug<TValue>(const instance: TValue); overload;
//    procedure Debug<TValue>(const instance: TValue; e: Exception); overload;
//    procedure Info<TValue>(const instance: TValue); overload;
//    procedure Info<TValue>(const instance: TValue; e: Exception); overload;
//    procedure Warn<TValue>(const instance: TValue); overload;
//    procedure Warn<TValue>(const instance: TValue; e: Exception); overload;
//    procedure Error<TValue>(const instance: TValue); overload;
//    procedure Error<TValue>(const instance: TValue; e: Exception); overload;
//    procedure Fatal<TValue>(const instance: TValue); overload;
//    procedure Fatal<TValue>(const instance: TValue; e: Exception); overload;
//    property IsTraceEnabled: Boolean read GetIsTraceEnabled;
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
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  /// <summary>
  /// IAppender
  /// </summary>
  IAppender = interface
    ['{76735B1E-5AF3-4DC3-A19C-A35FFC13DB55}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
    {$ENDREGION}
    procedure Close;
    procedure Append(const event: TLoggingEvent);
    property Name: string read GetName;
  end;

  /// <summary>
  /// IBulkAppender
  /// </summary>
  IBulkAppender = interface(IAppender)
    ['{E5EADA47-4ED2-4097-8BCC-25B94716F1E3}']
    procedure Append(const events: ICollection<TLoggingEvent>);
  end;

  /// <summary>
  /// Interface for attaching, removing and retrieving appenders.
  /// </summary>
  IAppenderAttachable = interface
    ['{25687F5E-642E-48D6-9DD9-C76C7443CE71}']
    {$REGION 'Property Getters & Setters'}
      function GetAppenders: ICollection<IAppender>;
    {$ENDREGION}
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAllAppenders;
    function GetAppender(const name: string): IAppender;
    function RemoveAppender(const name: string): IAppender; overload;
    function RemoveAppender(const appender: IAppender): IAppender; overload;
    property Appenders: ICollection<IAppender> read GetAppenders;
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
  
  // NEED REVISE
  ILoggerRepositoryListener = interface
    procedure OnConfigurationChanged;
    procedure OnConfigurationReset;
    procedure OnShutdown;
  end;

  // NEED REVISE
  ILoggerRepository = interface
    ['{C44BAA97-C52A-49DE-9E4D-B1721A4F5405}']
    {$REGION 'Property Getters & Setters'}
      function GetName: string;
      function GetConfigured: Boolean;
      function GetProperties: TStrings;
      function GetThreshold: TLevel;
      procedure SetName(const value: string);
      procedure SetConfigured(const value: Boolean);
      procedure SetThreshold(const value: TLevel);
    {$ENDREGION}
    procedure Log(const event: TLoggingEvent);
    procedure ResetConfiguration;
    procedure Shutdown;
    function FindLogger(const name: string): ILogger;
    function GetAppenders: ICollection<IAppender>;
    function GetCurrentLoggers: ICollection<ILogger>;
    function GetLogger(const name: string): ILogger;
    property Name: string read GetName write SetName;
    property Configured: Boolean read GetConfigured write SetConfigured;
    property Properties: TStrings read GetProperties;
    property Threshold: TLevel read GetThreshold write SetThreshold;
  end;

  /// <summary>
  /// Represents an abstract class of a logger.
  /// </summary>
  TLoggerBase = class abstract(TInterfacedObject, ILogger)
  private
    fName: string;
    function GetName: string;
    function GetIsDebugEnabled: Boolean; inline;
    function GetIsInfoEnabled: Boolean; inline;
    function GetIsWarnEnabled: Boolean; inline;
    function GetIsErrorEnabled: Boolean; inline;
    function GetIsFatalEnabled: Boolean; inline;
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
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  // NEED REVISE
  TLogger = class(TLoggerBase, IAppenderAttachable)
  private
    type
      TLoggerWalkProc = reference to function(logger: TLogger): Boolean;
  strict private
    fAdditivity: Boolean;
    fLevel: TLevel;
    fParent: TLogger;
    fAppenderAttachable: IAppenderAttachable;
  protected
    function GetAdditivity: Boolean; virtual;
    function GetEffectiveLevel: TLevel; virtual;
    function GetLevel: TLevel; virtual;
    function GetParent: TLogger; virtual;
    procedure SetAdditivity(const value: Boolean); virtual;
    procedure SetParent(const value: TLogger); virtual;
    procedure SetLevel(const value: TLevel); virtual;
  protected
    fRepository: ILoggerRepository;
    fAppenderAttachableLock: IReadWriteSync;
    function GetAppenderAttachable: IAppenderAttachable;
    function GetAppenders: ICollection<IAppender>;
    procedure CallAppenders(const event: TLoggingEvent); virtual;
    procedure WalkThroughLoggers(logger: TLogger; callback: TLoggerWalkProc);
  protected
    function IsEnabledFor(const level: TLevel): Boolean; override;
    procedure DoLog(const event: TLoggingEvent); override;
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
    { Properties }
    property Name: string read fName;
    property Additivity: Boolean read GetAdditivity write SetAdditivity;
    property EffectiveLevel: TLevel read GetEffectiveLevel;
    property Level: TLevel read GetLevel write SetLevel;
    property Parent: TLogger read GetParent write SetParent;
    property Repository: ILoggerRepository read fRepository write fRepository;
  end;

  TAppenderAttachable = class(TInterfacedObject, IAppenderAttachable)
  private
    fList: ICollection<IAppender>;
    function GetAppenders: ICollection<IAppender>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAllAppenders;
    function GetAppender(const name: string): IAppender;
    function RemoveAppender(const name: string): IAppender; overload;
    function RemoveAppender(const appender: IAppender): IAppender; overload;
    property Appenders: ICollection<IAppender> read GetAppenders;
  end;

  {$REGION 'Unfiled'}
  
  /// <summary>
  /// Activate the options that were previously set with calls to properties.
  /// </summary>
  IOptionHandler = interface
    ['{7E08147F-64F8-4FAC-926C-77A2357DE5E7}']
    procedure ActivateOptions;
  end;

  TErrorCode = (
		/// <summary>
		/// A general error
		/// </summary>
		GenericFailure,

		/// <summary>
		/// Error while writing output
		/// </summary>
		WriteFailure,

		/// <summary>
		/// Failed to flush file
		/// </summary>
		FlushFailure,

		/// <summary>
		/// Failed to close file
		/// </summary>
		CloseFailure,

		/// <summary>
		/// Unable to open output file
		/// </summary>
		FileOpenFailure,

		/// <summary>
		/// No layout specified
		/// </summary>
		MissingLayout,

		/// <summary>
		/// Failed to parse address
		/// </summary>
		AddressParseFailure
  );

  /// <summary>
  /// IErrorHandler
  /// </summary>
  IErrorHandler = interface
    ['{AD75257B-8091-4A1B-A17D-618CD22B366B}']
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure Error(const msg: string; e: Exception; errorCode: TErrorCode); overload;
  end;

  /// <summary>
  /// TFilterDecision
  /// </summary>
  TFilterDecision = (
		/// <summary>
		/// The log event must be dropped immediately without
		/// consulting with the remaining filters, if any, in the chain.
		/// </summary>
    Deny,       // -1
		/// <summary>
		/// This filter is neutral with respect to the log event.
		/// The remaining filters, if any, should be consulted for a final decision.
		/// </summary>
    Natural,
		/// <summary>
		/// The log event must be logged immediately without
		/// consulting with the remaining filters, if any, in the chain.
		/// </summary>
    Accept
  );

  /// <summary>
  /// Implement this interface to provide customized logging event filtering
  /// </summary>
  IFilter = interface(IOptionHandler)
    ['{243EA758-2948-476F-A089-C832798A4948}']
    function GetNext: IFilter;
    procedure SetNext(const value: IFilter);
    function Decide(const event: TLoggingEvent): TFilterDecision;
    property Next: IFilter read GetNext write SetNext;
  end;
  
  {$ENDREGION}
  
  ELoggingException = class(Exception);

implementation

uses
  Spring.ResourceStrings,
  Spring.Logging.Utils,
  Spring.Logging.Appenders,
  Spring.Logging.Repositories,
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

{$ENDREGION}


{$REGION 'TLogger'}

constructor TLogger.Create(const name: string);
begin
  inherited Create(name);
  fAdditivity := True;
  fAppenderAttachableLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure TLogger.DoLog(const event: TLoggingEvent);
begin
  CallAppenders(event);
end;

procedure TLogger.WalkThroughLoggers(logger: TLogger; callback: TLoggerWalkProc);
begin
  while logger <> nil do
  begin
    callback(logger);
    logger := logger.Parent;
  end;
end;

procedure TLogger.CallAppenders(const event: TLoggingEvent);
var
  logger: TLogger;
  collection: ICollection<IAppender>;
  appender: IAppender;
begin
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
          appender.Append(event);
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

// TODO: Optimization (Returns false if this level is disabled globally)
function TLogger.IsEnabledFor(const level: TLevel): Boolean;
begin
  Result := EffectiveLevel.IsGreaterThanOrEqualTo(level);
end;

function TLogger.GetEffectiveLevel: TLevel;
var
  logger: TLogger;
begin
  Result := fLevel;
  logger := Self;
  while Result = nil do
  begin
    logger := logger.Parent;
    Result := logger.Level;
  end;
  Assert(Result <> nil, 'EffectiveLevel should never be null.');
end;

procedure TLogger.SetAdditivity(const value: Boolean);
begin
  fAdditivity := value;
end;

procedure TLogger.SetLevel(const value: TLevel);
begin
  fLevel := value;
end;

procedure TLogger.SetParent(const value: TLogger);
begin
  fParent := value;
end;

{$ENDREGION}


{$REGION 'TAppenderAttachable'}

constructor TAppenderAttachable.Create;
begin
  inherited Create;
  fList := TCollections.CreateList<IAppender>;
end;

destructor TAppenderAttachable.Destroy;
begin

  inherited;
end;

procedure TAppenderAttachable.AddAppender(const appender: IAppender);
begin
  TArgument.CheckNotNull(appender, 'appender');
  fList.Add(appender);
end;

function TAppenderAttachable.GetAppender(const name: string): IAppender;
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

function TAppenderAttachable.GetAppenders: ICollection<IAppender>;
begin
  Result := fList;
end;

procedure TAppenderAttachable.RemoveAllAppenders;
begin
  fList.Clear;
end;

function TAppenderAttachable.RemoveAppender(const name: string): IAppender;
begin
  Result := GetAppender(name);
  Result := RemoveAppender(Result);
end;

function TAppenderAttachable.RemoveAppender(
  const appender: IAppender): IAppender;
begin
  TArgument.CheckNotNull(appender, 'appender');
  Result := appender;
  fList.Remove(Result);
end;

{$ENDREGION}

end.
