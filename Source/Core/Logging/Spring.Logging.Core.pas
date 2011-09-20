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

/// <summary>
/// Provides the default implementation of the logging service in spring4d.
/// </summary>
unit Spring.Logging.Core;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Services.Logging;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///   Defines the default set of levels recognized by the logging service.
  /// </summary>
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
    {$HINTS OFF}
    class destructor Destroy;
    {$HINTS ON}
  public
    constructor Create(const levelValue: Integer; const levelName: string);

    function Equals(obj: TObject): Boolean; override;
    function ToString: string; override;
    function IsGreaterThan(level: TLevel): Boolean; inline;
    function IsGreaterThanOrEqualTo(level: TLevel): Boolean; inline;

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
  public
    LoggerName: string;
    LoggerLevel: TLevel;
    TimeStamp: TDateTime;
    ThreadID: TThreadID;
    Message: string;
    ErrorMessage: string;
  end;

  IHierarchicalLogger = interface(ILogger)
    ['{4A229971-00AB-47E8-9BD4-4F01C6E47017}']
    {$REGION 'Property Accessors'}
      function GetLevel: TLevel;
      function GetParent: IHierarchicalLogger;
      function GetAdditivity: Boolean;
      procedure SetLevel(const value: TLevel);
      procedure SetParent(const value: IHierarchicalLogger);
      procedure SetAdditivity(const value: Boolean);
    {$ENDREGION}

    function GetEffectiveLevel: TLevel;

    property Level: TLevel read GetLevel write SetLevel;
    property Parent: IHierarchicalLogger read GetParent;
    property Additivity: Boolean read GetAdditivity write SetAdditivity;
  end;

  IAppender = interface
    ['{76735B1E-5AF3-4DC3-A19C-A35FFC13DB55}']
    {$REGION 'Property Accessors'}
      function GetName: string;
      procedure SetName(const value: string);
    {$ENDREGION}

    procedure Close;
    procedure Append(const event: TLoggingEvent); overload;
    procedure Append(const events: IEnumerable<TLoggingEvent>); overload;
    property Name: string read GetName write SetName;
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
    {$REGION 'Property Accessors'}
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

  ILoggerManager = interface(ILoggerFactory)
    ['{C44BAA97-C52A-49DE-9E4D-B1721A4F5405}']
    {$REGION 'Property Accessors'}
      function GetName: string;
      function GetThreshold: TLevel;
      function GetRoot: IHierarchicalLogger;
      function GetAppenders: IList<IAppender>;
      procedure SetName(const value: string);
      procedure SetThreshold(const value: TLevel);
    {$ENDREGION}

    procedure SendLoggingEvent(const sender: ILogger; const event: TLoggingEvent);

    function CreateAppender(const typeName: string): IAppender;
    function CreateLayout(const typeName: string): ILayout;

    procedure AddAppender(const appender: IAppender);
    procedure RemoveAppender(const appender: IAppender);

    procedure AddAppenderRef(const logger: ILogger; const appender: IAppender);
    procedure RemoveAppenderRef(const logger: ILogger; const appender: IAppender);

    function FindLevel(const name: string): TLevel;
    function FindLogger(const name: string): ILogger;
    function FindAppender(const name: string): IAppender;

    function GetLoggers: IEnumerable<ILogger>;

    property Name: string read GetName write SetName;
    property Threshold: TLevel read GetThreshold write SetThreshold;
    property Root: IHierarchicalLogger read GetRoot;
    property Appenders: IList<IAppender> read GetAppenders;
  end;

implementation

uses
  Spring.Logging.Utils,
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
  FreeAndNil(fAll);
  FreeAndNil(fDebug);
  FreeAndNil(fInfo);
  FreeAndNil(fWarn);
  FreeAndNil(fError);
  FreeAndNil(fFatal);
  FreeAndNil(fOff);
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

end.
