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

unit Spring.Logging;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Spring.System,
  Spring.Collections,
  Spring.Patterns,
  Spring.ResourceStrings,
  Spring.Logging.Core;

type
  { Forward Declarations }
  TLogManager = class;
  TGlobalContext = class;
  TThreadContext = class;
  TLogicalThreadContext = class;

  /// <summary>
  /// The ILogger interface is used by application to log messages.
  /// </summary>
  ILogger = interface
    ['{803DC36C-03FE-4C5C-B8BE-9CB79076DCB2}']
  {$REGION 'Property Getters & Setters'}
    function GetIsDebugEnabled: Boolean;
    function GetIsInfoEnabled: Boolean;
    function GetIsWarnEnabled: Boolean;
    function GetIsErrorEnabled: Boolean;
    function GetIsFatalEnabled: Boolean;
  {$ENDREGION}
    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const format: string; const args: array of const );
    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const format: string; const args: array of const );
    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const format: string; const args: array of const );
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const format: string; const args: array of const );
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const format: string; const args: array of const );
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  // TODO: RenderMap, PluginMap, LevelMap
  ILoggerRepository = interface(IObservable<ILoggerRepositoryListener>)
    ['{C44BAA97-C52A-49DE-9E4D-B1721A4F5405}']
    function GetName: string;
    function GetConfigured: Boolean;
    function GetProperties: TStrings;
    function GetThreshold: TLevel;
    procedure SetName(const value: string);
    procedure SetConfigured(const value: Boolean);
    procedure SetThreshold(const value: TLevel);
    procedure Log(const loggingEvent: TLoggingEvent);
    procedure ResetConfiguration;
    procedure Shutdown;
    function Exists(const name: string): ILogger;
    function GetAppenders: ICollection<IAppender>;
    function GetCurrentLoggers: ICollection<ILogger>;
    function GetLogger(const name: string): ILogger;
    property Name: string read GetName write SetName;
    property Configured: Boolean read GetConfigured write SetConfigured;
    property Properties: TStrings read GetProperties;
    property Threshold: TLevel read GetThreshold write SetThreshold;
  end;

  /// <summary>
  /// TLoggerManager
  /// </summary>
  TLogManager = class sealed(TObject)
  strict private
    class var
      fInstance: TLogManager;
      fLoggerRepository: ILoggerRepository;
  private
    class constructor Create;
    class destructor Destroy;
    class function GetLoggerRepository: ILoggerRepository;
  public
    procedure Configure(const xmlFileName: string);
    class function Exists(const name: string): ILogger;
    class procedure ResetConfiguration;
    class procedure Shutdown;
    class function GetLogger(const name: string): ILogger; overload;
    class function GetLogger(typeInfo: PTypeInfo): ILogger; overload;
    class property Instance: TLogManager read fInstance;
  end;

  TGlobalContext = class
  public
    // property Properties[const name: string]: string read GetProperty write SetProperty;
  end;

  TThreadContext = class

  end;

  TLogicalThreadContext = class

  end;

  TBasicConfigurator = class
  public
    class procedure Config;
  end;

  TXmlConfigurator = class
  public
    class procedure Config;
  end;

  TLoggerList = TListAdapter<ILogger>;

function LogManager: TLogManager;

implementation

uses
  XMLIntf,
  XMlDoc,
  Spring.Logging.Appenders,
  Spring.Logging.Filters,
  Spring.Logging.Layouts,
  Spring.Logging.Repositories;

{$REGION 'Routines'}

function LogManager: TLogManager;
begin
  Result := TLogManager.Instance;
end;

{$ENDREGION}


{$REGION 'TLoggerManager'}

class constructor TLogManager.Create;
begin
  fInstance := TLogManager.Create;
  fLoggerRepository := THierarchy.Create;
end;

class destructor TLogManager.Destroy;
begin
  fLoggerRepository := nil;
  FreeAndNil(fInstance);
end;

procedure TLogManager.Configure(const xmlFileName: string);
var
  xml: IXMLDocument;
  xmlNode: IXMLNode;
begin
  xml := LoadXMLDocument(xmlFileName);
  xmlNode := xml.DocumentElement;
  (GetLoggerRepository as IXmlRepositoryConfigurator).Configure(xmlNode);
end;

class function TLogManager.GetLogger(const name: string): ILogger;
begin
  Result := GetLoggerRepository.GetLogger(name);
end;

class function TLogManager.GetLogger(typeInfo: PTypeInfo): ILogger;
begin
  Result := GetLoggerRepository.GetLogger(TRtti.GetFullName(typeInfo));
end;

class function TLogManager.GetLoggerRepository: ILoggerRepository;
begin
  Result := fLoggerRepository;
end;

class function TLogManager.Exists(const name: string): ILogger;
begin

end;

class procedure TLogManager.ResetConfiguration;
begin

end;

class procedure TLogManager.Shutdown;
begin

end;

{$ENDREGION}


{$REGION 'TBasicConfigurator'}

class procedure TBasicConfigurator.Config;
var
  layout: TSimpleLayout;
  appender: TConsoleAppender;
  repository: ILoggerRepository;
  configurator: IBasicRepositoryConfigurator;
begin
  layout := TSimpleLayout.Create;
  layout.ActivateOptions;

  appender := TConsoleAppender.Create;
  appender.layout := layout;
  appender.ActivateOptions;

  if Supports(repository, IBasicRepositoryConfigurator, configurator) then
  begin
    configurator.Configure(appender);
  end;
end;
{$ENDREGION}


{$REGION 'TXmlConfigurator'}

class procedure TXmlConfigurator.Config;
begin

end;

{$ENDREGION}


end.
