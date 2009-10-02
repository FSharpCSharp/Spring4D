{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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

unit Spring.Logging.Core;

{$I Spring.inc}

{$SCOPEDENUMS ON}  // Enable Scoped Enumerations Synax

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  XmlIntf,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.ResourceStrings;

type
  { Forward Declarations }
  TLoggingEvent   = class;

  IOptionHandler  = interface;
  IAppender       = interface;
  IBulkAppender   = interface;
  ILayout         = interface;
  IFilter         = interface;


  {$IFDEF SUPPORTS_REGION} {$REGION 'TLevel'} {$ENDIF}

  /// <summary>
  /// Defines the default set of levels recognized by the system.
  /// </summary>
  TLevel = record
  strict private
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
      fNull:  TLevel;
    function GetIsNull: Boolean;
    class constructor Create; overload;
  public
    constructor Create(const levelValue: Integer; const levelName: string); overload;
    property Name: string read fName;
    property Value: Integer read fValue;
    property IsNull: Boolean read GetIsNull;
    class property All: TLevel read fAll;
    class property Debug: TLevel read fDebug;
    class property Info: TLevel read fInfo;
    class property Warn: TLevel read fWarn;
    class property Error: TLevel read fError;
    class property Fatal: TLevel read fFatal;
    class property Off: TLevel read fOff;
    class property Null: TLevel read fNull;
    { Comparison Operators }
    class operator Equal(const left, right: TLevel): Boolean;
    class operator NotEqual(const left, right: TLevel): Boolean;
    class operator GreaterThan(const left, right: TLevel): Boolean;
    class operator GreaterThanOrEqual(const left, right: TLevel): Boolean;
    class operator LessThan(const left, right: TLevel): Boolean;
    class operator LessThanOrEqual(const left, right: TLevel): Boolean;
  end;

  {$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


  {$REGION 'TErrorCode'}

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

  {$ENDREGION}


  /// <summary>
  /// Activate the options that were previously set with calls to properties.
  /// </summary>
  IOptionHandler = interface
    ['{7E08147F-64F8-4FAC-926C-77A2357DE5E7}']
    procedure ActivateOptions;
  end;

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
  /// IAppender
  /// </summary>
  IAppender = interface
    ['{76735B1E-5AF3-4DC3-A19C-A35FFC13DB55}']
    function GetName: string;
    procedure SetName(const value: string);
    procedure Close;
    procedure Append(const loggingEvent: TLoggingEvent);
    property Name: string read GetName write SetName;
  end;

  /// <summary>
  /// IBulkAppender
  /// </summary>
  IBulkAppender = interface(IAppender)
    ['{E5EADA47-4ED2-4097-8BCC-25B94716F1E3}']
    procedure Append(const loggingEvents: ICollection<TLoggingEvent>);
  end;

  /// <summary>
  /// Interface for attaching, removing and retrieving appenders.
  /// </summary>
  IAppenderAttachable = interface
    ['{25687F5E-642E-48D6-9DD9-C76C7443CE71}']
    function GetAppenders: ICollection<IAppender>;
    procedure AddAppender(const appender: IAppender);
    procedure RemoveAllAppenders;
    function GetAppender(const name: string): IAppender;
    function RemoveAppender(const name: string): IAppender; overload;
    function RemoveAppender(const appender: IAppender): IAppender; overload;
    property Appenders: ICollection<IAppender> read GetAppenders;
  end;

  /// <summary>
  /// ILayout
  /// </summary>
  ILayout = interface
    ['{96A344B1-6104-4C3D-A41F-0F5336BE2800}']
    function GetContentType: string;
    function GetHeader: string;
    function GetFooter: string;
    function GetIgnoresException: Boolean;
    procedure Format(writer: TTextWriter; const event: TLoggingEvent);
    property ContentType: string read GetContentType;
    property Header: string read GetHeader;
    property Footer: string read GetFooter;
    property IgnoresException: Boolean read GetIgnoresException;
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
    function Decide(const loggingEvent: TLoggingEvent): TFilterDecision;
    property Next: IFilter read GetNext write SetNext;
  end;

  ELogException = class(Exception);

  ILoggerRepositoryListener = interface
    procedure OnConfigurationChanged;
    procedure OnConfigurationReset;
    procedure OnShutdown;
  end;

  ILoggable = interface
    procedure Log(const event: TLoggingEvent);
  end;

  TLoggingEventData = record
//    Domain: string;
    ExceptionString: string;
    Identity: string;
    Level: TLevel;
    LocationInfo: string;
    LoggerName: string;
    Message: string;
//    MessageObject: TFunc<TObject>;
//    Properties: IDictionary<string, string>;
    ThreadName: string;
    TimeStamp: TDateTime;
    UserName: string;
  end;

  TLoggingEvent = class
  private
    fData: TLoggingEventData;
    function GetExceptionString: string;
    function GetIdentity: string;
    function GetLevel: TLevel;
    function GetLocationInfo: string;
    function GetLoggerName: string;
    function GetMessage: string;
    function GetThreadName: string;
    function GetTimeStamp: TDateTime;
    function GetUserName: string;
  public
    procedure WriteRenderedMessage(writer: TTextWriter);
//    Domain: string;
    property ExceptionString: string read GetExceptionString;
    property Identity: string read GetIdentity;
    property Level: TLevel read GetLevel;
    property LocationInfo: string read GetLocationInfo;
    property LoggerName: string read GetLoggerName;
    property Message: string read GetMessage;
//  property   Properties: IDictionary<string, string> read Get;
    property ThreadName: string read GetThreadName;
    property TimeStamp: TDateTime read GetTimeStamp;
    property UserName: string read GetUserName;
  end;

  ILoggerWrapper = interface
//    function GetLogger: ILoggable;
//    property Logger: ILoggable read GetLogger;
  end;

  /// <summary>
  /// Mapping between string name and Level object
  /// </summary>
  TLevelMap = record

  end;

  TAppenderList = TListAdapter<IAppender>;

implementation

uses
  Spring.Logging.Appenders,
  Spring.Logging.Filters,
  Spring.Logging.Utils,
  Spring.Logging.Repositories,
  Spring.Logging.ResourceStrings;


{$IFDEF SUPPORTS_REGION} {$REGION 'TLevel'} {$ENDIF}

constructor TLevel.Create(const levelValue: Integer; const levelName: string);
begin
  fValue := levelValue;
  fName := levelName;
end;

class constructor TLevel.Create;
const
  fCAllValue    = Low(Integer);
  fCDebugValue  = 10000;
  fCInfoValue   = 20000;
  fCWarnValue   = 30000;
  fCErrorValue  = 40000;
  fCFatalValue  = 50000;
  fCOffValue    = High(Integer);
begin
  fAll := TLevel.Create(fCAllValue, SAllDescription);
  fDebug := TLevel.Create(fCDebugValue, SDebugDescription);
  fInfo := TLevel.Create(fCInfoValue, SInfoDescription);
  fWarn := TLevel.Create(fCWarnValue, SWarnDescription);
  fError := TLevel.Create(fCErrorValue, SErrorDescription);
  fFatal := TLevel.Create(fCFatalValue, SFatalDescription);
  fOff := TLevel.Create(fCOffValue, SOffDescription);
  fNull := TLevel.Create(fCOffValue, '');
end;

function TLevel.GetIsNull: Boolean;
begin
  Result := fName = '';
end;

class operator TLevel.Equal(const left, right: TLevel): Boolean;
begin
  Result := left.Value = right.Value;
end;

class operator TLevel.NotEqual(const left, right: TLevel): Boolean;
begin
  Result := left.Value <> right.Value;
end;

class operator TLevel.GreaterThan(const left, right: TLevel): Boolean;
begin
  Result := left.Value > right.Value;
end;

class operator TLevel.GreaterThanOrEqual(const left, right: TLevel): Boolean;
begin
  Result := left.Value >= right.Value;
end;

class operator TLevel.LessThan(const left, right: TLevel): Boolean;
begin
  Result := left.Value < right.Value;
end;

class operator TLevel.LessThanOrEqual(const left, right: TLevel): Boolean;
begin
  Result := left.Value <= right.Value;
end;

{$IFDEF SUPPORTS_REGION} {$ENDREGION} {$ENDIF}


{$REGION 'TLoggingEvent'}

procedure TLoggingEvent.WriteRenderedMessage(writer: TTextWriter);
begin
  if Self.Message <> '' then
  begin
    writer.WriteLine(Self.Message);
  end;
  { TODO: FindAndRender Object }
end;

function TLoggingEvent.GetExceptionString: string;
begin
  Result := fData.ExceptionString;
end;

function TLoggingEvent.GetIdentity: string;
begin
  Result := fData.Identity;
end;

function TLoggingEvent.GetLevel: TLevel;
begin
  Result := fData.Level;
end;

function TLoggingEvent.GetLocationInfo: string;
begin
  Result := fData.LocationInfo;
end;

function TLoggingEvent.GetLoggerName: string;
begin
  Result := fData.LoggerName;
end;

function TLoggingEvent.GetMessage: string;
begin
  Result := fData.Message;
end;

function TLoggingEvent.GetThreadName: string;
begin
  Result := fData.ThreadName;
end;

function TLoggingEvent.GetTimeStamp: TDateTime;
begin
  Result := fData.TimeStamp;
end;

function TLoggingEvent.GetUserName: string;
begin
  Result := fData.UserName;
end;

{$ENDREGION}

end.
