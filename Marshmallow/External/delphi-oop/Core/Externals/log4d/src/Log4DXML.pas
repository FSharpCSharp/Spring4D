unit Log4DXML;

{
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/MPL-1.1.html
  
  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}

{
  Logging for Delphi - XML support.
  Based on log4j Java package from Apache
  (http://jakarta.apache.org/log4j/docs/index.html).

  XML document layout.
  Configurator from an XML document (uses MSXML v3 for parsing).

  Written by Keith Wood (kbwood@iprimus.com.au).
  Version 1.0 - 29 April 2001.
  Version 1.2 - 9 September 2003.
}

interface

{$I Defines.inc}

uses
  Classes, SysUtils, Windows, Log4D, ComObj, ActiveX, MSXML2_tlb;

type
  { This layout outputs events as an XML fragment. }
  TLogXMLLayout = class(TLogCustomLayout)
  protected
    function GetContentType: string; override;
    function GetFooter: string; override;
    function GetHeader: string; override;
  public
    function Format(const Event: TLogEvent): string; overload; override;
    function Format(const LoggerName, LevelName: string;
      const LevelValue, ThreadId: Integer; const Timestamp: TDateTime;
      const ElapsedTime: Integer;
      const Message, NDC, ErrorMsg, ErrorClass: string): string; reintroduce; overload; 
    function IgnoresException: Boolean; override;
  end;

  { Extends BasicConfigurator to provide configuration from an external XML
    file. See log4d.dtd for the expected format.

    It is sometimes useful to see how Log4D is reading configuration files.
    You can enable Log4D internal logging by defining the configDebug
    attribute on the log4d:configuration element. }
  TLogXMLConfigurator = class(TLogBasicConfigurator,
    IVBSAXContentHandler, IVBSAXErrorHandler)
  protected
    FAppender: ILogAppender;
    FErrorHandler: ILogErrorHandler;
    FFilter: ILogFilter;
    FHandlers: TInterfaceList;
    FHierarchy: TLogHierarchy;
    FLayout: ILogLayout;
    FLocator: IVBSAXLocator;
    FLogger: TLogLogger;
    FLoggerFactory: ILogLoggerFactory;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Configure(const ConfigURL: string); overload;
    class procedure Configure(const Document: TStream); overload;
    procedure DoConfigure(const ConfigURL: string;
      const Hierarchy: TLogHierarchy); overload;
    procedure DoConfigure(const Document: TStream;
      const Hierarchy: TLogHierarchy); overload;
    { IVBSAXContentHandler }
{$IFDEF MSXML4}
    procedure _Set_documentLocator(const Param1: IVBSAXLocator); safecall;
{$ELSE}
    procedure Set_documentLocator(const Param1: IVBSAXLocator); safecall;
{$ENDIF}
    procedure StartDocument; safecall;
    procedure EndDocument; safecall;
    procedure StartPrefixMapping(var strPrefix: WideString;
      var strURI: WideString); safecall;
    procedure EndPrefixMapping(var strPrefix: WideString); safecall;
    procedure StartElement(var strNamespaceURI: WideString;
      var strLocalName: WideString; var strQName: WideString;
      const oAttributes: IVBSAXAttributes); safecall;
    procedure EndElement(var strNamespaceURI: WideString;
      var strLocalName: WideString; var strQName: WideString); safecall;
    procedure Characters(var strChars: WideString); safecall;
    procedure IgnorableWhitespace(var strChars: WideString); safecall;
    procedure ProcessingInstruction(var strTarget: WideString;
      var strData: WideString); safecall;
    procedure SkippedEntity(var strName: WideString); safecall;
    { IVBSAXErrorHandler }
    procedure Error(const oLocator: IVBSAXLocator;
      var strErrorMessage: WideString; nErrorCode: Integer); safecall;
    procedure FatalError(const oLocator: IVBSAXLocator;
      var strErrorMessage: WideString; nErrorCode: Integer); safecall;
    procedure IgnorableWarning(const oLocator: IVBSAXLocator;
      var strErrorMessage: WideString; nErrorCode: Integer); safecall;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
      stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      stdcall;
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

const
  { Element and attribute names from the XML configuration document. }
  AdditiveAttr        = 'additive';
  AppenderTag         = 'appender';
  AppenderRefTag      = 'appender-ref';
  ClassAttr           = 'class';
  ConfigurationTag    = 'log4d:configuration';
  DebugAttr           = 'configDebug';
  ErrorHandlerTag     = 'errorHandler';
  FilterTag           = 'filter';
  LayoutTag           = 'layout';
  LevelTag            = 'level';
  LoggerTag           = 'logger';
  LoggerFactoryAttr   = 'loggerFactory';
  NameAttr            = 'name';
  ParamTag            = 'param';
  RefAttr             = 'ref';
  RendererTag         = 'renderer';
  RenderedClassAttr   = 'renderedClass';
  RenderingClassAttr  = 'renderingClass';
  RootTag             = 'root';
  ThresholdAttr       = 'threshold';
  ValueAttr           = 'value';
  { Element and attribute names from the XML events document. }
  NamespacePrefix     = 'log4d:';
  ElapsedAttr         = 'elapsed';
  EventSetTag         = NamespacePrefix + 'eventSet';
  EventTag            = NamespacePrefix + 'event';
  ExceptionTag        = NamespacePrefix + 'exception';
  LevelNameAttr       = 'levelName';
  LevelValueAttr      = 'levelValue';
  LoggerAttr          = 'logger';
  MessageTag          = NamespacePrefix + 'message';
  NDCTag              = NamespacePrefix + 'NDC';
  ThreadAttr          = 'thread';
  TimestampAttr       = 'timestamp';

  TimestampFormat     = 'yyyymmddhhnnsszzz';

implementation

const
  CRLF = #13#10;

resourcestring
  AppenderErrorHMsg = 'Appender "%s" uses error handler "%s"';
  AppenderFilterMsg = 'Appender "%s" uses filter "%s"';
  AppenderLayoutMsg = 'Appender "%s" uses layout "%s"';
  ErrorMsg          = 'Error';
  FatalMsg          = 'Fatal error';
  FinishedConfigMsg = 'Finished configuring "%s"';
  IgnoringConfigMsg = 'Ignoring configuration file';
  LoggerLevelMsg    = 'Logger "%s" level set to "%s"';
  NoAppenderMsg     = 'Appender "%s" was not found';
  ParsedAppenderMsg = 'Parsed appender "%s"';
  ParsedLoggerMsg   = 'Parsed logger "%s"';
  ParsedRootMsg     = 'Parsed root logger';
  SAXErrorMsg       = '%s during configuration: %s at %d,%d in %s';
  WarningMsg        = 'Warning';

{ TLogXMLLayout ---------------------------------------------------------------}

{ Write an XML element for each event. }
function TLogXMLLayout.Format(const Event: TLogEvent): string;
var
  ClassName: string;
begin
  if Event.Error = nil then
    ClassName := ''
  else
    ClassName := Event.Error.ClassName;
  Result := Format(Event.LoggerName, Event.Level.Name, Event.Level.Level,
    Event.ThreadId, Event.TimeStamp, Event.ElapsedTime, Event.Message,
    Event.NDC, Event.ErrorMessage, ClassName);
end;

{ Write an XML element for each event. }
function TLogXMLLayout.Format(const LoggerName, LevelName: string;
  const LevelValue, ThreadId: Integer;
  const Timestamp: TDateTime; const ElapsedTime: Integer;
  const Message, NDC, ErrorMsg, ErrorClass: string): string;
begin
  Result := '<' + EventTag + ' ' + LoggerAttr + '="' + LoggerName +
    '" ' + LevelNameAttr + '="' + LevelName +
    '" ' + LevelValueAttr + '="' + IntToStr(LevelValue) +
    '" ' + ThreadAttr + '="' + IntToStr(ThreadId) + '" ' +
    TimestampAttr + '="' + FormatDateTime(TimestampFormat, Timestamp) +
    '" ' + ElapsedAttr + '="' + IntToStr(ElapsedTime) +
    '"><' + MessageTag + '><![CDATA[' + Message + ']]></' +
    MessageTag + '>';
  if NDC <> '' then
    Result := Result + '<' + NDCTag + '><![CDATA[' + NDC + ']]></' +
      NDCTag + '>';
  if ErrorMsg <> '' then
    Result := Result + '<' + ExceptionTag + ' ' + ClassAttr + '="' +
      ErrorClass + '"><![CDATA[' + ErrorMsg + ']]></' + ExceptionTag + '>';
  Result := Result + '</' + EventTag + '>' + CRLF;
end;

{ Returns the content type output by this layout, i.e 'text/xml'. }
function TLogXMLLayout.GetContentType: string;
begin
  Result := 'text/xml';
end;

{ Returns appropriate XML closing tags. }
function TLogXMLLayout.GetFooter: string;
begin
  Result := '</' + EventSetTag + '>' + CRLF;
end;

{ Returns appropriate XML opening tags. }
function TLogXMLLayout.GetHeader: string;
begin
  Result := '<?xml version="1.0"?>' + CRLF +
    '<!DOCTYPE ' + EventSetTag + ' SYSTEM "log4d.dtd">' + CRLF +
    '<' + EventSetTag + ' version="1.2" xmlns:' +
    Copy(NamespacePrefix, 1, Length(NamespacePrefix) - 1) +
    '="http://log4d.sourceforge.net/log4d">' + CRLF;
end;

{ The XML layout handles the exception contained in logging events.
  Hence, this method return False. }
function TLogXMLLayout.IgnoresException: Boolean;
begin
  Result := False;
end;

{ TLogXMLConfigurator ---------------------------------------------------------}

const
  InternalRootName = 'root';

class procedure TLogXMLConfigurator.Configure(const ConfigURL: string);
var
  Config: TLogXMLConfigurator;
begin
  Config := TLogXMLConfigurator.Create;
  try
    Config.DoConfigure(ConfigURL, DefaultHierarchy);
  finally
    Config.Free;
  end;
end;

class procedure TLogXMLConfigurator.Configure(const Document: TStream);
var
  Config: TLogXMLConfigurator;
begin
  Config := TLogXMLConfigurator.Create;
  try
    Config.DoConfigure(Document, DefaultHierarchy);
  finally
    Config.Free;
  end;
end;

constructor TLogXMLConfigurator.Create;
begin
  inherited Create;
  FLoggerFactory := TLogDefaultLoggerFactory.Create;
  FHandlers      := TInterfaceList.Create;
end;

destructor TLogXMLConfigurator.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

procedure TLogXMLConfigurator.DoConfigure(const ConfigURL: string;
  const Hierarchy: TLogHierarchy);
var
  XMLReader: IVBSAXXMLReader;
begin
  FHierarchy := Hierarchy;
{$ifdef MSSAX}
  XMLReader  := ComsSAXXMLReader.Create;
{$else}
  XMLReader  := CoSAXXMLReader.Create;
{$endif}
  XMLReader.ContentHandler := Self;
  XMLReader.ErrorHandler   := Self;
  XMLReader.ParseURL(ConfigURL);
  LogLog.Debug(Format(FinishedConfigMsg, [ClassName]));
end;

procedure TLogXMLConfigurator.DoConfigure(const Document: TStream;
  const Hierarchy: TLogHierarchy);
var
  Stream: IStream;
  XMLReader: IVBSAXXMLReader;
begin
  Stream     := TStreamAdapter.Create(Document);
  FHierarchy := Hierarchy;
{$ifdef MSSAX}
  XMLReader  := ComsSAXXMLReader.Create;
{$else}
  XMLReader  := CoSAXXMLReader.Create;
{$endif}
  XMLReader.ContentHandler := Self;
  XMLReader.ErrorHandler   := Self;
  XMLReader.Parse(Stream);
  LogLog.Debug(Format(FinishedConfigMsg, [ClassName]));
end;

{ IVBSAXContentHandler --------------------------------------------------------}

procedure TLogXMLConfigurator.EndDocument;
begin
  { Do nothing. }
end;

{ Pop current option handler off the stack at the end of the element. }
procedure TLogXMLConfigurator.EndElement(var strNamespaceURI: WideString;
  var strLocalName: WideString; var strQName: WideString);
begin
  if (strQName = AppenderTag) or (strQName = LoggerTag) or
      (strQName = ErrorHandlerTag) or (strQName = FilterTag) or
      (strQName = LayoutTag) or (strQName = RootTag) then
    FHandlers.Delete(FHandlers.Count - 1);
  if (strQName = LoggerTag) or (strQName = RootTag) then
    FLogger.UnlockLogger;
end;

procedure TLogXMLConfigurator.EndPrefixMapping(var strPrefix: WideString);
begin
  { Do nothing. }
end;

procedure TLogXMLConfigurator.Characters(var strChars: WideString);
begin
  { Do nothing. }
end;

procedure TLogXMLConfigurator.IgnorableWhitespace(var strChars: WideString);
begin
  { Do nothing. }
end;

procedure TLogXMLConfigurator.ProcessingInstruction(var strTarget: WideString;
  var strData: WideString);
begin
  { Do nothing. }
end;

{ Save locator for later error reporting. }
{$IFDEF MSXML4}
procedure TLogXMLConfigurator._Set_documentLocator(const Param1: IVBSAXLocator);
{$ELSE}
procedure TLogXMLConfigurator.Set_documentLocator(const Param1: IVBSAXLocator);
{$ENDIF}
begin
  FLocator := Param1;
end;

procedure TLogXMLConfigurator.SkippedEntity(var strName: WideString);
begin
  { Do nothing. }
end;

procedure TLogXMLConfigurator.StartDocument;
begin
  FHandlers.Clear;
end;

{ Create new objects as elements are encountered and handle any attributes
  defined for them. }
procedure TLogXMLConfigurator.StartElement(var strNamespaceURI: WideString;
  var strLocalName: WideString; var strQName: WideString;
  const oAttributes: IVBSAXAttributes);
var
  Name: string;

  { Retrieve named attribute, returning an empty string if not there. }
  function GetAttribute(Name: string): string;
  begin
    try
      Result := oAttributes.getValueFromQName(Name);
    except on e: EOleException do
      Result := '';
    end;
  end;

begin
try
  if strQName = AppenderTag then
  begin
    { New appender. }
    FAppender := FindAppender(GetAttribute(ClassAttr));
    // if Appender is not found use NullAppender
    if FAppender = nil then
    begin
      LogLog.Error(Format(NoAppenderMsg, [GetAttribute(ClassAttr)]));
      FAppender := FindAppender('TLogNullAppender');
    end;
    if not Assigned(FAppender) then
      Abort;
    FAppender.Name := GetAttribute(NameAttr);
    AppenderPut(FAppender);
    FHandlers.Add(FAppender);
    LogLog.Debug(Format(ParsedAppenderMsg, [FAppender.Name]));
  end
  else if strQName = AppenderRefTag then
  begin
    { Reference to an appender for a logger. }
    Name      := GetAttribute(RefAttr);
    FAppender := AppenderGet(Name);
    if not Assigned(FAppender) then
      LogLog.Error(Format(NoAppenderMsg, [Name]))
    else
      FLogger.AddAppender(FAppender);
  end
  else if strQName = LoggerTag then
  begin
    { New logger. }
    FLogger          :=
      FHierarchy.GetLogger(GetAttribute(NameAttr), FLoggerFactory);
    FLogger.LockLogger;
    FLogger.Additive := StrToBool(GetAttribute(AdditiveAttr), True);
    (FLogger as ILogOptionHandler)._AddRef;
    FHandlers.Add(FLogger);
    LogLog.Debug(Format(ParsedLoggerMsg, [FLogger.Name]));
  end
  else if strQName = ConfigurationTag then
  begin
    { Global settings. }
    SetGlobalProps(FHierarchy, GetAttribute(LoggerFactoryAttr),
      GetAttribute(DebugAttr), GetAttribute(ThresholdAttr));
  end
  else if strQName = ErrorHandlerTag then
  begin
    { Error handler for an appender. }
    Name                   := GetAttribute(ClassAttr);
    FErrorHandler          := FindErrorHandler(Name);
    FAppender.ErrorHandler := FErrorHandler;
    FHandlers.Add(FErrorHandler);
    LogLog.Debug(Format(AppenderErrorHMsg, [FAppender.Name, Name]));
  end
  else if strQName = FilterTag then
  begin
    { New filter for an appender. }
    Name    := GetAttribute(ClassAttr);
    FFilter := FindFilter(Name);
    FAppender.AddFilter(FFilter);
    FHandlers.Add(FFilter);
    LogLog.Debug(Format(AppenderFilterMsg, [FAppender.Name, Name]));
  end
  else if strQName = LayoutTag then
  begin
    { Layout for an appender. }
    Name             := GetAttribute(ClassAttr);
    FLayout          := FindLayout(Name);
    FAppender.Layout := FLayout;
    FHandlers.Add(FLayout);
    LogLog.Debug(Format(AppenderLayoutMsg, [FAppender.Name, Name]));
  end
  else if strQName = ParamTag then
  begin
    { Parameter for an enclosing element (which must be an option handler). }
    ILogOptionHandler(FHandlers.Last).Options[GetAttribute(NameAttr)] :=
      GetAttribute(ValueAttr);
  end
  else if strQName = LevelTag then
  begin
    { Level for a logger. }
    Name := LowerCase(GetAttribute(ValueAttr));
    if (Name = InheritedLevel) and (FLogger.Name <> InternalRootName) then
      FLogger.Level := nil
    else
    begin
      FLogger.Level := TLogLevel.GetLevel(Name);
      LogLog.Debug(Format(LoggerLevelMsg, [FLogger.Name, FLogger.Level.Name]));
    end;
  end
  else if strQName = RendererTag then
  begin
    { Renderer and rendered class. }
    AddRenderer(FHierarchy, GetAttribute(RenderedClassAttr),
      GetAttribute(RenderingClassAttr));
  end
  else if strQName = RootTag then
  begin
    { Configure the root logger. }
    FLogger := FHierarchy.Root;
    FLogger.LockLogger;
    (FLogger as ILogOptionHandler)._AddRef;
    FHandlers.Add(FLogger);
    LogLog.Debug(ParsedRootMsg);
  end
except
  on E: Exception do
  begin
    LogLog.Debug('Exception', E);
    raise;
  end;
end;

end;

procedure TLogXMLConfigurator.StartPrefixMapping(var strPrefix: WideString;
  var strURI: WideString);
begin
  { Do nothing. }
end;

{ IVBSAXErrorHandler ----------------------------------------------------------}

{ Log any errors. }
procedure TLogXMLConfigurator.Error(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  LogLog.Error(Format(SAXErrorMsg, [ErrorMsg, strErrorMessage,
    oLocator.LineNumber, oLocator.ColumnNumber, oLocator.SystemId]), nil);
end;

{ Log any fatal errors and abort the configuration process. }
procedure TLogXMLConfigurator.FatalError(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  LogLog.Fatal(Format(SAXErrorMsg, [FatalMsg, strErrorMessage,
    oLocator.LineNumber, oLocator.ColumnNumber, oLocator.SystemId]), nil);
  LogLog.Fatal(IgnoringConfigMsg);
  Abort;
end;

{ Log any warnings. }
procedure TLogXMLConfigurator.IgnorableWarning(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  LogLog.Warn(Format(SAXErrorMsg, [WarningMsg, strErrorMessage,
    oLocator.LineNumber, oLocator.ColumnNumber, oLocator.SystemId]), nil);
end;

{ IDispatch -------------------------------------------------------------------}

{ These functions are required by the IDispatch interface but are not used. }
function TLogXMLConfigurator.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLogXMLConfigurator.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo):
  HResult;
begin
  Result := E_NOTIMPL;
end;

function TLogXMLConfigurator.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLogXMLConfigurator.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word;
  var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ IUnknown --------------------------------------------------------------------}

{ These functions are required by the IUnknown interface but are not used. }
function TLogXMLConfigurator.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

function TLogXMLConfigurator._AddRef: Integer;
begin
  Result := 1;
end;

function TLogXMLConfigurator._Release: Integer;
begin
  Result := 1;
end;

initialization
  CoInitialize(nil);
  { Registration of standard implementations. }
  RegisterLayout(TLogXMLLayout);
finalization
  CoUninitialize;
end.
