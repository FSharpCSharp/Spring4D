unit ChainsawFiler;

{
  Interface to XML files in Log4D format (see log4d.dtd).

  Written by Keith Wood (kbwood@iprimus.com.au)
  Version 1.0 - 19 September 2003.
}

interface

uses
  Classes, SysUtils, DBClient, Log4D, Log4DXML, ComObj, ActiveX, MSXML2_TLB;

{ Load a file in standard XML logging format into the dataset. }
procedure LoadFromFile(const DataSet: TCustomClientDataSet;
  const FileName: string);

{ Save the contents of the dataset to a file in standard XML logging format. }
procedure SaveToFile(const DataSet: TCustomClientDataSet;
  const FileName: string);

implementation

resourcestring
  MessageFormat = '%s during load: %s at %d,%d in %s';

type
  { SAX handler for Log4D XML files. }
  TLogFileHandler = class(TObject, IVBSAXContentHandler, IVBSAXErrorHandler)
  private
    FCurTag: string;
    FDataSet: TCustomClientDataSet;
    FElapsedTimeIndex: Integer;
    FErrorClassIndex: Integer;
    FErrorMsgIndex: Integer;
    FLevelNameIndex: Integer;
    FLevelValueIndex: Integer;
    FLoggerIndex: Integer;
    FMessageIndex: Integer;
    FNDCIndex: Integer;
    FThreadIdIndex: Integer;
    FTimestampIndex: Integer;
    FValue: WideString;
  public
    constructor Create(const DataSet: TCustomClientDataSet);
    { IVBSAXContentHandler }
    procedure _Set_documentLocator(const Param1: IVBSAXLocator); safecall;
    procedure startDocument; safecall;
    procedure endDocument; safecall;
    procedure startPrefixMapping(var strPrefix: WideString;
      var strURI: WideString); safecall;
    procedure endPrefixMapping(var strPrefix: WideString); safecall;
    procedure startElement(var strNamespaceURI: WideString;
      var strLocalName: WideString; var strQName: WideString;
      const oAttributes: IVBSAXAttributes); safecall;
    procedure endElement(var strNamespaceURI: WideString;
      var strLocalName: WideString; var strQName: WideString); safecall;
    procedure characters(var strChars: WideString); safecall;
    procedure ignorableWhitespace(var strChars: WideString); safecall;
    procedure processingInstruction(var strTarget: WideString;
      var strData: WideString); safecall;
    procedure skippedEntity(var strName: WideString); safecall;
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

{ TLogFileHandler -------------------------------------------------------------}

{ Parse logging files in XML format and add details to dataset.
  Sample XML:
  <?xml version="1.0"?>
  <!DOCTYPE log4d:eventSet SYSTEM "log4d.dtd">
  <log4d:eventSet version="1.2"
      xmlns:log4d="http://log4d.sourceforge.net/log4d">
  <log4d:event logger="myapp.other" levelName="FATAL" levelValue="50000"
      thread="1496" timestamp="20030919200503" elapsed="17685">
    <log4d:message><![CDATA[Error in calculation]]></log4d:message>
    <log4d:NDC><![CDATA[abc|def]]></log4d:NDC>
    <log4d:exception class="EDivByZero">
      <![CDATA[Floating point division by zero]]></log4d:exception>
  </log4d:event>
  </log4d:eventSet>
}
constructor TLogFileHandler.Create(const DataSet: TCustomClientDataSet);
begin
  inherited Create;
  FDataSet := DataSet;
end;

{ IVBSAXContentHandler --------------------------------------------------------}

procedure TLogFileHandler._Set_documentLocator(const Param1: IVBSAXLocator);
begin
  // Do nothing
end;

{ Save text content for later use. }
procedure TLogFileHandler.characters(var strChars: WideString);
begin
  FValue := FValue + strChars;
end;

procedure TLogFileHandler.endDocument;
begin
  // Do nothing
end;

{ Add accumulated text to appropriate field. }
procedure TLogFileHandler.endElement(var strNamespaceURI, strLocalName,
  strQName: WideString);
begin
  if strQName = EventTag then
    FDataSet.Post
  else if strQName = ExceptionTag then
    FDataSet.Fields[FErrorMsgIndex].AsString := FValue
  else if strQName = MessageTag then
    FDataSet.Fields[FErrorMsgIndex].AsString := FValue
  else if strQName = NDCTag then
    FDataSet.Fields[FNDCIndex].AsString      := FValue;
  FValue := '';
end;

procedure TLogFileHandler.endPrefixMapping(var strPrefix: WideString);
begin
  // Do nothing
end;

procedure TLogFileHandler.ignorableWhitespace(var strChars: WideString);
begin
  // Do nothing
end;

procedure TLogFileHandler.processingInstruction(var strTarget,
  strData: WideString);
begin
  // Do nothing
end;

procedure TLogFileHandler.skippedEntity(var strName: WideString);
begin
  // Do nothing
end;

{ Initialisation. }
procedure TLogFileHandler.startDocument;
begin
  FDataSet.EmptyDataSet;
  FValue            :='';
  // Locate fields once only
  FElapsedTimeIndex := FDataSet.FieldByName('ElapsedTime').Index;
  FErrorClassIndex  := FDataSet.FieldByName('ErrorClass').Index;
  FErrorMsgIndex    := FDataSet.FieldByName('ErrorMessage').Index;
  FLevelNameIndex   := FDataSet.FieldByName('LevelName').Index;
  FLevelValueIndex  := FDataSet.FieldByName('LevelValue').Index;
  FLoggerIndex      := FDataSet.FieldByName('LoggerName').Index;
  FMessageIndex     := FDataSet.FieldByName('Message').Index;
  FNDCIndex         := FDataSet.FieldByName('NDC').Index;
  FThreadIdIndex    := FDataSet.FieldByName('ThreadId').Index;
  FTimestampIndex   := FDataSet.FieldByName('Timestamp').Index;
end;

{ Create a record and/or load values from attributes. }
procedure TLogFileHandler.startElement(var strNamespaceURI, strLocalName,
  strQName: WideString; const oAttributes: IVBSAXAttributes);
begin
  FCurTag := strQName;
  if strQName = EventTag then
  begin
    FDataSet.Append;
    FDataSet.Fields[FLoggerIndex].AsString      :=
      oAttributes.getValueFromQName(LoggerAttr);
    FDataSet.Fields[FLevelNameIndex].AsString   :=
      oAttributes.getValueFromQName(LevelNameAttr);
    FDataSet.Fields[FLevelValueIndex].AsString  :=
      oAttributes.getValueFromQName(LevelValueAttr);
    FDataSet.Fields[FThreadIdIndex].AsString    :=
      oAttributes.getValueFromQName(ThreadAttr);
    FDataSet.Fields[FTimestampIndex].AsString   :=
      oAttributes.getValueFromQName(TimestampAttr);
    FDataSet.Fields[FElapsedTimeIndex].AsString :=
      oAttributes.getValueFromQName(ElapsedAttr);
  end
  else if strQName = ExceptionTag then
    FDataSet.Fields[FErrorClassIndex].AsString :=
      oAttributes.getValueFromQName(ClassAttr);
end;

procedure TLogFileHandler.startPrefixMapping(var strPrefix,
  strURI: WideString);
begin
  // Do nothing
end;

{ IVBSAXErrorHandler ----------------------------------------------------------}

{ Abort the load process. }
procedure TLogFileHandler.Error(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  raise ELogException.Create(Format(MessageFormat, ['Error', strErrorMessage,
    oLocator.LineNumber, oLocator.ColumnNumber, oLocator.SystemId]));
end;

{ Abort the load process. }
procedure TLogFileHandler.FatalError(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  raise ELogException.Create(Format(MessageFormat,
    ['Fatal error', strErrorMessage, oLocator.LineNumber,
    oLocator.ColumnNumber, oLocator.SystemId]));
end;

{ Ignore warnings. }
procedure TLogFileHandler.IgnorableWarning(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  // Do nothing
end;

{ IDispatch -------------------------------------------------------------------}

{ These functions are required by the IDispatch interface but are not used. }
function TLogFileHandler.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLogFileHandler.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo):
  HResult;
begin
  Result := E_NOTIMPL;
end;

function TLogFileHandler.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLogFileHandler.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word;
  var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ IUnknown --------------------------------------------------------------------}

{ These functions are required by the IUnknown interface but are not used. }
function TLogFileHandler.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

function TLogFileHandler._AddRef: Integer;
begin
  Result := 1;
end;

function TLogFileHandler._Release: Integer;
begin
  Result := 1;
end;

{ External routines -----------------------------------------------------------}

{ Load a file in standard XML logging format into the dataset. }
procedure LoadFromFile(const DataSet: TCustomClientDataSet;
  const FileName: string);
var
  Handler: TLogFileHandler;
  Reader: IVBSAXXMLReader;
begin
  Handler := TLogFileHandler.Create(DataSet);
  try
    Reader                := CoSAXXMLReader.Create;
    Reader.ContentHandler := Handler;
    Reader.ErrorHandler   := Handler;
    Reader.ParseURL(FileName);
  finally
    Handler.Free;
  end;
end;

{ Save the contents of the dataset to a file in standard XML logging format. }
procedure SaveToFile(const DataSet: TCustomClientDataSet;
  const FileName: string);
var
  Layout: TLogXMLLayout;
  Contents: TStringList;
  ElapsedTimeIndex, ErrorClassIndex, ErrorMsgIndex, LevelNameIndex,
  LevelValueIndex, LoggerIndex, MessageIndex, NDCIndex,
  ThreadIdIndex, TimestampIndex: Integer;
begin
  // Locate fields once only
  ElapsedTimeIndex := DataSet.FieldByName('ElapsedTime').Index;
  ErrorClassIndex  := DataSet.FieldByName('ErrorClass').Index;
  ErrorMsgIndex    := DataSet.FieldByName('ErrorMessage').Index;
  LevelNameIndex   := DataSet.FieldByName('LevelName').Index;
  LevelValueIndex  := DataSet.FieldByName('LevelValue').Index;
  LoggerIndex      := DataSet.FieldByName('LoggerName').Index;
  MessageIndex     := DataSet.FieldByName('Message').Index;
  NDCIndex         := DataSet.FieldByName('NDC').Index;
  ThreadIdIndex    := DataSet.FieldByName('ThreadId').Index;
  TimestampIndex   := DataSet.FieldByName('Timestamp').Index;
  // Reuse the standard XML layout
  Layout := TLogXMLLayout.Create;
  try
    Contents := TStringList.Create;
    try
      Contents.Append(Layout.Header);
      // Process each record through the layout
      with DataSet do
      begin
        First;
        while not EOF do
        begin
          Contents.Append(Layout.Format(Fields[LoggerIndex].AsString,
            Fields[LevelNameIndex].AsString, Fields[LevelValueIndex].AsInteger,
            Fields[ThreadIdIndex].AsInteger, Fields[TimestampIndex].AsDateTime,
            Fields[ElapsedTimeIndex].AsInteger, Fields[MessageIndex].AsString,
            Fields[NDCIndex].AsString, Fields[ErrorMsgIndex].AsString,
            Fields[ErrorClassIndex].AsString));
          Next;
        end;
      end;
      Contents.Append(Layout.Footer);
      // And write out the results
      Contents.SaveToFile(FileName);
    finally
      Contents.Free;
    end;
  finally
    Layout.Free;
  end;
end;

initialization
  CoInitialize(nil);
finalization
  CoUninitialize;
end.
