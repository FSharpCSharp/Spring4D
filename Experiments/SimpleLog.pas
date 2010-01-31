{*******************************************************}
{                                                       }
{       SimpleLog                                       }
{                                                       }
{       Copyright (C) 2008 Zuo Baoquan                  }
{                                                       }
{*******************************************************}

unit SimpleLog;

interface

uses
  Classes, SysUtils, RDSystem;

type
  { Log Level }
  TLogLevel = (
    llAll,    // 所有
    llDebug,  // 调试
    llInfo,   // 信息
    llWarn,   // 警告
    llError,  // 错误
    llFatal,  // 致命错误
    llOff     // 关闭
  );

  { 日志接口 }
  ILogger = interface
    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const msg: string; const args: array of const);
    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const msg: string; const args: array of const);
    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const msg: string; const args: array of const);
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const msg: string; const args: array of const);
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const msg: string; const args: array of const);
    {$REGION 'Getters'}
      function GetIsDebugEnabled: Boolean;
      function GetIsErrorEnabled: Boolean;
      function GetIsFatalEnabled: Boolean;
      function GetIsInfoEnabled: Boolean;
      function GetIsWarnEnabled: Boolean;
    {$ENDREGION}
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
  end;

  ITextWriter = interface
    procedure Write(const buffer: string);
    procedure WriteLine(const buffer: string);
  end;

  TLoggingEvent = class
  
  end;

  { Logging to a local file }
  TFileStreamWriter = class(TInterfacedObject, ITextWriter)
  private
    fStream: TStream;
    fFileName: string;
    {$IFDEF DELPHI2009_UP}
    {$ENDIF ~DELPHI2009_UP}
    fEncoding: TEncoding;
  protected
    procedure CreateStream; virtual;
    procedure DoWrite(const buffer: string); virtual;
  public
    constructor Create(const fileName: string; encoding: TEncoding);
    destructor Destroy; override;
    procedure Write(const buffer: string);
    procedure WriteLine(const buffer: string);
  end;

//  TSyncLogger = class(TInterfacedObject, ILogger)
//
//  end;

  { A logger logs to an appender in a particular layout (style). }
  { TLogger }
  TLogger = class(TIntfObject, ILogger, ISyncObject)
  private
//    fParent: TLogger;
    fName: string;
    fLogLevel: TLogLevel;
    fWriter: ITextWriter;
    fSyncObject: ISyncObject;
    function GetIsDebugEnabled: Boolean;
    function GetIsInfoEnabled: Boolean;
    function GetIsWarnEnabled: Boolean;
    function GetIsErrorEnabled: Boolean;
    function GetIsFatalEnabled: Boolean;
    function GetLogLevel: TLogLevel;
    function GetWriter: ITextWriter;
    procedure SetLogLevel(const Value: TLogLevel);
    procedure SetWriter(const Value: ITextWriter);
  protected
    function Support(const level: TLogLevel): Boolean;
    procedure WriteLog(const level: TLogLevel; const msg: string;
      const args: array of const); virtual;
    property LogLevel: TLogLevel read GetLogLevel write SetLogLevel;
    property Writer: ITextWriter read GetWriter write SetWriter;
    property SyncObject: ISyncObject read fSyncObject implements ISyncObject;
  public
    constructor Create(const name: string);
    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const msg: string; const args: array of const);
    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const msg: string; const args: array of const);
    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const msg: string; const args: array of const);
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const msg: string; const args: array of const);
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const msg: string; const args: array of const);
    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  { Root logger, sealed, Alwasy DEBUG }
  TRootLogger = class sealed(TLogger)
  public
    constructor Create;
  end;

  { An appender defines the properties of the logging target to log4D }
  TAppender = class

  end;

  { TLogPattern, place holder }
  TLogPattern = class
  public
    class function Generate(const level: TLogLevel; const msg: string;
      const args: array of const): string;
  end;

  { TLoggerManager }
  TLoggerManager = class(TObject)
  private
    fRoot: TLogger;             { Root Logger }
    fLogFileName: string;       { Root Logger FileName }
    fLogWriter: ITextWriter;    { Root Logger Writer }
    fSyncObject: ISyncObject;   { Sync Object }
    fErrors: TStrings;          { Internal Exceptions }
    fOnException: TNotifyEvent; { OnException Event }
    fEncoding: TEncoding;
    function GetRoot: ILogger;
    function GetLogLevel: TLogLevel;
    procedure SetLogLevel(const Value: TLogLevel);
    procedure SetLogFileName(const value: string);
    procedure SetEncoding(const Value: TEncoding);
  protected
    procedure HandleException(e: Exception; const msg: string);
  public
    constructor Create;
    destructor Destroy; override;
//    class function GetLogger(const name: string): ILog; overload;
//    class function GetSyncLogger(const name: string): ILog; overload;
    property Root: ILogger read GetRoot;
    property FileName: string read fLogFileName write SetLogFileName;
    property Encoding: TEncoding read fEncoding write SetEncoding;
    property LogLevel: TLogLevel read GetLogLevel write SetLogLevel;
    property Errors: TStrings read fErrors;
    property OnException: TNotifyEvent read fOnException write fOnException;
  end;

resourcestring
  { Log Levels }
  SAllDescription   = 'ALL';   // '所有';
  SDebugDescription = 'DEBUG'; // '调试';
  SInfoDescription  = 'INFO';  // '信息';
  SWarnDescription  = 'WARN';  // '警告';
  SErrorDescription = 'ERROR'; // '错误';
  SFatalDescription = 'FATAL'; // '致命错误';
  SOffDescription   = 'OFF';   // '关闭';

  SMakeDirectoryError = 'Failed to make the directory：%s.';

const
  LogLevelStrings: array[TLogLevel] of string = (
    SAllDescription,
    SDebugDescription,
    SInfoDescription,
    SWarnDescription,
    SErrorDescription,
    SFatalDescription,
    SOffDescription
  );

var
  LogManager: TLoggerManager;    { Global LogManager  }

{ Global Root Logger }
function Logger: ILogger;

implementation

uses
  TypInfo;

function Logger: ILogger;
begin
  Result := LogManager.Root;
end;


{$REGION 'TLogger'}

constructor TLogger.Create(const name: string);
begin
  inherited Create;
  fName := name;
  fSyncObject := TSyncObject.Create;
end;

function TLogger.Support(const level: TLogLevel): Boolean;
begin
  Result := level >= LogLevel;
end;

procedure TLogger.WriteLog(const level: TLogLevel; const msg: string;
  const args: array of const);
var
  context: string;
begin
  fSyncObject.Lock;
  if Support(level) and (fWriter <> nil) then
  begin
    context := TLogPattern.Generate(level, msg, args);
    fWriter.WriteLine(context);
  end;
end;

procedure TLogger.Info(const msg: string);
begin
  InfoFormat(msg, []);
end;

procedure TLogger.Info(const msg: string; e: Exception);
begin
  InfoFormat(msg, []);  { TEMP }
end;

procedure TLogger.InfoFormat(const msg: string;
  const args: array of const);
begin
  WriteLog(llInfo, msg, args);
end;

procedure TLogger.Debug(const msg: string);
begin
  DebugFormat(msg, []);
end;

procedure TLogger.Debug(const msg: string; e: Exception);
begin
  DebugFormat(msg, []);  { TEMP }
end;

procedure TLogger.DebugFormat(const msg: string;
  const args: array of const);
begin
  WriteLog(llDebug, msg, args);
end;

procedure TLogger.Warn(const msg: string);
begin
  WarnFormat(msg, []);
end;

procedure TLogger.Warn(const msg: string; e: Exception);
begin
  WarnFormat(msg, []);  { TEMP }
end;

procedure TLogger.WarnFormat(const msg: string;
  const args: array of const);
begin
  WriteLog(llWarn, msg, args);
end;

procedure TLogger.Error(const msg: string);
begin
  ErrorFormat(msg, []);
end;

procedure TLogger.Error(const msg: string; e: Exception);
begin
  ErrorFormat(msg, []);  { TEMP }
end;

procedure TLogger.ErrorFormat(const msg: string;
  const args: array of const);
begin
  WriteLog(llError, msg, args);
end;

procedure TLogger.Fatal(const msg: string);
begin
  FatalFormat(msg, []);
end;

procedure TLogger.Fatal(const msg: string; e: Exception);
begin
  FatalFormat(msg, []); { TEMP }
end;

procedure TLogger.FatalFormat(const msg: string;
  const args: array of const);
begin
  WriteLog(llFatal, msg, args);
end;

function TLogger.GetIsDebugEnabled: Boolean;
begin
  Result := Support(llDebug);
end;

function TLogger.GetIsErrorEnabled: Boolean;
begin
  Result := Support(llInfo);
end;

function TLogger.GetIsFatalEnabled: Boolean;
begin
  Result := Support(llFatal);
end;

function TLogger.GetIsInfoEnabled: Boolean;
begin
  Result := Support(llInfo);
end;

function TLogger.GetIsWarnEnabled: Boolean;
begin
  Result := Support(llWarn);
end;

function TLogger.GetLogLevel: TLogLevel;
begin
  SyncObject.Lock;
  Result := fLogLevel;
end;

function TLogger.GetWriter: ITextWriter;
begin
  SyncObject.Lock;
  Result := fWriter;
end;

procedure TLogger.SetLogLevel(const Value: TLogLevel);
begin
  SyncObject.Lock;
  fLogLevel := value;
end;

procedure TLogger.SetWriter(const Value: ITextWriter);
begin
  SyncObject.Lock;
  fWriter := value;
end;

{$ENDREGION}


{$REGION 'TRootLogger'}

constructor TRootLogger.Create;
begin
  inherited Create('Root');
end;

{$ENDREGION}


{$REGION 'TFileStreamWriter'}

constructor TFileStreamWriter.Create(const fileName: string;
  encoding: TEncoding);
begin
  inherited Create;
  fFileName := fileName;
  fEncoding := encoding;
end;

destructor TFileStreamWriter.Destroy;
begin
  fStream.Free;
  inherited Destroy;
end;

procedure TFileStreamWriter.DoWrite(const buffer: string);
var
  bytes: TBytes;
begin
  bytes := fEncoding.GetBytes(buffer);
  fStream.Write(PByte(bytes)^, Length(bytes));
end;

procedure TFileStreamWriter.CreateStream;
var
  dir: string;
  mode: Word;
begin
  mode := fmOpenReadWrite or fmShareDenyWrite;
  if not FileExists(fFileName) then
  begin
    dir := ExtractFileDir(fFileName);
    if ForceDirectories(dir) then
    begin
      mode := fmCreate or fmShareDenyWrite;
    end
    else
    begin
      raise Exception.CreateFmt(SMakeDirectoryError, [dir]);
    end;
  end;
  fStream := TFileStream.Create(fFileName, mode);
end;

procedure TFileStreamWriter.Write(const buffer: string);
var
  bytes: TBytes;
begin
  if fStream = nil then
  begin
    CreateStream;
    if fStream.Size = 0 then
    begin
      bytes := fEncoding.GetPreamble;
      fStream.Write(PByte(bytes)^, Length(bytes));
    end;
    fStream.Seek(0, soFromEnd);
  end;
  DoWrite(buffer);
end;

procedure TFileStreamWriter.WriteLine(const buffer: string);
begin
  Write(buffer + Environment.NewLine);
end;

{$ENDREGION}


{$REGION 'TLoggerManager'}

constructor TLoggerManager.Create;
begin
  inherited Create;
  fErrors := TStringList.Create;
  fSyncObject := TSyncObject.Create;
  fRoot := TLogger.Create('Root');
  fEncoding := TEncoding.UTF8;
end;

destructor TLoggerManager.Destroy;
begin
  fErrors.Free;
  fRoot.Free;
  inherited Destroy;
end;

function TLoggerManager.GetRoot: ILogger;
begin
  Result := fRoot;
end;

procedure TLoggerManager.HandleException(e: Exception; const msg: string);
begin
  fSyncObject.Lock;
  fErrors.Add(msg);
  fErrors.Add(Format('TLogManager.WriteLog raised an exception: %0:s, %1:s.',
    [e.ClassName, e.Message]));
  if Assigned(fOnException) then
  begin
    fOnException(Self);
  end;  
end;

function TLoggerManager.GetLogLevel: TLogLevel;
begin
  Result := fRoot.LogLevel;
end;

procedure TLoggerManager.SetLogLevel(const Value: TLogLevel);
begin
  fRoot.LogLevel := Value;
end;

procedure TLoggerManager.SetEncoding(const Value: TEncoding);
begin
  fSyncObject.Lock;
  fEncoding := Value;
  if fLogWriter <> nil then
  begin
    fLogWriter := TFileStreamWriter.Create(fLogFileName, fEncoding);
    fRoot.Writer := fLogWriter;
  end;
end;

procedure TLoggerManager.SetLogFileName(const value: string);
begin
  fSyncObject.Lock;
  if Pos(PathDelim, value) > 0 then
  begin
    fLogFileName := value;
  end
  else
  begin
    fLogFileName := Environment.ApplicationPath + value;
  end;
  fLogWriter := TFileStreamWriter.Create(fLogFileName, fEncoding);
  fRoot.Writer := fLogWriter;
end;

{$ENDREGION}


{$REGION 'TLogLayout'}

class function TLogPattern.Generate(const level: TLogLevel; const msg: string;
  const args: array of const): string;
const
  SDateTimePattern = 'yyyy-mm-dd'#9'hh:nn:sss.zzz';
  SLogPattern = '%0:s'#9'%1:s'#9'%2:s';
begin
  Result := Format(msg, args);
  if Result <> '' then
  begin
    Result := Format(SLogPattern, [
      FormatDateTime(SDateTimePattern, Now),
      LogLevelStrings[level],
      Result
    ]);
  end;
end;

{$ENDREGION}

initialization
  LogManager := TLoggerManager.Create;

finalization
  FreeAndNil(LogManager);

end.
