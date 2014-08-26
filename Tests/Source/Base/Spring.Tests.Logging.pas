{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
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

unit Spring.Tests.Logging;

{$I Spring.Tests.inc}

interface

uses
  Classes,
  Rtti,
  StrUtils,
  SysUtils,
  TypInfo,
  TestFramework,
  Spring,
  Spring.Collections,
  Spring.Reflection,
  Spring.Logging,
  Spring.Logging.Appenders,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Controller,
  Spring.Logging.Extensions,
  Spring.Logging.Loggers,
  Spring.Tests.Logging.Types;

type
  {$REGION 'TTestLoggerController'}
  TTestLoggerController = class(TTestCase)
  private
    fController: ILoggerController;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAppender;
    procedure TestSend;
    procedure TestSendDisabled;
    procedure TestAddSerializer;
    procedure TestFindSerializer;
    procedure TestSendWithSerializer;
  end;
  {$ENDREGION}


  {$REGION 'TTestLogger'}
  TTestLogger = class(TTestCase)
  private
    fController: ILoggerController;
    fLogger: ILogger;
    fException: Exception;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckEvent(enabled: Boolean; level: TLogLevel; const msg: string;
      const e: Exception = nil);
    procedure TestLogEntry(enabled: Boolean); overload;
    procedure TestLog(enabled: Boolean); overload;
    procedure TestFatal(enabled: Boolean); overload;
    procedure TestError(enabled: Boolean); overload;
    procedure TestWarn(enabled: Boolean); overload;
    procedure TestInfo(enabled: Boolean); overload;
    procedure TestText(enabled: Boolean); overload;
    procedure TestDebug(enabled: Boolean); overload;
    procedure TestVerbose(enabled: Boolean); overload;
  published
    procedure TestCreate;
    procedure TestLevels;
    procedure TestEnabled;
    procedure TestIsEnabled;

    procedure TestIsFatalEnabled;
    procedure TestIsErrorEnabled;
    procedure TestIsWarnEnabled;
    procedure TestIsInfoEnabled;
    procedure TestIsTextEnabled;
    procedure TestIsDebugEnabled;
    procedure TestIsVerboseEnabled;

    procedure TestLoggerProperties;

    procedure TestLogEntry; overload;

    procedure TestLog; overload;
    procedure TestFatal; overload;
    procedure TestError; overload;
    procedure TestWarn; overload;
    procedure TestInfo; overload;
    procedure TestText; overload;
    procedure TestDebug; overload;
    procedure TestVerbose; overload;

    procedure TestEntering;
    procedure TestLeaving;
    procedure TestTrack;
  end;
  {$ENDREGION}


  {$REGION 'TAppenderTestCase'}
  TAppenderTestCase = class abstract(TTestCase)
  protected
    fController: ILoggerController;
    fLogger: ILogger;
    fAppender: ILogAppender;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;
  {$ENDREGION}


  {$REGION 'TTestLogAppenderBase'}
  TTestLogAppenderBase = class(TTestCase)
  published
    procedure TestFormatMethodName;
    procedure TestFormatText;
    procedure TestFormatMsg;
  end;
  {$ENDREGION}


  {$REGION 'TTestStreamLogAppender'}
  TTestStreamLogAppender = class(TAppenderTestCase)
  private
    fStream: TStringStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestException;
  end;
  {$ENDREGION}


implementation


{$REGION 'Internal test classes and helpers'}
type
  TTestLoggerHelper = class helper for TTestLogger
  private
    function GetController: TLoggerControllerMock;
    function GetLogger: TLogger;
  public
    property Controller: TLoggerControllerMock read GetController;
    property Logger: TLogger read GetLogger;
  end;

{ TTestLoggerHelper }

function TTestLoggerHelper.GetController: TLoggerControllerMock;
begin
  Result := TObject(fController) as TLoggerControllerMock;
end;

function TTestLoggerHelper.GetLogger: TLogger;
begin
  Result := TObject(fLogger) as TLogger;
end;
{$ENDREGION}


{$REGION 'TTestLoggerController'}

procedure TTestLoggerController.SetUp;
begin
  inherited;
  fController := TLoggerController.Create;
end;

procedure TTestLoggerController.TearDown;
begin
  fController := nil;
  inherited;
end;

procedure TTestLoggerController.TestAddAppender;
var
  appender: TAppenderMock;
  intf: ILogAppender;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  appender := TAppenderMock.Create;
  intf := appender;

  fController.AddAppender(intf);

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(fController)).AsType<IList<ILogAppender>>;

  CheckEquals(1, appenders.Count);
  CheckSame(intf, appenders[0]);
end;

procedure TTestLoggerController.TestAddSerializer;
var
  serializer: TTypeSerializerMock;
  intf: ITypeSerializer;
  f: TRttiField;
  serializers: IList<ITypeSerializer>;
begin
  serializer := TTypeSerializerMock.Create;
  intf := serializer;

  (fController as ISerializerController).AddSerializer(intf);

  f := TType.GetType<TLoggerController>.GetField('fSerializers');
  serializers := f.GetValue(TObject(fController)).AsType<IList<ITypeSerializer>>;

  CheckEquals(1, serializers.Count);
  CheckSame(intf, serializers[0]);
end;

procedure TTestLoggerController.TestFindSerializer;
var
  serializer: TTypeSerializerMock;
  intf: ITypeSerializer;
  controller: ISerializerController;
begin
  serializer := TTypeSerializerMock.Create;
  intf := serializer;
  controller := (fController as ISerializerController);
  controller.AddSerializer(intf);

  CheckSame(intf, controller.FindSerializer(TypeInfo(Integer)));
  CheckNull(controller.FindSerializer(TypeInfo(Double)));
end;

procedure TTestLoggerController.TestSend;
const
  MSG = 'test';
var
  appender: TAppenderMock;
  intf: ILogAppender;
begin
  appender := TAppenderMock.Create;
  intf := appender;

  fController.AddAppender(intf);
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, MSG));

  CheckTrue(appender.WriteCalled);
  CheckEquals(Ord(TLogLevel.Fatal), Ord(appender.Entry.Level));
  CheckEquals(MSG, appender.Entry.Msg);
end;

procedure TTestLoggerController.TestSendDisabled;
const
  MSG = 'test';
var
  appender: TAppenderMock;
  intf: ILogAppender;
begin
  appender := TAppenderMock.Create;
  intf := appender;
  fController.AddAppender(intf);

  TLoggerController(fController).Enabled := False;
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, MSG));
  CheckFalse(appender.WriteCalled);

  TLoggerController(fController).Enabled := True;
  TLoggerController(fController).Levels := [];
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, MSG));
  CheckFalse(appender.WriteCalled);

  TLoggerController(fController).Levels := LOG_ALL_LEVELS - [TLogLevel.Fatal];
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, MSG));
  CheckFalse(appender.WriteCalled);
end;

procedure TTestLoggerController.TestSendWithSerializer;
var
  appender: TAppenderMock;
  intf: ILogAppender;
  serializer: TTypeSerializerMock;
begin
  serializer := TTypeSerializerMock.Create;
  appender := TAppenderMock.Create;
  intf := appender;

  fController.AddAppender(intf);
  (fController as ISerializerController).AddSerializer(serializer);

  //Check that we only output the message and not the data if the appender
  //does not habe SerializedData level
  appender.Levels := [TLogLevel.Fatal];
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, TLogEntryType.Text, 'test',
    nil, -1));
  CheckEquals(1, appender.WriteCount);
  CheckEquals(0, serializer.HandlesTypeCount);
  CheckEquals(Ord(TLogLevel.Fatal), Ord(appender.Entry.Level));

  //... or is disabled
  appender.Enabled := False;
  appender.Levels := [TLogLevel.Fatal, TLogLevel.SerializedData];
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, TLogEntryType.Text, 'test',
    nil, -1));
  CheckEquals(1, appender.WriteCount);
  CheckEquals(0, serializer.HandlesTypeCount);

  //Finally test that we dispatch the call and both messages are logged
  appender.Enabled := True;
  fController.Send(TLogEntry.Create(TLogLevel.Fatal, TLogEntryType.Text, 'test',
    nil, -1));
  CheckEquals(3, appender.WriteCount);
  CheckEquals(1, serializer.HandlesTypeCount);
  CheckEquals(Ord(TLogLevel.SerializedData), Ord(appender.Entry.Level));
end;

{$ENDREGION}


{$REGION 'TTestLogger'}

procedure TTestLogger.CheckEvent(enabled: Boolean; level: TLogLevel;
  const msg: string; const e: Exception = nil);
begin
  CheckEquals(enabled, Controller.LastEntry.Level <> TLogLevel.Unknown);
  if (not enabled) then
    Exit;

  CheckEquals(Ord(level), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Text), Ord(Controller.LastEntry.EntryType));
  CheckEquals(msg, Controller.LastEntry.Msg);
  CheckSame(e, Controller.LastEntry.Exception);
end;

procedure TTestLogger.SetUp;
var logger: TLogger;
begin
  inherited;
  fController := TLoggerControllerMock.Create;
  logger := TLogger.Create(fController);
  logger.Levels := [TLogLevel.Fatal];
  fLogger := logger;
  fException := ENotSupportedException.Create('');
end;

procedure TTestLogger.TearDown;
begin
  inherited;
  fLogger := nil;
  fController := nil;
  FreeAndNil(fException);
end;

procedure TTestLogger.TestCreate;
var logger: TLogger;
begin
  logger := TLogger.Create(fController);
  try
    CheckTrue(logger.Enabled);
    Check(LOG_BASIC_LEVELS = logger.Levels, 'Levels');
  finally
    logger.Free;
  end;
end;

procedure TTestLogger.TestDebug;
begin
  Logger.Levels := [TLogLevel.Debug];
  TestDebug(True);
  Logger.Levels := [];
  TestDebug(False);
end;

procedure TTestLogger.TestDebug(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Debug('D1');
  CheckEvent(enabled, TLogLevel.Debug, 'D1');

  Controller.Reset;
  fLogger.Debug('D2', fException);
  CheckEvent(enabled, TLogLevel.Debug, 'D2', fException);

  Controller.Reset;
  fLogger.Debug('D%d', [3]);
  CheckEvent(enabled, TLogLevel.Debug, 'D3');

  Controller.Reset;
  fLogger.Debug('D%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Debug, 'D4', fException);
end;

procedure TTestLogger.TestEnabled;
begin
  Logger.Enabled := False;
  CheckFalse(Logger.Enabled);

  Logger.Enabled := True;
  CheckTrue(Logger.Enabled);
end;

procedure TTestLogger.TestEntering;
begin
  Controller.Reset;

  Logger.Enabled := False;
  fLogger.Entering(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warning];
  fLogger.Entering(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  fLogger.Entering(TLogLevel.Warning, nil, '');
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Entering),  Ord(Controller.LastEntry.EntryType));

  Controller.Reset;

  Logger.Enabled := False;
  fLogger.Entering(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warning];
  fLogger.Entering(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  fLogger.Entering(TLogLevel.Warning, nil, '', ['value']);
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Entering),  Ord(Controller.LastEntry.EntryType));
  CheckEquals('value', Controller.LastEntry.Data.AsType<TArray<TValue>>[0].AsString);
end;

procedure TTestLogger.TestError(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Error('E1');
  CheckEvent(enabled, TLogLevel.Error, 'E1');

  Controller.Reset;
  fLogger.Error('E2', fException);
  CheckEvent(enabled, TLogLevel.Error, 'E2', fException);

  Controller.Reset;
  fLogger.Error('E%d', [3]);
  CheckEvent(enabled, TLogLevel.Error, 'E3');

  Controller.Reset;
  fLogger.Error('E%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Error, 'E4', fException);
end;

procedure TTestLogger.TestError;
begin
  Logger.Levels := [TLogLevel.Error];
  TestError(True);
  Logger.Levels := [];
  TestError(False);
end;

procedure TTestLogger.TestFatal(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Fatal('F1');
  CheckEvent(enabled, TLogLevel.Fatal, 'F1');

  Controller.Reset;
  fLogger.Fatal('F2', fException);
  CheckEvent(enabled, TLogLevel.Fatal, 'F2', fException);

  Controller.Reset;
  fLogger.Fatal('F%d', [3]);
  CheckEvent(enabled, TLogLevel.Fatal, 'F3');

  Controller.Reset;
  fLogger.Fatal('F%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Fatal, 'F4', fException);
end;

procedure TTestLogger.TestFatal;
begin
  Logger.Levels := [TLogLevel.Fatal];
  TestFatal(True);
  Logger.Levels := [];
  TestFatal(False);
end;

procedure TTestLogger.TestInfo(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Info('I1');
  CheckEvent(enabled, TLogLevel.Info, 'I1');

  Controller.Reset;
  fLogger.Info('I2', fException);
  CheckEvent(enabled, TLogLevel.Info, 'I2', fException);

  Controller.Reset;
  fLogger.Info('I%d', [3]);
  CheckEvent(enabled, TLogLevel.Info, 'I3');

  Controller.Reset;
  fLogger.Info('I%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Info, 'I4', fException);
end;

procedure TTestLogger.TestInfo;
begin
  Logger.Levels := [TLogLevel.Info];
  TestInfo(True);
  Logger.Levels := [];
  TestInfo(False);
end;

procedure TTestLogger.TestIsDebugEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Debug];
  CheckFalse(fLogger.IsDebugEnabled);

  Logger.Levels := [TLogLevel.Debug];
  CheckTrue(fLogger.IsDebugEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsDebugEnabled);
end;

procedure TTestLogger.TestIsEnabled;
var l: TLogLevel;
begin
  for l in LOG_ALL_LEVELS do
    CheckEquals(l in Logger.Levels, Logger.IsEnabled(l));
  Logger.Enabled := False;
  for l in LOG_ALL_LEVELS do
    CheckFalse(Logger.IsEnabled(l));
end;

procedure TTestLogger.TestIsErrorEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Error];
  CheckFalse(fLogger.IsErrorEnabled);

  Logger.Levels := [TLogLevel.Error];
  CheckTrue(fLogger.IsErrorEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsErrorEnabled);
end;

procedure TTestLogger.TestIsFatalEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Fatal];
  CheckFalse(fLogger.IsFatalEnabled);

  Logger.Levels := [TLogLevel.Fatal];
  CheckTrue(fLogger.IsFatalEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsFatalEnabled);
end;

procedure TTestLogger.TestIsInfoEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Info];
  CheckFalse(fLogger.IsInfoEnabled);

  Logger.Levels := [TLogLevel.Info];
  CheckTrue(fLogger.IsInfoEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsInfoEnabled);
end;

procedure TTestLogger.TestIsTextEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Text];
  CheckFalse(fLogger.IsTextEnabled);

  Logger.Levels := [TLogLevel.Text];
  CheckTrue(fLogger.IsTextEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsTextEnabled);
end;

procedure TTestLogger.TestIsVerboseEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Verbose];
  CheckFalse(fLogger.IsVerboseEnabled);

  Logger.Levels := [TLogLevel.Verbose];
  CheckTrue(fLogger.IsVerboseEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsVerboseEnabled);
end;

procedure TTestLogger.TestIsWarnEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Warning];
  CheckFalse(fLogger.IsWarnEnabled);

  Logger.Levels := [TLogLevel.Warning];
  CheckTrue(fLogger.IsWarnEnabled);

  Logger.Enabled := False;
  CheckFalse(fLogger.IsWarnEnabled);
end;

procedure TTestLogger.TestLeaving;
begin
  Controller.Reset;

  Logger.Enabled := False;
  fLogger.Leaving(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warning];
  fLogger.Leaving(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  fLogger.Leaving(TLogLevel.Warning, nil, '');
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Leaving),  Ord(Controller.LastEntry.EntryType));
end;

procedure TTestLogger.TestLevels;
begin
  Logger.Levels := [];
  Check([] = Logger.Levels, 'Levels');

  Logger.Levels := LOG_ALL_LEVELS;
  Check(LOG_ALL_LEVELS = Logger.Levels, 'Levels');
end;

procedure TTestLogger.TestLogEntry(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Log(TLogEntry.Create(TLogLevel.Fatal, 'L1'));
  CheckEvent(enabled, TLogLevel.Fatal, 'L1');

  Controller.Reset;
  fLogger.Log(TLogEntry.Create(TLogLevel.Error, 'L2').SetException(fException));
  CheckEvent(enabled, TLogLevel.Error, 'L2', fException);
end;

procedure TTestLogger.TestLogEntry;
begin
  Logger.Levels := [TLogLevel.Fatal, TLogLevel.Error];
  TestLogEntry(True);
  Logger.Levels := [];
  TestLogEntry(False);
end;

procedure TTestLogger.TestLoggerProperties;
var
  props: ILoggerProperties;
begin
  props := Logger as ILoggerProperties;

  Logger.Enabled := False;
  CheckFalse(props.Enabled);

  Logger.Enabled := True;
  CheckTrue(props.Enabled);

  props.Enabled := False;
  CheckFalse(Logger.Enabled);

  props.Enabled := True;
  CheckTrue(Logger.Enabled);

  Logger.Levels := [];
  Check([] = props.Levels, 'Levels');

  Logger.Levels := LOG_ALL_LEVELS;
  Check(LOG_ALL_LEVELS = props.Levels, 'Levels');

  props.Levels := [];
  Check([] = Logger.Levels, 'Levels');

  props.Levels := LOG_ALL_LEVELS;
  Check(LOG_ALL_LEVELS = Logger.Levels, 'Levels');
end;

procedure TTestLogger.TestText;
begin
  Logger.Levels := [TLogLevel.Text];
  TestText(True);
  Logger.Levels := [];
  TestText(False);
end;

procedure TTestLogger.TestTrack;
var
  result: IInterface;
begin
  Controller.Reset;

  Logger.Enabled := False;
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warning];
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  result := fLogger.Track(TLogLevel.Warning, nil, '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Entering), Ord(Controller.LastEntry.EntryType));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Leaving), Ord(Controller.LastEntry.EntryType));

  Controller.Reset;

  Logger.Enabled := False;
  result := fLogger.Track(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := True;
  Logger.Levels := [TLogLevel.Warning];
  result := fLogger.Track(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  result := fLogger.Track(TLogLevel.Warning, nil, '', ['value']);
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Entering), Ord(Controller.LastEntry.EntryType));
  CheckEquals('value', Controller.LastEntry.Data.AsType<TArray<TValue>>[0].AsString);
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals(Ord(TLogEntryType.Leaving), Ord(Controller.LastEntry.EntryType));
end;

procedure TTestLogger.TestText(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Text('T1');
  CheckEvent(enabled, TLogLevel.Text, 'T1');

  Controller.Reset;
  fLogger.Text('T2', fException);
  CheckEvent(enabled, TLogLevel.Text, 'T2', fException);

  Controller.Reset;
  fLogger.Text('T%d', [3]);
  CheckEvent(enabled, TLogLevel.Text, 'T3');

  Controller.Reset;
  fLogger.Text('T%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Text, 'T4', fException);
end;

procedure TTestLogger.TestVerbose;
begin
  Logger.Levels := [TLogLevel.Verbose];
  TestVerbose(True);
  Logger.Levels := [];
  TestVerbose(False);
end;

procedure TTestLogger.TestVerbose(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Verbose('V1');
  CheckEvent(enabled, TLogLevel.Verbose, 'V1');

  Controller.Reset;
  fLogger.Verbose('V2', fException);
  CheckEvent(enabled, TLogLevel.Verbose, 'V2', fException);

  Controller.Reset;
  fLogger.Verbose('V%d', [3]);
  CheckEvent(enabled, TLogLevel.Verbose, 'V3');

  Controller.Reset;
  fLogger.Verbose('V%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Verbose, 'V4', fException);
end;

procedure TTestLogger.TestWarn;
begin
  Logger.Levels := [TLogLevel.Warning];
  TestWarn(True);
  Logger.Levels := [];
  TestWarn(False);
end;

procedure TTestLogger.TestWarn(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Warn('W1');
  CheckEvent(enabled, TLogLevel.Warning, 'W1');

  Controller.Reset;
  fLogger.Warn('W2', fException);
  CheckEvent(enabled, TLogLevel.Warning, 'W2', fException);

  Controller.Reset;
  fLogger.Warn('W%d', [3]);
  CheckEvent(enabled, TLogLevel.Warning, 'W3');

  Controller.Reset;
  fLogger.Warn('W%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Warning, 'W4', fException);
end;

procedure TTestLogger.TestLog(enabled: Boolean);
begin
  Controller.Reset;
  fLogger.Log(TLogLevel.Fatal, 'l1');
  CheckEvent(enabled, TLogLevel.Fatal, 'l1');

  Controller.Reset;
  fLogger.Log(TLogLevel.Error, 'l2', fException);
  CheckEvent(enabled, TLogLevel.Error, 'l2', fException);

  Controller.Reset;
  fLogger.Log(TLogLevel.Fatal, 'l%d', [3]);
  CheckEvent(enabled, TLogLevel.Fatal, 'l3');

  Controller.Reset;
  fLogger.Log(TLogLevel.Error, 'l%d', [4], fException);
  CheckEvent(enabled, TLogLevel.Error, 'l4', fException);
end;

procedure TTestLogger.TestLog;
begin
  Logger.Levels := [TLogLevel.Fatal, TLogLevel.Error];
  TestLog(True);
  Logger.Levels := [];
  TestLog(False);
end;

{$ENDREGION}


{$REGION 'TAppenderTestCase'}

procedure TAppenderTestCase.SetUp;
begin
  inherited;
  fController := TLoggerController.Create;
  fLogger := TLogger.Create(fController);
end;

procedure TAppenderTestCase.TearDown;
begin
  inherited;
  fLogger := nil;
  fController := nil;
  fAppender := nil;
end;

{$ENDREGION}


{$REGION 'TTestLogAppenderBase'}

procedure TTestLogAppenderBase.TestFormatMethodName;
var
  result: string;
begin
    result := TLogAppenderBase.FormatMethodName(nil, 'MethodName');
    CheckEquals('MethodName', result);

    result := TLogAppenderBase.FormatMethodName(ClassType, 'MethodName');
    CheckEquals('Spring.Tests.Logging.TTestLogAppenderBase.MethodName', result);
end;

{$ENDREGION}


{$REGION 'TTestStreamLogAppender'}

procedure TTestLogAppenderBase.TestFormatMsg;
{$IF sizeof(Pointer) = 4}
const ZEROS = '00000000';
{$ELSEIF sizeof(Pointer) = 8}
const ZEROS = '0000000000000000';
{$IFEND}
var
  result: string;
  e: Exception;
begin
  result := TLogAppenderBase.FormatMsg(TLogEntry.Create(TLogLevel.Unknown,
    'message'));
  CheckEquals('message', result);

  e := Exception.Create('exception');
  try
    result := TLogAppenderBase.FormatMsg(TLogEntry.Create(TLogLevel.Unknown,
      'message', e));
    CheckEquals('message: Exception Exception in module ' +
{$IFDEF ANDROID}
      '<unknown>'
{$ELSE}
      ExtractFileName(ParamStr(0))
{$ENDIF}
       + ' at ' + ZEROS + '.' + sLineBreak +
      'exception.' + sLineBreak, result);
  finally
    e.Free;
  end;
end;

procedure TTestLogAppenderBase.TestFormatText;
var
  result: string;
begin
  result := TLogAppenderBase.FormatText(TLogEntry.Create(TLogLevel.Unknown,
    'message'));
  CheckEquals('message', result);

  result := TLogAppenderBase.FormatText(TLogEntry.Create(TLogLevel.Unknown,
    TLogEntryType.Entering, 'message', nil));
  CheckEquals('Entering: message', result);

  result := TLogAppenderBase.FormatText(TLogEntry.Create(TLogLevel.Unknown,
    TLogEntryType.Leaving, 'message', nil));
  CheckEquals('Leaving: message', result);
end;

procedure TTestStreamLogAppender.SetUp;
begin
  inherited;
  fStream := TStringStream.Create;
  fAppender := TStreamLogAppender.Create(fStream);
  fController.AddAppender(fAppender);
end;

procedure TTestStreamLogAppender.TearDown;
begin
  inherited;
  fStream := nil;
end;

procedure TTestStreamLogAppender.TestException;
begin
  try
    Abort;
  except
    on e: EAbort do
    begin
      fLogger.Error('', e);
      CheckEquals('[ERROR] Exception EAbort in module',
        Copy(fStream.DataString, 15, 34));
      fStream.Clear;
      fLogger.Error('With message', e);
      CheckEquals('[ERROR] With message: Exception EAbort in module',
        Copy(fStream.DataString, 15, 48));
    end;
  end;
end;

procedure TTestStreamLogAppender.TestWrite;
begin
  fLogger.Info('Test');
  CheckTrue(EndsStr('[INFO ] Test', fStream.DataString));
end;

{$ENDREGION}

end.

