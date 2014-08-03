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

interface

uses
  SysUtils,
  StrUtils,
  TypInfo,
  Classes,
  Rtti,
  TestFramework,
  Spring,
  Spring.Reflection,
  Spring.Collections,
  Spring.Container,
  Spring.Container.Common,
  Spring.Logging,
  Spring.Logging.Controller,
  Spring.Logging.Appenders,
  Spring.Logging.Loggers,
  Spring.Logging.Container,
  Spring.Logging.Configuration,
  Spring.Tests.Container,
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
      const exc: Exception = nil);
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

    procedure TestFormatMethodName;

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

  {$REGION 'TTestStreamLogAppender'}
  TTestStreamLogAppender = class(TAppenderTestCase)
  private
    fStream: TStringStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
  end;
  {$ENDREGION}

  {$REGION 'TTestLogInsideContainer'}
  TTestLogInsideContainer = class(TContainerTestCase)
  published
    procedure TestLog;
    procedure TestChainedControllers;
  end;
  {$ENDREGION}

  {$REGION 'TTestLogSubResolverAndConfiguration'}
  TTestLogSubResolverAndConfiguration = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestNoInject;
    procedure TestClass;
    procedure TestInterface;
    //procedure TestRecord;
    procedure TestBaseClass;
    procedure TestConstructor;
    procedure TestMethod;
    procedure TestLazy;
  end;
  {$ENDREGION}

  TTestLoggingConfiguration = class(TContainerTestCase)
  private
    fStrings: TStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInjectMultipleAppendersToSingleController;
    procedure TestLeak;
    procedure TestMultipleConfiguration;
    procedure TestDuplicateDefault;
    procedure TestUnknownClass;
    procedure TestUnknownProperty;
    procedure TestUnknownPropertyKind;
    procedure TestNonInstanceType;

    procedure TestReadProperties;
    procedure TestReadAppenders;
    procedure TestDefaultController;
    procedure TestReadController;
    procedure TestReadSingleControllerAsDefault;
    procedure TestDefaultLogger;
    procedure TestReadLogger;

    procedure TestAddAppendersToControllers;
    procedure TestAddChainedController;
    procedure TestAddLoggerAssignments;

    procedure TestSimpleConfiguration;
    procedure TestComplexConfiguration;
  end;

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

  TStringsHelper = class helper for TStrings
    function Add(const s: string): TStrings;
  end;

  TLoggerAccess = class(TLogger);

{ TTestLoggerHelper }

function TTestLoggerHelper.GetController: TLoggerControllerMock;
begin
  Result := TObject(fController) as TLoggerControllerMock;
end;

function TTestLoggerHelper.GetLogger: TLogger;
begin
  Result := TObject(fLogger) as TLogger;
end;

{ TStringsHelper }

function TStringsHelper.Add(const s: string): TStrings;
begin
  TStringList(Self).Add(s);
  Result := Self;
end;
{$ENDREGION}

{$REGION 'TTestLoggerController'}
{ TTestLoggerController }

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
{$ENDREGION}

{$REGION 'TTestLogger'}
{ TTestLogger }

procedure TTestLogger.CheckEvent(enabled: Boolean; level: TLogLevel;
  const msg: string; const exc: Exception = nil);
begin
  CheckEquals(enabled, Controller.LastEntry.Level <> TLogLevel.Unknown);
  if (not enabled) then
    Exit;

  CheckEquals(Ord(level), Ord(Controller.LastEntry.Level));
  CheckEquals(msg, Controller.LastEntry.Msg);
  CheckSame(exc, Controller.LastEntry.Exc);
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
  TestDebug(true);
  Logger.Levels := [];
  TestDebug(false);
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
  Logger.Enabled := false;
  CheckFalse(Logger.Enabled);

  Logger.Enabled := true;
  CheckTrue(Logger.Enabled);
end;

procedure TTestLogger.TestEntering;
begin
  Controller.Reset;

  Logger.Enabled := false;
  fLogger.Entering(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := true;
  Logger.Levels := [TLogLevel.Warning];
  fLogger.Entering(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  fLogger.Entering(TLogLevel.Warning, nil, '');
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));

  Controller.Reset;

  Logger.Enabled := false;
  fLogger.Entering(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := true;
  Logger.Levels := [TLogLevel.Warning];
  fLogger.Entering(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  fLogger.Entering(TLogLevel.Warning, nil, '', ['value']);
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
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
  TestError(true);
  Logger.Levels := [];
  TestError(false);
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
  TestFatal(true);
  Logger.Levels := [];
  TestFatal(false);
end;

procedure TTestLogger.TestFormatMethodName;
var
  result: string;
begin
    result := TLoggerAccess.FormatMethodName(nil, 'MethodName');
    CheckEquals('MethodName', result);

    result := TLoggerAccess.FormatMethodName(ClassType, 'MethodName');
    CheckEquals('Spring.Tests.Logging.TTestLogger.MethodName', result);
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
  TestInfo(true);
  Logger.Levels := [];
  TestInfo(false);
end;

procedure TTestLogger.TestIsDebugEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Debug];
  CheckFalse(fLogger.IsDebugEnabled);

  Logger.Levels := [TLogLevel.Debug];
  CheckTrue(fLogger.IsDebugEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsDebugEnabled);
end;

procedure TTestLogger.TestIsEnabled;
var l: TLogLevel;
begin
  for l in LOG_ALL_LEVELS do
    CheckEquals(l in Logger.Levels, Logger.IsEnabled(l));
  Logger.Enabled := false;
  for l in LOG_ALL_LEVELS do
    CheckFalse(Logger.IsEnabled(l));
end;

procedure TTestLogger.TestIsErrorEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Error];
  CheckFalse(fLogger.IsErrorEnabled);

  Logger.Levels := [TLogLevel.Error];
  CheckTrue(fLogger.IsErrorEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsErrorEnabled);
end;

procedure TTestLogger.TestIsFatalEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Fatal];
  CheckFalse(fLogger.IsFatalEnabled);

  Logger.Levels := [TLogLevel.Fatal];
  CheckTrue(fLogger.IsFatalEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsFatalEnabled);
end;

procedure TTestLogger.TestIsInfoEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Info];
  CheckFalse(fLogger.IsInfoEnabled);

  Logger.Levels := [TLogLevel.Info];
  CheckTrue(fLogger.IsInfoEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsInfoEnabled);
end;

procedure TTestLogger.TestIsTextEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Text];
  CheckFalse(fLogger.IsTextEnabled);

  Logger.Levels := [TLogLevel.Text];
  CheckTrue(fLogger.IsTextEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsTextEnabled);
end;

procedure TTestLogger.TestIsVerboseEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Verbose];
  CheckFalse(fLogger.IsVerboseEnabled);

  Logger.Levels := [TLogLevel.Verbose];
  CheckTrue(fLogger.IsVerboseEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsVerboseEnabled);
end;

procedure TTestLogger.TestIsWarnEnabled;
begin
  Logger.Levels := LOG_ALL_LEVELS - [TLogLevel.Warning];
  CheckFalse(fLogger.IsWarnEnabled);

  Logger.Levels := [TLogLevel.Warning];
  CheckTrue(fLogger.IsWarnEnabled);

  Logger.Enabled := false;
  CheckFalse(fLogger.IsWarnEnabled);
end;

procedure TTestLogger.TestLeaving;
begin
  Controller.Reset;

  Logger.Enabled := false;
  fLogger.Leaving(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := true;
  Logger.Levels := [TLogLevel.Warning];
  fLogger.Leaving(TLogLevel.Info, nil, '');
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  fLogger.Leaving(TLogLevel.Warning, nil, '');
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
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
  TestLogEntry(true);
  Logger.Levels := [];
  TestLogEntry(false);
end;

procedure TTestLogger.TestLoggerProperties;
var
  props: ILoggerProperties;
begin
  props := Logger as ILoggerProperties;

  Logger.Enabled := false;
  CheckFalse(props.Enabled);

  Logger.Enabled := true;
  CheckTrue(props.Enabled);

  props.Enabled := false;
  CheckFalse(Logger.Enabled);

  props.Enabled := true;
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
  TestText(true);
  Logger.Levels := [];
  TestText(false);
end;

procedure TTestLogger.TestTrack;
var
  result: IInterface;
begin
  Controller.Reset;

  Logger.Enabled := false;
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := true;
  Logger.Levels := [TLogLevel.Warning];
  result := fLogger.Track(TLogLevel.Info, nil, '');
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  result := fLogger.Track(TLogLevel.Warning, nil, '');
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));

  Controller.Reset;

  Logger.Enabled := false;
  result := fLogger.Track(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  Logger.Enabled := true;
  Logger.Levels := [TLogLevel.Warning];
  result := fLogger.Track(TLogLevel.Info, nil, '', [TValue.Empty]);
  CheckNull(result);
  CheckTrue(TLogLevel.Unknown = Controller.LastEntry.Level);

  result := fLogger.Track(TLogLevel.Warning, nil, '', ['value']);
  CheckNotNull(result);
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
  CheckEquals('value', Controller.LastEntry.Data.AsType<TArray<TValue>>[0].AsString);
  Controller.Reset;
  result := nil;
  CheckEquals(Ord(TLogLevel.Warning), Ord(Controller.LastEntry.Level));
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
  TestVerbose(true);
  Logger.Levels := [];
  TestVerbose(false);
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
  TestWarn(true);
  Logger.Levels := [];
  TestWarn(false);
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
  TestLog(true);
  Logger.Levels := [];
  TestLog(false);
end;
{$ENDREGION}

{$REGION 'TAppenderTestCase'}
{ TAppenderTestCase }

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

{$REGION 'TTestStreamLogAppender'}
{ TTestStreamLogAppender }

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

procedure TTestStreamLogAppender.TestWrite;
begin
  fLogger.Info('Test');
  CheckTrue(EndsStr('[INFO ] Test', fStream.DataString));
end;
{$ENDREGION}

{$REGION 'TTestLogInsideContainer'}
{ TTestLogInsideContainer }

procedure TTestLogInsideContainer.TestChainedControllers;
begin
  fContainer.RegisterType<TLogger>.AsSingleton.AsDefault;
  fContainer.RegisterType<TLogger>.AsSingleton
    .Implements<ILogger>('l2').InjectField('fController', 'c2.ctl');
  fContainer.RegisterType<TLoggerController>.AsSingleton
    .InjectMethod('AddAppender', ['c2']);
  //Acts as appender and controller together
  fContainer.RegisterType<TLoggerController>.AsSingleton
    .Implements<ILogAppender>('c2').Implements<ILoggerController>('c2.ctl');

  fContainer.Build;

  fContainer.Resolve<Ilogger>;
  fContainer.Resolve<Ilogger>('l2');

  Check(true); //If there are any errors this won't get called
end;

procedure TTestLogInsideContainer.TestLog;
var
  stream: TStringStream;
begin
  fContainer.RegisterType<TLogger>.AsSingleton;
  fContainer.RegisterType<TLoggerController>.AsSingleton;

  fContainer.Build;
  stream := TStringStream.Create;
  fContainer.Resolve<ILoggerController>.AddAppender(
    TStreamLogAppender.Create(stream));
  fContainer.Resolve<ILogger>.Warn('Test');
  CheckTrue(EndsStr('[WARN ] Test', stream.DataString));
end;
{$ENDREGION}

{$REGION 'TTestLogSubResolverAndConfiguration'}

{ TTestLogSubResolverAndConfiguration }

procedure TTestLogSubResolverAndConfiguration.SetUp;
begin
  inherited;
  fContainer.Kernel.Resolver.AddSubResolver(
    TLoggerResolver.Create(fContainer.Kernel));
  fContainer.RegisterType<TLoggerController>.AsSingleton;
  fContainer.RegisterType<TLoggingConfiguration>
    .Implements<TLoggingConfiguration>.AsSingleton;

  //And register some loggers that we may use
  fContainer.RegisterType<TLoggerDefault>.AsSingleton.AsDefault;
  fContainer.RegisterType<TLogger1>.AsSingleton.Implements<ILogger>('logging.logger1');
  fContainer.RegisterType<TLogger2>.AsSingleton.Implements<ILogger>('logging.logger2');
end;

procedure TTestLogSubResolverAndConfiguration.TestBaseClass;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObject>('logging.logger1');

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.Logger1, TLogger1);
  CheckIs(obj.Logger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestClass;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TImpl>('logging.logger1');

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.Logger1, TLogger1);
  CheckIs(obj.Logger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestConstructor;
var
  obj: TObjCtor;
begin
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjCtor>('logging.logger1');

  obj := fContainer.Resolve<TObjCtor>;

  CheckIs(obj.Logger, TLogger1);
end;

procedure TTestLogSubResolverAndConfiguration.TestInterface;
var
  intf: IService;
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TImpl>('logging.logger1');

  intf := fContainer.Resolve<TImpl>;
  obj := TObject(intf) as TImpl;

  CheckIs(obj.Logger1, TLogger1);
  CheckIs(obj.Logger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestLazy;
var
  obj: TObjLazy;
begin
  fContainer.RegisterType<TObjLazy>.Implements<TObjLazy>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjLazy>('logging.logger1');

  obj := fContainer.Resolve<TObjLazy>;

  CheckIs(obj.Logger1.Value, TLogger1);
  CheckIs(obj.Logger2.Value, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestMethod;
var
  obj: TObjProc;
begin
  fContainer.RegisterType<TObjProc>.Implements<TObjProc>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjProc>('logging.logger1');

  obj := fContainer.Resolve<TObjProc>;

  CheckIs(obj.Logger, TLogger1);
end;

procedure TTestLogSubResolverAndConfiguration.TestNoInject;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.Logger1, TLoggerDefault);
  CheckIs(obj.Logger2, TLogger2);
end;
{$ENDREGION}

{ TTestLoggingConfiguration }

procedure TTestLoggingConfiguration.SetUp;
begin
  inherited;
  fStrings := TStringList.Create;
end;

procedure TTestLoggingConfiguration.TearDown;
begin
  fStrings.Free;
  inherited;
end;

procedure TTestLoggingConfiguration.TestLeak;
begin
  //If done incorrectly this test will create a leak
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  Check(true);
end;

procedure TTestLoggingConfiguration.TestMultipleConfiguration;
begin
  fContainer.RegisterType<TLoggingConfiguration>
    .Implements<TLoggingConfiguration>;
  ExpectedException := ERegistrationException;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestNonInstanceType;
var
  rec: TSomeRecord; //Reference the type
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TSomeRecord');
  ExpectedException := EClassNotFound;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  rec := Default(TSomeRecord);
end;

procedure TTestLoggingConfiguration.TestReadAppenders;
var
  i: ILogAppender;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock2'); //Tets non-fully qualified name
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILogAppender>('logging.appender1.appender');
  CheckIs(TObject(i), TAppenderMock);
  i := fContainer.Resolve<ILogAppender>;
  CheckIs(TObject(i), TAppenderMock2);
end;

procedure TTestLoggingConfiguration.TestReadController;
var
  i: ILoggerController;
  a: ILogAppender;
begin
  fStrings
    .Add('[controllers\ctl1]')
    .Add('class = Spring.Tests.Logging.Types.TLoggerControllerMock');
  fStrings
    .Add('[controllers\default]');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILoggerController>('logging.ctl1.controller');
  CheckIs(TObject(i), TLoggerControllerMock);
  a := fContainer.Resolve<ILogAppender>('logging.ctl1.appender');
  CheckSame(TObject(i), TObject(a));
  i := fContainer.Resolve<ILoggerController>;
  CheckIs(TObject(i), TLoggerController);
end;

procedure TTestLoggingConfiguration.TestReadLogger;
var
  i: ILogger;
begin
  fStrings
    .Add('[loggers\log1]')
    .Add('class = Spring.Tests.Logging.Types.TLogger1');
  fStrings
    .Add('[loggers\default]');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILogger>('logging.log1');
  CheckIs(TObject(i), TLogger1);
  i := fContainer.Resolve<ILogger>;
  CheckIs(TObject(i), TLogger);
end;

procedure TTestLoggingConfiguration.TestReadProperties;
var
  i: ILogAppender;
  appender: TAppenderMock;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock')
    .Add('enabled = false')
    .Add('someInt = 1')
    .Add('someString = test')
    .Add('someEnum = Warning')
    .Add('levels = Error, Info');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;
  i := fContainer.Resolve<ILogAppender>;

  CheckIs(TObject(i), TAppenderMock);
  appender := TAppenderMock(i);
  CheckFalse(appender.Enabled);
  CheckEquals(1, appender.SomeInt);
  CheckEquals('test', appender.SomeString);
  CheckEquals(Ord(TLogLevel.Warning), Ord(appender.SomeEnum));
  Check([TLogLevel.Error, TLogLevel.Info] = appender.Levels);
end;

procedure TTestLoggingConfiguration.TestReadSingleControllerAsDefault;
var
  i: ILoggerController;
begin
  fStrings
    .Add('[controllers\ctl1]')
    .Add('class = Spring.Tests.Logging.Types.TLoggerControllerMock');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  i := fContainer.Resolve<ILoggerController>;
  CheckIs(TObject(i), TLoggerControllerMock);
end;

procedure TTestLoggingConfiguration.TestSimpleConfiguration;
var
  appender: TAppenderMock;
  o: TObjCtor;
begin
  fStrings
    .Add('[appenders\1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;

  fContainer.Build;

  o := fContainer.Resolve<TObjCtor>;
  o.Logger.Fatal('test');

  appender := TObject(fContainer.Resolve<ILogAppender>) as TAppenderMock;
  CheckTrue(appender.WriteCalled);
  CheckEquals('test', appender.Entry.Msg);
end;

procedure TTestLoggingConfiguration.TestUnknownClass;
begin
  fStrings
    .Add('[appenders\default]')
    .Add('class = NotExistent');
  ExpectedException := EClassNotFound;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestUnknownProperty;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock')
    .Add('notExistent = false');
  ExpectedException := EPropertyError;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestUnknownPropertyKind;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock')
    .Add('someFloat = 0');
  ExpectedException := EPropertyConvertError;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestAddAppendersToControllers;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\appender2]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock2');
  fStrings
    .Add('[controllers\controller1]')
    .Add('appender = appender2');

   TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(1, appenders.Count);
  CheckSame(fContainer.Resolve<ILogAppender>('logging.appender2.appender'),
    appenders[0]);
end;

procedure TTestLoggingConfiguration.TestAddChainedController;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[controllers\default]')
    .Add('appender = chained');
  fStrings
    .Add('[controllers\chained]');

   TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(1, appenders.Count);
  CheckSame(fContainer.Resolve<ILogAppender>('logging.chained.appender'),
    appenders[0]);
end;

procedure TTestLoggingConfiguration.TestAddLoggerAssignments;
var
  objCtor: TObjCtor;
  objProc: TObjProc;
  objImpl: TImpl;
begin
  fStrings
    .Add('[loggers\default]');
  fStrings
    .Add('[loggers\logger2]')
    .Add('assign = TObjCtor');
  fStrings
    .Add('[loggers\logger3]')
    .Add('assign = TObjProc');
  fStrings
    .Add('[loggers\logger4]')
    .Add('assign = TObject');

  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;
  fContainer.RegisterType<TObjProc>.Implements<TObjProc>.AsSingleton;
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  objCtor := fContainer.Resolve<TObjCtor>;
  objProc := fContainer.Resolve<TObjProc>;
  objImpl := fContainer.Resolve<TImpl>;

  CheckSame(fContainer.Resolve<ILogger>('logging.logger2'), objCtor.Logger);
  CheckSame(fContainer.Resolve<ILogger>('logging.logger3'), objProc.Logger);
  CheckSame(fContainer.Resolve<ILogger>('logging.logger4'), objImpl.Logger1);
  CheckSame(fContainer.Resolve<ILogger>('logging.logger2'), objImpl.Logger2);
end;

procedure TTestLoggingConfiguration.TestComplexConfiguration;
var
  logger1,
  logger2: ILogger;
  controller1,
  controller2: ILoggerController;
  appender1,
  appender2,
  appenderCtl: ILogAppender;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\appender2]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock2')
    .Add('levels = Fatal, Error, Warning');

  fStrings
    .Add('[controllers\default]')
    .Add('appender = appender1')
    .Add('appender = controller2');
  fStrings
    .Add('[controllers\controller2]')
    .Add('class = Spring.Tests.Logging.Types.TLoggerController2')
    .Add('appender = appender2')
    .Add('levels = Fatal, Error');

  fStrings
    .Add('[loggers\default]');
  fStrings
    .Add('[loggers\logger2]')
    .Add('class = Spring.Tests.Logging.Types.TLogger2')
    .Add('controller = controller2')
    .Add('levels = Fatal');

   TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  logger1 := fContainer.Resolve<ILogger>;
  logger2 := fContainer.Resolve<ILogger>('logging.logger2');
  controller1 := fContainer.Resolve<ILoggerController>;
  controller2 := fContainer.Resolve<ILoggerController>('logging.controller2.controller');
  appender1 := fContainer.Resolve<ILogAppender>('logging.appender1.appender');
  appender2 := fContainer.Resolve<ILogAppender>('logging.appender2.appender');
  appenderCtl := fContainer.Resolve<ILogAppender>('logging.controller2.appender');

  f := TType.GetType<TLogger>.GetField('fController');
  CheckSame(controller1, f.GetValue(TObject(logger1)).AsType<ILoggerController>);
  CheckSame(controller2, f.GetValue(TObject(logger2)).AsType<ILoggerController>);

  f := TType.GetType<TLoggerController2>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller1)).AsType<IList<ILogAppender>>;

  CheckEquals(2, appenders.Count);
  CheckSame(appender1, appenders[0]);
  CheckSame(appenderCtl, appenders[1]);

  appenders := f.GetValue(TObject(controller2)).AsType<IList<ILogAppender>>;
  CheckEquals(1, appenders.Count);
  CheckSame(appender2, appenders[0]);
end;

procedure TTestLoggingConfiguration.TestDefaultController;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  fStrings
    .Add('[appenders\appender1]')
    .Add('class = Spring.Tests.Logging.Types.TAppenderMock');
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock2'); //Tets non-fully qualified name
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);

  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;
  CheckIs(TObject(controller), TLoggerController);

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(appenders.Count, 2);
  CheckSame(fContainer.Resolve<ILogAppender>('logging.appender1.appender'), appenders[0]);
  CheckSame(fContainer.Resolve<ILogAppender>, appenders[1]);
end;

procedure TTestLoggingConfiguration.TestDefaultLogger;
var
  logger: ILogger;
begin
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
  fContainer.Build;

  logger := fContainer.Resolve<ILogger>;
  CheckIs(TObject(logger), TLogger);
end;

procedure TTestLoggingConfiguration.TestDuplicateDefault;
begin
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock');
  fStrings
    .Add('[appenders\default]')
    .Add('class = TAppenderMock');
  ExpectedException := ERegistrationException;
  TLoggingConfiguration.LoadFromStrings(fContainer, fStrings);
end;

procedure TTestLoggingConfiguration.TestInjectMultipleAppendersToSingleController;
var
  controller: ILoggerController;
  f: TRttiField;
  appenders: IList<ILogAppender>;
begin
  //Check that current configuration implementation will work with the container
  fContainer.RegisterType<TAppenderMock>.AsDefault.AsSingleton;
  fContainer.RegisterType<TAppenderMock>.Implements<ILogAppender>('appender1')
    .AsSingleton;
  fContainer.RegisterType<TAppenderMock>.Implements<ILogAppender>('appender2')
    .AsSingleton;
  fContainer.RegisterType<TLoggerController>.AsSingleton.Implements<ILoggerController>
    .InjectMethod('AddAppender', ['appender2'])
    .InjectMethod('AddAppender', ['appender1'])
    .InjectMethod('AddAppender'); //Creates circular dependency, not if it is registered with implements

  fContainer.Build;

  controller := fContainer.Resolve<ILoggerController>;

  f := TType.GetType<TLoggerController>.GetField('fAppenders');
  appenders := f.GetValue(TObject(controller)).AsType<IList<ILogAppender>>;

  CheckEquals(3, appenders.Count);
  CheckSame(fContainer.Resolve<ILogAppender>('appender2'), appenders[0]);
  CheckSame(fContainer.Resolve<ILogAppender>('appender1'), appenders[1]);
  CheckSame(fContainer.Resolve<ILogAppender>, appenders[2]);
end;

end.

