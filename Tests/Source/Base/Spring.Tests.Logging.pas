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
  Classes,
  Rtti,
  TestFramework,
  Spring,
  Spring.Reflection,
  Spring.Collections,
  Spring.Container.Common,
  Spring.Logging,
  Spring.Logging.Controller,
  Spring.Logging.Appenders,
  Spring.Logging.Loggers,
  Spring.Logging.Container,
  Spring.Logging.Configuration,
  Spring.Tests.Container;

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

implementation

{$REGION 'Internal test classes and helpers'}
type
  TAppenderMock = class(TInterfacedObject, ILogAppender)
  public
    writeCalled: Boolean;
    fEntry: TLogEntry;
    procedure Send(const entry: TLogEntry);
  end;

  TLoggerControllerMock = class(TInterfacedObject, ILoggerController)
  public
    fLastEntry: TLogEntry;
    procedure AddAppender(const appedner: ILogAppender);
    procedure Send(const entry: TLogEntry);
    procedure Reset;
  end;

  TTestLoggerHelper = class helper for TTestLogger
  private
    function GetController: TLoggerControllerMock;
    function GetLogger: TLogger;
  public
    property Controller: TLoggerControllerMock read GetController;
    property Logger: TLogger read GetLogger;
  end;

  TLoggerAccess = class(TLogger);

{ TAppenderMock }

procedure TAppenderMock.Send(const entry: TLogEntry);
begin
  writeCalled := true;
  fEntry := entry;
end;

{ TLoggerControllerMock }

procedure TLoggerControllerMock.AddAppender(const appedner: ILogAppender);
begin
  raise ETestError.Create('Should be inaccessible');
end;

procedure TLoggerControllerMock.Reset;
begin
  fLastEntry := TLogEntry.Create(TLogLevel.Unknown, '');
end;

procedure TLoggerControllerMock.Send(const entry: TLogEntry);
begin
  fLastEntry := entry;
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

  CheckTrue(appender.writeCalled);
  CheckEquals(Ord(TLogLevel.Fatal), Ord(appender.fEntry.Level));
  CheckEquals(MSG, appender.fEntry.Msg);
end;
{$ENDREGION}

{$REGION 'TTestLogger'}
{ TTestLogger }

procedure TTestLogger.CheckEvent(enabled: Boolean; level: TLogLevel;
  const msg: string; const exc: Exception = nil);
begin
  CheckEquals(enabled, Controller.fLastEntry.Level <> TLogLevel.Unknown);
  if (not enabled) then
    Exit;

  CheckEquals(Ord(level), Ord(Controller.fLastEntry.Level));
  CheckEquals(msg, Controller.fLastEntry.Msg);
  CheckSame(exc, Controller.fLastEntry.Exc);
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
{$REGION 'Test subtypes'}
type
  TLoggerDefault = class(TLogger);
  TLogger1 = class(TLogger);
  TLogger2 = class(TLogger);

  IService = interface
    ['{DFFC820C-120B-4828-8D22-21DDC60E4398}']
  end;

  TImpl = class(TInterfacedObject, IService)
  private
    [Inject]
    fLogger1: ILogger;
    [Inject('second')]
    fLogger2: ILogger;
  end;

  TObjProc = class
  private
    fLogger: ILogger;
  public
    [Inject]
    procedure SetLoggger(const logger: ILogger);
  end;

  TObjCtor = class
  private
    fLogger: ILogger;
  public
    constructor Create(const logger: ILogger);
  end;

  TObjLazy = class
  private
    [Inject]
    fLogger1: Lazy<ILogger>;
    [Inject('second')]
    fLogger2: Lazy<ILogger>;
  end;

{ TObjProc }

procedure TObjProc.SetLoggger(const logger: ILogger);
begin
  fLogger := logger;
end;

{ TObjCtor }

constructor TObjCtor.Create(const logger: ILogger);
begin
  fLogger := logger;
end;
{$ENDREGION}

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
  fContainer.RegisterType<TLogger1>.AsSingleton.Implements<ILogger>('first');
  fContainer.RegisterType<TLogger2>.AsSingleton.Implements<ILogger>('second');
end;

procedure TTestLogSubResolverAndConfiguration.TestBaseClass;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObject>('first');

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.fLogger1, TLogger1);
  CheckIs(obj.fLogger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestClass;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TImpl>('first');

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.fLogger1, TLogger1);
  CheckIs(obj.fLogger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestConstructor;
var
  obj: TObjCtor;
begin
  fContainer.RegisterType<TObjCtor>.Implements<TObjCtor>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjCtor>('first');

  obj := fContainer.Resolve<TObjCtor>;

  CheckIs(obj.fLogger, TLogger1);
end;

procedure TTestLogSubResolverAndConfiguration.TestInterface;
var
  intf: IService;
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TImpl>('first');

  intf := fContainer.Resolve<TImpl>;
  obj := TObject(intf) as TImpl;

  CheckIs(obj.fLogger1, TLogger1);
  CheckIs(obj.fLogger2, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestLazy;
var
  obj: TObjLazy;
begin
  fContainer.RegisterType<TObjLazy>.Implements<TObjLazy>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjLazy>('first');

  obj := fContainer.Resolve<TObjLazy>;

  CheckIs(obj.fLogger1.Value, TLogger1);
  CheckIs(obj.fLogger2.Value, TLogger2);
end;

procedure TTestLogSubResolverAndConfiguration.TestMethod;
var
  obj: TObjProc;
begin
  fContainer.RegisterType<TObjProc>.Implements<TObjProc>.AsSingleton;
  fContainer.Build;
  fContainer.Resolve<TLoggingConfiguration>.RegisterLogger<TObjProc>('first');

  obj := fContainer.Resolve<TObjProc>;

  CheckIs(obj.fLogger, TLogger1);
end;

procedure TTestLogSubResolverAndConfiguration.TestNoInject;
var
  obj: TImpl;
begin
  fContainer.RegisterType<TImpl>.Implements<TImpl>.AsSingleton;
  fContainer.Build;

  obj := fContainer.Resolve<TImpl>;

  CheckIs(obj.fLogger1, TLoggerDefault);
  CheckIs(obj.fLogger2, TLogger2);
end;
{$ENDREGION}

end.

