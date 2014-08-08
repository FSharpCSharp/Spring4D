program Spring.Tests.Logging.CodeSite;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Spring.Logging,
  Spring.Logging.Controller,
  Spring.Logging.Loggers,
  Spring.Logging.Appenders.CodeSite in '..\Source\Base\Logging\Spring.Logging.Appenders.CodeSite.pas';

function InitLog: ILogger;
var
  controller: TLoggerController;
  appender: TCodeSiteAppender;
begin
  appender := TCodeSiteAppender.Create;
  appender.Levels := LOG_ALL_LEVELS;

  controller := TLoggerController.Create;
  controller.Levels := LOG_ALL_LEVELS;
  controller.AddAppender(appender);

  Result := TLogger.Create(controller);
  (Result as ILoggerProperties).Levels := LOG_ALL_LEVELS;
end;

var
  log: ILogger;

procedure TestTrack;
begin
  log.Track(TLogLevel.Info, TLogger, 'TestTrack');
  log.Warn('Warning text');
  log.Log(TLogEntry.Create(TLogLevel.Info, 'Log message with color')
    .SetColor($5555FF));
end;

procedure TestException;
begin
  log.Track(TLogLevel.Info, TLogger, 'TestException');
  try
    Abort;
  except
    on E: EAbort do
    begin
      log.Error('', E);
      log.Error('With message', E);
    end;
  end;
end;

begin
  log := InitLog;
  log.Entering(TLogLevel.Info, nil, 'Spring.Tests.Logging.CodeSite');
  log.Fatal('Fatal message');
  log.Error('Error message');
  log.Info('Info message');
  log.Text('Text message');
  log.Debug('Debug message');
  log.Verbose('Verbose message');
  TestTrack;
  TestException;
  log.Leaving(TLogLevel.Info, nil, 'Spring.Tests.Logging.CodeSite');
end.
