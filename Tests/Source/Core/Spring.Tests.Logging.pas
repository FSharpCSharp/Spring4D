unit Spring.Tests.Logging;

interface

uses
  TestFramework,
  Spring.Configuration,
  Spring.Configuration.Sources,
  Spring.Services.Logging,
  Spring.Logging.Core,
  Spring.Logging.Appenders,
  Spring.Logging.LoggerManager;

type
  TTestLoggingConfig = class(TTestCase)
  private
    fLoggerManager: ILoggerManager;
    fLogger: ILogger;
    fConfigurationSource: IConfigurationSource;
    fConfiguration: IConfiguration;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLog;
  end;

implementation

{ TLoggingConfigTest }

(*
  GlobalContainer.RegisterComponent<TLoggerManager>.Implements<ILoggerManager>.Implements<ILoggerFactory>.AsSingleton;
  GlobalContainer.Build;
  manager := GlobalContainer.Resolve<ILoggerManager>;
*)

procedure TTestLoggingConfig.SetUp;
begin
  inherited;
  fLoggerManager := TLoggerManager.Create;
  fConfigurationSource := TXmlConfigurationSource.Create('Spring.Tests.xml');
  fConfiguration := fConfigurationSource.GetConfiguration.GetSection('logging');
  (fLoggerManager as IConfigurable).Configure(fConfiguration);
  fLogger := fLoggerManager.GetLogger(Self.ClassInfo);
end;

procedure TTestLoggingConfig.TearDown;
begin
  inherited;
  fLogger := nil;
  fLoggerManager := nil;
  fConfiguration := nil;
  fConfigurationSource := nil;
end;

procedure TTestLoggingConfig.TestLog;
begin
  fLogger.Info('This is a info.');
  fLogger.Error('This is a error.');
end;

end.
