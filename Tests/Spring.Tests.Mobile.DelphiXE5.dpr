program Spring.Tests.Mobile.DelphiXE5;

uses
  System.StartUpCopy,
  TestFramework,
  FMXTestRunner in 'Source\dUnit\FMXTestRunner.pas' {FMXTestRunner},
  Spring.TestUtils in 'Source\Spring.TestUtils.pas',
  Spring.Tests.Base in 'Source\Base\Spring.Tests.Base.pas',
  Spring.Tests.Collections in 'Source\Base\Spring.Tests.Collections.pas',
  Spring.Tests.DesignPatterns in 'Source\Base\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in 'Source\Base\Spring.Tests.Helpers.pas',
  Spring.Tests.Reflection.ValueConverters in 'Source\Base\Spring.Tests.Reflection.ValueConverters.pas',
  Spring.Tests.SysUtils in 'Source\Base\Spring.Tests.SysUtils.pas',
  Spring.Tests.Container.Interfaces in 'Source\Core\Spring.Tests.Container.Interfaces.pas',
  Spring.Tests.Container.Components in 'Source\Core\Spring.Tests.Container.Components.pas',
  Spring.Tests.Container.LifetimeManager in 'Source\Core\Spring.Tests.Container.LifetimeManager.pas',
  Spring.Tests.Container in 'Source\Core\Spring.Tests.Container.pas',
  Spring.Tests.Pool in 'Source\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in 'Source\Extensions\Spring.Tests.Cryptography.pas',
  Spring.Tests.Utils in 'Source\Extensions\Spring.Tests.Utils.pas',
  Spring.TestRegistration in 'Source\Spring.TestRegistration.pas',
  Spring.Container;

{$R *.res}

begin
  CleanupGlobalContainer;
  RegisterTestCases;
  TFMXTestRunner.RunRegisteredTests;
  TestFramework.ClearRegistry;
end.
