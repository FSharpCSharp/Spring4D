program Spring.Console.BuildAll;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Spring in '..\Source\Base\Spring.pas',
  Spring.Collections in '..\Source\Base\Collections\Spring.Collections.pas',
  Spring.Collections.Base in '..\Source\Base\Collections\Spring.Collections.Base.pas',
  Spring.Collections.Dictionaries in '..\Source\Base\Collections\Spring.Collections.Dictionaries.pas',
  Spring.Collections.Events in '..\Source\Base\Collections\Spring.Collections.Events.pas',
  Spring.Collections.Extensions in '..\Source\Base\Collections\Spring.Collections.Extensions.pas',
  Spring.Collections.Lists in '..\Source\Base\Collections\Spring.Collections.Lists.pas',
  Spring.Collections.Queues in '..\Source\Base\Collections\Spring.Collections.Queues.pas',
  Spring.Collections.Sets in '..\Source\Base\Collections\Spring.Collections.Sets.pas',
  Spring.Collections.Stacks in '..\Source\Base\Collections\Spring.Collections.Stacks.pas',
  Spring.DesignPatterns in '..\Source\Base\Spring.DesignPatterns.pas',
  Spring.Events in '..\Source\Base\Spring.Events.pas',
  Spring.Events.Base in '..\Source\Base\Spring.Events.Base.pas',
  Spring.Helpers in '..\Source\Base\Spring.Helpers.pas',
  Spring.Reflection in '..\Source\Base\Reflection\Spring.Reflection.pas',
  Spring.Reflection.ValueConverters in '..\Source\Base\Reflection\Spring.Reflection.ValueConverters.pas',
  Spring.ResourceStrings in '..\Source\Base\Spring.ResourceStrings.pas',
  Spring.SystemUtils in '..\Source\Base\Spring.SystemUtils.pas',
  Spring.Container in '..\Source\Core\Container\Spring.Container.pas',
  Spring.Container.Builder in '..\Source\Core\Container\Spring.Container.Builder.pas',
  Spring.Container.ComponentActivator in '..\Source\Core\Container\Spring.Container.ComponentActivator.pas',
  Spring.Container.Core in '..\Source\Core\Container\Spring.Container.Core.pas',
  Spring.Container.Extensions in '..\Source\Core\Container\Spring.Container.Extensions.pas',
  Spring.Container.Injection in '..\Source\Core\Container\Spring.Container.Injection.pas',
  Spring.Container.LifetimeManager in '..\Source\Core\Container\Spring.Container.LifetimeManager.pas',
  Spring.Container.Pool in '..\Source\Core\Container\Spring.Container.Pool.pas',
  Spring.Container.Registration in '..\Source\Core\Container\Spring.Container.Registration.pas',
  Spring.Container.Resolvers in '..\Source\Core\Container\Spring.Container.Resolvers.pas',
  Spring.Container.ResourceStrings in '..\Source\Core\Container\Spring.Container.ResourceStrings.pas',
  Spring.Services in '..\Source\Core\Services\Spring.Services.pas',
  Spring.Services.Logging in '..\Source\Core\Services\Spring.Services.Logging.pas',
  Spring.Cryptography.Base in '..\Source\Extensions\Cryptography\Spring.Cryptography.Base.pas',
  Spring.Cryptography.CRC in '..\Source\Extensions\Cryptography\Spring.Cryptography.CRC.pas',
  Spring.Cryptography.DES in '..\Source\Extensions\Cryptography\Spring.Cryptography.DES.pas',
  Spring.Cryptography.MD5 in '..\Source\Extensions\Cryptography\Spring.Cryptography.MD5.pas',
  Spring.Cryptography in '..\Source\Extensions\Cryptography\Spring.Cryptography.pas',
  Spring.Cryptography.SHA in '..\Source\Extensions\Cryptography\Spring.Cryptography.SHA.pas',
  Spring.Cryptography.Utils in '..\Source\Extensions\Cryptography\Spring.Cryptography.Utils.pas',
  Spring.Utils.IO in '..\Source\Extensions\Utils\Spring.Utils.IO.pas',
{$ifdef MSWINDOWS}
  Spring.Utils.WinApi in '..\Source\Extensions\Utils\Spring.Utils.WinApi.pas',
{$endif MSWINDOWS}
  Spring.Utils in '..\Source\Extensions\Utils\Spring.Utils.pas';

begin
end.
