{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

{ This project is used for personal test purpose. }

program Console;

{$APPTYPE CONSOLE}

{$WEAKLINKRTTI OFF}

uses
  FastMM4,
  Classes,
  Controls,
  Windows,
  IOUtils,
  SysUtils,
  Rtti,
  TypInfo,
  SyncObjs,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.IoC;

(*
type
  IEmailSender = interface
  //...
  end;

  ILogger = interface
  //...
  end;

  INetworkObservable = interface
  //...
  end;

//  [Singleton]
//  [Singleton]
//  [Implements(TypeInfo(IEmailSender), 'network.emailsender']
//  [Implements(TypeInfo(INetworkObservable), 'network.observable']

  TLogger = class(TInterfacedObject, ILogger)
  //...
  end;
  
  {$STRONGLINKTYPES ON} 
  TNetworkService = class(TInterfacedObject, IEmailSender, INetworkObservable)
  private
    fLogger: ILogger;
  public
    // [Injection]
    constructor Create(logger: ILogger); overload;
    constructor Create(logger: ILogger; port: Integer); overload;
  //...
  end;

  TUserRegistrationForm = class(TForm)
  private
    // [Injection]
    fEmailSender: IEmailSender;
  //...
  end;

//*)

var
  aType: TRttiType;
//  container: TContainer;

begin
  try
    with TRttiContext.Create do
    begin
      aType := FindType('Spring.IoC.TContainer');
      if aType <> nil then
      begin
        Writeln(aType.ToString);
      end;
      aType := FindType('TContainer');
      if aType <> nil then
      begin
        Writeln(aType.ToString);
      end;
    end;
//    // case #1
//    container.RegisterComponent<TLogger>;           // Implements ILogger
//    container.RegisterComponent<TNetworkService>;   // Implements IEmailSender and INetworkObservable
//    form := container.Resolve<TUserRegistrationForm>(Application);  // Bootstrap

//    // case #2
//    container.RegisterComponent<TLogger>;           
//    container.RegisterComponent<TNetworkService>;
//      .DelegateTo(
//        function: TNetworkService
//        begin
//          Result := TNetworkService.Create(
//            container.Resolve<ILogger>,
//            80
//          );
//          Result.SomeProperty := SomeValue;
//          Result.Start;
//        end
//      );
//    form := container.Resolve<TUserRegistrationForm>(Application);  // Bootstrap


//    // case #3
//    container.RegisterComponent<TNetworkService>
//      .Implements<IEmailSender>('network.emailsender')
//      .Implements<INetworkObservable>('network.observable')
//      .DelegateTo(
//        function: TNetworkService
//        begin
//          Result := TNetworkService.Create(
//            container.Resolve<ILogger>,
//            80
//          );
//          Result.SomeProperty := SomeValue;
//          Result.Start;
//        end
//      )
//      .AsSingleton;
//    form := container.Resolve<TUserRegistrationForm>(Application);  // Bootstrap

//    // case #4
//    container.RegisterComponent<TNetworkService>
//      .Implements<IEmailSender>('network.emailsender')
//      .Implements<INetworkObservable>('network.observable')
//      .InjectConstructor(['${logger}', 80]);
//      .InjectProperty('propertyName', '${network}')
//      .InjectMethod('methodName', ['arg1', arg2, arg3])
//      .InjectField('fieldName', 'fieldValue')
//      .AsSingleton;
//    form := container.Resolve<TUserRegistrationForm>(Application);  // Bootstrap

//    // register decorations
//    container.RegisterDecorations<IEmailSender>([TSecurity, TValidation, TSomethingElse]);
//    container.RegisterDecorations<IEmailSender>('network.emailsender', [TSecurity, TValidation, TSomethingElse]);
//
//
//    form := container.Resolve<TUserRegistrationForm>(
//      ???
//    );
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
