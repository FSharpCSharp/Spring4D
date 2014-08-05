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

unit Spring.Tests.Logging.Types;

{$I Spring.inc}

interface

uses
  Classes,
  TestFramework,
  Spring,
  Spring.Logging,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Controller,
  Spring.Logging.Loggers,
  Spring.Container.Common;

type
  TAppenderMock = class(TLogAppenderBase)
  private
    fWriteCalled: Boolean;
    fEntry: TLogEntry;
    fSomeFloat: Extended;
    fSomeInt: Integer;
    fSomeEnum: TLogLevel;
    fSomeString: string;
  protected
    procedure DoSend(const entry: TLogEntry); override;
  public
    property Entry: TLogEntry read fEntry;
    property WriteCalled: Boolean read fWriteCalled;
    property SomeFloat: Extended read fSomeFloat write fSomeFloat;
    property SomeInt: Integer read fSomeInt write fSomeInt;
    property SomeEnum: TLogLevel read fSomeEnum write fSomeEnum;
    property SomeString: string read fSomeString write fSomeString;
  end;

  TAppenderMock2 = class(TLogAppenderBase)
  protected
    procedure DoSend(const entry: TLogEntry); override;
  end;

  TLoggerControllerMock = class(TInterfacedObject, ILoggerController, ILogAppender)
  private
    fLastEntry: TLogEntry;
  public
    procedure AddAppender(const appedner: ILogAppender);
    procedure Send(const entry: TLogEntry);
    procedure Reset;
    property LastEntry: TLogEntry read FLastEntry;
  end;

  TLoggerController2 = class(TLoggerController);

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
    [Inject('logging.logger2')]
    fLogger2: ILogger;
  public
    property Logger1: ILogger read fLogger1;
    property Logger2: ILogger read fLogger2;
  end;

  TObjProc = class
  private
    fLogger: ILogger;
  public
    [Inject]
    procedure SetLoggger(const logger: ILogger);
    property Logger: ILogger read fLogger;
  end;

  TObjCtor = class
  private
    fLogger: ILogger;
  public
    constructor Create(const logger: ILogger);
    property Logger: ILogger read fLogger;
  end;

  TObjLazy = class
  private
    [Inject]
    fLogger1: Lazy<ILogger>;
    [Inject('logging.logger2')]
    fLogger2: Lazy<ILogger>;
  public
    property Logger1: Lazy<ILogger> read fLogger1;
    property Logger2: Lazy<ILogger> read fLogger2;
  end;

  TSomeRecord = record

  end;

implementation

{ TAppenderMock }

procedure TAppenderMock.DoSend(const entry: TLogEntry);
begin
  fWriteCalled := true;
  fEntry := entry;
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

{ TAppenderMock2 }

procedure TAppenderMock2.DoSend(const entry: TLogEntry);
begin

end;

end.
