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

unit Spring.Extensions.Logging.Controller;

interface

uses
  Spring.Collections,
  Spring.Extensions.Logging;

type
  {$REGION 'TLoggerController'}
  TLoggerController = class(TInterfacedObject, ILoggerController)
  private
    //fSerializers: IDictionary<TTypeKind, ITypeSerializer>;
    //fStackTraceCollector: IStackTraceCollector;
    //fStackTraceFromatter: IStackTraceFormatter;
    fAppenders: IList<ILogAppender>;
  public
    constructor Create;

    procedure Send(const entry: TLogEntry);
    procedure AddAppender(const appender: ILogAppender);
  end;
  {$ENDREGION}

implementation

uses
  Spring;

{$REGION 'TLoggerController'}
{ TLoggerController }

procedure TLoggerController.AddAppender(const appender: ILogAppender);
begin
  Guard.CheckNotNull(appender, 'appender');
  fAppenders.Add(appender);
end;

constructor TLoggerController.Create;
begin
  inherited;

  fAppenders := TCollections.CreateInterfaceList<ILogAppender>;
end;

procedure TLoggerController.Send(const entry: TLogEntry);
var appender: ILogAppender;
begin
  //After serialization or stack logging is added, and if such action is required
  //(we have a serializer capable of serializing given data or AddStack and
  //StackCollector) log the message first then go though all appenders first
  //and get their level and enabled state to check if there is something to
  //do in the first place
  for appender in fAppenders do
    appender.Write(entry);
end;
{$ENDREGION}

end.
