{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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

{$I Spring.inc}

unit Spring.Logging.Controller;

interface

uses
  TypInfo,
  Spring.Collections,
  Spring.Logging,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Extensions;

type
  {$REGION 'TLoggerController'}

  TLoggerController = class(TLogAppenderBase, ILoggerController, ISerializerController)
  private
    fSerializers: IList<ITypeSerializer>;
    fStackTraceCollector: IStackTraceCollector;
    fStackTraceFormatter: IStackTraceFormatter;
    fAppenders: IList<ILogAppender>;
  protected
    procedure DoSend(const event: TLogEvent); override;

    procedure SendData(const event: TLogEvent);
    procedure SendStack(const event: TLogEvent);

    /// <summary>
    ///   Returns <c>true</c> if level is enabled and any of the <c>eventTypes</c>
    ///    is enabled in any of the appenders or <c>false</c> otherwise
    /// </summary>
    function IsLoggable(level: TLogLevel; eventTypes: TLogEventTypes): Boolean;
  public
    constructor Create; overload;
    constructor Create(const appenders: TArray<ILogAppender>); overload;
    constructor Create(const appenders: array of ILogAppender); overload;

    procedure AddAppender(const appender: ILogAppender);
    procedure AddSerializer(const serializer: ITypeSerializer);

    function FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;

    property StackTraceCollector: IStackTraceCollector read fStackTraceCollector
      write fStackTraceCollector;
    property StackTraceFormatter: IStackTraceFormatter read fStackTraceFormatter
      write fStackTraceFormatter;
  end;

  {$ENDREGION}


implementation

uses
  Spring;


{$REGION 'TLoggerController'}

constructor TLoggerController.Create;
begin
  inherited Create;
  fAppenders := TCollections.CreateInterfaceList<ILogAppender>;
  fSerializers := TCollections.CreateInterfaceList<ITypeSerializer>;
end;

constructor TLoggerController.Create(const appenders: TArray<ILogAppender>);
begin
  Create;
  fAppenders.AddRange(appenders);
end;

constructor TLoggerController.Create(const appenders: array of ILogAppender);
begin
  Create;
  fAppenders.AddRange(appenders);
end;

procedure TLoggerController.AddAppender(const appender: ILogAppender);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(appender, 'appender');
{$ENDIF}
  fAppenders.Add(appender);
end;

procedure TLoggerController.AddSerializer(const serializer: ITypeSerializer);
begin
  fSerializers.Add(serializer);
end;

procedure TLoggerController.DoSend(const event: TLogEvent);
var
  appender: ILogAppender;
begin
  // After serialization or stack logging is added, and if such action is
  // required (we have a serializer capable of serializing given data or
  // AddStack and StackCollector) log the message first then go though all
  // appenders first and get their level and enabled state to check if there is
  // something to do in the first place
  for appender in fAppenders do
    appender.Send(event);

  if not event.Data.IsEmpty
    and IsLoggable(event.Level, [TLogEventType.SerializedData]) then
      SendData(event);

  if event.AddStackValue and Assigned(fStackTraceCollector)
    and Assigned(fStackTraceFormatter)
    and IsLoggable(event.Level, [TLogEventType.CallStack]) then
      SendStack(event);
end;

function TLoggerController.FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
var
  serializer: ITypeSerializer;
begin
  for serializer in fSerializers do
    if serializer.HandlesType(typeInfo) then
      Exit(serializer);

  Result := nil;
end;

function TLoggerController.IsLoggable(level: TLogLevel;
  eventTypes: TLogEventTypes): Boolean;
var
  appender: ILogAppender;
begin
  for appender in fAppenders do
    if appender.Enabled and (level in appender.Levels)
      and (eventTypes * appender.EventTypes <> []) then
        Exit(True);

  Result := False;
end;

procedure TLoggerController.SendData(const event: TLogEvent);
var
  serializer: ITypeSerializer;
begin
  serializer := FindSerializer(event.Data.TypeInfo);

  if Assigned(serializer) then
    DoSend(TLogEvent.Create(event.Level, TLogEventType.SerializedData,
      serializer.Serialize(Self, event.Data)));
end;

procedure TLoggerController.SendStack(const event: TLogEvent);
var
  stack: TArray<Pointer>;
  formatted: TArray<string>;
  i: Integer;
  s: string;
begin
  stack := fStackTraceCollector.Collect;
  if Length(stack) = 0 then
    Exit;

  formatted := fStackTraceFormatter.Format(stack);

  s := formatted[0];
  for i := 1 to High(formatted) do
    s := s + sLineBreak + formatted[i];

  DoSend(TLogEvent.Create(event.Level, TLogEventType.CallStack, s));
end;

{$ENDREGION}


end.
