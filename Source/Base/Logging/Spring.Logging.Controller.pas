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

unit Spring.Logging.Controller;

{$I Spring.inc}

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
    fStackTraceFromatter: IStackTraceFormatter;
    fAppenders: IList<ILogAppender>;
  protected
    procedure DoSend(const entry: TLogEntry); override;

    procedure SendData(const entry: TLogEntry);
    procedure SendStack(const entry: TLogEntry);

    function IsLoggable(level: TLogLevel): Boolean;
  public
    constructor Create;

    procedure AddAppender(const appender: ILogAppender);
    procedure AddSerializer(const serializer: ITypeSerializer);

    function FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
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

procedure TLoggerController.AddSerializer(const serializer: ITypeSerializer);
begin
  if (fSerializers = nil) then
    fSerializers := TCollections.CreateInterfaceList<ITypeSerializer>;
  fSerializers.Add(serializer);
end;

constructor TLoggerController.Create;
begin
  inherited;

  fAppenders := TCollections.CreateInterfaceList<ILogAppender>;
end;

procedure TLoggerController.DoSend(const entry: TLogEntry);
var
  appender: ILogAppender;
begin
  //After serialization or stack logging is added, and if such action is required
  //(we have a serializer capable of serializing given data or AddStack and
  //StackCollector) log the message first then go though all appenders first
  //and get their level and enabled state to check if there is something to
  //do in the first place
  for appender in fAppenders do
    appender.Send(entry);

  if not entry.Data.IsEmpty and IsLoggable(TLogLevel.SerializedData) then
    SendData(entry);

  if entry.AddStackValue and (fStackTraceCollector <> nil) and
    (fStackTraceFromatter <> nil) and IsLoggable(TLogLevel.CallStack) then
      SendStack(entry);
end;

function TLoggerController.FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
var
  serializer: ITypeSerializer;
begin
  if (fSerializers <> nil) then
    for serializer in fSerializers do
      if (serializer.HandlesType(typeInfo)) then
        Exit(serializer);

  Result := nil;
end;

function TLoggerController.IsLoggable(level: TLogLevel): Boolean;
var appender: ILogAppender;
begin
  for appender in fAppenders do
    if (appender.Enabled and (level in appender.Levels)) then
      Exit(true);

  Result := false;
end;

procedure TLoggerController.SendData(const entry: TLogEntry);
var
  serializer: ITypeSerializer;
begin
  serializer := FindSerializer(entry.Data.TypeInfo);

  if serializer <> nil then
    DoSend(TLogEntry.Create(TLogLevel.SerializedData,
      serializer.Serialize(Self, entry.Data)));
end;

procedure TLoggerController.SendStack(const entry: TLogEntry);
var
  stack: TArray<Pointer>;
  formatted: TStringDynArray;
  i: Integer;
  s: string;
begin
  stack := fStackTraceCollector.Collect;
  formatted := fStackTraceFromatter.Format(stack);

  if Length(stack) < 1 then
    Exit;

  s := formatted[0];
  for i := 1 to High(formatted) do
    s := s + #$A + formatted[i];
end;
{$ENDREGION}

end.
