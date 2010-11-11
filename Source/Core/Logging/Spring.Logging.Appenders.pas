{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Logging.Appenders;

interface

uses
  Classes,
  SysUtils,
  Spring,
  windows,
  Spring.Collections,
  Spring.Configuration,
  Spring.Logging.Core;

type
  TAppenderBase = class abstract(TInterfacedObject, IBulkAppender, IAppender, IConfigurable)
  private
    fName: string;
    fThreshold: TLevel;
    fClosed: Boolean;
    fRecursiveGuard: Boolean;
    fLayout: ILayout;
    function GetName: string;
    procedure SetLayout(const Value: ILayout);
  protected
    procedure Lock;
    procedure Unlock;
  protected
    procedure DoAppend(const event: TLoggingEvent); virtual;
    procedure DoClose; virtual;
    procedure CheckLayout;
    function Format(const event: TLoggingEvent): string; virtual;
    function AcceptEvent(const event: TLoggingEvent): Boolean; virtual;
    function CanAppend: Boolean;
  protected
    { IConfigurable }
    procedure Configure(const configuration: IConfigurationNode);
  public
    constructor Create(const name: string);
    destructor Destroy; override;
    { IBulkAppender }
    procedure Append(const events: ICollection<TLoggingEvent>); overload; virtual;
    { IAppender }
    procedure Close;
    procedure Append(const event: TLoggingEvent); overload;
    property Name: string read GetName;
    property Layout: ILayout read fLayout write SetLayout;
  end;

  TConsoleAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

  TOutputDebugStringAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

//  TFileAppender = class(TTextWriterAppender)
//
//  end;

implementation

uses Spring.Logging.Utils;


{$REGION 'TAppenderBase'}

constructor TAppenderBase.Create;
begin
  inherited Create;
//  fErrorHandler := TOnlyOnceErrorHandler.Create(ClassName);
end;

destructor TAppenderBase.Destroy;
begin
  if not fClosed then
  begin
    TInternalLogger.Debug('TAppenderBase: Destroying appender named ['+fName+'].');
    Close;
  end;
  inherited Destroy;
end;

procedure TAppenderBase.Lock;
begin
  MonitorEnter(Self);
end;

procedure TAppenderBase.SetLayout(const Value: ILayout);
begin
  if fLayout <> Value then
    fLayout := Value;
end;

procedure TAppenderBase.Unlock;
begin
  MonitorExit(Self);
end;

procedure TAppenderBase.Close;
begin
  Lock;
  try
    if not fClosed then
    begin
      DoClose;
      fClosed := True;
    end;
  finally
    Unlock;
  end;
end;

procedure TAppenderBase.Configure(const configuration: IConfigurationNode);
begin
  fName := configuration.Attributes['name'];
end;

function TAppenderBase.AcceptEvent(const event: TLoggingEvent): Boolean;
begin
  Result := True;
end;

procedure TAppenderBase.Append(const event: TLoggingEvent);
begin
  Lock;
  try
    if fRecursiveGuard then Exit;
    fRecursiveGuard := True;
    try
      try
        if AcceptEvent(event) and CanAppend then
          DoAppend(event);
      except on e: Exception do
        // TODO: Handle internal exception
      end;
    finally
      fRecursiveGuard := False;
    end;
  finally
    Unlock;
  end;
end;

procedure TAppenderBase.Append(const events: ICollection<TLoggingEvent>);
var
  event: TLoggingEvent;
begin
  for event in events do
  begin
    Append(event);
  end;
end;

procedure TAppenderBase.DoAppend(const event: TLoggingEvent);
begin

end;

procedure TAppenderBase.DoClose;
begin
end;

function TAppenderBase.Format(const event: TLoggingEvent): string;
begin
  CheckLayout;
  result:= Layout.Format(event);
end;

function TAppenderBase.CanAppend: Boolean;
begin
  Result := True;
end;

procedure TAppenderBase.CheckLayout;
begin
  if Layout = nil then
    raise ELoggingException.CreateFMT('Appender[%s] needs a layout', [Self.ClassName]);
end;

function TAppenderBase.GetName: string;
begin
  Result := fName;
end;

{$ENDREGION}

{ TConsoleAppender }

procedure TConsoleAppender.DoAppend(const event: TLoggingEvent);
begin
  Write(Format(Event));
end;

{ TOutputDebugStringAppend }

procedure TOutputDebugStringAppender.DoAppend(const event: TLoggingEvent);
var
  outString: string;
begin
  outString := Format(event);
  Windows.OutputDebugString(PWidechar(outString));
end;

end.
