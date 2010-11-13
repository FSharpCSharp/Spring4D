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
  Windows,
  Ioutils,
  Spring,
  Spring.Collections,
  Spring.Configuration,
  Spring.Logging.Core;

type
  TAppenderBase = class abstract(TInterfacedObject, IBulkAppender, IAppender,
    IConfigurable, ILoggerRepositoryInit)
  private
    fName: string;
    fThreshold: TLevel;
    fClosed: Boolean;
    fLayout: ILayout;
    fRepository: ILoggerRepository;
    function GetName: string;
    procedure SetName(const value: string);
    procedure SetLayout(const value: ILayout);
    procedure SetThreshold(const Value: TLevel);
  protected
    procedure Lock;
    procedure Unlock;
  protected
    { ILoggerRepositoryInit }
    procedure InitializeRepository(const repository: ILoggerRepository);
    { IConfigurable }
    procedure Configure(const configuration: IConfigurationNode);
  protected
    procedure DoAppend(const event: TLoggingEvent); virtual;
    procedure DoClose; virtual;
    procedure DoConfigure(const configuration: IConfigurationNode); virtual;
    function AcceptEvent(const event: TLoggingEvent): Boolean; virtual;
    function CanAppend: Boolean; virtual;
    function RequireLayout: Boolean; virtual;
    function Format(const event: TLoggingEvent): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IBulkAppender }
    procedure Append(const events: IEnumerable<TLoggingEvent>); overload; virtual;
    { IAppender }
    procedure Close;
    procedure Append(const event: TLoggingEvent); overload;
    property Name: string read GetName write SetName;
    property Layout: ILayout read fLayout write SetLayout;
    property Threshold: TLevel read fThreshold write SetThreshold;
  end;

  TConsoleAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

  TOutputDebugStringAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

  TFileAppender = class(TAppenderBase)
  private
    fFilename: string;
    fEncoding: TEncoding;
    fInstance: TStream;
    fEncodings: TStrings;
    procedure SetLogfile(const Value: string);
    procedure DestroyFileStream;
    procedure InitializeEncodings;
    procedure RegistEncoding(const name: string; const encoding: TEncoding);
    function GetEncoding(const name: string): TEncoding;
    function GetInstance: TStream;
    function CreateFileStream: TStream;
  protected
    procedure DoConfigure(const configuration: IConfigurationNode); override;
    procedure DoClose; override;
    procedure DoAppend(const event: TLoggingEvent); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Filename: string read fFilename write SetLogfile;
  end;


  TColorConsoleAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;




implementation

uses Spring.Logging.Utils;


{$REGION 'TAppenderBase'}

constructor TAppenderBase.Create;
begin
  inherited Create;
  fThreshold := TLevel.All;
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
  TArgument.CheckNotNull(configuration, 'configuration');
  Lock;
  try
    DoConfigure(configuration);
  finally
    Unlock;
  end;
end;

function TAppenderBase.AcceptEvent(const event: TLoggingEvent): Boolean;
begin
  Result := (event.LoggerLevel <> nil) and
    event.LoggerLevel.IsGreaterThanOrEqualTo(Threshold);
end;

procedure TAppenderBase.Append(const event: TLoggingEvent);
begin
  Lock;
  try
    try
      if CanAppend and AcceptEvent(event) then
        DoAppend(event);
    except on e: Exception do
      // TODO: Internal log
    end;
  finally
    Unlock;
  end;
end;

procedure TAppenderBase.Append(const events: IEnumerable<TLoggingEvent>);
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

procedure TAppenderBase.DoConfigure(const configuration: IConfigurationNode);
var
  node: IConfigurationNode;
  layoutType: string;
  level: TLevel;
begin
  fName := configuration.Attributes['name'];
  node := configuration.FindNode('threshold');
  if node <> nil then
  begin
    level := fRepository.FindLevel(node.Attributes['value']);
    SetThreshold(level);
  end;

  if RequireLayout then
  begin
    node := configuration.FindNode('layout');
    if node = nil then
    begin
      // TODO: Handle internal exception.
      Exit;
    end;
    layoutType := node.Attributes['type'];
    fLayout := fRepository.CreateLayout(layoutType);
    TryConfigure(fLayout, node);
  end;
end;

function TAppenderBase.Format(const event: TLoggingEvent): string;
begin
  Assert(Layout <> nil, 'Layout should not be nil.');
  Result:= Layout.Format(event);
end;

function TAppenderBase.CanAppend: Boolean;
begin
  Result := not RequireLayout or (Layout <> nil);
end;

function TAppenderBase.RequireLayout: Boolean;
begin
  Result := True;
end;

function TAppenderBase.GetName: string;
begin
  Result := fName;
end;

procedure TAppenderBase.InitializeRepository(
  const repository: ILoggerRepository);
begin
  fRepository := repository;
end;

procedure TAppenderBase.SetName(const value: string);
begin
  fName := name;
end;

procedure TAppenderBase.SetThreshold(const Value: TLevel);
begin
  if value <> nil then
  begin
    fThreshold := Value;
  end
  else
  begin
    fThreshold := TLevel.All;
    // TODO: Internal log
  end;
end;

procedure TAppenderBase.SetLayout(const value: ILayout);
begin
  if fLayout <> value then
    fLayout := value;
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

{ TFileAppender }

constructor TFileAppender.Create;
begin
  inherited Create;
  fEncodings := TStringList.Create;
  InitializeEncodings;
end;

function TFileAppender.CreateFileStream: TStream;
var
  filePath: string;
  fileOpenMode: Word;
  preamble: TBytes;
begin
  fileOpenMode := fmOpenReadWrite or fmShareDenyWrite;
  if not FileExists(fFileName) then
  begin
    filePath := ExtractFileDir(fFileName);
    if not ForceDirectories(filePath) then
      raise Exception.CreateFmt('Logfile path [%s] is invalid or unaccessable', [filePath]);
    fileOpenMode := fmCreate or fmShareDenyWrite;
  end;

  // open file
  Result := TFileStream.Create(fFileName, fileOpenMode);
  Result.Seek(0, soFromEnd);

  // initialize new logfile encoding BOM
  if Result.Size = 0 then
  begin
    preamble := fEncoding.GetPreamble;
    Result.Write(preamble[0], Length(preamble));
  end;
end;

destructor TFileAppender.Destroy;
begin
  fEncodings.Free;
  inherited;
end;

procedure TFileAppender.DestroyFileStream;
begin
  FreeAndNil(fInstance);
end;

procedure TFileAppender.DoAppend(const event: TLoggingEvent);
var
  instance: TStream;
  msg: string;
  buffer: TBytes;
begin
  msg := Format(event);
  if msg <> '' then
  begin
    // Instance in here needn't to be protected,as the lock protection has been used
    // in append procedure which in ancestor class
    Assert(fEncoding <> nil, 'Encoding is nil');
    buffer := fEncoding.GetBytes(msg);
    instance := GetInstance;
    instance.Write(buffer[0], Length(buffer));
  end;
end;

procedure TFileAppender.DoClose;
begin
  DestroyFileStream;
end;

procedure TFileAppender.DoConfigure(const configuration: IConfigurationNode);
var
  encodingName : string;
  node : IConfigurationNode;
begin
  inherited;
  node := configuration.FindNode('encoding');
  if node <> nil then
    encodingName := node.Attributes['value']
  else
    encodingName := '';

  node := configuration.FindNode('filename');
  if node <> nil then
    filename := node.Attributes['value']
  else
    filename := '';

  fEncoding := GetEncoding(encodingname);
end;

function TFileAppender.GetEncoding(const name: string): TEncoding;
var
  index: integer;
begin
  Result := TEncoding.Default;
  index := fEncodings.IndexOf(name);
  if index >=0 then
    result := TEncoding(fEncodings.Objects[index]);
end;

function TFileAppender.GetInstance: TStream;
begin
  if fInstance = nil then
  begin
    fInstance := CreateFileStream;
  end;
  Result := fInstance;
end;

procedure TFileAppender.InitializeEncodings;
begin
  RegistEncoding('utf-7',TEncoding.UTF7);
  RegistEncoding('utf7',TEncoding.UTF7);
  RegistEncoding('utf-8', TEncoding.UTF8);
  RegistEncoding('utf', TEncoding.UTF8);
  RegistEncoding('default', TEncoding.Default);
  RegistEncoding('unicode', TEncoding.Unicode);
  RegistEncoding('utf16', TEncoding.Unicode);
  RegistEncoding('utf-16', TEncoding.Unicode);
end;

procedure TFileAppender.RegistEncoding(const name: string; const encoding: TEncoding);
begin
  fEncodings.AddObject(name, TObject(encoding));
end;

procedure TFileAppender.SetLogfile(const Value: string);
begin
  if Value <> fFilename then
  begin
    Lock;
    try
      if fFilename <> Value then
      begin
        fFilename := value;
        if fFilename <> '' then
          fFilename:= TPath.GetFullPath(fFilename);

        DestroyFileStream;
      end;
    finally
      Unlock;
    end;
  end;
end;

{ TColorConsoleAppender }

procedure TColorConsoleAppender.DoAppend(const event: TLoggingEvent);
var
  handle: THandle;
begin
  handle := Windows.GetStdHandle(STD_OUTPUT_HANDLE);
  //inherited;
  windows.SetConsoleTextAttribute(handle, 100);
  Write(format(event));
end;

end.


