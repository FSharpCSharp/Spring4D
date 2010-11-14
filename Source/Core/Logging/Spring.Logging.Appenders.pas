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
  Messages,
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
    procedure SetThreshold(const value: TLevel);
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

  { NEED REVIEW }
  TFileAppender = class(TAppenderBase)
  private
    fFilename: string;
    fEncoding: TEncoding;
    fStream: TStream;
    fEncodings: TStrings;
    function GetEncoding(const name: string): TEncoding;
    function GetStream: TStream;
    function CreateFileStream: TStream;
    procedure SetLogfile(const value: string);
    procedure DestroyFileStream;
    procedure InitializeEncodings;
    procedure RegistEncoding(const name: string; const encoding: TEncoding);
  protected
    procedure DoConfigure(const configuration: IConfigurationNode); override;
    procedure DoClose; override;
    procedure DoAppend(const event: TLoggingEvent); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Filename: string read fFilename write SetLogfile;
  end;


  TColoredConsoleAppender = class(TAppenderBase)
  private type
    TConsoleColor = (
        ccWhite, ccBlack, ccNavy, ccMaroon, ccGreen, ccPurple, ccTeal, ccOlive,
        ccHighlight, ccGray, ccBlue, ccRed, ccLime, ccFuchsia, ccAqua, ccYellow);
  private const
    TConsoleColorValues : array [TConsoleColor] of Word =
    (
      FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED,
      0 or FOREGROUND_INTENSITY,
      FOREGROUND_BLUE,
      FOREGROUND_RED,
      FOREGROUND_GREEN,
      FOREGROUND_BLUE or FOREGROUND_RED,
      FOREGROUND_BLUE or FOREGROUND_GREEN,
      FOREGROUND_RED or FOREGROUND_GREEN,
      // high light
      FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY,
      0 or FOREGROUND_INTENSITY,
      FOREGROUND_BLUE or FOREGROUND_INTENSITY,
      FOREGROUND_RED or FOREGROUND_INTENSITY,
      FOREGROUND_GREEN or FOREGROUND_INTENSITY,
      FOREGROUND_BLUE or FOREGROUND_RED or FOREGROUND_INTENSITY,
      FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY,
      FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY
    );

  private
    fConsoleHandle: THandle;
    fColorAttribute: Word;
    fColorMapping: TStrings;
    function GetMappingColor(const level: TLevel): Word;
    procedure RegisterColor(const levelname: string; const color: TConsoleColor);
    procedure InitializeColorMapping;
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  public
    constructor Create();
    destructor Destroy; override;
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
    InternalLogger.Debug('TAppenderBase: Destroying appender named ['+fName+'].');
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
      InternalLogger.Error('Failed to append the event.', e);
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
  TryGetAttributeValue(configuration, 'name', fName);

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
      InternalLogger.Error('The Layout child element was expected but there is none.');
      Exit;
    end;

    if not TryGetAttributeValue(node, 'type', layoutType) then
    begin
      Exit;
    end;

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

procedure TAppenderBase.SetThreshold(const value: TLevel);
begin
  if value <> nil then
  begin
    fThreshold := Value;
  end
  else
  begin
    fThreshold := TLevel.All;
    InternalLogger.Error('Threashold should not be set as null. Setting to TLevel.All.');
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
{$REGION 'TFileAppender'}
constructor TFileAppender.Create;
begin
  inherited Create;
  fEncodings := TStringList.Create;
  fEncoding := TEncoding.Default;
  InitializeEncodings;
end;

destructor TFileAppender.Destroy;
begin
  fEncodings.Free;
  DestroyFileStream;
  inherited;
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

procedure TFileAppender.DestroyFileStream;
begin
  FreeAndNil(fStream);
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
    instance := GetStream;
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
    Filename := node.Attributes['value']
  else
    Filename := '';

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

function TFileAppender.GetStream: TStream;
begin
  if fStream = nil then
  begin
    fStream := CreateFileStream;
  end;
  Result := fStream;
end;

procedure TFileAppender.InitializeEncodings;
begin
  RegistEncoding('utf-7',TEncoding.UTF7);
  RegistEncoding('utf7',TEncoding.UTF7);
  RegistEncoding('utf-8', TEncoding.UTF8);
  RegistEncoding('utf8', TEncoding.UTF8);
  RegistEncoding('default', TEncoding.Default);
  RegistEncoding('unicode', TEncoding.Unicode);
  RegistEncoding('utf16', TEncoding.Unicode);
  RegistEncoding('utf-16', TEncoding.Unicode);
end;

procedure TFileAppender.RegistEncoding(const name: string; const encoding: TEncoding);
begin
  fEncodings.AddObject(name, TObject(encoding));
end;

procedure TFileAppender.SetLogfile(const value: string);
begin
  if value <> fFilename then
  begin
    Lock;
    try
      if fFilename <> value then
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
{$ENDREGION}

{ TColorConsoleAppender }

constructor TColoredConsoleAppender.Create;
begin
  inherited Create;
  fColorMapping := TStringList.Create;
  InitializeColorMapping;
end;

destructor TColoredConsoleAppender.Destroy;
begin
  fColorMapping.Free;
  inherited;
end;

procedure TColoredConsoleAppender.DoAppend(const event: TLoggingEvent);
var
  hConsole: THandle;
  bakBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
  curBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
  loggingMessage: string;
  outputedCharacterCount: Cardinal;
begin
  hConsole := Windows.GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE then
    raise Exception.Create('Console is unaccessable');

  // Backup the original console screen buffer info
  Windows.GetConsoleScreenBufferInfo(hConsole, bakBufferInfo);

  // Set console text attribute
  Windows.SetConsoleTextAttribute(hConsole, GetMappingColor(event.LoggerLevel));

  // Output the logging message
  loggingMessage := Format(Event);
  Windows.WriteConsole(hConsole, PChar(loggingMessage), Length(loggingMessage), outputedCharacterCount, nil);

  // Restore the original console text attribute
  Windows.SetConsoleTextAttribute(hConsole, bakBufferInfo.wAttributes);

  // to do : Make the detail description for the following code snips
  Windows.GetConsoleScreenBufferInfo(hConsole, curBufferInfo);
  if (curBufferInfo.dwCursorPosition.y >= (curBufferInfo.dwSize.Y - 1)) then
  begin
    Windows.FillConsoleOutputAttribute(
        hConsole,
        bakBufferInfo.wAttributes,
        curBufferInfo.dwSize.X - curBufferInfo.dwCursorPosition.X,
        curBufferInfo.dwCursorPosition,
        outputedCharacterCount);
  end;
end;


function TColoredConsoleAppender.GetMappingColor(const level: TLevel): Word;
var
  index: Integer;
begin
  index:= fColorMapping.IndexOf(level.Name);
  if index >= 0 then
    Result := Word(fColorMapping.Objects[index])
  else
    Result := TConsoleColorValues[ccWhite];
end;

procedure TColoredConsoleAppender.InitializeColorMapping;
begin
  fColorMapping.Clear;
  RegisterColor(TLevel.Error.Name, ccRed);
  RegisterColor(TLevel.Info.Name, ccLime);
  RegisterColor(TLevel.Warn.Name, ccYellow);
  RegisterColor(TLevel.Debug.Name, ccHighlight);
  RegisterColor(TLevel.Fatal.Name, ccFuchsia);

end;

procedure TColoredConsoleAppender.RegisterColor(const levelname: string;
  const color: TConsoleColor);
begin
  fColorMapping.AddObject(levelname, TObject(TConsoleColorValues[color]));
end;

end.


