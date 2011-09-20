{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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
  Spring,
  Spring.Collections,
  Spring.Services.Logging,
  Spring.Logging.Core;

type
  TAppenderBase = class abstract(TInterfacedObject, IAppender)
  private
    fName: string;
    fThreshold: TLevel;
    fClosed: Boolean;
    fLayout: ILayout;
    function GetName: string;
    procedure SetName(const value: string);
    procedure SetLayout(const value: ILayout);
    procedure SetThreshold(const value: TLevel);
  protected
    procedure Lock;
    procedure Unlock;
  protected
    procedure DoAppend(const event: TLoggingEvent); virtual;
    procedure DoClose; virtual;
    function AcceptEvent(const event: TLoggingEvent): Boolean; virtual;
    function CanAppend: Boolean; virtual;
    function RequireLayout: Boolean; virtual;
    function Format(const event: TLoggingEvent): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    procedure Append(const event: TLoggingEvent); overload;
    procedure Append(const events: IEnumerable<TLoggingEvent>); overload; virtual;
    property Name: string read GetName write SetName;
    property Layout: ILayout read fLayout write SetLayout;
    property Threshold: TLevel read fThreshold write SetThreshold;
  end;

  TConsoleAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

  TColoredConsoleAppender = class(TAppenderBase)
  private
    type
      TConsoleColor = (
        ccWhite, ccBlack, ccNavy, ccMaroon, ccGreen, ccPurple, ccTeal, ccOlive,
        ccHighlight, ccGray, ccBlue, ccRed, ccLime, ccFuchsia, ccAqua, ccYellow
      );
    const
      CConsoleColorValues : array [TConsoleColor] of Word =
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
//    fConsoleHandle: THandle;
//    fColorAttribute: Word;
    fColorMappings: TStrings;
    function GetMappingColor(const level: TLevel): Word;
    procedure RegisterColor(const levelname: string; const color: TConsoleColor);
    procedure InitializeColorMappings;
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TOutputDebugStringAppender = class(TAppenderBase)
  protected
    procedure DoAppend(const event: TLoggingEvent); override;
  end;

  TFileAppender = class(TAppenderBase)
  private
    fFileName: string;
    fEncoding: TEncoding;
    fFileStream: TStream;
    function GetFileStream: TStream;
    function GetEncoding: TEncoding;
    procedure SetEncoding(const value: TEncoding);
    procedure SetFileName(const value: string);
  protected
    function CreateFileStream: TStream;
    procedure DoClose; override;
    procedure DoAppend(const event: TLoggingEvent); override;
  public
    constructor Create;
    property FileName: string read fFileName write SetFileName;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
  end;

implementation

uses
  IOUtils,
  Spring.Logging.Utils;


{$REGION 'TAppenderBase'}

constructor TAppenderBase.Create;
begin
  inherited Create;
  fThreshold := TLevel.All;
end;

destructor TAppenderBase.Destroy;
begin
  if not fClosed then
  begin
    InternalLogger.Debug('TAppenderBase: Destroying appender named [' + Name + '].');
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
  fLayout := value;
end;

{$ENDREGION}


{$REGION 'Console Appenders'}

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

{$ENDREGION}


{$REGION 'TColoredConsoleAppender'}

constructor TColoredConsoleAppender.Create;
begin
  inherited Create;
  fColorMappings := TStringList.Create;
  InitializeColorMappings;
end;

destructor TColoredConsoleAppender.Destroy;
begin
  fColorMappings.Free;
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
  index:= fColorMappings.IndexOf(level.Name);
  if index >= 0 then
    Result := Word(fColorMappings.Objects[index])
  else
    Result := CConsoleColorValues[ccWhite];
end;

procedure TColoredConsoleAppender.InitializeColorMappings;
begin
  fColorMappings.Clear;
  RegisterColor(TLevel.Error.Name, ccRed);
  RegisterColor(TLevel.Info.Name, ccLime);
  RegisterColor(TLevel.Warn.Name, ccYellow);
  RegisterColor(TLevel.Debug.Name, ccHighlight);
  RegisterColor(TLevel.Fatal.Name, ccFuchsia);
end;

procedure TColoredConsoleAppender.RegisterColor(const levelname: string;
  const color: TConsoleColor);
begin
  fColorMappings.AddObject(levelname, TObject(CConsoleColorValues[color]));
end;

{$ENDREGION}


{$REGION 'TFileAppender'}

constructor TFileAppender.Create;
begin
  inherited Create;
  fEncoding := TEncoding.UTF8;
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
    preamble := Encoding.GetPreamble;
    Result.Write(preamble[0], Length(preamble));
  end;
end;

procedure TFileAppender.DoAppend(const event: TLoggingEvent);
var
  msg: string;
  buffer: TBytes;
begin
  msg := Format(event);
  if msg <> '' then
  begin
    buffer := Encoding.GetBytes(msg);
    GetFileStream.Write(buffer[0], Length(buffer));
  end;
end;

procedure TFileAppender.DoClose;
begin
  FreeAndNil(fFileStream);
end;

function TFileAppender.GetFileStream: TStream;
begin
  if fFileStream = nil then
  begin
    fFileStream := CreateFileStream;
  end;
  Result := fFileStream;
end;

function TFileAppender.GetEncoding: TEncoding;
begin
  Result := fEncoding;
end;

procedure TFileAppender.SetEncoding(const value: TEncoding);
begin
  fEncoding := value;
end;

procedure TFileAppender.SetFileName(const value: string);
begin
  if value <> fFileName then
  begin
    fFileName := value;
  end;
end;

{$ENDREGION}

end.
