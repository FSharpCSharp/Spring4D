unit Log4DIndy;

{
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/MPL-1.1.html
  
  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}

{
  Logging for Delphi - Internet support.
  Based on log4j Java package from Apache
  (http://jakarta.apache.org/log4j/docs/index.html).
  Currently based on log4j 1.2.8.

  E-mail and socket appenders based on Indy components.

  Written by Keith Wood (kbwood@iprimus.com.au).
  Version 1.0 - 29 April 2001.
  Version 1.2 - 9 September 2003.
}

interface

{$I Defines.inc}

uses
  Classes, SysUtils,
{$IFDEF DELPHI5_UP}
  Contnrs,
{$ENDIF}
  Log4D, IdSMTP, IdMessage, IdEMailAddress, IdUDPClient;

const
  { Buffer size option for TLogIndy*Appender. }
  BufferSizeOpt = 'bufferSize';
  { From address option for TLogIndySMTPAppender. }
  FromAddrOpt   = 'from';
  { Host option for TLogIndySMTPAppender. }
  HostOpt       = 'host';
  { Password option for TLogIndySMTPAppender. }
  PasswordOpt   = 'password';
  { Port option for TLogIndySMTPAppender. }
  PortOpt       = 'port';
  { Subject option for TLogIndySMTPAppender. }
  SubjectOpt    = 'subject';
  { Receiving timeout option for TLogIndySocketAppender. }
  TimeoutOpt    = 'timeout';
  { To address option for TLogIndySMTPAppender. }
  ToAddrOpt     = 'to';
  { User ID option for TLogIndySMTPAppender. }
  UserIDOpt     = 'userID';

type
  { Send messages via e-mail.
    An e-mail is only sent when a triggering condition arises;
    when an event of level Error or greater arrives.
    When it is sent, the previous BufferSize messages are also sent.

    Accepts the following options
    (as well as the standard layout and filter ones):

    # Class identification
    log4d.appender.<name>=TLogIndySMTPAppender
    # Number of messages sent when triggered, optional, default 20
    log4d.appender.<name>.bufferSize=20
    # Sender identification, optional
    log4d.appender.<name>.from=Log4D <log4d@log4d.com>
    # URL of mail host, mandatory
    log4d.appender.<name>.host=mail.log4d.com
    # Port on that machine for mail, optional, default 25
    log4d.appender.<name>.port=25
    # User id to access the mail system, optional
    log4d.appender.<name>.userID=myid
    # Password to access the mail system, optional
    log4d.appender.<name>.password=mypwd
    # Subject line for the e-mail, optional
    log4d.appender.<name>.subject=Log4D Demonstration
    # Recipient for the e-mail, mandatory
    log4d.appender.<name>.to=Administrator <admin@log4d.com>
  }
  TLogIndySMTPAppender = class(TLogCustomAppender)
  private
    FBuffer: TStringList;
    FBufferSize: Integer;
    FFromAddr: string;
    FHost: string;
    FPassword: string;
    FPort: Integer;
    FSubject: string;
    FToAddr: string;
    FTrigger: ILogFilter;
    FUserID: string;
  protected
    procedure SetOption(const Name, Value: string); override;
    procedure DoAppend(const Event: TLogEvent); overload; override;
    procedure DoAppend(const Message: string); overload; override;
  public
    constructor Create(const Name, Host: string; const Port: Integer;
      const UserId, Password, FromAddr, ToAddr, Subject: string;
      const Layout: ILogLayout = nil; const BufferSize: Integer = 20);
      reintroduce; virtual;
    destructor Destroy; override;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property FromAddr: string read FFromAddr write FFromAddr;
    property Host: string read FHost write FHost;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Subject: string read FSubject write FSubject;
    property ToAddr: string read FToAddr write FToAddr;
    property Trigger: ILogFilter read FTrigger write FTrigger;
    property UserID: string read FUserID write FUserID;
    procedure Init; override;
  end;

  { Send e-mail only on error message. }
  TLogIndySMTPTrigger = class(TLogCustomFilter)
  public
    function Decide(const Event: TLogEvent): TLogFilterDecision; override;
  end;

  { Send messages to a socket using UDP.
    Uses an internal layout by default: concatenates fields separated by tabs.
    Fields in order are: message, thread Id, timestamp, elapsed time,
    level name, level value, logger name, NDC, error message, error class name.

    See the Chainsaw program for an example of a listener for these messages.

    Accepts the following options
    (as well as the standard layout and filter ones):

    # Class identification
    log4d.appender.<name>=TLogIndySocketAppender
    # URL of host to send message to, mandatory
    log4d.appender.<name>.host=localhost
    # Port on that host, mandatory
    log4d.appender.<name>.port=9009
    # Largest packet size sent, optional, default 8192
    log4d.appender.<name>.bufferSize=8192
    # Millisecs to wait for socket to be readable, optional, default infinite
    log4d.appender.<name>.timeout=1000
  }
  TLogIndySocketAppender = class(TLogCustomAppender)
  private
    FClient: TIdUDPClient;
    procedure Final;
    function GetBufferSize: Integer;
    function GetHost: string;
    function GetPort: Integer;
    function GetTimeout: Integer;
  protected
    procedure SetOption(const Name, Value: string); override;
    procedure DoAppend(const Event: TLogEvent); overload; override;
    procedure DoAppend(const Message: string); overload; override;
  public
    constructor Create(const Name, Host: string; const Port: Integer);
      reintroduce; virtual;
    destructor Destroy; override;
    property BufferSize: Integer read GetBufferSize;
    property Host: string read GetHost;
    property Port: Integer read GetPort;
    property Timeout: Integer read GetTimeout;
    procedure Init; override;
    function RequiresLayout: Boolean; override;
  end;

implementation

resourcestring
  ConvertErrorMsg = 'Non-numeric value found for %s property "%s" - ignored';
  SendErrorMsg    = 'Error during e-mail send - %s';

{$IFDEF DELPHI4}
type
  TObjectList = TList;
{$ENDIF}

{ TLogIndySMTPAppender --------------------------------------------------------}

{ Initialise properties of the IndySMTP appender. }
constructor TLogIndySMTPAppender.Create(const Name, Host: string;
  const Port: Integer; const UserId, Password, FromAddr, ToAddr,
  Subject: string; const Layout: ILogLayout; const BufferSize: Integer);
begin
  inherited Create(Name, Layout);
  Self.BufferSize := BufferSize;
  Self.FromAddr   := FromAddr;
  Self.Host       := Host;
  Self.Password   := Password;
  Self.Port       := Port;
  Self.Subject    := Subject;
  Self.ToAddr     := ToAddr;
  Self.UserId     := UserID;
end;

{ Release resources. }
destructor TLogIndySMTPAppender.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

{ Append as usual, then see if e-mail is triggered. }
procedure TLogIndySMTPAppender.DoAppend(const Event: TLogEvent);
var
  SMTP: TIdSMTP;
  Message: TIdMessage;
  Body: string;
begin
  inherited DoAppend(Event);
  { An e-mail is only sent when a triggering condition arises.
    When it is sent, the previous BufferSize messages are also sent. }
  if Trigger.Decide(Event) = fdAccept then
  begin
    SMTP    := TIdSMTP.Create(nil);
    Message := TIdMessage.Create(nil);
    Body    := FBuffer.Text;
    FBuffer.Clear;
    try
      try
        SMTP.Host                   := Host;
        if Port <> 0 then
          SMTP.Port                 := Port;
{$IFDEF DELPHI7_UP}
        SMTP.Username               := UserID;
{$ELSE}
        SMTP.UserId                 := UserID;
{$ENDIF}
        SMTP.Password               := Password;
        Message.From                := TIdEMailAddressItem.Create(nil);
        Message.From.Text           := FromAddr;
        Message.Recipients.Add.Text := ToAddr;
        Message.Subject             := Subject;
        Message.Body.Text           := Body;
        SMTP.Connect;
        SMTP.Send(Message);
        SMTP.Disconnect;
      except on Ex: Exception do
        LogLog.Error(Format(SendErrorMsg, [Ex.Message]));
      end;
    finally
      SMTP.Free;
      Message.Free;
    end;
  end;
end;

{ Add the new message to the buffer. }
procedure TLogIndySMTPAppender.DoAppend(const Message: string);
begin
  while FBuffer.Count > BufferSize do
    FBuffer.Delete(0);
  FBuffer.Add(Message);
end;

{ Initialisation. }
procedure TLogIndySMTPAppender.Init;
begin
  inherited Init;
  FBuffer := TStringList.Create;
  Trigger := TLogIndySMTPTrigger.Create;
  if FBufferSize = 0 then
    FBufferSize := 20;
end;

{ Set options for this appender. }
procedure TLogIndySMTPAppender.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if Name = BufferSizeOpt then
    try
      BufferSize := StrToInt(Value);
    except
      LogLog.Warn(Format(ConvertErrorMsg, [BufferSizeOpt, Value]));
    end
  else if Name = FromAddrOpt then
    FromAddr := Value
  else if Name = HostOpt then
    Host := Value
  else if Name = PasswordOpt then
    Password := Value
  else if Name = PortOpt then
    try
      Port := StrToInt(Value);
    except
      LogLog.Warn(Format(ConvertErrorMsg, [PortOpt, Value]));
    end
  else if Name = SubjectOpt then
    Subject := Value
  else if Name = ToAddrOpt then
    ToAddr := Value
  else if Name = UserIDOpt then
    UserID := Value;
end;

{ TLogIndySMTPTrigger ---------------------------------------------------------}

{ An e-mail is only sent when the level is Error or greater. }
function TLogIndySMTPTrigger.Decide(const Event: TLogEvent): TLogFilterDecision;
begin
  if Event.Level.IsGreaterOrEqual(Error) then
    Result := fdAccept
  else
    Result := fdDeny;
end;

{ TLogIndySocketAppender ------------------------------------------------------}

const
  { Default format for UDP message sent.
    Fields are: message, thread Id, timestamp, elapsed time, level name,
    level value, logger name, NDC, error message, error class name. }
  SocketFormat = '%s'#9'%d'#9'%s'#9'%d'#9'%s'#9'%d'#9'%s'#9'%s'#9'%s'#9'%s';

var
  { Keep track of sockets allocated so that they can be freed in this unit. }
  Sockets: TObjectList;

{ Initialise the UDP client. }
constructor TLogIndySocketAppender.Create(const Name, Host: string;
  const Port: Integer);
begin
  inherited Create(Name);
  Layout := nil;
  Init;
  SetOption(HostOpt, Host);
  SetOption(PortOpt, IntToStr(Port));
end;

{ Release resources. }
destructor TLogIndySocketAppender.Destroy;
begin
  Final;
  inherited Destroy;
end;

{ Serialise the event and send it off. }
procedure TLogIndySocketAppender.DoAppend(const Event: TLogEvent);
begin
  if Layout <> nil then
    DoAppend(Layout.Format(Event))
  else
    DoAppend(Format(SocketFormat, [Event.Message, Event.ThreadId,
      FormatDateTime('yyyymmddhhnnsszzz', Event.TimeStamp),
      Event.ElapsedTime, Event.Level.Name, Event.Level.Level,
      Event.LoggerName, Event.NDC, Event.ErrorMessage, Event.ErrorClass]));
end;

{ Send the message off. }
procedure TLogIndySocketAppender.DoAppend(const Message: string);
begin
  if Assigned(FClient) then
    FClient.Send(Message);
end;

{ Release the UDP client resource. }
procedure TLogIndySocketAppender.Final;
begin
  FreeAndNil(FClient);
end;

{ Return the maximum size of packet sent. }
function TLogIndySocketAppender.GetBufferSize: Integer;
begin
  Result := FClient.BufferSize;
end;

{ Return the name of the host to send to. }
function TLogIndySocketAppender.GetHost: string;
begin
  Result := FClient.Host;
end;

{ Return the port on the host to send to. }
function TLogIndySocketAppender.GetPort: Integer;
begin
  Result := FClient.Port;
end;

{ Return the maximum time (ms) to wait for the socket to become readable. }
function TLogIndySocketAppender.GetTimeout: Integer;
begin
  Result := FClient.ReceiveTimeout;
end;

{ Create UDP client. }
procedure TLogIndySocketAppender.Init;
begin
  if FClient <> nil then
    Exit;
  inherited Init;
  FClient := TIdUDPClient.Create(nil);
  // Remember that this appender has been created, so it can be finalised later
  Sockets.Add(Self);
end;

{ Set socket options. }
procedure TLogIndySocketAppender.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if Name = HostOpt then
  begin
    FClient.Active := False;
    FClient.Host   := Value;
  end
  else if Name = PortOpt then
  begin
    FClient.Active := False;
    try
      FClient.Port := StrToInt(Value);
    except
      LogLog.Warn(Format(ConvertErrorMsg, [PortOpt, Value]));
    end;
  end
  else if Name = BufferSizeOpt then
    try
      FClient.BufferSize := StrToInt(Value);
    except
      LogLog.Warn(Format(ConvertErrorMsg, [BufferSizeOpt, Value]));
    end
  else if Name = TimeoutOpt then
    try
      FClient.ReceiveTimeout := StrToInt(Value);
    except
      LogLog.Warn(Format(ConvertErrorMsg, [TimeoutOpt, Value]));
    end;
  FClient.Active := (FClient.Host <> '') and (FClient.Port <> 0);
end;

{ Socket appender uses no layout - it serialises the event. }
function TLogIndySocketAppender.RequiresLayout: Boolean;
begin
  Result := False;
end;

procedure IndySocketAppenderFree;
var Index: Integer;
begin
  for Index := 0 to Sockets.Count - 1 do
    TLogIndySocketAppender(Sockets[Index]).Final;
end;

initialization
  { Registration of standard implementations. }
  RegisterAppender(TLogIndySMTPAppender);
  RegisterAppender(TLogIndySocketAppender);
  Sockets             := TObjectList.Create;
{$IFDEF DELPHI5_UP}
  Sockets.OwnsObjects := False;
{$ENDIF}
finalization
  { The UDP clients in the socket appenders must be released
    in this unit so that they are freed before the
    supporting stack (IdWinSock2) is destroyed. }
  IndySocketAppenderFree;
  Sockets.Free;
end.
