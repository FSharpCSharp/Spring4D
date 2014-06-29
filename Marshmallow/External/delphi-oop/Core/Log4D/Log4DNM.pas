unit Log4DNM;

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

  E-mail appender based on Net Masters components.

  Written by Keith Wood (kbwood@iprimus.com.au).
  Version 1.0 - 29 April 2001.
}

interface

uses
  Classes, SysUtils, Log4D, NMSMTP;

const
  { Buffer size option for TLogNMSMTPAppender. }
  BufferSizeOpt = 'bufferSize';
  { From address option for TLogNMSMTPAppender. }
  FromAddrOpt   = 'from';
  { Host option for TLogNMSMTPAppender. }
  HostOpt       = 'host';
  { Port option for TLogNMSMTPAppender. }
  PortOpt       = 'port';
  { Subject option for TLogNMSMTPAppender. }
  SubjectOpt    = 'subject';
  { To address option for TLogNMSMTPAppender. }
  ToAddrOpt     = 'to';
  { User ID option for TLogNMSMTPAppender. }
  UserIDOpt     = 'userID';

type
  { Send messages via e-mail.
    An e-mail is only sent when a triggering condition arises;
    when an event of level Error or greater arrives.
    When it is sent, the previous BufferSize messages are also sent.

    Accepts the following options
    (as well as the standard layout and filter ones):

    # Class identification
    log4d.appender.<name>=TLogNMSMTPAppender
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
    # Subject line for the e-mail, optional
    log4d.appender.<name>.subject=Log4D Demonstration
    # Recipient for the e-mail, mandatory
    log4d.appender.<name>.to=Administrator <admin@log4d.com>
  }
  TLogNMSMTPAppender = class(TLogCustomAppender)
  private
    FBuffer: TStringList;
    FBufferSize: Integer;
    FFromAddr: string;
    FHost: string;
    FPort: Integer;
    FSubject: string;
    FToAddr: string;
    FTrigger: ILogFilter;
    FUserID: string;
  protected
    procedure SetOption(const Name, Value: string); override;
    procedure DoAppend(Event: TLogEvent); overload; override;
    procedure DoAppend(Message: string); overload; override;
  public
    constructor Create(Name, Host: string; Port: Integer;
      UserId, FromAddr, ToAddr, Subject: string; Layout: ILogLayout = nil;
      BufferSize: Integer = 20); virtual;
    destructor Destroy; override;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property FromAddr: string read FFromAddr write FFromAddr;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Subject: string read FSubject write FSubject;
    property ToAddr: string read FToAddr write FToAddr;
    property Trigger: ILogFilter read FTrigger write FTrigger;
    property UserID: string read FUserID write FUserID;
    procedure Init; override;
  end;

  { Send e-mail only on error message. }
  TLogNMSMTPTrigger = class(TLogCustomFilter)
  public
    function Decide(Event: TLogEvent): TLogFilterDecision; override;
  end;

implementation

resourcestring
  ConvertErrorMsg = 'Non-numeric value found for %s property "%s" - ignored';
  SendErrorMsg    = 'Error during e-mail send - %s';

{ TLogNMSMTPAppender ----------------------------------------------------------}

{ Initialise properties of the NMSMTP appender. }
constructor TLogNMSMTPAppender.Create(const Name, Host: string;
  const Port: Integer; const UserId, FromAddr, ToAddr, Subject: string;
  const Layout: ILogLayout; const BufferSize: Integer);
begin
  inherited Create(Name, Layout);
  Self.BufferSize := BufferSize;
  Self.FromAddr   := FromAddr;
  Self.Host       := Host;
  Self.Port       := Port;
  Self.Subject    := Subject;
  Self.ToAddr     := ToAddr;
  Self.UserId     := UserID;
end;

{ Release resources. }
destructor TLogNMSMTPAppender.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

{ Append as usual, then see if e-mail is triggered. }
procedure TLogNMSMTPAppender.DoAppend(Event: TLogEvent);
var
  SMTP: TNMSMTP;
  Body: string;
  Index: Integer;
begin
  inherited DoAppend(Event);
  { An e-mail is only sent when a triggering condition arises.
    When it is sent, the previous BufferSize messages are also sent. }
  if Trigger.Decide(Event) = fdAccept then
  begin
    SMTP := TNMSMTP.Create(nil);
    Body := FBuffer.Text;
    FBuffer.Clear;
    try
      try
        SMTP.Host                       := Host;
        if Port <> 0 then
          SMTP.Port                     := Port;
        SMTP.UserID                     := UserID;
        SMTP.Connect;
        SMTP.PostMessage.Date           := DateTimeToStr(Now);
        Index                           := Pos('<', FromAddr);
        if Index > 0 then
        begin
          SMTP.PostMessage.FromAddress  :=
            Copy(FromAddr, Index + 1, Length(FromAddr) - Index - 1);
          SMTP.PostMessage.FromName     := Copy(FromAddr, 1, Index - 1);
        end
        else
          SMTP.PostMessage.FromAddress  := FromAddr;
        SMTP.PostMessage.ToAddress.Text := ToAddr;
        SMTP.PostMessage.Subject        := Subject;
        SMTP.PostMessage.Body.Text      := Body;
        SMTP.SendMail;
        SMTP.Disconnect;
      except on Ex: Exception do
        LogLog.Error(Format(SendErrorMsg, [Ex.Message]));
      end;
    finally
      SMTP.Free;
    end;
  end;
end;

{ Add the new message to the buffer. }
procedure TLogNMSMTPAppender.DoAppend(Message: string);
begin
  while FBuffer.Count > BufferSize do
    FBuffer.Delete(0);
  FBuffer.Add(Message);
end;

{ Initialisation. }
procedure TLogNMSMTPAppender.Init;
begin
  inherited Init;
  FBuffer := TStringList.Create;
  Trigger := TLogNMSMTPTrigger.Create;
  if FBufferSize = 0 then
    FBufferSize := 20;
end;

{ Set options for this appender. }
procedure TLogNMSMTPAppender.SetOption(const Name, Value: string);
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

{ TLogNMSMTPTrigger -----------------------------------------------------------}

{ An e-mail is only sent when the level is Error or greater. }
function TLogNMSMTPTrigger.Decide(Event: TLogEvent): TLogFilterDecision;
begin
  if Event.Level.IsGreaterOrEqual(Error) then
    Result := fdAccept
  else
    Result := fdDeny;
end;

initialization
  { Registration of standard implementations. }
  RegisterAppender(TLogNMSMTPAppender);
end.
