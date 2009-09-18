{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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

unit Spring.Communications;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  XMLIntf,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Patterns;

type
  ITrackBytes = interface;
  IConnection = interface;
  IDataPacket = interface;
  ICommunicationsServer = interface;
//  ICommunicationsClient = interface;
  ISession = interface;
  IMessage = interface;
//  IMessageHandler = interface;

  /// <summary>
  /// Tracks network activity
  /// </summary>
  ITrackBytes = interface
    function GetBytesSent: Int64;
    function GetBytesReceived: Int64;
    property BytesSent: Int64 read GetBytesSent;
    property BytesReceived: Int64 read GetBytesReceived;
  end;

  TConnectionNotifyEvent = procedure(const sender: IConnection) of object;

  /// <summary>
  /// Represents an incoming connection or outgoing connection.
  /// </summary>
  IConnection = interface(ITrackBytes)
    procedure Open;
    procedure Close;
//    procedure SendStream(stream: TStream);
//    procedure ReceiveStream(stream: TStream);
    procedure SendBuffer(const buffer: Pointer; count: Integer);
    procedure ReceiveBuffer(var buffer: Pointer; count: Integer);
    function  GetOnDataReceived: TConnectionNotifyEvent;
    procedure SetOnDataReceived(const value: TConnectionNotifyEvent);
    property OnDataReceived: TConnectionNotifyEvent read GetOnDataReceived write SetOnDataReceived;
//    property RemoteAddress: string;
//    property Type: string;
  end;

  /// <summary>
  /// Represents a single physical data packet
  /// </summary>
  IDataPacket = interface
    ['{8ED02ECF-59D7-4943-B26D-8AF85F03D7E8}']
    function GetAsStream: TStream;
    property AsStream: TStream read GetAsStream;
  end;

  /// <summary>
  /// Defines a simple data packet format.
  /// </summary>
  TSimpleDataPacket = class
  private
    type
      THeadRec = packed record
        HeadFlag:  Byte;      // $CC
        DataSize:  Integer;
        SentSize:  Integer;
        TotalSize: Integer;   // Message Size
        Reserved:  array[0..31] of Byte;
      end;

      TTailRec = packed record
        Mac:       Integer;
        TailFlag:  Byte;      // $DD
      end;
  private
    fData: TBytes;
  end;

  /// <summary>
  /// Represents a communications server.
  /// </summary>
  ICommunicationsServer = interface //(IObservable<ICommunicationsServerListener>)
    procedure Configure(properties: TStrings);
    procedure Start;
    procedure ShutDown;
    function GetSessions: ICollection<ISession>;
    property Sessions: ICollection<ISession> read GetSessions;
  end;

  /// <summary>
  ///
  /// </summary>
  ICommunicationsPortal = interface

  end;

  IMessage = interface
//    procedure SaveToStream(stream: TStream; startIndex, count: Integer);
  end;

  /// <summary>
  /// Represents a session between server and client.
  /// </summary>
  ISession = interface
//    procedure Send(const msg: IMessage);
//    procedure Receive(out msg: IMessage); overload;
//    procedure Receive(out msg: IMessage; progressMonitor: IProgressMonitor); overload;
    function GetConnection: IConnection;
    property Connection: IConnection read GetConnection;
  end;

  ISessionFactory = interface
    function CreateSession(const connection: IConnection): ISession;
  end;

  TSessionBase = class(TInterfaceBase, ISession)
  private
    fConnection: IConnection;
    function GetConnection: IConnection;
  public
    constructor Create(const connection: IConnection);
//    procedure DoClientDataReceived(const connection: IConnection); virtual;
    property Connection: IConnection read GetConnection;
  end;

  TSessionNotification = (
    snAdded,
    snRemoved
  );
  
  /// <summary>
  /// Provides a simple implementation for communications server.
  /// </summary>
  /// <remarks>
  /// Thread-Safety.
  /// </remarks>
  TCommunicationsServer = class(TInterfaceBase, ICommunicationsServer)
  private
    fSessions: IDictionary<TObject, ISession>;
    fSessionsLock: IReadWriteSync;
    function GetSessions: ICollection<ISession>;
  protected
    property SessionsLock: IReadWriteSync read fSessionsLock;
    function CreateSession(const connection: IConnection): ISession; virtual;
    function FindSession(nativeConnection: TObject): ISession;
    procedure Notify(sender: TObject; const session: ISession; action: TSessionNotification); virtual;
    procedure DoAddSession(sender: TObject; const session: ISession); virtual;
    procedure DoRemoveSession(sender: TObject; const session: ISession); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(properties: TStrings); overload; virtual;
//    procedure Configure(const xmlNode: IXmlNode); overload; virtual;
    procedure Start; virtual;
    procedure ShutDown; virtual;
    property Sessions: ICollection<ISession> read GetSessions;
  end;

  TConnectionList = TListAdapter<IConnection>;

  TSessionList = TListAdapter<ISession>;

  ECommunicationsException = class(Exception);

implementation

{$REGION 'TCommunicationsServer'}

constructor TCommunicationsServer.Create;
begin
  inherited Create;
  fSessionsLock := TMREWSync.Create;
end;

destructor TCommunicationsServer.Destroy;
begin

  inherited;
end;

function TCommunicationsServer.CreateSession(
  const connection: IConnection): ISession;
begin
  raise ENotImplementedException.Create('CreateSession not implemented.');
end;

function TCommunicationsServer.FindSession(nativeConnection: TObject): ISession;
begin
  Result := fSessions[nativeConnection];
end;

procedure TCommunicationsServer.Notify(sender: TObject; 
  const session: ISession; action: TSessionNotification);
begin
  SessionsLock.BeginWrite;
  try
    case action of
      snAdded:
      begin
        DoAddSession(sender, session);
      end;
      snRemoved:
      begin
        DoRemoveSession(sender, session);
      end;
    end;
  finally
    SessionsLock.EndWrite;
  end;
end;

procedure TCommunicationsServer.DoAddSession(sender: TObject; const session: ISession);
begin
  fSessions.Add(sender, session);
end;

procedure TCommunicationsServer.DoRemoveSession(sender: TObject; const session: ISession);
begin
  fSessions.Remove(sender);
end;

procedure TCommunicationsServer.Configure(properties: TStrings);
begin
end;

procedure TCommunicationsServer.Start;
begin
end;

procedure TCommunicationsServer.ShutDown;
begin
end;

function TCommunicationsServer.GetSessions: ICollection<ISession>;
begin
  SessionsLock.BeginRead;
  try
    if fSessions = nil then
    begin
      fSessions := TContainer.CreateDictionary<TObject, ISession>;
    end;
    Result := fSessions.Values;
  finally
    SessionsLock.EndRead;
  end;
end;

{$ENDREGION}


{$REGION 'TSessionBase'}

constructor TSessionBase.Create(const connection: IConnection);
begin
  inherited Create;
  fConnection := connection;
end;

function TSessionBase.GetConnection: IConnection;
begin
  Result := fConnection;
end;

{$ENDREGION}

end.
