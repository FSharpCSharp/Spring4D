{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://delphi-spring-framework.googlecode.com               }
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
  Math,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns;

type
  ITrackable = interface;
  IConnection = interface;
  ICommunicationServer = interface;
  // ICommunicationClient = interface;
  ISession = interface;
  IMessage = interface;
  IMessageHandler = interface;

  /// <summary>
  /// Tracks network traffic and duration.
  /// </summary>
  ITrackable = interface

    {$REGION 'Property Getters & Setters'}
      function GetBytesSent: Int64;
      function GetBytesReceived: Int64;
      function GetDuration: TTimeSpan;
    {$ENDREGION}

    property BytesSent: Int64 read GetBytesSent;
    property BytesReceived: Int64 read GetBytesReceived;
    property Duration: TTimeSpan read GetDuration;
  end;

  /// <summary>
  /// Represents an incoming connection or outgoing connection.
  /// </summary>
  IConnection = interface(ITrackable)

    {$REGION 'Property Getters & Setters'}
      function GetConnected: Boolean;
      function GetTimeout: Integer;
      procedure SetConnected(const value: Boolean);
      procedure SetTimeout(const value: Integer);
    {$ENDREGION}

    procedure Open;
    procedure Close;
    procedure SendBuffer(const buffer: Pointer; bufferSize: Integer);
    procedure ReceiveBuffer(const buffer: Pointer; bufferSize: Integer);
    
    property Connected: Boolean read GetConnected write SetConnected;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;
  
  /// <summary>
  /// Represents a communication server listener.
  /// </summary>
  ICommunicationServerListener = interface
    procedure OnSessionBegin(const session: ISession);
    procedure OnSessionIdle(const session: ISession);
    procedure OnSessionEnd(const session: ISession);
//    procedure OnPacketSending(const session: ISession; packetStream: TCustomMemoryStream);
//    procedure OnPacketReceiving(const session: ISession; packetStream: TCustomMemoryStream);
  end;

  /// <summary>
  /// Represents a communication server.
  /// </summary>
  ICommunicationServer = interface(IObservable<ICommunicationServerListener>)
    procedure Start;
    procedure ShutDown;
    function GetSessions: ICollection<ISession>;
    property Sessions: ICollection<ISession>read GetSessions;
  end;

  TConnectionNotification = (
    cnConnect,
    cnCommunicate,
    cnDisconnect
  );

  TConnectionEventHandler = reference to procedure(const sender: IConnection; 
    notification: TConnectionNotification);

  /// <summary>
  /// Represents a communication portal, listening for incoming connection requests.
  /// </summary>
  IPortal = interface

    {$REGION 'Property Getters & Setters'}
      function GetIsConfigured: Boolean;
      function GetEventHandler: TConnectionEventHandler;
      procedure SetEventHandler(const value: TConnectionEventHandler);
    {$ENDREGION}

    procedure Configure(properties: TStrings);
    procedure Start;
    procedure ShutDown;
    property IsConfigured: Boolean read GetIsConfigured;
    property EventHandler: TConnectionEventHandler read GetEventHandler write SetEventHandler;
  end;

  TMessageType = Integer;

  /// <summary>
  /// Represents a meaningful message which can be handled.
  /// </summary>
  IMessage = interface
    {$REGION 'Property Getters & Setters'}
      function GetMessageSize: Integer;
//      function GetMessageType: TMessageType;
    {$ENDREGION}
    procedure LoadFromStream(stream: TStream);
    procedure SaveToStream(stream: TStream; const offset, count: Int64);
    property Size: Integer read GetMessageSize;
//    property MessageType: TMessageType read GetMessageType;
  end deprecated;

  /// <summary>
  /// TPacketType
  /// </summary>
  TPacketType = (
    ptInvalid,      // An invalid packet.
    ptMessage,      // A message has arrived that need to be handled.
    ptMessagePart,  // A message part has arrived, waiting for the next part.
    ptNOP           // NOP (No Operation Performed)
  );

  TPacketInfo = record
    PacketType:  TPacketType;
    MessageType: TMessageType;
  end;

  /// <summary>
  /// ICommunicationProtocol
  /// </summary>
  ICommunicationProtocol = interface
    function GetMaxPacketSize: Integer;
    function CreateMessage(messageType: TMessageType): IMessage;
    procedure Pack(const &message: IMessage; packetStream: TStream;
      var offSet: Integer; out hasNextPacket: Boolean);
    procedure Unpack(packetBuffer: Pointer; packetSize: Integer; messageStream: TStream;
      out packetInfo: TPacketInfo);
    property MaxPacketSize: Integer read GetMaxPacketSize;
  end;

  /// <summary>
  /// Represents a message handler.
  /// </summary>
  IMessageHandler = interface
    procedure HandleMessage(const &message: IMessage);
  end deprecated;

  /// <summary>
  /// Represents a session between a communication server and a client.
  /// </summary>
  ISession = interface
    procedure Execute;
    procedure Terminate;
    procedure Send(const &message: IMessage);
    procedure Receive(out &message: IMessage);
//    function GetConnection: IConnection;
//    function GetOnIdle: TSessionNotifyEvent;
//    procedure SetOnIdle(const value: TSessionNotifyEvent);
//    property Connection: IConnection read GetConnection;
//    property OnIdle: TSessionNotifyEvent read GetOnIdle write SetOnIdle;
  end;

  /// <summary>
  /// ISessionFactory
  /// </summary>
  ISessionFactory = interface
    function CreateSession(const connection: IConnection): ISession;
  end;

  /// <summary>
  /// Provides a simple implementation for communication server.
  /// </summary>
  /// <remarks>
  /// Thread-Safety.
  /// </remarks>
  TCommunicationServer = class(TObservable<ICommunicationServerListener>, ICommunicationServer, IInterface)
  private
    fSessionFactory: ISessionFactory;
    fPortals: IList<IPortal>;
    fPortalsLock: IReadWriteSync;
    fSessions: IDictionary<IConnection, ISession>;
    fSessionsLock: IReadWriteSync;
    function GetSessions: ICollection<ISession>;
  protected
    procedure DoHandleConnectionEvent(const connection: IConnection; notification: TConnectionNotification); virtual;
    procedure DoClientConnect(const connection: IConnection); virtual;
    procedure DoClientCommunicate(const connection: IConnection); virtual;
    procedure DoClientDisconnect(const connection: IConnection); virtual;
    procedure DoSessionBegin(const session: ISession); virtual;
    procedure DoSessionEnd(const session: ISession); virtual;
    function CreateSession(const connection: IConnection): ISession;
    function GetSession(const connection: IConnection): ISession;
    property SessionsLock: IReadWriteSync read fSessionsLock;
    property PortalsLock: IReadWriteSync read fPortalsLock;
  public
    constructor Create(const sessionFactory: ISessionFactory);
    destructor Destroy; override;
    procedure AddPortal(const portal: IPortal);
    procedure RemovePortal(const portal: IPortal);
    procedure Start; virtual;
    procedure ShutDown; virtual;
    property Sessions: ICollection<ISession> read GetSessions;
    property Portals: IList<IPortal> read fPortals;
  end;

//  TCommunicationClient = class
//
//  end;

  ECommunicationsException = class(Exception);

  TPortalList = TListAdapter<IPortal>;
  TConnectionList = TListAdapter<IConnection>;
  TSessionList = TListAdapter<ISession>;

implementation

uses 
  Spring.Communications.Core;


{$REGION 'TCommunicationsServer'}

constructor TCommunicationServer.Create(const sessionFactory: ISessionFactory);
begin
  inherited Create;
  TArgument.CheckNotNull(sessionFactory, 'sessionFactory');
  fSessionFactory := sessionFactory;
  fPortals := TContainer.CreateList<IPortal>;
  fPortalsLock := TMREWSync.Create;
  fSessions := TContainer.CreateDictionary<IConnection, ISession>;
  fSessionsLock := TMREWSync.Create;
end;

destructor TCommunicationServer.Destroy;
begin
  ShutDown;
  inherited Destroy;
end;

procedure TCommunicationServer.AddPortal(const portal: IPortal);
begin
  TArgument.CheckNotNull(portal, 'portal');
  PortalsLock.BeginWrite;
  try
    fPortals.Add(portal);
    portal.EventHandler := DoHandleConnectionEvent;
  finally
    PortalsLock.EndWrite;
  end;
end;

procedure TCommunicationServer.RemovePortal(const portal: IPortal);
begin
  TArgument.CheckNotNull(portal, 'portal');
  PortalsLock.BeginWrite;
  try
    fPortals.Remove(portal);
    portal.EventHandler := nil;
  finally
    PortalsLock.EndWrite;
  end;
end;

function TCommunicationServer.CreateSession(
  const connection: IConnection): ISession;
begin
  Result := fSessionFactory.CreateSession(connection);
end;

function TCommunicationServer.GetSession(const connection: IConnection): ISession;
begin
  SessionsLock.BeginRead;
  try
    Result := fSessions[connection];
  finally
    SessionsLock.EndRead;
  end;
end;

procedure TCommunicationServer.DoClientConnect(const connection: IConnection);
var
  session: ISession;
begin
  SessionsLock.BeginWrite;
  try
    session := CreateSession(connection);
    fSessions.Add(connection, session);
    DoSessionBegin(session);
  finally
    SessionsLock.EndWrite;
  end;
end;

procedure TCommunicationServer.DoClientCommunicate(
  const connection: IConnection);
var
  session: ISession;
begin
  session := GetSession(connection);
  session.Execute;
end;

procedure TCommunicationServer.DoClientDisconnect(
  const connection: IConnection);
var
  session: ISession;
begin
  SessionsLock.BeginWrite;
  try
    session := GetSession(connection);
    fSessions.Remove(connection);
    DoSessionEnd(session);
  finally
    SessionsLock.EndWrite;
  end;
end;

procedure TCommunicationServer.DoHandleConnectionEvent(
  const connection: IConnection; notification: TConnectionNotification);
begin
  case notification of
    cnConnect:      DoClientConnect(connection);
    cnCommunicate:  DoClientCommunicate(connection);
    cnDisconnect:   DoClientDisconnect(connection);
  end;
end;

procedure TCommunicationServer.DoSessionBegin(const session: ISession);
begin
  // TODO: Thread-Safety
  NotifyObservers(
    procedure(listener: ICommunicationServerListener)
    begin
      listener.OnSessionBegin(session);
    end
  );
end;

procedure TCommunicationServer.DoSessionEnd(const session: ISession);
begin
  // TODO: Thread-Safety
  NotifyObservers(
    procedure(listener: ICommunicationServerListener)
    begin
      listener.OnSessionEnd(session);
    end
  );
end;

procedure TCommunicationServer.Start;
var
  portal: IPortal;
begin
  PortalsLock.BeginRead;
  try
    for portal in Portals do
    begin
      portal.Start;
    end;
  finally
    PortalsLock.EndRead;
  end;
end;

procedure TCommunicationServer.ShutDown;
var
  portal: IPortal;
begin
  PortalsLock.BeginRead;
  try
    for portal in Portals do
    begin
      portal.ShutDown;
    end;
  finally
    PortalsLock.EndRead;
  end;
end;

function TCommunicationServer.GetSessions: ICollection<ISession>;
begin
  SessionsLock.BeginRead;
  try
    Result := fSessions.Values;
  finally
    SessionsLock.EndRead;
  end;
end;

{$ENDREGION}

end.
