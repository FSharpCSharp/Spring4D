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
  Math,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Patterns;

type
  ITrackActivity = interface;
  IConnection = interface;
  ICommunicationsServer = interface;
  // ICommunicationsClient = interface;
  ISession = interface;
  IMessage = interface;
  IMessageHandler = interface;

  /// <summary>
  /// Tracks network activity
  /// </summary>
  ITrackActivity = interface

  {$REGION 'Property Getters & Setters'}
    function GetBytesSent: Int64;
    function GetBytesReceived: Int64;
    function GetElapsedTime: TTimeSpan;
  {$ENDREGION}

    property BytesSent: Int64 read GetBytesSent;
    property BytesReceived: Int64 read GetBytesReceived;
    property ElapsedTime: TTimeSpan read GetElapsedTime;
  end;

  /// <summary>
  /// Represents an incoming connection or outgoing connection.
  /// </summary>
  IConnection = interface(ITrackActivity)

  {$REGION 'Property Getters & Setters'}
    function GetConnected: Boolean;
    function GetTimeout: Integer;
    procedure SetConnected(const value: Boolean);
    procedure SetTimeout(const value: Integer);
  {$ENDREGION}

    procedure Open;
    procedure Close;
    procedure SendBuffer(const buffer: Pointer; bufferSize: Integer);
    procedure ReceiveBuffer(var buffer: Pointer; bufferSize: Integer);
    
    property Connected: Boolean read GetConnected write SetConnected;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;
  
  /// <summary>
  /// Represents a communications server listener.
  /// </summary>
  ICommunicationsServerListener = interface
    procedure OnSessionBegin(const session: ISession);
    procedure OnSessionIdle(const session: ISession);
    procedure OnSessionEnd(const session: ISession);
  end;

  /// <summary>
  /// Represents a communications server.
  /// </summary>
  ICommunicationsServer = interface(IObservable<ICommunicationsServerListener>)
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
  /// Represents a communications portal, listening for incoming connection requests.
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

  /// <summary>
  /// Represents a message.
  /// </summary>
  IMessage = interface
    procedure SaveToStream(stream: TStream; offSet, count: Integer);
    function GetSize: Integer;
    property Size: Integer read GetSize;
  end;

  /// <summary>
  /// Represents a message handler.
  /// </summary>
  IMessageHandler = interface
    ['{DDC0C554-C9F8-459F-9CBA-D4C1F62C6F57}']
    procedure HandleMessage(const &message: IMessage);
  end;

  /// <summary>
  /// Represents a session between a communications server and a client.
  /// </summary>
  ISession = interface
    procedure Execute;
    procedure Send(const &message: IMessage);
    // procedure Receive(out &message: IMessage); overload;
//    function GetConnection: IConnection;
//    function GetOnIdle: TSessionNotifyEvent;
//    procedure SetOnIdle(const value: TSessionNotifyEvent);
//    property Connection: IConnection read GetConnection;
//    property OnIdle: TSessionNotifyEvent read GetOnIdle write SetOnIdle;
  end;

  IDataPacketFormat = interface
    function GetHeaderSize: Integer;
    function GetFooterSize: Integer;
//    function IsValidDataPacket(const buffer: Pointer; count: Integer): Boolean;
//    function GetDataSize(const buffer: Pointer; count: Integer): Boolean;
//    function GetMessageSize(const buffer: Pointer; count: Integer): Boolean;
//    procedure ExtractMessage(dataPacket: TStream);
    property HeaderSize: Integer read GetHeaderSize;
    property FooterSize: Integer read GetFooterSize;
  end;
    
  /// <summary>
  ///
  /// </summary>
  TDataPacketBuilder = class
  strict private
    fStream: TCustomMemoryStream;
    fMaxPacketSize: Integer;
    function GetStream: TCustomMemoryStream;
    procedure SetMaxPacketSize(const value: Integer);
  protected
    procedure DoWriteHeader(stream: TStream); virtual;
    procedure DoWriteData(stream: TStream); virtual;
    procedure DoWriteFooter(stream: TStream); virtual;
    function WriteHeader: TDataPacketBuilder;
    function WriteData: TDataPacketBuilder;
    function WriteFooter: TDataPacketBuilder;
    function GetMaxDataSize: Integer; virtual;
    function GetFooterSize: Integer; virtual;
    function GetHeaderSize: Integer; virtual;
    property Stream: TCustomMemoryStream read GetStream;
    property MaxDataSize: Integer read GetMaxDataSize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; virtual;
    function MoveNext: Boolean; virtual; abstract;
    property DataPacketStream: TCustomMemoryStream read fStream;
    property MaxPacketSize: Integer read fMaxPacketSize write SetMaxPacketSize;
    property HeaderSize: Integer read GetHeaderSize;
    property FooterSize: Integer read GetFooterSize;
  end;

  /// <summary>
  /// ISessionFactory
  /// </summary>
  ISessionFactory = interface
    function CreateSession(const connection: IConnection): ISession;
  end;

  /// <summary>
  /// Provides a simple implementation for communications server.
  /// </summary>
  /// <remarks>
  /// Thread-Safety.
  /// </remarks>
  TCommunicationsServer = class(TObservable<ICommunicationsServerListener>, ICommunicationsServer, IInterface)
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
    function CreateSession(const connection: IConnection): ISession; virtual;
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

  ECommunicationsException = class(Exception);

  TPortalList = TListAdapter<IPortal>;
  TConnectionList = TListAdapter<IConnection>;
  TSessionList = TListAdapter<ISession>;

implementation

uses 
  Spring.Communications.Core;


{$REGION 'TCommunicationsServer'}

constructor TCommunicationsServer.Create(const sessionFactory: ISessionFactory);
begin
  inherited Create;
  TArgument.CheckNotNull(sessionFactory, 'sessionFactory');
  fSessionFactory := sessionFactory;
  fPortals := TContainer.CreateList<IPortal>;
  fPortalsLock := TMREWSync.Create;
  fSessions := TContainer.CreateDictionary<IConnection, ISession>;
  fSessionsLock := TMREWSync.Create;
end;

destructor TCommunicationsServer.Destroy;
begin
  ShutDown;
  inherited Destroy;
end;

procedure TCommunicationsServer.AddPortal(const portal: IPortal);
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

procedure TCommunicationsServer.RemovePortal(const portal: IPortal);
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

function TCommunicationsServer.CreateSession(
  const connection: IConnection): ISession;
begin
  Result := fSessionFactory.CreateSession(connection);
end;

function TCommunicationsServer.GetSession(const connection: IConnection): ISession;
begin
  SessionsLock.BeginRead;
  try
    Result := fSessions[connection];
  finally
    SessionsLock.EndRead;
  end;
end;

procedure TCommunicationsServer.DoClientConnect(const connection: IConnection);
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

procedure TCommunicationsServer.DoClientCommunicate(
  const connection: IConnection);
var
  session: ISession;
begin
  session := GetSession(connection);
  session.Execute;
end;

procedure TCommunicationsServer.DoClientDisconnect(
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

procedure TCommunicationsServer.DoHandleConnectionEvent(
  const connection: IConnection; notification: TConnectionNotification);
begin
  case notification of
    cnConnect:      DoClientConnect(connection);
    cnCommunicate:  DoClientCommunicate(connection);
    cnDisconnect:   DoClientDisconnect(connection);
  end;
end;

procedure TCommunicationsServer.DoSessionBegin(const session: ISession);
begin
  // TODO: Thread-Safety
  NotifyObservers(
    procedure(listener: ICommunicationsServerListener)
    begin
      listener.OnSessionBegin(session);
    end
  );
end;

procedure TCommunicationsServer.DoSessionEnd(const session: ISession);
begin
  // TODO: Thread-Safety
  NotifyObservers(
    procedure(listener: ICommunicationsServerListener)
    begin
      listener.OnSessionEnd(session);
    end
  );
end;

procedure TCommunicationsServer.Start;
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

procedure TCommunicationsServer.ShutDown;
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

function TCommunicationsServer.GetSessions: ICollection<ISession>;
begin
  SessionsLock.BeginRead;
  try
    Result := fSessions.Values;
  finally
    SessionsLock.EndRead;
  end;
end;

{$ENDREGION}


{$REGION 'TDataPacketBuilder'}

type
  TMemoryStreamHacker = class(Classes.TMemoryStream);

constructor TDataPacketBuilder.Create;
begin
  inherited Create;
  fStream := TMemoryStream.Create;
  MaxPacketSize := 1024 * 4;
end;

destructor TDataPacketBuilder.Destroy;
begin
  fStream.Free;
  inherited Destroy;
end;

function TDataPacketBuilder.GetMaxDataSize: Integer;
begin
  Result := MaxPacketSize - HeaderSize - FooterSize;
end;

function TDataPacketBuilder.GetHeaderSize: Integer;
begin
  Result := 0;
end;

function TDataPacketBuilder.GetFooterSize: Integer;
begin
  Result := 0;
end;

procedure TDataPacketBuilder.Reset;
begin
  fStream.Size := 0;  // fStream.Position := 0;
end;

function TDataPacketBuilder.WriteHeader: TDataPacketBuilder;
begin
  DoWriteHeader(Stream);
  Result := Self;
end;

function TDataPacketBuilder.WriteData: TDataPacketBuilder;
begin
  DoWriteData(Stream);
  Result := Self;
end;

function TDataPacketBuilder.WriteFooter: TDataPacketBuilder;
begin
  DoWriteFooter(Stream);
  Result := Self;
end;

procedure TDataPacketBuilder.DoWriteHeader(stream: TStream);
begin
end;

procedure TDataPacketBuilder.DoWriteData(stream: TStream);
begin
end;

procedure TDataPacketBuilder.DoWriteFooter(stream: TStream);
begin
end;

function TDataPacketBuilder.GetStream: TCustomMemoryStream;
begin
  if fStream = nil then
  begin
    fStream := TMemoryStream.Create;
  end;
  Result := fStream;
end;

procedure TDataPacketBuilder.SetMaxPacketSize(const value: Integer);
begin
  fMaxPacketSize := value;
  TMemoryStreamHacker(fStream).Capacity := fMaxPacketSize;
end;

{$ENDREGION}

end.
