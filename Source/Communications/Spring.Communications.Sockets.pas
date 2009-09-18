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

unit Spring.Communications.Sockets;

interface

uses
  Classes,
  SysUtils,
  ScktComp,
  Generics.Collections,
  Spring.System,
  Spring.Communications;

type
  TSocketIncomingConnection = class;

  TSocketCommunicationsServer = class(TCommunicationsServer)
  private
    fServerSocket: TServerSocket;
  protected
    function GetIsConfigured: Boolean; virtual;
    procedure DoGetThread(sender: TObject; clientSocket: TServerClientWinSocket;
      var socketThread: TServerClientThread); virtual;
    procedure DoClientConnect(sender: TObject; socket: TCustomWinSocket); virtual;
    procedure DoClientDisconnect(sender: TObject; socket: TCustomWinSocket); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(properties: TStrings); override;
    procedure Start; override;
    procedure ShutDown; override;
  end;

  TSocketIncomingConnection = class(TInterfaceBase, IConnection)
  strict private
    fServer: ICommunicationsServer;
    fSocket: TServerClientWinSocket;
    fSocketStream: TWinSocketStream;
    fBytesSent: Int64;
    fBytesReceived: Int64;
    fOnDataReceived: TConnectionNotifyEvent;
    function GetBytesSent: Int64;
    function GetBytesReceived: Int64;
    function GetOnDataReceived: TConnectionNotifyEvent;
    procedure SetOnDataReceived(const value: TConnectionNotifyEvent);
  protected
    procedure HandleCommunications; virtual;
    property ClientSocket: TServerClientWinSocket read fSocket;
  public
    constructor Create(clientSocket: TServerClientWinSocket);
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure SendStream(stream: TStream);
    procedure SendBuffer(const buffer: Pointer; count: Integer);
    procedure ReceiveStream(stream: TStream);
    procedure ReceiveBuffer(var buffer: Pointer; count: Integer);
    property BytesSent: Int64 read GetBytesSent;
    property BytesReceived: Int64 read GetBytesReceived;
    property OnDataReceived: TConnectionNotifyEvent read GetOnDataReceived write SetOnDataReceived;
  end;

  TSocketIncomingThread = class(TServerClientThread)
  private
    fConnection: TSocketIncomingConnection;
  protected
    procedure ClientExecute; override;
  public
    constructor Create(connection: TSocketIncomingConnection);
  end;

implementation

{$REGION 'TSocketClientConnection'}

constructor TSocketIncomingConnection.Create(clientSocket: TServerClientWinSocket);
begin
  inherited Create;
  fSocket := clientSocket;
  fSocketStream := TWinSocketStream.Create(fSocket, 0);
end;

destructor TSocketIncomingConnection.Destroy;
begin
  fSocketStream.Free;
  inherited Destroy;
end;

procedure TSocketIncomingConnection.HandleCommunications;
begin
  if fSocketStream.WaitForData(500) then
  begin
    if Assigned(fOnDataReceived) then
      fOnDataReceived(Self);
  end;
end;

procedure TSocketIncomingConnection.Open;
begin
//  fSocket.Open('', '', '', 0);
end;

procedure TSocketIncomingConnection.Close;
begin
  fSocket.Close;
end;

procedure TSocketIncomingConnection.SendStream(stream: TStream);
begin
  fSocketStream.CopyFrom(stream, stream.Size);
end;

procedure TSocketIncomingConnection.SendBuffer(const buffer: Pointer;
  count: Integer);
begin
  fSocketStream.WriteBuffer(buffer, count);
end;

procedure TSocketIncomingConnection.ReceiveBuffer(var buffer: Pointer;
  count: Integer);
begin
  fSocketStream.ReadBuffer(buffer, count);
end;

procedure TSocketIncomingConnection.ReceiveStream(stream: TStream);
begin
  stream.CopyFrom(fSocketStream, fSocketStream.Size);
end;

function TSocketIncomingConnection.GetBytesSent: Int64;
begin
  Result := fBytesSent;
end;

function TSocketIncomingConnection.GetBytesReceived: Int64;
begin
  Result := fBytesReceived;
end;

function TSocketIncomingConnection.GetOnDataReceived: TConnectionNotifyEvent;
begin
  Result := fOnDataReceived;
end;

procedure TSocketIncomingConnection.SetOnDataReceived(
  const value: TConnectionNotifyEvent);
begin
  fOnDataReceived := value;
end;

{$ENDREGION}


{$REGION 'TSocketCommunicationsServer'}

constructor TSocketCommunicationsServer.Create;
begin
  fServerSocket := TServerSocket.Create(nil);
  fServerSocket.ServerType := stThreadBlocking;
  fServerSocket.OnGetThread := DoGetThread;
  fServerSocket.OnClientConnect := DoClientConnect;
  fServerSocket.OnClientDisconnect := DoClientDisconnect;
end;

destructor TSocketCommunicationsServer.Destroy;
begin
  fServerSocket.Free;
  inherited Destroy;
end;

procedure TSocketCommunicationsServer.Configure(properties: TStrings);
begin
  fServerSocket.Port := StrToIntDef(properties.Values['Port'], 0);
end;

procedure TSocketCommunicationsServer.DoClientConnect(sender: TObject;
  socket: TCustomWinSocket);
var
  connection: TSocketIncomingConnection;
  session: ISession;
begin
  connection := TSocketIncomingConnection.Create(socket as TServerClientWinSocket);
  session := CreateSession(connection);
  Notify(socket, session, snAdded);
end;

procedure TSocketCommunicationsServer.DoClientDisconnect(sender: TObject;
  socket: TCustomWinSocket);
var
  session: ISession;
begin
  session := FindSession(socket);
  Notify(socket, session, snRemoved);
end;

procedure TSocketCommunicationsServer.DoGetThread(sender: TObject;
  clientSocket: TServerClientWinSocket; var socketThread: TServerClientThread);
var
  session: ISession;
begin
  session := FindSession(clientSocket);
  socketThread := TSocketIncomingThread.Create(session.Connection as TSocketIncomingConnection);
end;

function TSocketCommunicationsServer.GetIsConfigured: Boolean;
begin
  Result := fServerSocket.Port > 0;
end;

procedure TSocketCommunicationsServer.Start;
begin
  fServerSocket.Open;
end;

procedure TSocketCommunicationsServer.ShutDown;
begin
  fServerSocket.Close;
end;

{$ENDREGION}


{$REGION 'TServerSocketThread'}

constructor TSocketIncomingThread.Create(connection: TSocketIncomingConnection);
begin
  inherited Create(False, connection.ClientSocket);
  fConnection := connection;
end;

procedure TSocketIncomingThread.ClientExecute;
begin
  while not Terminated and ClientSocket.Connected do
  begin
    fConnection.HandleCommunications;
  end;
end;

{$ENDREGION}

end.
