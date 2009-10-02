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

unit Spring.Communications.Sockets;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  ScktComp,
  Generics.Collections,
  Spring.System,
  Spring.Communications,
  Spring.Communications.Core;

type
  TSocketPortal = class;
  TIncomingSocketConnection = class;

  TSocketPortal = class(TPortalBase)
  private
    fServerSocket: TServerSocket;
    fTimeout: Integer;
  protected
    function GetIsConfigured: Boolean; override;
    procedure DoGetThread(sender: TObject; clientSocket: TServerClientWinSocket;
      var socketThread: TServerClientThread); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(properties: TStrings); override;
    procedure Start; override;
    procedure ShutDown; override;
  end;

  TIncomingSocketConnection = class(TConnectionBase)
  strict private
    fSocket: TServerClientWinSocket;
    fSocketStream: TWinSocketStream;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DoReceiveBuffer(const buffer: Pointer; count: Integer); override;
    function GetConnected: Boolean; override;
    function DoWaitForData(timeout: Integer): Boolean; override;
    property ClientSocket: TServerClientWinSocket read fSocket;
  public
    constructor Create(clientSocket: TServerClientWinSocket);
    destructor Destroy; override;
    procedure SendBuffer(const buffer: Pointer; count: Integer); override;
  end;

  TIncomingSocketThread = class(TServerClientThread)
  protected
    fPortal: TSocketPortal;
    procedure ClientExecute; override;
  public
    constructor Create(portal: TSocketPortal; serverClientSocket: TServerClientWinSocket);
  end;

implementation


{$REGION 'TSocketServerPortal'}

constructor TSocketPortal.Create;
begin
  fServerSocket := TServerSocket.Create(nil);
  fServerSocket.ServerType := stThreadBlocking;
  fServerSocket.OnGetThread := DoGetThread;
  fTimeout := 0;
end;

destructor TSocketPortal.Destroy;
begin
  fServerSocket.Free;
  inherited Destroy;
end;

procedure TSocketPortal.Configure(properties: TStrings);
begin
  fServerSocket.Port := StrToIntDef(properties.Values['Port'], 0);
  fTimeout := StrToIntDef(properties.Values['Timeout'], 0);
end;

procedure TSocketPortal.Start;
begin
  fServerSocket.Open;
end;

procedure TSocketPortal.ShutDown;
begin
  fServerSocket.Close;
end;

procedure TSocketPortal.DoGetThread(sender: TObject;
  clientSocket: TServerClientWinSocket; var socketThread: TServerClientThread);
begin
  socketThread := TIncomingSocketThread.Create(Self, clientSocket);
end;

function TSocketPortal.GetIsConfigured: Boolean;
begin
  Result := fServerSocket.Port > 0;
end;

{$ENDREGION}


{$REGION 'TIncomingSocketConnection'}

constructor TIncomingSocketConnection.Create(clientSocket: TServerClientWinSocket);
begin
  inherited Create;
  fSocket := clientSocket;
  fSocketStream := TWinSocketStream.Create(fSocket, 0);
end;

destructor TIncomingSocketConnection.Destroy;
begin
  fSocketStream.Free;
  inherited Destroy;
end;

procedure TIncomingSocketConnection.DoConnect;
begin
//  fSocket.Open('', '', '', 0);
end;

procedure TIncomingSocketConnection.DoDisconnect;
begin
  fSocket.Close;
end;

function TIncomingSocketConnection.GetConnected: Boolean;
begin
  Result := fSocket.Connected;
end;

procedure TIncomingSocketConnection.SendBuffer(const buffer: Pointer;
  count: Integer);
begin
  fSocketStream.WriteBuffer(buffer, count);
end;

procedure TIncomingSocketConnection.DoReceiveBuffer(const buffer: Pointer;
  count: Integer);
begin
  fSocketStream.ReadBuffer(buffer^, count);
end;

function TIncomingSocketConnection.DoWaitForData(timeout: Integer): Boolean;
begin
  Result := fSocketStream.WaitForData(timeout);
end;

{$ENDREGION}


{$REGION 'TIncomingSocketThread'}

constructor TIncomingSocketThread.Create(portal: TSocketPortal;
  serverClientSocket: TServerClientWinSocket);
begin
  inherited Create(False, serverClientSocket);
  fPortal := portal;
end;

procedure TIncomingSocketThread.ClientExecute;
var
  connection: IConnection;
begin
  connection := TIncomingSocketConnection.Create(ClientSocket);
  try
    fPortal.ExecuteConnection(connection);
  finally
    connection := nil;
  end;
end;

{$ENDREGION}

end.
