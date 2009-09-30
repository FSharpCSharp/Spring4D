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

unit Spring.Communications.Core;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Windows,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Communications;

type
  TPortalBase = class abstract(TInterfacedObject, IPortal, IInterface)
  private
    fEventHandler: TConnectionEventHandler;
    function GetEventHandler: TConnectionEventHandler;
    procedure SetEventHandler(const value: TConnectionEventHandler);
  protected
    procedure ExecuteConnection(const connection: IConnection); virtual;
    function GetIsConfigured: Boolean; virtual; abstract;
  public
    procedure Configure(properties: TStrings); virtual;
    procedure Start; virtual; abstract;
    procedure ShutDown; virtual; abstract;
    property IsConfigured: Boolean read GetIsConfigured;
    property EventHandler: TConnectionEventHandler read GetEventHandler write SetEventHandler;
  end;

  TConnectionBase = class abstract(TInterfacedObject, IConnection, ITrackable, IInterface)
  private
    fBytesSent: Int64;
    fBytesReceived: Int64;
    fTimeout: Integer;
    fStopWatch: TStopwatch;
    function GetBytesSent: Int64;
    function GetBytesReceived: Int64;
    function GetDuration: TTimeSpan;
    function GetTimeout: Integer;
    procedure SetConnected(const value: Boolean);
    procedure SetTimeout(const Value: Integer);
  private
    procedure InternalConnect;
    procedure InternalDisconnect;
  protected
    function GetConnected: Boolean; virtual; abstract;
    function DoWaitForData(timeout: Integer): Boolean; virtual; abstract;
    procedure DoReceiveBuffer(const buffer: Pointer; count: Integer); virtual; abstract;
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
  protected
    procedure DoBeforeConnect; virtual;
    procedure DoAfterConnect; virtual;
    procedure DoBeforeDisconnect; virtual;
    procedure DoAfterDisconnect; virtual;
  public
    procedure Open;
    procedure Close;
    procedure SendBuffer(const buffer: Pointer; count: Integer); virtual; abstract;
    procedure ReceiveBuffer(const buffer: Pointer; count: Integer); virtual;
    property Connected: Boolean read GetConnected write SetConnected;
    property BytesSent: Int64 read GetBytesSent;
    property BytesReceived: Int64 read GetBytesReceived;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property Duration: TTimeSpan read GetDuration;
  end;

  TMessageBase = class(TInterfacedObject, IMessage, IInterface)
  protected
    function GetMessageSize: Integer; virtual; abstract;
//    function GetMessageType: TMessageType;
  public
    procedure LoadFromStream(stream: TStream); virtual; abstract;
    procedure SaveToStream(stream: TStream; const offset, count: Int64); virtual; abstract;
    property Size: Integer read GetMessageSize;
//    property IsFixedSize: Boolean read GetIsFixedSize;
//    property MessageType: TMessageType read GetMessageType;
  end;

  /// <summary>
  /// Provides a reusable memory stream.
  /// </summary>
  TReusableMemoryStream = class(TMemoryStream)
  public
    constructor Create;
    procedure Reset;
  end;

  TDataPacketReader = class
  private
    fConnection: IConnection;
    fDataStream: TReusableMemoryStream;
    function GetDataStream: TCustomMemoryStream;
  public
    constructor Create(const connection: IConnection);
    destructor Destroy; override;
    function GetNextDataPacket: Boolean; virtual;
    property PacketStream: TCustomMemoryStream read GetDataStream;
  end;

  TSessionBase = class abstract(TInterfacedObject, ISession, IInterface)
  private
    fConnection: IConnection;
    fProtocol: ICommunicationProtocol;
    fSendBuffer: TReusableMemoryStream;
    fMessageBuffer: TReusableMemoryStream;
    fReader: TDataPacketReader;
    fTerminated: Boolean;
    function GetConnection: IConnection;
  protected
    procedure DoIdle; virtual;
    procedure DoCommunicate; virtual;
    procedure GetNextMessage(out msg: IMessage); virtual;
    procedure HandleMessage(const &message: IMessage); virtual;
  public
    constructor Create(const connection: IConnection);
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure Terminate;
    procedure Send(const msg: IMessage);
    procedure Receive(out msg: IMessage);
    property Connection: IConnection read GetConnection;
    property Terminated: Boolean read fTerminated;
  end;

  TClientSession = class(TSessionBase)

  end;

  /// <summary>
  /// Provides a simple communication protocol implementation.
  /// </summary>
  /// <remarks>
  /// <p>
  /// Every data packet should consist of the following parts:
  /// [Header][MessageType][Content][Footer]
  /// </p>
  /// </remarks>
  TSimpleCommunicationProtocol = class(TInterfacedObject, ICommunicationProtocol)
  strict private
    type
      THeaderRec = packed record
        Flag:       Byte;        // fCHeaderFlag ($CC)
        Padding:    array [0..2] of Byte;
        TotalSize:  Integer;
        Position:   Integer;
        DataSize:   Integer;
        Reserved:   array [0..31] of Byte;
      end;

      TFooterRec = packed record
        Code: Integer;    // e.g. CRC, MAC
        Flag: Byte;       // fCFooterFlag ($DD)
        Padding:    array [0..2] of Byte;
      end;

    const
      fCHeaderFlag: Byte = $CC;
      fCFooterFlag: Byte = $DD;
      fCMetaDataSize = SizeOf(THeaderRec) + SizeOf(TFooterRec);
      fCMaxPacketSize = 1024 * 4;
  private
    function GetMaxPacketSize: Integer;
  public
    procedure Pack(const &message: IMessage; packetStream: TStream;
      var offset: Integer; out hasNextPacket: Boolean);
    procedure Unpack(packetBuffer: Pointer; packetSize: Integer; messageStream: TStream;
      out packetInfo: TPacketInfo);
    function CreateMessage(messageType: TMessageType): IMessage;
    property MaxPacketSize: Integer read GetMaxPacketSize;
  end;

implementation

uses
  Math,
  Spring.ResourceStrings;


{$REGION 'TPortalBase'}

procedure TPortalBase.Configure(properties: TStrings);
begin
end;

procedure TPortalBase.ExecuteConnection(const connection: IConnection);
begin
  Assert(connection <> nil, 'connection should not be nil.');
  if Assigned(fEventHandler) then
  begin
    fEventHandler(connection, cnConnect);
    try
      fEventHandler(connection, cnCommunicate);
    finally
      fEventHandler(connection, cnDisconnect);
    end;
  end;
end;

function TPortalBase.GetEventHandler: TConnectionEventHandler;
begin
  Result := fEventHandler;
end;

procedure TPortalBase.SetEventHandler(const value: TConnectionEventHandler);
begin
  fEventHandler := value;
end;

{$ENDREGION}


{$REGION 'TConnectionBase'}

procedure TConnectionBase.Open;
begin
  Connected := True;
end;

procedure TConnectionBase.Close;
begin
  Connected := False;
end;

procedure TConnectionBase.ReceiveBuffer(const buffer: Pointer; count: Integer);
begin
  if not DoWaitForData(fTimeout) then
  begin
    raise ETimeoutException.Create(SConnectionTimeout);
  end;
  DoReceiveBuffer(buffer, count);
end;

procedure TConnectionBase.InternalConnect;
begin
  fStopWatch := fStopWatch.StartNew;
  DoBeforeConnect;
  DoConnect;
  DoAfterConnect;
end;

procedure TConnectionBase.InternalDisconnect;
begin
  DoBeforeDisconnect;
  DoDisconnect;
  DoAfterDisconnect;
  fStopWatch.Stop;
end;

procedure TConnectionBase.DoAfterConnect;
begin
end;

procedure TConnectionBase.DoAfterDisconnect;
begin
end;

procedure TConnectionBase.DoBeforeConnect;
begin
end;

procedure TConnectionBase.DoBeforeDisconnect;
begin
end;

function TConnectionBase.GetDuration: TTimeSpan;
begin
  Result := fStopWatch.Elapsed;
end;

function TConnectionBase.GetTimeout: Integer;
begin
  Result := fTimeout;
end;

function TConnectionBase.GetBytesSent: Int64;
begin
  Result := fBytesSent;
end;

function TConnectionBase.GetBytesReceived: Int64;
begin
  Result := fBytesReceived;
end;

procedure TConnectionBase.SetConnected(const value: Boolean);
begin
  if Connected <> value then
  begin
    if value then
      InternalConnect
    else
      InternalDisconnect;
  end;
end;

procedure TConnectionBase.SetTimeout(const Value: Integer);
begin
  if fTimeout <> value then
  begin
    fTimeout := value;
  end;
end;

{$ENDREGION}


{$REGION 'TSessionBase'}

constructor TSessionBase.Create(const connection: IConnection);
begin
  inherited Create;
  fConnection := connection;
  fReader := TDataPacketReader.Create(fConnection);
  fMessageBuffer := TReusableMemoryStream.Create;
end;

destructor TSessionBase.Destroy;
begin
  fReader.Free;
  fMessageBuffer.Free;
  inherited Destroy;
end;

procedure TSessionBase.DoCommunicate;
var
  msg: IMessage;
begin
  while not Terminated do
  begin
    GetNextMessage(msg);
    HandleMessage(msg);
  end;
end;

procedure TSessionBase.DoIdle;
begin
end;

procedure TSessionBase.GetNextMessage(out msg: IMessage);
var
  packetInfo: TPacketInfo;
begin
  fMessageBuffer.Reset;
  while not Terminated and fReader.GetNextDataPacket do
  begin
    fProtocol.Unpack(fReader.PacketStream.Memory, fReader.PacketStream.Size, fMessageBuffer, packetInfo);
    case packetInfo.PacketType of
      ptMessage:
      begin
        msg := fProtocol.CreateMessage(packetInfo.MessageType);
        msg.LoadFromStream(fMessageBuffer);
        HandleMessage(msg);
        fMessageBuffer.Reset;
        Break;
      end;
      ptMessagePart:
      begin
        // do nothing
      end;
      ptInvalid:
      begin
        Terminate;    // raise ECommunicationError.Create;
      end;
      ptNOP:
      begin
        DoIdle;
      end;
    end;
  end;
end;

procedure TSessionBase.HandleMessage(const &message: IMessage);
begin
  Assert(message <> nil, 'message should not be nil.');
  { TODO: Find a handler to handle the message }
end;

procedure TSessionBase.Execute;
begin
  try
    DoCommunicate;
  except
    on e: Exception do
    begin
      // TODO: Handle Exception (ETimeoutException, ECommunicationError, etc.)
      Terminate;
      Connection.Close;
      raise;
    end;
  end;
end;

procedure TSessionBase.Terminate;
begin
  fTerminated := True;
end;

procedure TSessionBase.Send(const msg: IMessage);
var
  responseMessage: IMessage;
  offset: Integer;
  hasNextPacket: Boolean;
begin
  TArgument.CheckNotNull(msg, 'message');
  offset := 0;
  hasNextPacket := True;
  while not Terminated and hasNextPacket do
  begin
    fProtocol.Pack(msg, fSendBuffer, offset, hasNextPacket);
    fConnection.SendBuffer(fSendBuffer.Memory, fSendBuffer.Size);
    if hasNextPacket then
    begin
      GetNextMessage(responseMessage);
      HandleMessage(responseMessage);
    end;
  end;
end;

procedure TSessionBase.Receive(out msg: IMessage);
begin
  GetNextMessage(msg);
end;

function TSessionBase.GetConnection: IConnection;
begin
  Result := fConnection;
end;

{$ENDREGION}


{$REGION 'TServerSession'}

//procedure TServerSession.DoExecute;
//var
//  stream: TMemoryStream;  // TBufferStream
//begin
//  stream := TMemoryStream.Create;
//  try
//    if GetNextDataPacket(stream) then
//    begin
//      HandleDataPacket(stream);
//      stream.Clear;
//    end;
//  finally
//    stream.Free;
//  end;
//end;

{$ENDREGION}


{$REGION 'TDataPacketReader'}

constructor TDataPacketReader.Create(const connection: IConnection);
begin
  inherited Create;
  fConnection := connection;
  fDataStream := TReusableMemoryStream.Create;
end;

destructor TDataPacketReader.Destroy;
begin
  fDataStream.Free;
  inherited Destroy;
end;

function TDataPacketReader.GetNextDataPacket: Boolean;
begin
  fDataStream.Reset;
  fConnection.ReceiveBuffer(fDataStream.Memory, fDataStream.Size);
  Result := True;
end;

function TDataPacketReader.GetDataStream: TCustomMemoryStream;
begin
  Result := fDataStream;
end;

{$ENDREGION}


{$REGION 'TReusableMemoryStream'}

constructor TReusableMemoryStream.Create;
begin
  inherited Create;
  Capacity := 4096;
end;

procedure TReusableMemoryStream.Reset;
begin
  // TEMP
  Size := 0;
  Capacity := 4096;
end;

{$ENDREGION}


{$REGION 'TSimpleCommunicationProtocol'}

function TSimpleCommunicationProtocol.CreateMessage(messageType: TMessageType): IMessage;
begin
  Result := nil;
end;

procedure TSimpleCommunicationProtocol.Pack(const &message: IMessage;
  packetStream: TStream; var offset: Integer; out hasNextPacket: Boolean);
var
  bytesLeft: Int64;
  dataSize: Integer;
begin
  TArgument.CheckNotNull(&message, 'message');
  TArgument.CheckNotNull(packetStream, 'packetStream');
  TArgument.CheckRange((offset >= 0) and (offset <= &message.Size), 'offset');

  bytesLeft := &message.Size - offset;
  dataSize := Min(bytesLeft, MaxPacketSize-fCMetaDataSize);
//  packetBuffer.Position := 0;
//  packetBuffer.Write(PByte(messageStream.Memory)[messageStream.Position], dataSize);
//  hasPackets := messageStream.Position < messageStream.Size;
end;

procedure TSimpleCommunicationProtocol.Unpack(packetBuffer: Pointer;
  packetSize: Integer; messageStream: TStream; out packetInfo: TPacketInfo);
var
  header: THeaderRec;
  footer: TFooterRec;
begin
  TArgument.CheckNotNull(packetBuffer, 'packetBuffer');
  TArgument.CheckRange(packetSize >= 0, 'packetSize');
  TArgument.CheckNotNull(messageStream, 'messageStream');

  packetInfo.PacketType := ptInvalid;
  packetInfo.MessageType := -1;  // TEMP
  if packetSize >= fCMetaDataSize + SizeOf(TMessageType) then
  begin
    Move(packetBuffer^, header, SizeOf(header));
    Move(PByte(packetBuffer)[packetSize-SizeOf(footer)], footer, SizeOf(footer));
    if (header.Flag = fCHeaderFlag) and (footer.Flag = fCFooterFlag) and
      (header.Position + header.DataSize <= header.TotalSize) then
    begin
      { TODO: Extract MessageType }
      Move(PByte(packetBuffer)[SizeOf(header)], packetInfo.MessageType, SizeOf(packetInfo.MessageType));
      { TODO: Validate footer.Code; }
      if header.Position + header.DataSize = header.TotalSize then
      begin
        { TODO: Determines whether it is a NOP }
        packetInfo.PacketType := ptMessage;
      end
      else
      begin
        packetInfo.PacketType := ptMessagePart;
      end;
      messageStream.Write(PByte(packetBuffer)[SizeOf(THeaderRec) + SizeOf(TMessageType)],
        packetSize - fCMetaDataSize - SizeOf(TMessageType));
    end;
  end;
end;

function TSimpleCommunicationProtocol.GetMaxPacketSize: Integer;
begin
  Result := fCMaxPacketSize;
end;

{$ENDREGION}

end.
