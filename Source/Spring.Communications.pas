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

interface

uses
  Classes,
  SysUtils,
  Spring.System;

type
  ITrackBytes = interface;
  IConnection = interface;
  ISession = interface;
  IDataPacket = interface;
  IMessage = interface;
  IMessageHandler = interface;

//  IRunnable = interface
//    procedure Run;
//  end;

  /// <summary>
  /// Tracks network activity
  /// </summary>
  ITrackBytes = interface
    function GetBytesSent: Int64;
    function GetBytesReceived: Int64;
    property BytesSent: Int64 read GetBytesSent;
    property BytesReceived: Int64 read GetBytesReceived;
  end;

  /// <summary>
  /// Represents an incoming or outgoing connection.
  /// </summary>
  IConnection = interface(ITrackBytes)
    procedure Open;
    procedure Close;
    procedure Send(const dataPacket: IDataPacket);
    procedure Recieve(out dataPacket: IDataPacket);
  end;

  /// <summary>
  /// Represents a business session between server side and client side.
  /// </summary>
  ISession = interface

  end;

  /// <summary>
  /// Represents a single physical data packet
  /// </summary>
  IDataPacket = interface
    ['{8ED02ECF-59D7-4943-B26D-8AF85F03D7E8}']
//    function GetAsBuffer: TBuffer;
//    property AsBuffer: TBuffer read GetAsBuffer;
  end;

  ITransmission = interface

  end;

  TDataPacket = class

  end;

  /// <summary>
  /// Represents an integrated logical domain data.
  /// </summary>
  IMessage = interface
    ['{98A357D4-EFF6-4602-85FD-ABBDDD557267}']
  end;

  /// <summary>
  /// Represents a handler for a message
  /// </summary>
  IMessageHandler = interface
    ['{DDC0C554-C9F8-459F-9CBA-D4C1F62C6F57}']
    procedure HandleMessage(const msg: IMessage);
  end;

  TMessageBuffer = class

  end;

  /// <summary>
  /// Represents a communications server.
  /// </summary>
  ICommunicationsServer = interface
    procedure Start;
    procedure ShutDown;
  end;

  /// <summary>
  /// Represents a communications client.
  /// </summary>
  ICommunicationsClient = interface

  end;

implementation

end.
