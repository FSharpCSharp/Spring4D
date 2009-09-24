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

unit Spring.Communications.Tests;

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring.System,
  Spring.Communications;

type
  TTestCommunicationsServer = class(TTestCase)
  strict private
    fServer: ICommunicationsServer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnection;
  end;

  TMockMessage = class(TInterfaceBase, IMessage)
  private
    fStream: TStream;
    function GetSize: Integer;
  public
    constructor Create(stream: TStream);
    destructor Destroy; override;
    procedure SaveToStream(stream: TStream; startIndex, count: Integer);
    property Size: Integer read GetSize;
  end;

  TTestSimpleDataPacketBuilder = class(TTestCase)
  strict private
//    fBuilder: TSimpleDataPacketBuilder;
    fStream: TStringStream;
    fMessage: TMockMessage;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOnePacket;
  end;

implementation

procedure TTestCommunicationsServer.SetUp;
begin
end;

procedure TTestCommunicationsServer.TearDown;
begin
end;

procedure TTestCommunicationsServer.TestConnection;
begin

end;

{ TTestSimpleDataPacketBuilder }

procedure TTestSimpleDataPacketBuilder.SetUp;
begin
  inherited;
//  fBuilder := TSimpleDataPacketBuilder.Create;
  fStream := TStringStream.Create;
  fMessage := TMockMessage.Create(fStream);
end;

procedure TTestSimpleDataPacketBuilder.TearDown;
begin
  inherited;
//  fBuilder.Free;
  fStream.Free;
  fMessage.Free;
end;

procedure TTestSimpleDataPacketBuilder.TestOnePacket;
begin
//  fStream.WriteString('Hello, World!');
//  fBuilder.ToDataPacket;
//  fBuilder.HeaderSize
//  fBuilder.ToDataPacket;
end;

{ TMockMessage }

constructor TMockMessage.Create(stream: TStream);
begin
  inherited Create;
  fStream := stream;
end;

destructor TMockMessage.Destroy;
begin
  inherited Destroy;
end;

function TMockMessage.GetSize: Integer;
begin
  Result := fStream.Size;
end;

procedure TMockMessage.SaveToStream(stream: TStream; startIndex,
  count: Integer);
begin
  fStream.Position := startIndex;
  stream.CopyFrom(fStream, count);
end;

initialization
  RegisterTest(TTestCommunicationsServer.Suite);

end.

