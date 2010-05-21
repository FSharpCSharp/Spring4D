{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Utils.Network;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Spring;

type
  {$REGION 'TNetwork (Experimental)'}

  TNetworkStatus = (
    nsOnline,
    nsOffline
  );

  TNetwork = class sealed
  private
    class function GetIsAvailable: Boolean; static;
    class function GetStatus: TNetworkStatus; static;
    class property Status: TNetworkStatus read GetStatus;
  public
    class function GetMacAddress: string; overload;
//    function GetMacAddress(const hostNameOrAddress: string): string; overload; static;
    class function GetIPAddress: string; overload;
//    function GetIPAddress(const hostName: string): string; overload; static;
    class function GetPublicIPAddress: string;
//    class procedure Ping(const hostNameOrAddress: string);
    class property IsAvailable: Boolean read GetIsAvailable;
  end experimental;

  {$ENDREGION}


{$REGION 'Temp global variables'}

var
  GetPublicIPAddressMethod: function: string;
  ExtractIPAddressMethod:   function(const htmlText: string): string;

var
  DefaultPingUrl: string            = 'http://www.google.com';  // DO NOT LOCALIZE
  DefaultPingUrl2: string           = 'http://www.sina.com';    // DO NOT LOCALIZE
  DefaultPublicIPAddressUrl: string = 'http://www.whatismyip.com/automation/n09230945.asp'; // DO NOT LOCALIZE

{$ENDREGION}


implementation

uses
  ActiveX,
  ComObj,
  WinSock,
  WinInet,
  Spring.Win32API,
  Spring.Utils;


function _GetPublicIPAddress: string;
var
  xml: OleVariant;
begin
  xml := CreateOleObject('Microsoft.XMLHTTP');
  xml.Open('GET', DefaultPublicIPAddressUrl, False);
  xml.Send;
  Result := ExtractIPAddressMethod(xml.responseText);
end;

function _ExtractIPAddress(const htmlText: string): string;
begin
  Result := Trim(htmlText);
end;


{$REGION 'TNetwork'}

class function TNetwork.GetMacAddress: string;
var
  adapterInfo: array[0..3] of IP_ADAPTER_INFO;
  pAdapterInfo: PIP_ADAPTER_INFO;
  len: Cardinal;
  dwBufLen: Cardinal;
  dwStatus: DWORD;
begin
  Result := '';
  FillChar(adapterInfo, SizeOf(adapterInfo), 0);
  pAdapterInfo := @adapterInfo[0];
  dwBufLen := SizeOf(adapterInfo);
  dwStatus := GetAdaptersInfo(pAdapterInfo, dwBufLen);
  if (dwStatus = ERROR_SUCCESS) and (pAdapterInfo <> nil) then
  begin
    len := 0;
    while len < pAdapterInfo.AddressLength do
    begin
      Result := Result + IntToHex(PByte(@pAdapterInfo.Address[len])^, 2) + '-';
      Inc(len, 1);
    end;
    SetLength(Result, Length(Result) - 1);
  end;
end;

class function TNetwork.GetPublicIPAddress: string;
begin
  Result := GetPublicIPAddressMethod;
end;

class function TNetwork.GetIPAddress: string;
var
  rc: Integer;
  data: TWSAData;
  hostName: AnsiString;
  hostEnt: PHostEnt;
  sockAddr: TSockAddrIn;
begin
  rc := Winsock.WSAStartup(MakeWord(1, 1), data);
  if rc = 0 then
  try
    hostName := AnsiString(Environment.MachineName);
    hostEnt := Winsock.GetHostByName(PAnsiChar(hostName));
    if hostEnt <> nil then
    begin
      sockAddr.sin_addr.S_addr := Longint(PLongint(hostEnt^.h_addr_list^)^);
      Result := string(Winsock.inet_ntoa(sockAddr.sin_addr));
    end;
  finally
    Winsock.WSACleanup;
  end;
end;

class function TNetwork.GetIsAvailable: Boolean;
begin
  Result := TNetwork.Status = nsOnline;
end;

class function TNetwork.GetStatus: TNetworkStatus;
var
  flags: DWORD;
  available: Boolean;
begin
  available := InternetGetConnectedState(@flags, 0) and
    (InternetCheckConnection(PChar(DefaultPingUrl), 1, 0) or
      (InternetCheckConnection(PChar(DefaultPingUrl2), 1, 0))
    );
  if available then
    Result := nsOnline
  else
    Result := nsOffline;
end;

{$ENDREGION}

initialization
  GetPublicIPAddressMethod := _GetPublicIPAddress;
  ExtractIPAddressMethod := _ExtractIPAddress;
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
