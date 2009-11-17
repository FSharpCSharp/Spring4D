{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

{ TODO: Complete TServiceController class }
{ TODO: TClipboardWatcher }
{ TODO: Single Application Instance }

unit Spring.Utils;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Controls,
  Dialogs,
  WinSvc,
  IOUtils,
  Generics.Collections,
  Spring.System,
  Spring.Win32API;

type
  {$REGION 'TMessageBox'}

  TMessageDialogButton  = Dialogs.TMsgDlgBtn;
  TMessageDialogButtons = Dialogs.TMsgDlgButtons;

  TMessageDialogShowProc = reference to function(const text, caption: string;
    dialogType: TMsgDlgType; buttons: TMessageDialogButtons;
    defaultButton: TMessageDialogButton): TModalResult;

  /// <summary>
  /// Encapsulates common message dialogs.
  /// </summary>
  TMessageBox = class
  private
    class constructor Create;
    class var fMessageDialogProc: TMessageDialogShowProc;
    class function GetDefaultButton(const buttons: TMessageDialogButtons): TMessageDialogButton; inline;
    class function ShowMessageDialog(const text, caption: string;
      dialogType: TMsgDlgType; buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult;
  public
    { Information Message Box }
    class function Info(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Info(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Info(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Info(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Warning Message Box }
    class function Warn(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Warn(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Warn(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Warn(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Error Message Box }
    class function Error(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Error(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Error(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Error(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Confirm Message Box }
    class function Confirm(const text: string; const buttons: TMessageDialogButtons = [mbYes, mbNo]): TModalResult; overload;
    class function Confirm(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Confirm(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Confirm(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
  public
    class property MessageDialogProc: TMessageDialogShowProc read fMessageDialogProc write fMessageDialogProc;
  end;

  TDialog = TMessageBox deprecated 'Use TMessageBox instead.';

const
  { Copied from Dialogs.pas }
  mbYesNo = [mbYes, mbNo];
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  mbAbortIgnore = [mbAbort, mbIgnore];

const
  { Copied from Controls.pas }
  mrNone     = 0;
  mrOk       = IDOK;
  mrCancel   = IDCANCEL;
  mrAbort    = IDABORT;
  mrRetry    = IDRETRY;
  mrIgnore   = IDIGNORE;
  mrYes      = IDYES;
  mrNo       = IDNO;
  mrAll      = mrNo + 1;
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;
  mrClose    = mrYesToAll + 1;

  {$ENDREGION}


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


  TFileMapping        = class;
  TFileMappingView    = class;
  TFileMappingStream  = class;

  TFileMappingAccess = (
    ReadWrite,	      // Read and write access to the file.
    Read,             // Read-only access to the file.
    Write,            // Write-only access to file.
    CopyOnWrite,      // Read and write access to the file, with the restriction that any write operations will not be seen by other processes.
    ReadExecute,      // Read access to the file that can store and run executable code.
    ReadWriteExecute	// Read and write access to the file that can can store and run executable code.
  );

  /// <summary>
  /// Represents a memory-mapped file.
  /// </summary>
  TFileMapping = class
  private
    fHandle: THandle;
    fViews: TList<TFileMappingView>;
  protected
    procedure Notify(view: TFileMappingView; action: TCollectionNotification); virtual;
  public
    constructor Create(fileHandle: THandle; access: TFileMappingAccess; const maximumSize: Int64);
    destructor Destroy; override;
    function GetFileView(const offset: Int64; size: Cardinal): TFileMappingView;
    property Handle: THandle read fHandle;
  end;

  /// <summary>
  /// Represents a file view of a file mapping object.
  /// </summary>
  /// <remarks>
  /// The offset must be a multiple of fCAllocationGranularity.
  /// </remarks>
  TFileMappingView = class
  private
    fFileMapping: TFileMapping;
    fMemory: Pointer;   // Base Address
  private
    class var
      fCAllocationGranularity: Cardinal;  // Memory Allocation Granularity of the System
    class constructor Create;
  public
    constructor Create(fileMapping: TFileMapping; const offset: Int64; size: Cardinal);
    destructor Destroy; override;
    procedure Flush;
    property Memory: Pointer read fMemory;
  end;

  // NOT READY
  TFileMappingStream = class(TCustomMemoryStream)
  private
    fFileMapping: TFileMapping;
    fFileName: string;
    fFileHandle: THandle;
  protected
    function CreateFileHandle(const fileName: string; mode: TFileMode;
      access: TFileAccess; share: TFileShare): THandle;
  public
    constructor Create(const fileName: string; mode: TFileMode); overload;
    constructor Create(const fileName: string; mode: TFileMode; access: TFileAccess); overload;
    constructor Create(const fileName: string; mode: TFileMode; access: TFileAccess; share: TFileShare); overload;
    constructor Create(const fileName: string; mode: TFileMode; access: TFileAccess; share: TFileShare; fileMappingAccess: TFileMappingAccess); overload;
    destructor Destroy; override;
    property FileName: string read fFileName;
  end;

var
  GetPublicIPAddressMethod: function: string;
  ExtractIPAddressMethod:   function(const htmlText: string): string;

var
  DefaultPingUrl: string            = 'http://www.google.com';  // DO NOT LOCALIZE
  DefaultPingUrl2: string           = 'http://www.sina.com';    // DO NOT LOCALIZE
  DefaultPublicIPAddressUrl: string = 'http://www.whatismyip.com/automation/n09230945.asp'; // DO NOT LOCALIZE


implementation

uses
  ActiveX,
  ComObj,
  WinSock,
  WinInet,
  Spring.ResourceStrings;

{$REGION 'Routines'}

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

{$ENDREGION}


{$REGION 'TMessageBox'}

class constructor TMessageBox.Create;
begin
  fMessageDialogProc := ShowMessageDialog;
end;

class function TMessageBox.GetDefaultButton(
  const buttons: TMessageDialogButtons): TMessageDialogButton;
begin
  if mbOk in Buttons then
    Result := mbOk
  else if mbYes in Buttons then
    Result := mbYes
  else
    Result := mbRetry;
end;

class function TMessageBox.ShowMessageDialog(const text, caption: string;
  dialogType: TMsgDlgType; buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  if caption = '' then
  begin
    Result := MessageDlg(text, dialogType, buttons, -1, defaultButton);
  end
  else
  begin
    Result := Dialogs.TaskMessageDlg(caption, text, dialogType, buttons, -1, defaultButton);
  end;
end;

class function TMessageBox.Confirm(const text, caption: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Confirm(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Confirm(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtConfirmation, buttons, defaultButton);
end;

class function TMessageBox.Confirm(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Confirm(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Confirm(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Confirm(text, '', buttons, defaultButton);
end;

class function TMessageBox.Info(const text: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Info(const text: string;
  const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Info(text, '', buttons, defaultButton);
end;

class function TMessageBox.Info(const text, caption: string;
  const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtInformation, buttons, defaultButton);
end;

class function TMessageBox.Info(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Warn(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Warn(text, '', buttons, defaultButton);
end;

class function TMessageBox.Warn(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Warn(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtWarning, buttons, defaultButton);
end;

class function TMessageBox.Warn(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Error(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtError, buttons, defaultButton);
end;

class function TMessageBox.Error(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Error(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Error(text, '', buttons, defaultButton);
end;

{$ENDREGION}


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


{$REGION 'TFileMapping'}

constructor TFileMapping.Create(fileHandle: THandle; access: TFileMappingAccess;
  const maximumSize: Int64);
//var
//  fSecurityAttributes: TSecurityAttributes;
begin
  inherited Create;
  fHandle := CreateFileMapping(
    fileHandle,
    nil,
    PAGE_READWRITE,
    TInt64Rec(maximumSize).Hi,
    TInt64Rec(maximumSize).Lo,
    nil
  );
  Win32Check(fHandle <> 0);
end;

destructor TFileMapping.Destroy;
begin
  if fHandle <> 0 then
  begin
    CloseHandle(fHandle);
  end;
  fViews.Free;
  inherited Destroy;
end;

procedure TFileMapping.Notify(view: TFileMappingView; action: TCollectionNotification);
begin
  if fViews = nil then
  begin
    fViews := TObjectList<TFileMappingView>.Create(True);
  end;
  case action of
    cnAdded:
    begin
      fViews.Add(view);
    end;
    cnRemoved, cnExtracted:
    begin
      fViews.Remove(view);
    end;
  end;
end;

function TFileMapping.GetFileView(const offset: Int64; size: Cardinal): TFileMappingView;
begin
  Result := TFileMappingView.Create(Self, offset, size);   // Its lifecycle is automatically managed by fViews.
end;

{$ENDREGION}


{$REGION 'TFileMappingView'}

class constructor TFileMappingView.Create;
var
  systemInfo: TSystemInfo;
begin
  GetSystemInfo(systemInfo);
  fCAllocationGranularity := systemInfo.dwAllocationGranularity;
end;

constructor TFileMappingView.Create(fileMapping: TFileMapping;
  const offset: Int64; size: Cardinal);
var
  desiredAccess: DWORD;
begin
  TArgument.CheckNotNull(fileMapping, 'fileMapping');

  inherited Create;
  fFileMapping := fileMapping;
  desiredAccess := FILE_MAP_ALL_ACCESS;    // TEMP
  fMemory := MapViewOfFile(
    fileMapping.Handle,
    desiredAccess,
    Int64Rec(offset).Hi,
    Int64Rec(offset).Lo,
    size
  );
  Win32Check(fMemory <> nil);

  fFileMapping.Notify(Self, cnAdded);
end;

destructor TFileMappingView.Destroy;
begin
  if fMemory <> nil then
  begin
    UnmapViewOfFile(fMemory);
  end;
  fFileMapping.Notify(Self, cnRemoved);
  inherited Destroy;
end;

procedure TFileMappingView.Flush;
begin
  Win32Check(FlushViewOfFile(fMemory, 0));
end;

{$ENDREGION}


{$REGION 'TFileMappingStream'}

constructor TFileMappingStream.Create(const fileName: string; mode: TFileMode);
begin
  Create(fileName, mode, TFileAccess.faReadWrite, TFileShare.fsNone, TFileMappingAccess.ReadWrite);
end;

constructor TFileMappingStream.Create(const fileName: string; mode: TFileMode;
  access: TFileAccess);
begin
  Create(fileName, mode, access, TFileShare.fsNone, TFileMappingAccess.ReadWrite);
end;

constructor TFileMappingStream.Create(const fileName: string; mode: TFileMode;
  access: TFileAccess; share: TFileShare);
begin
  Create(fileName, mode, access, share, TFileMappingAccess.ReadWrite);
end;

constructor TFileMappingStream.Create(const fileName: string; mode: TFileMode;
  access: TFileAccess; share: TFileShare;
  fileMappingAccess: TFileMappingAccess);
begin
  inherited Create;
  fFileName := fileName;
  fFileHandle := CreateFileHandle(fileName, mode, access, share);
//  fFileMapping := TFileMapping.Create(fFileMapping, fileMappingAccess, 0);
end;

destructor TFileMappingStream.Destroy;
begin
  fFileMapping.Free;
  CloseHandle(fFileHandle);
  inherited Destroy;
end;

function TFileMappingStream.CreateFileHandle(const fileName: string;
  mode: TFileMode; access: TFileAccess; share: TFileShare): THandle;
var
  fileMode: Word;
  fileRights: Word;
const
  FileAccessMappings: array[TFileAccess] of Word = (
    fmOpenRead,         // faRead
    fmOpenWrite,        // faWrite
    fmOpenReadWrite     // faReadWrite
  );

  FileShareMappings: array[TFileShare] of Word = (
    fmShareExclusive,   // fsNone
    fmShareDenyWrite,   // fsRead
    fmShareDenyRead,    // fsWrite
    fmShareDenyNone     // fsReadWrite
  );
begin
  fileMode := FileAccessMappings[access];
  fileRights := FileShareMappings[share];
//  if mode = TFileMode.fmCreateNew then
  Result := THandle(SysUtils.FileCreate(fileName, fileMode, fileRights));
end;

{$ENDREGION}

initialization
  GetPublicIPAddressMethod := _GetPublicIPAddress;
  ExtractIPAddressMethod := _ExtractIPAddress;
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
