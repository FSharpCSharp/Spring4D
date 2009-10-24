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
  WinSvc,
  Spring.Win32API,
  Spring.System;

type
  {$REGION 'TServiceController (NOT COMPLETED)'}

  TServiceType = (
    stKernelDriver,       // A Kernel device driver such as a hard disk or other low-level hardware device driver.
    stFileSystemDriver,   // A file system driver, which is also a Kernel device driver.
    stAdapter,            // A service for a hardware device that requires its own driver.
    stRecognizerDriver,   // A file system driver used during startup to determine the file systems present on the system.
    stWin32OwnProcess,    // A Win32 program that can be started by the Service Controller and that obeys the service control protocol. This type of Win32 service runs in a process by itself.
    stWin32ShareProcess,  // A Win32 service that can share a process with other Win32 services.
    stInteractiveProcess  // A service that can communicate with the desktop.
  );

  TServiceTypes = set of TServiceType;

  TServiceStatus = (
    ssUnknown,            // The status is unknown.
    ssStopped,            // The service is not running.
    ssStartPending,       // The service is starting.
    ssStopPending,        // The service is stopping.
    ssRunning,            // The service is running.
    ssContinuePending,    // The service continue is pending.
    ssPausePending,       // The service pause is pending.
    ssPaused              // The service is paused.
  );

  TServiceStartMode = (
    smBoot,
    smSystem,
    smAutomatic,
    smManual,
    smDisabled
  );

  TServiceAccount = (
    LocalService,         // An account that acts as a non-privileged user on the local computer, and presents anonymous credentials to any remote server.
    NetworkService,       // An account that provides extensive local privileges, and presents the computer's credentials to any remote server.
    LocalSystem,          // An account, used by the service control manager, that has extensive privileges on the local computer and acts as the computer on the network.
    User                  // An account defined by a specific user on the network.
  );

  TServiceControlAccepted = (
    caStop,               // SERVICE_ACCEPT_STOP
    caPauseAndResume,     // SERVICE_ACCEPT_PAUSE_CONTINUE
    caShutdown            // SERVICE_ACCEPT_SHUTDOWN
  );

  TServiceControls = set of TServiceControlAccepted;

  /// <summary>
  /// Represents a Windows service and allows you to connect to a running or
  /// stopped service, manipulate it, or get information about it.
  /// </summary>
  TServiceController = record
  private
    type
      TServiceKind = (
        skDrive,  // SERVICE_DRIVER (0x0000000B)
        skWin32   // SERVICE_WIN32  (0x00000030)
      );
      TServiceAction = (
        saStart,
        saStop,
        saPause,
        saResume
      );
    const
      fCInvalidServiceHandle: THandle = 0;
      fCPendingStatusSet = [
        ssStartPending,
        ssStopPending,
        ssContinuePending,
        ssPausePending
      ];
  strict private
    fManagerHandle: TValueHolder<THandle>;
    fServiceHandle: TValueHolder<THandle>;
    fServiceStatusProcess: TServiceStatusProcess;
    fServiceConfig: TValueHolder<PQueryServiceConfig>;
    fStatus: TServiceStatus;
    fDisplayName: string;
    fServiceName: string;
    fMachineName: string;
    fDescription: string;
    fFileName:    string;
    fServiceType: TServiceTypes;
    fStartMode: TServiceStartMode;
    fCanStop: Boolean;
    fCanPauseAndResume: Boolean;
    fCanShutdown: Boolean;
    fDependentServices: TArray<TServiceController>;
    fServicesDependedOn: TArray<TServiceController>;
    function GetDisplayName: string;
    function GetMachineName: string;
    function GetDescription: string;
    function GetServiceName: string;
    function GetServiceType: TServiceTypes;
    function GetServiceHandle: THandle;
    function GetStatus: TServiceStatus;
    function GetStartMode: TServiceStartMode;
    function GetCanPauseAndResume: Boolean;
    function GetCanStop: Boolean;
    function GetCanShutdown: Boolean;
    function GetExists: Boolean;
    function GetDependentServices: TArray<TServiceController>;
    function GetServicesDependedOn: TArray<TServiceController>;
  private
    fIsLoaded: Boolean;
    procedure Loaded;
    procedure ManagerHandleNeeded;
    procedure Open(desiredAccess: Cardinal);
    procedure Close;
    procedure PerformAction(action: TServiceAction);
    procedure UpdateStatus;
    procedure UpdateConfig;
    procedure UpdateDescription;
    procedure UpdateDependentServices;
    function ParseStatus(const value: Cardinal): TServiceStatus;
    function ParseServiceTypes(const value: Cardinal): TServiceTypes;
    function ParseStartMode(const value: Cardinal): TServiceStartMode;
  private
    constructor Create(const serviceName, machineName: string; const managerHandle: TValueHolder<THandle>); overload;
    class procedure GetManagerHandleHolder(const machineName: string; out handleHolder: TValueHolder<THandle>); static;
    class procedure GetHandleHolder(handle: THandle; out valueHolder: TValueHolder<THandle>); static;
    class procedure InternalEnumServices(const machineName: string; serviceKind: TServiceKind; out services: TArray<TServiceController>); static;
  public
    constructor Create(const serviceName: string); overload;
    constructor Create(const serviceName, machineName: string); overload;
    class function GetDevices: TArray<TServiceController>; overload; static;
    class function GetDevices(const machineName: string): TArray<TServiceController>; overload; static;
    class function GetServices: TArray<TServiceController>; overload; static;
    class function GetServices(const machineName: string): TArray<TServiceController>; overload; static;
    procedure ExecuteCommand(command: Integer);
    procedure Start; overload;
    procedure Start(const args: array of string); overload;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    procedure Restart;
    procedure Refresh;
    procedure WaitForStatus(status: TServiceStatus); overload;
    procedure WaitForStatus(desiredStatus: TServiceStatus; const timeout: TTimeSpan); overload;
    property DisplayName: string read GetDisplayName;
    property Description: string read GetDescription;
    property MachineName: string read GetMachineName;
    property ServiceName: string read GetServiceName;
    property Status: TServiceStatus read GetStatus;
    property ServiceHandle: THandle read GetServiceHandle;
    property ServiceType: TServiceTypes read GetServiceType;
    property StartMode: TServiceStartMode read GetStartMode;
    property CanStop: Boolean read GetCanStop;
    property CanPauseAndResume: Boolean read GetCanPauseAndResume;
    property CanShutdown: Boolean read GetCanShutdown;
    property Exists: Boolean read GetExists;
    property DependentServices: TArray<TServiceController> read GetDependentServices;
    property ServicesDependedOn: TArray<TServiceController> read GetServicesDependedOn;
  end;

  {$ENDREGION}


implementation

uses
  Spring.ResourceStrings;

{$REGION 'TServiceController'}

constructor TServiceController.Create(const serviceName: string);
begin
  Create(serviceName, '');
end;

constructor TServiceController.Create(const serviceName, machineName: string);
begin
  Create(serviceName, machineName, fCInvalidServiceHandle);
end;

constructor TServiceController.Create(const serviceName,
  machineName: string; const managerHandle: TValueHolder<THandle>);
begin
  fServiceName := serviceName;
  fMachineName := machineName;
  fManagerHandle := managerHandle;
  fIsLoaded := False;
end;

class procedure TServiceController.GetManagerHandleHolder(
  const machineName: string; out handleHolder: TValueHolder<THandle>);
var
  handle: THandle;
begin
  handle := OpenSCManager(
    PChar(machineName),
    SERVICES_ACTIVE_DATABASE,
    SC_MANAGER_ALL_ACCESS
  );
  Win32Check(handle <> fCInvalidServiceHandle);
  GetHandleHolder(handle, handleHolder);
end;

class procedure TServiceController.GetHandleHolder(handle: THandle;
  out valueHolder: TValueHolder<THandle>);
begin
  valueHolder := TValueHolder<THandle>.Create(
    handle,
    procedure(var serviceHandle: THandle)
    begin
      if serviceHandle <> fCInvalidServiceHandle then
      begin
        Win32Check(CloseServiceHandle(serviceHandle));
      end;
      serviceHandle := fCInvalidServiceHandle;
    end
  );
end;

class procedure TServiceController.InternalEnumServices(const machineName: string;
  serviceKind: TServiceKind; out services: TArray<TServiceController>);
var
  managerHandle: TValueHolder<THandle>;
  bufferSize: DWORD;
  bytesNeeded: DWORD;
  servicesReturned: DWORD;
  resumeHandle: THandle;
  returnValue: BOOL;
  serviceName: string;
  serviceArray, pService: PEnumServiceStatusProcess;
  i: Integer;
const
  serviceTypes: array[TServiceKind] of Cardinal = (
    SERVICE_DRIVER,
    SERVICE_WIN32
  );
begin
  TServiceController.GetManagerHandleHolder(machineName, managerHandle);
  resumeHandle := 0;  // Set resumeHandle to zero the first time
  returnValue := EnumServicesStatusEx(
    managerHandle,
    SC_ENUM_PROCESS_INFO,
    serviceTypes[serviceKind],
    SERVICE_STATE_ALL,
    nil,
    0,
    bytesNeeded,
    servicesReturned,
    resumeHandle,
    nil
  );
  if not returnValue and (GetLastError <> ERROR_MORE_DATA) then
  begin
    RaiseLastOSError;
  end;
  bufferSize := bytesNeeded;
  serviceArray := AllocMem(bytesNeeded);
  try
    returnValue := EnumServicesStatusEx(
      managerHandle,
      SC_ENUM_PROCESS_INFO,
      serviceTypes[serviceKind],
      SERVICE_STATE_ALL,
      PByte(serviceArray),
      bufferSize,
      bytesNeeded,
      servicesReturned,
      resumeHandle,
      nil
    );
    Win32Check(returnValue);
    SetLength(services, servicesReturned);
    pService := serviceArray;
    for i := 0 to servicesReturned - 1 do
    begin
      serviceName := pService.lpServiceName;
      services[i] := TServiceController.Create(serviceName, machineName, managerHandle);
      Inc(pService);
    end;
  finally
    FreeMem(serviceArray);
  end;
end;

procedure TServiceController.ManagerHandleNeeded;
begin
  if fManagerHandle.Value = fCInvalidServiceHandle then
  begin
    GetManagerHandleHolder(fMachineName, fManagerHandle);
    Assert(fManagerHandle.Value <> fCInvalidServiceHandle);
  end;
end;

procedure TServiceController.UpdateStatus;
var
  bytesNeeded: Cardinal;
begin
  Assert(fServiceHandle.Value <> fCInvalidServiceHandle, 'Invalid fServiceHandle');
  Win32Check(QueryServiceStatusEx(
    fServiceHandle,
    SC_STATUS_PROCESS_INFO,
    PByte(@fServiceStatusProcess),
    SizeOf(fServiceStatusProcess),
    bytesNeeded
  ));
  fStatus := ParseStatus(fServiceStatusProcess.dwCurrentState);
  fCanStop := fServiceStatusProcess.dwControlsAccepted and SERVICE_ACCEPT_SHUTDOWN <> 0;
  fCanPauseAndResume := fServiceStatusProcess.dwControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE <> 0;
  fCanShutdown := fServiceStatusProcess.dwControlsAccepted and SERVICE_ACCEPT_SHUTDOWN <> 0;
end;

procedure TServiceController.UpdateConfig;
var
  bufferSize,
  bytesNeeded: Cardinal;
  serviceConfig: WinSvc.PQueryServiceConfig;
  list: TStringDynArray;
  serviceName: string;
  i: Integer;
begin
  Assert(fServiceHandle.Value <> fCInvalidServiceHandle, 'Invalid fServiceHandle');
  if not QueryServiceConfig(fServiceHandle, nil, 0, bytesNeeded) and
    (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
  begin
    RaiseLastOSError;
  end;
  bufferSize := bytesNeeded;
  serviceConfig := AllocMem(bytesNeeded);
  fServiceConfig := TValueHolder<WinSvc.PQueryServiceConfig>.Create(
    serviceConfig,
    procedure(var serviceConfig: WinSvc.PQueryServiceConfig)
    begin
      Dispose(serviceConfig);
    end
  );
  Win32Check(QueryServiceConfig(fServiceHandle, fServiceConfig, bufferSize, bytesNeeded));
  with fServiceConfig.Value^ do
  begin
    fServiceType := ParseServiceTypes(dwServiceType);
    fStartMode := ParseStartMode(dwStartType);
    fDisplayName := lpDisplayName;
    fFileName := lpBinaryPathName;
    list := SplitString(lpDependencies);
  end;
  // TODO: Handle SC_GROUP_IDENTIFIER
  SetLength(fServicesDependedOn, Length(list));
  for i := 0 to High(list) do
  begin
    serviceName := list[i];
    fServicesDependedOn[i] := TServiceController.Create(serviceName);
  end;
end;

procedure TServiceController.UpdateDependentServices;
begin
  // EnumDependentServices
end;

procedure TServiceController.UpdateDescription;
var
  bufferSize,
  bytesNeeded: Cardinal;
  buffer: TBytes;
begin
  Assert(fServiceHandle.Value <> fCInvalidServiceHandle, 'Invalid fServiceHandle');
  if not QueryServiceConfig2(fServiceHandle, SERVICE_CONFIG_DESCRIPTION, nil, 0, bytesNeeded) and
    (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
  begin
    RaiseLastOSError;
  end;
  bufferSize := bytesNeeded;
  SetLength(buffer, bufferSize);
  Win32Check(QueryServiceConfig2(fServiceHandle, SERVICE_CONFIG_DESCRIPTION, PByte(buffer), bufferSize, bytesNeeded));
  fDescription := PServiceDescription(buffer).lpDescription;
end;

procedure TServiceController.Loaded;
begin
  if not fIsLoaded then
  begin
    Refresh;
    fIsLoaded := True;
  end;
end;

procedure TServiceController.Open(desiredAccess: Cardinal);
begin
  Assert(desiredAccess and not SERVICE_ALL_ACCESS = 0, 'Invalid desiredAccess.');
  ManagerHandleNeeded;
  fServiceHandle := OpenService(fManagerHandle, PChar(fServiceName), desiredAccess);
  if fServiceHandle.Value = fCInvalidServiceHandle then
  case GetLastError of
    ERROR_SERVICE_DOES_NOT_EXIST:
    begin
      raise EOSError.CreateResFmt(@SServiceNotExists, [fServiceName]);
    end;
    else
    begin
      RaiseLastOSError;
    end;
  end;
end;

procedure TServiceController.Close;
begin
  if fServiceHandle.Value <> fCInvalidServiceHandle then
  begin
    CloseServiceHandle(fServiceHandle);
    fServiceHandle := fCInvalidServiceHandle;
  end;
end;

procedure TServiceController.Refresh;
begin
  Open(SERVICE_QUERY_STATUS or SERVICE_QUERY_CONFIG or SERVICE_CONFIG_DESCRIPTION);
  try
    UpdateStatus;
    UpdateConfig;
    UpdateDescription;
    UpdateDependentServices;
  finally
    Close;
  end;
end;

procedure TServiceController.ExecuteCommand(command: Integer);
begin

end;

procedure TServiceController.PerformAction(action: TServiceAction);
var
  serviceStatus: WinSvc.TServiceStatus;
const
  AccessArray: array[TServiceAction] of DWORD = (
    SERVICE_START,              // saStart
    SERVICE_STOP,               // saStop
    SERVICE_PAUSE_CONTINUE,     // saPause
    SERVICE_PAUSE_CONTINUE      // saResume
  );
  ControlArray: array[TServiceAction] of DWORD = (
    0,                          // saStart
    SERVICE_CONTROL_STOP,       // saStop
    SERVICE_CONTROL_PAUSE,      // saPause
    SERVICE_CONTROL_CONTINUE    // saResume
  );
begin
  Assert(action in [saStop, saPause, saResume]);
  Open(AccessArray[action] or SERVICE_QUERY_STATUS);
  try
    UpdateStatus;
    serviceStatus := WinSvc.PServiceStatus(@fServiceStatusProcess)^;
    Win32Check(ControlService(fServiceHandle, ControlArray[action], serviceStatus));
  finally
    Close;
  end;
end;

procedure TServiceController.Start;
begin
  Start([]);
end;

procedure TServiceController.Start(const args: array of string);
var
  pArgs: PChar;
  i: Integer;
  argCount: Integer;
type
  PStrArray = ^TStrArray;
  TStrArray = array[0..32767] of PChar;
begin
  Open(SERVICE_START or SERVICE_QUERY_STATUS);
  try
    if Length(args) = 0 then
    begin
      pArgs := nil;
      argCount := 0;
    end
    else
    begin
      argCount := Length(args) + 1;
      pArgs := AllocMem(argCount * SizeOf(PChar));
      PStrArray(pArgs)^[0] := PChar(fServiceName);
      for i := 0 to High(args) do
      begin
        PStrArray(pArgs)^[i+1] := PChar(args[i]);
      end;
    end;
    Win32Check(StartService(fServiceHandle, argCount, pArgs));
  finally
    FreeMem(pArgs);
    Close;
  end;
end;

procedure TServiceController.Stop;
begin
  PerformAction(saStop);
end;

procedure TServiceController.Pause;
begin
  PerformAction(saPause);
end;

procedure TServiceController.Resume;
begin
  PerformAction(saResume);
end;

procedure TServiceController.Restart;
begin
  Stop;
  WaitForStatus(ssStopped);
  Start;
end;

procedure TServiceController.WaitForStatus(status: TServiceStatus);
begin
  WaitForStatus(status, TTimeSpan.Create(Windows.INFINITE));
end;

procedure TServiceController.WaitForStatus(desiredStatus: TServiceStatus;
  const timeout: TTimeSpan);
var
  oldCheckPoint: DWORD;
  startTickCount: DWORD;
  waitTime: DWORD;
  ticks: DWORD;
//  n: Integer;
begin
  TArgument.CheckEnum<TServiceStatus>(desiredStatus, 'desiredStatus');
  Open(SERVICE_QUERY_STATUS);
  try
    UpdateStatus;
    startTickCount := GetTickCount;
    oldCheckPoint := fServiceStatusProcess.dwCheckPoint;
//    n := 0;
    while fStatus <> desiredStatus do
    begin
      // FROM <MSDN Library>:
      // Do not wait longer than the wait hint. A good interval is
      // one-tenth the wait hint, but no less than 1 second and no
      // more than 10 seconds.
      waitTime := fServiceStatusProcess.dwWaitHint div 10;
      if waitTime < 1000 then
        waitTime := 1000
      else if waitTime > 10000 then
        waitTime := 10000;
      Sleep(waitTime);
//      Inc(n);
      UpdateStatus;
      ticks := GetTickCount - startTickCount;
      if fStatus = desiredStatus then Break;
      if fServiceStatusProcess.dwCheckPoint > oldCheckPoint then
      begin
        // The service is making progress.
        startTickCount := GetTickCount;
        oldCheckPoint := fServiceStatusProcess.dwCheckPoint;
      end
      else if ticks > fServiceStatusProcess.dwWaitHint then
      begin
        // No progress made within the wait hint.
        Break;
      end
      else if ticks > timeout.Ticks  then
      begin
        raise ETimeoutException.CreateRes(@STimeoutException);
      end;
    end;
    if fStatus <> desiredStatus then
    begin
      RaiseLastOSError;
    end;
  finally
    Close;
  end;
end;

function TServiceController.ParseServiceTypes(const value: Cardinal): TServiceTypes;
begin
  Result := [];
  if value and SERVICE_ADAPTER <> 0 then
    Include(Result, stAdapter);
  if value and SERVICE_FILE_SYSTEM_DRIVER <> 0 then
    Include(Result, stFileSystemDriver);
  if value and SERVICE_INTERACTIVE_PROCESS <> 0 then
    Include(Result, stInteractiveProcess);
  if value and SERVICE_KERNEL_DRIVER <> 0 then
    Include(Result, stKernelDriver);
  if value and SERVICE_RECOGNIZER_DRIVER <> 0 then
    Include(Result, stRecognizerDriver);
  if value and SERVICE_WIN32_OWN_PROCESS <> 0 then
    Include(Result, stWin32OwnProcess);
  if value and SERVICE_WIN32_SHARE_PROCESS <> 0 then
    Include(Result, stWin32ShareProcess);
end;

function TServiceController.ParseStartMode(const value: Cardinal): TServiceStartMode;
begin
  case value of
    SERVICE_BOOT_START:   Result := smBoot;
    SERVICE_SYSTEM_START: Result := smSystem;
    SERVICE_AUTO_START:   Result := smAutomatic;
    SERVICE_DEMAND_START: Result := smManual;
    SERVICE_DISABLED:     Result := smDisabled;
    else
    begin
      raise EArgumentException.CreateResFmt(@SArgumentOutOfRangeException, [IntToStr(value)]);
    end;
  end;
end;

function TServiceController.ParseStatus(const value: Cardinal): TServiceStatus;
begin
  case value of
    SERVICE_STOPPED:           Result := ssStopped;
    SERVICE_START_PENDING:     Result := ssStartPending;
    SERVICE_STOP_PENDING:      Result := ssStopPending;
    SERVICE_RUNNING:           Result := ssRunning;
    SERVICE_CONTINUE_PENDING:  Result := ssContinuePending;
    SERVICE_PAUSE_PENDING:     Result := ssPausePending;
    SERVICE_PAUSED:            Result := ssPaused;
    else
    begin
      raise EArgumentException.CreateResFmt(@SArgumentOutOfRangeException, [IntToStr(value)]);
    end;
  end;
end;

class function TServiceController.GetDevices: TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices('', skDrive, Result);
end;

class function TServiceController.GetDevices(const machineName: string): TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices(machineName, skDrive, Result);
end;

class function TServiceController.GetServices: TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices('', skWin32, Result);
end;

class function TServiceController.GetServices(
  const machineName: string): TArray<TServiceController>;
begin
  TServiceController.InternalEnumServices(machineName, skWin32, Result);
end;

function TServiceController.GetDisplayName: string;
begin
  Loaded;
  Result := fDisplayName;
end;

function TServiceController.GetMachineName: string;
begin
  Loaded;
  Result := fMachineName;
end;

function TServiceController.GetServiceHandle: THandle;
begin
  Loaded;
  Result := fServiceHandle;
end;

function TServiceController.GetServiceName: string;
begin
  Loaded;
  Result := fServiceName;
end;

function TServiceController.GetServiceType: TServiceTypes;
begin
  Loaded;
  Result := fServiceType;
end;

function TServiceController.GetStartMode: TServiceStartMode;
begin
  Loaded;
  Result := fStartMode;
end;

function TServiceController.GetDescription: string;
begin
  Loaded;
  Result := fDescription;
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  Loaded;
  Result := fStatus;
end;

function TServiceController.GetCanStop: Boolean;
begin
  Loaded;
  Result := fCanStop;
end;

function TServiceController.GetCanPauseAndResume: Boolean;
begin
  Loaded;
  Result := fCanPauseAndResume;
end;

function TServiceController.GetCanShutdown: Boolean;
begin
  Loaded;
  Result := fCanShutdown;
end;

function TServiceController.GetExists: Boolean;
var
  serviceHandle: THandle;
begin
  ManagerHandleNeeded;
  serviceHandle := OpenService(fManagerHandle, PChar(fServiceName), SERVICE_QUERY_STATUS);
  Result := serviceHandle <> fCInvalidServiceHandle;
  if Result then
  begin
    CloseServiceHandle(serviceHandle);
  end;
end;

function TServiceController.GetServicesDependedOn: TArray<TServiceController>;
begin
  Loaded;
  Result := fServicesDependedOn;
end;

function TServiceController.GetDependentServices: TArray<TServiceController>;
begin
  Loaded;
  Result := fDependentServices;
end;

{$ENDREGION}


end.
