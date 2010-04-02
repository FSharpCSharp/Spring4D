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

{TODO -oPaul -cGeneral : Add TProcess class}
{TODO -oPaul -cGeneral : Add TServiceController class}

unit Spring.Utils;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  Messages,
  SysUtils,
  Controls,
  Dialogs,
  DB,
  Registry,
  ShlObj,
  ShellAPI,
  IOUtils,
  ActiveX,
  ComObj,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Win32API,
  Spring.Utils.IO,
  Spring.Utils.Network;

type
  TDriveType = Spring.Utils.IO.TDriveType;
  TDriveInfo = Spring.Utils.IO.TDriveInfo;
  TFileVersionInfo = Spring.Utils.IO.TFileVersionInfo;

  {$WARNINGS OFF}
    TNetwork = Spring.Utils.Network.TNetwork;
  {$WARNINGS ON}


  {$REGION 'TOperatingSystem'}

  TOSPlatformType = (
    ptUnknown,
    ptWin3x,
    ptWin9x,
    ptWinNT
  );

  TOSVersionType = (
    vtUnknown,
    vtWin95,            // DEPRECATED
    vtWin98,            // DEPRECATED
    vtWinME,            // DEPRECATED
    vtWinNT351,         // DEPRECATED
    vtWinNT4,           // DEPRECATED
    vtWinServer2000,
    vtWinXP,
    vtWinServer2003,
    vtWinVista,
    vtWinServer2008,
    vtWin7
  );

  TOSProductType = (
    ptInvalid,
    ptWorkstation,
    ptServer,
    ptDomainController
  );

  TOSSuiteType = (
    etUnknown,
    etWorkStation,
    etServer,
    etAdvancedServer,
    etPersonal,
    etProfessional,
    etDatacenterServer,
    etEnterprise,
    etWebEdition
  );

  /// <summary>
  /// Represents information about the operating system.
  /// </summary>
  TOperatingSystem = class sealed
  strict private
    fPlatformType: TOSPlatformType;
    fProductType: TOSProductType;
    fServicePack: string;
    fVersion: TVersion;
    fVersionType: TOSVersionType;
    function GetIsWin3x: Boolean;
    function GetIsWin9x: Boolean;
    function GetIsWinNT: Boolean;
    function GetVersionString: string;
  private
    function GetOSVersionType(platformType: TOSPlatformType; productType: TOSProductType;
      majorVersion, minorVersion: Integer): TOSVersionType;
  public
    constructor Create;
    function ToString: string; override;
    property IsWin3x: Boolean read GetIsWin3x;
    property IsWin9x: Boolean read GetIsWin9x;
    property IsWinNT: Boolean read GetIsWinNT;
    property PlatformType: TOSPlatformType read fPlatformType;
    property ProductType: TOSProductType read fProductType;
    property ServicePack: string read fServicePack;
    property Version: TVersion read fVersion;
    property VersionString: string read GetVersionString;
    property VersionType: TOSVersionType read fVersionType;
  end;

  {$ENDREGION}


  {$REGION 'Special Folder Enumeration'}

  /// <summary>
  /// Special Folder Enumeration
  /// </summary>
  TSpecialFolder = (
    sfDesktop,                // <desktop>
    sfInternet,               // Internet Explorer (icon on desktop)
    sfPrograms,               // Start Menu\Programs
    sfControls,               // My Computer\Control Panel
    sfPrinters,               // My Computer\Printers
    sfPersonal,               // My Documents
    sfFavorites,              // <user name>\Favorites
    sfStartup,                // Start Menu\Programs\Startup
    sfRecent,                 // <user name>\Recent
    sfSendTo,                 // <user name>\SendTo
    sfBitBucket,              // <desktop>\Recycle Bin
    sfStartMenu,              // <user name>\Start Menu
    { For Windows >= XP }
    sfMyDocuments,            // logical "My Documents" desktop icon
    sfMyMusic,                // "My Music" folder
    { For Windows >= XP }
    sfMyVideo,                // "My Videos" folder
    sfDesktopDirectory,       // <user name>\Desktop
    sfDrives,                 // My Computer
    sfNetwork,                // Network Neighborhood (My Network Places)
    sfNethood,                // <user name>\nethood
    sfFonts,                  // windows\fonts
    sfTemplates,              // <user name>\Templates
    sfCommonStartMenu,        // All Users\Start Menu
    sfCommonPrograms,         // All Users\Start Menu\Programs
    sfCommonStartup,          // All Users\Startup
    sfCommonDesktopDirectory, // All Users\Desktop
    sfAppData,                // <user name>\Application Data
    sfPrinthood,              // <user name>\PrintHood
    sfLocalAppData,           // <user name>\Local Settings\Applicaiton Data (non roaming)
    sfALTStartup,             // non localized startup
    sfCommonALTStartup,       // non localized common startup
    sfCommonFavorites,        // All Users\Favorites
    sfInternetCache,          // <user name>\Local Settings\Temporary Internet Files
    sfCookies,                // <user name>\Cookies
    sfHistory,                // <user name>\Local Settings\History
    sfCommonAppData,          // All Users\Application Data
    sfWindows,                // GetWindowsDirectory()
    sfSystem,                 // GetSystemDirectory()
    sfProgramFiles,           // C:\Program Files
    sfMyPictures,             // C:\Program Files\My Pictures
    sfProfile,                // USERPROFILE
    sfSystemX86,              // x86 system directory on RISC
    sfProgramFilesX86,        // x86 C:\Program Files on RISC
    sfProgramFilesCommon,     // C:\Program Files\Common
    sfProgramFilesCommonX86,  // x86 Program Files\Common on RISC
    sfCommonTemplates,        // All Users\Templates
    sfCommonDocuments,        // All Users\Documents
    sfCommonAdminTools,       // All Users\Start Menu\Programs\Administrative Tools
    sfAdminTools,             // <user name>\Start Menu\Programs\Administrative Tools
    sfConnections,            // Network and Dial-up Connections
    { For Windows >= XP }
    sfCommonMusic,            // All Users\My Music
    { For Windows >= XP }
    sfCommonPictures,         // All Users\My Pictures
    { For Windows >= XP }
    sfCommonVideo,            // All Users\My Video
    sfResources,              // Resource Direcotry
    sfResourcesLocalized,     // Localized Resource Direcotry
    sfCommonOEMLinks,         // Links to All Users OEM specific apps
    { For Windows >= XP }
    sfCDBurnArea,             // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    sfComputersNearMe         // Computers Near Me (computered from Workgroup membership)
  );

  {$ENDREGION}


  {$REGION 'TEnvironment'}

  /// <summary>
  /// Specifies the location where an environment variable is stored or
  /// retrieved in a set or get operation.
  /// </summary>
  TEnvironmentVariableTarget = (
    /// <summary>
    /// The environment variable is stored or retrieved from the environment
    /// block associated with the current process.
    /// </summary>
    evtProcess,
    /// <summary>
    /// The environment variable is stored or retrieved from the
    /// HKEY_CURRENT_USER\Environment key in the Windows operating system registry.
    /// </summary>
    evtUser,
    /// <summary>
    /// The environment variable is stored or retrieved from the
    /// HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment key
    /// in the Windows operating system registry.
    /// </summary>
    evtMachine
  );

  /// <summary>
  /// Identifies the processor and bits-per-word of the platform targeted by an executable.
  /// </summary>
  TProcessorArchitecture = (
    paUnknown,    // Unknown processor
    paX86,        // Intel x86 and compatible microprocessors.
    paIA64,       // 64-bit Intel and compatible microprocessors.
    paAmd64       // 64-bit AMD microprocessors.
  );

  /// <summary>
  /// Provides information about, and means to manipulate, the current environment.
  /// </summary>
  TEnvironment = record
  private
    class var
      fOperatingSystem: TOperatingSystem;
      fApplicationPath: string;
      fApplicationVersionInfo: TFileVersionInfo;
      fApplicationVersion: TVersion;
      fApplicationVersionString: string;
      class constructor Create;
      {$HINTS OFF}
      class destructor Destroy;
      {$HINTS ON}
  private
    class function GetCurrentDirectory: string; static;
    class function GetMachineName: string; static;
    class function GetIsAdmin: Boolean; static;
    class function GetUserDomainName: string; static;
    class function GetUserName: string; static;
    class function GetTickCount: Cardinal; static;
    class function GetNewLine: string; static;
    class function GetUserInteractive: Boolean; static;
    class function GetCommandLine: string; static;
    class function GetSystemDirectory: string; static;
    class function GetProcessorCount: Integer; static;
    class function GetProcessorArchitecture: TProcessorArchitecture; static;
    class function GetRegisteredOrganization: string; static;
    class function GetRegisteredOwner: string; static;
    class procedure SetCurrentDirectory(const value: string); static;
  private
    class procedure OpenEnvironmentVariableKey(registry: TRegistry;
      target: TEnvironmentVariableTarget; keyAccess: Cardinal); static;
    class function GetCurrentVersionKey: string; static;
    class procedure GetProcessEnvironmentVariables(list: TStrings); static;
  public
    class function  GetCommandLineArgs: TStringDynArray; overload; static;
    class procedure GetCommandLineArgs(list: TStrings); overload; static;
    class function  GetLogicalDrives: TStringDynArray; overload; static;
    class procedure GetLogicalDrives(list: TStrings); overload; static;
    class function  GetFolderPath(const folder: TSpecialFolder): string; static;
    class function  GetEnvironmentVariable(const variable: string): string; overload; static;
    class function  GetEnvironmentVariable(const variable: string; target: TEnvironmentVariableTarget): string; overload; static;
    class procedure GetEnvironmentVariables(list: TStrings); overload; static;
    class procedure GetEnvironmentVariables(list: TStrings; target: TEnvironmentVariableTarget); overload; static;
    class procedure SetEnvironmentVariable(const variable, value: string); overload; static;
    class procedure SetEnvironmentVariable(const variable, value: string; target: TEnvironmentVariableTarget); overload; static;
    class function ExpandEnvironmentVariables(const variable: string): string; static;
    class property ApplicationPath: string read fApplicationPath;
    class property ApplicationVersion: TVersion read fApplicationVersion;
    class property ApplicationVersionInfo: TFileVersionInfo read fApplicationVersionInfo;
    class property ApplicationVersionString: string read fApplicationVersionString;
    class property CommandLine: string read GetCommandLine;
    class property CurrentDirectory: string read GetCurrentDirectory write SetCurrentDirectory;
    class property IsAdmin: Boolean read GetIsAdmin; { experimental }
    class property MachineName: string read GetMachineName;
    class property NewLine: string read GetNewLine;
    class property OperatingSystem: TOperatingSystem read fOperatingSystem;
    class property ProcessorCount: Integer read GetProcessorCount;
    class property ProcessorArchitecture: TProcessorArchitecture read GetProcessorArchitecture;
    class property RegisteredOrganization: string read GetRegisteredOrganization;
    class property RegisteredOwner: string read GetRegisteredOwner;
    class property SystemDirectory: string read GetSystemDirectory;
    class property TickCount: Cardinal read GetTickCount;
    class property UserDomainName: string read GetUserDomainName;
    class property UserName: string read GetUserName;
    class property UserInteractive: Boolean read GetUserInteractive;
  end;

  /// <summary>
  /// Represents a type alias of TEnvironment class.
  /// </summary>
  Environment = TEnvironment;

  {$ENDREGION}


  {$REGION 'TMessageBox'}

  TMessageDialogButton  = Dialogs.TMsgDlgBtn;
  TMessageDialogButtons = Dialogs.TMsgDlgButtons;

  TMessageDialogShowProc = reference to function(const text, caption: string;
    dialogType: TMsgDlgType; buttons: TMessageDialogButtons;
    defaultButton: TMessageDialogButton): TModalResult;

  /// <summary>
  /// Provides static methods to show common message dialogs.
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

  TMsgBox = TMessageBox;

  TDialog = TMessageBox deprecated 'Use TMessageBox/TMsgBox instead.';

  {$ENDREGION}


  {$REGION 'TBaseNCalculator (Experimental)'}

  /// <summary>
  /// Represents a Base-N Calculator.
  /// </summary>
  TBaseNCalculator = record
  private
    fElements: string;
    function IndexOf(const digit: Char): Integer;
  public
    constructor Create(const elements: string); overload;
    constructor Create(const elements: array of Char); overload;
    function Add(const left, right: string): string;
    function Subtract(const left, right: string): string;
    function GetNextValue(const value: string): string;
    function GetQuantity(const left, right: string): Int64;
    function GetEndNumber(const startNumber: string; quantity: Int64): string;
    function Compare(const left, right: string): Integer;
    function ConvertFromDecimal(const value: Int64): string;
    function ConvertToDecimal(const s: string): Int64;
    function IsValid(const s: string): Boolean;
    function FormatNumber(const s: string; len: Integer): string;
    property Elements: string read fElements;
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

  /// <summary>
  /// Returns the path of the application.
  /// </summary>
  function ApplicationPath: string;

  /// <summary>
  /// Returns the version number of the application.
  /// </summary>
  function ApplicationVersion: TVersion;

  /// <summary>
  /// Returns the version information of the application.
  /// </summary>
  function ApplicationVersionString: string;

  /// <summary>
  /// Returns the last system error message.
  /// </summary>
  function GetLastErrorMessage: string;

  /// <summary>
  /// Try setting focus to a control.
  /// </summary>
  /// <remarks>
  /// </remarks>
  function TrySetFocus(control: TWinControl): Boolean;

  function TryFocusControl(control: TWinControl): Boolean;
    deprecated 'Use TrySetFocus instead.';

  /// <summary>
  /// Enumerates all child components, recursively.
  /// </summary>
  /// <param name="callback">Returning false will stop the enumeration.</param>
  procedure EnumerateComponents(owner: TComponent; callback: TFunc<TComponent, Boolean>);

  /// <summary>
  /// Walkthrough all child controls in tab-order, recursively.
  /// </summary>
  /// <param name="callback">Returning false will stop the enumeration.</param>
  procedure EnumerateControls(parentControl: TWinControl; callback: TFunc<TWinControl, Boolean>);

  /// <summary>
  /// Walkthrough all dataset records from the first one.
  /// </summary>
  /// <param name="callback">Returning false will stop the enumeration.</param>
  procedure EnumerateDataSet(dataSet: TDataSet; callback: TFunc<Boolean>);

  /// <summary>
  /// GetDroppedFiles
  /// </summary>
  procedure GetDroppedFiles(const dataObject: IDataObject; list: TStrings); overload;

  /// <summary>
  /// GetDroppedFiles
  /// </summary>
  procedure GetDroppedFiles(dropHandle: THandle; list: TStrings); overload;

  /// <summary>
  /// Converts a windows TFiletime value to a delphi TDatetime value.
  /// </summary>
  function ConvertFileTimeToDateTime(const fileTime: TFileTime; useLocalTimeZone: Boolean): TDateTime; overload;

  function ConvertDateTimeToFileTime(const datetime: TDateTime; useLocalTimeZone: Boolean): TFileTime; overload;

  {$ENDREGION}


  {$REGION 'Constants'}

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

  const
    SpecialFolderCSIDLs: array[TSpecialFolder] of Integer = (
      CSIDL_DESKTOP,                  // <desktop>
      CSIDL_INTERNET,                 // Internet Explorer (icon on desktop)
      CSIDL_PROGRAMS,                 // Start Menu\Programs
      CSIDL_CONTROLS,                 // My Computer\Control Panel
      CSIDL_PRINTERS,                 // My Computer\Printers
      CSIDL_PERSONAL,                 // My Documents.  This is equivalent to CSIDL_MYDOCUMENTS in XP and above
      CSIDL_FAVORITES,                // <user name>\Favorites
      CSIDL_STARTUP,                  // Start Menu\Programs\Startup
      CSIDL_RECENT,                   // <user name>\Recent
      CSIDL_SENDTO,                   // <user name>\SendTo
      CSIDL_BITBUCKET,                // <desktop>\Recycle Bin
      CSIDL_STARTMENU,                // <user name>\Start Menu
      CSIDL_MYDOCUMENTS,              // logical "My Documents" desktop icon
      CSIDL_MYMUSIC,                  // "My Music" folder
      CSIDL_MYVIDEO,                  // "My Video" folder
      CSIDL_DESKTOPDIRECTORY,         // <user name>\Desktop
      CSIDL_DRIVES,                   // My Computer
      CSIDL_NETWORK,                  // Network Neighborhood (My Network Places)
      CSIDL_NETHOOD,                  // <user name>\nethood
      CSIDL_FONTS,                    // windows\fonts
      CSIDL_TEMPLATES,
      CSIDL_COMMON_STARTMENU,         // All Users\Start Menu
      CSIDL_COMMON_PROGRAMS,          // All Users\Start Menu\Programs
      CSIDL_COMMON_STARTUP,           // All Users\Startup
      CSIDL_COMMON_DESKTOPDIRECTORY,  // All Users\Desktop
      CSIDL_APPDATA,                  // <user name>\Application Data
      CSIDL_PRINTHOOD,                // <user name>\PrintHood
      CSIDL_LOCAL_APPDATA,            // <user name>\Local Settings\Application Data (non roaming)
      CSIDL_ALTSTARTUP,               // non localized startup
      CSIDL_COMMON_ALTSTARTUP,        // non localized common startup
      CSIDL_COMMON_FAVORITES,
      CSIDL_INTERNET_CACHE,
      CSIDL_COOKIES,
      CSIDL_HISTORY,
      CSIDL_COMMON_APPDATA,           // All Users\Application Data
      CSIDL_WINDOWS,                  // GetWindowsDirectory()
      CSIDL_SYSTEM,                   // GetSystemDirectory()
      CSIDL_PROGRAM_FILES,            // C:\Program Files
      CSIDL_MYPICTURES,               // C:\Program Files\My Pictures
      CSIDL_PROFILE,                  // USERPROFILE
      CSIDL_SYSTEMX86,                // x86 system directory on RISC
      CSIDL_PROGRAM_FILESX86,         // x86 C:\Program Files on RISC
      CSIDL_PROGRAM_FILES_COMMON,     // C:\Program Files\Common
      CSIDL_PROGRAM_FILES_COMMONX86,  // x86 C:\Program Files\Common on RISC
      CSIDL_COMMON_TEMPLATES,         // All Users\Templates
      CSIDL_COMMON_DOCUMENTS,         // All Users\Documents
      CSIDL_COMMON_ADMINTOOLS,        // All Users\Start Menu\Programs\Administrative Tools
      CSIDL_ADMINTOOLS,               // <user name>\Start Menu\Programs\Administrative Tools
      CSIDL_CONNECTIONS,              // Network and Dial-up Connections
      CSIDL_COMMON_MUSIC,             // All Users\My Music
      CSIDL_COMMON_PICTURES,          // All Users\My Pictures
      CSIDL_COMMON_VIDEO,             // All Users\My Video
      CSIDL_RESOURCES,                // Resource Directory
      CSIDL_RESOURCES_LOCALIZED,      // Localized Resource Directory
      CSIDL_COMMON_OEM_LINKS,         // Links to All Users OEM specific apps
      CSIDL_CDBURN_AREA,              // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
      CSIDL_COMPUTERSNEARME           // Computers Near Me (computered from Workgroup membership)
    );

  {$ENDREGION}

implementation

uses
  Math,
  Spring.ResourceStrings;

const
  OSVersionTypeStrings: array[TOSVersionType] of string = (
    SUnknownOSDescription,
    SWin95Description,
    SWin98Description,
    SWinMEDescription,
    SWinNT351Description,
    SWinNT40Description,
    SWinServer2000Description,
    SWinXPDescription,
    SWinServer2003Description,
    SWinVistaDescription,
    SWinServer2008Description,
    SWin7Description
  );


{$REGION 'Routines'}

function ApplicationPath: string;
begin
  Result := TEnvironment.ApplicationPath;
end;

function ApplicationVersion: TVersion;
begin
  Result := TEnvironment.ApplicationVersion;
end;

function ApplicationVersionString: string;
begin
  Result := TEnvironment.ApplicationVersionString;
end;

function GetLastErrorMessage: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

function TrySetFocus(control: TWinControl): Boolean;
begin
  TArgument.CheckNotNull(control, 'control');

  Result := control.Showing and control.CanFocus;
  if Result then
  begin
    control.SetFocus;
  end;
end;

function TryFocusControl(control: TWinControl): Boolean;
begin
  Result := TrySetFocus(control);
end;

procedure EnumerateComponents(owner: TComponent; callback: TFunc<TComponent, Boolean>);
var
  component: TComponent;
begin
  TArgument.CheckNotNull(owner, 'owner');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  for component in owner do
  begin
    if not callback(component) then
    begin
      Exit;
    end;
    if component.ComponentCount > 0 then
    begin
      EnumerateComponents(component, callback);
    end;
  end;
end;

procedure EnumerateControls(parentControl: TWinControl; callback: TFunc<TWinControl, Boolean>);
var
  list: TList;
  i: Integer;
begin
  TArgument.CheckNotNull(parentControl, 'parentControl');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  list := TList.Create;
  try
    parentControl.GetTabOrderList(list);
    for i := 0 to list.Count - 1 do
    begin
      if not callback(TWinControl(list[i])) then
      begin
        Exit;
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure EnumerateDataSet(dataSet: TDataSet; callback: TFunc<Boolean>);
begin
  TArgument.CheckNotNull(dataSet, 'dataSet');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  dataSet.DisableControls;
  try
    dataSet.First;
    while not dataSet.Eof and callback do
    begin
      dataSet.Next;
    end;
  finally
    dataSet.EnableControls;
  end;
end;

procedure GetDroppedFiles(const dataObject: IDataObject; list: TStrings); overload;
var
  handle: THandle;
  medium: TStgMedium;
const
  f: tagFORMATETC = (
    cfFormat: CF_HDROP;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: LongInt($FFFFFFFF)
  );
begin
  TArgument.CheckNotNull(dataObject, 'dataObject');
  OleCheck(dataObject.GetData(f, medium));
  handle := medium.hGlobal;
  GetDroppedFiles(handle, list);
end;

procedure GetDroppedFiles(dropHandle: THandle; list: TStrings);
var
  count, size, i: Integer;
  fileName: array[0..MAX_PATH] of Char;
const
  f: tagFORMATETC = (
    cfFormat: CF_HDROP;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: LongInt($FFFFFFFF)
  );
begin
  TArgument.CheckNotNull(list, 'list');
  count := DragQueryFile(dropHandle, $FFFFFFFF, nil, 0);
  try
    for i := 0 to count - 1 do
    begin
      size := DragQueryFile(dropHandle, i, nil, 0) + 1;
      DragQueryFile(dropHandle, i, fileName, size);
      list.Add(fileName);
    end;
  finally
    DragFinish(dropHandle);
  end;
end;

function ConvertFileTimeToDateTime(const fileTime: TFileTime; useLocalTimeZone: Boolean): TDateTime;
var
  localFileTime: TFileTime;
  systemTime: TSystemTime;
begin
  if useLocalTimeZone then
  begin
    FileTimeToLocalFileTime(fileTime, localFileTime);
  end
  else
  begin
    localFileTime := fileTime;
  end;
  if FileTimeToSystemTime(localFileTime, systemTime) then
  begin
    Result := SystemTimeToDateTime(systemTime);
  end
  else
  begin
    Result := 0;
  end;
end;

function ConvertDateTimeToFileTime(const datetime: TDateTime;
  useLocalTimeZone: Boolean): TFileTime;
var
  systemTime: TSystemTime;
  fileTime: TFileTime;
begin
  Result.dwLowDateTime := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(datetime, systemTime);
  if SystemTimeToFileTime(systemTime, fileTime) then
  begin
    if useLocalTimeZone then
    begin
      LocalFileTimeToFileTime(fileTime, Result);
    end
    else
    begin
      Result := fileTime;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TOperatingSystem'}

constructor TOperatingSystem.Create;
var
  versionInfo: TOSVersionInfoEx;
begin
  inherited Create;
  ZeroMemory(@versionInfo, SizeOf(versionInfo));
  versionInfo.dwOSVersionInfoSize := SizeOf(versionInfo);
  Win32Check(Windows.GetVersionEx(versionInfo));
  case versionInfo.dwPlatformId of
    VER_PLATFORM_WIN32s:        fPlatformType := ptWin3x;
    VER_PLATFORM_WIN32_WINDOWS: fPlatformType := ptWin9x;
    VER_PLATFORM_WIN32_NT:      fPlatformType := ptWinNT;
    else fPlatformType := ptUnknown;
  end;
  fProductType := ptInvalid;
  case versionInfo.wProductType of
    VER_NT_WORKSTATION:       fProductType := ptWorkstation;
    VER_NT_DOMAIN_CONTROLLER: fProductType := ptDomainController;
    VER_NT_SERVER:            fProductType := ptServer;
  end;
  fVersion := TVersion.Create(
    versionInfo.dwMajorVersion,
    versionInfo.dwMinorVersion,
    versionInfo.dwBuildNumber
  );
  fVersionType := GetOSVersionType(
    fPlatformType,
    fProductType,
    versionInfo.dwMajorVersion,
    versionInfo.dwMinorVersion
  );
  fServicePack := versionInfo.szCSDVersion;
end;

function TOperatingSystem.GetOSVersionType(platformType: TOSPlatformType;
  productType: TOSProductType; majorVersion, minorVersion: Integer): TOSVersionType;
begin
  Result := vtUnknown;
  case platformType of
    ptWin9x:
    begin
      if majorVersion = 4 then
      case minorVersion of
        0:  Result := vtWin95;
        10: Result := vtWin98;
        90: Result := vtWinMe;
      end;
    end;
    ptWinNT:
    begin
      if (majorVersion = 3) and (minorVersion = 51) then
      begin
        Result := vtWinNT351;
      end
      else if (majorVersion = 4) and (minorVersion = 0) then
      begin
        Result := vtWinNT4;
      end
      else if majorVersion = 5 then
      case minorVersion of
        0: Result := vtWinServer2000;
        1: Result := vtWinXP;
        2: Result := vtWinServer2003;
      end
      else if majorVersion = 6 then
      case minorVersion of
        0:
        begin
          if productType = ptWorkstation then
            Result := vtWinVista
          else
            Result := vtWinServer2008;
        end;
        1:
        begin
          if productType = ptWorkstation then
            Result := vtWin7
          else
            Result := vtWinServer2008;   { TODO: WinServer2008 R2 }
        end;
      end;
    end;
  end;
end;

function TOperatingSystem.ToString: string;
begin
  Result := OSVersionTypeStrings[fVersionType];
  if fVersionType <> vtUnknown then
  begin
    Result := Result + ' Version ' + fVersion.ToString;
    if ServicePack <> '' then
      Result := Result + ' ' + ServicePack;
  end;
end;

function TOperatingSystem.GetIsWin3x: Boolean;
begin
  Result := Self.PlatformType = ptWin3x;
end;

function TOperatingSystem.GetIsWin9x: Boolean;
begin
  Result := Self.PlatformType = ptWin9x;
end;

function TOperatingSystem.GetIsWinNT: Boolean;
begin
  Result := Self.PlatformType = ptWinNT;
end;

function TOperatingSystem.GetVersionString: string;
begin
  Result := ToString;
end;

{$ENDREGION}


{$REGION 'TEnvironment'}

class constructor TEnvironment.Create;
begin
  fApplicationPath := ExtractFilePath(ParamStr(0));
  fApplicationVersionInfo := TFileVersionInfo.GetVersionInfo(ParamStr(0));
  fApplicationVersion := fApplicationVersionInfo.FileVersionNumber;
  fApplicationVersionString := fApplicationVersionInfo.FileVersion;
  fOperatingSystem := TOperatingSystem.Create;
end;

class destructor TEnvironment.Destroy;
begin
  fOperatingSystem.Free;
end;

class function TEnvironment.GetCommandLineArgs: TStringDynArray;
var
  pArgs: PPWideChar;
  count: Integer;
  i: Integer;
begin
  pArgs := ShellAPI.CommandLineToArgvW(PWideChar(Windows.GetCommandLineW), count);
  if pArgs <> nil then
  try
    SetLength(Result, count);
    for i := 0 to count - 1 do
    begin
      Result[i] := string(pArgs^);
      Inc(pArgs);
    end;
  finally
    Windows.LocalFree(HLocal(pArgs));
  end;
end;

class procedure TEnvironment.GetCommandLineArgs(list: TStrings);
var
  args: TStringDynArray;
begin
  args := GetCommandLineArgs;
  UpdateStrings(list,
    procedure
    var
      i: Integer;
    begin
      for i := 0 to High(args) do
      begin
        list.Add(args[i]);
      end;
    end
  );
end;

class function TEnvironment.GetLogicalDrives: TStringDynArray;
var
  len: Cardinal;
  buffer: string;
begin
  len := Windows.GetLogicalDriveStrings(0, nil);
  SetLength(buffer, len);
  Windows.GetLogicalDriveStrings(len * SizeOf(Char), PChar(buffer));
  Result := SplitString(PChar(buffer));
end;

class procedure TEnvironment.GetLogicalDrives(list: TStrings);
var
  drives: TStringDynArray;
begin
  drives := TEnvironment.GetLogicalDrives;
  UpdateStrings(list,
    procedure
    var
      drive: string;
    begin
      for drive in drives do
      begin
        list.Add(drive);
      end;
    end
  );
end;

function TryGetAccessToken(out hToken: THandle): Boolean;
begin
  Result := Windows.OpenThreadToken(GetCurrentThread, TOKEN_QUERY, TRUE, hToken);
  if not Result and (Windows.GetLastError = ERROR_NO_TOKEN) then
  begin
    Result := Windows.OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);
  end;
end;

class function TEnvironment.GetFolderPath(const folder: TSpecialFolder): string;
var
  pidl : PItemIDList;
  buffer: array[0..MAX_PATH-1] of Char;
//  returnCode: HRESULT;
  hToken : THandle;
begin
  if TryGetAccessToken(hToken) then
  try
    ShlObj.SHGetFolderLocation(INVALID_HANDLE_VALUE,
      SpecialFolderCSIDLs[folder], hToken, 0, pidl);
    ShlObj.SHGetPathFromIDList(pidl, @buffer[0]);
    Result := buffer;
  finally
    CloseHandle(hToken);
  end;
end;

class procedure TEnvironment.OpenEnvironmentVariableKey(registry: TRegistry;
  target: TEnvironmentVariableTarget; keyAccess: Cardinal);
var
  key: string;
begin
  Assert(registry <> nil, 'registry should not be nil.');
  Assert(target in [evtUser, evtMachine], Format('Illegal target: %d.', [Integer(target)]));
  if target = evtUser then
  begin
    registry.RootKey := HKEY_CURRENT_USER;
    key := 'Environment';
  end
  else
  begin
    registry.RootKey := HKEY_LOCAL_MACHINE;
    key := 'System\CurrentControlSet\Control\Session Manager\Environment';
  end;
  registry.Access := keyAccess;
  if not registry.OpenKey(key, False) then
  begin
    raise EOSError.CreateResFmt(@SCannotAccessRegistryKey, [key]);
  end;
end;

class function TEnvironment.GetEnvironmentVariable(
  const variable: string): string;
begin
  Result := TEnvironment.GetEnvironmentVariable(variable, evtProcess);
end;

class function TEnvironment.GetEnvironmentVariable(const variable: string;
  target: TEnvironmentVariableTarget): string;
var
  registry: TRegistry;

  function GetProcessEnvironmentVariable: string;
  var
    len: DWORD;
  begin
    len := Windows.GetEnvironmentVariable(PChar(variable), nil, 0);
    if len > 0 then
    begin
      SetLength(Result, len - 1);
      Windows.GetEnvironmentVariable(PChar(variable), PChar(Result), len);
    end
    else
    begin
      Result := '';
    end;
  end;
begin
  TArgument.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    Result := GetProcessEnvironmentVariable;
    Exit;
  end;
  registry := TRegistry.Create;
  try
    OpenEnvironmentVariableKey(registry, target, KEY_READ);
    if registry.ValueExists(variable) then
    begin
      Result := registry.GetDataAsString(variable);
    end
    else
    begin
      Result := '';
    end;
  finally
    registry.Free;
  end;
end;

class procedure TEnvironment.GetProcessEnvironmentVariables(list: TStrings);
var
  p: PChar;
  strings: TStringDynArray;
begin
  Assert(list <> nil, 'list should not be nil.');
  p := Windows.GetEnvironmentStrings;
  try
    strings := SplitString(p);
    UpdateStrings(list,
      procedure
      var
        s: string;
      begin
        for s in strings do
        begin
          if (Length(s) > 0) and (s[1] <> '=') then // Skip entries start with '='
          begin
            list.Add(s);
          end;
        end;
      end
    );
  finally
    Win32Check(Windows.FreeEnvironmentStrings(p));
  end;
end;

class procedure TEnvironment.GetEnvironmentVariables(list: TStrings);
begin
  TEnvironment.GetEnvironmentVariables(list, evtProcess);
end;

class procedure TEnvironment.GetEnvironmentVariables(list: TStrings;
  target: TEnvironmentVariableTarget);
var
  registry: TRegistry;
  value: string;
  i: Integer;
begin
  TArgument.CheckNotNull(list, 'list');
  TArgument.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    GetProcessEnvironmentVariables(list);
    Exit;
  end;
  registry := TRegistry.Create;
  try
    OpenEnvironmentVariableKey(registry, target, KEY_READ);
    registry.GetValueNames(list);
    for i := 0 to list.Count - 1 do
    begin
      value := registry.GetDataAsString(list[i]);
      list[i] := list[i] + list.NameValueSeparator + value;
    end;
  finally
    registry.Free;
  end;
end;

class procedure TEnvironment.SetEnvironmentVariable(const variable, value: string);
begin
  TEnvironment.SetEnvironmentVariable(variable, value, evtProcess);
end;

class procedure TEnvironment.SetEnvironmentVariable(const variable,
  value: string; target: TEnvironmentVariableTarget);
var
  registry: TRegistry;
begin
  TArgument.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    Win32Check(Windows.SetEnvironmentVariable(PChar(variable), PChar(value)));
    Exit;
  end;
  registry := TRegistry.Create;
  try
    OpenEnvironmentVariableKey(registry, target, KEY_WRITE);
    if Pos('%', value) > 0 then
    begin
      registry.WriteExpandString(variable, value);
    end
    else
    begin
      registry.WriteString(variable, value);
    end;
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
  finally
    registry.Free;
  end;
end;

class function TEnvironment.ExpandEnvironmentVariables(
  const variable: string): string;
var
  len: Cardinal;
begin
  len := MAX_PATH;
  SetLength(Result, len);
  len := Windows.ExpandEnvironmentStrings(PChar(variable), PChar(Result), len);
  Win32Check(len > 0);
  SetLength(Result, len - 1);
end;

class function TEnvironment.GetCommandLine: string;
begin
  Result := Windows.GetCommandLine;
end;

class function TEnvironment.GetCurrentDirectory: string;
var
  size: DWORD;
begin
  size := Windows.GetCurrentDirectory(0, nil);
  SetLength(Result, size - 1);
  Windows.GetCurrentDirectory(size, PChar(Result));
end;

class function TEnvironment.GetCurrentVersionKey: string;
const
  HKLM_CURRENT_VERSION_NT      = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  HKLM_CURRENT_VERSION_WINDOWS = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
begin
  if OperatingSystem.IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

class function TEnvironment.GetMachineName: string;
var
  size: Cardinal;
begin
  size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, size);
  if GetComputerName(PChar(Result), size) then
  begin
    SetLength(Result, size);
  end;
end;

class function TEnvironment.GetNewLine: string;
begin
  Result := System.sLineBreak;
end;

class function TEnvironment.GetProcessorArchitecture: TProcessorArchitecture;
var
  systemInfo: TSystemInfo;
const
  PROCESSOR_ARCHITECTURE_INTEL          = 0;
  PROCESSOR_ARCHITECTURE_AMD64          = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64  = 10;
  PROCESSOR_ARCHITECTURE_IA64           = 6;
begin
  ZeroMemory(@systemInfo, Sizeof(systemInfo));
  Windows.GetSystemInfo(systemInfo);
  case systemInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL:
      Result := paX86;
    PROCESSOR_ARCHITECTURE_IA64:
      Result := paIA64;
    PROCESSOR_ARCHITECTURE_AMD64:
      Result := paAmd64;
    else
      Result := paUnknown;
  end;
end;

class function TEnvironment.GetProcessorCount: Integer;
var
  systemInfo: TSystemInfo;
begin
  ZeroMemory(@systemInfo, Sizeof(systemInfo));
  Windows.GetSystemInfo(systemInfo);
  Result := systemInfo.dwNumberOfProcessors;
end;

class function TEnvironment.GetRegisteredOrganization: string;
begin
  Result := ComObj.GetRegStringValue(
    GetCurrentVersionKey,
    'RegisteredOrganization',  // DO NOT LOCALIZE
    HKEY_LOCAL_MACHINE
  );
end;

class function TEnvironment.GetRegisteredOwner: string;
begin
  Result := ComObj.GetRegStringValue(
    GetCurrentVersionKey,
    'RegisteredOwner',  // DO NOT LOCALIZE
    HKEY_LOCAL_MACHINE
  );
end;

class function TEnvironment.GetSystemDirectory: string;
begin
  Result := TEnvironment.GetFolderPath(sfSystem);
end;

class function TEnvironment.GetUserDomainName: string;
var
  hasToken: Boolean;
  hToken: THandle;
  ptiUser: PSIDAndAttributes;
  cbti: DWORD;
  snu: SID_NAME_USE;
  userSize, domainSize: Cardinal;
  userName: string;
begin
  ptiUser := nil;
  userSize := 0;
  domainSize := 0;
  hasToken := Windows.OpenThreadToken(GetCurrentThread, TOKEN_QUERY, TRUE, hToken);
  if not hasToken and (Windows.GetLastError = ERROR_NO_TOKEN) then
  begin
    hasToken := Windows.OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);
  end;
  if hasToken then
  try
    Windows.GetTokenInformation(hToken, TokenUser, nil, 0, cbti);
    ptiUser := AllocMem(cbti);
    if Windows.GetTokenInformation(hToken, TokenUser, ptiUser, cbti, cbti) then
    begin
      if not Windows.LookupAccountSid(nil, ptiUser.Sid, nil, userSize, nil, domainSize, snu) and
        (Windows.GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        SetLength(userName, userSize - 1);
        SetLength(Result, domainSize - 1);
        Win32Check(Windows.LookupAccountSid(nil, ptiUser.Sid, PChar(userName), userSize,
          PChar(Result), domainSize, snu));
      end;
    end;
  finally
    Windows.CloseHandle(hToken);
    FreeMem(ptiUser);
  end;
end;

class function TEnvironment.GetUserInteractive: Boolean;
begin
  { TODO: UserInteractive }
  Result := True;
end;

class function TEnvironment.GetUserName: string;
var
  size: Cardinal;
begin
  size := 255;
  SetLength(Result, size);
  Win32Check(Windows.GetUserName(PChar(Result), size));
  SetLength(Result, size - 1);
end;

/// http://www.gumpi.com/Blog/2007/10/02/EKON11PromisedEntry3.aspx
/// <author>Daniel Wischnewski</author>
class function TEnvironment.GetIsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_ENABLED = $00000004;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result   := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  if bSuccess then
  begin
    GetTokenInformation(hAccessToken, TokenGroups, nil, 0, dwInfoBufferSize);
    ptgGroups := GetMemory(dwInfoBufferSize);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, dwInfoBufferSize, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
      for x := 0 to ptgGroups.GroupCount - 1 do
      begin
        if (SE_GROUP_ENABLED = (ptgGroups.Groups[x].Attributes and SE_GROUP_ENABLED)) and EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          Break;
        end;
      end;
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

class function TEnvironment.GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;

class procedure TEnvironment.SetCurrentDirectory(const value: string);
begin
  Win32Check(Windows.SetCurrentDirectory(PChar(value)));
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


{$REGION 'TBaseNCalculator'}

constructor TBaseNCalculator.Create(const elements: string);
begin
  if Length(elements) < 2 then
  begin
    raise EArgumentException.CreateRes(@SAtLeastTwoElements);
  end;
  fElements := elements;
end;

constructor TBaseNCalculator.Create(const elements: array of Char);
begin
  Create(elements);
end;

function TBaseNCalculator.Add(const left, right: string): string;
var
  n: Integer;
  x: string;
  y: string;
  p1: Integer;
  p2: Integer;
  p: Integer;
  i: Integer;
  carried: Boolean;
begin
  n := Max(Length(left), Length(right));
  x := FormatNumber(left, n);
  y := FormatNumber(right, n);
  carried := False;
  Result := '';
  for i := Length(x) downto 1 do
  begin
    p1 := IndexOf(x[i]);
    p2 := IndexOf(y[i]);
    if carried then
    begin
      Inc(p1);
    end;
    p := (p1 + p2) mod Length(fElements);
    carried := p1 + p2 >= Length(fElements);
    Result := fElements[p + 1] + Result;
  end;
  if carried then
  begin
    Result := fElements[2] + Result;
  end;
end;

function TBaseNCalculator.Subtract(const left, right: string): string;
var
  x: string;
  y: string;
  n: Integer;
  i: Integer;
  p1, p2, p: Integer;
  borrowed: Boolean;
begin
  n := Max(Length(left), Length(right));
  x := FormatNumber(left, n);
  y := FormatNumber(right, n);
  borrowed := False;
  for i := n downto 1 do
  begin
    p1 := IndexOf(x[i]);
    p2 := IndexOf(y[i]);
    if borrowed then
    begin
      Dec(p1);
    end;
    p := p1 - p2;
    borrowed := p < 0;
    if not borrowed then
    begin
      Result := fElements[p + 1] + Result;
    end
    else
    begin
      Result := fElements[Length(fElements) + p + 1] + Result;
    end;
  end;
end;

function TBaseNCalculator.GetQuantity(const left, right: string): Int64;
var
  n: Integer;
  s: string;
begin
  n := Compare(left, right);
  if n = 0 then
  begin
    Result := 1;
  end
  else if n > 0 then
  begin
    s := Subtract(left, right);
    Result := ConvertToDecimal(s) + 1;
  end
  else
  begin
    s := Subtract(right, left);
    Result := ConvertToDecimal(s) + 1;
  end;
end;

function TBaseNCalculator.FormatNumber(const s: string; len: Integer): string;
begin
  if Length(s) < len then
  begin
    Result := StringOfChar(fElements[1], len - Length(s)) + s;
  end
  else
  begin
    Result := s;
  end;
end;

function TBaseNCalculator.GetNextValue(const value: string): string;
begin
  Assert(Length(fElements) >= 2);
  Result := Add(value, fElements[2]);
end;

function TBaseNCalculator.IndexOf(const digit: Char): Integer;
begin
  Result := Pos(digit, fElements);
  Dec(Result);
end;

function TBaseNCalculator.Compare(const left, right: string): Integer;
var
  n: Integer;
  x: string;
  y: string;
  i: Integer;
  p1, p2: Integer;
begin
  n := Max(Length(left), Length(right));
  x := FormatNumber(left, n);
  y := FormatNumber(right, n);
  Result := 0;
  for i := 1 to n do
  begin
    p1 := IndexOf(x[i]);
    p2 := IndexOf(y[i]);
    Result := p1 - p2;
    if Result <> 0 then
    begin
      Break;
    end;
  end;
end;

function TBaseNCalculator.GetEndNumber(const startNumber: string;
  quantity: Int64): string;
var
  s: string;
begin
  if quantity > 1 then
  begin
    s := ConvertFromDecimal(quantity - 1);
    Result := Add(startNumber, s);
  end
  else if quantity = 1 then
  begin
    Result := startNumber;
  end
  else
  begin
    raise EArgumentException.CreateResFmt(@SIllegalArgumentQuantity, [quantity]);
  end;
end;

function TBaseNCalculator.IsValid(const s: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(s) do
  begin
    if IndexOf(s[i]) = -1 then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TBaseNCalculator.ConvertFromDecimal(const value: Int64): string;
var
  m, n: Integer;
begin
  Result := '';
  m := value;
  while m > 0 do
  begin
    n := m mod Length(fElements);
    m := m div Length(fElements);
    Result := fElements[n + 1] + Result;
  end;
end;

function TBaseNCalculator.ConvertToDecimal(const s: string): Int64;
var
  i: Integer;
  p: Integer;
  base: Integer;
begin
  base := Length(fElements);
  Result := 0;
  for i := 1 to Length(s)  do
  begin
    p := IndexOf(s[i]);
    Result := Result + p * Trunc(IntPower(base, Length(s) - i));
  end;
end;

{$ENDREGION}

end.
