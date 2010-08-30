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

{TODO -oOwner -cGeneral : TServiceController}
{TODO -oOwner -cGeneral : TProcess}
{TODO -oOwner -cGeneral : TRecycleBin}
{TODO -oOwner -cGeneral : TClipboardWatcher}
{TODO -oOwner -cGeneral : TDeviceWatcher}

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
  Spring,
  Spring.Collections,
  Spring.Utils.Win32API;

type
  /// <summary>
  /// Drive Type Enumeration
  /// </summary>
  TDriveType = (
    dtUnknown,          // The type of drive is unknown.
    dtNoRootDirectory,  // The drive does not have a root directory.
    dtRemovable,        // The drive is a removable storage device, such as a floppy disk drive or a USB flash drive.
    dtFixed,            // The drive is a fixed disk.
    dtNetwork,          // The drive is a network drive.
    dtCDRom,            // The drive is an optical disc device, such as a CD or DVD-ROM.
    dtRam               // The drive is a RAM disk.
  );

  {TODO -oPaul -cGeneral : Add the Refresh method}

  {$REGION 'Documentation'}
  ///	<summary>Provides access to information on a drive.</summary>
  ///	<remarks>
  ///	  Use the static <see cref=
  ///	  "Spring.Utils|TDriveInfo.GetDrives">TDriveInfo.GetDrives</see> method to retrieve
  ///	  all drives of the computer.
  ///	  <alert class="caller">Caller must use the <see cref=
  ///	  "IsReady">IsReady</see> property to check whether the drive is ready
  ///	  before accessing other members. Otherwise, an <see cref=
  ///	  "Spring|EIOException" /> exception will be raised if it
  ///	  is not ready.</alert>
  ///	</remarks>
  /// <seealso href="http://msdn.microsoft.com/en-us/library/system.io.driveinfo.aspx">System.IO.DriveInfo (.Net Framework)</seealso>
  {$ENDREGION}
  TDriveInfo = record
  private
    fDriveName: string;
    fRootDirectory: string;
    fAvailableFreeSpace: Int64;
    fTotalSize: Int64;
    fTotalFreeSpace: Int64;
    fVolumeName: array[0..MAX_PATH] of Char;
    fFileSystemName: array[0..MAX_PATH] of Char;
    fSerialNumber: DWORD;
    fMaximumComponentLength: DWORD;
    fFileSystemFlags: DWORD;
    function GetAvailableFreeSpace: Int64;
    function GetDriveFormat: string;
    function GetDriveType: TDriveType;
    function GetDriveTypeString: string;
    function GetIsReady: Boolean;
    function GetTotalFreeSpace: Int64;
    function GetTotalSize: Int64;
    function GetVolumeLabel: string;
    procedure SetVolumeLabel(const Value: string);
  private
    procedure UpdateProperties;
  public
    constructor Create(const driveName: string);

    ///	<summary>Retrieves the drive names of all logical drives on a
    ///	computer.</summary>
    class function GetDrives: TArray<TDriveInfo>; static;

    procedure CheckIsReady;

    ///	<summary>Indicates the amount of available free space on a
    ///	drive.</summary>
    property AvailableFreeSpace: Int64 read GetAvailableFreeSpace;

    ///	<summary>Gets the name of the file system, such as NTFS or
    ///	FAT32.</summary>
    property DriveFormat: string read GetDriveFormat;

    ///	<summary>Gets the drive type.</summary>
    property DriveType: TDriveType read GetDriveType;

    ///	<summary>Gets the drive type.</summary>
    property DriveTypeString: string read GetDriveTypeString;

    ///	<summary>Gets a value indicating whether a drive is ready.</summary>
    property IsReady: Boolean read GetIsReady;

    ///	<summary>Gets the name of a drive.</summary>
    property Name: string read fDriveName;

    ///	<summary>Gets the root directory of a drive.</summary>
    property RootDirectory: string read fRootDirectory;

    ///	<summary>Gets the total amount of free space available on a
    ///	drive.</summary>
    property TotalFreeSpace: Int64 read GetTotalFreeSpace;

    ///	<summary>Gets the total size of storage space on a drive.</summary>
    property TotalSize: Int64 read GetTotalSize;

    ///	<summary>Gets or sets the volume label of a drive.</summary>
    property VolumeLabel: string read GetVolumeLabel write SetVolumeLabel;
  end;


  {$REGION 'TFileVersionInfo'}

  {$REGION 'Documentation'}
  ///	<summary>Provides version information for a physical file on
  ///	disk.</summary>
  ///	<remarks>
  ///	  Use the <see cref="GetVersionInfo(string)">GetVersionInfo</see>
  ///	  method of this class to get a FileVersionInfo containing
  ///	  information about a file, then look at the properties for information
  ///	  about the file. Call <see cref="ToString"></see> to get
  ///	  a partial list of properties and their values for this file.
  ///	  <para>The TFileVersionInfo properties are based on version
  ///	  resource information built into the file. Version resources are often
  ///	  built into binary files such as .exe or .dll files; text files do not
  ///	  have version resource information.</para>
  ///	  <para>Version resources are typically specified in a Win32 resource
  ///	  file, or in assembly attributes. For example the <see cref=
  ///	  "IsDebug"></see> property reflects theVS_FF_DEBUG flag value
  ///	  in the file's VS_FIXEDFILEINFO block, which is built from
  ///	  the VERSIONINFO resource in a Win32 resource file. For more
  ///	  information about specifying version resources in a Win32 resource
  ///	  file, see "About Resource Files" and "VERSIONINFO Resource" in the
  ///	  Platform SDK.</para>
  ///	</remarks>
  {$ENDREGION}
  TFileVersionInfo = record
  private
    type
      TLangAndCodePage = record
        Language: Word;
        CodePage: Word;
      end;

      TLangAndCodePageArray  = array[0..9] of TLangAndCodePage;
      PTLangAndCodePageArray = ^TLangAndCodePageArray;

      TFileVersionResource = record
      private
        fBlock: Pointer;
        fLanguage: Word;
        fCodePage: Word;
      public
        constructor Create(block: Pointer; language, codePage: Word);
        function ReadString(const stringName: string): string;
        property Language: Word read fLanguage;
        property CodePage: Word read fCodePage;
      end;
  strict private
    fExists: Boolean;
    fFileFlags: DWORD;
    fComments: string;
    fCompanyName: string;
    fFileName: string;
    fFileVersion: string;
    fFileVersionNumber: TVersion;
    fFileDescription: string;
    fProductName: string;
    fProductVersion: string;
    fProductVersionNumber: TVersion;
    fInternalName: string;
    fLanguage: string;
    fLegalCopyright: string;
    fLegalTrademarks: string;
    fOriginalFilename: string;
    fPrivateBuild: string;
    fSpecialBuild: string;
    function GetIsDebug: Boolean;
    function GetIsPatched: Boolean;
    function GetIsPreRelease: Boolean;
    function GetIsPrivateBuild: Boolean;
    function GetIsSpecialBuild: Boolean;
  private
    constructor Create(const fileName: string);
    procedure LoadVersionResource(const resource: TFileVersionResource);
  public
    ///	<summary>Gets the file version info of the specified file.</summary>
    ///	<exception cref="Spring|EFileNotFoundException">Raised if the file
    ///	doesn't exist.</exception>
    class function GetVersionInfo(const fileName: string): TFileVersionInfo; static;
    function ToString: string;
    property Exists: Boolean read fExists;
    property Comments: string read fComments;
    property CompanyName: string read fCompanyName;
    property FileName: string read fFileName;
    property FileDescription: string read fFileDescription;
    property FileVersion: string read fFileVersion;
    property FileVersionNumber: TVersion read fFileVersionNumber;
    property InternalName: string read fInternalName;
    property Language: string read fLanguage;
    property LegalCopyright: string read fLegalCopyright;
    property LegalTrademarks: string read fLegalTrademarks;
    property OriginalFilename: string read fOriginalFilename;
    property ProductName: string read fProductName;
    property ProductVersion: string read fProductVersion;
    property ProductVersionNumber: TVersion read fProductVersionNumber;
    property PrivateBuild: string read fPrivateBuild;
    property SpecialBuild: string read fSpecialBuild;
    property IsDebug: Boolean read GetIsDebug;
    property IsPatched: Boolean read GetIsPatched;
    property IsPreRelease: Boolean read GetIsPreRelease;
    property IsSpecialBuild: Boolean read GetIsSpecialBuild;
    property IsPrivateBuild: Boolean read GetIsPrivateBuild;
  end;

  {$ENDREGION}


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

  {$REGION 'Documentation'}
  ///	<summary>Specifies enumerated constants used to retrieve directory paths
  ///	to system special folders.</summary>
  ///	<remarks>
  ///	  <para>The system special folders are folders such as <b>Program
  ///	  Files</b>, <b>Programs</b>, <b>System</b>,
  ///	  or <b>Startup</b>, which contain common information. Special
  ///	  folders are set by default by the system, or explicitly by the user,
  ///	  when installing a version of Windows.</para>
  ///	  <para>The <see cref=
  ///	  "TEnvironment.GetFolderPath(TSpecialFolder)">GetFolderPath</see> method
  ///	  returns the locations associated with this enumeration. The locations
  ///	  of these folders can have different values on different operating
  ///	  systems, the user can change some of the locations, and the locations
  ///	  are localized.</para>
  ///	  <para>For more information about special folders, see
  ///	  the <see href=
  ///	  "http://go.microsoft.com/fwlink/?LinkId=116664">CSIDL</see> values
  ///	  topic.</para>
  ///	</remarks>
  {$ENDREGION}
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
    /// <summary>
    /// Unknown processor
    /// </summary>
    paUnknown,
    /// <summary>
    /// Intel x86 and compatible microprocessors.
    /// </summary>
    paX86,
    /// <summary>
    /// 64-bit Intel and compatible microprocessors.
    /// </summary>
    paIA64,
    /// <summary>
    /// 64-bit AMD microprocessors.
    /// </summary>
    paAmd64
  );

  {$REGION 'Documentation'}
  ///	<summary>Provides information about, and means to manipulate, the current
  ///	environment.</summary>
  ///	<remarks>Use the TEnvironment structure to retrieve information such
  ///	as command-line arguments, environment variable settings. </remarks>
  {$ENDREGION}
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
    ///	<summary>Returns a string array containing the command-line arguments
    ///	for the current process.</summary>
    class function  GetCommandLineArgs: TStringDynArray; overload; static;

    // TODO: Consider using Extract*** insteading of Get*** for the methods with a
    // TString parameter.

    ///	<summary>Returns a string array containing the command-line arguments
    ///	for the current process.</summary>
    class procedure GetCommandLineArgs(list: TStrings); overload; static;

    ///	<summary>Returns an array of string containing the names of the logical
    ///	drives on the current computer.</summary>
    class function  GetLogicalDrives: TStringDynArray; overload; static;

    class procedure GetLogicalDrives(list: TStrings); overload; static;

    ///	<summary>Gets the path to the system special folder that is identified
    ///	by the specified enumeration.</summary>
    class function  GetFolderPath(const folder: TSpecialFolder): string; static;

    ///	<summary>Retrieves the value of an environment variable from the
    ///	current process.</summary>
    class function  GetEnvironmentVariable(const variable: string): string; overload; static;

    ///	<summary>Retrieves the value of an environment variable from the
    ///	current process or from the Windows operating system registry key for
    ///	the current user or local machine.</summary>
    class function  GetEnvironmentVariable(const variable: string; target: TEnvironmentVariableTarget): string; overload; static;

    ///	<summary>Retrieves all environment variable names and their values from
    ///	the current process.</summary>
    class procedure GetEnvironmentVariables(list: TStrings); overload; static;

    ///	<summary>Retrieves the value of an environment variable from the
    ///	current process or from the Windows operating system registry key for
    ///	the current user or local machine.</summary>
    class procedure GetEnvironmentVariables(list: TStrings; target: TEnvironmentVariableTarget); overload; static;

    ///	<summary>Creates, modifies, or deletes an environment variable stored
    ///	in the current process.</summary>
    class procedure SetEnvironmentVariable(const variable, value: string); overload; static;

    ///	<summary>Creates, modifies, or deletes an environment variable stored
    ///	in the current process or in the Windows operating system registry key
    ///	reserved for the current user or local machine.</summary>
    class procedure SetEnvironmentVariable(const variable, value: string; target: TEnvironmentVariableTarget); overload; static;

    ///	<summary>Replaces the name of each environment variable embedded in the
    ///	specified string with the string equivalent of the value of the
    ///	variable, then returns the resulting string.</summary>
    class function ExpandEnvironmentVariables(const variable: string): string; static;

    class property ApplicationPath: string read fApplicationPath;

    class property ApplicationVersion: TVersion read fApplicationVersion;

    class property ApplicationVersionInfo: TFileVersionInfo read fApplicationVersionInfo;

    class property ApplicationVersionString: string read fApplicationVersionString;

    ///	<summary>Gets the command line for this process.</summary>
    class property CommandLine: string read GetCommandLine;

    ///	<summary>Gets or sets the fully qualified path of the current working
    ///	directory.</summary>
    class property CurrentDirectory: string read GetCurrentDirectory write SetCurrentDirectory;

    class property IsAdmin: Boolean read GetIsAdmin; { experimental }

    ///	<summary>Gets the NetBIOS name of this local computer.</summary>
    class property MachineName: string read GetMachineName;

    ///	<summary>Gets the newline string defined for this
    ///	environment.</summary>
    class property NewLine: string read GetNewLine;

    ///	<summary>Gets an <see cref="TOperatingSystem" /> object that
    ///	contains the current platform identifier and version number.</summary>
    class property OperatingSystem: TOperatingSystem read fOperatingSystem;

    ///	<summary>Gets the number of processors on the current
    ///	machine.</summary>
    class property ProcessorCount: Integer read GetProcessorCount;

    class property ProcessorArchitecture: TProcessorArchitecture read GetProcessorArchitecture;

    class property RegisteredOrganization: string read GetRegisteredOrganization;

    class property RegisteredOwner: string read GetRegisteredOwner;

    ///	<summary>Gets the fully qualified path of the system
    ///	directory.</summary>
    class property SystemDirectory: string read GetSystemDirectory;

    ///	<summary>Gets the number of milliseconds elapsed since the system
    ///	started.</summary>
    class property TickCount: Cardinal read GetTickCount;

    ///	<summary>Gets the network domain name associated with the current
    ///	user.</summary>
    class property UserDomainName: string read GetUserDomainName;

    ///	<summary>Gets the user name of the person who is currently logged on to
    ///	the Windows operating system.</summary>
    class property UserName: string read GetUserName;

    ///	<summary>Gets a value indicating whether the current process is running
    ///	in user interactive mode.</summary>
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


  /// <summary>
  /// Defines an anonymous function which returns a callback pointer.
  /// </summary>
  TCallbackFunc = TFunc<Pointer>;

  {$REGION '> Adapts class instance (object) method as standard callback function.'}
  ///	<summary>Adapts class instance (object) method as standard callback
  ///	function.</summary>
  ///	<remarks>Both the object method and the callback function need to be
  ///	declared as stdcall.</remarks>
  ///	<example>
  ///	  This sample shows how to call CreateCallback method.
  ///	  <code>
  ///	private
  ///	  fCallback: TCallbackFunc;
  ///	//...
  ///	fCallback := CreateCallback(Self, @TSomeClass.SomeMethod);
  ///	</code>
  ///	</example>
  {$ENDREGION}
  TCallback = class(TInterfacedObject, TCallbackFunc)
  private
    fInstance: Pointer;
  public
    constructor Create(objectAddress: TObject; methodAddress: Pointer);
    destructor Destroy; override;
    function Invoke: Pointer;
  end; // Consider hide the implementation.


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

  ///	<summary>Creates a standard callback function which was adapted from a
  ///	instance method.</summary>
  ///	<param name="obj">an instance</param>
  ///	<param name="methodAddress">address of an instance method</param>
  function CreateCallback(obj: TObject; methodAddress: Pointer): TCallbackFunc;

  /// <summary>
  /// Try setting focus to a control.
  /// </summary>
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

const
  DriveTypeStrings: array[TDriveType] of string = (
    SUnknownDriveDescription,
    SNoRootDirectoryDescription,
    SRemovableDescription,
    SFixedDescription,
    SNetworkDescription,
    SCDRomDescription,
    SRamDescription
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

function CreateCallback(obj: TObject; methodAddress: Pointer): TCallbackFunc;
begin
  TArgument.CheckNotNull(obj, 'obj');
  TArgument.CheckNotNull(methodAddress, 'methodAddress');
  Result := TCallback.Create(obj, methodAddress);
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


{$REGION 'TDriveInfo'}

constructor TDriveInfo.Create(const driveName: string);
var
  s: string;
begin
  s := UpperCase(driveName);
  if not (Length(s) in [1..3]) or not CharInSet(s[1], ['A'..'Z']) then
  begin
    raise EArgumentException.Create('driveName');
  end;
  case Length(s) of
    1:
    begin
      fRootDirectory := s + DriveDelim + PathDelim;
    end;
    2:
    begin
      if s[2] <> DriveDelim then
      begin
        raise EArgumentException.Create('driveName');
      end;
      fRootDirectory := s + PathDelim;
    end;
    3:
    begin
      if s[2] <> DriveDelim then
        raise EArgumentException.Create('driveName');
      if s[3] <> PathDelim then
        raise EArgumentException.Create('driveName');
      fRootDirectory := s;
    end;
    else
    begin
      Assert(False);
    end;
  end;
  Assert(Length(fRootDirectory) = 3, 'Length of fRootDirectory should be 3.');
  fDriveName := Copy(fRootDirectory, 1, 2);
end;

class function TDriveInfo.GetDrives: TArray<TDriveInfo>;
var
  drives: TStringDynArray;
  i: Integer;
begin
  drives := Environment.GetLogicalDrives;
  SetLength(Result, Length(drives));
  for i := 0 to High(drives) do
  begin
    Result[i] := TDriveInfo.Create(drives[i]);
  end;
end;

procedure TDriveInfo.CheckIsReady;
begin
  if not IsReady then
  begin
    raise EIOException.CreateResFmt(@SDriveNotReady, [fDriveName]);
  end;
end;

procedure TDriveInfo.UpdateProperties;
begin
  CheckIsReady;
  Win32Check(SysUtils.GetDiskFreeSpaceEx(
    PChar(fRootDirectory),
    fAvailableFreeSpace,
    fTotalSize,
    @fTotalFreeSpace
  ));
  Win32Check(Windows.GetVolumeInformation(
    PChar(fRootDirectory),
    fVolumeName,
    Length(fVolumeName),
    @fSerialNumber,
    fMaximumComponentLength,
    fFileSystemFlags,
    fFileSystemName,
    Length(fFileSystemName)
  ));
end;

function TDriveInfo.GetAvailableFreeSpace: Int64;
begin
  UpdateProperties;
  Result := fAvailableFreeSpace;
end;

function TDriveInfo.GetDriveFormat: string;
begin
  UpdateProperties;
  Result := fFileSystemName;
end;

function TDriveInfo.GetDriveType: TDriveType;
var
  value: Cardinal;
begin
  value := Windows.GetDriveType(PChar(fRootDirectory));
  case value of
    DRIVE_NO_ROOT_DIR:  Result := dtNoRootDirectory;
    DRIVE_REMOVABLE:    Result := dtRemovable;
    DRIVE_FIXED:        Result := dtFixed;
    DRIVE_REMOTE:       Result := dtNetwork;
    DRIVE_CDROM:        Result := dtCDRom;
    DRIVE_RAMDISK:      Result := dtRam;
    else                Result := dtUnknown;  // DRIVE_UNKNOWN
  end;
end;

function TDriveInfo.GetDriveTypeString: string;
begin
  Result := DriveTypeStrings[Self.DriveType];
end;

function TDriveInfo.GetIsReady: Boolean;
begin
  Result := Length(fRootDirectory) > 0;
  Result := Result and (SysUtils.DiskSize(Ord(fRootDirectory[1]) - $40) > -1);
end;

function TDriveInfo.GetTotalFreeSpace: Int64;
begin
  UpdateProperties;
  Result := fTotalFreeSpace;
end;

function TDriveInfo.GetTotalSize: Int64;
begin
  UpdateProperties;
  Result := fTotalSize;
end;

function TDriveInfo.GetVolumeLabel: string;
begin
  UpdateProperties;
  Result := fVolumeName;
end;

procedure TDriveInfo.SetVolumeLabel(const Value: string);
begin
  CheckIsReady;
  Win32Check(Windows.SetVolumeLabel(PChar(fRootDirectory), PChar(value)));
end;

{$ENDREGION}


{$REGION 'TFileVersionInfo'}

constructor TFileVersionInfo.Create(const fileName: string);
var
  block: Pointer;
  fixedFileInfo: PVSFixedFileInfo;
  translations: PTLangAndCodePageArray;
  size: DWORD;
  valueSize: DWORD;
  translationSize: Cardinal;
  translationCount: Integer;
  dummy: DWORD;
begin
  Finalize(Self);
  ZeroMemory(@Self, SizeOf(Self));
  fFileName := fileName;
  CheckFileExists(fFileName);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  UniqueString(fFileName);
  size := GetFileVersionInfoSize(PChar(fFileName), dummy);
  fExists := size <> 0;
  if fExists then
  begin
    block := AllocMem(size);
    try
      Win32Check(Windows.GetFileVersionInfo(
        PChar(fFileName),
        0,
        size,
        block
      ));
      Win32Check(VerQueryValue(
        block,
        '\',
        Pointer(fixedFileInfo),
        valueSize
      ));
      Win32Check(VerQueryValue(
        block,
        '\VarFileInfo\Translation',
        Pointer(translations),
        translationSize
      ));
      fFileVersionNumber := TVersion.Create(
        HiWord(fixedFileInfo.dwFileVersionMS),
        LoWord(fixedFileInfo.dwFileVersionMS),
        HiWord(fixedFileInfo.dwFileVersionLS),
        LoWord(fixedFileInfo.dwFileVersionLS)
      );
      fProductVersionNumber := TVersion.Create(
        HiWord(fixedFileInfo.dwProductVersionMS),
        LoWord(fixedFileInfo.dwProductVersionMS),
        HiWord(fixedFileInfo.dwProductVersionLS),
        LoWord(fixedFileInfo.dwProductVersionLS)
      );
      fFileFlags := fixedFileInfo.dwFileFlags;
      translationCount := translationSize div SizeOf(TLangAndCodePage);
      if translationCount > 0 then
      begin
        LoadVersionResource(
          TFileVersionResource.Create(
            block,
            translations[0].Language,
            translations[0].CodePage
          )
        );
      end;
    finally
      FreeMem(block);
    end;
  end;
end;

class function TFileVersionInfo.GetVersionInfo(
  const fileName: string): TFileVersionInfo;
var
  localFileName: string;
begin
  localFileName := Environment.ExpandEnvironmentVariables(fileName);
  Result := TFileVersionInfo.Create(localFileName);
end;

procedure TFileVersionInfo.LoadVersionResource(const resource: TFileVersionResource);
begin
  fCompanyName := resource.ReadString('CompanyName');
  fFileDescription := resource.ReadString('FileDescription');
  fFileVersion := resource.ReadString('FileVersion');
  fInternalName := resource.ReadString('InternalName');
  fLegalCopyright := resource.ReadString('LegalCopyright');
  fLegalTrademarks := resource.ReadString('LegalTrademarks');
  fOriginalFilename := resource.ReadString('OriginalFilename');
  fProductName := resource.ReadString('ProductName');
  fProductVersion := resource.ReadString('ProductVersion');
  fComments := resource.ReadString('Comments');
  fLanguage := Languages.NameFromLocaleID[resource.Language];
end;

function TFileVersionInfo.ToString: string;
begin
  Result := Format(SFileVersionInfoFormat, [
    FileName,
    InternalName,
    OriginalFilename,
    FileVersion,
    FileDescription,
    ProductName,
    ProductVersion,
    BoolToStr(IsDebug, True),
    BoolToStr(IsPatched, True),
    BoolToStr(IsPreRelease, True),
    BoolToStr(IsPrivateBuild, True),
    BoolToStr(IsSpecialBuild, True),
    Language
  ]);
end;

function TFileVersionInfo.GetIsDebug: Boolean;
begin
  Result := (fFileFlags and VS_FF_DEBUG) <> 0;
end;

function TFileVersionInfo.GetIsPatched: Boolean;
begin
  Result := (fFileFlags and VS_FF_PATCHED) <> 0;
end;

function TFileVersionInfo.GetIsPreRelease: Boolean;
begin
  Result := (fFileFlags and VS_FF_PRERELEASE) <> 0;
end;

function TFileVersionInfo.GetIsPrivateBuild: Boolean;
begin
  Result := (fFileFlags and VS_FF_PRIVATEBUILD) <> 0;
end;

function TFileVersionInfo.GetIsSpecialBuild: Boolean;
begin
  Result := (fFileFlags and VS_FF_SPECIALBUILD) <> 0;
end;

{ TFileVersionInfo.TFileVersionData }

constructor TFileVersionInfo.TFileVersionResource.Create(block: Pointer;
  language, codePage: Word);
begin
  fBlock := block;
  fLanguage := language;
  fCodePage := codePage;
end;

function TFileVersionInfo.TFileVersionResource.ReadString(
  const stringName: string): string;
var
  subBlock: string;
  data: PChar;
  len: Cardinal;
const
  SubBlockFormat = '\StringFileInfo\%4.4x%4.4x\%s';   // do not localize
begin
  subBlock := Format(
    SubBlockFormat,
    [fLanguage, fCodePage, stringName]
  );
  data := nil;
  len := 0;
  VerQueryValue(fBlock, PChar(subBlock), Pointer(data), len);
  Result := data;
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


{$REGION 'TCallback'}

type
  PInstruction = ^TInstruction;
  TInstruction = array[1..16] of Byte;

{----------------------------}
{        Code DASM           }
{----------------------------}
{  push  [ESP]               }
{  mov   [ESP+4], ObjectAddr }
{  jmp   MethodAddr          }
{----------------------------}

/// <author>
/// savetime
/// </author>
/// <seealso>http://savetime.delphibbs.com</seealso>
constructor TCallback.Create(objectAddress: TObject; methodAddress: Pointer);
const
  Instruction: TInstruction = (
    $FF,$34,$24,$C7,$44,$24,$04,$00,$00,$00,$00,$E9,$00,$00,$00,$00
  );
var
  p: PInstruction;
begin
  inherited Create;
  New(p);
  Move(Instruction, p^, SizeOf(Instruction));
  PInteger(@p[8])^ := Integer(objectAddress);
  PInteger(@p[13])^ := Longint(methodAddress) - (Longint(p) + SizeOf(Instruction));
  fInstance := p;
end;

destructor TCallback.Destroy;
begin
  Dispose(fInstance);
  inherited Destroy;
end;

function TCallback.Invoke: Pointer;
begin
  Result := fInstance;
end;

{$ENDREGION}

end.
