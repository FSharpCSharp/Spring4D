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

(*

+TFileMapping
+TFileMappingView
+TFileMappingStream

+TFileSearcher
+TFileSystemEntry

+TFileSystemWatcher

*)

unit Spring.System.IO;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Windows,
  IOUtils,
  Generics.Collections;

type
  TTextReader     = Classes.TTextReader;
  TStringReader   = Classes.TStringReader;
  TStreamReader   = Classes.TStreamReader;

  TTextWriter     = Classes.TTextWriter;
  TStringWriter   = Classes.TStringWriter;
  TStreamWriter   = Classes.TStreamWriter;

  TSearchOption   = IOUtils.TSearchOption;
  TPathPrefixType = IOUtils.TPathPrefixType;

  TDirectory      = IOUtils.TDirectory;
  TPath           = IOUtils.TPath;
  TFile           = IOUtils.TFile;

  TFileAttribute  = IOUtils.TFileAttribute;
  TFileAttributes = IOUtils.TFileAttributes;
  TFileMode       = IOUtils.TFileMode;
  TFileAccess     = IOUtils.TFileAccess;
  TFileShare      = IOUtils.TFileShare;

  TFileMapping        = class;
  TFileMappingView           = class;
  TFileMappingStream  = class;

  (*

  TFileAttribute = (
    faReadOnly,
    faHidden,
    faSystem,
    faDirectory,
    faArchive,
    faDevice,
    faNormal,
    faTemporary,
    faSparseFile,
    faReparsePoint,
    faCompressed,
    faOffline,
    faNotContentIndexed,
    faEncrypted
  );

  TFileAttributes = set of TFileAttribute;

  TFileMode = (
    fmCreateNew,
    fmCreate,
    fmOpen,
    fmOpenOrCreate,
    fmTruncate,
    fmAppend
  );

  /// <summary>
  /// TFileAccess
  /// </summary>
  TFileAccess = (
    faRead,       // The file is opened in read-only mode.
    faWrite,      // The file is created, truncated, or opened in write-only mode.
    faReadWrite   // The file is created, truncated, or opened in read-write mode.
  );

  TFileShare = (
    fsNone,
    fsRead,
    fsWrite,
    fsReadWrite
  );

  *)


  {$REGION 'TDriveInfo'}

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

  /// <summary>
  /// Provides access to information on a drive.
  /// </summary>
  /// <remarks>
  /// Use TDriveInfo.GetDrives method to retrieve all drives of the computer.
  /// Caller must check IsReady property before using TDriveInfo.
  /// </remarks>
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
    class function GetDrives: TArray<TDriveInfo>; static;
    procedure CheckIsReady;
    property AvailableFreeSpace: Int64 read GetAvailableFreeSpace;
    property DriveFormat: string read GetDriveFormat;
    property DriveType: TDriveType read GetDriveType;
    property DriveTypeString: string read GetDriveTypeString;
    property IsReady: Boolean read GetIsReady;
    property Name: string read fDriveName;
    property RootDirectory: string read fRootDirectory;
    property TotalFreeSpace: Int64 read GetTotalFreeSpace;
    property TotalSize: Int64 read GetTotalSize;
    property VolumeLabel: string read GetVolumeLabel write SetVolumeLabel;
  end;

  {$ENDREGION}


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


  {$REGION 'TFileSystemEntry (NOT READY)'}

//  TFileSystemEntry = record
//
//  end;

  {$ENDREGION}


  {$REGION 'TFileSearcher (NOT READY)'}

//  TFileSearcher = class
//
//  end;

  {$ENDREGION}


  {$REGION 'TFileSystemWatcher (NOT READY)'}

  /// <summary>
  /// Listens to the file system change notifications and raises events when a
  /// directory, or file in a directory, changes.
  /// </summary>
//  TFileSystemWatcher = record
//  end;

  {$ENDREGION}


implementation

uses
  Spring.System,
  Spring.ResourceStrings;

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


end.
