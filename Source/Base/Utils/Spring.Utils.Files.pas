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

unit Spring.Utils.Files;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  IOUtils,
  Generics.Collections,
  Spring.System;

type
  {$REGION 'TFileVersionInfo'}

  /// <summary>
  /// Provides version information for a physical file on disk.
  /// </summary>
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
    /// <summary>
    /// Returns a TFileVersionInfo object.
    /// </summary>
    class function GetVersionInfo(fileName: string): TFileVersionInfo; static;
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


  {$REGION 'File Mapping'}

  TFileMapping        = class;
  TFileMappingView    = class;
//  TFileMappingStream  = class;

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

  (*
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
  //*)

  {$ENDREGION}


implementation

uses
  Spring.ResourceStrings,
  Spring.Utils;

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
  fileName: string): TFileVersionInfo;
begin
  fileName := Environment.ExpandEnvironmentVariables(fileName);
  Result := TFileVersionInfo.Create(fileName);
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
    Int64Rec(maximumSize).Hi,
    Int64Rec(maximumSize).Lo,
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

(*

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
//*)

{$ENDREGION}

end.
