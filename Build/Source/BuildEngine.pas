{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2013 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
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

unit BuildEngine;

interface

uses
  Classes,
  IniFiles,
  Registry,
  ShellAPI,
  SysUtils,
  Windows,
  Spring,
  Spring.Collections,
  Spring.Utils;

type
  {$SCOPEDENUMS ON}

  TConfigurationType = (
    Debug,
    Release
  );

  TCompilerTargetBase = class
  strict private
    fBrowsingPaths: TStrings;
    fDisplayName: string;
    fEnvironmentVariables: TStrings;
    fExists: Boolean;
    fLibraryPaths: TStrings;
    fPlatform: string;
    fRootDir: string;
    fTypeName: string;
  strict protected
    function GetBrowsingPaths(): TStrings; virtual;
    function GetEnvironmentVariables(): TStrings; virtual;
    function GetLibraryPaths(): TStrings; virtual;
  public
    destructor Destroy(); override;
    property BrowsingPaths: TStrings read GetBrowsingPaths;
    {TODO -o##jwp -cFix : R/O}
    property DisplayName: string read fDisplayName write fDisplayName;
    property EnvironmentVariables: TStrings read GetEnvironmentVariables;
    {TODO -o##jwp -cFix : R/O}
    property Exists: Boolean read fExists write fExists;
    property LibraryPaths: TStrings read GetLibraryPaths;
    {TODO -o##jwp -cFix : R/O}
    property Platform: string read fPlatform write fPlatform;
    {TODO -o##jwp -cFix : R/O}
    property RootDir: string read fRootDir write fRootDir;
    {TODO -o##jwp -cFix : R/O}
    property TypeName: string read fTypeName write fTypeName;
  end;
  TCompilerTarget = class(TCompilerTargetBase)
  strict private
    type
      TKeys = record
        BDS: string;
        LibraryKey: string;
        Globals: string;
        EnvironmentVariables: string;
      end;
      TNames = record
        RootDir: string;
        LibraryPath: string;
        BrowsingPath: string;
      end;
  strict private
    fRegistry: TRegistry;
    fKeys: TKeys;
    fNames: TNames;
  strict protected
    procedure EnsureOpenKey(const aKey: string; aCreateIfNotExists: Boolean = False); virtual;
    procedure LoadEnvironmentVariables(const aEnvironmentVariables: TStrings); virtual;
    procedure SaveEnvironmentVariables(const aEnvironmentVariables: TStrings); virtual;
    property Keys: TKeys read fKeys;
    property Names: TNames read fNames;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(const aTypeName: string; const aProperties: TStrings); virtual;
    procedure LoadOptions; virtual;
    procedure SaveOptions; virtual;
  end;

  TBuildTask = class
  public
    constructor Create;
    destructor Destroy; override;
  public
    Compiler: TCompilerTarget;
    Projects: TStrings;
    UnitOutputPath: string;
    function Name: string; virtual;
    function CanBuild: Boolean; virtual;
  end;

  TBuildEngineBase = class
  strict private
    fConfigurationType: TConfigurationType;
    FModifyDelphiRegistrySettings: Boolean;
    FPauseAfterEachStep: Boolean;
    fRunTests: Boolean;
    fSelectedTasks: IList<TBuildTask>;
    fSourcePaths: TStrings;
    fTargets: IList<TCompilerTarget>;
    fTasks: IList<TBuildTask>;
  strict protected
    function GetSelectedTasks(): IList<TBuildTask>; virtual;
    function GetSourcePaths(): TStrings; virtual;
    function GetTargets(): IList<TCompilerTarget>; virtual;
    function GetTasks(): IList<TBuildTask>; virtual;
    property Targets: IList<TCompilerTarget> read GetTargets;
  public
    destructor Destroy(); override;
    property ConfigurationType: TConfigurationType read fConfigurationType write fConfigurationType;
    property ModifyDelphiRegistrySettings: Boolean read FModifyDelphiRegistrySettings write FModifyDelphiRegistrySettings;
    property PauseAfterEachStep: Boolean read FPauseAfterEachStep write FPauseAfterEachStep;
    property RunTests: Boolean read fRunTests write fRunTests;
    property SelectedTasks: IList<TBuildTask> read GetSelectedTasks;
    property SourcePaths: TStrings read GetSourcePaths;
    property Tasks: IList<TBuildTask> read GetTasks;
  end;

  TBuildEngine = class(TBuildEngineBase)
  strict private
    fSourceBaseDir: string;
  strict protected
    procedure RemoveReleatedEntries(const aBaseDir: string; aEntries: TStrings); virtual;
    procedure ExecuteCommandLine(const aApplicationName, aCommandLine: string;
      var exitCode: Cardinal; const aWorkingDirectory: string = ''); virtual;
    procedure BuildTarget(const aTask: TBuildTask); virtual;
  public
    constructor Create;
    procedure ConfigureCompilers(const aFileName: string);
    procedure LoadSettings(const aFileName: string);
    procedure SaveSettings(const aFileName: string);

    procedure CleanUp;
    procedure BuildAll;
  end;

  ECommandLineException = class(Exception);
  EBuildException = class(Exception);

resourcestring
  SFailedToOpenRegistryKey = 'Failed to open the registry key: "%s".';
  SFailedToCreateProcess = 'Failed to create the process: "%s".';
  SBuildFailed = 'Failed to build the task: "%s"';

implementation

const
  SPause = ' pause';
  ConfigurationNames: array[TConfigurationType] of string = (
    'Debug',
    'Release'
  );

type
  TStringsHelper = class helper for TStrings
  public
    function AddIfNotExists(const S: string): Integer; virtual;
    procedure AddStringsThatNotExist(const aStrings: TStrings); overload; virtual;
    function GetValueOrDefault(const aName, aDefaultValue: string): string;
  end;

{$REGION 'TStringsHelper'}
function TStringsHelper.AddIfNotExists(const S: string): Integer;
begin
  if -1 = Self.IndexOf(S) then
    Result := Add(S)
  else
    Result := -1;
end;

procedure TStringsHelper.AddStringsThatNotExist(const aStrings: TStrings);
var
  I: Integer;
  S: string;
begin
  BeginUpdate();
  try
    for I := 0 to aStrings.Count - 1 do
    begin
      S := aStrings[I];
      if -1 = Self.IndexOf(S) then
        AddObject(S, aStrings.Objects[I]);
    end;
  finally
    EndUpdate();
  end;
end;

function TStringsHelper.GetValueOrDefault(const aName, aDefaultValue: string): string;
var
  index: Integer;
begin
  index := IndexOfName(aName);
  if index > -1 then
  begin
    Result := ValueFromIndex[index];
  end
  else
  begin
    Result := aDefaultValue;
  end;
end;

{$ENDREGION}


{$REGION 'TCompilerTarget'}

constructor TCompilerTarget.Create;
begin
  inherited Create();
  fRegistry := TRegistry.Create();
  fRegistry.RootKey := HKEY_CURRENT_USER;
  LibraryPaths.Delimiter := ';';
  LibraryPaths.StrictDelimiter := True;
  BrowsingPaths.Delimiter := ';';
  BrowsingPaths.StrictDelimiter := True;
end;

destructor TCompilerTarget.Destroy;
begin
  fRegistry.Free();
  inherited Destroy();
end;

procedure TCompilerTarget.EnsureOpenKey(const aKey: string; aCreateIfNotExists: Boolean);
begin
  if not fRegistry.OpenKey(aKey, aCreateIfNotExists) then
  begin
    raise ERegistryException.CreateResFmt(@SFailedToOpenRegistryKey, [aKey]);
  end;
end;

procedure TCompilerTarget.LoadEnvironmentVariables(const aEnvironmentVariables: TStrings);
var
  i: Integer;
begin
  if fRegistry.KeyExists(Keys.EnvironmentVariables) then
  begin
    EnsureOpenKey(Keys.EnvironmentVariables);
    fRegistry.GetValueNames(aEnvironmentVariables);
    with aEnvironmentVariables do {TODO -o##jwp -cCleanup : Remove with}
    for i := 0 to Count - 1 do
    begin
      Strings[i] := Strings[i] + NameValueSeparator + fRegistry.ReadString(Strings[i]);
    end;
    fRegistry.CloseKey;
  end;
end;

procedure TCompilerTarget.SaveEnvironmentVariables(const aEnvironmentVariables: TStrings);
var
  i: Integer;
begin
  EnsureOpenKey(Keys.EnvironmentVariables, True);
  with aEnvironmentVariables do {TODO -o##jwp -cCleanup : Remove with}
  for i := 0 to Count - 1 do
  begin
    fRegistry.WriteString(Names[i], ValueFromIndex[i]);
  end;
  fRegistry.CloseKey;
end;

procedure TCompilerTarget.LoadOptions;
var
  lPath: string;
begin
  with fRegistry do {TODO -o##jwp -cCleanup : Remove with}
  begin
    EnsureOpenKey(Keys.BDS);
    RootDir := ReadString(Names.RootDir);
    CloseKey;

    EnsureOpenKey(Keys.LibraryKey);
    lPath := ReadString(Names.LibraryPath);
    LibraryPaths.DelimitedText := lPath;
    lPath := ReadString(Names.BrowsingPath);
    BrowsingPaths.DelimitedText := lPath;
    CloseKey;

    LoadEnvironmentVariables(EnvironmentVariables);
  end;
end;

procedure TCompilerTarget.SaveOptions;
begin
  EnsureOpenKey(Keys.LibraryKey);
  fRegistry.WriteString(Names.LibraryPath, LibraryPaths.DelimitedText);
  fRegistry.WriteString(Names.BrowsingPath, BrowsingPaths.DelimitedText);
  fRegistry.CloseKey;

  SaveEnvironmentVariables(EnvironmentVariables);

  EnsureOpenKey(Keys.Globals);
  fRegistry.WriteString('ForceEnvOptionsUpdate', '1');
  fRegistry.CloseKey;
end;

procedure TCompilerTarget.Configure(const aTypeName: string; const aProperties: TStrings);
var
  lFileName: string;
begin
  Guard.CheckNotNull(aProperties, 'aProperties');

  TypeName := aTypeName;
  DisplayName := aProperties.GetValueOrDefault('DisplayName', '');
  Platform := aProperties.GetValueOrDefault('Platform', 'Win32');
  fKeys.BDS := aProperties.GetValueOrDefault('Keys.BDS', '');
  fKeys.LibraryKey := IncludeTrailingPathDelimiter(fKeys.BDS) + aProperties.GetValueOrDefault('Keys.Library', 'Library');
  fKeys.Globals := IncludeTrailingPathDelimiter(fKeys.BDS) + aProperties.GetValueOrDefault('Keys.Globals', 'Globals');
  fKeys.EnvironmentVariables := IncludeTrailingPathDelimiter(fKeys.BDS) + aProperties.GetValueOrDefault('Keys.EnvironmentVariables', 'Environment Variables');
  fNames.LibraryPath := aProperties.GetValueOrDefault('Names.LibraryPath', 'Search Path');
  fNames.BrowsingPath := aProperties.GetValueOrDefault('Names.BrowsingPath', 'Browsing Path');
  fNames.RootDir := aProperties.GetValueOrDefault('Names.RootDir', 'RootDir');
  Exists := fRegistry.KeyExists(fKeys.BDS);
  if Exists then
  begin
    EnsureOpenKey(fKeys.BDS);
    lFileName := fRegistry.ReadString('App');
    Exists := FileExists(lFileName);
    fRegistry.CloseKey;
  end;
end;

{$ENDREGION}


{$REGION 'TBuildTask'}

constructor TBuildTask.Create;
begin
  inherited Create();
  Projects := TStringList.Create();
  Projects.Delimiter := ';';
  Projects.StrictDelimiter := True;
end;

destructor TBuildTask.Destroy;
begin
  Projects.Free();
  inherited Destroy();
end;

function TBuildTask.Name: string;
begin
  Result := Compiler.DisplayName;
end;

function TBuildTask.CanBuild: Boolean;
begin
  Result := Compiler.Exists;
end;

{$ENDREGION}


{$REGION 'TBuildEngine'}

constructor TBuildEngine.Create;
begin
  inherited Create();
  ConfigurationType := TConfigurationType.Release;
  SourcePaths.Delimiter := ';';
  SourcePaths.StrictDelimiter := True;
end;

procedure TBuildEngine.ExecuteCommandLine(const aApplicationName, aCommandLine: string;
  var exitCode: Cardinal; const aWorkingDirectory: string);
const
  nSize: Cardinal = 1024;
var
  lLocalCommandLine: string;
  lStartupInfo: TStartupInfo;
  lProcessInfo: TProcessInformation;
  lCurrentDirectory: PChar;
begin
  ZeroMemory(@lStartupInfo, SizeOf(lStartupInfo));
  ZeroMemory(@lProcessInfo, SizeOf(lProcessInfo));
  lStartupInfo.cb := SizeOf(lStartupInfo);
  lLocalCommandLine := aCommandLine;
  UniqueString(lLocalCommandLine);
  if aWorkingDirectory <> '' then
    lCurrentDirectory := PChar(aWorkingDirectory)
  else
    lCurrentDirectory := nil;
  if not CreateProcess(PChar(aApplicationName), PChar(lLocalCommandLine), nil, nil, True,
    0, nil, lCurrentDirectory, lStartupInfo, lProcessInfo) then
  begin
    raise ECommandLineException.CreateResFmt(@SFailedToCreateProcess, [aApplicationName]);
  end;
  try
    WaitForSingleObject(lProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(lProcessInfo.hProcess, exitCode);
  finally
    CloseHandle(lProcessInfo.hProcess);
    CloseHandle(lProcessInfo.hThread);
  end;
end;

procedure TBuildEngine.BuildAll;
var
  lTask: TBuildTask;
begin
  for lTask in SelectedTasks do
  begin
    BuildTarget(lTask);
  end;
end;

procedure TBuildEngine.BuildTarget(const aTask: TBuildTask);
var
  lProjectPath: string;
  lUnitOutputPath: string;
  lConfigurationName: string;
  lProjectName: string;
  lCommandFileName: string;
  lCommandLine: string;
  lExitCode: Cardinal;
  lPauseAfterEachStep: string;
  lPlatform: string;
  lRsVars: string;
  lTarget: TCompilerTarget;
begin
  Guard.CheckNotNull(aTask, 'aTask');
  lTarget := aTask.Compiler;

  lProjectPath := ExtractFilePath(ParamStr(0));
  lConfigurationName := ConfigurationNames[ConfigurationType];

  RemoveReleatedEntries(lProjectPath, lTarget.LibraryPaths);
  RemoveReleatedEntries(lProjectPath, lTarget.BrowsingPaths);

  lUnitOutputPath := lProjectPath + aTask.UnitOutputPath;
  lUnitOutputPath := StringReplace(lUnitOutputPath, '$(Config)', lConfigurationName, [rfIgnoreCase, rfReplaceAll]);
  lPlatform := lTarget.Platform;
  lUnitOutputPath := StringReplace(lUnitOutputPath, '$(Platform)', lPlatform, [rfIgnoreCase, rfReplaceAll]);

  lTarget.LibraryPaths.AddIfNotExists(lUnitOutputPath);
  lTarget.BrowsingPaths.AddStringsThatNotExist(SourcePaths);
  if ModifyDelphiRegistrySettings then
    lTarget.SaveOptions();

  lCommandFileName := IncludeTrailingPathDelimiter(TEnvironment.GetFolderPath(sfSystem)) + 'cmd.exe';
  lRsVars := IncludeTrailingPathDelimiter(lTarget.RootDir) + 'bin\rsvars.bat';
  for lProjectName in aTask.Projects do
  begin
    if PauseAfterEachStep then
      lPauseAfterEachStep := SPause
    else
      lPauseAfterEachStep := '';
    lCommandLine := Format('/C BuildHelper "%0:s" "%1:s" "Config=%2:s" "Platform=%3:s"%4:s', [
      lRsVars, lProjectName, lConfigurationName, lPlatform, lPauseAfterEachStep
    ]);
    ExecuteCommandLine(lCommandFileName, lCommandLine, lExitCode);
    if lExitCode <> 0 then
    begin
      raise EBuildException.CreateResFmt(@SBuildFailed, [lProjectName]);
    end;
  end;

  if RunTests then
  begin
    if (lPlatForm = 'Win32') or (lPlatform = 'Win64') then
    begin
      lCommandLine := Format('%0:s\Tests\Bin\%1:s\Spring.Tests.exe', [
        ExcludeTrailingPathDelimiter(lProjectPath),
        StringReplace(aTask.Compiler.TypeName, '.', '\', [])]);
      ExecuteCommandLine(lCommandLine, '', lExitCode, ExtractFileDir(lCommandLine));
    end;
  end;
end;

procedure TBuildEngine.CleanUp;
const
  SCClean = '/C Clean';
var
  lCleanCommand: string;
  lCommandFileName: string;
  lExitCode: Cardinal;
begin
  lCommandFileName := IncludeTrailingPathDelimiter(TEnvironment.GetFolderPath(sfSystem)) + 'cmd.exe';
  if PauseAfterEachStep then
    lCleanCommand := SCClean + SPause
  else
    lCleanCommand := SCClean;
  ExecuteCommandLine(lCommandFileName, lCleanCommand, lExitCode);
end;

procedure TBuildEngine.ConfigureCompilers(const aFileName: string);
var
  lIni: TIniFile;
  lSections: TStrings;
  lProperties: TStrings;
  lSectionName: string;
  lTarget: TCompilerTarget;
begin
  CheckFileExists(aFileName);

  Targets.Clear();

  lIni := TIniFile.Create(aFileName);
  lSections := nil;
  lProperties := nil;
  try
    lSections := TStringList.Create();
    lProperties := TStringList.Create();

    lIni.ReadSections(lSections);

    for lSectionName in lSections do
    begin
      lIni.ReadSectionValues(lSectionName, lProperties);
      lTarget := TCompilerTarget.Create();
      Targets.Add(lTarget);
      lTarget.Configure(lSectionName, lProperties);
      if lTarget.Exists then
        lTarget.LoadOptions();
    end;
  finally
    lProperties.Free();
    lSections.Free();
    lIni.Free();
  end;
end;

procedure TBuildEngine.LoadSettings(const aFileName: string);
var
  lIni: TCustomIniFile;
  lSections: TStrings;
  lSectionName: string;
  lConfig: string;
  lTarget: TCompilerTarget;
  lTask: TBuildTask;
  i: Integer;
  lSelectedTasks: TStrings;
begin
  lIni := TIniFile.Create(aFileName);
  lSections := TStringList.Create();
  lSelectedTasks := TStringList.Create();
  lSelectedTasks.Delimiter := ';';
  try
    lConfig := lIni.ReadString('Globals', 'Config', 'Debug');
    if SameText(lConfig, 'Debug') then
      ConfigurationType := TConfigurationType.Debug
    else
      ConfigurationType := TConfigurationType.Release;
    fSourceBaseDir := lIni.ReadString('Globals', 'SourceBaseDir', '');
    fSourceBaseDir := ApplicationPath + fSourceBaseDir;
    SourcePaths.DelimitedText := lIni.ReadString('Globals', 'SourcePaths', '');
    for i := 0 to SourcePaths.Count - 1 do
    begin
      SourcePaths[i] := IncludeTrailingPathDelimiter(fSourceBaseDir) + SourcePaths[i];
    end;
    lSelectedTasks.DelimitedText := lIni.ReadString('Globals', 'SelectedTasks', '');
    PauseAfterEachStep := lIni.ReadBool('Globals', 'PauseAfterEachStep', False);
    RunTests := lIni.ReadBool('Globals', 'RunTests', False);
    ModifyDelphiRegistrySettings := lIni.ReadBool('Globals', 'ModifyDelphiRegistrySettings', False);

    for lTarget in Targets do
    begin
      lSectionName := lTarget.TypeName;
      if lIni.SectionExists(lSectionName) then
      begin
        lTask := TBuildTask.Create();
        Tasks.Add(lTask);
        lTask.Compiler := lTarget;
        lTask.Projects.DelimitedText := lIni.ReadString(lSectionName, 'Projects', '');
        lTask.UnitOutputPath := lIni.ReadString(lSectionName, 'UnitOutputPaths', '');
        if lTask.CanBuild and ((lSelectedTasks.Count = 0) or (lSelectedTasks.IndexOf(lSectionName) > -1)) then
          SelectedTasks.Add(lTask);
      end;
    end;
  finally
    lSelectedTasks.Free();
    lSections.Free();
    lIni.Free();
  end;
end;

procedure TBuildEngine.RemoveReleatedEntries(const aBaseDir: string; aEntries: TStrings);
var
  lEntry: string;
  i: Integer;
begin
  Assert(aEntries <> nil, 'entries should not be nil.');
  for i := aEntries.Count - 1 downto 0 do
  begin
    lEntry := aEntries[i];
    if (Pos(aBaseDir, lEntry) > 0) {or (Pos('$(SPRING)', entry) > 0)} then
    begin
      aEntries.Delete(i);
    end;
  end;
end;

procedure TBuildEngine.SaveSettings(const aFileName: string);
var
  lIni: TCustomIniFile;
  lSelectedTasks: TStrings;
  lTask: TBuildTask;
begin
  lIni := TIniFile.Create(aFileName);
  lSelectedTasks := TStringList.Create();
  lSelectedTasks.Delimiter := ';';
  try
    for lTask in SelectedTasks do
    begin
      lSelectedTasks.Add(lTask.Compiler.TypeName);
    end;
    lIni.WriteString('Globals', 'Config', ConfigurationNames[ConfigurationType]);
    lIni.WriteString('Globals', 'SelectedTasks', lSelectedTasks.DelimitedText);
    lIni.WriteBool('Globals', 'PauseAfterEachStep', PauseAfterEachStep);
    lIni.WriteBool('Globals', 'RunTests', RunTests);
    lIni.WriteBool('Globals', 'ModifyDelphiRegistrySettings', ModifyDelphiRegistrySettings);
  finally
    lIni.Free();
    lSelectedTasks.Free();
  end;
end;

destructor TCompilerTargetBase.Destroy();
begin
  fBrowsingPaths.Free();
  fEnvironmentVariables.Free();
  fLibraryPaths.Free();
  inherited Destroy();
end;

function TCompilerTargetBase.GetBrowsingPaths(): TStrings;
begin
  if not Assigned(fBrowsingPaths) then
    fBrowsingPaths := TStringList.Create();
  Result := fBrowsingPaths;
end;

function TCompilerTargetBase.GetEnvironmentVariables(): TStrings;
begin
  if not Assigned(fEnvironmentVariables) then
    fEnvironmentVariables := TStringList.Create();
  Result := fEnvironmentVariables;
end;

function TCompilerTargetBase.GetLibraryPaths(): TStrings;
begin
  if not Assigned(FLibraryPaths) then
    FLibraryPaths := TStringList.Create();
  Result := fLibraryPaths;
end;

destructor TBuildEngineBase.Destroy();
begin
  fSourcePaths.Free();
  inherited Destroy();
end;

function TBuildEngineBase.GetSelectedTasks(): IList<TBuildTask>;
begin
  // SelectedTasks references elements in Tasks (Tasks owns all objects), hence CreateList
  if not Assigned(fSelectedTasks) then
    fSelectedTasks := TCollections.CreateList<TBuildTask>;
  Result := fSelectedTasks;
end;

function TBuildEngineBase.GetSourcePaths(): TStrings;
begin
  if not Assigned(fSourcePaths) then
    fSourcePaths := TStringList.Create;
  Result := fSourcePaths;
end;

function TBuildEngineBase.GetTargets(): IList<TCompilerTarget>;
begin
  // Targets owns all objects, hence CreateObjectList
  if not Assigned(fTargets) then
    fTargets := TCollections.CreateObjectList<TCompilerTarget>;
  Result := fTargets;
end;

function TBuildEngineBase.GetTasks(): IList<TBuildTask>;
begin
  // Tasks owns all objects, hence CreateObjectList
  if not Assigned(fTasks) then
    fTasks := TCollections.CreateObjectList<TBuildTask>;
  Result := fTasks;
end;

{$ENDREGION}

end.
