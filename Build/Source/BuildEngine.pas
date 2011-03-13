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

unit BuildEngine;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Windows,
  Registry,
  IniFiles,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Utils;

type
  TConfigurationType = (
    ctDebug,
    ctRelease
  );

  TTarget = class
  private
    type
      TKeys = record
        BDS: string;
        LibraryKey: string;
        Globals: string;
        EnvironmentVariables: string;
      end;

      TNames = record
        LibraryPath: string;
        BrowsingPath: string;
        RootPath: string;
      end;
  private
    fRegistry: TRegistry;
    fBrowsingPaths: TStrings;
    fLibraryPaths: TStrings;
    fEnvironmentVariables: TStrings;
    fID: string;
    fDisplayName: string;
    fRootPath: string;
    fExists: Boolean;
    fKeys: TKeys;
    fNames: TNames;
  protected
    procedure EnsureOpenKey(const key: string; createIfNotExists: Boolean = False);
    procedure LoadEnvironmentVariables(environmentVariables: TStrings);
    procedure SaveEnvironmentVariables(environmentVariables: TStrings);
    property Keys: TKeys read fKeys;
    property Names: TNames read fNames;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(properties: TStrings);
    procedure LoadParameters;
    procedure SaveParameters;
    property ID: string read fID;
    property DisplayName: string read fDisplayName;
    property Exists: Boolean read fExists;
    property RootPath: string read fRootPath;
    property LibraryPaths: TStrings read fLibraryPaths;
    property BrowsingPaths: TStrings read fBrowsingPaths;
    property EnvironmentVariables: TStrings read fEnvironmentVariables;
  end;

  TBuildEngine = class
  private
    fTargets: IList<TTarget>;
    fSelectedTargets: IList<TTarget>;
    fProjects: TStrings;
    fSourcePaths: TStrings;
    fUnitOutputPaths: TStrings;
    fConfigurationType: TConfigurationType;
    function GetTargets: IList<TTarget>;
    function GetSelectedTargets: IList<TTarget>;
    function AddTarget: TTarget;
    procedure RemoveReleatedEntries(const baseDir: string; entries: TStrings);
    procedure ExecuteCommandLine(const commandLine: string; var exitCode: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure(const fileName: string);
    procedure BuildTarget(target: TTarget);
    function IsSelected(target: TTarget): Boolean;
    property Targets: IList<TTarget> read GetTargets;
    property SelectedTargets: IList<TTarget> read GetSelectedTargets;
    property Projects: TStrings read fProjects;
    property SourcePaths: TStrings read fSourcePaths;
    property UnitOutputPaths: TStrings read fUnitOutputPaths;
    property ConfigurationType: TConfigurationType read fConfigurationType write fConfigurationType;
  end;

resourcestring
  SFailedToOpenRegistryKey = 'Failed to open the registry: "%s".';

implementation

{$REGION 'TTarget'}

constructor TTarget.Create;
begin
  inherited Create;
  fRegistry := TRegistry.Create;
  fRegistry.RootKey := HKEY_CURRENT_USER;
  fLibraryPaths := TStringList.Create;
  fLibraryPaths.Delimiter := ';';
  fLibraryPaths.StrictDelimiter := True;
  fBrowsingPaths := TStringList.Create;
  fBrowsingPaths.Delimiter := ';';
  fBrowsingPaths.StrictDelimiter := True;
  fEnvironmentVariables := TStringList.Create;
end;

destructor TTarget.Destroy;
begin
  fEnvironmentVariables.Free;
  fBrowsingPaths.Free;
  fLibraryPaths.Free;
  fRegistry.Free;
  inherited Destroy;
end;

procedure TTarget.EnsureOpenKey(const key: string; createIfNotExists: Boolean);
begin
  if not fRegistry.OpenKey(key, createIfNotExists) then
  begin
    raise Exception.CreateResFmt(@SFailedToOpenRegistryKey, [key]);
  end;
end;

procedure TTarget.LoadEnvironmentVariables(environmentVariables: TStrings);
var
  i: Integer;
begin
  if fRegistry.KeyExists(Keys.EnvironmentVariables) then
  begin
    EnsureOpenKey(Keys.EnvironmentVariables);
    fRegistry.GetValueNames(environmentVariables);
    with environmentVariables do
    for i := 0 to Count - 1 do
    begin
      Strings[i] := Strings[i] + NameValueSeparator + fRegistry.ReadString(Strings[i]);
    end;
    fRegistry.CloseKey;
  end;
end;

procedure TTarget.SaveEnvironmentVariables(environmentVariables: TStrings);
var
  i: Integer;
begin
  EnsureOpenKey(Keys.EnvironmentVariables, True);
  with environmentVariables do
  for i := 0 to Count - 1 do
  begin
    fRegistry.WriteString(Names[i], ValueFromIndex[i]);
  end;
  fRegistry.CloseKey;
end;

procedure TTarget.LoadParameters;
var
  path: string;
begin
  with fRegistry do
  begin
    EnsureOpenKey(Keys.BDS);
    fRootPath := ReadString(Names.RootPath);
    CloseKey;

    EnsureOpenKey(Keys.LibraryKey);
    path := ReadString(Names.LibraryPath);
    fLibraryPaths.DelimitedText := path;
    path := ReadString(Names.BrowsingPath);
    fBrowsingPaths.DelimitedText := path;
    CloseKey;

    LoadEnvironmentVariables(fEnvironmentVariables);
  end;
end;

procedure TTarget.SaveParameters;
begin
  EnsureOpenKey(Keys.LibraryKey);
  fRegistry.WriteString(Names.LibraryPath, fLibraryPaths.DelimitedText);
  fRegistry.WriteString(Names.BrowsingPath, fBrowsingPaths.DelimitedText);
  fRegistry.CloseKey;

  SaveEnvironmentVariables(fEnvironmentVariables);

  EnsureOpenKey(Keys.Globals);
  fRegistry.WriteString('ForceEnvOptionsUpdate', '1');
  fRegistry.CloseKey;
end;

procedure TTarget.Configure(properties: TStrings);
begin
  TArgument.CheckNotNull(properties, 'properties');
  fID := properties.Values['ID'];
  fDisplayName := properties.Values['DisplayName'];
  fKeys.BDS := properties.Values['Keys.BDS'];
  fKeys.LibraryKey := properties.Values['Keys.Library'];
  fKeys.Globals := properties.Values['Keys.Globals'];
  fKeys.EnvironmentVariables := properties.Values['Keys.EnvironmentVariables'];
  fNames.LibraryPath := properties.Values['Names.LibraryPath'];
  fNames.BrowsingPath := properties.Values['Names.BrowsingPath'];
  fNames.RootPath := properties.Values['Names.RootPath'];
  fExists := fRegistry.KeyExists(fKeys.BDS);
end;

{$ENDREGION}


{$REGION 'TBuildEngine'}

constructor TBuildEngine.Create;
begin
  inherited Create;
  fProjects := TStringList.Create;
  fSourcePaths := TStringList.Create;
  fUnitOutputPaths := TStringList.Create;
  fConfigurationType := ctRelease;
end;

destructor TBuildEngine.Destroy;
begin
  fUnitOutputPaths.Free;
  fSourcePaths.Free;
  fProjects.Free;
  inherited Destroy;
end;

procedure TBuildEngine.ExecuteCommandLine(const commandLine: string;
  var exitCode: Cardinal);
var
  localCommandLine: string;
  startupInfo: TStartupInfo;
  processInfo: TProcessInformation;
begin
  ZeroMemory(@startupInfo, SizeOf(startupInfo));
  ZeroMemory(@processInfo, SizeOf(processInfo));
  startupInfo.cb := SizeOf(startupInfo);
  localCommandLine := commandLine;
  UniqueString(localCommandLine);
  if not CreateProcess(nil, PChar(localCommandLine), nil, nil, True,
    CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
    nil, nil, startupInfo, processInfo) then
  begin
    raise Exception.Create('Failed to create the process.');
  end;
  WaitForSingleObject(processInfo.hProcess, INFINITE);
  GetExitCodeProcess(processInfo.hProcess, exitCode);
  CloseHandle(processInfo.hProcess);
  CloseHandle(processInfo.hThread);
end;

function TBuildEngine.AddTarget: TTarget;
begin
  Result := TTarget.Create;
  Targets.Add(Result);
end;

procedure TBuildEngine.BuildTarget(target: TTarget);
var
  projectPath: string;
  projectDir: string;
  libraryDir: string;
  configurationName: string;
  path: string;
  commandLine: string;
  projectName: string;
  exitCode: Cardinal;
//  steps: Integer;
const
  ConfigurationNames: array[TConfigurationType] of string = (
    'Debug',
    'Release'
  );
  SCleanCommandLine = 'msbuild.exe /nologo /t:Clean /p:Config=%0:s "%1:s"';
  SBuildCommandLine = 'msbuild.exe /nologo /t:Build /p:Config=%0:s "%1:s"';
begin
  TArgument.CheckNotNull(target, 'target');

  projectPath := ExtractFilePath(ParamStr(0));
  projectDir := ExcludeTrailingPathDelimiter(projectPath);
  configurationName := ConfigurationNames[fConfigurationType];
  target.LoadParameters;
  RemoveReleatedEntries(projectDir, target.LibraryPaths);
  RemoveReleatedEntries(projectDir, target.BrowsingPaths);
  libraryDir := UnitOutputPaths.Values[target.ID];
  path := projectPath + IncludeTrailingPathDelimiter(libraryDir) + configurationName;
  target.LibraryPaths.Add(path);
  target.BrowsingPaths.AddStrings(fSourcePaths);
  target.EnvironmentVariables.Values['SPRING'] := projectDir;
  target.EnvironmentVariables.Values['SPRING_LIBRARY'] := projectPath + libraryDir;
  target.SaveParameters;

  TEnvironment.SetEnvironmentVariable('SPRING', projectDir);
  TEnvironment.SetEnvironmentVariable('SPRING_LIBRARY', projectPath + libraryDir);

  // $(BDS)\rsvars.bat
  TEnvironment.SetEnvironmentVariable('BDS', target.RootPath);
//  TEnvironment.SetEnvironmentVariable('BDSCOMMONDIR', '');
  path := TEnvironment.GetEnvironmentVariable('Path');
  path := TEnvironment.ExpandEnvironmentVariables('%WINDIR%\Microsoft.NET\Framework\v2.0.50727;') + path;
  TEnvironment.SetEnvironmentVariable('PATH', path);

  (*
//  steps := fProjects.Count * 2;
  for projectName in fProjects do
  begin
    commandLine := Format(SCleanCommandLine, [configurationName, projectName]);
    ExecuteCommandLine(commandLine, exitCode);
    if exitCode <> 0 then
    begin
      raise Exception.CreateFmt('Failed to execute the clean task for "%s".', [projectName]);
    end;
//    Inc(steps);
  end;
  //*)
  for projectName in fProjects do
  begin
    commandLine := Format(SBuildCommandLine, [configurationName, projectName]);
    ExecuteCommandLine(commandLine, exitCode);
    if exitCode <> 0 then
    begin
      raise Exception.CreateFmt('Failed to execute the build task for "%s".', [projectName]);
    end;
//    Inc(steps);
  end;
end;

procedure TBuildEngine.Configure(const fileName: string);
var
  ini: TIniFile;
  appPath: string;
  properties: TStrings;
  targetCount: Integer;
  target: TTarget;
  i: Integer;
begin
  CheckFileExists(fileName);
  appPath := ExtractFilePath(ParamStr(0));
  ini := TIniFile.Create(fileName);
  properties := TStringList.Create;
  try
    ini.ReadSectionValues('SourcePaths', SourcePaths);
    for i := 0 to SourcePaths.Count - 1 do
    begin
      SourcePaths[i] := SourcePaths.ValueFromIndex[i];
    end;
    targetCount := ini.ReadInteger('General', 'TargetCount', 0);
    for i := 0 to targetCount - 1 do
    begin
      ini.ReadSectionValues('Target_' + IntToStr(i), properties);
      target := AddTarget;
      target.Configure(properties);
      if target.Exists then
      begin
        SelectedTargets.Add(target);
      end;
    end;
    ini.ReadSectionValues('UnitOutputPaths', fUnitOutputPaths);
    ini.ReadSectionValues('Projects', fProjects);
    for i := 0 to fProjects.Count - 1 do
    begin
      fProjects[i] :=  appPath + fProjects.ValueFromIndex[i];
    end;
  finally
    properties.Free;
    ini.Free;
  end;
end;

function TBuildEngine.IsSelected(target: TTarget): Boolean;
begin
  Result := (fSelectedTargets <> nil) and fSelectedTargets.Contains(target);
end;

procedure TBuildEngine.RemoveReleatedEntries(const baseDir: string; entries: TStrings);
var
  entry: string;
  i: Integer;
begin
  Assert(entries <> nil, 'entries should not be nil.');
  for i := entries.Count - 1 downto 0 do
  begin
    entry := entries[i];
    if (Pos(baseDir, entry) > 0) or (Pos('$(SPRING)', entry) > 0) then
    begin
      entries.Delete(i);
    end;
  end;
end;

function TBuildEngine.GetTargets: IList<TTarget>;
begin
  if fTargets = nil then
  begin
    fTargets := TCollections.CreateList<TTarget>(True);
  end;
  Result := fTargets;
end;

function TBuildEngine.GetSelectedTargets: IList<TTarget>;
begin
  if fSelectedTargets = nil then
  begin
    fSelectedTargets := TCollections.CreateList<TTarget>;
  end;
  Result := fSelectedTargets;
end;

{$ENDREGION}


end.
