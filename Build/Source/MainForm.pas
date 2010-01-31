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

unit MainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Registry,
  ShellAPI,
  Spring.System,
  Spring.Utils;

type
  TConfigurationType = (
    ctDebug,
    ctRelease
  );

  TfrmMain = class(TForm)
    btnBuild: TButton;
    btnClose: TButton;
    grpOptions: TGroupBox;
    chkBrowsingPath: TCheckBox;
    chkAddLibrary: TCheckBox;
    grpConfiguration: TGroupBox;
    rbDebug: TRadioButton;
    rbRelease: TRadioButton;
    mmoDetails: TMemo;
    lblDetails: TLabel;
    procedure btnBuildClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure rbDebugClick(Sender: TObject);
    procedure rbReleaseClick(Sender: TObject);
  private
    const
      fCBdsKey = 'Software\CodeGear\BDS\7.0';
      fCBdsLibraryKey = 'Software\CodeGear\BDS\7.0\Library';
      fCGlobalsKey = 'Software\CodeGear\BDS\7.0\Globals';
      fCEnvironmentVariablesKey = 'Software\CodeGear\BDS\7.0\Environment Variables';
      fCLibraryPathName = 'Search Path';
      fCBrowsingPathName = 'Browsing Path';
      fCRootDirName = 'RootDir';
  private
    { Private declarations }
    fConfigurationType: TConfigurationType;
    fRegistry: TRegistry;
    fLibraryPaths: TStringList;
    fBrowsingPaths: TStringList;
    fBdsDir: string;
    function GetConfigurationName: string;
    procedure AddSourcePaths(const projectPath: string; strings: TStrings);
    procedure RemoveProjectPaths(const projectPath: string; strings: TStrings);
  public
    { Public declarations }
    procedure LoadParameters;
    procedure UpdateParameters;
    procedure ExecuteTasks;
    property ConfigurationName: string read GetConfigurationName;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  ConfigurationNames: array[TConfigurationType] of string = (
    'Debug',
    'Release'
  );

type
  TStringsHelper = class helper for TStrings
  public
    procedure AddOrUpdate(const s: string);
  end;

{ TStringsHelper }

procedure TStringsHelper.AddOrUpdate(const s: string);
begin
  if IndexOf(s) = -1 then
  begin
    Add(s);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fRegistry := TRegistry.Create;
  fLibraryPaths := TStringList.Create;
  fLibraryPaths.Delimiter := ';';
  fLibraryPaths.StrictDelimiter := True;
  fBrowsingPaths := TStringList.Create;
  fBrowsingPaths.Delimiter := ';';
  fBrowsingPaths.StrictDelimiter := True;
  fConfigurationType := ctRelease;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBrowsingPaths.Free;
  fLibraryPaths.Free;
  fRegistry.Free;
end;

procedure TfrmMain.rbDebugClick(Sender: TObject);
begin
  fConfigurationType := ctDebug;
end;

procedure TfrmMain.rbReleaseClick(Sender: TObject);
begin
  fConfigurationType := ctRelease;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnBuildClick(Sender: TObject);
begin
  LoadParameters;
  UpdateParameters;
  ExecuteTasks;
end;

function TfrmMain.GetConfigurationName: string;
begin
  Result := ConfigurationNames[fConfigurationType];
end;

procedure TfrmMain.LoadParameters;
begin
  with fRegistry do
  begin
    RootKey := HKEY_CURRENT_USER;
    if not OpenKey(fCBdsKey, False) then
    begin
      raise Exception.Create('Failed: RAD Studio 2010 was not installed.');
    end;
    fBdsDir := ReadString(fCRootDirName);
    CloseKey;
    if not OpenKey(fCBdsLibraryKey, False) then
    begin
      raise Exception.Create('Failed to read the registry.');
    end;
    fLibraryPaths.DelimitedText := ReadString(fCLibraryPathName);
    fBrowsingPaths.DelimitedText := ReadString(fCBrowsingPathName);
  end;
end;

procedure TfrmMain.UpdateParameters;
var
  libraryPath: string;
  projectPath: string;
begin
  projectPath := ApplicationPath;
  libraryPath := projectPath + 'Lib\D2010\' + ConfigurationName;
  if not ForceDirectories(libraryPath) then
  begin
    raise Exception.CreateFmt('Failed to create the directory: "%s"', [libraryPath]);
  end;
  RemoveProjectPaths(projectPath, fLibraryPaths);
  RemoveProjectPaths(projectPath, fBrowsingPaths);
  if chkAddLibrary.Checked then
  begin
    fLibraryPaths.AddOrUpdate(libraryPath);
  end
  else
  begin
    AddSourcePaths(projectPath, fLibraryPaths);
  end;
  fRegistry.WriteString(fCLibraryPathName, fLibraryPaths.DelimitedText);
  if chkBrowsingPath.Checked then
  begin
    AddSourcePaths(projectPath, fBrowsingPaths);
    fRegistry.WriteString(fCBrowsingPathName, fBrowsingPaths.DelimitedText);
  end;
  fRegistry.CloseKey;
  if not fRegistry.OpenKey(fCGlobalsKey, False) then
  begin
    raise Exception.Create('Failed to open the "Globals" key.');
  end;
  fRegistry.WriteString('ForceEnvOptionsUpdate', '1');
  fRegistry.CloseKey;
  if not fRegistry.OpenKey(fCEnvironmentVariablesKey, False) then
  begin
    raise Exception.Create('Failed to open the "EnvironmentVariables" key.');
  end;
  fRegistry.WriteString('SPRING', ExtractFileDir(Application.ExeName));
  fRegistry.WriteString('SPRING_LIBRARY', ApplicationPath + 'Lib\D2010');
  fRegistry.CloseKey;
end;

procedure TfrmMain.ExecuteTasks;
var
  path: string;
begin
  TEnvironment.SetEnvironmentVariable('BDS', fBdsDir);
  TEnvironment.SetEnvironmentVariable('SPRING', ExtractFileDir(Application.ExeName));
  TEnvironment.SetEnvironmentVariable('SPRING_LIBRARY', ApplicationPath + 'Lib\D2010');
  path := TEnvironment.GetEnvironmentVariable('Path');
  path := TEnvironment.ExpandEnvironmentVariables('%WINDIR%\Microsoft.NET\Framework\v2.0.50727;') + path;
  TEnvironment.SetEnvironmentVariable('PATH', path);
//  ShowMessage(TEnvironment.GetEnvironmentVariable('BDS'));
//  ShowMessage(TEnvironment.GetEnvironmentVariable('PATH'));
  ShellExecute(Handle, PChar('open'), PChar(ApplicationPath + 'Build.bat'), PChar(ConfigurationName), nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.AddSourcePaths(const projectPath: string; strings: TStrings);
begin
  Assert(strings <> nil, 'strings should not be nil.');
  with strings do
  begin
    // TODO: Move these paths to a configuration file.
    AddOrUpdate('$(SPRING)\Source\Base');
    AddOrUpdate('$(SPRING)\Source\Base\Cryptography');
    AddOrUpdate('$(SPRING)\Source\Base\Collections');
    AddOrUpdate('$(SPRING)\Source\Base\Utils');
    AddOrUpdate('$(SPRING)\Source\Core');
    AddOrUpdate('$(SPRING)\Source\Core\IoC');
    AddOrUpdate('$(SPRING)\Source\Core\Pool');
    AddOrUpdate('$(SPRING)\Source\Extensions');
    AddOrUpdate('$(SPRING)\Source\Extensions\Numbering');
  end;
end;

procedure TfrmMain.RemoveProjectPaths(const projectPath: string;
  strings: TStrings);
var
  i: Integer;
  path: string;
begin
  Assert(strings <> nil, 'strings should not be nil.');
  for i := strings.Count - 1 downto 0 do
  begin
    path := strings[i];
    if (Pos(projectPath, path) > 0) or (Pos('$(SPRING)', path) > 0) then
    begin
      strings.Delete(i);
    end;
  end;
end;

end.
