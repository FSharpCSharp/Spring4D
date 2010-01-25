unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Registry, ShellAPI, Spring.System, Spring.Utils;

type
  TConfigurationType = (
    ctDebug,
    ctRelease
  );

  TfrmMain = class(TForm)
    btnInstall: TButton;
    btnClose: TButton;
    grpOptions: TGroupBox;
    chkBrowsingPath: TCheckBox;
    chkAddLibrary: TCheckBox;
    grpConfiguration: TGroupBox;
    rbDebug: TRadioButton;
    rbRelease: TRadioButton;
    mmoDetails: TMemo;
    lblDetails: TLabel;
    procedure btnInstallClick(Sender: TObject);
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
  public
    { Public declarations }
    procedure LoadParameters;
    procedure UpdateParameters;
    procedure ExecuteTasks;
    procedure AddSourcePaths(const projectPath: string; strings: TStrings);
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

procedure TfrmMain.btnInstallClick(Sender: TObject);
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
  libraryPath := projectPath + 'Lib\' + ConfigurationName;
  CheckDirectoryExists(libraryPath);
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
end;

procedure TfrmMain.AddSourcePaths(const projectPath: string; strings: TStrings);
begin
  TArgument.CheckNotNull(strings, 'strings');
  with strings do
  begin
    AddOrUpdate(projectPath + 'Source\Base');
    AddOrUpdate(projectPath + 'Source\Base\Cryptography');
    AddOrUpdate(projectPath + 'Source\Base\Collections');
    AddOrUpdate(projectPath + 'Source\Base\Utils');
    AddOrUpdate(projectPath + 'Source\Core');
    AddOrUpdate(projectPath + 'Source\Core\IoC');
    AddOrUpdate(projectPath + 'Source\Core\Pool');
    AddOrUpdate(projectPath + 'Source\Extensions');
    AddOrUpdate(projectPath + 'Source\Extensions\Numbering');
  end;
end;

procedure TfrmMain.ExecuteTasks;
var
  path: string;
begin
  TEnvironment.SetEnvironmentVariable('BDS', fBdsDir);
  path := TEnvironment.GetEnvironmentVariable('Path');
  path := TEnvironment.ExpandEnvironmentVariables('%WINDIR%\Microsoft.NET\Framework\v2.0.50727;') + path;
  TEnvironment.SetEnvironmentVariable('PATH', path);
//  ShowMessage(TEnvironment.GetEnvironmentVariable('BDS'));
//  ShowMessage(TEnvironment.GetEnvironmentVariable('PATH'));
  ShellExecute(Handle, PChar('open'), PChar(ApplicationPath + 'Build.bat'), PChar(ConfigurationName), nil, SW_SHOWNORMAL);
end;

end.
