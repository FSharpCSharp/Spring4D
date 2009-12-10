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

unit DemoForm;

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
  Grids,
  ValEdit,
  ComCtrls,
  StdCtrls,
  StrUtils,
  ExtCtrls,
  XPMan,
  Sockets,
  Generics.Collections;

type
  TfrmDemo = class(TForm)
    pgcMain: TPageControl;
    tsGeneral: TTabSheet;
    tsSpecialFolders: TTabSheet;
    vleGeneral: TValueListEditor;
    Panel1: TPanel;
    Label1: TLabel;
    vleSpecialFolders: TValueListEditor;
    tsEnvironmentVariables: TTabSheet;
    tsNetwork: TTabSheet;
    vleEnvironment: TValueListEditor;
    Label2: TLabel;
    lblHomepage: TLabel;
    vleNetwork: TValueListEditor;
    Panel2: TPanel;
    rgEnvironmentVariableTarget: TRadioGroup;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    tsDriveInfo: TTabSheet;
    mmoDriveInfo: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    tsServiceController: TTabSheet;
    Label8: TLabel;
    lvServices: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblHomepageClick(Sender: TObject);
    procedure rgEnvironmentVariableTargetClick(Sender: TObject);
    procedure pgcMainChange(Sender: TObject);
  private
    { Private declarations }
    fList: TStrings;
    fInitializationList: TDictionary<TTabSheet, TProc>;
    procedure InitializeGeneralPage;
    procedure InitializeEnvironmentVariables;
    procedure InitializeSpecialFolders;
    procedure InitializeNetwork;
    procedure InitializeDriveInfo;
    procedure InitializeServices;
  public
    { Public declarations }
  end;

var
  frmDemo: TfrmDemo;

implementation

uses
  ShellAPI,
  TypInfo,
  Spring.System,
  Spring.Utils;

{$R *.dfm}

type
  TStringsHelper = class helper for TStrings
    procedure AddOrUpdate(const name, value: string);
  end;

/// <summary>
/// Make sure the name exists even if its correlative value is empty.
/// </summary>
procedure TStringsHelper.AddOrUpdate(const name, value: string);
var
  index: Integer;
  s: string;
begin
  index := IndexOfName(name);
  s := name + NameValueSeparator + value;
  if index > -1 then
  begin
    Strings[index] := s;
  end
  else
  begin
    Add(s);
  end;
end;

//---------------------------------------------------------------------------

procedure TfrmDemo.FormCreate(Sender: TObject);
begin
  fList := TStringList.Create;  // Shared Strings
  fList.Delimiter := ';';
  fInitializationList := TDictionary<TTabSheet, TProc>.Create;
  with fInitializationList do
  begin
    Add(tsGeneral, Self.InitializeGeneralPage);
    Add(tsEnvironmentVariables, Self.InitializeEnvironmentVariables);
    Add(tsSpecialFolders, Self.InitializeSpecialFolders);
    Add(tsNetwork, Self.InitializeNetwork);
    Add(tsDriveInfo, Self.InitializeDriveInfo);
    Add(tsServiceController, Self.InitializeServices);
  end;
  pgcMain.ActivePage := tsGeneral;
  pgcMainChange(pgcMain);
end;

procedure TfrmDemo.FormDestroy(Sender: TObject);
begin
  fInitializationList.Free;
  fList.Free;
end;

procedure TfrmDemo.pgcMainChange(Sender: TObject);
var
  tabSheet: TTabSheet;
  initializationProc: TProc;
begin
  tabSheet := (sender as TPageControl).ActivePage;
  if fInitializationList.TryGetValue(tabSheet, initializationProc) then
  begin
    fInitializationList.Remove(tabSheet);
    initializationProc;
  end;
end;

procedure TfrmDemo.InitializeGeneralPage;
begin
  with vleGeneral do
  begin
    Strings.AddOrUpdate('ApplicationPath', ApplicationPath);
    Strings.AddOrUpdate('ApplicationVersion', ApplicationVersion.ToString);
    Strings.AddOrUpdate('ApplicationVersionString', ApplicationVersionString);
    Strings.AddOrUpdate('Environment.CommandLine', Environment.CommandLine);
    Environment.GetCommandLineArgs(fList);
    Strings.AddOrUpdate('Environment.GetCommandArgs', fList.DelimitedText);
    Strings.AddOrUpdate('Environment.CurrentDirectory', Environment.CurrentDirectory);
    Strings.AddOrUpdate('Environment.SystemDirectory', Environment.SystemDirectory);
    Strings.AddOrUpdate('Environment.MachineName', Environment.MachineName);
    Strings.AddOrUpdate('Environment.UserDomainName', Environment.UserDomainName);
    Strings.AddOrUpdate('Environment.UserName', Environment.UserName);
    Strings.AddOrUpdate('Environment.RegisteredOrganization', Environment.RegisteredOrganization);
    Strings.AddOrUpdate('Environment.RegisteredOwner', Environment.RegisteredOwner);
    Strings.AddOrUpdate('Environment.IsAdmin', BoolToStr(Environment.IsAdmin, True));
    Strings.AddOrUpdate('Environment.UserInteractive', BoolToStr(Environment.UserInteractive, True));
    Environment.GetLogicalDrives(fList);   { overloaded methods }
    Strings.AddOrUpdate('Environment.GetLogicalDrives', fList.DelimitedText);
    Strings.AddOrUpdate('Environment.OperatingSystem', Environment.OperatingSystem.ToString);
    Strings.AddOrUpdate('Environment.ProcessorCount', IntToStr(Environment.ProcessorCount));
    Strings.AddOrUpdate('Environment.ProcessorArchitecture', TEnum.GetName<TProcessorArchitecture>(Environment.ProcessorArchitecture));
    Strings.AddOrUpdate('Environment.TickCount', IntToStr(Environment.TickCount));
    Strings.AddOrUpdate('Environment.NewLine', TBuffer.Create(Environment.NewLine).ToHexString('$'));
  end;
end;

procedure TfrmDemo.InitializeEnvironmentVariables;
begin
  rgEnvironmentVariableTarget.ItemIndex := 0;
end;

procedure TfrmDemo.InitializeServices;
//var
//  service: TServiceController;
//  listItem: TListItem;
begin
//  with lvServices.Items do
//  begin
//    BeginUpdate;
//    try
//      for service in TServiceController.GetServices do
//      begin
//        listItem := Add;
//        listItem.Caption := service.DisplayName;
//        listItem.SubItems.Add(TEnum.GetName<TServiceStatus>(service.Status));
//        listItem.SubItems.Add(service.Description);
//      end;
//    finally
//      EndUpdate;
//    end;
//  end;
end;

procedure TfrmDemo.InitializeSpecialFolders;
var
  folder: TSpecialFolder;
  name: string;
  value: string;
begin
  for folder := Low(TSpecialFolder) to High(TSpecialFolder) do
  begin
    name := GetEnumName(TypeInfo(TSpecialFolder), Integer(folder));
    value := Environment.GetFolderPath(folder);
    vleSpecialFolders.Strings.AddOrUpdate(name, value);
  end;
end;

procedure TfrmDemo.InitializeNetwork;
begin
  {$WARNINGS OFF}
  with vleNetwork.Strings do
  begin
    AddOrUpdate('IsAvailable', BoolToStr(TNetwork.IsAvailable, True));
    AddOrUpdate('GetMacAddress', TNetwork.GetMacAddress);
    AddOrUpdate('GetIPAddress', TNetwork.GetIPAddress);
    try
      AddOrUpdate('GetPublicIPAddress', TNetwork.GetPublicIPAddress);
    except on e: Exception do
      AddOrUpdate('GetPublicIPAddress', e.ClassName + ': ' + e.Message);
    end;
  end;
  {$WARNINGS ON}
end;

procedure TfrmDemo.InitializeDriveInfo;
  procedure DoInitializeDriveInfo(strings: TStrings);
  begin
    UpdateStrings(strings,
      procedure
      var
        drive: TDriveInfo;
      begin
        for drive in TDriveInfo.GetDrives do
        begin
          if drive.IsReady then
          begin
            strings.Add(Format('Name: %s', [drive.Name]));
            strings.Add(Format('AvailableFreeSpace: %d', [drive.AvailableFreeSpace]));
            strings.Add(Format('TotalFreeSpace:     %d', [drive.TotalFreeSpace]));
            strings.Add(Format('TotalSize:   %d', [drive.TotalSize]));
            strings.Add(Format('DriveFormat: %s', [drive.DriveFormat]));
            strings.Add(Format('DriveType:   %s', [TEnum.GetName<TDriveType>(drive.DriveType)]));
            strings.Add(Format('VolumeLabel: %s', [drive.VolumeLabel]));
          end
          else
          begin
            strings.Add(Format('Drive %s is not ready', [drive.Name]));
          end;
          strings.Add('');
        end;
      end
    );
  end;
begin
  DoInitializeDriveInfo(mmoDriveInfo.Lines);
end;

procedure TfrmDemo.lblHomepageClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar('http://www.zuobaoquan.com'), nil, nil, 1);
end;

procedure TfrmDemo.rgEnvironmentVariableTargetClick(Sender: TObject);
var
  index: Integer;
  target: TEnvironmentVariableTarget;
begin
  index := rgEnvironmentVariableTarget.ItemIndex;
  target := TEnum.Parse<TEnvironmentVariableTarget>(index);
  Environment.GetEnvironmentVariables(fList, target);
  vleEnvironment.Strings := fList;
end;

end.
