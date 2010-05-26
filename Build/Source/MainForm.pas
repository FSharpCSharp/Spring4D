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
  CheckLst,
  ComCtrls,
  Spring.System,
  Spring.Utils,
  BuildEngine;

type
  TfrmMain = class(TForm)
    btnBuild: TButton;
    btnClose: TButton;
    grpConfiguration: TGroupBox;
    rbDebug: TRadioButton;
    rbRelease: TRadioButton;
    mmoDetails: TMemo;
    lblDetails: TLabel;
    grpTargets: TGroupBox;
    lbTargets: TCheckListBox;
    lblHomepage: TLinkLabel;
    BalloonHint1: TBalloonHint;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure rbDebugClick(Sender: TObject);
    procedure rbReleaseClick(Sender: TObject);
    procedure lbTargetsClickCheck(Sender: TObject);
    procedure lblHomepageLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
    fBuildEngine: TBuildEngine;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  target: TTarget;
  index: Integer;
begin
  fBuildEngine := TBuildEngine.Create;
  fBuildEngine.Configure(ApplicationPath + 'Build.Settings.ini');
  fBuildEngine.ConfigurationType := ctRelease;

  lbTargets.Clear;
  for target in fBuildEngine.Targets do
  begin
    index := lbTargets.Items.AddObject(target.DisplayName, target);
    lbTargets.ItemEnabled[index] := target.Exists;
    lbTargets.Checked[index] := target.Exists and fBuildEngine.IsSelected(target);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBuildEngine.Free;
end;

procedure TfrmMain.rbDebugClick(Sender: TObject);
begin
  fBuildEngine.ConfigurationType := ctDebug;
end;

procedure TfrmMain.rbReleaseClick(Sender: TObject);
begin
  fBuildEngine.ConfigurationType := ctRelease;
end;

procedure TfrmMain.lblHomepageLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(Handle, 'open', PChar(Link), nil, nil, SW_NORMAL);
end;

procedure TfrmMain.lbTargetsClickCheck(Sender: TObject);
var
  target: TTarget;
  i: Integer;
begin
  fBuildEngine.SelectedTargets.Clear;
  for i := 0 to lbTargets.Count - 1 do
  begin
    if lbTargets.Checked[i] then
    begin
      target := TTarget(lbTargets.Items.Objects[i]);
      fBuildEngine.SelectedTargets.Add(target);
    end;
  end;
  btnBuild.Enabled := not fBuildEngine.SelectedTargets.IsEmpty;
end;

procedure TfrmMain.btnBuildClick(Sender: TObject);
var
  target: TTarget;
begin
  for target in fBuildEngine.SelectedTargets do
  begin
    fBuildEngine.BuildTarget(target);
  end;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
