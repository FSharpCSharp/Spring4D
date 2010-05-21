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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spring.Utils.IO, Buttons, ComCtrls;

type
  TfrmSearchDemo = class(TForm)
    btnStart: TButton;
    mmoFileTypes: TMemo;
    lblFileTypes: TLabel;
    btnStop: TButton;
    btnPause: TButton;
    btnResume: TButton;
    mmoLocations: TMemo;
    lblLocations: TLabel;
    lblResults: TLabel;
    grpOptions: TGroupBox;
    chkIncludeSubfolders: TCheckBox;
    lvResults: TListView;
    grpStatics: TGroupBox;
    btnClear: TButton;
    lbledtTotalCount: TLabeledEdit;
    lbledtElapsed: TLabeledEdit;
    edtFiles: TLabeledEdit;
    edtFolders: TLabeledEdit;
    tmr1: TTimer;
    rgScope: TRadioGroup;
    edtLocation: TLabeledEdit;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnResumeClick(Sender: TObject);
    procedure chkIncludeSubfoldersClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure rgScopeClick(Sender: TObject);
  private
    { Private declarations }
    fFileSearcher: TFileSearcher;
  public
    { Public declarations }
  end;

var
  frmSearchDemo: TfrmSearchDemo;

implementation

{$R *.dfm}

procedure TfrmSearchDemo.FormCreate(Sender: TObject);
begin
  fFileSearcher := TFileSearcher.Create;
  fFileSearcher.OnSearchBegin :=
    procedure (sender: TObject)
    begin
      lbledtTotalCount.Clear;
      lbledtTotalCount.Refresh;
      edtFiles.Clear;
      edtFolders.Clear;
      tmr1.Enabled := True;
      tmr1Timer(tmr1);
    end;
  fFileSearcher.OnSearchEnd :=
    procedure (sender: TObject)
    begin
      tmr1.Enabled := False;
    end;
  fFileSearcher.OnLocationChanged :=
    procedure (sender: TObject; const location: string)
    begin
      edtLocation.Text := location;
    end;
  fFileSearcher.OnProgress :=
    procedure (sender: TObject; const entry: TFileSystemEntry)
    var
      item: TListItem;
    begin
      item := lvResults.Items.Add;
      item.Caption := entry.Name;
      with item.SubItems do
      begin
        Add(entry.Location);
        Add(entry.TypeString);
        Add(entry.Size.ToString);
        Add(DateTimeToStr(entry.LastWriteTime));
      end;
      item.MakeVisible(True);
      with fFileSearcher.Statistics do
      begin
        lbledtTotalCount.Text := IntToStr(TotalCount);
        lbledtTotalCount.Refresh;
        edtFiles.Text := IntToStr(FileCount);
        edtFolders.Text := IntToStr(FolderCount);
      end;
    end;
  fFileSearcher.OnStatusChanged :=
    procedure (sender: TObject)
    begin
      mmoLocations.Enabled := fFileSearcher.CanStart;
      mmoFileTypes.Enabled := fFileSearcher.CanStart;
      btnStart.Enabled := fFileSearcher.CanStart;
      btnStop.Enabled := fFileSearcher.CanStop;
      btnPause.Enabled := fFileSearcher.CanPause;
      btnResume.Enabled := fFileSearcher.CanResume;
    end;
end;

procedure TfrmSearchDemo.FormDestroy(Sender: TObject);
begin
  fFileSearcher.Free;
end;

procedure TfrmSearchDemo.rgScopeClick(Sender: TObject);
begin
  fFileSearcher.SearchScope := TFileSearchScope(rgScope.ItemIndex);
end;

procedure TfrmSearchDemo.tmr1Timer(Sender: TObject);
begin
  with fFileSearcher.Statistics do
  begin
    lbledtElapsed.Text := Format('%.2d:%.2d', [Elapsed.Minutes, Elapsed.Seconds]);
    lbledtElapsed.Refresh;
  end;
end;

procedure TfrmSearchDemo.btnStartClick(Sender: TObject);
begin
  if mmoLocations.Lines.Count > 0 then
  begin
    fFileSearcher.Locations := mmoLocations.Lines;
    fFileSearcher.FileTypes := mmoFileTypes.Lines;
    fFileSearcher.Start;
  end;
  Application.ProcessMessages;
end;

procedure TfrmSearchDemo.btnStopClick(Sender: TObject);
begin
  fFileSearcher.Stop;
end;

procedure TfrmSearchDemo.chkIncludeSubfoldersClick(Sender: TObject);
begin
  fFileSearcher.IncludeSubfolders := chkIncludeSubfolders.Checked;
end;

procedure TfrmSearchDemo.btnClearClick(Sender: TObject);
begin
  lvResults.Clear;
end;

procedure TfrmSearchDemo.btnPauseClick(Sender: TObject);
begin
  fFileSearcher.Pause;
end;

procedure TfrmSearchDemo.btnResumeClick(Sender: TObject);
begin
  fFileSearcher.Resume;
end;

end.
