{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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

unit Spring.Dialogs;

{$I Spring.inc}

interface

uses
  Windows, Controls, Dialogs;

type
  TModalResult  = Controls.TModalResult;
  TMessageDialogButton  = Dialogs.TMsgDlgBtn;
  TMessageDialogButtons = Dialogs.TMsgDlgButtons;

  TMessageDialogShowProc = reference to function(const text, caption: string;
    dialogType: TMsgDlgType; buttons: TMessageDialogButtons;
    defaultButton: TMessageDialogButton): TModalResult;

  /// <summary>
  /// Encapsulates common message dialogs.
  /// </summary>
  /// <author>Paul</author>
  /// <author>HR168</author>
  TDialog = class
  private
    class constructor Create;
    class var fMessageDialogProc: TMessageDialogShowProc;
    class function GetDefaultButton(const buttons: TMessageDialogButtons): TMessageDialogButton; inline;
    class function ShowMessageDialog(const text, caption: string;
      dialogType: TMsgDlgType; buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult;
  public
    { Information Dialogs }
    class function Info(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Info(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Info(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Info(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Warning Dialogs }
    class function Warn(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Warn(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Warn(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Warn(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Error Dialogs }
    class function Error(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Error(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Error(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Error(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Confirm Dialogs }
    class function Confirm(const text: string; const buttons: TMessageDialogButtons = [mbYes, mbNo]): TModalResult; overload;
    class function Confirm(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Confirm(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Confirm(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
  public
    class property MessageDialogProc: TMessageDialogShowProc read fMessageDialogProc write fMessageDialogProc;
  end;

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

implementation

const
  SWarning = '';
  SError = '';
  SInformation = '';
  SConfirm = '';

{ TDialog }

class constructor TDialog.Create;
begin
  fMessageDialogProc := ShowMessageDialog;
end;

class function TDialog.GetDefaultButton(
  const buttons: TMessageDialogButtons): TMessageDialogButton;
begin
  if mbOk in Buttons then
    Result := mbOk
  else if mbYes in Buttons then
    Result := mbYes
  else
    Result := mbRetry;
end;

class function TDialog.ShowMessageDialog(const text, caption: string;
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

class function TDialog.Confirm(const text, caption: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Confirm(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Confirm(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtConfirmation, buttons, defaultButton);
end;

class function TDialog.Confirm(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Confirm(text, SConfirm, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Confirm(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Confirm(text, SConfirm, buttons, defaultButton);
end;

class function TDialog.Info(const text: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, SInformation, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Info(const text: string;
  const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Info(text, SInformation, buttons, defaultButton);
end;

class function TDialog.Info(const text, caption: string;
  const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtInformation, buttons, defaultButton);
end;

class function TDialog.Info(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Warn(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Warn(text, SWarning, buttons, defaultButton);
end;

class function TDialog.Warn(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, SWarning, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Warn(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtWarning, buttons, defaultButton);
end;

class function TDialog.Warn(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Error(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Error(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Error(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtError, buttons, defaultButton);
end;

class function TDialog.Error(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Error(text, SError, buttons, GetDefaultButton(buttons));
end;

class function TDialog.Error(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Error(text, SError, buttons, defaultButton);
end;

end.
