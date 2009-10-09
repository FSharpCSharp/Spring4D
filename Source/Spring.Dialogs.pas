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
  TMessageBox = class
  private
    class constructor Create;
    class var fMessageDialogProc: TMessageDialogShowProc;
    class function GetDefaultButton(const buttons: TMessageDialogButtons): TMessageDialogButton; inline;
    class function ShowMessageDialog(const text, caption: string;
      dialogType: TMsgDlgType; buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult;
  public
    { Information Message Box }
    class function Info(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Info(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Info(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Info(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Warning Message Box }
    class function Warn(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Warn(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Warn(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Warn(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Error Message Box }
    class function Error(const text: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Error(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Error(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Error(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    { Confirm Message Box }
    class function Confirm(const text: string; const buttons: TMessageDialogButtons = [mbYes, mbNo]): TModalResult; overload;
    class function Confirm(const text: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
    class function Confirm(const text, caption: string; const buttons: TMessageDialogButtons = [mbOK]): TModalResult; overload;
    class function Confirm(const text, caption: string; const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult; overload;
  public
    class property MessageDialogProc: TMessageDialogShowProc read fMessageDialogProc write fMessageDialogProc;
  end;

  TDialog = TMessageBox deprecated 'Use TMessageBox instead.';

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

class constructor TMessageBox.Create;
begin
  fMessageDialogProc := ShowMessageDialog;
end;

class function TMessageBox.GetDefaultButton(
  const buttons: TMessageDialogButtons): TMessageDialogButton;
begin
  if mbOk in Buttons then
    Result := mbOk
  else if mbYes in Buttons then
    Result := mbYes
  else
    Result := mbRetry;
end;

class function TMessageBox.ShowMessageDialog(const text, caption: string;
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

class function TMessageBox.Confirm(const text, caption: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Confirm(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Confirm(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtConfirmation, buttons, defaultButton);
end;

class function TMessageBox.Confirm(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Confirm(text, SConfirm, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Confirm(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Confirm(text, SConfirm, buttons, defaultButton);
end;

class function TMessageBox.Info(const text: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, SInformation, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Info(const text: string;
  const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Info(text, SInformation, buttons, defaultButton);
end;

class function TMessageBox.Info(const text, caption: string;
  const buttons: TMessageDialogButtons; defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtInformation, buttons, defaultButton);
end;

class function TMessageBox.Info(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Warn(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Warn(text, SWarning, buttons, defaultButton);
end;

class function TMessageBox.Warn(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, SWarning, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Warn(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtWarning, buttons, defaultButton);
end;

class function TMessageBox.Warn(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text, caption: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Error(text, caption, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text, caption: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := ShowMessageDialog(text, caption, mtError, buttons, defaultButton);
end;

class function TMessageBox.Error(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Error(text, SError, buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Error(text, SError, buttons, defaultButton);
end;

end.
