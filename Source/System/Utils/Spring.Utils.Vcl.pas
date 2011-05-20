{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2011 DevJET                                  }
{                                                                           }
{           http://www.DevJET.net                                           }
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

unit Spring.Utils.Vcl;

{$I Spring.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,
  Controls,
  Dialogs,
  Forms,
  DB,
  Spring;

type
  {$REGION 'TMessageBox'}

  TMessageDialogButton  = Dialogs.TMsgDlgBtn;
  TMessageDialogButtons = Dialogs.TMsgDlgButtons;

  TMessageDialogShowProc = reference to function(const text, caption: string;
    dialogType: TMsgDlgType; buttons: TMessageDialogButtons;
    defaultButton: TMessageDialogButton): TModalResult;

  /// <summary>
  /// Provides static methods to show common message dialogs.
  /// </summary>
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

  TMsgBox = TMessageBox;

  TDialog = TMessageBox deprecated 'Use TMessageBox/TMsgBox instead.';

  {$ENDREGION}

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

  /// <summary>
  /// Try setting focus to a control.
  /// </summary>
  function TrySetFocus(control: TWinControl): Boolean;

  function TryFocusControl(control: TWinControl): Boolean;
    deprecated 'Use TrySetFocus instead.';

  /// <summary>
  /// Set screen cursor as follow.
  /// </summary>
  procedure SetMouseCursor(const cursor: TCursor);

  /// <summary>
  /// Enumerates all child components, recursively.
  /// </summary>
  /// <param name="callback">Returning false will stop the enumeration.</param>
  procedure EnumerateComponents(owner: TComponent; callback: TFunc<TComponent, Boolean>);

  /// <summary>
  /// Walkthrough all child controls in tab-order, recursively.
  /// </summary>
  /// <param name="callback">Returning false will stop the enumeration.</param>
  procedure EnumerateControls(parentControl: TWinControl; callback: TFunc<TWinControl, Boolean>);

  /// <summary>
  /// Walkthrough all dataset records from the first one.
  /// </summary>
  /// <param name="callback">Returning false will stop the enumeration.</param>
  procedure EnumerateDataSet(dataSet: TDataSet; callback: TFunc<Boolean>);

implementation

function TrySetFocus(control: TWinControl): Boolean;
begin
  TArgument.CheckNotNull(control, 'control');

  Result := control.Showing and control.CanFocus;
  if Result then
  begin
    control.SetFocus;
  end;
end;

function TryFocusControl(control: TWinControl): Boolean;
begin
  Result := TrySetFocus(control);
end;

procedure SetMouseCursor(const cursor: TCursor);
begin
  TArgument.CheckNotNull(cursor, 'cursor');

  Screen.Cursor := cursor;
end;

procedure EnumerateComponents(owner: TComponent; callback: TFunc<TComponent, Boolean>);
var
  component: TComponent;
begin
  TArgument.CheckNotNull(owner, 'owner');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  for component in owner do
  begin
    if not callback(component) then
    begin
      Exit;
    end;
    if component.ComponentCount > 0 then
    begin
      EnumerateComponents(component, callback);
    end;
  end;
end;

procedure EnumerateControls(parentControl: TWinControl; callback: TFunc<TWinControl, Boolean>);
var
  list: TList;
  i: Integer;
begin
  TArgument.CheckNotNull(parentControl, 'parentControl');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  list := TList.Create;
  try
    parentControl.GetTabOrderList(list);
    for i := 0 to list.Count - 1 do
    begin
      if not callback(TWinControl(list[i])) then
      begin
        Exit;
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure EnumerateDataSet(dataSet: TDataSet; callback: TFunc<Boolean>);
begin
  TArgument.CheckNotNull(dataSet, 'dataSet');
  TArgument.CheckNotNull(Assigned(callback), 'callback');

  dataSet.DisableControls;
  try
    dataSet.First;
    while not dataSet.Eof and callback do
    begin
      dataSet.Next;
    end;
  finally
    dataSet.EnableControls;
  end;
end;


{$REGION 'TMessageBox'}

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
  Result := Confirm(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Confirm(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Confirm(text, '', buttons, defaultButton);
end;

class function TMessageBox.Info(const text: string;
  const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Info(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Info(const text: string;
  const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Info(text, '', buttons, defaultButton);
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
  Result := Warn(text, '', buttons, defaultButton);
end;

class function TMessageBox.Warn(const text: string; const buttons: TMessageDialogButtons): TModalResult;
begin
  Result := Warn(text, '', buttons, GetDefaultButton(buttons));
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
  Result := Error(text, '', buttons, GetDefaultButton(buttons));
end;

class function TMessageBox.Error(const text: string; const buttons: TMessageDialogButtons;
  defaultButton: TMessageDialogButton): TModalResult;
begin
  Result := Error(text, '', buttons, defaultButton);
end;

{$ENDREGION}

end.



