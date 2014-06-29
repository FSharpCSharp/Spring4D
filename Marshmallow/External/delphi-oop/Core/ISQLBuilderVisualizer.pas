(*
* Copyright (c) 2013, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit ISQLBuilderVisualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolsAPI, StdCtrls, ExtCtrls, ActnList, StdActns, ToolWin, ImgList;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope);

  TSQLViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater, IOTAThreadNotifier)
    mmoSQL: TMemo;
    clbrMain: TCoolBar;
    tlbMain: TToolBar;
    alMain: TActionList;
    ToolButton1: TToolButton;
    ilMain: TImageList;
    EditCopy1: TEditCopy;
    EditSelectAll1: TEditSelectAll;
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FSQL: string;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure DisplaySQL(const Expression, TypeName, EvalResult: string);

    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
  end;

procedure Register;

implementation

uses
  DesignIntf, Menus, IniFiles, StrUtils, Clipbrd;

{$R *.dfm}

resourcestring
  sVisualizerName = 'ISQLBuilder Visualizer for Delphi';
  sVisualizerDescription = 'Displays SQL text of ISQLBuilder';
  sMenuText = 'Show SQL';
  sFormCaption = 'ISQLBuilder Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sOutOfScope = 'out of scope';

type

  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TISQLBuilderVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    FMyFrame: TSQLViewerFrame;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerISQLBuilderVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

{ TDebuggerDateTimeVisualizer }

function TDebuggerISQLBuilderVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDebuggerISQLBuilderVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'ISQLBuilder';
  AllDescendants := False;
end;

function TDebuggerISQLBuilderVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerISQLBuilderVisualizer.GetVisualizerDescription: string;
begin
  Result := sVisualizerDescription;
end;

function TDebuggerISQLBuilderVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerISQLBuilderVisualizer.GetVisualizerName: string;
begin
  Result := sVisualizerName;
end;

function TDebuggerISQLBuilderVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TSQLViewerFrame;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TISQLBuilderVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TSQLViewerFrame;
  AFrame.DisplaySQL(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TColorViewerFrame }

procedure TSQLViewerFrame.DisplaySQL(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  FExpression := Expression;
  try
    FSQL := Evaluate(Expression + '.ToString');

    //change returns and remove quotes
    if (Length(FSQL) > 2) then
      FSQL := Copy(FSQL, 2, Length(FSQL) -2);

    FSQL :=  StringReplace(FSQL, '''#$D', #13, [rfReplaceAll]);
    FSQL := StringReplace(FSQL, '#$A''', #10, [rfReplaceAll]);

    mmoSQL.Lines.Text := FSQL;
  except
    on E: Exception do
    begin
      mmoSQL.Lines.Text := E.Message;
    end;
  end;
  mmoSQL.Invalidate;
  Self.Invalidate;
end;

procedure TSQLViewerFrame.AfterSave;
begin

end;

procedure TSQLViewerFrame.BeforeSave;
begin

end;

procedure TSQLViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

procedure TSQLViewerFrame.Destroyed;
begin

end;

function TSQLViewerFrame.Evaluate(Expression: string): string;
const
  SIZE_RESULT = MAXWORD - 1;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..SIZE_RESULT] of Char;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
          CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
        case EvalRes of
          erOK: Result := ResultStr;
          erDeferred:
            begin
              FCompleted := False;
              FDeferredResult := '';
              FDeferredError := False;
              FNotifierIndex := CurThread.AddNotifier(Self);
              while not FCompleted do
                DebugSvcs.ProcessDebugEvents;
              CurThread.RemoveNotifier(FNotifierIndex);
              FNotifierIndex := -1;
              if not FDeferredError then
              begin
                if FDeferredResult <> '' then
                  Result := FDeferredResult
                else
                  Result := ResultStr;
              end;
            end;
          erBusy:
            begin
              DebugSvcs.ProcessDebugEvents;
              Result := Evaluate(Expression);
            end;
        end;
      end;
    end;
  end;
end;

procedure TSQLViewerFrame.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

procedure TSQLViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;

end;

procedure TSQLViewerFrame.Modified;
begin

end;

procedure TSQLViewerFrame.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TSQLViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  DisplaySQL(Expression, TypeName, EvalResult);
end;

procedure TSQLViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TSQLViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
  AForm.KeyPreview := False;
end;

procedure TSQLViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    FSQL := '';
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TSQLViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{ TColorVisualizerForm }

constructor TISQLBuilderVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TISQLBuilderVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TISQLBuilderVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TISQLBuilderVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TISQLBuilderVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame :=  TSQLViewerFrame(AFrame);
end;

function TISQLBuilderVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TISQLBuilderVisualizerForm.GetEditState: TEditState;
begin
  Result := [esCanCopy, esCanSelectAll, esCanPrint];
end;

function TISQLBuilderVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TISQLBuilderVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TISQLBuilderVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TSQLViewerFrame;
end;

function TISQLBuilderVisualizerForm.GetIdentifier: string;
begin
  Result := 'ISQLBuilderDebugVisualizer';
end;

function TISQLBuilderVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TISQLBuilderVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TISQLBuilderVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TISQLBuilderVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TISQLBuilderVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TISQLBuilderVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TISQLBuilderVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TISQLBuilderVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TSQLViewerFrame(Frame);
end;

var
  Visualizer: IOTADebuggerVisualizer;

procedure Register;
begin
  Visualizer := TDebuggerISQLBuilderVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(Visualizer);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(Visualizer);
    Visualizer := nil;
  end;
end;

initialization

finalization
  RemoveVisualizer;
end.

