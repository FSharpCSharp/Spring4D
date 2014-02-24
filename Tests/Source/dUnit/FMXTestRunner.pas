unit FMXTestRunner;

interface

uses
  System.SysUtils,
  System.Types,
  System.Variants,
  System.UITypes,
  System.Classes,
  System.Actions,
  System.Rtti,
  System.IOUtils,
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ActnList,
  FMX.Layouts,
  FMX.Memo,
  FMX.TabControl,
  FMX.ListBox,
  FMX.TreeView,
  TestFramework;

type
  TRunnerExitBehavior = (
    rxbContinue,
    rxbPause,
    rxbHaltOnFailures);

  TFMXTestRunner = class(TForm, ITestListener, ITestListenerX)
    Header: TToolBar;
    HeaderLabel: TLabel;
    ActionList1: TActionList;
    RunTests: TAction;
    StopTests: TAction;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Memo: TMemo;
    TabItem2: TTabItem;
    ToolBar1: TToolBar;
    cmdRun: TButton;
    cmdSettings: TButton;
    cmdBack: TButton;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    chkBreakOnFailures: TListBoxItem;
    chkFailIfNoChecksExecuted: TListBoxItem;
    chkFailIfMemoryLeaked: TListBoxItem;
    ChangeTab2: TChangeTabAction;
    ChangeTab1: TChangeTabAction;
    chkIngnoreLeaksInSetupTeardown: TListBoxItem;
    TestTree: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    Panel1: TPanel;
    procedure RunTestsExecute(Sender: TObject);
    procedure ListBoxItemCheckInvert(Sender: TObject);
    procedure ChangeTabUpdate(Sender: TObject);
    procedure RunTestsUpdate(Sender: TObject);
    procedure ChangeTab2Update(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    class var Suite: ITest;
  protected
    startTime: TDateTime;
    endTime: TDateTime;
    runTime: TDateTime;
    FRunning: Boolean;
    FTestResult: TTestResult;
    FErrorCount: Integer;
    FFailureCount: Integer;
    FIniName: string;

    procedure Setup;
    procedure SetupTree;
    procedure RunTheTest(aTest: ITest);
    procedure EnableUI(Enable: Boolean);
    procedure ClearResult;
    procedure ClearFailureMessage;
  public
    // implement the ITestListener interface
    procedure AddSuccess(Test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function ShouldRunTest(Test: ITest): Boolean; virtual;
    procedure StartSuite(Suite: ITest); virtual;
    procedure EndSuite(Suite: ITest); virtual;
    procedure StartTest(Test: ITest); virtual;
    procedure EndTest(Test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(Test: ITest; const Msg: string);
    procedure Warning(Test: ITest; const Msg: string);
    function Report(r: TTestResult): string;
    procedure AfterConstruction; override;

    property testResult: TTestResult read FTestResult Write FTestResult;

    class procedure RunTest(Suite: ITest;
      exitBehavior: TRunnerExitBehavior = rxbContinue); overload;
    class procedure RunRegisteredTests(
      exitBehavior : TRunnerExitBehavior = rxbContinue);
  protected
    function PrintErrors(r: TTestResult): string; virtual;
    function PrintFailures(r: TTestResult): string; virtual;
    function PrintHeader(r: TTestResult): string; virtual;
    function PrintFailureItems(r: TTestResult): string; virtual;
    function PrintErrorItems(r: TTestResult): string; virtual;
    function TruncateString(s: string; len: Integer): string; virtual;
    procedure Write(const s: string);
    procedure Writeln(const s: string = '');
    procedure FillTestTree(const RootNode: TTreeViewItem; const aTest: ITest);
  end;

  { : This type defines what the RunTest and RunRegisteredTests methods will do when
    testing has ended.
    @enum rxbContinue Just return the TestResult.
    @enum rxbPause    Pause with a ReadLn before returnng the TestResult.
    @enum rxbHaltOnFailures   Halt the program if errors or failures occurred, setting
    the program exit code to FailureCount+ErrorCount;
    behave like rxbContinue if all tests suceeded.
    @seeAlso <See Unit="TextTestRunner" Routine="RunTest">
    @seeAlso <See Unit="TextTestRunner" Routine="RunRegisteredTests">
  }

  { : Run the given Test suite }
procedure RunTest(Suite: ITest;
  exitBehavior: TRunnerExitBehavior = rxbContinue); overload;
procedure RunRegisteredTests(
  exitBehavior : TRunnerExitBehavior = rxbContinue); overload;

implementation

{$R *.fmx}

type
  TListBoxItemHelper = class helper for TListBoxItem
  private
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  public
    property Checked: Boolean read GetChecked Write SetChecked;
  end;

  TTestTreeItem = class(TTreeViewItem)
  private
    FData: TValue;
  protected
    procedure SetData(const Value: TValue); override;
    function GetData: TValue; override;
  end;

  { TFMXTestRunner }

procedure TFMXTestRunner.AddSuccess(Test: ITest);
begin
  // No display for successes
end;

procedure TFMXTestRunner.AfterConstruction;
begin
  inherited;
  FIniName := 'dUnit.ini';
{$IF Defined(ANDROID)}
  // Note that this requires Read/Write External Storage permissions
  // Write permissions option is enough, reading will be available as well
  //
  // Use shared path since private one will be cleared upon application
  // reinstallation - oops this one as well
  // FIniName := TPath.Combine(TPath.GetSharedDocumentsPath, FIniName);
  // Use the global SDCard path
  FIniName := TPath.Combine('/storage/emulated/0', FIniName);
{$ELSE IF Defined(IOS)}
  FIniName := TPath.Combine(TPath.GetDocumentsPath, FIniName);
{$ENDIF}
end;

procedure TFMXTestRunner.ChangeTab2Update(Sender: TObject);
var
  Action: TChangeTabAction;
begin
  Action := Sender as TChangeTabAction;
  Action.Enabled := Action.Supported and (not FRunning) and
    Assigned(Action.Tab) and
  // Assigned(Action.Tab.TabControl) and
    (TabControl1.TabIndex <> Action.Tab.Index);
end;

procedure TFMXTestRunner.ChangeTabUpdate(Sender: TObject);
var
  Action: TChangeTabAction;
begin
  Action := Sender as TChangeTabAction;
  Action.Enabled := Action.Supported and (not FRunning) and
    Assigned(Action.Tab) and
    { Assigned(Action.Tab.TabControl) and }
    (TabControl1.TabIndex <> Action.Tab.Index);
  Action.Visible := Action.Enabled;
end;

procedure TFMXTestRunner.ClearFailureMessage;
begin
  Memo.Lines.Clear;
end;

procedure TFMXTestRunner.ClearResult;
begin
  if FTestResult <> nil then
  begin
    FTestResult.Free;
    FTestResult := nil;
  end;
  ClearFailureMessage;
end;

procedure TFMXTestRunner.AddError(error: TTestFailure);
begin
  Self.Write('E');
end;

procedure TFMXTestRunner.AddFailure(failure: TTestFailure);
begin
  Self.Write('F');
end;

{ : Prints failures to the standard output }
function TFMXTestRunner.Report(r: TTestResult): string;
begin
  Result := PrintHeader(r) + PrintErrors(r) + PrintFailures(r);
end;

{ : Prints the errors to the standard output }
function TFMXTestRunner.PrintErrors(r: TTestResult): string;
begin
  Result := '';
  if (r.errorCount <> 0) then
  begin
    if (r.errorCount = 1) then
        Result := Result + Format('There was %d error:', [r.errorCount]) +
        sLineBreak
    else Result := Result + Format('There were %d errors:', [r.errorCount]) +
        sLineBreak;

    Result := Result + PrintErrorItems(r);
    Result := Result + sLineBreak
  end
end;

function TFMXTestRunner.PrintFailureItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  Result := '';
  for i := 0 to r.FailureCount - 1 do
  begin
    failure := r.Failures[i];
    Result := Result + Format('%3d) %s: %s' + sLineBreak + '     at %s' +
      sLineBreak + '      "%s"', [i + 1, failure.failedTest.name,
      failure.thrownExceptionName, failure.LocationInfo,
      failure.thrownExceptionMessage]) + sLineBreak;
  end;
end;

function TFMXTestRunner.PrintErrorItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  Result := '';
  for i := 0 to r.errorCount - 1 do
  begin
    failure := r.Errors[i];
    Result := Result + Format('%3d) %s: %s' + sLineBreak + '     at %s' +
      sLineBreak + '      "%s"', [i + 1, failure.failedTest.name,
      failure.thrownExceptionName, failure.LocationInfo,
      failure.thrownExceptionMessage]) + sLineBreak;
  end;
end;

{ : Prints failures to the standard output }
function TFMXTestRunner.PrintFailures(r: TTestResult): string;
begin
  Result := '';
  if (r.FailureCount <> 0) then
  begin
    if (r.FailureCount = 1) then
        Result := Result + Format('There was %d failure:', [r.FailureCount]) +
        sLineBreak
    else Result := Result + Format('There were %d failures:', [r.FailureCount])
        + sLineBreak;

    Result := Result + PrintFailureItems(r);
    Result := Result + sLineBreak
  end
end;

{ : Prints the ClassName of the Report }
function TFMXTestRunner.PrintHeader(r: TTestResult): string;
begin
  Result := '';
  if r.wasSuccessful then
  begin
    Result := Result + sLineBreak;
    Result := Result + Format('OK: %d tests' + sLineBreak, [r.runCount]);
  end
  else
  begin
    Result := Result + sLineBreak;
    Result := Result + 'FAILURES!!!' + sLineBreak;
    Result := Result + 'Test Results:' + sLineBreak;
    Result := Result + Format('Run:      %8d' + sLineBreak + 'Failures: %8d' +
      sLineBreak + 'Errors:   %8d' + sLineBreak, [r.runCount, r.FailureCount,
      r.errorCount]);
  end
end;

procedure TFMXTestRunner.StartTest(Test: ITest);
begin
  Self.Write('.');
  Application.ProcessMessages;
end;

procedure TFMXTestRunner.EndTest(Test: ITest);
begin
  Application.ProcessMessages;
end;

procedure TFMXTestRunner.FillTestTree(const RootNode: TTreeViewItem;
  const aTest: ITest);
var
  TestTests: IInterfaceList;
  i: Integer;
  NewNode: TTreeViewItem;
  NewTest: ITest;
begin
  if aTest = nil then Exit;

  NewNode := TTestTreeItem.Create(Self);
  if (RootNode = nil) then
  begin
    NewNode.Parent := TestTree;
  end
  else
  begin
    NewNode.Parent := RootNode;
  end;
  NewNode.data := TValue.From(aTest);
  NewNode.Text := aTest.name;
  NewNode.IsChecked := aTest.Enabled;
  aTest.GUIObject := NewNode;

  TestTests := aTest.Tests;
  if (TestTests.Count > 0) then
  begin
    if (RootNode <> nil) then RootNode.Expand;
    for i := 0 to TestTests.Count - 1 do
    begin
      NewTest := TestTests[i] as ITest;
      FillTestTree(NewNode, NewTest);
    end;
  end
  else if (RootNode <> nil) then RootNode.Collapse;
  if (not aTest.Enabled) then NewNode.Collapse;
end;

procedure TFMXTestRunner.FormShow(Sender: TObject);
begin
  SetupTree;
end;

procedure TFMXTestRunner.ListBoxItemCheckInvert(Sender: TObject);
var
  Item: TListBoxItem;
begin
  Item := (Sender as TListBoxItem);
  if (Item.Enabled) then Item.Checked := not Item.Checked;
end;

function TFMXTestRunner.TruncateString(s: string; len: Integer): string;
begin
  if Length(s) > len then Result := copy(s, 1, len) + '...'
  else Result := s
end;

procedure TFMXTestRunner.TestingStarts;
begin
  Self.Writeln;
  Self.Writeln('DUnit / Testing ');
  startTime := now;
end;

procedure TFMXTestRunner.TestingEnds(testResult: TTestResult);
var
  h, m, s, l: Word;
begin
  endTime := now;
  runTime := endTime - startTime;
  Self.Writeln;
  DecodeTime(runTime, h, m, s, l);
  Self.Writeln(Format('Time: %d:%2.2d:%2.2d.%d', [h, m, s, l]));
  Self.Writeln(Report(testResult));
  Self.Writeln;
end;

class procedure TFMXTestRunner.RunTest(Suite: ITest;
  exitBehavior: TRunnerExitBehavior = rxbContinue);
var
  Form: TFMXTestRunner;
begin
  Application.Initialize;
  Application.CreateForm(TFMXTestRunner, Form);
  TFMXTestRunner.Suite := Suite;
  Application.Run;
  TFMXTestRunner.Suite := nil;
end;

procedure TFMXTestRunner.RunTestsExecute(Sender: TObject);
begin
  if Suite = nil then Exit;

  Setup;
  try
    Suite.SaveConfiguration(FIniName, False, True);
  except
    // Consume the exception
    // Android: Write External Storage permission is not set
    on EFileStreamError do ;
    else raise;
  end;
  RunTheTest(Suite);
end;

procedure TFMXTestRunner.RunTestsUpdate(Sender: TObject);
begin
  RunTests.Enabled := not FRunning;
  RunTests.Visible := TabControl1.TabIndex = 0;
end;

procedure TFMXTestRunner.RunTheTest(aTest: ITest);
begin
  if aTest = nil then Exit;
  if FRunning then
  begin
    // warning: we're reentering this method if FRunning is true
    assert(FTestResult <> nil);
    FTestResult.Stop;
    Exit;
  end;

  FRunning := True;
  try
    StopTests.Enabled := True;

    // CopyMessageToClipboardAction.Enabled := False;

    EnableUI(False);
    // AutoSaveConfiguration;
    ClearResult;
    testResult := TTestResult.Create;
    try
      testResult.addListener(Self);
      testResult.BreakOnFailures := chkBreakOnFailures.Checked;
      testResult.FailsIfNoChecksExecuted := chkFailIfNoChecksExecuted.Checked;
      testResult.FailsIfMemoryLeaked := chkFailIfMemoryLeaked.Checked;
      testResult.IgnoresMemoryLeakInSetUpTearDown :=
        chkIngnoreLeaksInSetupTeardown.Checked;
      aTest.Run(testResult);
    finally
      FErrorCount := testResult.errorCount;
      FFailureCount := testResult.FailureCount;
{$IFNDEF NEXTGEN}
      testResult.Free;
{$ENDIF}
      testResult := nil;
    end;
  finally
    FRunning := False;
    EnableUI(True);
  end;
end;

class procedure TFMXTestRunner.RunRegisteredTests(
  exitBehavior : TRunnerExitBehavior = rxbContinue);
begin
  RunTest(registeredTests, exitBehavior);
end;

procedure RunTest(Suite: ITest;
  exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  TFMXTestRunner.RunTest(Suite, exitBehavior);
end;

procedure RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  TFMXTestRunner.RunRegisteredTests(exitBehavior);
end;

procedure TFMXTestRunner.Status(Test: ITest; const Msg: string);
begin
  Self.Writeln(Format('%s: %s', [Test.name, Msg]));
end;

procedure TFMXTestRunner.Warning(Test: ITest; const Msg: string);
begin
  Self.Writeln(Format('%s: %s', [Test.name, Msg]));
end;

procedure TFMXTestRunner.Write(const s: string);
begin
  if (Memo.Lines.Count = 0) then Writeln(s)
  else Memo.Lines[Memo.Lines.Count - 1] := Memo.Lines[Memo.Lines.Count - 1] + s;
end;

procedure TFMXTestRunner.Writeln(const s: string);
begin
  Memo.Lines.Add(s);
end;

procedure TFMXTestRunner.Setup;
  procedure TraverseItems(const Item: TTreeViewItem);
  var
    Test: ITest;
    i: Integer;
  begin
    Test := Item.data.AsInterface as ITest;
    Test.Enabled := Item.IsChecked;
    for i := 0 to Item.Count - 1 do TraverseItems(Item[i]);
  end;
begin
  TraverseItems(TestTree.Items[0]);
end;

procedure TFMXTestRunner.SetupTree;
begin
  if (Suite = nil) then Exit;

  TestTree.BeginUpdate;
  TestTree.Clear;
  try
    Suite.LoadConfiguration(FIniName, False, True);
  except
    // Consume the exception
    // Android: Read External Storage permission is not set
    on EFileStreamError do ;
    else raise;
  end;
  FillTestTree(nil, Suite);
  TestTree.EndUpdate;
end;

function TFMXTestRunner.ShouldRunTest(Test: ITest): Boolean;
begin
  Result := Test.Enabled;
end;

procedure TFMXTestRunner.EnableUI(Enable: Boolean);
begin

end;

procedure TFMXTestRunner.EndSuite(Suite: ITest);
begin
end;

procedure TFMXTestRunner.StartSuite(Suite: ITest);
begin

end;

{ TListBoxItemHelper }

function TListBoxItemHelper.GetChecked: Boolean;
begin
  Result := Self.ItemData.Accessory = TListBoxItemData.TAccessory.aCheckmark;
end;

procedure TListBoxItemHelper.SetChecked(const Value: Boolean);
begin
  if (Value) then
      Self.ItemData.Accessory := TListBoxItemData.TAccessory.aCheckmark
  else Self.ItemData.Accessory := TListBoxItemData.TAccessory.aNone;
end;

{ TTestTreeItem }

function TTestTreeItem.GetData: TValue;
begin
  Result := FData;
end;

procedure TTestTreeItem.SetData(const Value: TValue);
begin
  FData := Value;
end;

end.
