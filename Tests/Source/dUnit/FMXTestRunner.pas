unit FMXTestRunner;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, TestFramework,
  System.Actions, FMX.ActnList, FMX.Layouts, FMX.Memo, FMX.TabControl,
  FMX.ListBox, FMX.TreeView, System.Rtti;

type
  TRunnerExitBehavior = (
    rxbContinue,
    rxbPause,
    rxbHaltOnFailures
    );

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
  protected class var
	Suite : ITest;
  protected
	startTime: TDateTime;
	endTime: TDateTime;
	runTime: TDateTime;
	FRunning : Boolean;
	FTestResult : TTestResult;
	FErrorCount:    Integer;
    FFailureCount:  Integer;

	procedure Setup;
	procedure SetupTree;
	procedure RunTheTest(aTest : ITest);
	procedure EnableUI(enable :Boolean);
	procedure ClearResult;
	procedure ClearFailureMessage;
  public
    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function  ShouldRunTest(test :ITest):boolean; virtual;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
	procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(test :ITest; const Msg :string);
	procedure Warning(test :ITest; const Msg :string);
	function  Report(r: TTestResult): string;

	property TestResult : TTestResult read FTestResult write FTestResult;

	class procedure RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue); overload;
	class procedure RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue);
  protected
    function  PrintErrors(r: TTestResult): string; virtual;
    function  PrintFailures(r: TTestResult): string; virtual;
    function  PrintHeader(r: TTestResult): string; virtual;
    function  PrintFailureItems(r :TTestResult): string; virtual;
	function  PrintErrorItems(r :TTestResult): string; virtual;
	function  TruncateString(s: string; len: integer): string; virtual;
	procedure write(const s : string);
	procedure writeln(const s : string = '');
	procedure FillTestTree(const RootNode: TTreeViewItem; const ATest: ITest);
  end;

  {: This type defines what the RunTest and RunRegisteredTests methods will do when
     testing has ended.
     @enum rxbContinue Just return the TestResult.
     @enum rxbPause    Pause with a ReadLn before returnng the TestResult.
     @enum rxbHaltOnFailures   Halt the program if errors or failures occurred, setting
                               the program exit code to FailureCount+ErrorCount;
                               behave like rxbContinue if all tests suceeded.
     @seeAlso <See Unit="TextTestRunner" Routine="RunTest">
	 @seeAlso <See Unit="TextTestRunner" Routine="RunRegisteredTests">
     }

{: Run the given test suite
}
procedure RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue); overload;
procedure RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue); overload;

implementation

{$R *.fmx}

type
  TListBoxItemHelper = class helper for TListBoxItem
  private
    function GetChecked: Boolean;
	procedure SetChecked(const Value: Boolean);
  public
	property Checked : Boolean read GetChecked write SetChecked;
  end;

  TTestTreeItem = class(TTreeViewItem)
  private
	FData	: TValue;
  protected
	procedure SetData(const Value: TValue); override;
	function GetData: TValue; override;
  end;

{ TFMXTestRunner }

procedure TFMXTestRunner.AddSuccess(test: ITest);
begin
// No display for successes
end;

procedure TFMXTestRunner.ChangeTab2Update(Sender: TObject);
var Action 	: TChangeTabAction;
begin
	Action:=Sender as TChangeTabAction;
	Action.Enabled:=Action.Supported and (not FRunning) and
			   Assigned(Action.Tab) and
			   //Assigned(Action.Tab.TabControl) and
			   (TabControl1.TabIndex <> Action.Tab.Index);
end;

procedure TFMXTestRunner.ChangeTabUpdate(Sender: TObject);
var Action 	: TChangeTabAction;
begin
	Action:=Sender as TChangeTabAction;
	Action.Enabled:=Action.Supported and (not FRunning) and
			   Assigned(Action.Tab) and
			   //Assigned(Action.Tab.TabControl) and
			   (TabControl1.TabIndex <> Action.Tab.Index);
	Action.Visible:=Action.Enabled;
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
  Self.write('E');
end;

procedure TFMXTestRunner.AddFailure(failure: TTestFailure);
begin
  Self.write('F');
end;

{:
   Prints failures to the standard output
 }
function TFMXTestRunner.Report(r: TTestResult): string;
begin
  result := PrintHeader(r) +
            PrintErrors(r) +
            PrintFailures(r);
end;

{:
   Prints the errors to the standard output
 }
function TFMXTestRunner.PrintErrors(r: TTestResult): string;
begin
  result := '';
  if (r.errorCount <> 0) then begin
    if (r.errorCount = 1) then
	  result := result + format('There was %d error:', [r.errorCount]) + sLineBreak
	else
      result := result + format('There were %d errors:', [r.errorCount]) + sLineBreak;

    result := result + PrintErrorItems(r);
    result := result + sLineBreak
  end
end;

function TFMXTestRunner.PrintFailureItems(r :TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.FailureCount-1 do begin
    failure := r.Failures[i];
	result := result + format('%3d) %s: %s'+sLineBreak+'     at %s'+sLineBreak+'      "%s"',
                               [
                               i+1,
                               failure.failedTest.name,
                               failure.thrownExceptionName,
                               failure.LocationInfo,
                               failure.thrownExceptionMessage
                               ]) + sLineBreak;
  end;
end;

function TFMXTestRunner.PrintErrorItems(r :TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.ErrorCount-1 do begin
    failure := r.Errors[i];
	result := result + format('%3d) %s: %s'+sLineBreak+'     at %s'+sLineBreak+'      "%s"',
                               [
                               i+1,
                               failure.failedTest.name,
                               failure.thrownExceptionName,
                               failure.LocationInfo,
                               failure.thrownExceptionMessage
                               ]) + sLineBreak;
  end;
end;

{:
   Prints failures to the standard output
 }
function TFMXTestRunner.PrintFailures(r: TTestResult): string;
begin
  result := '';
  if (r.failureCount <> 0) then begin
    if (r.failureCount = 1) then
      result := result + format('There was %d failure:', [r.failureCount]) + sLineBreak
    else
      result := result + format('There were %d failures:', [r.failureCount]) + sLineBreak;

    result := result + PrintFailureItems(r);
    result := result + sLineBreak
  end
end;

{:
   Prints the ClassName of the Report
 }
function TFMXTestRunner.PrintHeader(r: TTestResult): string;
begin
  result := '';
  if r.wasSuccessful then
  begin
	result := result + sLineBreak;
	result := result + format('OK: %d tests'+sLineBreak, [r.runCount]);
  end
  else
  begin
    result := result + sLineBreak;
	result := result + 'FAILURES!!!'+sLineBreak;
	result := result + 'Test Results:'+sLineBreak;
	result := result + format('Run:      %8d'+sLineBreak+'Failures: %8d'+sLineBreak+'Errors:   %8d'+sLineBreak,
                      [r.runCount, r.failureCount, r.errorCount]
                      );
  end
end;

procedure TFMXTestRunner.StartTest(test: ITest);
begin
  Self.write('.');
  Application.ProcessMessages;
end;

procedure TFMXTestRunner.EndTest(test: ITest);
begin
  Application.ProcessMessages;
end;

procedure TFMXTestRunner.FillTestTree(const RootNode: TTreeViewItem;
  const ATest: ITest);
var
  TestTests: IInterfaceList;
  i: Integer;
  NewNode: TTreeViewItem;
begin
  if ATest = nil then
    Exit;

  NewNode := TTestTreeItem.Create(Self);
  if (RootNode = nil) then NewNode.Parent:=TestTree
  else NewNode.Parent:=RootNode;
  NewNode.data := TValue.From(ATest);
  NewNode.Text := ATest.Name;
  NewNode.IsChecked := ATest.Enabled;

  TestTests := ATest.Tests;
  for i := 0 to TestTests.count - 1 do
  begin
    FillTestTree(NewNode, TestTests[i] as ITest);
  end;
end;

procedure TFMXTestRunner.FormShow(Sender: TObject);
begin
	SetupTree;
end;

procedure TFMXTestRunner.ListBoxItemCheckInvert(Sender: TObject);
var Item	: TListBoxItem;
begin
	Item:=(Sender as TListBoxItem);
	if (Item.Enabled) then Item.Checked:=not Item.Checked;
end;

function TFMXTestRunner.TruncateString(s: string; len: integer): string;
begin
  if Length(s) > len then
    result := copy(s, 1, len) + '...'
  else
    result := s
end;

procedure TFMXTestRunner.TestingStarts;
begin
  Self.writeln;
  Self.writeln('DUnit / Testing ');
  startTime := now;
end;

procedure TFMXTestRunner.TestingEnds(testResult: TTestResult);
var
  h, m, s, l :Word;
begin
  endTime := now;
  runTime := endTime-startTime;
  Self.writeln;
  DecodeTime(runTime, h,  m, s, l);
  Self.writeln(Format('Time: %d:%2.2d:%2.2d.%d', [h, m, s, l]));
  Self.writeln(Report(testResult));
  Self.writeln;
end;

class procedure TFMXTestRunner.RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue);
var Form	: TFMXTestRunner;
begin
	Application.Initialize;
	Application.CreateForm(TFMXTestRunner, Form);
	TFMXTestRunner.Suite:=suite;
	Application.Run;
	TFMXTestRunner.Suite:=nil;
end;

procedure TFMXTestRunner.RunTestsExecute(Sender: TObject);
begin
  if Suite = nil then
	Exit;

  Setup;
  RunTheTest(Suite);
end;

procedure TFMXTestRunner.RunTestsUpdate(Sender: TObject);
begin
	RunTests.Enabled:=not FRunning;
	RunTests.Visible:=TabControl1.TabIndex = 0;
end;

procedure TFMXTestRunner.RunTheTest(aTest: ITest);
begin
  if aTest = nil then
    EXIT;
  if FRunning then
  begin
    // warning: we're reentering this method if FRunning is true
    assert(FTestResult <> nil);
    FTestResult.Stop;
    EXIT;
  end;

  FRunning := true;
  try
    StopTests.Enabled := True;

    //CopyMessageToClipboardAction.Enabled := false;

	EnableUI(false);
    //AutoSaveConfiguration;
    ClearResult;
    TestResult := TTestResult.create;
    try
      TestResult.addListener(self);
	  TestResult.BreakOnFailures := chkBreakOnFailures.Checked;
	  TestResult.FailsIfNoChecksExecuted := chkFailIfNoChecksExecuted.Checked;
	  TestResult.FailsIfMemoryLeaked := chkFailIfMemoryLeaked.Checked;
	  TestResult.IgnoresMemoryLeakInSetUpTearDown :=
		chkIngnoreLeaksInSetupTeardown.Checked;
	  aTest.run(TestResult);
	finally
      FErrorCount := TestResult.ErrorCount;
	  FFailureCount := TestResult.FailureCount;
{$IFNDEF NEXTGEN}
	  TestResult.Free;
{$ENDIF}
      TestResult := nil;
    end;
  finally
      FRunning := false;
	  EnableUI(true);
  end;
end;

class procedure TFMXTestRunner.RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  RunTest(registeredTests, exitBehavior);
end;

procedure RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  TFMXTestRunner.RunTest(suite, exitBehavior);
end;

procedure RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
   TFMXTestRunner.RunRegisteredTests(exitBehavior);
end;


procedure TFMXTestRunner.Status(test: ITest; const Msg: string);
begin
  Self.writeln(Format('%s: %s', [test.Name, Msg]));
end;

procedure TFMXTestRunner.Warning(test: ITest; const Msg: string);
begin
  Self.writeln(Format('%s: %s', [test.Name, Msg]));
end;

procedure TFMXTestRunner.write(const s: string);
begin
	if (Memo.Lines.Count = 0) then writeln(s)
	else Memo.Lines[Memo.Lines.Count - 1]:=Memo.Lines[Memo.Lines.Count - 1] + s;
end;

procedure TFMXTestRunner.writeln(const s: string);
begin
	Memo.Lines.Add(s);
end;

procedure TFMXTestRunner.Setup;
//**********
procedure TraverseItems(const Item : TTreeViewItem);
var Test	: ITest;
	i		: Integer;
begin
	Test:=Item.Data.AsInterface as ITest;
	Test.Enabled:=Item.IsChecked;
	for i:=0 to Item.Count - 1 do TraverseItems(Item[i]);
end;
//**********
begin
	TraverseItems(TestTree.Items[0]);
end;

procedure TFMXTestRunner.SetupTree;
begin
	TestTree.Clear;
	FillTestTree(nil, Suite);
	TestTree.ExpandAll;
end;

function TFMXTestRunner.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
end;

procedure TFMXTestRunner.EnableUI(enable: Boolean);
begin

end;

procedure TFMXTestRunner.EndSuite(suite: ITest);
begin
end;

procedure TFMXTestRunner.StartSuite(suite: ITest);
begin

end;

{ TListBoxItemHelper }

function TListBoxItemHelper.GetChecked: Boolean;
begin
	Result:=Self.ItemData.Accessory = TListBoxItemData.TAccessory.aCheckmark;
end;

procedure TListBoxItemHelper.SetChecked(const Value: Boolean);
begin
	if (Value) then Self.ItemData.Accessory:=TListBoxItemData.TAccessory.aCheckmark
	else Self.ItemData.Accessory:=TListBoxItemData.TAccessory.aNone;
end;

{ TTestTreeItem }

function TTestTreeItem.GetData: TValue;
begin
	Result:=FData;
end;

procedure TTestTreeItem.SetData(const Value: TValue);
begin
	FData:=Value;
end;

end.
