unit Log4DDemo1;

{
  Demonstrate the Log4D package.

  Written by Keith Wood (kbwood@iprimus.com.au)
}

interface

{$I Defines.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Log4D, Log4DXML;

type
  { A custom appender to write to a named memo component. }
  TMemoAppender = class(TLogCustomAppender)
  private
    FMemo: TMemo;
  protected
    procedure DoAppend(const Message: string); override;
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const Name: string; const Memo: TMemo); reintroduce;
  end;

  { A custom renderer for components. }
  TComponentRenderer = class(TLogCustomRenderer)
  public
    function Render(const Message: TObject): string; override;
  end;

  { The demonstration form.
    Allows you to set logging levels, filter value, NDC, and threshold,
    then generate logging events at will. }
  TfrmLog4DDemo = class(TForm)
    pnlControls: TPanel;
      Label1: TLabel;
      cmbLevel: TComboBox;
      Label2: TLabel;
      cmbLogger: TComboBox;
      Label3: TLabel;
      edtMessage: TEdit;
      btnLog: TButton;
      btnLoop: TButton;
      grpFilter: TGroupBox;
        edtFilter: TEdit;
      grpNDC: TGroupBox;
        edtNDC: TEdit;
        btnPush: TButton;
        lblNDC: TLabel;
        btnPop: TButton;
      grpThreshold: TGroupBox;
        cmbThreshold: TComboBox;
    pnlLeft: TPanel;
      pnlMyapp: TPanel;
        chkMyappAdditive: TCheckBox;
        cmbMyappLevel: TComboBox;
      memMyApp: TMemo;
      splLeft: TSplitter;
      pnlMyappMore: TPanel;
        chkMyappMoreAdditive: TCheckBox;
        cmbMyappMoreLevel: TComboBox;
      memMyAppMore: TMemo;
    splVert: TSplitter;
    pnlRight: TPanel;
      pnlMyappOther: TPanel;
        chkMyappOtherAdditive: TCheckBox;
        cmbMyappOtherLevel: TComboBox;
      memMyAppOther: TMemo;
      splRight: TSplitter;
      pnlAlt: TPanel;
        chkAltAdditive: TCheckBox;
        cmbAltLevel: TComboBox;
      memAlt: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure btnPushClick(Sender: TObject);
    procedure btnPopClick(Sender: TObject);
    procedure btnLoopClick(Sender: TObject);
    procedure cmbLoggerLevelChange(Sender: TObject);
    procedure cmbThresholdChange(Sender: TObject);
    procedure chkAdditiveChange(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure splLeftMoved(Sender: TObject);
    procedure splRightMoved(Sender: TObject);
  private
    FFilter: ILogFilter;
    FLogs: array [0..3] of TLogLogger;
  public
  end;

var
  frmLog4DDemo: TfrmLog4DDemo;

implementation

{$R *.DFM}

{ TMemoAppender ---------------------------------------------------------------}

{ Initialisation - attach to memo. }
constructor TMemoAppender.Create(const Name: string; const Memo: TMemo);
begin
  inherited Create(Name);
  FMemo := Memo;
end;

{ Add the message to the memo. }
procedure TMemoAppender.DoAppend(const Message: string);
var
  Msg: string;
begin
  if Assigned(FMemo) and Assigned(FMemo.Parent) then
  begin
    if Copy(Message, Length(Message) - 1, 2) = #13#10 then
      Msg := Copy(Message, 1, Length(Message) - 2)
    else
      Msg := Message;
    FMemo.Lines.Add(Msg);
  end;
end;

{ Find the named memo and attach to it (if the param is 'memo'). }
procedure TMemoAppender.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if (Name = 'memo') and (Value <> '') then
  begin
    FMemo := frmLog4DDemo.FindComponent(Value) as TMemo;
    WriteHeader;
  end;
end;

{ TComponentRenderer ----------------------------------------------------------}

{ Display a component as its name and type. }
function TComponentRenderer.Render(const Message: TObject): string;
var
  Comp: TComponent;
begin
  if not (Message is TComponent) then
    Result := 'Object must be a TComponent'
  else
  begin
    Comp := Message as TComponent;
    if Comp is TControl then
      // Add position and size info
      with TControl(Comp) do
        Result := Format('%s: %s [%d x %d at %d, %d]',
          [Name, ClassName, Width, Height, Left, Top])
    else
      Result := Format('%s: %s', [Comp.Name, Comp.ClassName]);
  end;
end;

{ TfrmLog4DDemo ---------------------------------------------------------------}

{ Initialisation. }
procedure TfrmLog4DDemo.FormCreate(Sender: TObject);
const
  IsAdditive: array [Boolean] of string = ('not ', '');
  Header = ' %s (%sadditive) - %s';
begin
  // Initialise from stored configuration - select between INI style file
  // or XML document by uncommenting one of the following two lines
  TLogPropertyConfigurator.Configure('log4d.props');
//  TLogXMLConfigurator.Configure('log4d.xml');
  // Create loggers for logging - both forms are equivalent
  FLogs[0] := DefaultHierarchy.GetLogger('myapp');
  FLogs[1] := TLogLogger.GetLogger('myapp.more');
  FLogs[2] := DefaultHierarchy.GetLogger('myapp.other');
  FLogs[3] := TLogLogger.GetLogger('alt');
  // Show their state on the form
  pnlMyApp.Caption      := '  ' + FLogs[0].Name;
  pnlMyAppMore.Caption  := '  ' + FLogs[1].Name;
  pnlMyAppOther.Caption := '  ' + FLogs[2].Name;
  pnlAlt.Caption        := '  ' + FLogs[3].Name;
  // Attach to the filter on the first logger
  FFilter        := ILogFilter(ILogAppender(FLogs[0].Appenders[0]).Filters[0]);
  edtFilter.Text := FFilter.Options['match'];
  // Load levels into a combobox
  cmbLevel.Items.AddObject(Fatal.Name, Fatal);
  cmbLevel.Items.AddObject(Error.Name, Error);
  cmbLevel.Items.AddObject(Warn.Name,  Warn);
  cmbLevel.Items.AddObject(Info.Name,  Info);
  cmbLevel.Items.AddObject(Debug.Name, Debug);
  cmbLevel.ItemIndex  := 0;
  // Set threshold level
  cmbThreshold.Items.Assign(cmbLevel.Items);
  cmbThreshold.Items.InsertObject(0, Off.Name, Off);
  cmbThreshold.Items.AddObject(All.Name, All);
  cmbThreshold.ItemIndex :=
    cmbThreshold.Items.IndexOfObject(FLogs[0].Hierarchy.Threshold);
  // Set levels and additivity per logger
  cmbMyappLevel.Items.Assign(cmbThreshold.Items);
  cmbMyappLevel.ItemIndex  := cmbMyAppLevel.Items.IndexOf(FLogs[0].Level.Name);
  chkMyappAdditive.Checked := Flogs[0].Additive;
  cmbMyappMoreLevel.Items.Assign(cmbThreshold.Items);
  cmbMyappMoreLevel.ItemIndex  :=
    cmbMyAppMoreLevel.Items.IndexOf(FLogs[1].Level.Name);
  chkMyappMoreAdditive.Checked := Flogs[1].Additive;
  cmbMyappOtherLevel.Items.Assign(cmbThreshold.Items);
  cmbMyappOtherLevel.ItemIndex  :=
    cmbMyAppOtherLevel.Items.IndexOf(FLogs[2].Level.Name);
  chkMyappOtherAdditive.Checked := Flogs[2].Additive;
  cmbAltLevel.Items.Assign(cmbThreshold.Items);
  cmbAltLevel.ItemIndex  :=
    cmbAltLevel.Items.IndexOf(FLogs[3].Level.Name);
  chkAltAdditive.Checked := Flogs[3].Additive;
end;

{ Log an event based on user selections. }
procedure TfrmLog4DDemo.btnLogClick(Sender: TObject);
var
  Level: TLogLevel;
begin
  Level := TLogLevel(cmbLevel.Items.Objects[cmbLevel.ItemIndex]);
  // Log message as given
  FLogs[cmbLogger.ItemIndex].Log(Level, edtMessage.Text);
  // Log the edit control as an object as well
  FLogs[cmbLogger.ItemIndex].Log(Level, edtMessage);
end;

{ Add a context entry. }
procedure TfrmLog4DDemo.btnPushClick(Sender: TObject);
begin
  if edtNDC.Text <> '' then
  begin
    TLogNDC.Push(edtNDC.Text);
    edtNDC.Text    := '';
    edtNDC.SetFocus;
    lblNDC.Caption := TLogNDC.Peek;
  end;
end;

{ Remove a context entry. }
procedure TfrmLog4DDemo.btnPopClick(Sender: TObject);
begin
  TLogNDC.Pop;
  lblNDC.Caption := TLogNDC.Peek;
end;

{ Sample logging from loop, including error. }
procedure TfrmLog4DDemo.btnLoopClick(Sender: TObject);
var
  Index: Integer;
begin
  try
    for Index := 5 downto 0 do
      FLogs[cmbLogger.ItemIndex].Info(
        Format('%d divided by %d is %g', [3, Index, 3 / Index]));
  except on ex: Exception do
    FLogs[cmbLogger.ItemIndex].Fatal('Error in calculation', ex);
  end;
end;

{ Change logging level for a log. }
procedure TfrmLog4DDemo.cmbLoggerLevelChange(Sender: TObject);
begin
  with TComboBox(Sender) do
    FLogs[Tag].Level := TLogLevel(Items.Objects[ItemIndex]);
end;

{ Set the overall logging level threshold. }
procedure TfrmLog4DDemo.cmbThresholdChange(Sender: TObject);
begin
  FLogs[0].Hierarchy.Threshold :=
    TLogLevel(cmbThreshold.Items.Objects[cmbThreshold.ItemIndex]);
end;

{ Change additivity for a log. }
procedure TfrmLog4DDemo.chkAdditiveChange(Sender: TObject);
begin
  with TCheckBox(Sender) do
    FLogs[Tag].Additive := Checked;
end;

{ Apply a new filter value. }
procedure TfrmLog4DDemo.edtFilterChange(Sender: TObject);
begin
  FFilter.Options[MatchOpt] := edtFilter.Text;
end;

{ Keep splitters aligned. }
procedure TfrmLog4DDemo.splLeftMoved(Sender: TObject);
begin
  memMyAppOther.Height := memMyApp.Height;
end;

{ Keep splitters aligned. }
procedure TfrmLog4DDemo.splRightMoved(Sender: TObject);
begin
  memMyApp.Height := memMyAppOther.Height;
end;

initialization
  { Register new logging classes. }
  RegisterAppender(TMemoAppender);
  RegisterRendered(TComponent);
  RegisterRenderer(TComponentRenderer);
end.
