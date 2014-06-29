unit ChainsawMain;

{
  Chainsaw - a tool to cut your logs down to size.
  Designed to work with the Log4D implementation.
  Based on the tool of the same name for log4j (http://log4j.apache.org).

  Written by Keith Wood (kbwood@iprimus.com.au)
  Version 1.0 - 19 September 2003.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, DBGrids, DB, DBClient, Log4D, Menus, ComCtrls,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, IdSocketHandle,
  DateUtils, StrUtils;

type
  TfrmChainsaw = class(TForm)
    Label3: TLabel;
    datStartDate: TDateTimePicker;
    datStartTime: TDateTimePicker;
    Label4: TLabel;
    datEndDate: TDateTimePicker;
    datEndTime: TDateTimePicker;
    Label1: TLabel;
    cmbLevelOp: TComboBox;
    cmbLevel: TComboBox;
    Label6: TLabel;
    edtLogger: TEdit;
    cmbLogger: TComboBox;
    chkLoggerIgnoreCase: TCheckBox;
    Label2: TLabel;
    edtMessage: TEdit;
    cmbMessage: TComboBox;
    Label5: TLabel;
    edtNDC: TEdit;
    cmbNDC: TComboBox;
    chkNDCIgnoreCase: TCheckBox;
    btnClear: TButton;
    dbgLogging: TDBGrid;
    memDetails: TRichEdit;
    stbStatus: TStatusBar;
    srcLogging: TDataSource;
    mnuMain: TMainMenu;
      mniFile: TMenuItem;
        mniOpen: TMenuItem;
        mniSave: TMenuItem;
        mniSep1: TMenuItem;
        mniClear: TMenuItem;
        mniSep2: TMenuItem;
        mniExit: TMenuItem;
      mniOptions: TMenuItem;
        mniListen: TMenuItem;
        mniConfigure: TMenuItem;
    popMenu: TPopupMenu;
      mniColumns: TMenuItem;
      mniFormat: TMenuItem;
        mniDefault: TMenuItem;
        mniDebug: TMenuItem;
        mniInfo: TMenuItem;
        mniWarn: TMenuItem;
        mniError: TMenuItem;
        mniFatal: TMenuItem;
    udpServer: TIdUDPServer;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure dbgLoggingColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure dbgLoggingDrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure dbgLoggingTitleClick(Column: TColumn);
    procedure FilterChange(Sender: TObject);
    procedure mniClearClick(Sender: TObject);
    procedure mniColumnClick(Sender: TObject);
    procedure mniConfigureClick(Sender: TObject);
    procedure mniFormatClick(Sender: TObject);
    procedure mniListenClick(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure popMenuPopup(Sender: TObject);
    procedure srcLoggingDataChange(Sender: TObject; Field: TField);
    procedure udpServerUDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);
  private
    FClearing: Boolean;
    procedure Configure;
    procedure LoadFile(const FileName: string);
    procedure SetListening;
  public
  end;

var
  frmChainsaw: TfrmChainsaw;

implementation

{$R *.dfm}

uses
  ChainsawConfig, ChainsawFiler, ChainsawData;

{ Initialise level lists and dialogs. }
procedure TfrmChainsaw.FormCreate(Sender: TObject);
begin
  FClearing := False;
  cmbLevel.AddItem(Fatal.Name, Fatal);
  cmbLevel.AddItem(Error.Name, Error);
  cmbLevel.AddItem(Warn.Name,  Warn);
  cmbLevel.AddItem(Info.Name,  Info);
  cmbLevel.AddItem(Debug.Name, Debug);
  cmbLevel.AddItem(All.Name,   All);
  cmbLevel.ItemIndex := 5;
  dlgSave.InitialDir := ExtractFileDir(Application.ExeName);
  dlgOpen.InitialDir := ExtractFileDir(Application.ExeName);
  frmConfig          := TfrmConfig.Create(Self);
  Configure;
end;

{ Save current form location and size. }
procedure TfrmChainsaw.FormDestroy(Sender: TObject);
begin
  frmConfig.SetCoords(Top, Left, Height, Width);
end;

{ Reset filter fields to defaults values. }
procedure TfrmChainsaw.btnClearClick(Sender: TObject);
begin
  FClearing                   := True;
  datStartDate.Date           := EncodeDate(2001, 01, 01);
  datStartTime.Time           := EncodeTime(00, 00, 00, 000);
  datEndDate.Date             := EncodeDate(2099, 12, 31);
  datEndTime.Time             := EncodeTime(00, 00, 00, 000);
  cmbLevelOp.ItemIndex        := 0;
  cmbLevel.ItemIndex          := 5;
  cmbLogger.ItemIndex         := 0;
  edtLogger.Text              := '';
  chkLoggerIgnoreCase.Checked := False;
  cmbMessage.ItemIndex        := 0;
  edtMessage.Text             := '';
  cmbNDC.ItemIndex            := 0;
  edtNDC.Text                 := '';
  chkNDCIgnoreCase.Checked    := False;
  datStartDate.SetFocus;
  FClearing                   := True;
  FilterChange(nil);
end;

{ Initialise controls from saved settings. }
procedure TfrmChainsaw.Configure;
var
  Index: Integer;
  FieldName: string;
begin
  Top    := frmConfig.MainTop;
  Left   := frmConfig.MainLeft;
  Height := frmConfig.MainHeight;
  Width  := frmConfig.MainWidth;
  for Index := 0 to srcLogging.DataSet.FieldCount - 1 do
  begin
    FieldName := frmConfig.FieldName[Index];
    srcLogging.DataSet.FieldByName(FieldName).Index := Index;
    srcLogging.DataSet.Fields[Index].Visible        :=
      frmConfig.FieldVisible[FieldName];
  end;
  udpServer.DefaultPort := frmConfig.SocketPort;
  mniListen.Checked     := frmConfig.SocketEnabled;
  SetListening;
  dbgLogging.Invalidate;
end;

{ Save new column positions. }
procedure TfrmChainsaw.dbgLoggingColumnMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
var
  Index: Integer;
begin
  for Index := 0 to srcLogging.DataSet.FieldCount - 1 do
    frmConfig.FieldIndex[srcLogging.DataSet.Fields[Index].FieldName] := Index;
end;

{ Draw the grid cells, taking into account format settings. }
procedure TfrmChainsaw.dbgLoggingDrawDataCell(Sender: TObject;
  const Rect: TRect; Field: TField; State: TGridDrawState);
begin
  if gdSelected in State then
  begin
    dbgLogging.Canvas.Font.Assign(dbgLogging.Font);
    dbgLogging.Canvas.Brush.Color := clHighlight;
  end
  else if not frmConfig.FormatColumnOnly or
      (Copy(Field.FieldName, 1, 5) = 'Level') then
  begin
    dbgLogging.Canvas.Font.Assign(frmConfig.FormatFont[
      srcLogging.DataSet.FieldByName('LevelValue').AsInteger]);
    dbgLogging.Canvas.Brush.Color := frmConfig.FormatBackground[
      srcLogging.DataSet.FieldByName('LevelValue').AsInteger];
  end;
  dbgLogging.DefaultDrawDataCell(Rect, Field, State);
end;

{ Sort by column (ascending only) when header clicked. }
procedure TfrmChainsaw.dbgLoggingTitleClick(Column: TColumn);
begin
  dtmLogging.AddSort(Column.FieldName);
end;

{ Update the filter applying to the logging dataset. }
procedure TfrmChainsaw.FilterChange(Sender: TObject);
const
  DTFormat = 'dd/mm/yyyy hh:nn:ss ampm';
var
  Filter: string;
  Level: TLogLevel;

  { Create a text field clause for the filter. }
  procedure SetTextCheck(FieldName, Value: string; const Op: Integer;
    const IgnoreCase: Boolean);
  begin
    if Value <> '' then
    begin
      Filter := Filter + ' and ' +
        IfThen(IgnoreCase, 'Upper(' + FieldName + ')', FieldName);
      Value  := IfThen(IgnoreCase, UpperCase(Value), Value);
      case Op of
        0: Filter := Filter + ' like ''%' + Value + '%''';  // Contains
        1: Filter := Filter + ' = ''' + Value + '''';       // Equals
        2: Filter := Filter + ' like ''' + Value + '%''';   // Starts with
        3: Filter := Filter + ' like ''%' + Value + '''';   // Ends with
      end;
    end;
  end;

begin
  if FClearing then
    Exit;
  Filter := 'Timestamp >= ''' + FormatDateTime(DTFormat,
    DateOf(datStartDate.Date) + TimeOf(datStartTime.Time)) + '''' +
    ' and Timestamp < ''' + FormatDateTime(DTFormat,
    DateOf(datEndDate.Date) + TimeOf(datEndTime.Time)) + '''';
  Level  := TLogLevel(cmbLevel.Items.Objects[cmbLevel.ItemIndex]);
  if Level.Level <> All.Level then
  begin
    Filter := Filter + ' and LevelValue ';
    case cmbLevelOp.ItemIndex of
      0: Filter := Filter + '>= ';  // At least
      1: Filter := Filter + '= ';   // Equals
      2: Filter := Filter + '<= ';  // At most
    end;
    Filter := Filter + IntToStr(Level.Level);
  end;
  SetTextCheck('LoggerName', edtLogger.Text, cmbLogger.ItemIndex,
    chkLoggerIgnoreCase.Checked);
  SetTextCheck('Message', edtMessage.Text, cmbMessage.ItemIndex, False);
  SetTextCheck('NDC', edtNDC.Text, cmbNDC.ItemIndex, chkNDCIgnoreCase.Checked);
  srcLogging.DataSet.Filter := Filter;
end;

{ Load a logging file in XML format into the dataset. }
procedure TfrmChainsaw.LoadFile(const FileName: string);
begin
  stbStatus.SimpleText := 'Loading file ' + FileName;
  LoadFromFile(TClientDataSet(srcLogging.DataSet), FileName);
  stbStatus.SimpleText := 'Loaded file ' + FileName;
end;

{ Empty the dataset after confirmation. }
procedure TfrmChainsaw.mniClearClick(Sender: TObject);
begin
  if MessageDlg('Clear logging events?', mtConfirmation, [mbYes, mbNo], 0) =
      mrYes then
    dtmLogging.EmptyDataSet;
end;

{ Toggle visibility of columns in the grid. }
procedure TfrmChainsaw.mniColumnClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  FieldName: string;
begin
  MenuItem  := TMenuItem(Sender);
  FieldName := srcLogging.DataSet.Fields[MenuItem.Tag].FieldName;
  srcLogging.DataSet.Fields[MenuItem.Tag].Visible := not MenuItem.Checked;
  // Save visibility setting
  frmConfig.FieldVisible[FieldName]               := not MenuItem.Checked;
end;

{ Bring up the configuration dialog. }
procedure TfrmChainsaw.mniConfigureClick(Sender: TObject);
begin
  if frmConfig.ShowModal = mrOK then
    Configure;
end;

{ Bring up the configuration for a particular level format. }
procedure TfrmChainsaw.mniFormatClick(Sender: TObject);
begin
  frmConfig.SetFormatLevel(TMenuItem(Sender).Tag);
  mniConfigureClick(mniConfigure);
end;

{ Toggle listening on input socket. }
procedure TfrmChainsaw.mniListenClick(Sender: TObject);
begin
  mniListen.Checked := not mniListen.Checked;
  SetListening;
end;

{ Select a file to load into the dataset. }
procedure TfrmChainsaw.mniOpenClick(Sender: TObject);
var
  Listening: Boolean;
begin
  Listening         := mniListen.Checked;
  mniListen.Checked := False;
  with dlgOpen do
    if Execute then
      LoadFile(FileName)
    else
      mniListen.Checked := Listening;
  SetListening;
end;

{ Select an output file and save the dataset contents as XML. }
procedure TfrmChainsaw.mniSaveClick(Sender: TObject);
begin
  with dlgSave do
    if Execute then
      SaveToFile(TClientDataSet(srcLogging.DataSet), FileName);
end;

{ Update the list of columns in the popup menu for the grid. }
procedure TfrmChainsaw.popMenuPopup(Sender: TObject);
var
  Index: Integer;
  MenuItem: TMenuItem;
begin
  for Index := 0 to mniColumns.Count - 1 do
  begin
    MenuItem := mniColumns[0];
    mniColumns.Remove(MenuItem);
    MenuItem.Free;
  end;
  for Index := 0 to srcLogging.DataSet.FieldCount - 1 do
  begin
    MenuItem         := TMenuItem.Create(Self);
    MenuItem.Caption := srcLogging.DataSet.Fields[Index].DisplayLabel;
    MenuItem.Checked := srcLogging.DataSet.Fields[Index].Visible;
    MenuItem.Tag     := Index;
    MenuItem.OnClick := mniColumnClick;
    mniColumns.Add(MenuItem);
  end;
end;

{ Update UDP server based on user settings. }
procedure TfrmChainsaw.SetListening;
begin
  udpServer.Active        := mniListen.Checked;
  frmConfig.SocketEnabled := mniListen.Checked;
  if udpServer.Active then
    stbStatus.SimpleText := 'Listening on socket ' +
      IntToStr(udpServer.DefaultPort) + ' (' + frmConfig.Threshold.Name + ')'
  else
    stbStatus.SimpleText := '';
end;

{ Display log event details as rich text when selected. }
procedure TfrmChainsaw.srcLoggingDataChange(Sender: TObject; Field: TField);
const
  Cr  = #13;
  Tab = #9;
var
  Index, Posn: Integer;
begin
  with memDetails, srcLogging.DataSet do
  begin
    Text := '';
    // Set details as straight text
    Lines.Append(FieldByName('ThreadId').DisplayLabel + ':' + Tab +
      FieldByName('ThreadId').AsString);
    Lines.Append(FieldByName('Timestamp').DisplayLabel + ':' + Tab +
      FieldByName('Timestamp').AsString);
    Lines.Append(FieldByName('ElapsedTime').DisplayLabel + ':' + Tab +
      FieldByName('ElapsedTime').AsString);
    Lines.Append(FieldByName('LevelName').DisplayLabel + ':' + Tab +
      FieldByName('LevelName').AsString +
      ' (' + FieldByName('LevelValue').AsString + ')');
    Lines.Append(FieldByName('LoggerName').DisplayLabel + ':' + Tab +
      FieldByName('LoggerName').AsString);
    Lines.Append(FieldByName('Message').DisplayLabel +':' + Tab +
      FieldByName('Message').AsString);
    Lines.Append(FieldByName('NDC').DisplayLabel + ':' + Tab + Tab +
      FieldByName('NDC').AsString);
    Lines.Append(FieldByName('ErrorMessage').DisplayLabel +':' + Tab +
      '(' + FieldByName('ErrorClass').AsString + ')' + Cr +
      FieldByName('ErrorMessage').AsString);
    // Apply formatting
    SelStart := 0;
    for Index := 0 to Lines.Count - 1 do
    begin
      Posn := Pos(Tab, Lines[Index]);
      if Posn = 0 then
        SelLength := Length(Lines[Index]) + 1
      else
      begin
        SelLength           := Posn;
        SelAttributes.Style := [fsBold];
        SelStart            := SelStart + Posn;
        Posn                := Length(Lines[Index]) + 2 - Posn;
      end;
      SelLength           := Posn;
      SelAttributes.Style := [];
      SelStart            := SelStart + Posn;
    end;
  end;
end;

{ Respond to an incoming UDP message.
  The expected format is fields separated by tabs (#9):
  message#9threadId#9timestamp(yyyymmddhhnnsszzz)#9elapsedTime(ms)#9
  levelName#9levelValue#9loggerName#9NDC#9errorMessage#9errorClass
  This is the format used by default by the TLogIndySocketAppender. }
procedure TfrmChainsaw.udpServerUDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var
  LogStream: TStringStream;
  Event, Value: string;
  Index, Field: Integer;
  Timestamp: TDateTime;
  Message, ThreadId, LevelName, LoggerName: string;
  NDC, ErrorMessage, ErrorClass: string;
  ElapsedTime, LevelValue: Integer;
begin
  LogStream := TStringStream.Create('');
  try
    LogStream.CopyFrom(AData, AData.Size);
    Event := LogStream.DataString;
    if Event = '' then
      Exit;
    ElapsedTime := 0;
    LevelValue  := 0;
    Timestamp   := Now;
    Field       := 1;
    repeat
      Index := Pos(#9, Event);
      if Index > 0 then
      begin
        Value := Copy(Event, 1, Index - 1);
        Delete(Event, 1, Index);
      end
      else
        Value := Event;
      case Field of
        1:  Message  := Value;
        2:  ThreadId := Value;
        3:  try
              Timestamp := EncodeDateTime(StrToInt(Copy(Value, 1, 4)),
                StrToInt(Copy(Value, 5, 2)), StrToInt(Copy(Value, 7, 2)),
                StrToInt(Copy(Value, 9, 2)), StrToInt(Copy(Value, 11, 2)),
                StrToInt(Copy(Value, 13, 2)), StrToInt(Copy(Value, 15, 3)));
            except on E: EConvertError do
              // Ignore
            end;
        4:  try
              ElapsedTime := StrToInt(Value);
            except on E: EConvertError do
              ElapsedTime := 0;
            end;
        5:  LevelName := Value;
        6:  try
              LevelValue := StrToInt(Value);
            except on E: EConvertError do
              LevelValue := 0;
            end;
        7:  LoggerName   := Value;
        8:  NDC          := Value;
        9:  ErrorMessage := Value;
        10: ErrorClass   := Value;
      end;
      Inc(Field);
    until Index = 0;
    if LevelValue >= frmConfig.Threshold.Level then
      dtmLogging.AddLog(Message, ThreadId, Timestamp, ElapsedTime, LevelName,
        LevelValue, LoggerName, NDC, ErrorMessage, ErrorClass);
  finally
    LogStream.Free;
  end;
end;

end.
