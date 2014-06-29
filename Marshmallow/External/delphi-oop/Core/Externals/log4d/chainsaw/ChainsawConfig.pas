unit ChainsawConfig;

{
  Configuration for Chainsaw.

  Written by Keith Wood (kbwood@iprimus.com.au)
  Version 1.0 - 19 September 2003.
}

interface

uses
  SysUtils, Variants, Classes, Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, ColorGrd, ComCtrls, Spin,
  IniFiles, TypInfo, StrUtils, Log4D;

const
  { Section and property names in the .ini file. }
  Section           = 'Chainsaw';
  ColumnOnlyProp    = 'Column Only';
  ColumnsProp       = 'Columns';
  FontProp          = 'Font ';
  HeightProp        = 'Height';
  LeftProp          = 'Left';
  SocketEnabledProp = 'Socket Enabled';
  SocketPortProp    = 'Socket Port';
  ThresholdProp     = 'Threshold';
  TopProp           = 'Top';
  WidthProp         = 'Width';
  { The default port to listen on. }
  DefaultPort       = 9009;

type
  { Details about the formatting for a particular log level. }
  TLogLevelFormat = class(TObject)
  public
    Name: string;
    Level: Integer;
    Font: TFont;
    Background: TColor;
    constructor Create(const Name: string; const Level: Integer);
    destructor Destroy; override;
  end;

  { The configuration dialog. }
  TfrmConfig = class(TForm)
    pgcConfig: TPageControl;
      tabFormat: TTabSheet;
        lbxLevels: TListBox;
        grdColour: TColorGrid;
        btnFont: TButton;
        chkColumnOnly: TCheckBox;
      tabColumns: TTabSheet;
        chlColumns: TCheckListBox;
        btnUp: TBitBtn;
        btnDown: TBitBtn;
      tabSocket: TTabSheet;
        Label1: TLabel;
        spnPort: TSpinEdit;
        Label2: TLabel;
        cmbThreshold: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    dlgFont: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure chlColumnsClick(Sender: TObject);
    procedure grdColourChange(Sender: TObject);
    procedure lbxLevelsClick(Sender: TObject);
    procedure lbxLevelsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FCoords: array [0..3] of Integer;
    FLevelFormats: array [0..5] of TLogLevelFormat;
    FSettingColour: Boolean;
    FSocketEnabled: Boolean;
    function GetCoord(const Index: Integer): Integer;
    function GetFieldIndex(const Name: string): Integer;
    function GetFieldName(const Index: Integer): string;
    function GetFieldVisible(const Name: string): Boolean;
    function GetFormatBackground(const Level: Integer): TColor;
    function GetFormatColumnOnly: Boolean;
    function GetFormatFont(const Level: Integer): TFont;
    function GetSocketPort: Integer;
    function GetThreshold: TLogLevel;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetFieldIndex(const Name: string; const Index: Integer);
    procedure SetFieldVisible(const Name: string; const Visible: Boolean);
    procedure SetSocketEnabled(const Enabled: Boolean);
  public
    { The column order for a field by name. }
    property FieldIndex[const Name: string]: Integer read GetFieldIndex
      write SetFieldIndex;
    { The name of the field at a given column position. }
    property FieldName[const Index: Integer]: string read GetFieldName;
    { Whether a field (by name) is visible. }
    property FieldVisible[const Name: string]: Boolean read GetFieldVisible
      write SetFieldVisible;
    { The background colour for a given logging level
      (any integer value - uses next lowest standard level). }
    property FormatBackground[const Level: Integer]: TColor
      read GetFormatBackground;
    { Whether only the level columns are formatted this way. }
    property FormatColumnOnly: Boolean read GetFormatColumnOnly;
    { The font for a given logging level
      (any integer value - uses next lowest standard level). }
    property FormatFont[const Level: Integer]: TFont read GetFormatFont;
    { The height of the main form. }
    property MainHeight: Integer index 2 read GetCoord;
    { The left position of the main form. }
    property MainLeft: Integer index 1 read GetCoord;
    { The top position of the main form. }
    property MainTop: Integer index 0 read GetCoord;
    { The width of the main form. }
    property MainWidth: Integer index 3 read GetCoord;
    { Whether the socket should be activated. }
    property SocketEnabled: Boolean read FSocketEnabled write SetSocketEnabled;
    { The socket number to listen on. }
    property SocketPort: Integer read GetSocketPort;
    { The minimum logging level accepted over the socket. }
    property Threshold: TLogLEvel read GetThreshold;
    { Update the position and size of the main form in one go. }
    procedure SetCoords(const Top, Left, Height, Width: Integer);
    { Select which logging level is selected for formatting
      (any integer value - uses next lowest standard level). }
    procedure SetFormatLevel(const Level: Integer);
  end;

var
  frmConfig: TfrmConfig;

implementation

{$R *.dfm}

uses
  ChainsawData;

{ TLogLevelFormat -------------------------------------------------------------}

{ Initialisation. }
constructor TLogLevelFormat.Create(const Name: string; const Level: Integer);
begin
  inherited Create;
  Self.Name  := Name;
  Self.Level := Level;
  Font       := TFont.Create;
  Background := clWhite;
end;

{ Release resources. }
destructor TLogLevelFormat.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

{ TfrmConfig ------------------------------------------------------------------}

var
  DefaultFont: TFont;

{ Initialisation - set formatting levels, column order, load saved settings. }
procedure TfrmConfig.FormCreate(Sender: TObject);
var
  Index: Integer;
begin
  FSettingColour := False;
  // Create level formats
  FLevelFormats[0] := TLogLevelFormat.Create(All.Name,   All.Level);
  FLevelFormats[1] := TLogLevelFormat.Create(Debug.Name, Debug.Level);
  FLevelFormats[2] := TLogLevelFormat.Create(Info.Name,  Info.Level);
  FLevelFormats[3] := TLogLevelFormat.Create(Warn.Name,  Warn.Level);
  FLevelFormats[4] := TLogLevelFormat.Create(Error.Name, Error.Level);
  FLevelFormats[5] := TLogLevelFormat.Create(Fatal.Name, Fatal.Level);
  // Set formats into listbox
  for Index := Low(FLevelFormats) to High(FLevelFormats) do
    lbxLevels.AddItem(FLevelFormats[Index].Name, FLevelFormats[Index]);
  lbxLevels.ItemIndex := 0;
  lbxLevelsClick(lbxLevels);
  // Set fields in check listbox
  chlColumns.Items.Add('Thread id');
  chlColumns.Items.Add('Timestamp');
  chlColumns.Items.Add('Elapsed time');
  chlColumns.Items.Add('Level name');
  chlColumns.Items.Add('Level value');
  chlColumns.Items.Add('Logger name');
  chlColumns.Items.Add('Message');
  chlColumns.Items.Add('NDC');
  chlColumns.Items.Add('Error class');
  chlColumns.Items.Add('Error message');
  chlColumns.ItemIndex := 0;
  // Set levels in threshold combobox
  cmbThreshold.AddItem(All.Name,   All);
  cmbThreshold.AddItem(Debug.Name, Debug);
  cmbThreshold.AddItem(Info.Name,  Info);
  cmbThreshold.AddItem(Warn.Name,  Warn);
  cmbThreshold.AddItem(Error.Name, Error);
  cmbThreshold.AddItem(Fatal.Name, Fatal);
  cmbThreshold.AddItem(Off.Name,   Off);
  cmbThreshold.ItemIndex := 0;
  // Load saved settings
  LoadSettings;
end;

{ Release resources. }
procedure TfrmConfig.FormDestroy(Sender: TObject);
var
  Index: Integer;
begin
  for Index := Low(FLevelFormats) to High(FLevelFormats) do
    FLevelFormats[Index].Free;
end;

{ Reload the previous settings. }
procedure TfrmConfig.btnCancelClick(Sender: TObject);
begin
  LoadSettings;
end;

{ Move a column down in the order. }
procedure TfrmConfig.btnDownClick(Sender: TObject);
begin
  chlColumns.Items.Exchange(chlColumns.ItemIndex, chlColumns.ItemIndex + 1);
  chlColumnsClick(chlColumns);
end;

{ Update the font for the current logging level. }
procedure TfrmConfig.btnFontClick(Sender: TObject);
begin
  with dlgFont do
  begin
    Font.Assign(FLevelFormats[lbxLevels.ItemIndex].Font);
    if Execute then
    begin
      FLevelFormats[lbxLevels.ItemIndex].Font.Assign(Font);
      grdColour.ForegroundIndex := grdColour.ColorToIndex(Font.Color);
      lbxLevels.Invalidate;
    end;
  end;
end;

{ Save the updated settings. }
procedure TfrmConfig.btnOKClick(Sender: TObject);
begin
  SaveSettings;
end;

{ Move a column up in the order. }
procedure TfrmConfig.btnUpClick(Sender: TObject);
begin
  chlColumns.Items.Exchange(chlColumns.ItemIndex, chlColumns.ItemIndex - 1);
  chlColumnsClick(chlColumns);
end;

{ Enable/disable up/down buttons based on selected column. }
procedure TfrmConfig.chlColumnsClick(Sender: TObject);
begin
  btnUp.Enabled   := (chlColumns.ItemIndex > 0);
  btnDown.Enabled := (chlColumns.ItemIndex < chlColumns.Items.Count - 1);
end;

{ Return the position or size of the main form. }
function TfrmConfig.GetCoord(const Index: Integer): Integer;
begin
  Result := FCoords[Index];
end;

{ Return the column order for a field by name. }
function TfrmConfig.GetFieldIndex(const Name: string): Integer;
var
  DisplayName: string;
begin
  DisplayName := dtmLogging.FieldMapping.Values[Name];
  Result      := chlColumns.Items.IndexOf(DisplayName);
end;

{ Return the name of the field at a given column position. }
function TfrmConfig.GetFieldName(const Index: Integer): string;
begin
  Result := dtmLogging.FieldMapping.Values[chlColumns.Items[Index]];
end;

{ Determine whether a field (by name) is visible. }
function TfrmConfig.GetFieldVisible(const Name: string): Boolean;
var
  DisplayName: string;
begin
  DisplayName := dtmLogging.FieldMapping.Values[Name];
  Result      := chlColumns.Checked[chlColumns.Items.IndexOf(DisplayName)];
end;

{ Return the background colour for a given logging level
  (any integer value - uses next lowest standard level). }
function TfrmConfig.GetFormatBackground(const Level: Integer): TColor;
var
  Index: Integer;
begin
  for Index := Low(FLevelFormats) to High(FLevelFormats) do
    if FLevelFormats[Index].Level >= Level then
    begin
      Result := FLevelFormats[Index].Background;
      Exit;
    end;
  Result := clWhite;
end;

{ Determine whether only the level columns are formatted this way. }
function TfrmConfig.GetFormatColumnOnly: Boolean;
begin
  Result := chkColumnOnly.Checked;
end;

{ Return the font for a given logging level
  (any integer value - uses next lowest standard level). }
function TfrmConfig.GetFormatFont(const Level: Integer): TFont;
var
  Index: Integer;
begin
  for Index := Low(FLevelFormats) to High(FLevelFormats) do
    if FLevelFormats[Index].Level >= Level then
    begin
      Result := FLevelFormats[Index].Font;
      Exit;
    end;
  Result := DefaultFont;
end;

{ Return the socket number to listen on. }
function TfrmConfig.GetSocketPort: Integer;
begin
  Result := spnPort.Value;
end;

{ Return the minimum logging level accepted over the socket. }
function TfrmConfig.GetThreshold: TLogLevel;
begin
  Result := TLogLevel(cmbThreshold.Items.Objects[cmbThreshold.ItemIndex]);
end;

{ Update the foreground and background colours for the current logging level. }
procedure TfrmConfig.grdColourChange(Sender: TObject);
begin
  if FSettingColour then
    Exit;
  FLevelFormats[lbxLevels.ItemIndex].Background := grdColour.BackgroundColor;
  FLevelFormats[lbxLevels.ItemIndex].Font.Color := grdColour.ForegroundColor;
  lbxLevels.Invalidate;
end;

{ Show the formatting settings for the selected logging level. }
procedure TfrmConfig.lbxLevelsClick(Sender: TObject);
begin
  FSettingColour := True;
  grdColour.BackgroundIndex :=
    grdColour.ColorToIndex(FLevelFormats[lbxLevels.ItemIndex].Background);
  grdColour.ForegroundIndex :=
    grdColour.ColorToIndex(FLevelFormats[lbxLevels.ItemIndex].Font.Color);
  FSettingColour := False;
end;

{ Display the logging level as formatted. }
procedure TfrmConfig.lbxLevelsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with lbxLevels.Canvas do
  begin
    Brush.Color := FLevelFormats[Index].Background;
    Font.Assign(FLevelFormats[Index].Font);
    FillRect(Rect);
    TextOut(Rect.Left + 2, Rect.Top + 2, lbxLevels.Items[Index]);
  end;
end;

{ Load settings from the .ini file. }
procedure TfrmConfig.LoadSettings;
var
  Index, Index2: Integer;
  FontStyles: TFontStyles;
  Value, Style: string;

  { Extract the next token (delimited by '|') from the string. }
  function NextToken(var Value: string): string;
  var
    Index: Integer;
  begin
    Index  := Pos('|', Value);
    Result := IfThen(Index = 0, Value, Copy(Value, 1, Index - 1));
    Delete(Value, 1, Index);
  end;

begin
  with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
    try
      // Load settings for formatting logging levels
      for Index := Low(FLevelFormats) to High(FLevelFormats) do
      begin
        Value := ReadString(Section, FontProp +
          FLevelFormats[Index].Name, 'MS Sans Serif|8||clBlack|clWhite');
        FLevelFormats[Index].Font.Name := NextToken(Value);
        FLevelFormats[Index].Font.Size := StrToIntDef(NextToken(Value), 8);
        Style                          := NextToken(Value);
        FontStyles                     := [];
        if Pos('B', Style) > 0 then
          Include(FontStyles, fsBold);
        if Pos('I', Style) > 0 then
          Include(FontStyles, fsItalic);
        if Pos('S', Style) > 0 then
          Include(FontStyles, fsStrikeOut);
        if Pos('U', Style) > 0 then
          Include(FontStyles, fsUnderline);
        FLevelFormats[Index].Font.Style := FontStyles;
        FLevelFormats[Index].Font.Color := StringToColor(NextToken(Value));
        FLevelFormats[Index].Background := StringToColor(NextToken(Value));
      end;
      // Load list of columns in order
      for Index := 0 to dtmLogging.cdsLogging.FieldCount - 1 do
      begin
        Value  := ReadString(Section, ColumnsProp + IntToStr(Index),
          'Y' + chlColumns.Items[Index]);
        Index2 := chlColumns.Items.IndexOf(Copy(Value, 2, Length(Value)));
        chlColumns.Items.Move(Index2, Index);
        chlColumns.Checked[Index] := (Value[1] = 'Y');
      end;
      // Load remaining fields
      chkColumnOnly.Checked := ReadBool(Section, ColumnOnlyProp, False);
      FSocketEnabled := ReadBool(Section, SocketEnabledProp, False);
      spnPort.Value  := ReadInteger(Section, SocketPortProp, DefaultPort);
      cmbThreshold.ItemIndex := cmbThreshold.Items.IndexOfObject(
        TLogLEvel.GetLevel(ReadInteger(Section, ThresholdProp, All.Level)));
      FCoords[0]     := ReadInteger(Section, TopProp, 130);
      FCoords[1]     := ReadInteger(Section, LeftProp, 130);
      FCoords[2]     := ReadInteger(Section, HeightProp, 620);
      FCoords[3]     := ReadInteger(Section, WidthProp, 630);
    finally
      Free;
    end;
end;

{ Save user selections to the .ini file. }
procedure TfrmConfig.SaveSettings;
var
  Index: Integer;
  Value: string;
begin
  with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
    try
      // Save settings for formatting logging levels
      for Index := Low(FLevelFormats) to High(FLevelFormats) do
      begin
        Value := FLevelFormats[Index].Font.Name + '|' +
          IntToStr(FLevelFormats[Index].Font.Size) + '|' +
          IfThen(fsBold in FLevelFormats[Index].Font.Style, 'B', '') +
          IfThen(fsItalic in FLevelFormats[Index].Font.Style, 'I', '') +
          IfThen(fsStrikeOut in FLevelFormats[Index].Font.Style, 'S', '') +
          IfThen(fsUnderline in FLevelFormats[Index].Font.Style, 'U', '') + '|' +
          ColorToString(FLevelFormats[Index].Font.Color) + '|' +
          ColorToString(FLevelFormats[Index].Background);
        WriteString(Section, FontProp + FLevelFormats[Index].Name, Value);
      end;
      // Save list of columns in order
      for Index := 0 to 9 do
        WriteString(Section, ColumnsProp + IntToStr(Index),
          IfThen(chlColumns.Checked[Index], 'Y', 'N') + chlColumns.Items[Index]);
      // Save remaining fields
      WriteBool(Section, ColumnOnlyProp, chkColumnOnly.Checked);
      WriteBool(Section, SocketEnabledProp, FSocketEnabled);
      WriteInteger(Section, SocketPortProp, spnPort.Value);
      WriteInteger(Section, ThresholdProp, Threshold.Level);
      WriteInteger(Section, TopProp, FCoords[0]);
      WriteInteger(Section, LeftProp, FCoords[1]);
      WriteInteger(Section, HeightProp, FCoords[2]);
      WriteInteger(Section, WidthProp, FCoords[3]);
    finally
      Free;
    end;
end;

{ Update the position and size of the main form in one go. }
procedure TfrmConfig.SetCoords(const Top, Left, Height, Width: Integer);
begin
  FCoords[0] := Top;
  FCoords[1] := Left;
  FCoords[2] := Height;
  FCoords[3] := Width;
  SaveSettings;
end;

{ Update the column order for a field by name. }
procedure TfrmConfig.SetFieldIndex(const Name: string;
  const Index: Integer);
var
  DisplayName: string;
  Index2: Integer;
begin
  DisplayName := dtmLogging.FieldMapping.Values[Name];
  Index2      := chlColumns.Items.IndexOf(DisplayName);
  chlColumns.Items.Move(Index2, Index);
end;

{ Set whether a field (by name) is visible. }
procedure TfrmConfig.SetFieldVisible(const Name: string;
  const Visible: Boolean);
var
  DisplayName: string;
  Index: Integer;
begin
  DisplayName := dtmLogging.FieldMapping.Values[Name];
  Index       := chlColumns.Items.IndexOf(DisplayName);
  chlColumns.Checked[Index] := Visible;
end;

{ Select which logging level is selected for formatting
  (any integer value - uses next lowest standard level). }
procedure TfrmConfig.SetFormatLevel(const Level: Integer);
var
  Index: Integer;
begin
  pgcConfig.ActivePage := tabFormat;
  ActiveControl        := lbxLevels;
  lbxLevels.ItemIndex  := 0;
  for Index := 0 to lbxLevels.Count - 1 do
    if TLogLevelFormat(lbxLevels.Items.Objects[Index]).Level >= Level then
    begin
      lbxLevels.ItemIndex := Index;
      Break;
    end;
  lbxLevelsClick(lbxLevels);
end;

{ Update whether the socket should be activated. }
procedure TfrmConfig.SetSocketEnabled(const Enabled: Boolean);
begin
  if FSocketEnabled <> Enabled then
  begin
    FSocketEnabled := Enabled;
    SaveSettings;
  end;
end;

initialization
  DefaultFont := TFont.Create;
finalization
  DefaultFont.Free;
end.
