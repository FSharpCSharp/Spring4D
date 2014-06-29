object frmChainsaw: TfrmChainsaw
  Left = 192
  Top = 133
  Width = 631
  Height = 621
  Caption = 'Chainsaw'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    623
    575)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 26
    Height = 13
    Caption = '&Level'
    FocusControl = cmbLevel
  end
  object Label2: TLabel
    Left = 8
    Top = 80
    Width = 43
    Height = 13
    Caption = '&Message'
    FocusControl = cmbMessage
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = '&Timestamp'
    FocusControl = datStartDate
  end
  object Label4: TLabel
    Left = 252
    Top = 8
    Width = 9
    Height = 13
    Caption = 'to'
  end
  object Label5: TLabel
    Left = 8
    Top = 104
    Width = 23
    Height = 13
    Caption = '&NDC'
    FocusControl = cmbNDC
  end
  object Label6: TLabel
    Left = 8
    Top = 56
    Width = 33
    Height = 13
    Caption = 'L&ogger'
    FocusControl = cmbLogger
  end
  object dbgLogging: TDBGrid
    Left = 4
    Top = 128
    Width = 613
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = srcLogging
    DefaultDrawing = False
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    PopupMenu = popMenu
    TabOrder = 15
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnColumnMoved = dbgLoggingColumnMoved
    OnDrawDataCell = dbgLoggingDrawDataCell
    OnTitleClick = dbgLoggingTitleClick
  end
  object stbStatus: TStatusBar
    Left = 0
    Top = 556
    Width = 623
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object memDetails: TRichEdit
    Left = 4
    Top = 412
    Width = 613
    Height = 137
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    TabOrder = 16
  end
  object cmbLevel: TComboBox
    Left = 160
    Top = 28
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = FilterChange
  end
  object edtMessage: TEdit
    Left = 160
    Top = 76
    Width = 173
    Height = 21
    TabOrder = 10
    OnChange = FilterChange
  end
  object datStartDate: TDateTimePicker
    Left = 68
    Top = 4
    Width = 89
    Height = 21
    Date = 36892.855412465280000000
    Time = 36892.855412465280000000
    TabOrder = 0
    OnChange = FilterChange
  end
  object datEndDate: TDateTimePicker
    Left = 268
    Top = 4
    Width = 89
    Height = 21
    Date = 73050.855501250000000000
    Time = 73050.855501250000000000
    TabOrder = 2
    OnChange = FilterChange
  end
  object cmbMessage: TComboBox
    Left = 68
    Top = 76
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 9
    Text = 'contains'
    OnChange = FilterChange
    Items.Strings = (
      'contains'
      'equals'
      'starts with'
      'ends with')
  end
  object cmbLevelOp: TComboBox
    Left = 68
    Top = 28
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'at least'
    OnChange = FilterChange
    Items.Strings = (
      'at least'
      'equals'
      'at most')
  end
  object datStartTime: TDateTimePicker
    Left = 160
    Top = 4
    Width = 89
    Height = 21
    Date = 37882.000000000000000000
    Format = 'hh:mm:ss tt'
    Time = 37882.000000000000000000
    Kind = dtkTime
    TabOrder = 1
    OnChange = FilterChange
  end
  object datEndTime: TDateTimePicker
    Left = 360
    Top = 4
    Width = 89
    Height = 21
    Date = 37882.000000000000000000
    Format = 'hh:mm:ss tt'
    Time = 37882.000000000000000000
    Kind = dtkTime
    TabOrder = 3
    OnChange = FilterChange
  end
  object btnClear: TButton
    Left = 540
    Top = 4
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 14
    OnClick = btnClearClick
  end
  object edtNDC: TEdit
    Left = 160
    Top = 100
    Width = 173
    Height = 21
    TabOrder = 12
    OnChange = FilterChange
  end
  object cmbNDC: TComboBox
    Left = 68
    Top = 100
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 11
    Text = 'contains'
    OnChange = FilterChange
    Items.Strings = (
      'contains'
      'equals'
      'starts with'
      'ends with')
  end
  object chkNDCIgnoreCase: TCheckBox
    Left = 340
    Top = 104
    Width = 81
    Height = 17
    Caption = 'Igno&re case'
    TabOrder = 13
    OnClick = FilterChange
  end
  object edtLogger: TEdit
    Left = 160
    Top = 52
    Width = 173
    Height = 21
    TabOrder = 7
    OnChange = FilterChange
  end
  object cmbLogger: TComboBox
    Left = 68
    Top = 52
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 6
    Text = 'contains'
    OnChange = FilterChange
    Items.Strings = (
      'contains'
      'equals'
      'starts with'
      'ends with')
  end
  object chkLoggerIgnoreCase: TCheckBox
    Left = 340
    Top = 56
    Width = 81
    Height = 17
    Caption = '&Ignore case'
    TabOrder = 8
    OnClick = FilterChange
  end
  object mnuMain: TMainMenu
    Left = 148
    Top = 240
    object mniFile: TMenuItem
      Caption = '&File'
      object mniOpen: TMenuItem
        Caption = '&Open'
        ShortCut = 16463
        OnClick = mniOpenClick
      end
      object mniSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = mniSaveClick
      end
      object mniSep1: TMenuItem
        Caption = '-'
      end
      object mniClear: TMenuItem
        Caption = '&Clear'
        OnClick = mniClearClick
      end
      object mniSep2: TMenuItem
        Caption = '-'
      end
      object mniExit: TMenuItem
        Caption = 'E&xit'
      end
    end
    object mniOptions: TMenuItem
      Caption = '&Options'
      object mniListen: TMenuItem
        Caption = '&Listen on socket'
        OnClick = mniListenClick
      end
      object mniConfigure: TMenuItem
        Caption = '&Configure'
        OnClick = mniConfigureClick
      end
    end
  end
  object udpServer: TIdUDPServer
    Bindings = <>
    DefaultPort = 0
    OnUDPRead = udpServerUDPRead
    Left = 260
    Top = 240
  end
  object srcLogging: TDataSource
    DataSet = dtmLogging.cdsLogging
    OnDataChange = srcLoggingDataChange
    Left = 92
    Top = 240
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Logging File'
    Left = 316
    Top = 240
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Logging File'
    Left = 372
    Top = 240
  end
  object popMenu: TPopupMenu
    OnPopup = popMenuPopup
    Left = 204
    Top = 240
    object mniColumns: TMenuItem
      Caption = '&Columns'
    end
    object mniFormat: TMenuItem
      Caption = '&Format'
      object mniDefault: TMenuItem
        Caption = 'Def&ault...'
        OnClick = mniFormatClick
      end
      object mniDebug: TMenuItem
        Tag = 10000
        Caption = '&Debug...'
        OnClick = mniFormatClick
      end
      object mniInfo: TMenuItem
        Tag = 20000
        Caption = '&Info...'
        OnClick = mniFormatClick
      end
      object mniWarn: TMenuItem
        Tag = 30000
        Caption = '&Warn...'
        OnClick = mniFormatClick
      end
      object mniError: TMenuItem
        Tag = 40000
        Caption = '&Error...'
        OnClick = mniFormatClick
      end
      object mniFatal: TMenuItem
        Tag = 50000
        Caption = '&Fatal...'
        OnClick = mniFormatClick
      end
    end
  end
end
