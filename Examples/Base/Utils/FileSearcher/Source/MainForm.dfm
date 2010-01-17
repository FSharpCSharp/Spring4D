object frmSearchDemo: TfrmSearchDemo
  Left = 0
  Top = 0
  Caption = 'File Searcher'
  ClientHeight = 445
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    625
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileTypes: TLabel
    Left = 8
    Top = 86
    Width = 52
    Height = 13
    Caption = 'File Types:'
  end
  object lblLocations: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'Locations:'
  end
  object lblResults: TLabel
    Left = 8
    Top = 317
    Width = 39
    Height = 13
    Caption = 'Results:'
  end
  object btnStart: TButton
    Left = 519
    Top = 22
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object mmoFileTypes: TMemo
    Left = 8
    Top = 105
    Width = 145
    Height = 87
    Lines.Strings = (
      '*.*')
    TabOrder = 1
  end
  object btnStop: TButton
    Left = 519
    Top = 53
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object btnPause: TButton
    Left = 519
    Top = 84
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Pause'
    Enabled = False
    TabOrder = 3
    OnClick = btnPauseClick
  end
  object btnResume: TButton
    Left = 519
    Top = 115
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Resume'
    Enabled = False
    TabOrder = 4
    OnClick = btnResumeClick
  end
  object mmoLocations: TMemo
    Left = 8
    Top = 27
    Width = 487
    Height = 51
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'C:\'
      'D:\')
    TabOrder = 5
  end
  object grpOptions: TGroupBox
    Left = 320
    Top = 92
    Width = 175
    Height = 100
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options'
    TabOrder = 6
    object chkIncludeSubfolders: TCheckBox
      Left = 16
      Top = 24
      Width = 165
      Height = 17
      Caption = 'Include subfolders'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkIncludeSubfoldersClick
    end
  end
  object lvResults: TListView
    Left = 8
    Top = 336
    Width = 609
    Height = 101
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 120
      end
      item
        Caption = 'Location'
        Width = 120
      end
      item
        Caption = 'Type'
        Width = 90
      end
      item
        Caption = 'Size'
        Width = 90
      end
      item
        Caption = 'Last Modified'
        Width = 120
      end>
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 7
    ViewStyle = vsReport
  end
  object grpStatics: TGroupBox
    Left = 8
    Top = 198
    Width = 608
    Height = 107
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Statistics'
    TabOrder = 8
    DesignSize = (
      608
      107)
    object lbledtTotalCount: TLabeledEdit
      Left = 81
      Top = 21
      Width = 105
      Height = 21
      Color = clBtnFace
      Ctl3D = True
      EditLabel.Width = 60
      EditLabel.Height = 13
      EditLabel.Caption = 'Total Count:'
      LabelPosition = lpLeft
      LabelSpacing = 5
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 0
    end
    object lbledtElapsed: TLabeledEdit
      Left = 81
      Top = 48
      Width = 105
      Height = 21
      Color = clBtnFace
      Ctl3D = True
      EditLabel.Width = 41
      EditLabel.Height = 13
      EditLabel.Caption = 'Elapsed:'
      LabelPosition = lpLeft
      LabelSpacing = 5
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 1
    end
    object edtFiles: TLabeledEdit
      Left = 272
      Top = 48
      Width = 105
      Height = 21
      Color = clBtnFace
      Ctl3D = True
      EditLabel.Width = 25
      EditLabel.Height = 13
      EditLabel.Caption = 'Files:'
      LabelPosition = lpLeft
      LabelSpacing = 5
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 2
    end
    object edtFolders: TLabeledEdit
      Left = 272
      Top = 21
      Width = 105
      Height = 21
      Color = clBtnFace
      Ctl3D = True
      EditLabel.Width = 39
      EditLabel.Height = 13
      EditLabel.Caption = 'Folders:'
      LabelPosition = lpLeft
      LabelSpacing = 5
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 3
    end
    object edtLocation: TLabeledEdit
      Left = 81
      Top = 73
      Width = 509
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      Ctl3D = True
      EditLabel.Width = 44
      EditLabel.Height = 13
      EditLabel.Caption = 'Location:'
      LabelPosition = lpLeft
      LabelSpacing = 5
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 4
    end
  end
  object btnClear: TButton
    Left = 519
    Top = 146
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    TabOrder = 9
    OnClick = btnClearClick
  end
  object rgScope: TRadioGroup
    Left = 168
    Top = 92
    Width = 146
    Height = 100
    Caption = 'Scope'
    ItemIndex = 0
    Items.Strings = (
      'Directories and Files'
      'Directories'
      'Files')
    TabOrder = 10
    OnClick = rgScopeClick
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmr1Timer
    Left = 424
    Top = 224
  end
end
