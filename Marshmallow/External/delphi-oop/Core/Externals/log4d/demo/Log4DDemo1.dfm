object frmLog4DDemo: TfrmLog4DDemo
  Left = 192
  Top = 120
  Width = 588
  Height = 610
  ActiveControl = edtMessage
  Caption = 'Log4D Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splVert: TSplitter
    Left = 284
    Top = 76
    Width = 2
    Height = 507
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 580
    Height = 76
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 22
      Top = 7
      Width = 26
      Height = 13
      Alignment = taRightJustify
      Caption = 'Le&vel'
      FocusControl = cmbLevel
    end
    object Label2: TLabel
      Left = 123
      Top = 7
      Width = 33
      Height = 13
      Alignment = taRightJustify
      Caption = 'Lo&gger'
      FocusControl = cmbLogger
    end
    object Label3: TLabel
      Left = 5
      Top = 29
      Width = 43
      Height = 13
      Alignment = taRightJustify
      Caption = '&Message'
      FocusControl = edtMessage
    end
    object edtMessage: TEdit
      Left = 52
      Top = 26
      Width = 193
      Height = 21
      Hint = 'Enter the message to be logged'
      TabOrder = 2
    end
    object btnLog: TButton
      Left = 53
      Top = 50
      Width = 45
      Height = 20
      Hint = 'Log the above message at the given level'
      Caption = '&Log'
      Default = True
      TabOrder = 3
      OnClick = btnLogClick
    end
    object cmbLogger: TComboBox
      Left = 160
      Top = 3
      Width = 85
      Height = 21
      Hint = 'Select the logger to perform the logging'
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'myapp'
      Items.Strings = (
        'myapp'
        'myapp.more'
        'myapp.other'
        'alt')
    end
    object grpFilter: TGroupBox
      Left = 252
      Top = 3
      Width = 41
      Height = 66
      Caption = '&Filter'
      TabOrder = 5
      object edtFilter: TEdit
        Left = 9
        Top = 22
        Width = 20
        Height = 21
        Hint = 'Exclude messages to '#39'myapp'#39' with this character'
        MaxLength = 1
        TabOrder = 0
        OnChange = edtFilterChange
      end
    end
    object grpNDC: TGroupBox
      Left = 300
      Top = 3
      Width = 186
      Height = 66
      Caption = '&NDC'
      TabOrder = 6
      object lblNDC: TLabel
        Left = 8
        Top = 44
        Width = 121
        Height = 13
        AutoSize = False
      end
      object edtNDC: TEdit
        Left = 8
        Top = 13
        Width = 121
        Height = 21
        Hint = 'Enter context information'
        TabOrder = 0
      end
      object btnPush: TButton
        Left = 133
        Top = 15
        Width = 45
        Height = 20
        Hint = 'Add the context information to the stack'
        Caption = 'P&ush'
        TabOrder = 1
        OnClick = btnPushClick
      end
      object btnPop: TButton
        Left = 133
        Top = 39
        Width = 45
        Height = 20
        Hint = 'Remove the latest context information from the stack'
        Caption = 'P&op'
        TabOrder = 2
        OnClick = btnPopClick
      end
    end
    object cmbLevel: TComboBox
      Left = 52
      Top = 3
      Width = 63
      Height = 21
      Hint = 'Select the level of the message'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object btnLoop: TButton
      Left = 168
      Top = 50
      Width = 75
      Height = 20
      Hint = 'Loop through info messages to a fatal error'
      Caption = 'Loop to &Error'
      TabOrder = 4
      OnClick = btnLoopClick
    end
    object grpThreshold: TGroupBox
      Left = 492
      Top = 3
      Width = 81
      Height = 66
      Caption = 'T&hreshold'
      TabOrder = 7
      DesignSize = (
        81
        66)
      object cmbThreshold: TComboBox
        Left = 8
        Top = 24
        Width = 65
        Height = 21
        Hint = 'Set the overall threshold level for logging'
        Style = csDropDownList
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 0
        OnChange = cmbThresholdChange
      end
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 76
    Width = 284
    Height = 507
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object splLeft: TSplitter
      Left = 0
      Top = 264
      Width = 284
      Height = 3
      Cursor = crVSplit
      Align = alTop
      OnMoved = splLeftMoved
    end
    object memMyApp: TMemo
      Left = 0
      Top = 21
      Width = 284
      Height = 243
      Hint = 'Output for '#39'myapp'#39' logger'
      Align = alTop
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object memMyAppMore: TMemo
      Left = 0
      Top = 288
      Width = 284
      Height = 219
      Hint = 'Output for '#39'myapp.more'#39' logger'
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object pnlMyapp: TPanel
      Left = 0
      Top = 0
      Width = 284
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'myapp'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      DesignSize = (
        284
        21)
      object chkMyappAdditive: TCheckBox
        Left = 220
        Top = 2
        Width = 61
        Height = 17
        Hint = 'Add to parent logger as well?'
        Anchors = [akTop, akRight]
        Caption = 'Additive'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = chkAdditiveChange
      end
      object cmbMyappLevel: TComboBox
        Left = 148
        Top = 0
        Width = 65
        Height = 21
        Hint = 'Set the threshold for this logger'
        Style = csDropDownList
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 0
        OnChange = cmbLoggerLevelChange
      end
    end
    object pnlMyappMore: TPanel
      Left = 0
      Top = 267
      Width = 284
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'myapp.more'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      DesignSize = (
        284
        21)
      object chkMyappMoreAdditive: TCheckBox
        Tag = 1
        Left = 220
        Top = 2
        Width = 61
        Height = 17
        Hint = 'Add to parent logger as well?'
        Anchors = [akTop, akRight]
        Caption = 'Additive'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = chkAdditiveChange
      end
      object cmbMyappMoreLevel: TComboBox
        Tag = 1
        Left = 148
        Top = 0
        Width = 65
        Height = 21
        Hint = 'Set the threshold for this logger'
        Style = csDropDownList
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 0
        OnChange = cmbLoggerLevelChange
      end
    end
  end
  object pnlRight: TPanel
    Left = 286
    Top = 76
    Width = 294
    Height = 507
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object splRight: TSplitter
      Left = 0
      Top = 264
      Width = 294
      Height = 3
      Cursor = crVSplit
      Align = alTop
      OnMoved = splRightMoved
    end
    object memMyAppOther: TMemo
      Left = 0
      Top = 21
      Width = 294
      Height = 243
      Hint = 'Output for '#39'myapp.other'#39' logger'
      Align = alTop
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object memAlt: TMemo
      Left = 0
      Top = 288
      Width = 294
      Height = 219
      Hint = 'Output for '#39'alt'#39' logger'
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object pnlMyappOther: TPanel
      Left = 0
      Top = 0
      Width = 294
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'myapp.other'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      DesignSize = (
        294
        21)
      object chkMyappOtherAdditive: TCheckBox
        Tag = 2
        Left = 230
        Top = 2
        Width = 61
        Height = 17
        Hint = 'Add to parent logger as well?'
        Anchors = [akTop, akRight]
        Caption = 'Additive'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = chkAdditiveChange
      end
      object cmbMyappOtherLevel: TComboBox
        Tag = 2
        Left = 158
        Top = 0
        Width = 65
        Height = 21
        Hint = 'Set the threshold for this logger'
        Style = csDropDownList
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 0
        OnChange = cmbLoggerLevelChange
      end
    end
    object pnlAlt: TPanel
      Left = 0
      Top = 267
      Width = 294
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'alt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      DesignSize = (
        294
        21)
      object chkAltAdditive: TCheckBox
        Tag = 3
        Left = 230
        Top = 2
        Width = 61
        Height = 17
        Hint = 'Add to parent logger as well?'
        Anchors = [akTop, akRight]
        Caption = 'Additive'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = chkAdditiveChange
      end
      object cmbAltLevel: TComboBox
        Tag = 3
        Left = 158
        Top = 0
        Width = 65
        Height = 21
        Hint = 'Set the threshold for this logger'
        Style = csDropDownList
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 0
        OnChange = cmbLoggerLevelChange
      end
    end
  end
end
