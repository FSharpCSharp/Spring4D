object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ORM Register Tool'
  ClientHeight = 167
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    457
    167)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 16
    Top = 8
    Width = 159
    Height = 13
    Caption = 'Directory of "Marshmallow" ORM:'
  end
  object lblDescription: TLabel
    Left = 8
    Top = 139
    Width = 207
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Name of the environment variable: $(ORM)'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lbl2: TLabel
    Left = 16
    Top = 59
    Width = 61
    Height = 13
    Caption = 'BDS version:'
  end
  object edDirectory: TButtonedEdit
    Left = 16
    Top = 32
    Width = 404
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Full path to directory...'
    OnChange = edDirectoryChange
    ExplicitWidth = 495
  end
  object btnAddDir: TButton
    Left = 426
    Top = 31
    Width = 23
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnAddDirClick
    ExplicitLeft = 517
  end
  object btnCreate: TButton
    Left = 374
    Top = 134
    Width = 75
    Height = 25
    Hint = 'Create environment variable'
    Anchors = [akRight, akBottom]
    Caption = 'Create'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnCreateClick
    ExplicitLeft = 465
    ExplicitTop = 298
  end
  object cbbBDSVersion: TComboBox
    Left = 208
    Top = 56
    Width = 212
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = cbbBDSVersionChange
  end
end
