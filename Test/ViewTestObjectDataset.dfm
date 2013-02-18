object frmObjectDatasetTest: TfrmObjectDatasetTest
  Left = 0
  Top = 0
  Caption = 'TObjectDataset Test'
  ClientHeight = 392
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    665
    392)
  PixelsPerInch = 96
  TextHeight = 13
  object dbgList: TDBGrid
    Left = 0
    Top = 25
    Width = 665
    Height = 327
    Align = alClient
    BorderStyle = bsNone
    DataSource = dsList
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnTitleClick = dbgListTitleClick
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 665
    Height = 25
    DataSource = dsList
    Align = alTop
    TabOrder = 1
  end
  object edFilter: TEdit
    Left = 0
    Top = 352
    Width = 665
    Height = 21
    Align = alBottom
    TabOrder = 2
    OnKeyDown = edFilterKeyDown
  end
  object cbFiltered: TCheckBox
    Left = 560
    Top = 367
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Filtered'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = cbFilteredClick
  end
  object sbTotal: TStatusBar
    Left = 0
    Top = 373
    Width = 665
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object dsList: TDataSource
    Left = 488
    Top = 264
  end
end
