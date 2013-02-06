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
  PixelsPerInch = 96
  TextHeight = 13
  object dbgList: TDBGrid
    Left = 0
    Top = 25
    Width = 665
    Height = 367
    Align = alClient
    BorderStyle = bsNone
    DataSource = dsList
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 665
    Height = 25
    DataSource = dsList
    Align = alTop
    TabOrder = 1
    ExplicitLeft = 16
    ExplicitTop = 8
    ExplicitWidth = 240
  end
  object dsList: TDataSource
    Left = 488
    Top = 264
  end
end
