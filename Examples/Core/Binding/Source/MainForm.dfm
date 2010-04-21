object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Data Binding Sample of VCL Controls'
  ClientHeight = 469
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lstCustomers: TDBListBox
    Left = 8
    Top = 8
    Width = 225
    Height = 453
    ItemHeight = 13
    TabOrder = 0
  end
  object grpInformation: TGroupBox
    Left = 248
    Top = 8
    Width = 434
    Height = 161
    Caption = 'Information'
    TabOrder = 1
    object lbl1: TLabel
      Left = 24
      Top = 51
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object lbl2: TLabel
      Left = 24
      Top = 105
      Width = 23
      Height = 13
      Caption = 'City:'
    end
    object lbl3: TLabel
      Left = 24
      Top = 78
      Width = 23
      Height = 13
      Caption = 'Age:'
    end
    object edtName: TDBEdit
      Left = 80
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtCity: TDBEdit
      Left = 80
      Top = 102
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object edtAge: TDBEdit
      Left = 80
      Top = 75
      Width = 121
      Height = 21
      TabOrder = 2
    end
  end
  object dbnvgr1: TDBNavigator
    Left = 248
    Top = 185
    Width = 430
    Height = 25
    TabOrder = 2
  end
  object dbgrd1: TDBGrid
    Left = 248
    Top = 240
    Width = 434
    Height = 221
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
end
