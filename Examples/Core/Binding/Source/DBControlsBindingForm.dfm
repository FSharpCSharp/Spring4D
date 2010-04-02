object frmBindingDemo: TfrmBindingDemo
  Left = 0
  Top = 0
  Caption = 'frmBindingDemo'
  ClientHeight = 292
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 48
    Height = 13
    Caption = 'Order No.'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 65
    Height = 13
    Caption = 'Date of order'
  end
  object Label3: TLabel
    Left = 8
    Top = 69
    Width = 56
    Height = 13
    Caption = 'Order items'
  end
  object DBEdit1: TDBEdit
    Left = 86
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object DBEdit2: TDBEdit
    Left = 86
    Top = 37
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 88
    Width = 538
    Height = 196
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 224
    Top = 6
    Width = 217
    Height = 25
    Caption = 'Test notification: Change Number By Code'
    TabOrder = 3
    OnClick = Button1Click
  end
end
