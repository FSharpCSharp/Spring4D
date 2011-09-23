object Form28: TForm28
  Left = 0
  Top = 0
  Caption = 'Observer Pattern Demo'
  ClientHeight = 300
  ClientWidth = 635
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
  object Edit1: TEdit
    Left = 160
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 160
    Top = 83
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit2'
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 48
  end
end
