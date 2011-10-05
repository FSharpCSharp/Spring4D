object EnumerationDemoForm: TEnumerationDemoForm
  Left = 0
  Top = 0
  Caption = 'Enumeration Demo Form'
  ClientHeight = 453
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 232
    Top = 8
    Width = 343
    Height = 437
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 218
    Height = 25
    Caption = 'Run Regular Enumerable'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 218
    Height = 25
    Caption = 'Run WhereEnumerable (evens only)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 218
    Height = 25
    Caption = 'Run SkipEnumerable'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 101
    Width = 218
    Height = 25
    Caption = 'Run SkipWhileEnumerable to skip evens'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 132
    Width = 218
    Height = 25
    Caption = 'Run TakeEnumerable'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 163
    Width = 218
    Height = 25
    Caption = 'Run TakeWhileEnumerable'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 8
    Top = 194
    Width = 218
    Height = 25
    Caption = 'Concat two lists'
    TabOrder = 7
    OnClick = Button7Click
  end
end
