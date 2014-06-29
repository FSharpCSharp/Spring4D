object frmTest: TfrmTest
  Left = 0
  Top = 0
  Caption = 'Test Form'
  ClientHeight = 316
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 16
    Top = 43
    Width = 16
    Height = 13
    Caption = 'lbl1'
  end
  object edt1: TEdit
    Left = 16
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'edt1'
  end
  object Memo1: TMemo
    Left = 310
    Top = 8
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 312
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 64
    Width = 121
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 3
  end
  object ListBox1: TListBox
    Left = 152
    Top = 8
    Width = 145
    Height = 89
    ItemHeight = 13
    TabOrder = 4
  end
  object SpinEdit1: TSpinEdit
    Left = 16
    Top = 114
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
  object ColorBox1: TColorBox
    Left = 16
    Top = 144
    Width = 121
    Height = 22
    TabOrder = 6
  end
  object DateTimePicker1: TDateTimePicker
    Left = 312
    Top = 152
    Width = 183
    Height = 21
    Date = 40995.882421446760000000
    Time = 40995.882421446760000000
    TabOrder = 7
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 256
    Width = 481
    Height = 33
    ShowSelRange = False
    TabOrder = 8
  end
  object edScript: TEdit
    Left = 24
    Top = 184
    Width = 121
    Height = 21
    TabOrder = 9
    Text = 'edScript'
  end
  object edDate: TEdit
    Left = 24
    Top = 216
    Width = 121
    Height = 21
    TabOrder = 10
    Text = 'edDate'
  end
end
