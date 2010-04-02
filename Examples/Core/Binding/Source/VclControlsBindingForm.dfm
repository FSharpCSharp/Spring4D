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
  object lstCustomers: TListBox
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
    object edtName: TLabeledEdit
      Left = 80
      Top = 48
      Width = 121
      Height = 21
      EditLabel.Width = 27
      EditLabel.Height = 13
      EditLabel.Caption = 'Name'
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object edtCity: TLabeledEdit
      Left = 80
      Top = 88
      Width = 121
      Height = 21
      EditLabel.Width = 19
      EditLabel.Height = 13
      EditLabel.Caption = 'City'
      LabelPosition = lpLeft
      TabOrder = 1
    end
  end
end
