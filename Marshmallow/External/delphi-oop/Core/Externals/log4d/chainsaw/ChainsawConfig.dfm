object frmConfig: TfrmConfig
  Left = 233
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Chainsaw Configuration'
  ClientHeight = 271
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgcConfig: TPageControl
    Left = 4
    Top = 4
    Width = 193
    Height = 229
    ActivePage = tabFormat
    TabOrder = 0
    object tabFormat: TTabSheet
      Caption = '&Format'
      object lbxLevels: TListBox
        Left = 8
        Top = 8
        Width = 168
        Height = 101
        Style = lbOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 0
        OnClick = lbxLevelsClick
        OnDrawItem = lbxLevelsDrawItem
      end
      object grdColour: TColorGrid
        Left = 8
        Top = 116
        Width = 168
        Height = 44
        GridOrdering = go8x2
        TabOrder = 1
        OnChange = grdColourChange
      end
      object btnFont: TButton
        Left = 7
        Top = 168
        Width = 75
        Height = 25
        Caption = 'F&ont'
        TabOrder = 2
        OnClick = btnFontClick
      end
      object chkColumnOnly: TCheckBox
        Left = 96
        Top = 172
        Width = 85
        Height = 17
        Caption = 'Col&umn only'
        TabOrder = 3
      end
    end
    object tabColumns: TTabSheet
      Caption = '&Columns'
      ImageIndex = 1
      object chlColumns: TCheckListBox
        Left = 8
        Top = 8
        Width = 141
        Height = 137
        ItemHeight = 13
        TabOrder = 0
        OnClick = chlColumnsClick
      end
      object btnUp: TBitBtn
        Left = 152
        Top = 40
        Width = 25
        Height = 25
        TabOrder = 1
        OnClick = btnUpClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333333777F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
          3333333777737777F333333099999990333333373F3333373333333309999903
          333333337F33337F33333333099999033333333373F333733333333330999033
          3333333337F337F3333333333099903333333333373F37333333333333090333
          33333333337F7F33333333333309033333333333337373333333333333303333
          333333333337F333333333333330333333333333333733333333}
        NumGlyphs = 2
      end
      object btnDown: TBitBtn
        Left = 152
        Top = 84
        Width = 25
        Height = 25
        TabOrder = 2
        OnClick = btnDownClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
          333333333337F33333333333333033333333333333373F333333333333090333
          33333333337F7F33333333333309033333333333337373F33333333330999033
          3333333337F337F33333333330999033333333333733373F3333333309999903
          333333337F33337F33333333099999033333333373333373F333333099999990
          33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333300033333333333337773333333}
        NumGlyphs = 2
      end
    end
    object tabSocket: TTabSheet
      Caption = '&Socket'
      ImageIndex = 2
      object Label1: TLabel
        Left = 8
        Top = 12
        Width = 19
        Height = 13
        Caption = '&Port'
        FocusControl = spnPort
      end
      object Label2: TLabel
        Left = 8
        Top = 40
        Width = 47
        Height = 13
        Caption = '&Threshold'
        FocusControl = cmbThreshold
      end
      object spnPort: TSpinEdit
        Left = 60
        Top = 8
        Width = 77
        Height = 22
        MaxValue = 65535
        MinValue = 0
        TabOrder = 0
        Value = 9009
      end
      object cmbThreshold: TComboBox
        Left = 60
        Top = 36
        Width = 77
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
      end
    end
  end
  object btnOK: TButton
    Left = 19
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 107
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 88
    Top = 104
  end
end
