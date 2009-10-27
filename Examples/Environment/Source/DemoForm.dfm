object frmDemo: TfrmDemo
  Left = 274
  Top = 150
  Caption = 'System & Environment Demo'
  ClientHeight = 494
  ClientWidth = 678
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 65
    Width = 672
    Height = 426
    ActivePage = tsDriveInfo
    Align = alClient
    TabOrder = 0
    OnChange = pgcMainChange
    object tsGeneral: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 233
        Height = 13
        Align = alTop
        Caption = 'Global Variables && Environment Members'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object vleGeneral: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 658
        Height = 373
        Align = alClient
        DefaultRowHeight = 20
        TabOrder = 0
        TitleCaptions.Strings = (
          'Member'
          'Value')
        ColWidths = (
          192
          460)
      end
    end
    object tsEnvironmentVariables: TTabSheet
      Caption = 'Environment Variables'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object vleEnvironment: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 108
        Width = 658
        Height = 287
        Align = alClient
        TabOrder = 0
        TitleCaptions.Strings = (
          'Name'
          'Value')
        ColWidths = (
          185
          467)
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 664
        Height = 105
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label3: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 658
          Height = 13
          Align = alTop
          Caption = 'Environment.GetEnvironmentVariables method'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 266
        end
        object rgEnvironmentVariableTarget: TRadioGroup
          AlignWithMargins = True
          Left = 3
          Top = 22
          Width = 658
          Height = 80
          Align = alClient
          Caption = 'Environment Variable Target'
          Items.Strings = (
            'evtProcess (Default)'
            'evtUser'
            'evtMachine')
          TabOrder = 0
          OnClick = rgEnvironmentVariableTargetClick
        end
      end
    end
    object tsSpecialFolders: TTabSheet
      Caption = 'Special Folders'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 491
        Height = 13
        Align = alTop
        Caption = 
          'Using Environment.GetFolderPath method, we can get some certain ' +
          'special folder path.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object vleSpecialFolders: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 658
        Height = 373
        Align = alClient
        TabOrder = 0
        TitleCaptions.Strings = (
          'Special Folder'
          'Path')
        ColWidths = (
          150
          502)
      end
    end
    object tsNetwork: TTabSheet
      Caption = 'Network'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label7: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 389
        Height = 13
        Align = alTop
        Caption = 
          'Demonstrates how to get network status, local and public ip addr' +
          'ess.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object vleNetwork: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 658
        Height = 373
        Align = alClient
        TabOrder = 0
        TitleCaptions.Strings = (
          'Member'
          'Value')
        ColWidths = (
          150
          502)
      end
    end
    object tsDriveInfo: TTabSheet
      Caption = 'Drive Info'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 598
        Height = 13
        Align = alTop
        Caption = 
          'Demonstrates how to enumerate all drives of the computer with th' +
          'e help of TDriveInfo.GetDrives method.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object mmoDriveInfo: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 658
        Height = 373
        Align = alClient
        TabOrder = 0
      end
    end
    object tsServiceController: TTabSheet
      Caption = 'Service Controller'
      ImageIndex = 5
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label8: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 468
        Height = 13
        Align = alTop
        Caption = 
          'List all Windows services and learn how to start,  stop, pause a' +
          'nd resume a service.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lvServices: TListView
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 658
        Height = 373
        Align = alClient
        Columns = <
          item
            Caption = 'Service Name'
            Width = 150
          end
          item
            Caption = 'Status'
            Width = 90
          end
          item
            Caption = 'Description'
            Width = 400
          end>
        GridLines = True
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 678
    Height = 62
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 14
      Width = 436
      Height = 14
      Caption = 
        'Demonstrates how to use some utility classes in the Spring.Syste' +
        'm namespace.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 10
      Top = 34
      Width = 273
      Height = 13
      Caption = 'For more information, please visit the project homepage:'
    end
    object lblHomepage: TLabel
      Left = 289
      Top = 34
      Width = 268
      Height = 14
      Cursor = crHandPoint
      Caption = 'http://delphi-spring-framework.googlecode.com/'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      OnClick = lblHomepageClick
    end
  end
end
