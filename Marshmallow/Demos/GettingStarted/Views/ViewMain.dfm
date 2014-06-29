object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Getting Started'
  ClientHeight = 395
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmView
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvProducts: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 543
    Height = 370
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Price'
        Width = 75
      end
      item
        Caption = 'Quantity'
        Width = 80
      end>
    GridLines = True
    HotTrack = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = lvProductsData
    OnDblClick = lvProductsDblClick
  end
  object sbView: TStatusBar
    Left = 0
    Top = 376
    Width = 549
    Height = 19
    Panels = <
      item
        Width = 250
      end
      item
        Width = 50
      end>
  end
  object mmView: TMainMenu
    Left = 472
    Top = 272
    object Database1: TMenuItem
      Caption = 'Database'
      object RebuildDatabase1: TMenuItem
        Action = aBuildDatabase
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object CommitChanges1: TMenuItem
        Action = aCommit
      end
    end
    object Products1: TMenuItem
      Caption = 'Products'
      object AddNewProduct1: TMenuItem
        Action = aAddProduct
        ShortCut = 16462
      end
      object EditProduct1: TMenuItem
        Action = aEditProduct
        ShortCut = 16453
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object RemoveProduct1: TMenuItem
        Action = aRemoveProduct
        ShortCut = 16430
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object RefreshProducts1: TMenuItem
        Action = aReLoadProducts
        ShortCut = 116
      end
    end
  end
  object alMain: TActionList
    Left = 424
    Top = 272
    object aBuildDatabase: TAction
      Category = 'Database'
      Caption = 'Rebuild Database'
      OnExecute = aBuildDatabaseExecute
    end
    object aAddProduct: TAction
      Category = 'Products'
      Caption = 'Add New Product'
      OnExecute = aAddProductExecute
    end
    object aRemoveProduct: TAction
      Category = 'Products'
      Caption = 'Remove Product'
      OnExecute = aRemoveProductExecute
      OnUpdate = aEditProductUpdate
    end
    object aEditProduct: TAction
      Category = 'Products'
      Caption = 'Edit Product'
      OnExecute = aEditProductExecute
      OnUpdate = aEditProductUpdate
    end
    object aReLoadProducts: TAction
      Category = 'Products'
      Caption = 'Load Products'
      OnExecute = aReLoadProductsExecute
    end
    object aCommit: TAction
      Category = 'Database'
      Caption = 'Commit Changes'
      ShortCut = 16467
      OnExecute = aCommitExecute
    end
  end
end
