unit ViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, ComCtrls
  ,Core.Session, Core.Interfaces
  ,Spring.Collections
  ,ProductModel
  {$IF CompilerVersion > 23}
  , System.Actions
  {$IFEND}
  ;

type
  TfrmMain = class(TForm)
    mmView: TMainMenu;
    alMain: TActionList;
    aBuildDatabase: TAction;
    Database1: TMenuItem;
    RebuildDatabase1: TMenuItem;
    lvProducts: TListView;
    aAddProduct: TAction;
    aRemoveProduct: TAction;
    aEditProduct: TAction;
    Products1: TMenuItem;
    AddNewProduct1: TMenuItem;
    EditProduct1: TMenuItem;
    N1: TMenuItem;
    RemoveProduct1: TMenuItem;
    N2: TMenuItem;
    aReLoadProducts: TAction;
    RefreshProducts1: TMenuItem;
    sbView: TStatusBar;
    aCommit: TAction;
    N3: TMenuItem;
    CommitChanges1: TMenuItem;
    procedure aBuildDatabaseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aAddProductExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aEditProductUpdate(Sender: TObject);
    procedure aEditProductExecute(Sender: TObject);
    procedure aReLoadProductsExecute(Sender: TObject);
    procedure aRemoveProductExecute(Sender: TObject);
    procedure aCommitExecute(Sender: TObject);
    procedure lvProductsData(Sender: TObject; Item: TListItem);
    procedure lvProductsDblClick(Sender: TObject);
  private
    { Private declarations }
    FConnection: IDBConnection;
    FProducts: IList<TProduct>;
    FSession: TSession;
  protected
    procedure DoBuildDatabase();
    procedure DoAddNewProduct();
    procedure DoEditProduct();
    procedure DoRemoveProduct();
    procedure DoRepaint();
    procedure DoReLoadProducts();
    procedure DoCommitChanges();
    procedure DoCheckEntities();
  public
    { Public declarations }
    property Connection: IDBConnection read FConnection;
    property Session: TSession read FSession;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Core.DatabaseManager
  ,Core.ConnectionFactory
  ,ViewEditProduct
  ;

{$R *.dfm}

procedure TfrmMain.aAddProductExecute(Sender: TObject);
begin
  DoAddNewProduct();
end;

procedure TfrmMain.aBuildDatabaseExecute(Sender: TObject);
begin
  DoBuildDatabase();
end;

procedure TfrmMain.aCommitExecute(Sender: TObject);
begin
  DoCommitChanges();
end;

procedure TfrmMain.aEditProductExecute(Sender: TObject);
begin
  DoEditProduct();
end;

procedure TfrmMain.aEditProductUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lvProducts.Selected);
end;

procedure TfrmMain.aReLoadProductsExecute(Sender: TObject);
begin
  DoReLoadProducts;
end;

procedure TfrmMain.aRemoveProductExecute(Sender: TObject);
begin
  DoRemoveProduct();
end;

procedure TfrmMain.DoAddNewProduct;
var
  LProduct: TProduct;
  LConfirmed: Boolean;
begin
  LConfirmed := False;
  LProduct := TProduct.Create;
  try
    LConfirmed := TfrmEditProduct.Edit(LProduct);
    if LConfirmed  then
    begin
      FProducts.Add(LProduct);
      DoRepaint();
    end;
  finally
    if not LConfirmed then
      LProduct.Free;
  end;
end;

procedure TfrmMain.DoBuildDatabase;
var
  LDbManager: TDatabaseManager;
begin
  LDbManager := TDatabaseManager.Create(FConnection);
  try
    LDbManager.BuildDatabase;
  finally
    LDbManager.Free;
  end;
end;

procedure TfrmMain.DoCheckEntities;
var
  LDbManager: TDatabaseManager;
begin
  LDbManager := TDatabaseManager.Create(FConnection);
  try
    if not LDbManager.EntityExists(TProduct) then
    begin
      LDbManager.BuildDatabase;
      DoReLoadProducts;
    end;
  finally
    LDbManager.Free;
  end;
end;

procedure TfrmMain.DoCommitChanges;
var
  LTran: IDBTransaction;
begin
  LTran := FSession.Connection.BeginTransaction;
  FSession.SaveList<TProduct>(FProducts);
  LTran.Commit;
end;

procedure TfrmMain.DoEditProduct;
var
  LProduct: TProduct;
begin
  LProduct := FProducts[lvProducts.Selected.Index];
  if TfrmEditProduct.Edit(LProduct) then
  begin
    DoRepaint;
  end;
end;

procedure TfrmMain.DoRepaint;
begin
  lvProducts.Items.Count := FProducts.Count;
  lvProducts.Invalidate;
  sbView.Panels[0].Text := Format('Total: %D', [FProducts.Count]);
end;

procedure TfrmMain.DoReLoadProducts;
begin
  lvProducts.Items.BeginUpdate;
  try
    FProducts := FSession.FindAll<TProduct>();
    DoRepaint();
  finally
    lvProducts.Items.EndUpdate;
  end;
end;

procedure TfrmMain.DoRemoveProduct;
var
  LProduct: TProduct;
begin
  LProduct := FProducts[lvProducts.Selected.Index];
  FSession.Delete(LProduct);
  FProducts.Remove(LProduct);
  DoRepaint();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  LConnJson: string;
begin
  LConnJson := IncludeTrailingPathDelimiter( ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + '..\..\') )
    + 'Config\Conn_Sqlite.json';
  FConnection := TConnectionFactory.GetInstanceFromFilename(dtSQLite, LConnJson);
  FSession := TSession.Create(FConnection);
  FProducts := TCollections.CreateObjectList<TProduct>(True);
  DoCheckEntities();
  DoRepaint();
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FProducts := nil;
  FSession.Free;
end;

procedure TfrmMain.lvProductsData(Sender: TObject; Item: TListItem);
var
  LProduct: TProduct;
begin
  LProduct := FProducts[Item.Index];
  Item.Caption := LProduct.Name;
  Item.SubItems.Add(CurrToStr(LProduct.Price));
  Item.SubItems.Add(IntToStr(LProduct.Quantity));
end;

procedure TfrmMain.lvProductsDblClick(Sender: TObject);
var
  LSelected: TListItem;
begin
  LSelected := lvProducts.Selected;
  if Assigned(LSelected) then
  begin
    DoEditProduct();
  end;
end;

end.
