unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Rtti,
  Model, ExtCtrls, Mask, DBCtrls, DB, Grids, DBGrids,
  Spring.System,
  Spring.Binding,
  Spring.Collections,
  Spring.Reflection,
  Spring.Binding.DB;

type
  TfrmMain = class(TForm)
    lstCustomers: TDBListBox;
    grpInformation: TGroupBox;
    edtName: TDBEdit;
    edtCity: TDBEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    dbnvgr1: TDBNavigator;
    edtAge: TDBEdit;
    lbl3: TLabel;
    dbgrd1: TDBGrid;
    mmRemarks: TDBMemo;
    lbl4: TLabel;
    cmbSex: TDBComboBox;
    lbl5: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fContext: IBindingContext;
    fCustomers: IList<TCustomer>;
    function GetCustomers: IList<TCustomer>;
    function GetSelected: TCustomer;
    procedure PopulateCustomers(const customers: IList<TCustomer>);
  public
    { Public declarations }
    property Customers: IList<TCustomer> read GetCustomers;
    property SelectedCustomer: TCustomer read GetSelected;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fContext := TBindingContext.Create(Self);

//  fContext.AddBinding('lstCustomers.Items', 'Customers', 'Name', 'ID');  // DisplayMemberPath, ValueMemberPath
  fContext.AddBinding('edtName', 'SelectedCustomer.Name');
  fContext.AddBinding('cmbSex', 'SelectedCustomer.Sex');
  fContext.AddBinding('edtAge', 'SelectedCustomer.Age');
  fContext.AddBinding('edtCity', 'SelectedCustomer.Address.City');

  fContext.IsActive := True;
end;

function TfrmMain.GetCustomers: IList<TCustomer>;
begin
  if fCustomers = nil then
  begin
    fCustomers := TCollections.CreateList<TCustomer>(True);
    PopulateCustomers(fCustomers);
  end;
  Result := fCustomers;
end;

function TfrmMain.GetSelected: TCustomer;
begin
  Result := Customers.First;
end;

procedure TfrmMain.PopulateCustomers(const customers: IList<TCustomer>);
var
  paul: TCustomer;
  david: TCustomer;
begin
  paul := TCustomer.Create;
  paul.Name := 'Paul';
  paul.BirthDate := EncodeDate(1983, 10, 27);
  paul.Sex := csMale;
  paul.Address.City := 'Shanghai';

  david := TCustomer.Create;
  david.Name := 'David';
  david.Address.City := 'New York';

  customers.Add(paul);
  customers.Add(david);
end;

end.
