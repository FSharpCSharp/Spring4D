unit VclControlsBindingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  Spring.System,
  Spring.Binding,
  Spring.Collections,
  Spring.Reflection, Rtti,
  Model, ExtCtrls;

type
  TfrmMain = class(TForm)
    lstCustomers: TListBox;
    grpInformation: TGroupBox;
    edtName: TLabeledEdit;
    edtCity: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fContext: IBindingContext;
    fCustomers: IList<TCustomer>;
    function GetCustomers: IList<TCustomer>;
    procedure PopulateCustomers(const customers: IList<TCustomer>);
  public
    { Public declarations }
    property Customers: IList<TCustomer> read GetCustomers;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fContext := TBindingContext.Create(Self);
  fContext.DataSource := Self;
  fContext.AddBinding('lstCustomers.Items', 'Customers').Columns.Add('Name');

//  with fContext.AddContext('Clients') do
//  begin
//    AddBinding('Name', 'edtName.Text');
//    AddBinding('Address.City', 'edtName.Text');
//  end;

//  fContext.AddBinding('Customers.Name', 'edtName.Text');
//  fContext.AddBinding('Customers.Address.City', 'edtName.Text');
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

procedure TfrmMain.PopulateCustomers(const customers: IList<TCustomer>);
var
  paul: TCustomer;
  david: TCustomer;
begin
  paul := TCustomer.Create;
  paul.Name := 'Paul';
  paul.Address.City := 'Shanghai';

  david := TCustomer.Create;
  david.Name := 'David';
  david.Address.City := 'New York';

  customers.Add(paul);
  customers.Add(david);
end;

end.
