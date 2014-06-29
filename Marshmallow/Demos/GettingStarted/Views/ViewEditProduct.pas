unit ViewEditProduct;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Mask
  ,ProductModel
  ;

type
  TfrmEditProduct = class(TForm)
    edName: TEdit;
    medtPrice: TMaskEdit;
    seQuantity: TSpinEdit;
    lblQuantity: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblName: TLabel;
    lblPrice: TLabel;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FProduct: TProduct;
    procedure SetProduct(const Value: TProduct);
  protected
    procedure DoGetProductProperties(); virtual;
    procedure DoSetProductProperties(); virtual;
  public
    { Public declarations }
    property Product: TProduct read FProduct write SetProduct;

    class function Edit(AProduct: TProduct): Boolean;
  end;

var
  frmEditProduct: TfrmEditProduct;

implementation

{$R *.dfm}

{ TfrmEditProduct }

procedure TfrmEditProduct.btnOKClick(Sender: TObject);
begin
  DoSetProductProperties();
end;

procedure TfrmEditProduct.DoGetProductProperties;
begin
  edName.Text := FProduct.Name;
  medtPrice.Text := CurrToStr(FProduct.Price);
  seQuantity.Value := FProduct.Quantity;
end;

procedure TfrmEditProduct.DoSetProductProperties;
begin
  FProduct.Name := edName.Text;
  FProduct.Price := StrToCurr(medtPrice.Text);
  FProduct.Quantity := seQuantity.Value;
end;

class function TfrmEditProduct.Edit(AProduct: TProduct): Boolean;
var
  LForm: TfrmEditProduct;
begin
  LForm := TfrmEditProduct.Create(Application);
  try
    LForm.Product := AProduct;
    Result := (LForm.ShowModal = mrOk);
  finally
    LForm.Free;
  end;
end;

procedure TfrmEditProduct.SetProduct(const Value: TProduct);
begin
  FProduct := Value;
  DoGetProductProperties();
end;

end.
