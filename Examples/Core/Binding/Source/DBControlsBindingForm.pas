unit DBControlsBindingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  Spring.Binding,
  Model, StdCtrls, Mask, DBCtrls, Grids, DBGrids;

type
  TfrmBindingDemo = class(TForm)
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    DBGrid1: TDBGrid;
    Label3: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fBinder: TBinder;
    fOrder: TOrder;
    procedure BuildDomainObject;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBindingDemo: TfrmBindingDemo;

implementation

{$R *.dfm}

uses
  Spring.DesignPatterns;

procedure TfrmBindingDemo.BuildDomainObject;
begin
  fOrder := TOrder.Create;
  fOrder.Number := '123/2010';
  fOrder.DateOfOrder := Now;
  fOrder.AddItem('Fish', 20, 12.45);
  fOrder.AddItem('Chips', 3, 2.25);
end;

procedure TfrmBindingDemo.Button1Click(Sender: TObject);
begin
  // This change should update DBEdit1
  fOrder.Number := '125-byCode';
end;

procedure TfrmBindingDemo.FormCreate(Sender: TObject);
{
var
  bt: IBindingTable;
}
begin
  BuildDomainObject;

  // *** this register will stay in other unit, preference on framework. Spring.Binding.MapControls i think
  GetBindableMapping.Add(TDBEdit, TDatawareBindable);
  GetBindableMapping.Add(TDBGrid, TDatawareBindable);

  fBinder := TBinder.Create(Self, fOrder);
  fBinder.Add('DBEdit1', 'Number');
  fBinder.Add('DBEdit2', 'DateOfOrder');
  {
  bt := fBinder.AddTable('DBGrid1', 'Items');
  bt.Add('Name');
  bt.Add('Quantity');
  bt.Add('Price');  
  }
  fBinder.Active := True;
end;

procedure TfrmBindingDemo.FormDestroy(Sender: TObject);
begin
  fBinder.Free;
  fOrder.Free;
end;

end.
