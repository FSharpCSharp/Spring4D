program CustomerList;

uses
  Vcl.Forms,
  Spring.Container,
  MainForm in 'Views\MainForm.pas' {CustomersView},
  CustomersViewModel in 'ViewModels\CustomersViewModel.pas',
  Customer in 'Models\Customer.pas',
  CustomerRepository in 'Services\CustomerRepository.pas',
  Registrations in 'Registrations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  RegisterTypes;
  GlobalContainer.Resolve<TCustomersView>;
//  Application.CreateForm(TCustomersView, CustomersView);
//  CustomersView.DataContext := TCustomersViewModel.Create(TCustomerRepository.Create(nil));
  Application.Run;
  ReportMemoryLeaksOnShutdown := True;
end.
