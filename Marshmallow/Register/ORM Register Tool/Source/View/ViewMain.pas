unit ViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Registration.EnvironmentVariable;

type
  TfrmMain = class(TForm)
    edDirectory: TButtonedEdit;
    lbl1: TLabel;
    btnAddDir: TButton;
    btnCreate: TButton;
    lblDescription: TLabel;
    cbbBDSVersion: TComboBox;
    lbl2: TLabel;
    procedure btnAddDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edDirectoryChange(Sender: TObject);
    procedure cbbBDSVersionChange(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
  private
    FRegisterEnvVar: TRegistration;
  public
    property RegisterEnvVar: TRegistration read FRegisterEnvVar;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileCtrl
  ,Constants
  ;

{$R *.dfm}

procedure TfrmMain.btnAddDirClick(Sender: TObject);
var
  sDir: string;
begin
  if SelectDirectory('Select main source directory', '', sDir) then
  begin
    edDirectory.Text := sDir;
    FRegisterEnvVar.ORMDirectory := sDir;
  end;
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);
begin
  if not FRegisterEnvVar.Execute then
  begin
    MessageBox(0, PChar(FRegisterEnvVar.LastErrorMsg), 'Error', MB_OK + MB_ICONSTOP);
  end
  else
  begin
    MessageBox(0, PChar('Environment variable created succesfully'), 'Success', MB_OK + MB_ICONINFORMATION);
  end;
end;

procedure TfrmMain.cbbBDSVersionChange(Sender: TObject);
begin
  FRegisterEnvVar.BDSVersion := TBDSVersionNumber(TComboBox(Sender).ItemIndex);
end;

procedure TfrmMain.edDirectoryChange(Sender: TObject);
begin
  FRegisterEnvVar.ORMDirectory := TButtonedEdit(Sender).Text;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FRegisterEnvVar := TRegistration.Create;
  cbbBDSVersion.Items.AddStrings(FRegisterEnvVar.BDSVersionsList);
  cbbBDSVersion.ItemIndex := 2;
  lblDescription.Caption := Format('Name of the environment variable: $(%S)', [ORM_ENV_VARIABLE_NAME]);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FRegisterEnvVar.Free;
end;

end.
