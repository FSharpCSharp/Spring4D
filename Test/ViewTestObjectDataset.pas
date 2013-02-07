unit ViewTestObjectDataset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB, JvMemoryDataset, StdCtrls;

type
  TfrmObjectDatasetTest = class(TForm)
    dsList: TDataSource;
    dbgList: TDBGrid;
    DBNavigator1: TDBNavigator;
    JvMemoryData1: TJvMemoryData;
    edFilter: TEdit;
    procedure edFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmObjectDatasetTest: TfrmObjectDatasetTest;

implementation

{$R *.dfm}

procedure TfrmObjectDatasetTest.edFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    dsList.DataSet.Filter := edFilter.Text;
  end;
end;

end.
