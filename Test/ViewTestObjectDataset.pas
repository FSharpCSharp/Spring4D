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
    cbFiltered: TCheckBox;
    procedure edFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dbgListTitleClick(Column: TColumn);
    procedure FormCreate(Sender: TObject);
    procedure cbFilteredClick(Sender: TObject);
  private
    { Private declarations }
    FIndex: Integer;
  public
    { Public declarations }
  end;

var
  frmObjectDatasetTest: TfrmObjectDatasetTest;

implementation

uses
  Adapters.ObjectDataset;

{$R *.dfm}

procedure TfrmObjectDatasetTest.cbFilteredClick(Sender: TObject);
begin
  (dsList.DataSet as TObjectDataset).Filtered := TCheckBox(Sender).Checked;
end;

procedure TfrmObjectDatasetTest.dbgListTitleClick(Column: TColumn);
var
  sDir: string;
begin
  if FIndex mod 2 = 0 then
    sDir := ' ASC'
  else
    sDir := ' DESC';

  (dsList.DataSet as TObjectDataset).Sort := Column.FieldName + sDir;
  Inc(FIndex);
end;

procedure TfrmObjectDatasetTest.edFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    dsList.DataSet.Filter := edFilter.Text;
  end;
end;

procedure TfrmObjectDatasetTest.FormCreate(Sender: TObject);
begin
  FIndex := 0;
end;

end.
