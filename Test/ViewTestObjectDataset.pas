unit ViewTestObjectDataset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB;

type
  TfrmObjectDatasetTest = class(TForm)
    dsList: TDataSource;
    dbgList: TDBGrid;
    DBNavigator1: TDBNavigator;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmObjectDatasetTest: TfrmObjectDatasetTest;

implementation

{$R *.dfm}

end.
