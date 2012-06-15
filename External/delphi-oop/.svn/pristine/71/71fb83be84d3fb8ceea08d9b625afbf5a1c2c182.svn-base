unit TestSvHelpers;

interface

uses
  TestFramework,  SysUtils, SvHelpers, Classes;

type
  TestHelpers = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPictureHelper;
    procedure TestFieldHelper;
  end;

implementation

uses
  Graphics,
  jpeg,
  Types,
  DBClient,
  DB;

{ TestHelpers }

procedure TestHelpers.SetUp;
begin
  inherited;
end;

procedure TestHelpers.TearDown;
begin
  inherited;
end;

procedure TestHelpers.TestFieldHelper;
var
  cs: TClientDataset;
  i: Integer;
  iVal: Integer;
  sVal: string;
begin
  cs := TClientDataSet.Create(nil);
  try
    cs.FieldDefs.Add('ID', ftInteger);
    cs.FieldDefs.Add('Name', ftWideString, 50);
    cs.FieldDefs.Add('Salary', ftFloat);

    cs.CreateDataSet;

    CheckEquals(3, cs.FieldCount);

    cs.DisableControls;

    //populate some data
    for i := 1 to 10 do
    begin
      cs.Append;
      cs.Fields[0].Value := i;
      cs.Fields[1].Value := 'Name ' + IntToStr(i);
      cs.Fields[2].Value := i;
      cs.Post;
    end;

    cs.First;
    iVal := cs.Fields[0].ValueOrDef(0);
    CheckEquals(1, iVal);
    cs.Edit;
    cs.Fields[0].Clear;
    cs.Post;
    iVal := cs.Fields[0].ValueOrDef(0);
    CheckEquals(0, iVal);
    sVal := cs.Fields[1].ValueOrDef('null');
    CheckEquals('Name 1', sVal);
    cs.Edit;
    cs.Fields[1].Clear;
    cs.Post;
    sVal := cs.Fields[1].ValueOrDef('null');
    CheckEqualsString('null', sVal);
  finally
    cs.Free;
  end;
end;

procedure TestHelpers.TestPictureHelper;
var
  pic: TPicture;
  rStream: TResourceStream;
begin
  pic := TPicture.Create;
  rStream := TResourceStream.Create(HInstance, 'JpgImage_1', RT_RCDATA);
  try
    rStream.Position := 0;

    CheckTrue(rStream.Size > 0, 'Cannot load image stream from resources');

    pic.LoadFromStreamSmart(rStream);

    CheckTrue(pic.Graphic is TJPEGImage);

    CheckEquals(800, pic.Width);
    CheckEquals(600, pic.Height);
  finally
    pic.Free;
    rStream.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestHelpers.Suite);

end.
