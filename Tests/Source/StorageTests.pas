unit StorageTests;

interface

uses
  TestFramework,
  Storage,
  StorageFactory;

type
  TStorageTests = class(TTestCase)
  private
    fPrimaryStorage: TPrimaryStorage;
    fSecondaryStorage: TSecondaryStorage;
    fSUT: TStorageFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Save_should_use_primaryStorage_when_it_is_up;
    procedure Save_should_use_secondaryStorage_when_primaryStorage_is_down;
    procedure Save_should_go_back_to_primaryStorage_when_is_goes_from_down_to_up;
  end;

implementation

uses
  Spring.Collections,
  Spring.Interception,
  Interfaces;

{ TStorageTests }

procedure TStorageTests.SetUp;
begin
  fPrimaryStorage := TPrimaryStorage.Create;
  fPrimaryStorage.IsUp := True;
  fSecondaryStorage := TSecondaryStorage.Create;
  fSUT := TStorageFactory.Create(fPrimaryStorage);
  fSUT.SecondaryStorage := fSecondaryStorage;
end;

procedure TStorageTests.TearDown;
begin
  fSUT.Free;
end;

procedure TStorageTests.Save_should_use_primaryStorage_when_it_is_up;
var
  storage: IStorage;
  msg: string;
begin
  storage := fSUT.GetStorage;
  msg := 'message';
  storage.Save(msg);

  CheckTrue(fSecondaryStorage.Items.IsEmpty);
  CheckFalse(fPrimaryStorage.Items.IsEmpty);
  CheckEquals(msg, fPrimaryStorage.Items.First);
end;

procedure TStorageTests.Save_should_use_secondaryStorage_when_primaryStorage_is_down;
var
  storage: IStorage;
  msg: string;
begin
  fPrimaryStorage.IsUp := False;
  storage := fSUT.GetStorage;
  msg := 'message';
  storage.Save(msg);

  CheckTrue(fPrimaryStorage.Items.IsEmpty);
  CheckFalse(fSecondaryStorage.Items.IsEmpty);
  CheckEquals(msg, fSecondaryStorage.Items.First);
end;

procedure TStorageTests.Save_should_go_back_to_primaryStorage_when_is_goes_from_down_to_up;
var
  storage: IStorage;
  msg1, msg2, msg3: string;
  primary, secondary: IList<string>;
begin
  msg1 := 'message1';
  msg2 := 'message2';
  msg3 := 'message3';

  storage.Save(msg1);
  fPrimaryStorage.IsUp := False;
  storage.Save(msg2);
  fPrimaryStorage.IsUp := True;
  storage.Save(msg3);
  primary := fPrimaryStorage.Items;
  secondary := fSecondaryStorage.Items;

  CheckEquals(2, primary.Count);
  CheckEquals(1, secondary.Count);
  CheckTrue(primary.Contains(msg1));
  CheckTrue(primary.Contains(msg3));
  CheckTrue(secondary.Contains(msg2));
end;

initialization
  RegisterTest(TStorageTests.Suite);

end.
