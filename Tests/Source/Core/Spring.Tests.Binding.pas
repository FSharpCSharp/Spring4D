{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Tests.Binding;

interface

uses
  Classes,
  SysUtils,
  Forms,
  StdCtrls,
  TestFramework,
  Spring.System,
  Spring.Reflection,
  Spring.Collections,
  Spring.Notifications,
  Spring.Binding;

type
  {$REGION 'Business Objects'}

  TSimpleAddress = class;
  
  TSimplePerson = class(TNotifiableObject)
  private
    fName: string;
    fAge: Integer;
    fAddress: TSimpleAddress;
    procedure SetName(const Value: string);
    procedure SetAge(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read fName write SetName;
    property Age: Integer read fAge write SetAge;
    property Address: TSimpleAddress read fAddress;
  end;

  TSimpleAddress = class(TNotifiableObject)
  private
    fCity: string;
    procedure SetCity(const Value: string);
  public
    property City: string read fCity write SetCity;
  end;

  TAddress = class;

  TPerson = class(TNotifiableObject)
  private
    fName: TNullableString;
    fAge: TNullableInteger;
    fAddress: TAddress;
    fAddresses: IList<TAddress>;
    procedure SetName(const Value: TNullableString);
    procedure SetAge(const Value: TNullableInteger);
  public
    constructor Create;
    destructor Destroy; override;
    property Name: TNullableString read fName write SetName;
    property Age: TNullableInteger read fAge write SetAge;
    property Address: TAddress read fAddress;
    property Addresses: IList<TAddress> read fAddresses;
  end;

  TAddress = class(TNotifiableObject)
  private
    fCity: TNullableString;
    procedure SetCity(const Value: TNullableString);
  public
    property City: TNullableString read fCity write SetCity;
  end;

  TFriendlyAddress = class(TAddress)
  public
    function ToString: string; override;
  end;

  {$ENDREGION}


  TBindingTestCase = class(TTestCase)
  protected
    fContext: IBindingContext;
    fForm: TComponent;
    fNameEdit: TEdit;
    fAgeEdit: TEdit;
    fCityEdit: TEdit;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestBindSimpleProperty = class(TBindingTestCase)
  private
    fPerson: TSimplePerson;
//    fNameBinding: IBinding;
//    fAgeBinding: IBinding;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameEditText;
    procedure TestAgeEditText;
    procedure TestCityEditText;
    procedure TestNamePropertyChanged;
    procedure TestAgePropertyChanged;
    procedure TestCityPropertyChanged;
  end;

  TTestBindNullableProperty = class(TBindingTestCase)
  private
    fPerson: TPerson;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameEditText;
    procedure TestAgeEditText;
    procedure TestCityEditText;
    procedure TestNamePropertyChanged;
    procedure TestAgePropertyChanged;
    procedure TestCityPropertyChanged;
  end;

  // Without data template
  TTestBindIList = class(TTestCase)
  private
    fContext: IBindingContext;
    fForm: TForm;
    fListBox: TListBox;
    fPerson: TPerson;
  protected
    procedure CheckItems; virtual;
    procedure InitializeBindings; virtual;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialItems;
    procedure TestAddItem;
    procedure TestRemoveItem;
    procedure TestInsertItem;
    procedure TestClearItem;
  end;

  TTestSimpleDataTemplate = class(TTestBindIList)
  protected
    procedure CheckItems; override;
    procedure InitializeBindings; override;
  published
    procedure TestItemChanged;
  end;

implementation

{ TSimplePerson }

constructor TSimplePerson.Create;
begin
  inherited Create;
  fAddress := TSimpleAddress.Create(Self, 'Address');
end;

destructor TSimplePerson.Destroy;
begin
  fAddress.Free;
  inherited Destroy;
end;

procedure TSimplePerson.SetName(const Value: string);
begin
  SetProperty<string>('Name', fName, Value);
end;

procedure TSimplePerson.SetAge(const Value: Integer);
begin
  SetProperty<Integer>('Age', fAge, Value);
end;

{ TAddress }

procedure TSimpleAddress.SetCity(const Value: string);
begin
  SetProperty<string>('City', fCity, value);
end;

{ TAddress }

procedure TAddress.SetCity(const Value: TNullableString);
begin
  SetProperty<TNullableString>('City', fCity, value);
end;

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  fAddress := TAddress.Create(Self, 'Address');
  fAddresses := TObjectNotifiableCollection<TAddress>.Create;
end;

destructor TPerson.Destroy;
begin
  fAddress.Free;
  inherited Destroy;
end;

procedure TPerson.SetName(const Value: TNullableString);
begin
  SetProperty<TNullableString>('Name', fName, Value);
end;

procedure TPerson.SetAge(const Value: TNullableInteger);
begin
  SetProperty<TNullableInteger>('Age', fAge, Value);
end;

{ TFriendlyAddress }

function TFriendlyAddress.ToString: string;
begin
  if City.HasValue then
    Result := City
  else
    Result := inherited ToString;
end;

{ TBindingTestCase }

procedure TBindingTestCase.SetUp;
begin
  inherited;
  fForm := TComponent.Create(nil);
  fContext := TBindingContext.Create(fForm);
  fNameEdit := TEdit.Create(fForm);
  fAgeEdit := TEdit.Create(fForm);
  fCityEdit := TEdit.Create(fForm);

  fNameEdit.Name := 'NameEdit';
  fAgeEdit.Name := 'AgeEdit';
  fCityEdit.Name := 'CityEdit';
end;

procedure TBindingTestCase.TearDown;
begin
  fForm.Free;
  fContext := nil;
  inherited;
end;


{$REGION 'TTestBindSimpleProperty'}

procedure TTestBindSimpleProperty.SetUp;
begin
  inherited;
  fPerson := TSimplePerson.Create;
  fPerson.Name := 'Paul';
  fPerson.Age := 28;
  fPerson.Address.City := 'Shanghai';

  fContext.DataSource := fPerson;
  fContext.AddBinding('NameEdit.Text', 'Name');
  fContext.AddBinding('AgeEdit.Text', 'Age');
  fContext.AddBinding('CityEdit.Text', 'Address.City');
  fContext.IsActive := True;
end;

procedure TTestBindSimpleProperty.TearDown;
begin
//  fNameBinding := nil;
//  fAgeBinding := nil;
  fPerson.Free;
  inherited;
end;

procedure TTestBindSimpleProperty.TestNameEditText;
begin
  CheckEquals(fPerson.Name, fNameEdit.Text);
end;

procedure TTestBindSimpleProperty.TestAgeEditText;
begin
  CheckEquals(IntToStr(fPerson.Age), fAgeEdit.Text);
end;

procedure TTestBindSimpleProperty.TestCityEditText;
begin
  CheckEquals(fPerson.Address.City, fCityEdit.Text);
end;

procedure TTestBindSimpleProperty.TestNamePropertyChanged;
var
  newName: string;
begin
  newName := 'New Paul';
  fPerson.Name := newName;
  CheckEquals(newName, fNameEdit.Text);
end;

procedure TTestBindSimpleProperty.TestAgePropertyChanged;
var
  newAge: Integer;
begin
  newAge := fPerson.Age + 1;
  fPerson.Age := newAge;
  CheckEquals(IntToStr(newAge), fAgeEdit.Text);
end;

procedure TTestBindSimpleProperty.TestCityPropertyChanged;
var
  newCity: string;
begin
  newCity := 'New ' + fPerson.Address.City;
  fPerson.Address.City := newCity;
  CheckEquals(fPerson.Address.City, fCityEdit.Text);
end;

{$ENDREGION}


{$REGION 'TTestBindNullableProperty'}

procedure TTestBindNullableProperty.SetUp;
begin
  inherited;
  fPerson := TPerson.Create;
  fPerson.Name := 'Paul';
  fPerson.Age := 28;
  fPerson.Address.City := 'Shanghai';

  fContext.DataSource := fPerson;
  fContext.AddBinding('NameEdit.Text', 'Name');
  fContext.AddBinding('AgeEdit.Text', 'Age');
  fContext.AddBinding('CityEdit.Text', 'Address.City');

  fContext.IsActive := True;
end;

procedure TTestBindNullableProperty.TearDown;
begin
  fPerson.Free;
  inherited;
end;

procedure TTestBindNullableProperty.TestNameEditText;
begin
  CheckEquals(fPerson.Name, fNameEdit.Text);
end;

procedure TTestBindNullableProperty.TestAgeEditText;
begin
  CheckEquals(IntToStr(fPerson.Age), fAgeEdit.Text);
end;

procedure TTestBindNullableProperty.TestCityEditText;
begin
  CheckEquals(fPerson.Address.City, fCityEdit.Text);
end;

procedure TTestBindNullableProperty.TestNamePropertyChanged;
var
  newName: string;
begin
  newName := 'New Paul';
  fPerson.Name := newName;
  CheckEquals(newName, fNameEdit.Text);
end;

procedure TTestBindNullableProperty.TestAgePropertyChanged;
var
  newAge: Integer;
begin
  newAge := fPerson.Age.Value + 1;
  fPerson.Age := newAge;
  CheckEquals(IntToStr(newAge), fAgeEdit.Text);
end;

procedure TTestBindNullableProperty.TestCityPropertyChanged;
var
  newCity: string;
begin
  newCity := 'New ' + fPerson.Address.City;
  fPerson.Address.City := newCity;
  CheckEquals(fPerson.Address.City, fCityEdit.Text);
end;

{$ENDREGION}


{$REGION 'TTestBindIList'}

procedure TTestBindIList.InitializeBindings;
begin
  fContext.AddBinding('AddressListBox.Items', 'Addresses');
end;

procedure TTestBindIList.SetUp;
var
  fAddress: TAddress;
  fFriendlyAddress: TAddress;
begin
  inherited;
  fForm := TForm.Create(nil);
  fContext := TBindingContext.Create(fForm);
  fListBox := TListBox.Create(fForm);
  fPerson := TPerson.Create;
  fListBox.Name := 'AddressListBox';
  fListBox.Parent := fForm;
  fAddress := TAddress.Create;
  fAddress.City := 'Shanghai';
  fFriendlyAddress := TFriendlyAddress.Create;
  fFriendlyAddress.City := 'Anhui';
  fPerson.Addresses.Add(fAddress);
  fPerson.Addresses.Add(fFriendlyAddress);
  fContext.DataSource := fPerson;
  InitializeBindings;
  fContext.IsActive := True;
end;

procedure TTestBindIList.TearDown;
begin
  fForm.Free;
  fContext := nil;
  fPerson.Free;
  inherited;
end;

procedure TTestBindIList.CheckItems;
var
  expectedName: string;
  i: Integer;
begin
  CheckEquals(fPerson.Addresses.Count, fListBox.Items.Count);
  for i := 0 to fListBox.Count - 1 do
  begin
    expectedName := fPerson.Addresses[i].ToString;
    CheckEquals(expectedName, fListBox.Items[i]);
    CheckSame(fPerson.Addresses[i], fListBox.Items.Objects[i]);
  end;
end;

procedure TTestBindIList.TestInitialItems;
begin
  CheckItems;
end;

procedure TTestBindIList.TestAddItem;
var
  address: TAddress;
begin
  address := TAddress.Create;
  address.City := 'New York';
  fPerson.Addresses.Add(address);
  CheckItems;
end;

procedure TTestBindIList.TestRemoveItem;
begin
  fPerson.Addresses.RemoveAt(1);
  CheckItems;
  fPerson.Addresses.RemoveAt(0);
  CheckItems;
end;

procedure TTestBindIList.TestInsertItem;
var
  address: TAddress;
begin
  address := TAddress.Create;
  address.City := 'New York';
  fPerson.Addresses.Insert(0, address);
  CheckItems;
end;

procedure TTestBindIList.TestClearItem;
begin
  fPerson.Addresses.Clear;
  CheckItems;
end;

{$ENDREGION}


{ TTestSimpleDataTemplate }

procedure TTestSimpleDataTemplate.InitializeBindings;
begin
  fContext.AddBinding('AddressListBox.Items', 'Addresses').Columns.Add('City');
end;

procedure TTestSimpleDataTemplate.CheckItems;
var
  expectedString: string;
  i: Integer;
begin
  CheckEquals(fPerson.Addresses.Count, fListBox.Items.Count);
  for i := 0 to fListBox.Count - 1 do
  begin
    expectedString := fPerson.Addresses[i].City;
    CheckEquals(expectedString, fListBox.Items[i]);
    CheckSame(fPerson.Addresses[i], fListBox.Items.Objects[i]);
  end;
end;

procedure TTestSimpleDataTemplate.TestItemChanged;
begin
  fPerson.Addresses.First.City := 'City-A';
  CheckItems;
  fPerson.Addresses.Last.City := 'City-B';
  CheckItems;
end;

end.
