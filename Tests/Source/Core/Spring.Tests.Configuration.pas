{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.spring4d.org                                         }
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

unit Spring.Tests.Configuration;

interface

uses
  SysUtils,
  TestFramework,
  Spring.Configuration,
  Spring.UnitTests;

type
  TTestConfiguration = class(TTestCase)
  private
    fSource: IConfigurationSource;
    fConfiguration: IConfiguration;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConfigurationProperty;
    procedure TestSection;
    procedure TestSectionAttribute;
  end;

implementation

uses
  Rtti,
  Spring.Configuration.Node,
  Spring.Configuration.Sources,
  Spring.Configuration.ConfigurationProperty;

{ TTestConfiguration }

procedure TTestConfiguration.SetUp;
begin
  inherited;
  fSource := TXmlConfigurationSource.Create('Spring.Tests.xml');
end;

procedure TTestConfiguration.TearDown;
begin
  fSource := nil;
  fConfiguration := nil;
  inherited TearDown;
end;

procedure TTestConfiguration.TestConfigurationProperty;
var
  flag: boolean;
  item1, item2: IConfiguration;
begin
  fConfiguration := fSource.GetConfiguration;

  //string
  fConfiguration.Properties['key'] := 'as';
  CheckEquals('as', fConfiguration.Properties['key'], 'String assignment error');

  //integer
  fConfiguration.Properties['key'] := 5;
  CheckEquals(5, fConfiguration.Properties['key'], 'Integer assignment error');

  //extended
  fConfiguration.Properties['key'] := 3.14;
  CheckEquals(3.14, fConfiguration.Properties['key'], 'Extended assignment error');

  //boolean
  fConfiguration.Properties['key'] := false;
  CheckEquals(false, fConfiguration.Properties['key'], 'Boolean assignment error');

  fConfiguration.Properties['key'].Validator := procedure(value: TValue)
                                                begin
                                                  if value.IsType<string> then
                                                    raise Exception.Create('Value validated: unexpected string found');
                                                end;

  flag := false;
  try
    fConfiguration.Properties['key'] := 'as';
  except
    flag := true;
  end;
  Check(flag, 'Validator error');
  fConfiguration.Properties['key'].Validator := nil;

  fConfiguration.Properties['key'].Clear;
  Check(fConfiguration.Properties['key'].Value.IsEmpty, 'Clearing error');

  fConfiguration.Properties['key'].DefaultValue := 'as';
  CheckEquals('as', fConfiguration.Properties['key'], 'Default value error');
  fConfiguration.Properties['key'].DefaultValue := TValue.Empty;

  fConfiguration.Properties['key'] := TConfigurationProperty.Empty;
  Check(fConfiguration.Properties['key'].Value.IsEmpty, 'Emptying error');

  fConfiguration.Properties['key'] := 'as';
  item1 := fConfiguration.AddChild;
  CheckEquals('as', item1.Properties['key'], 'Inheritancy error (lack of value)');
  item1.Properties['key'] := 'sa';
  CheckEquals('sa', item1.Properties['key'], 'Inheritancy error (not inherited value)');
  item1.Properties['key'].Clear;
  CheckEquals('as', item1.Properties['key'], 'Inheritancy error (value exist, but is empty)');
  item2 := item1.AddChild;
  CheckEquals('as', item2.Properties['key'], 'Inheritancy error (two levels)');

  item1.Children.Remove(item2);
  Check(item1.Children.IsEmpty, 'Removing error (single item)');
  fConfiguration.Children.Remove(function(const value: IConfiguration): boolean begin Result := true; end);
  Check(fConfiguration.Children.IsEmpty, 'Removing error (predicate)');
end;

procedure TTestConfiguration.TestSection;
begin
  fConfiguration := fSource.GetConfiguration;
  CheckEquals('logging', fConfiguration.GetChild('logging').Name);
end;

procedure TTestConfiguration.TestSectionAttribute;
var
  value: TConfigurationProperty;
begin
  fConfiguration := fSource.GetConfiguration;
  CheckTrue(fConfiguration.GetChild('logging').TryGetProperty('debug', value));
  CheckEquals('true', value);

  fConfiguration.GetChild('logging').Properties['debug'] := 'false';
  value := fConfiguration.GetChild('logging').Properties['debug'];
  CheckEquals('false', value);
end;

end.
