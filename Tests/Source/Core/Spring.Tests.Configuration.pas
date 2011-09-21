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
  TestFramework,
  Spring.Configuration;

type
  TTestConfiguration = class(TTestCase)
  private
    fConfiguration: IConfiguration;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAttribute;
  end;

implementation

uses
  Rtti,
  Spring.Configuration.Adapters;

{ TTestConfiguration }

procedure TTestConfiguration.SetUp;
begin
  inherited;
  fConfiguration := TXmlConfigurationAdapter.Create('Spring.Tests.config');
end;

procedure TTestConfiguration.TearDown;
begin
  inherited TearDown;
end;

procedure TTestConfiguration.TestAttribute;
var
  value: TValue;
begin
  fConfiguration.TryGetAttribute('version', value);
  CheckEquals('1.1.34.568', value.ToString);
end;

end.
