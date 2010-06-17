{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
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

unit Spring.Tests.ValueConverters;

interface

uses
  TestFramework,
  TestExtensions,
  Rtti,
  Spring.ValueConverters;

type
  TTestValueConverters = class(TTestCase)
  private
    fConverter: IValueConverter;
  protected
    procedure SetUp; override;
  published
    procedure TestIntegerToString;
    procedure TestStringToInteger;
  end;

implementation

{$REGION 'TTestValueConverters'}

procedure TTestValueConverters.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestValueConverters.TestIntegerToString;
var
  outValue: TValue;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(string), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckEquals(outValue.AsString, '1');
end;

procedure TTestValueConverters.TestStringToInteger;
var
  outValue: TValue;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Integer), TValue.Empty);
  CheckFalse(outValue.IsEmpty);
  CheckEquals(outValue.AsInteger, 1);
end;

{$ENDREGION}

end.
