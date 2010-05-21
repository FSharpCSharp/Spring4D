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

program Numbering;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Spring,
  Spring.Numbering;

type
  /// <summary>
  /// Represents a mock number souce which might be a file or a database table.
  /// </summary>
  TMockNumberSouce = class(TInterfacedObject, INumberSource)
  private
    fLastNumber: string;
  public
    constructor Create(const lastNumber: string);
    procedure UpdateNumber(proc: TNumberProc);
    property LastNumber: string read fLastNumber;
  end;

{ TMockNumberSouce }

constructor TMockNumberSouce.Create(const lastNumber: string);
begin
  inherited Create;
  fLastNumber := lastNumber;
end;

procedure TMockNumberSouce.UpdateNumber(proc: TNumberProc);
begin
  TArgument.CheckNotNull(Assigned(proc), 'proc');
  proc(fLastNumber);
end;

//---------------------------------------------------------------------------

function CreateNumberRule: INumberRule;
var
  builder: TNumberRuleBuilder;
begin
  builder := TNumberRuleBuilder.Create;
  builder.AddCode('RK');
  builder.AddDateTime('YYYYMM');
  builder.AddDigits('000000', '999999');
  Result := builder.ToRule;
end;

//---------------------------------------------------------------------------

var
  rule: INumberRule;
  source: INumberSource;
  generator: INumberGenerator;
  i: Integer;
begin
  try
    rule := CreateNumberRule;
    source := TMockNumberSouce.Create('RK200912000001');
    generator := TNumberGenerator.Create(rule, source);
    for i := 0 to 10 do
    begin
      Writeln(generator.NextNumber);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Writeln('Press Enter to quit.');
  Readln;
end.
