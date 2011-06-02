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

/// <summary>
/// This program is only for experimental use. Please add it to Ignored list.
/// </summary>
program Spike;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,   Windows,
  StrUtils,
  Spring,
  Spring.Collections,
  Spring.Utils;


type
  TRec = record
    f: Int64;
    s: string;
//    e: Double;
  end;

  TTest = class
  private
    fRec: TRec;
  public
//    property V: Integer read fRec.Value;
  end;

  procedure Test;
  var
    r: TRec;
    v: TGuid;
  begin
    ZeroMemory(@r, SizeOf(r));
  end;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    Test();
//    TLinq.From(TStrings)
//    TLinq.From(TArray[])
//    TLinq.From(TObjectList)
//    TLinq.From(TCollection)
//    TLinq.From<T>(IEnumerable<T>)
//    TLinq.From<T>(TEnumerable<T>)
//    TLinq.From<T>(TArray<T>)
//    TLinq.From<string>(strings).Skip(3).Select<string>(selector);

//    strings := TList<string>.Create;
//    strings.AddRange(['a', 'aba', 'ced']);
//
//    for s in strings.Where(TStringMatchers.InArray(['a', 'aba'])) do
//    begin
//      Writeln(s);
//    end;

    (*

    list := TList<Integer>.Create;
    list2 := TList<Integer>.Create;
    list.AddRange([3, 5, 12, 14]);
    list2.AddRange([9,50]);

//    p := function (value, index: Integer): Boolean
//      begin
//        Result := value <= 5;
//      end;

    r := list.Reversed;
    for element in r do
    begin
      Writeln(element);
    end;
    //*)
//    Readln(s);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
