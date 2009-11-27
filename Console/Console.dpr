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

{ This project is used for personal test purpose. }

program Console;

{$APPTYPE CONSOLE}

{$WEAKLINKRTTI OFF}

uses
  FastMM4,
  Classes,
  Controls,
  Windows,
  IOUtils,
  SysUtils,
  Rtti,
  TypInfo,
  SyncObjs,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Reflection,
  Spring.Helpers,
  Spring.IoC;

type
  IA = interface
    ['{CE7F702E-76CD-4667-A5BE-0F5B1D5B1D12}']
    procedure DoSomething;
  end;

  IB = interface(IA)
    ['{65403FE3-9EF2-4AD2-984F-F138E232D60F}']
    procedure DoSomething;
  end;

  TA = class(TInterfacedObject, IA, IInterface)
    procedure DoSomething; virtual; abstract;
  end;

  TB = class(TInterfacedObject, IA, IB, IInterface)
    procedure DoSomething; virtual; abstract;
  end;

var
  aType: TRttiType;
  method: TRttiMethod;
  container: TContainer;
  args: TArray<Integer>;

procedure Test;
var
  list: IList<Integer>;
  collection: IEnumerableEx<Integer>;
  obj: TInterfacedObject;
  item: Integer;
begin
  list := TCollections.CreateList<Integer>;
  list.Add(1);
  list.Add(2);
  list.Add(3);
  list.Add(4);
  list.Add(5);
  Writeln(list.Count);
  Writeln(list.First);
  Writeln(list.FirstOrDefault);
  Writeln(list.Last);
  Writeln(list.LastOrDefault);
  collection := list.Where(
    function(value: Integer): Boolean
    begin
      Result := value = 2;
    end
  );
  Writeln(list.Count, TInterfacedObject(list).RefCount);
  obj := TInterfacedObject(collection);
  Writeln(collection.Count, TInterfacedObject(collection).RefCount);
    for item in collection do
    begin
      Writeln(item);
    end;
//  collection := nil;
//  list := nil;
end;

procedure PrintInterfaces(classType: TClass);
var
  table: PInterfaceTable;
  i: Integer;
begin
  table := classType.GetInterfaceTable;
  for i := 0 to table.EntryCount - 1 do
  begin
    Writeln(table.Entries[i].IID.ToString);
  end;
end;

begin
  try
    aType := TRttiContext.Create.GetType(TContainer);
    for method in aType.GetMethods.Where(TMethodFilters.IsInstanceMethod()) do
    begin
      Writeln(method.Parent.Name,  method.ToString);
    end;
    Writeln('----------------------------------------------------');
    method := aType.GetConstructors.First;
    Writeln(method.ToString);
    Test;
    Writeln('----------------------------------------------------');
    PrintInterfaces(TA);
    Writeln('----------------------------------------------------');
    PrintInterfaces(TB);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
