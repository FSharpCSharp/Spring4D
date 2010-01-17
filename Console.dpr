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
{$I Spring.inc}

uses
  FastMM4,
  Classes,
  Controls,
  Windows,
  SysUtils,
  Rtti,
  TypInfo,
  SyncObjs,
  ShellAPI,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Reflection,
  Spring.Cryptography,
  Spring.Helpers,
  Spring.IoC,
  Spring.Utils,
  Spring.Utils.IO;

const
  RootPath = 'C:\';
  Pattern = '*.dll';

type
  TPerson = class(TNotifiableObject)
  private
    fName: string;
    fAge: Integer;
    procedure SetName(const Value: string);
    procedure SetAge(const Value: Integer);
  public
    property Name: string read fName write SetName;
    property Age: Integer read fAge write SetAge;
  end;

  TTest = class
  public
    procedure SetProperty<T>(const propertyName: string; const value: T);
  end;

procedure Test;
var
  paul: TPerson;
//  t: TTest;
begin
//  t.SetProperty('a', 'abc');
  paul := TPerson.Create;
  paul.IsPropertyNotificationEnabled := True;
  paul.OnPropertyChanging.AddHandler(
    procedure (sender: TObject; const e: TPropertyNotificationEventArgs)
    begin
      Writeln('Changing#1:' + e.PropertyName);
    end
  ).AddHandler(
    procedure (sender: TObject; const e: TPropertyNotificationEventArgs)
    begin
      Writeln('Changing#2:' + e.PropertyName);
    end
  );
  paul.OnPropertyChanging.AddHandler(
    procedure (sender: TObject; const e: TPropertyNotificationEventArgs)
    begin
      Writeln('Changed#1:' + e.PropertyName);
    end
  ).AddHandler(
    procedure (sender: TObject; const e: TPropertyNotificationEventArgs)
    begin
      Writeln('Changed#2:' + e.PropertyName);
    end
  );
  paul.Name := 'Paul';
  paul.Age := 28;
  paul.BeginUpdate;
  try
    paul.Name := 'Bob';
    paul.BeginUpdate;
    paul.Age := 26;
    paul.EndUpdate;
  finally
    paul.EndUpdate;
  end;
  paul.Free;
end;

{ TPerson }

procedure TPerson.SetName(const Value: string);
begin
  SetProperty('Name', fName, Value);
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  SetProperty('Age', fAge, value);
end;

{ TTest }

procedure TTest.SetProperty<T>(const propertyName: string; const value: T);
begin
  Writeln(propertyName);
end;

begin
  Test;

  (*
  // warm up
  count := 0;
  for entry in TDirectory.FileSystemEntries(RootPath, Pattern, True) do
  begin
    //
    Inc(count);
  end;
  Writeln(count);
  stopwatch := TStopwatch.Create;
  stopwatch.Start;
  for entry in TDirectory.FileSystemEntries(RootPath, Pattern, True) do
  begin
    //
  end;
  stopwatch.Stop;
  Writeln(Format('%.2d:%.3d', [stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds]));
  stopwatch.Reset;
  stopwatch.Start;
  for e2 in EnumerateFileSystemEntries(RootPath, Pattern, True) do
  begin
    //
  end;
  stopwatch.Stop;
  Writeln(Format('%.2d:%.3d', [stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds]));
  stopwatch.Reset;
  stopwatch.Start;
  for s in TDirectory.Entries(RootPath, Pattern, True) do
  begin
    //
  end;
  stopwatch.Stop;
  Writeln(Format('%.2d:%.3d', [stopwatch.Elapsed.Seconds, stopwatch.Elapsed.Milliseconds]));
  //*)
  Readln;
end.
