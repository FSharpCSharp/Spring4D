program CallbackDemo;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils, Windows, Spring.System;

type
  TWindowsEnumerator = class
  private
    fCallback: TCallbackFunc;
    { The class instance method must be declared as "stdcall". }
    procedure AddWindowCaption(handle: THandle; list: TStrings); stdcall;
  public
    procedure GetWindowNames(list: TStrings);
  end;

{ TWindowsEnumerator }

procedure TWindowsEnumerator.AddWindowCaption(handle: THandle; list: TStrings);
var
  caption: array[0..256] of Char;
begin
  if GetWindowText(handle, caption, Length(caption)) > 0 then
  begin
    list.Add(Format('Handle: %8x, Caption: %s', [handle, caption]));
  end;
end;

procedure TWindowsEnumerator.GetWindowNames(list: TStrings);
begin
  CheckArgumentNotNull(list, 'list');
  if not Assigned(fCallback) then
  begin
    fCallback := TCallBack.Create(Self, @TWindowsEnumerator.AddWindowCaption);
  end;
  Windows.EnumWindows(fCallback, lParam(list));
end;

var
  enumerator: TWindowsEnumerator;
  list: TStrings;
begin
  try
    list := TStringList.Create;
    try
      enumerator := TWindowsEnumerator.Create;
      try
        enumerator.GetWindowNames(list);
      finally
        enumerator.Free;
      end;
      Writeln(list.Text);
    finally
      list.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
