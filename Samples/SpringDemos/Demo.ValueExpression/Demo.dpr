program Demo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RTTI,
  uSomeClass in 'uSomeClass.pas';

var
  VE: IValueExpression;
  SomeClass: TSomeClass;
  AnotherSomeClass: TSomeClass;
  ThirdSomeClass: TSomeClass;
  TempValue: TValue;
  SomeObj: TObject;


begin
  try
    SomeClass := TSomeClass.Create;
    try
      VE := TValueExpression.Create(SomeClass);
      VE.Follow('FPrivateInt').SetValue(9999);
      WriteLn(VE.Follow('FPrivateInt').Value.AsInteger);
      WriteLn(SomeClass.PrivateInt);

      VE.Follow('PublicString').SetValue('This is public');
      WriteLn(SomeClass.PublicString);

      if VE.Value.TryAsType<TSomeClass>(SomeClass) then
      begin
        WriteLn('Yay!! It really is a TSomeClass!');
      end else
      begin
        WriteLn('Booooo!  It''s not a TSomeClass');
      end;

    finally
      SomeClass.Free;
    end;

    TempValue := TValue.From<TSomeClass>(AnotherSomeClass);
    if TempValue.TryAsType<TSomeClass>(ThirdSomeClass) then
    begin
      WriteLn('Woohoo!');
    end else
    begin
      WriteLn('MASSIVE FAIL!');
    end;

    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
