program Demo.Spring.Coroutines;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Coroutines,
  Spring.Collections.Sequences;

procedure Fibonacci(const n: Integer);
var
  a, b, c: UInt64;
  i: Integer;
begin
  a := 0;
  b := 1;
  for i := 0 to n do
  begin
    c := a;
    a := b;
    b := c + b;
    Yield(c);
  end;
end;

procedure Main;
var
  fibo1: IEnumerable<UInt64>;
  fibo2: Func<Integer,IEnumerable<UInt64>>;
  i: UInt64;
  foo: Func<Integer,Integer>;
  foo2: Func<Integer>;
begin
  fibo1 := TSequence<Integer, UInt64>.Create(Fibonacci).Bind(10);
  for i in fibo1 do
    Write(i, ' ');
  Writeln;
  fibo2 := TSequence<Integer, UInt64>.Create(Fibonacci);
  for i in fibo2(10) do
    Write(i, ' ');
  Writeln;

  foo := TCoroutine<Integer,Integer>.Create(
    procedure(const a: Integer)
    begin
      Yield(a);
      Yield(a + 1);
      Yield(a + 2);
    end);

  Writeln(foo(1));
  Writeln(foo(2));
  Writeln(foo(1));
  Writeln(foo(2));

  foo2 := TCoroutine<Integer,Integer>.Create(
    procedure(const a: Integer)
    begin
      Yield(a);
      Yield(a + 1);
      Yield(a + 2);
    end).Bind(1);

  Writeln(foo2);
  Writeln(foo2);
  Writeln(foo2);
  Writeln(foo2);
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
  ReportMemoryLeaksOnShutdown := True;
end.
