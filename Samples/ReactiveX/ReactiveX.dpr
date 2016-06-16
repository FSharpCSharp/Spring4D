  program ReactiveX;

{$APPTYPE CONSOLE}

// examples from http://www.introtorx.com/

uses
  Classes,
  SysUtils,
  TimeSpan,
  Example1 in 'Example1.pas',
  Example2 in 'Example2.pas',
  Example3 in 'Example3.pas',
  Example4 in 'Example4.pas',
  Example5 in 'Example5.pas',
  Example6 in 'Example6.pas',
  Example7 in 'Example7.pas',
  Test1 in 'Test1.pas',
  Test2 in 'Test2.pas',
  Spring.Collections,
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Concurrency.DefaultScheduler;

procedure WriteLine(const value: Integer); overload;
begin
  Writeln(value);
end;

procedure WriteLine(const value: string); overload;
begin
  Writeln(value);
end;

procedure Main;
var
  sub: IDisposable;
  range: IObservable<Integer>;
  numbers: IDisposable;

  subject: ISubject<Integer>;
  distinct: IObservable<Integer>;
  noElements: IObservable<Integer>;
  s: IScheduler;
  work: Action<Action>;
  token: IDisposable;

begin
//  Observable.Interval(TTimeSpan.FromMilliseconds(150))
//    .Sample(TTimeSpan.FromSeconds(1))
//    .Subscribe(WriteLine);
//
//  Readln;

  Enumerable.ToObservable<Integer>(TEnumerable.Range(1, 100)).Subscribe(
    WriteLine);
  Readln;


//  Example1.Main;
//  Example2.Main;
//  Example3.Main;
//  Example4.Main;
//  Example5.Main;
//  Example6.Main;
//  Example7.Main;
//  Exit;

//  // Example 8
//  sub := Observable.Return('Value').Subscribe(Console.WriteLine);
//  sub := nil;
//
//  // Example 9
//  Observable.Empty<string>.Subscribe(Console.WriteLine,
//    procedure
//    begin
//      Writeln('completed');
//    end);
//
//  // Example 10
//  Observable.Never<string>;
//
//  // Example 11
//  Observable.Throw<string>(Exception.Create('some error')).Subscribe(Console.WriteLine,
//    procedure(const e: Exception)
//    begin
//      Writeln(e.ClassName, ': ', e.Message);
//    end);

//  // Example 12
//  Observable.Create<string>(
//    function(const observer: IObserver<string>): IDisposable
//    begin
//      observer.OnNext('a');
//      observer.OnNext('b');
//      observer.OnCompleted;
//      Sleep(1000);
//      Result := Disposable.Create(
//        procedure
//        begin
//          Writeln('Observer has unsubscribed');
//        end)
//    end).Subscribe(Console.WriteLine);

//  range := Observable.Range(10, 15);
//  range.Subscribe(
//    procedure(const i: Integer)
//    begin
//      Writeln(i);
//    end,
//    procedure
//    begin
//      Writeln('Completed');
//    end);

//  numbers := Observable.Range(0, 10)
//    .Where(function(const i: Integer): Boolean begin Result := i mod 2 = 0 end)
//    .Subscribe(procedure(const i: Integer) begin Writeln(i) end,
//    procedure begin Writeln('Completed') end);

//  subject := TSubject<Integer>.Create;
//  distinct := subject.Distinct;
//  subject.Subscribe(
//    procedure(const i: Integer)
//    begin
//      Writeln(i);
//    end,
//    procedure
//    begin
//      Writeln('subject.OnCompleted()');
//    end);
//  distinct.Subscribe(
//    procedure(const i: Integer)
//    begin
//      WritelnFmt('distinct.OnNext(%d)', [i]);
//    end,
//    procedure
//    begin
//      Writeln('distinct.OnCompleted()');
//    end);
//  subject.OnNext(1);
//  subject.OnNext(2);
//  subject.OnNext(3);
//  subject.OnNext(1);
//  subject.OnNext(1);
//  subject.OnNext(4);
//  subject.OnCompleted();

//  subject := TSubject<Integer>.Create;
//  distinct := subject.DistinctUntilChanged;
//  subject.Subscribe(
//    procedure(const i: Integer)
//    begin
//      Writeln(i);
//    end,
//    procedure
//    begin
//      Writeln('subject.OnCompleted()');
//    end);
//  distinct.Subscribe(
//    procedure(const i: Integer)
//    begin
//      WritelnFmt('distinct.OnNext(%d)', [i]);
//    end,
//    procedure
//    begin
//      Writeln('distinct.OnCompleted()');
//    end);
//  subject.OnNext(1);
//  subject.OnNext(2);
//  subject.OnNext(3);
//  subject.OnNext(1);
//  subject.OnNext(1);
//  subject.OnNext(4);
//  subject.OnCompleted();

//  subject := TSubject<Integer>.Create;
//  noElements := subject.IgnoreElements;
//  subject.Subscribe(
//    procedure(const i: Integer)
//    begin
//      WritelnFmt('subject.OnNext(%d)', [i]);
//    end,
//    procedure
//    begin
//      Writeln('subject.OnCompleted()');
//    end);
//  noElements.Subscribe(
//    procedure(const i: Integer)
//    begin
//      WritelnFmt('noElements.OnNext(%d)', [i]);
//    end,
//    procedure
//    begin
//      Writeln('noElements.OnCompleted()');
//    end);
//  subject.OnNext(1);
//  subject.OnNext(2);
//  subject.OnNext(3);
//  subject.OnCompleted();

//  Observable.Range(0, 10)
//    .Skip(3)
//    .Subscribe(Console.WriteLine<Integer>, procedure begin Writeln('Completed') end);
//
//  Observable.Range(0, 10)
//    .Take(3)
//    .Subscribe(Console.WriteLine<Integer>, procedure begin Writeln('Completed') end);

//  Observable.Interval(TTimeSpan.FromSeconds(1))
//    .Where(function(const n: Integer): Boolean begin Result := n < 5 end)
//    .Subscribe(Console.WriteLine<Integer>, procedure begin Writeln('Completed') end);
//  Readln;

  //!!!TODO!!!
//  Enumerable.ToObservable<Integer>(TCollections.CreateList<Integer>([1, 2, 3, 4, 5, 6, 7, 8, 9]))
//    .Subscribe(Console.WriteLine<Integer>);


//  s := TDefaultScheduler.Create;
//  work :=
//    procedure(const self: Action)
//    begin
//      Writeln('Running on ', TThread.CurrentThread.ThreadID);
//      self();
//    end;
//  token := s.Schedule(work);
//  Readln;
//  Writeln('Cancelling');
//  token.Dispose;
//  Writeln('Cancelled');
end;

begin
  try
    Main;
//    Writeln('ended');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReportMemoryLeaksOnShutdown := True;
//  Readln;
end.
