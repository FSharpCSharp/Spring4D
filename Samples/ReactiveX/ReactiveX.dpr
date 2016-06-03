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
  Spring.Console in '..\..\Source\Reactive\Spring.Console.pas',
  Spring.Reactive in '..\..\Source\Reactive\Spring.Reactive.pas',
  Spring.Reactive.AnonymousObservable in '..\..\Source\Reactive\Spring.Reactive.AnonymousObservable.pas',
  Spring.Reactive.AnonymousObserver in '..\..\Source\Reactive\Spring.Reactive.AnonymousObserver.pas',
  Spring.Reactive.Disposables in '..\..\Source\Reactive\Spring.Reactive.Disposables.pas',
  Spring.Reactive.Observable.Empty in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Empty.pas',
  Spring.Reactive.Observable.Never in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Never.pas',
  Spring.Reactive.Observable.Return in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Return.pas',
  Spring.Reactive.Observable.Throw in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Throw.pas',
  Spring.Reactive.ObservableBase in '..\..\Source\Reactive\Spring.Reactive.ObservableBase.pas',
  Spring.Reactive.Observers in '..\..\Source\Reactive\Spring.Reactive.Observers.pas',
  Spring.Reactive.Producer in '..\..\Source\Reactive\Spring.Reactive.Producer.pas',
  Spring.Reactive.Sink in '..\..\Source\Reactive\Spring.Reactive.Sink.pas',
  Spring.Reactive.Stubs in '..\..\Source\Reactive\Spring.Reactive.Stubs.pas',
  Spring.Reactive.Subjects.AsyncSubject in '..\..\Source\Reactive\Spring.Reactive.Subjects.AsyncSubject.pas',
  Spring.Reactive.Subjects.BehaviorSubject in '..\..\Source\Reactive\Spring.Reactive.Subjects.BehaviorSubject.pas',
  Spring.Reactive.Subjects.ReplaySubject in '..\..\Source\Reactive\Spring.Reactive.Subjects.ReplaySubject.pas',
  Spring.Reactive.Subjects.Subject in '..\..\Source\Reactive\Spring.Reactive.Subjects.Subject.pas',
  Spring.Reactive.Subjects.SubjectBase in '..\..\Source\Reactive\Spring.Reactive.Subjects.SubjectBase.pas',
  Spring.Reactive.TimeInterval in '..\..\Source\Reactive\Spring.Reactive.TimeInterval.pas',
  Spring.Reactive.Observable.Range in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Range.pas',
  Spring.Reactive.Observable.Where in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Where.pas',
  Spring.Reactive.Observable.Distinct in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Distinct.pas',
  Spring.Reactive.Observable.DistinctUntilChanged in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.DistinctUntilChanged.pas',
  Spring.Reactive.Observable.IgnoreElements in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.IgnoreElements.pas',
  Spring.Reactive.Observable.Skip in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Skip.pas',
  Spring.Reactive.Observable.Take in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Take.pas',
  Spring.Reactive.Concurrency.DefaultScheduler in '..\..\Source\Reactive\Concurrency\Spring.Reactive.Concurrency.DefaultScheduler.pas',
  Spring.Reactive.Concurrency.LocalScheduler in '..\..\Source\Reactive\Concurrency\Spring.Reactive.Concurrency.LocalScheduler.pas',
  Spring.Reactive.Concurrency.ImmediateScheduler in '..\..\Source\Reactive\Concurrency\Spring.Reactive.Concurrency.ImmediateScheduler.pas',
  Spring.Reactive.Concurrency.SchedulerDefaults in '..\..\Source\Reactive\Concurrency\Spring.Reactive.Concurrency.SchedulerDefaults.pas',
  Spring.Reactive.Observable.Timer in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.Timer.pas',
  Spring.Reactive.Observable.ToObservable in '..\..\Source\Reactive\Observable\Spring.Reactive.Observable.ToObservable.pas';

procedure Main;
var
  sub: IDisposable;
  range: IObservable<Integer>;
  numbers: IDisposable;

  subject: ISubject<Integer>;
  distinct: IObservable<Integer>;
  noElements: IObservable<Integer>;
begin
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

  Enumerable.ToObservable<Integer>(TCollections.CreateList<Integer>([1, 2, 3, 4, 5, 6, 7, 8, 9]))
    .Subscribe(Console.WriteLine<Integer>);
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReportMemoryLeaksOnShutdown := True;
  Readln;
end.
