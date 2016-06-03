unit Example5;

interface

// using the replay subject with a timespan

procedure Main;

implementation

uses
  SysUtils,
  Spring,
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Subjects.ReplaySubject;

procedure Main;
var
  window: TTimeSpan;
  subject: ISubject<string>;
begin
  window := TTimeSpan.FromMilliseconds(150);
  subject := TReplaySubject<string>.Create(window);
  subject.OnNext('w');
  Sleep(100);
  subject.OnNext('x');
  Sleep(100);
  subject.OnNext('y');
  subject.Subscribe(Console.WriteLine);
  subject.OnNext('z');
end;

end.
