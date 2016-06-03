unit Example4;

interface

// using the replay subject with a limited buffer size

procedure Main;

implementation

uses
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Subjects.ReplaySubject;

procedure Main;
var
  subject: ISubject<string>;
begin
  subject := TReplaySubject<string>.Create(2);
  subject.OnNext('a');
  subject.OnNext('b');
  subject.OnNext('c');
  subject.Subscribe(Console.WriteLine);
  subject.OnNext('d');
end;

end.
