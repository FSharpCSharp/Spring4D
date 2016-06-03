unit Example3;

interface

// introduction to the replay subject

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
  subject := TReplaySubject<string>.Create;
  subject.OnNext('a');
  subject.Subscribe(Console.WriteLine);
  subject.OnNext('b');
  subject.OnNext('c');
end;

end.
