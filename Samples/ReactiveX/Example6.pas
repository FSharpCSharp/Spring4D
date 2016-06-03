unit Example6;

interface

// introduction to the behavior subject

procedure Main;

implementation

uses
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Subjects.BehaviorSubject;

procedure Main;
var
  subject: ISubject<string>;
begin
  subject := TBehaviorSubject<string>.Create('a');
  subject.OnNext('b');
  subject.OnNext('c');
  subject.OnCompleted;
  subject.Subscribe(Console.WriteLine);
end;

end.
