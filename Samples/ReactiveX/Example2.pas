unit Example2;

interface

// introduction to the subject type

procedure Main;

implementation

uses
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Subjects.Subject;

procedure Main;
var
  subject: ISubject<string>;
begin
  subject := TSubject<string>.Create;
  subject.Subscribe(Console.WriteLine);
  subject.OnNext('a');
  subject.OnNext('b');
  subject.OnNext('c');
end;

end.
