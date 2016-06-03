unit Example7;

interface

// introduction to the async subject

procedure Main;

implementation

uses
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Subjects.AsyncSubject;

procedure Main;
var
  subject: ISubject<string>;
begin
  subject := TAsyncSubject<string>.Create;
  subject.OnNext('a');
  subject.Subscribe(Console.WriteLine);
  subject.OnNext('b');
  subject.OnNext('c');
  subject.OnCompleted;
end;

end.
