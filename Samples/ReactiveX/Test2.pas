unit Test2;

interface

// testing OnCompleted

procedure Main;

implementation

uses
  Spring.Console,
  Spring.Reactive,
  Spring.Reactive.Subjects.Subject;

procedure Main;
var
  subject: ISubject<Integer>;
begin
  subject := TSubject<Integer>.Create;
  subject.Subscribe(
    procedure(const value: Integer)
    begin
      Writeln(value)
    end,
    procedure
    begin
      Console.WriteLine('Completed')
    end);
  subject.OnCompleted();
  subject.OnNext(2);
end;

end.
