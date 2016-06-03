unit Test1;

interface

// testing unsubscribing

procedure Main;

implementation

uses
  Spring.Reactive,
  Spring.Reactive.Subjects.Subject;

procedure Main;
var
  values: ISubject<Integer>;
  firstSubscription: IDisposable;
  secondSubscription: IDisposable;
begin
  values := TSubject<Integer>.Create;
  firstSubscription := values.Subscribe(
    procedure(const value: Integer)
    begin
      Writeln('1st subscription received ', value);
    end);
  secondSubscription := values.Subscribe(
    procedure(const value: Integer)
    begin
      Writeln('2nd subscription received ', value);
    end);
  values.OnNext(0);
  values.OnNext(1);
  values.OnNext(2);
  values.OnNext(3);
  firstSubscription.Dispose();
  Writeln('Disposed of 1st subscription');
  values.OnNext(4);
  values.OnNext(5);
end;

end.
