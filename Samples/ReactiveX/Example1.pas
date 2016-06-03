unit Example1;

interface

// introduction to the basic Spring.Reactive interfaces

procedure Main;

implementation

uses
  Rtti, SysUtils,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TMyConsoleObserver<T> = class(TDisposableObject, IObserver<T>)
    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

{ TMyConsoleObserver<T> }

procedure TMyConsoleObserver<T>.OnCompleted;
begin
  Writeln('Sequence terminated');
end;

procedure TMyConsoleObserver<T>.OnError(const error: Exception);
begin
  Writeln('Sequence faulted with ', error.Message);
end;

procedure TMyConsoleObserver<T>.OnNext(const value: T);
begin
  Writeln('Received value ', TValue.From<T>(value).ToString);
end;

type
  TMySequenceOfNumbers = class(TObservableBase<Integer>, IObservable<Integer>)
    function Subscribe(const observer: IObserver<Integer>): IDisposable; override;
  end;

{ TMySequenceOfNumbers }

function TMySequenceOfNumbers.Subscribe(
  const observer: IObserver<Integer>): IDisposable;
begin
  observer.OnNext(1);
  observer.OnNext(2);
  observer.OnNext(3);
  observer.OnCompleted();
end;

procedure Main;
var
  numbers: IObservable<Integer>;
  observer: IObserver<Integer>;
begin
  numbers := TMySequenceOfNumbers.Create;
  observer := TMyConsoleObserver<Integer>.Create;
  numbers.Subscribe(observer);
end;

end.
