unit Spring.Reactive.Subjects.SubjectBase;

interface

uses
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TSubjectBase<T> = class(TObservableBase<T>, ISubject<T>, IObserver<T>)
  public
    procedure OnNext(const value: T); virtual; abstract;
    procedure OnError(const error: Exception); virtual; abstract;
    procedure OnCompleted; virtual; abstract;
  end;

implementation

end.
