unit Spring.Reactive.Subjects.AsyncSubject;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Subjects.SubjectBase;

type
  TAsyncSubject<T> = class(TSubjectBase<T>)
  private
    fObservers: TArray<IObserver<T>>;
    fIsStopped: Boolean;
    fValue: T;
    fHasValue: Boolean;
    fError: Exception;
  public
    procedure OnNext(const value: T); override;
    procedure OnError(const error: Exception); override;
    procedure OnCompleted; override;

    function Subscribe(const observer: IObserver<T>): IDisposable; override;

    type
      TSubscription = class(TDisposableObject)
      private
        fSubject: TAsyncSubject<T>;//unsafe
        fObserver: IObserver<T>;
      public
        constructor Create(const subject: TAsyncSubject<T>; const observer: IObserver<T>);

        procedure Dispose; override;
      end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TAsyncSubject<T>'}

procedure TAsyncSubject<T>.OnCompleted;
var
  observers: TArray<IObserver<T>>;
  value: T;
  hasValue: Boolean;
  observer: IObserver<T>;
begin
  if not fIsStopped then
  begin
    observers := fObservers;
    fObservers := nil;
    fIsStopped := True;
    value := fValue;
    hasValue := fHasValue;
  end;

  if hasValue then
    for observer in observers do
    begin
      observer.OnNext(value);
      observer.OnCompleted;
    end
  else
    for observer in observers do
      observer.OnCompleted;
end;

procedure TAsyncSubject<T>.OnError(const error: Exception);
var
  observers: TArray<IObserver<T>>;
  observer: IObserver<T>;
begin
  if not fIsStopped then
  begin
    observers := fObservers;
    fObservers := nil;
    fIsStopped := True;
    fError := error;
  end;

  for observer in observers do
    observer.OnError(error);
end;

procedure TAsyncSubject<T>.OnNext(const value: T);
begin
  if not fIsStopped then
  begin
    fValue := value;
    fHasValue := True;
  end;
end;

function TAsyncSubject<T>.Subscribe(const observer: IObserver<T>): IDisposable;
var
  error: Exception;
  value: T;
  hasValue: Boolean;
begin
  error := nil;
  value := Default(T);
  hasValue := False;

  // lock {
  if not fIsStopped then
  begin
    fObservers := TArray.Add<IObserver<T>>(fObservers, observer);
    Exit(TSubscription.Create(Self, observer));
  end;

  error := fError;
  hasValue := fHasValue;
  value := fValue;
  // }

  if Assigned(error) then
    observer.OnError(error)
  else
  begin
    if hasValue then
      observer.OnNext(value);
    observer.OnCompleted;
  end;

  Result := Disposable.Empty;
end;

{$ENDREGION}


{$REGION 'TAsyncSubject<T>.TSubscription'}

constructor TAsyncSubject<T>.TSubscription.Create(
  const subject: TAsyncSubject<T>; const observer: IObserver<T>);
begin
  inherited Create;
  fSubject := subject;
  fSubject._AddRef;
  fObserver := observer;
end;

procedure TAsyncSubject<T>.TSubscription.Dispose;
begin
  if Assigned(fObserver) then
  begin // lock
    if Assigned(fObserver) then
    begin
      fSubject.fObservers := TArray.Remove<IObserver<T>>(fSubject.fObservers, fObserver);
      fSubject._Release;
      fObserver := nil;
    end;
  end;
end;

{$ENDREGION}


end.
