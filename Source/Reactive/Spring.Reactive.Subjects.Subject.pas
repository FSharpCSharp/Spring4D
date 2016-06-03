unit Spring.Reactive.Subjects.Subject;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Subjects.SubjectBase;

type
  TSubject<T> = class(TSubjectBase<T>, IDisposable)
  private
    fObserver: IObserver<T>;
    procedure Unsubscribe(const observer: IObserver<T>);
  public
    constructor Create;
    destructor Destroy; override;

    procedure OnNext(const value: T); override;
    procedure OnError(const error: Exception); override;
    procedure OnCompleted; override;

    function Subscribe(const observer: IObserver<T>): IDisposable; override;

    procedure Dispose; override;

    type
      TSubscription = class(TDisposableObject)
      private
        fSubject: TSubject<T>;//unsafe
        fObserver: IObserver<T>;
      public
        constructor Create(const subject: TSubject<T>; const observer: IObserver<T>);

        procedure Dispose; override;
      end;
  end;

implementation

uses
  Spring.Reactive.Disposables,
  Spring.Reactive.Observers;


{$REGION 'TSubject<T>'}

constructor TSubject<T>.Create;
begin
  inherited Create;
  fObserver := TNopObserver<T>.Create;
end;

destructor TSubject<T>.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TSubject<T>.Dispose;
begin
  fObserver := TDisposedObserver<T>.Instance;
end;

procedure TSubject<T>.OnCompleted;
var
  oldObserver, newObserver: IObserver<T>;
begin
  newObserver := TDoneObserver<T>.Completed;

  repeat
    oldObserver := fObserver;
    if oldObserver is TDoneObserver<T> then
      Break;
  until TInterlocked.CompareExchange<IObserver<T>>(fObserver, newObserver, oldObserver) = oldObserver;

  oldObserver.OnCompleted;
end;

procedure TSubject<T>.OnError(const error: Exception);
var
  oldObserver, newObserver: IObserver<T>;
begin
  newObserver := TDoneObserver<T>.Create(error);

  repeat
    oldObserver := fObserver;
    if oldObserver is TDoneObserver<T> then
      Break;
  until TInterlocked.CompareExchange<IObserver<T>>(fObserver, newObserver, oldObserver) = oldObserver;

  oldObserver.OnError(error);
end;

procedure TSubject<T>.OnNext(const value: T);
begin
  fObserver.OnNext(value);
end;

function TSubject<T>.Subscribe(const observer: IObserver<T>): IDisposable;
var
  oldObserver, newObserver: IObserver<T>;
  done: TDoneObserver<T>;
  obs: TObserver<T>;
begin
  repeat
    oldObserver := fObserver;
    if oldObserver = TDisposedObserver<T>.Instance then
      raise EObjectDisposedException.Create('');

    if oldObserver = TDoneObserver<T>.Completed then
    begin
      observer.OnCompleted;
      Exit(Disposable.Empty);
    end;

    if oldObserver is TDoneObserver<T> then
    begin
      done := oldObserver as TDoneObserver<T>;
      observer.OnError(done.Error);
      Exit(Disposable.Empty);
    end;

    if oldObserver = TNopObserver<T>.Instance then
    begin
      newObserver := observer;
    end
    else
    begin
      if oldObserver is TObserver<T> then
      begin
        obs := oldObserver as TObserver<T>;
        newObserver := obs.Add(observer);
      end
      else
        newObserver := TObserver<T>.Create([oldObserver, observer]);
    end;
  until TInterlocked.CompareExchange<IObserver<T>>(fObserver, newObserver, oldObserver) = oldObserver;

  Result := TSubscription.Create(Self, observer);
end;

procedure TSubject<T>.Unsubscribe(const observer: IObserver<T>);
var
  oldObserver, newObserver: IObserver<T>;
  obs: TObserver<T>;
begin
  repeat
    oldObserver := fObserver;
    if oldObserver is TDoneObserver<T> then
      Break;
    if oldObserver is TObserver<T> then
    begin
      obs := oldObserver as TObserver<T>;
      newObserver := obs.Remove(observer);
    end
    else
    begin
      if oldObserver <> observer then
        Exit;
      newObserver := TNopObserver<T>.Instance;
    end;
  until TInterlocked.CompareExchange<IObserver<T>>(fObserver, newObserver, oldObserver) = oldObserver;
end;

{$ENDREGION}


{$REGION 'TSubject<T>.TSubscription'}

constructor TSubject<T>.TSubscription.Create(const subject: TSubject<T>;
  const observer: IObserver<T>);
begin
  inherited Create;
  fSubject := subject;
  fSubject._AddRef;
  fObserver := observer;
end;

procedure TSubject<T>.TSubscription.Dispose;
var
  observer: IObserver<T>;
begin
  observer := TInterlocked.Exchange<IObserver<T>>(fObserver, nil);
  if Assigned(observer) then
  begin
    fSubject.Unsubscribe(observer);
    fSubject._Release;
    fSubject := nil;
  end;
end;

{$ENDREGION}


end.
