unit Spring.Reactive.Subjects.BehaviorSubject;

interface

uses
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Subjects.SubjectBase;

type
  TBehaviorSubject<T> = class(TSubjectBase<T>)
  private
    fObservers: IList<IObserver<T>>;
    fIsStopped: Boolean;
    fValue: T;
    fError: Exception;
    fIsDisposed: Boolean;
    function GetIsDisposed: Boolean; inline;
  public
    constructor Create(const value: T);

    procedure Dispose; override;

    procedure OnNext(const value: T); override;
    procedure OnError(const error: Exception); override;
    procedure OnCompleted; override;

    function Subscribe(const observer: IObserver<T>): IDisposable; override;

    type
      TSubscription = class(TInterfacedObject, IDisposable)
      private
        fSubject: TBehaviorSubject<T>;//unsafe
        fObserver: IObserver<T>;
      public
        constructor Create(const subject: TBehaviorSubject<T>; const observer: IObserver<T>);
        destructor Destroy; override;

        procedure Dispose;
      end;

    property IsDisposed: Boolean read GetIsDisposed;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TBehaviorSubject<T>'}

constructor TBehaviorSubject<T>.Create(const value: T);
begin
  inherited Create;
  fObservers := TCollections.CreateInterfaceList<IObserver<T>>;
  fValue := value;
end;

procedure TBehaviorSubject<T>.Dispose;
begin
  //lock
  fIsDisposed := True;
  fObservers.Clear;
  fValue := Default(T);
  fError := nil;
end;

function TBehaviorSubject<T>.GetIsDisposed: Boolean;
begin
  Result := fIsDisposed;
end;

procedure TBehaviorSubject<T>.OnCompleted;
var
  observers: TArray<IObserver<T>>;
  observer: IObserver<T>;
begin
  if not fIsStopped then
  begin
    observers := fObservers.ToArray;
    fObservers.Clear;
    fIsStopped := True;
  end;

  for observer in fObservers.ToArray do
    observer.OnCompleted;
end;

procedure TBehaviorSubject<T>.OnError(const error: Exception);
var
  observers: TArray<IObserver<T>>;
  observer: IObserver<T>;
begin
  if not fIsStopped then
  begin
    observers := fObservers.ToArray;
    fObservers.Clear;
    fIsStopped := True;
    fError := error;
  end;

  for observer in observers do
    observer.OnError(error);
end;

procedure TBehaviorSubject<T>.OnNext(const value: T);
var
  observers: TArray<IObserver<T>>;
  observer: IObserver<T>;
begin
  if not fIsStopped then
  begin
    fValue := value;
    observers := fObservers.ToArray;
  end;

  for observer in observers do
    observer.OnNext(value);
end;

function TBehaviorSubject<T>.Subscribe(
  const observer: IObserver<T>): IDisposable;
begin
  if not fIsStopped then
  begin
    fObservers.Add(observer);
    observer.OnNext(fValue);
    Exit(TSubscription.Create(Self, observer));
  end;

  if Assigned(fError) then
    observer.OnError(fError)
  else
    observer.OnCompleted;

  Result := Disposable.Empty;
end;

{$ENDREGION}


{$REGION 'TBehaviorSubject<T>.TSubscription'}

constructor TBehaviorSubject<T>.TSubscription.Create(
  const subject: TBehaviorSubject<T>; const observer: IObserver<T>);
begin
  inherited Create;
  fSubject := subject;
  fSubject._AddRef;
  fObserver := observer;
end;

destructor TBehaviorSubject<T>.TSubscription.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TBehaviorSubject<T>.TSubscription.Dispose;
begin
  if Assigned(fObserver) then
  begin
    if not fSubject.IsDisposed and Assigned(fObserver) then
    begin
      fSubject.fObservers.Remove(fObserver);
      fSubject._Release;
      fSubject := nil;
      fObserver := nil;
    end;
  end;
end;

{$ENDREGION}


end.
