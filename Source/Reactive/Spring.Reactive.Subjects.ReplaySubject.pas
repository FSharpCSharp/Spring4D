unit Spring.Reactive.Subjects.ReplaySubject;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Subjects.SubjectBase,
  Spring.Reactive.TimeInterval;

type
  TReplaySubject<T> = class(TSubjectBase<T>)
  private const
    InfiniteBufferSize = MaxInt;
  private
    fQueue: IQueue<TTimeInterval<T>>;
    fObservers: IList<IObserver<T>>;
    fBufferSize: Integer;
    fWindow: TTimeSpan;
    fStopwatch: TStopwatch;
    procedure Unsubscribe(const observer: IObserver<T>);
  protected
    procedure Next(const value: T);
    function Replay(const observer: IObserver<T>): Integer;
    procedure Trim;
  public
    constructor Create(bufferSize: Integer; const window: TTimeSpan); overload;
    constructor Create(bufferSize: Integer = InfiniteBufferSize); overload;
    constructor Create(const window: TTimeSpan); overload;

    procedure OnNext(const value: T); override;
    procedure OnError(const error: Exception); override;
    procedure OnCompleted; override;

    function Subscribe(const observer: IObserver<T>): IDisposable; override;

    type
      TSubscription = class(TDisposableObject)
      private
        fSubject: TReplaySubject<T>;//unsafe
        fObserver: IObserver<T>;
      public
        constructor Create(const subject: TReplaySubject<T>; const observer: IObserver<T>);

        procedure Dispose; override;
      end;
  end;

implementation


{$REGION 'TReplaySubject<T>'}

constructor TReplaySubject<T>.Create(bufferSize: Integer;
  const window: TTimeSpan);
begin
  inherited Create;
  fQueue := TCollections.CreateQueue<TTimeInterval<T>>;
  fObservers := TCollections.CreateInterfaceList<IObserver<T>>;
  fBufferSize := bufferSize;
  fWindow := window;
  fStopwatch.Start;
end;

constructor TReplaySubject<T>.Create(bufferSize: Integer);
begin
  Create(bufferSize, TTimeSpan.MaxValue);
end;

constructor TReplaySubject<T>.Create(const window: TTimeSpan);
begin
  Create(InfiniteBufferSize, window);
end;

procedure TReplaySubject<T>.Next(const value: T);
begin
  fQueue.Enqueue(TTimeInterval<T>.Create(value, fStopwatch.Elapsed));
end;

procedure TReplaySubject<T>.OnCompleted;
var
  observer: IObserver<T>;
begin
  for observer in fObservers.ToArray do
    observer.OnCompleted;
end;

procedure TReplaySubject<T>.OnError(const error: Exception);
var
  observer: IObserver<T>;
begin
  for observer in fObservers.ToArray do
    observer.OnError(error);
end;

procedure TReplaySubject<T>.OnNext(const value: T);
var
  observer: IObserver<T>;
begin
  Next(value);
  Trim;
  for observer in fObservers.ToArray do
    observer.OnNext(value);
end;

function TReplaySubject<T>.Replay(const observer: IObserver<T>): Integer;
var
  item: TTimeInterval<T>;
begin
  Result := fQueue.Count;
  for item in fQueue do
    observer.OnNext(item.Value);
end;

function TReplaySubject<T>.Subscribe(const observer: IObserver<T>): IDisposable;
begin
  Trim;
  Replay(observer);
  fObservers.Add(observer);
  Result := TSubscription.Create(Self, observer);
end;

procedure TReplaySubject<T>.Trim;
var
  now: TTimeSpan;
begin
  now := fStopwatch.Elapsed;
  while fQueue.Count > fBufferSize do
    fQueue.Dequeue;
  while (fQueue.Count > 0) and (now.Subtract(fQueue.Peek.Interval) > fWindow) do
    fQueue.Dequeue;
end;

procedure TReplaySubject<T>.Unsubscribe(const observer: IObserver<T>);
begin
  fObservers.Remove(observer);
end;

{$ENDREGION}


{$REGION 'TReplaySubject<T>.TSubscription'}

constructor TReplaySubject<T>.TSubscription.Create(
  const subject: TReplaySubject<T>; const observer: IObserver<T>);
begin
  inherited Create;
  fSubject := subject;
  fSubject._AddRef;
  fObserver := observer;
end;

procedure TReplaySubject<T>.TSubscription.Dispose;
begin
  fObserver.Dispose;
  fSubject.Unsubscribe(fObserver);
  fSubject._Release;
  fSubject := nil;
end;

{$ENDREGION}


end.
