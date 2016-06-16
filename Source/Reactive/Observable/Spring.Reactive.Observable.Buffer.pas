{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Reactive.Observable.Buffer;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Concurrency.AsyncLock,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TBuffer<T> = class(TProducer<IList<T>>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
    fSkip: Integer;
    fTimeSpan: TTimeSpan;
    fTimeShift: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<IList<T>>, IObserver<T>)
      private
        fParent: TBuffer<T>;
        fQueue: IQueue<IList<T>>;
        fn: Integer;
        procedure CreateWindow;
      public
        constructor Create(const parent: TBuffer<T>;
          const observer: IObserver<IList<T>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;

      TBufferTimeShift = class(TSink<IList<T>>, IObserver<T>)
      private
        fParent: TBuffer<T>;
        fList: IList<T>;
        procedure Tick;
      public
        constructor Create(const parent: TBuffer<T>;
          const observer: IObserver<IList<T>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<IList<T>>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; count, skip: Integer); overload;
    constructor Create(const source: IObservable<T>;
      const timeSpan, timeShift: TTimeSpan; const scheduler: IScheduler); overload;
  end;

  TBuffer<TSource, TBufferClosing> = class(TProducer<IList<TSource>>)
  private
    fSource: IObservable<TSource>;
    fBufferClosingSelector: Func<IObservable<TBufferClosing>>;
    fBufferBoundaries: IObservable<TBufferClosing>;

    type
      TSinkBase = class(TSink<IList<TSource>>)
      public
        function Run: IDisposable; virtual; abstract;
      end;

      TSink = class(TSinkBase, IObserver<TSource>)
      private
        fParent: TBuffer<TSource, TBufferClosing>;
        fBuffer: IList<TSource>;
        fBufferGate: TAsyncLock;
        fm: ISerialDisposable;
        procedure CreateBufferClose;
        procedure CloseBuffer(const closingSubscription: IDisposable);

        type
          TOmega = class(TDisposableObject, IObserver<TBufferClosing>)
          private
            fParent: TSink;
            fDisposable: IDisposable;
          public
            constructor Create(const parent: TSink; const disposable: IDisposable);
            destructor Destroy; override;
            procedure OnNext(const value: TBufferClosing);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;
      public
        constructor Create(const parent: TBuffer<TSource, TBufferClosing>;
          const observer: IObserver<IList<TSource>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable; override;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;

      TBeta = class(TSinkBase, IObserver<TSource>)
      private
        fParent: TBuffer<TSource, TBufferClosing>;
        fBuffer: IList<TSource>;
        fRefCountDisposable: IRefCountDisposable;

        type
          TOmega = class(TDisposableObject, IObserver<TBufferClosing>)
          private
            fParent: TBeta;
          public
            constructor Create(const parent: TBeta);
            destructor Destroy; override;
            procedure OnNext(const value: TBufferClosing);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;
      public
        constructor Create(const parent: TBuffer<TSource, TBufferClosing>;
          const observer: IObserver<IList<TSource>>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure Dispose; override;
        function Run: IDisposable; override;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<IList<TSource>>;
      const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>;
      const bufferClosingSelector: Func<IObservable<TBufferClosing>>); overload;
    constructor Create(const source: IObservable<TSource>;
      const bufferBoundaries: IObservable<TBufferClosing>); overload;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TBuffer<T>'}

constructor TBuffer<T>.Create(const source: IObservable<T>; count,
  skip: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
  fSkip := skip;
end;

constructor TBuffer<T>.Create(const source: IObservable<T>;
  const timeSpan, timeShift: TTimeSpan; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fTimeSpan := timeSpan;
  fTimeShift := timeShift;
  fScheduler := scheduler;
end;

function TBuffer<T>.Run(const observer: IObserver<IList<T>>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
  buffered: TBufferTimeShift;
begin
  if fScheduler = nil then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    Result := sink.Run;
  end
  else if fCount > 0 then
  begin
  {
      var sink = new Impl(this, observer, cancel);
      setSink(sink);
      return sink.Run();
  }
  end
  else
//    if fTimeSpan == fTimeShift then
    begin
      buffered := TBufferTimeShift.Create(Self, observer, cancel);
      setSink(buffered);
      Result := buffered.Run;
    end
//    else
      {
          var sink = new BufferImpl(this, observer, cancel);
          setSink(sink);
          return sink.Run();
      }
end;

{$ENDREGION}


{$REGION 'TBuffer<T>.TSink'}

constructor TBuffer<T>.TSink.Create(const parent: TBuffer<T>;
  const observer: IObserver<IList<T>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TBuffer<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TBuffer<T>.TSink.Run: IDisposable;
begin
  fQueue := TCollections.CreateQueue<IList<T>>;
  fn := 0;

  CreateWindow;
  Result := fParent.fSource.Subscribe(Self);
end;

procedure TBuffer<T>.TSink.CreateWindow;
var
  s: IList<T>;
begin
  s := TCollections.CreateList<T>;
  fQueue.Enqueue(s);
end;

procedure TBuffer<T>.TSink.OnNext(const value: T);
var
  s: IList<T>;
  c: Integer;
begin
  for s in fQueue do
    s.Add(value);

  c := fn - fParent.fCount + 1;
  if (c >= 0) and (c mod fParent.fSkip = 0) then
  begin
    s := fQueue.Dequeue;
    if s.Count > 0 then
      fObserver.OnNext(s);
  end;

  Inc(fn);
  if fn mod fParent.fSkip = 0 then
    CreateWindow;
end;

procedure TBuffer<T>.TSink.OnError(const error: Exception);
begin
  while fQueue.Count > 0 do
    fQueue.Dequeue.Clear;

  fObserver.OnError(error);
  Dispose;
end;

procedure TBuffer<T>.TSink.OnCompleted;
var
  s: IList<T>;
begin
  while fQueue.Count > 0 do
  begin
    s := fQueue.Dequeue;
    if s.Count > 0 then
      fObserver.OnNext(s);
  end;

  fObserver.OnCompleted;
  Dispose;
end;

{$ENDREGION}


{$REGION 'TBuffer<T>.TBufferTimeShift'}

constructor TBuffer<T>.TBufferTimeShift.Create(const parent: TBuffer<T>;
  const observer: IObserver<IList<T>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TBuffer<T>.TBufferTimeShift.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TBuffer<T>.TBufferTimeShift.Run: IDisposable;
var
  d: IDisposable;
  s: IDisposable;
begin
  fList := TCollections.CreateList<T>;

  d := (fParent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(fParent.fTimeSpan, Tick);
  s := fParent.fSource.Subscribe(Self);

  Result := TStableCompositeDisposable.Create(d, s);
end;

procedure TBuffer<T>.TBufferTimeShift.Tick;
begin
  MonitorEnter(Self);
  try
    fObserver.OnNext(fList);
    fList := TCollections.CreateList<T>;
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<T>.TBufferTimeShift.OnNext(const value: T);
begin
  MonitorEnter(Self);
  try
    fList.Add(value);
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<T>.TBufferTimeShift.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    fList.Clear;

    fObserver.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<T>.TBufferTimeShift.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fObserver.OnNext(fList);
    fObserver.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TBuffer<TSource, TBufferClosing>'}

constructor TBuffer<TSource, TBufferClosing>.Create(
  const source: IObservable<TSource>;
  const bufferClosingSelector: Func<IObservable<TBufferClosing>>);
begin
  inherited Create;
  fSource := source;
  fBufferClosingSelector := bufferClosingSelector;
end;

constructor TBuffer<TSource, TBufferClosing>.Create(
  const source: IObservable<TSource>;
  const bufferBoundaries: IObservable<TBufferClosing>);
begin
  inherited Create;
  fSource := source;
  fBufferBoundaries := bufferBoundaries;
end;

function TBuffer<TSource, TBufferClosing>.Run(
  const observer: IObserver<IList<TSource>>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSinkBase;
begin
  if Assigned(fBufferClosingSelector) then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    Result := sink.Run;
  end
  else
  begin
    sink := TBeta.Create(Self, observer, cancel);
    setSink(sink);
    Result := sink.Run;
  end;
end;

{$ENDREGION}


{$REGION 'TBuffer<TSource, TBufferClosing>.TSink'}

constructor TBuffer<TSource, TBufferClosing>.TSink.Create(
  const parent: TBuffer<TSource, TBufferClosing>;
  const observer: IObserver<IList<TSource>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TBuffer<TSource, TBufferClosing>.TSink.Destroy;
begin
  fBufferGate.Free;
  fParent._Release;
  inherited;
end;

function TBuffer<TSource, TBufferClosing>.TSink.Run: IDisposable;
var
  groupDisposable: ICompositeDisposable;
begin
  fBuffer := TCollections.CreateList<TSource>;
  fBufferGate := TAsyncLock.Create;

  fm := TSerialDisposable.Create;
  groupDisposable := TCompositeDisposable.Create([fm]);
  groupDisposable.Add(fParent.fSource.Subscribe(Self));

  fBufferGate.Wait(CreateBufferClose);

  Result := groupDisposable;
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.CreateBufferClose;
var
  bufferClose: IObservable<TBufferClosing>;
  closingSubscription: ISingleAssignmentDisposable;
begin
  try
    bufferClose := fParent.fBufferClosingSelector();
  except
    on e: Exception do
    begin
      MonitorEnter(Self);
      try
        fObserver.OnError(e);
        Dispose;
      finally
        MonitorExit(Self);
      end;
    end;
  end;

  closingSubscription := TSingleAssignmentDisposable.Create;
  fm.Disposable := closingSubscription;
  closingSubscription.Disposable := bufferClose.Subscribe(
    TOmega.Create(Self, closingSubscription) as IObserver<TBufferClosing>);
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.CloseBuffer(
  const closingSubscription: IDisposable);
var
  res: IList<TSource>;
begin
  closingSubscription.Dispose;

  MonitorEnter(Self);
  try
    res := fBuffer;
    fBuffer := TCollections.CreateList<TSource>;
    fObserver.OnNext(res);
  finally
    MonitorExit(Self);
  end;

  fBufferGate.Wait(CreateBufferClose);
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.OnNext(const value: TSource);
begin
  MonitorEnter(Self);
  try
    fBuffer.Add(value);
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.OnError(
  const error: Exception);
begin
  MonitorEnter(Self);
  try
    fBuffer.Clear;
    fObserver.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fObserver.OnNext(fBuffer);
    fObserver.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TBuffer<TSource, TBufferClosing>.TSink.TOmega'}

constructor TBuffer<TSource, TBufferClosing>.TSink.TOmega.Create(
  const parent: TSink; const disposable: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fDisposable := disposable;
end;

destructor TBuffer<TSource, TBufferClosing>.TSink.TOmega.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.TOmega.OnNext(
  const value: TBufferClosing);
begin
  fParent.CloseBuffer(fDisposable);
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.TOmega.OnError(
  const error: Exception);
begin
  fParent.OnError(error);
end;

procedure TBuffer<TSource, TBufferClosing>.TSink.TOmega.OnCompleted;
begin
  fParent.CloseBuffer(fDisposable);
end;

{$ENDREGION}


{$REGION 'TBuffer<TSource, TBufferClosing>.TBeta'}

constructor TBuffer<TSource, TBufferClosing>.TBeta.Create(
  const parent: TBuffer<TSource, TBufferClosing>;
  const observer: IObserver<IList<TSource>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TBuffer<TSource, TBufferClosing>.TBeta.Destroy;
begin
  Dispose;
  inherited Destroy;
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.Dispose;
var
  obj: TDisposableObject;
begin
  if not IsDisposed then
  begin
    fParent._Release;
    fBuffer := nil;
    fRefCountDisposable := nil;
  end;

  inherited Dispose;
end;

function TBuffer<TSource, TBufferClosing>.TBeta.Run: IDisposable;
var
  d: ICompositeDisposable;
begin
  fBuffer := TCollections.CreateList<TSource>;

  d := TCompositeDisposable.Create([]);
  fRefCountDisposable := TRefCountDisposable.Create(d);

  d.Add(fParent.fSource.Subscribe(Self));
  d.Add(fParent.fBufferBoundaries.Subscribe(
    TOmega.Create(Self) as IObserver<TBufferClosing>));

  Result := fRefCountDisposable;
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.OnNext(const value: TSource);
begin
  MonitorEnter(Self);
  try
    fBuffer.Add(value);
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.OnError(
  const error: Exception);
begin
  MonitorEnter(Self);
  try
    fBuffer.Clear;
    fObserver.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fObserver.OnNext(fBuffer);
    fObserver.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TBuffer<TSource, TBufferClosing>.TBeta.TOmega'}

constructor TBuffer<TSource, TBufferClosing>.TBeta.TOmega.Create(
  const parent: TBeta);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
end;

destructor TBuffer<TSource, TBufferClosing>.TBeta.TOmega.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.TOmega.OnNext(
  const value: TBufferClosing);
var
  res: IList<TSource>;
begin
  MonitorEnter(fParent);
  try
    res := fParent.fBuffer;
    fParent.fBuffer := TCollections.CreateList<TSource>;
    fParent.fObserver.OnNext(res);
  finally
    MonitorExit(fParent);
  end;
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.TOmega.OnError(
  const error: Exception);
begin
  fParent.OnError(error);
end;

procedure TBuffer<TSource, TBufferClosing>.TBeta.TOmega.OnCompleted;
begin
  fParent.OnCompleted;
end;

{$ENDREGION}


end.
