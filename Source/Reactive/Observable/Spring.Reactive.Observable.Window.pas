{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
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

unit Spring.Reactive.Observable.Window;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Sink,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Observable.AddRef;

type
  TWindow<TSource> = class
  public type
    TCount = class(TProducer<IObservable<TSource>>)
    private
      fSource: IObservable<TSource>;
      fCount: Integer;
      fSkip: Integer;

      type
        TSink = class(TSink<IObservable<TSource>>, IObserver<TSource>)
        private
          fQueue: IQueue<ISubject<TSource>>;
          fm: ISingleAssignmentDisposable;
          fRefCountDisposable: IRefCountDisposable;
          fCount: Integer;
          fSkip: Integer;
          fn: Integer;
          function CreateWindow: IObservable<TSource>;
        public
          constructor Create(const parent: TCount;
            const observer: IObserver<IObservable<TSource>>; const cancel: IDisposable);
          function Run(const source: IObservable<TSource>): IDisposable;
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function CreateSink(const observer: IObserver<IObservable<TSource>>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>; const count, skip: Integer);
    end;

    TTimeSliding = class(TProducer<IObservable<TSource>>)
    private
      fSource: IObservable<TSource>;
      fTimeSpan: TTimeSpan;
      fTimeShift: TTimeSpan;
      fScheduler: IScheduler;

      type
        TSink = class(TSink<IObservable<TSource>>, IObserver<TSource>)
        private
          fQueue: IQueue<ISubject<TSource>>;
          fTimerD: ISerialDisposable;
          fScheduler: IScheduler;
          fTimeShift: TTimeSpan;
          fRefCountDisposable: IRefCountDisposable;
          fTotalTime: TTimeSpan;
          fNextShift: TTimeSpan;
          fNextSpan: TTimeSpan;

          type
            TState = record
              isSpan, isShift: Boolean
            end;
        private
          procedure CreateWindow;
          procedure CreateTimer;
          function Tick(const scheduler: IScheduler; const state: TState): IDisposable;
        public
          constructor Create(const parent: TTimeSliding;
            const observer: IObserver<IObservable<TSource>>;
            const cancel: IDisposable);
          function Run(const parent: TTimeSliding): IDisposable;
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function CreateSink(const observer: IObserver<IObservable<TSource>>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>;
        const timeSpan: TTimeSpan; const timeShift: TTimeSpan;
        const scheduler: IScheduler);
    end;

    TTimeHopping = class(TProducer<IObservable<TSource>>)
    private
      fSource: IObservable<TSource>;
      fTimeSpan: TTimeSpan;
      fScheduler: IScheduler;

      type
        TSink = class(TSink<IObservable<TSource>>, IObserver<TSource>)
        private
          fSubject: ISubject<TSource>;
          fRefCountDisposable: IRefCountDisposable;
          procedure Tick;
          procedure CreateWindow;
        public
          function Run(const parent: TTimeHopping): IDisposable;
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function CreateSink(const observer: IObserver<IObservable<TSource>>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>;
      const timeSpan: TTimeSpan; const scheduler: IScheduler);
    end;
  end;

  TWindowObservable<TSource> = class(TAddRef<TSource>)
  end;

implementation

uses
  Rtti, // H2443
  Spring.Reactive.Disposables,
  Spring.Reactive.Subjects.Subject;


{$REGION 'TWindow<TSource>.TCount'}

constructor TWindow<TSource>.TCount.Create(const source: IObservable<TSource>; const count,
  skip: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
  fSkip := skip;
end;

function TWindow<TSource>.TCount.CreateSink(
  const observer: IObserver<IObservable<TSource>>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel)
end;

function TWindow<TSource>.TCount.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fSource);
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TCount.TSink'}

constructor TWindow<TSource>.TCount.TSink.Create(const parent: TCount;
  const observer: IObserver<IObservable<TSource>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fQueue := TCollections.CreateQueue<ISubject<TSource>>;
  fm := TSingleAssignmentDisposable.Create;
  fRefCountDisposable := TRefCountDisposable.Create(fm);

  fCount := parent.fCount;
  fSkip := parent.fSkip;
end;

function TWindow<TSource>.TCount.TSink.Run(const source: IObservable<TSource>): IDisposable;
var
  firstWindow: IObservable<TSource>;
begin
  fn := 0;
  firstWindow := CreateWindow;
  Observer.OnNext(firstWindow);
  fm.Disposable := source.Subscribe(Self);
  Result := fRefCountDisposable;
end;

function TWindow<TSource>.TCount.TSink.CreateWindow: IObservable<TSource>;
var
  s: ISubject<TSource>;
begin
  s := TSubject<TSource>.Create;
  fQueue.Enqueue(s);
  Result := TWindowObservable<TSource>.Create(s, fRefCountDisposable);
end;

procedure TWindow<TSource>.TCount.TSink.OnNext(const value: TSource);
var
  s: ISubject<TSource>;
  c: Integer;
  newWindow: IObservable<TSource>;
begin
  for s in fQueue do
    s.OnNext(value);

  c := fn - fCount + 1;
  if (c >= 0) and (c mod fSkip = 0) then
  begin
    s := fQueue.Dequeue;
    s.OnCompleted;
  end;

  Inc(fn);
  if fn mod fSkip = 0 then
  begin
    newWindow := CreateWindow;
    Observer.OnNext(newWindow);
  end;
end;

procedure TWindow<TSource>.TCount.TSink.OnError(const error: Exception);
begin
  while fQueue.Count > 0 do
    fQueue.Dequeue.OnError(error);

  Observer.OnError(error);
  Dispose;
end;

procedure TWindow<TSource>.TCount.TSink.OnCompleted;
begin
  while fQueue.Count > 0 do
    fQueue.Dequeue.OnCompleted;

  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TTimeSliding'}

constructor TWindow<TSource>.TTimeSliding.Create(
  const source: IObservable<TSource>; const timeSpan, timeShift: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fTimeSpan := timeSpan;
  fTimeShift := timeShift;
  fScheduler := scheduler;
end;

function TWindow<TSource>.TTimeSliding.CreateSink(
  const observer: IObserver<IObservable<TSource>>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TWindow<TSource>.TTimeSliding.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self);
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TTimeSliding.TSink'}

constructor TWindow<TSource>.TTimeSliding.TSink.Create(
  const parent: TTimeSliding; const observer: IObserver<IObservable<TSource>>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fScheduler := parent.fScheduler;
  fTimeShift := parent.fTimeShift;

  fQueue := TCollections.CreateQueue<ISubject<TSource>>;
  fTimerD := TSerialDisposable.Create;
end;

function TWindow<TSource>.TTimeSliding.TSink.Run(
  const parent: TTimeSliding): IDisposable;
var
  groupDisposable: ICompositeDisposable;
begin
  fTotalTime := TTimeSpan.Zero;
  fNextShift := parent.fTimeShift;
  fNextSpan := parent.fTimeSpan;

  groupDisposable := TCompositeDisposable.Create([fTimerD]);
  fRefCountDisposable := TRefCountDisposable.Create(groupDisposable);

  CreateWindow;
  CreateTimer;

  groupDisposable.Add(parent.fSource.Subscribe(Self));

  Result := fRefCountDisposable;
end;

procedure TWindow<TSource>.TTimeSliding.TSink.CreateWindow;
var
  s: ISubject<TSource>;
begin
  s := TSubject<TSource>.Create;
  fQueue.Enqueue(s);
  Observer.OnNext(TWindowObservable<TSource>.Create(s, fRefCountDisposable) as IObservable<TSource>);
end;

procedure TWindow<TSource>.TTimeSliding.TSink.CreateTimer;
var
  m: ISingleAssignmentDisposable;
  isSpan: Boolean;
  isShift: Boolean;
  newTotalTime: TTimeSpan;
  ts: TTimeSpan;
  state: TState;
  guard: IInterface;
begin
  m := TSingleAssignmentDisposable.Create;
  fTimerD.Disposable := m;

  isSpan := False;
  isShift := False;
  if fNextSpan = fNextShift then
  begin
    isSpan := True;
    isShift := True;
  end
  else if fNextSpan < fNextShift then
    isSpan := True
  else
    isShift := True;

  if isSpan then
    newTotalTime := fNextSpan
  else
    newTotalTime := fNextShift;
  ts := newTotalTime - fTotalTime;
  fTotalTime := newTotalTime;

  if isSpan then
    fNextSpan := fNextSpan + fTimeShift;
  if isShift then
    fNextShift := fNextShift + fTimeShift;

  state.isSpan := isSpan;
  state.isShift := isShift;
  guard := Self; // make sure that self is kept alive by capturing it
  m.Disposable := fScheduler.Schedule(TValue.From(state), ts,
    function (const scheduler: IScheduler; const state: TValue): IDisposable
    begin
      if Assigned(guard) then
        Tick(scheduler, state.AsType<TState>);
    end);
end;

function TWindow<TSource>.TTimeSliding.TSink.Tick(const scheduler: IScheduler;
  const state: TState): IDisposable;
var
  s: ISubject<TSource>;
begin
  MonitorEnter(Self);
  try
    if state.isSpan then
    begin
      s := fQueue.Dequeue;
      s.OnCompleted;
      s := nil;
    end;

    if state.isShift then
      CreateWindow;
  finally
    MonitorExit(Self);
  end;

  CreateTimer;

  Result := Disposable.Empty;
end;

procedure TWindow<TSource>.TTimeSliding.TSink.OnNext(const value: TSource);
var
  s: ISubject<TSource>;
begin
  MonitorEnter(Self);
  try
    for s in fQueue do
      s.OnNext(value);
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TTimeSliding.TSink.OnError(const error: Exception);
var
  s: ISubject<TSource>;
begin
  MonitorEnter(Self);
  try
    for s in fQueue do
      s.OnError(error);

    Observer.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TTimeSliding.TSink.OnCompleted;
var
  s: ISubject<TSource>;
begin
  MonitorEnter(Self);
  try
    for s in fQueue do
      s.OnCompleted;

    Observer.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TTimeHopping'}

constructor TWindow<TSource>.TTimeHopping.Create(
  const source: IObservable<TSource>; const timeSpan: TTimeSpan;
  const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fTimeSpan := timeSpan;
  fScheduler := scheduler;
end;

function TWindow<TSource>.TTimeHopping.CreateSink(
  const observer: IObserver<IObservable<TSource>>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TWindow<TSource>.TTimeHopping.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self);
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TTimeHopping.TSink'}

function TWindow<TSource>.TTimeHopping.TSink.Run(
  const parent: TTimeHopping): IDisposable;
var
  groupDisposable: ICompositeDisposable;
begin
  groupDisposable := TCompositeDisposable.Create([]);
  fRefCountDisposable := TRefCountDisposable.Create(groupDisposable);

  CreateWindow;

  groupDisposable.Add((parent.fScheduler as ISchedulerPeriodic).SchedulePeriodic(parent.fTimeSpan, Tick));
  groupDisposable.Add(parent.fSource.Subscribe(Self));

  Result := fRefCountDisposable;
end;

procedure TWindow<TSource>.TTimeHopping.TSink.Tick;
begin
  MonitorEnter(Self);
  try
    fSubject.OnCompleted;
    CreateWindow;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TTimeHopping.TSink.CreateWindow;
begin
  fSubject := TSubject<TSource>.Create;
  Observer.OnNext(TWindowObservable<TSource>.Create(fSubject, fRefCountDisposable) as IObservable<TSource>);
end;

procedure TWindow<TSource>.TTimeHopping.TSink.OnNext(const value: TSource);
begin
  MonitorEnter(Self);
  try
    fSubject.OnNext(value);
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TTimeHopping.TSink.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    fSubject.OnError(error);

    Observer.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TTimeHopping.TSink.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fSubject.OnCompleted;

    Observer.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

{$ENDREGION}


end.
