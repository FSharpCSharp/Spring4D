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
  TWindow<TSource> = class(TProducer<IObservable<TSource>>)
  private
    fSource: IObservable<TSource>;
    fCount: Integer;
    fSkip: Integer;
    fTimeSpan: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSinkBase = class(TSink<IObservable<TSource>>)
      public
        function Run: IDisposable; virtual; abstract;
      end;

      TSink = class(TSinkBase, IObserver<TSource>)
      private
        fParent: TWindow<TSource>;
        fQueue: IQueue<ISubject<TSource>>;
        fn: Integer;
        fm: ISingleAssignmentDisposable;
        fRefCountDisposable: IRefCountDisposable;
        function CreateWindow: IObservable<TSource>;
      public
        constructor Create(const parent: TWindow<TSource>;
          const observer: IObserver<IObservable<TSource>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable; override;
        procedure OnNext(const value: TSource);
      end;

      TBoundedWindowImpl = class(TSinkBase, IObserver<TSource>)
      private
        fParent: TWindow<TSource>;
        fs: ISubject<TSource>;
        fn: Integer;
        fWindowId: Integer;
        fTimerD: ISerialDisposable;
        fRefCountDisposable: IRefCountDisposable;
        procedure CreateTimer(id: Integer);
        function Tick(const scheduler: IScheduler; const id: TValue): IDisposable;
      public
        constructor Create(const parent: TWindow<TSource>;
          const observer: IObserver<IObservable<TSource>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable; override;
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

  TWindowObservable<TSource> = class(TAddRef<TSource>)
  end;

implementation

uses
  Rtti, // H2443
  Spring.Reactive.Disposables,
  Spring.Reactive.Subjects.Subject;


{$REGION 'TWindow<TSource>'}

constructor TWindow<TSource>.Create(const source: IObservable<TSource>; const count,
  skip: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
  fSkip := skip;
end;

function TWindow<TSource>.CreateSink(
  const observer: IObserver<IObservable<TSource>>;
  const cancel: IDisposable): TObject;
begin
  if fScheduler = nil then
    Result := TSink.Create(Self, observer, cancel)
  else if fCount > 0 then
    Result := TBoundedWindowImpl.Create(Self, observer, cancel);
end;

function TWindow<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TSink'}

constructor TWindow<TSource>.TSink.Create(const parent: TWindow<TSource>;
  const observer: IObserver<IObservable<TSource>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TWindow<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TWindow<TSource>.TSink.Run: IDisposable;
var
  firstWindow: IObservable<TSource>;
begin
  fQueue := TCollections.CreateQueue<ISubject<TSource>>;
  fn := 0;
  fm := TSingleAssignmentDisposable.Create;
  fRefCountDisposable := TRefCountDisposable.Create(fm);

  firstWindow := CreateWindow;
  Observer.OnNext(firstWindow);
  fm.Disposable := fParent.fSource.Subscribe(Self);
  Result := fRefCountDisposable;
end;

function TWindow<TSource>.TSink.CreateWindow: IObservable<TSource>;
var
  s: ISubject<TSource>;
begin
  s := TSubject<TSource>.Create;
  fQueue.Enqueue(s);
  Result := TWindowObservable<TSource>.Create(s, fRefCountDisposable);
end;

procedure TWindow<TSource>.TSink.OnNext(const value: TSource);
var
  s: ISubject<TSource>;
  c: Integer;
  newWindow: IObservable<TSource>;
begin
  for s in fQueue do
    s.OnNext(value);

  c := fn - fParent.fCount + 1;
  if (c >= 0) and (c mod fParent.fSkip = 0) then
  begin
    s := fQueue.Dequeue;
    s.OnCompleted;
  end;

  Inc(fn);
  if fn mod fParent.fSkip = 0 then
  begin
    newWindow := CreateWindow;
    Observer.OnNext(newWindow);
  end;
end;

{$ENDREGION}


{$REGION 'TWindow<TSource>.TBoundedWindowImpl'}

constructor TWindow<TSource>.TBoundedWindowImpl.Create(const parent: TWindow<TSource>;
  const observer: IObserver<IObservable<TSource>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

procedure TWindow<TSource>.TBoundedWindowImpl.CreateTimer(id: Integer);
var
  m: TSingleAssignmentDisposable;
begin
  m := TSingleAssignmentDisposable.Create;
  fTimerD.Disposable := m;

  m.Disposable := fParent.fScheduler.Schedule(id, fParent.fTimeSpan, Tick);
end;

destructor TWindow<TSource>.TBoundedWindowImpl.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TWindow<TSource>.TBoundedWindowImpl.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fs.OnCompleted;
    Observer.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TBoundedWindowImpl.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    fs.OnError(error);
    Observer.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<TSource>.TBoundedWindowImpl.OnNext(const value: TSource);
var
  newWindow: Boolean;
  newId: Integer;
begin
  newWindow := False;
  newId := 0;
  MonitorEnter(Self);
  try
    fs.OnNext(value);
    Inc(fn);
    if fn = fParent.fCount then
    begin
      newWindow := True;
      fn := 0;
      Inc(fWindowId);
      newId := fWindowId;

      fs.OnCompleted;
      fs := TSubject<TSource>.Create;
      Observer.OnNext(TWindowObservable<TSource>.Create(fs, fRefCountDisposable));
    end;
  finally
    MonitorExit(Self);
  end;

  if newWindow then
    CreateTimer(newId);
end;

function TWindow<TSource>.TBoundedWindowImpl.Run: IDisposable;
var
  groupDisposable: TCompositeDisposable;
begin
  fs := nil;
  fn := 0;
  fWindowId := 0;

  fTimerD := TSerialDisposable.Create;
  groupDisposable := TCompositeDisposable.Create([fTimerD]);
  fRefCountDisposable := TRefCountDisposable.Create(groupDisposable);

  fs := TSubject<TSource>.Create;
  Observer.OnNext(TWindowObservable<TSource>.Create(fs, fRefCountDisposable));
  CreateTimer(0);

  groupDisposable.Add(fParent.fSource.Subscribe(Self));

  Result := fRefCountDisposable;
end;

function TWindow<TSource>.TBoundedWindowImpl.Tick(const scheduler: IScheduler;
  const id: TValue): IDisposable;
var
  d: IDisposable;
  newId: Integer;
begin
  d := Disposable.Empty;
  newId := 0;
  MonitorEnter(Self);
  try
    if id <> fWindowId then
      Exit(d);

    fn := 0;
    Inc(fWindowId);
    newId := fWindowId;

    fs.OnCompleted;
    fs := TSubject<TSource>.Create;
    Observer.OnNext(TWindowObservable<TSource>.Create(fs, fRefCountDisposable));
  finally
    MonitorExit(Self);
  end;

  CreateTimer(newId);
  Result := d;
end;

{$ENDREGION}


end.
