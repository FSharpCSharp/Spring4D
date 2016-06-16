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
  TWindow<T> = class(TProducer<IObservable<T>>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
    fSkip: Integer;
    fTimeSpan: TTimeSpan;
    fScheduler: IScheduler;

    type
      TSinkBase = class(TSink<IObservable<T>>)
      public
        function Run: IDisposable; virtual; abstract;
      end;

      TSink = class(TSinkBase, IObserver<T>)
      private
        fParent: TWindow<T>;
        fQueue: IQueue<ISubject<T>>;
        fn: Integer;
        fm: ISingleAssignmentDisposable;
        fRefCountDisposable: IRefCountDisposable;
        function CreateWindow: IObservable<T>;
      public
        constructor Create(const parent: TWindow<T>;
          const observer: IObserver<IObservable<T>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable; override;
        procedure OnNext(const value: T);
      end;

      TBoundedWindowImpl = class(TSinkBase, IObserver<T>)
      private
        fParent: TWindow<T>;
        fs: ISubject<T>;
        fn: Integer;
        fWindowId: Integer;
        fTimerD: ISerialDisposable;
        fRefCountDisposable: IRefCountDisposable;
        procedure CreateTimer(id: Integer);
        function Tick(const scheduler: IScheduler; const id: TValue): IDisposable;
      public
        constructor Create(const parent: TWindow<T>;
          const observer: IObserver<IObservable<T>>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable; override;
        procedure OnNext(const value: T);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<IObservable<T>>;
      const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const count, skip: Integer);
  end;

  TWindowObservable<T> = class(TAddRef<T>)
  end;

implementation

uses
  Rtti, // H2443
  Spring.Reactive.Disposables,
  Spring.Reactive.Subjects.Subject;


{$REGION 'TWindow<T>'}

constructor TWindow<T>.Create(const source: IObservable<T>; const count,
  skip: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
  fSkip := skip;
end;

function TWindow<T>.Run(const observer: IObserver<IObservable<T>>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSinkBase;
begin
  if fScheduler = nil then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    Result := sink.Run;
  end
  else if fCount > 0 then
  begin
    sink := TBoundedWindowImpl.Create(Self, observer, cancel);
    setSink(sink);
    Result := sink.Run;
  end;

(*
  else
  {
      if (_timeSpan == _timeShift)
      {
          var sink = new TimeShiftImpl(this, observer, cancel);
          setSink(sink);
          return sink.Run();
      }
      else
      {
          var sink = new WindowImpl(this, observer, cancel);
          setSink(sink);
          return sink.Run();
      }
  }
}
*)
end;



{$ENDREGION}


{$REGION 'TWindow<T>.TSink'}

constructor TWindow<T>.TSink.Create(const parent: TWindow<T>;
  const observer: IObserver<IObservable<T>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TWindow<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TWindow<T>.TSink.Run: IDisposable;
var
  firstWindow: IObservable<T>;
begin
  fQueue := TCollections.CreateQueue<ISubject<T>>;
  fn := 0;
  fm := TSingleAssignmentDisposable.Create;
  fRefCountDisposable := TRefCountDisposable.Create(fm);

  firstWindow := CreateWindow;
  fObserver.OnNext(firstWindow);
  fm.Disposable := fParent.fSource.Subscribe(Self);
  Result := fRefCountDisposable;
end;

function TWindow<T>.TSink.CreateWindow: IObservable<T>;
var
  s: ISubject<T>;
begin
  s := TSubject<T>.Create;
  fQueue.Enqueue(s);
  Result := TWindowObservable<T>.Create(s, fRefCountDisposable);
end;

procedure TWindow<T>.TSink.OnNext(const value: T);
var
  s: ISubject<T>;
  c: Integer;
  newWindow: IObservable<T>;
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
    fObserver.OnNext(newWindow);
  end;
end;

{$ENDREGION}


{$REGION 'TWindow<T>.TBoundedWindowImpl'}

constructor TWindow<T>.TBoundedWindowImpl.Create(const parent: TWindow<T>;
  const observer: IObserver<IObservable<T>>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

procedure TWindow<T>.TBoundedWindowImpl.CreateTimer(id: Integer);
var
  m: TSingleAssignmentDisposable;
begin
  m := TSingleAssignmentDisposable.Create;
  fTimerD.Disposable := m;

  m.Disposable := fParent.fScheduler.Schedule(id, fParent.fTimeSpan, Tick);
end;

destructor TWindow<T>.TBoundedWindowImpl.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TWindow<T>.TBoundedWindowImpl.OnCompleted;
begin
  MonitorEnter(Self);
  try
    fs.OnCompleted;
    fObserver.OnCompleted;
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<T>.TBoundedWindowImpl.OnError(const error: Exception);
begin
  MonitorEnter(Self);
  try
    fs.OnError(error);
    fObserver.OnError(error);
    Dispose;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWindow<T>.TBoundedWindowImpl.OnNext(const value: T);
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
      fs := TSubject<T>.Create;
      fObserver.OnNext(TWindowObservable<T>.Create(fs, fRefCountDisposable));
    end;
  finally
    MonitorExit(Self);
  end;

  if newWindow then
    CreateTimer(newId);
end;

function TWindow<T>.TBoundedWindowImpl.Run: IDisposable;
var
  groupDisposable: TCompositeDisposable;
begin
  fs := nil;
  fn := 0;
  fWindowId := 0;

  fTimerD := TSerialDisposable.Create;
  groupDisposable := TCompositeDisposable.Create([fTimerD]);
  fRefCountDisposable := TRefCountDisposable.Create(groupDisposable);

  fs := TSubject<T>.Create;
  fObserver.OnNext(TWindowObservable<T>.Create(fs, fRefCountDisposable));
  CreateTimer(0);

  groupDisposable.Add(fParent.fSource.Subscribe(Self));

  Result := fRefCountDisposable;
end;

function TWindow<T>.TBoundedWindowImpl.Tick(const scheduler: IScheduler;
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
    fs := TSubject<T>.Create;
    fObserver.OnNext(TWindowObservable<T>.Create(fs, fRefCountDisposable));
  finally
    MonitorExit(Self);
  end;

  CreateTimer(newId);
  Result := d;
end;

{$ENDREGION}


end.
