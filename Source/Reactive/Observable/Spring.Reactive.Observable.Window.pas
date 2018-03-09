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

//    TTimeSliding = class(TProducer<IObservable<TSource>>)
//    private
//      fSource: IObservable<TSource>;
//      fTimeSpan: TTimeSpan;
//      fTimeShift: TTimeSpan;
//      fScheduler: IScheduler;
//    end;
//
//    TTimeHopping = class(TProducer<IObservable<TSource>>)
//    private
//      fSource: IObservable<TSource>;
//      fTimeSpan: TTimeSpan;
//      fScheduler: IScheduler;
//    end;
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


end.
