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

unit Spring.Reactive.Subjects.Subject;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase,
  Spring.Reactive.Subjects.SubjectBase;

type
  TSubject<T> = class(TSubjectBase<T>, IDisposable)
  private
    fObserver: IObserver<T>;
    // getter function is necessary to ensure the
    // instance is alive during any calls made on it
    function GetObserver: IObserver<T>;
    procedure Unsubscribe(const observer: IObserver<T>);
  protected
    property Observer: IObserver<T> read GetObserver;
  public
    constructor Create;
    procedure Dispose; override;

    procedure OnNext(const value: T); override;
    procedure OnError(const error: Exception); override;
    procedure OnCompleted; override;

    function Subscribe(const observer: IObserver<T>): IDisposable; override;

    type
      TSubscription = class(TDisposableObject)
      private
        fSubject: TSubject<T>;//unsafe
        fObserver: IObserver<T>;
      public
        constructor Create(const subject: TSubject<T>; const observer: IObserver<T>);
        destructor Destroy; override;
        procedure Dispose; override;
      end;
  end;

  TSubject = record
  private type
    TAnonymousSubject<TSource, TResult> = class(TObservableBase<TResult>,
      ISubject<TSource, TResult>, IObserver<TSource>)
    private
      fObserver: IObserver<TSource>;
      fObservable: IObservable<TResult>;
      // getter function is necessary to ensure the
      // instance is alive during any calls made on it
      function GetObserver: IObserver<TSource>;
    protected
      property Observer: IObserver<TSource> read GetObserver;
    public
      constructor Create(const observer: IObserver<TSource>;
        const observable: IObservable<TResult>);
      procedure Dispose;

      procedure OnNext(const value: TSource);
      procedure OnError(const error: Exception);
      procedure OnCompleted;

      function Subscribe(const observer: IObserver<TResult>): IDisposable; override;
    end;

    TAnonymousSubject<T> = class(TAnonymousSubject<T, T>, ISubject<T>);
  public
    class function Create<TSource, TResult>(const observer: IObserver<TSource>;
      const observable: IObservable<TResult>): ISubject<TSource, TResult>; overload; static;
    class function Create<T>(const observer: IObserver<T>;
      const observable: IObservable<T>): ISubject<T>; overload; static;

//    class function Synchronize
  end;

implementation

uses
  Spring.Reactive.Disposables,
  Spring.Reactive.Internal.Observers;


{$REGION 'TSubject<T>'}

constructor TSubject<T>.Create;
begin
  inherited Create;
  fObserver := TNopObserver<T>.Instance;
end;

procedure TSubject<T>.Dispose;
begin
  fObserver := TDisposedObserver<T>.Instance;
end;

function TSubject<T>.GetObserver: IObserver<T>;
begin
  Result := fObserver;
end;

procedure TSubject<T>.OnCompleted;
var
  oldObserver, newObserver: IObserver<T>;
begin
  newObserver := TDoneObserver<T>.Completed;

  repeat
    oldObserver := fObserver;
    if (oldObserver = TDisposedObserver<T>.Instance) or (oldObserver is TDoneObserver<T>) then
      Break;
  until TInterlocked.CompareExchange<IObserver<T>>(fObserver, newObserver, oldObserver) = oldObserver;

  oldObserver.OnCompleted;
end;

procedure TSubject<T>.OnError(const error: Exception);
var
  oldObserver, newObserver: IObserver<T>;
begin
  Guard.CheckNotNull(error, 'error');

  newObserver := TDoneObserver<T>.Create(error);

  repeat
    oldObserver := fObserver;
    if (oldObserver = TDisposedObserver<T>.Instance) or (oldObserver is TDoneObserver<T>) then
      Break;
  until TInterlocked.CompareExchange<IObserver<T>>(fObserver, newObserver, oldObserver) = oldObserver;

  oldObserver.OnError(error);
end;

procedure TSubject<T>.OnNext(const value: T);
begin
  Observer.OnNext(value);
end;

function TSubject<T>.Subscribe(const observer: IObserver<T>): IDisposable;
var
  oldObserver, newObserver: IObserver<T>;
  done: TDoneObserver<T>;
  obs: TObserver<T>;
begin
  Guard.CheckNotNull(observer, 'observer');

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
  oldObserver := nil;
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

destructor TSubject<T>.TSubscription.Destroy;
begin
  if Assigned(fSubject) then
    fSubject._Release;
  inherited;
end;

procedure TSubject<T>.TSubscription.Dispose;
var
  observer: IObserver<T>;
begin
  observer := TInterlocked.Exchange<IObserver<T>>(fObserver, nil);
  if observer = nil then
    Exit;

  fSubject.Unsubscribe(observer);
  fSubject._Release;
  fSubject := nil;
end;

{$ENDREGION}


{$REGION 'TSubject'}

class function TSubject.Create<TSource, TResult>(
  const observer: IObserver<TSource>;
  const observable: IObservable<TResult>): ISubject<TSource, TResult>;
begin
  Guard.CheckNotNull(observer, 'observer');
  Guard.CheckNotNull(observable, 'observable');

  Result := TAnonymousSubject<TSource, TResult>.Create(observer, observable);
end;

class function TSubject.Create<T>(const observer: IObserver<T>;
  const observable: IObservable<T>): ISubject<T>;
begin
  Guard.CheckNotNull(observer, 'observer');
  Guard.CheckNotNull(observable, 'observable');

  Result := TAnonymousSubject<T>.Create(observer, observable);
end;

{$ENDREGION}


{$REGION 'TSubject.TAnonymousSubject<TSource, TResult>'}

constructor TSubject.TAnonymousSubject<TSource, TResult>.Create(
  const observer: IObserver<TSource>; const observable: IObservable<TResult>);
begin
  inherited Create;
  fObserver := observer;
  fObservable := observable;
end;

procedure TSubject.TAnonymousSubject<TSource, TResult>.Dispose;
begin
end;

function TSubject.TAnonymousSubject<TSource, TResult>.GetObserver: IObserver<TSource>;
begin
  Result := fObserver;
end;

procedure TSubject.TAnonymousSubject<TSource, TResult>.OnCompleted;
begin
  Observer.OnCompleted;
end;

procedure TSubject.TAnonymousSubject<TSource, TResult>.OnError(
  const error: Exception);
begin
  Guard.CheckNotNull(error, 'error');
  Observer.OnError(error);
end;

procedure TSubject.TAnonymousSubject<TSource, TResult>.OnNext(
  const value: TSource);
begin
  Observer.OnNext(value);
end;

function TSubject.TAnonymousSubject<TSource, TResult>.Subscribe(
  const observer: IObserver<TResult>): IDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');
  Result := fObservable.Subscribe(observer);
end;

{$ENDREGION}


end.
