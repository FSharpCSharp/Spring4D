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
    fIsDisposed: Boolean;
    fIsStopped: Boolean;
    fValue: T;
    fHasValue: Boolean;
    fError: Exception;
    procedure CheckDisposed;
  public
    procedure Dispose; override;

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
        destructor Destroy; override;

        procedure Dispose; override;
      end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TAsyncSubject<T>'}

procedure TAsyncSubject<T>.Dispose;
begin
  MonitorEnter(Self);
  try
    Lock(Self);

    fIsDisposed := True;
    fObservers := nil;
    fError := nil;
    fValue := Default(T);
  finally
    MonitorExit(Self);
  end;
end;

procedure TAsyncSubject<T>.OnCompleted;
var
  observers: TArray<IObserver<T>>;
  value: T;
  hasValue: Boolean;
  observer: IObserver<T>;
begin
  MonitorEnter(Self);
  try
    CheckDisposed;

    if not fIsStopped then
    begin
      observers := fObservers;
      fObservers := nil;
      fIsStopped := True;
      value := fValue;
      hasValue := fHasValue;
    end;
  finally
    MonitorExit(Self);
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
  Guard.CheckNotNull(error, 'error');

  MonitorEnter(Self);
  try
    CheckDisposed;

    if not fIsStopped then
    begin
      observers := fObservers;
      fObservers := nil;
      fIsStopped := True;
      fError := error;
    end;
  finally
    MonitorExit(Self);
  end;

  for observer in observers do
    observer.OnError(error);
end;

procedure TAsyncSubject<T>.OnNext(const value: T);
begin
  MonitorEnter(Self);
  try
    CheckDisposed;

    if not fIsStopped then
    begin
      fValue := value;
      fHasValue := True;
    end;
  finally
    MonitorExit(Self);
  end;
end;

procedure TAsyncSubject<T>.CheckDisposed;
begin
  if fIsDisposed then
    raise EObjectDisposedException.Create('');
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

  MonitorEnter(Self);
  try
    CheckDisposed;

    if not fIsStopped then
    begin
      fObservers := TArray.Add<IObserver<T>>(fObservers, observer);
      Exit(TSubscription.Create(Self, observer));
    end;

    error := fError;
    hasValue := fHasValue;
    value := fValue;
  finally
    MonitorExit(Self);
  end;

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

destructor TAsyncSubject<T>.TSubscription.Destroy;
begin
  fSubject._Release;
  inherited;
end;

procedure TAsyncSubject<T>.TSubscription.Dispose;
begin
  if Assigned(fObserver) then
  begin
    MonitorEnter(fSubject);
    try
      if not fSubject.fIsDisposed and Assigned(fObserver) then
      begin
        fSubject.fObservers := TArray.Remove<IObserver<T>>(fSubject.fObservers, fObserver);
        fObserver := nil;
      end;
    finally
      MonitorExit(fSubject);
    end;
  end;
end;

{$ENDREGION}


end.
