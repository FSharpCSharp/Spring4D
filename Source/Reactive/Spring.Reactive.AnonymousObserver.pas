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

unit Spring.Reactive.AnonymousObserver;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObserverBase;

type
  TAnonymousObserver<T> = class sealed(TObserverBase<T>)
  private
    fOnNext: Action<T>;
    fOnError: Action<Exception>;
    fOnComplected: Action;
  public
    constructor Create(const onNext: Action<T>;
      const onError: Action<Exception>; const onCompleted: Action); overload;
    constructor Create(const onNext: Action<T>); overload;
    constructor Create(const onNext: Action<T>;
      const onError: Action<Exception>); overload;
    constructor Create(const onNext: Action<T>;
      const onCompleted: Action); overload;

    procedure OnNextCore(const value: T); override;
    procedure OnErrorCore(const error: Exception); override;
    procedure OnCompletedCore; override;
  end;

implementation

uses
  Spring.Reactive.Internal.Stubs;


{$REGION 'TAnonymousObserver<T>'}

constructor TAnonymousObserver<T>.Create(const onNext: Action<T>;
  const onError: Action<Exception>; const onCompleted: Action);
begin
  Guard.CheckNotNull(Assigned(onNext), 'onNext');
  Guard.CheckNotNull(Assigned(onError), 'onError');
  Guard.CheckNotNull(Assigned(onCompleted), 'onCompleted');

  inherited Create;
  fOnNext := onNext;
  fOnError := onError;
  fOnComplected := onCompleted;
end;

constructor TAnonymousObserver<T>.Create(const onNext: Action<T>);
begin
  Create(onNext, Stubs.Throw, Stubs.Nop);
end;

constructor TAnonymousObserver<T>.Create(const onNext: Action<T>;
  const onError: Action<Exception>);
begin
  Create(onNext, onError, Stubs.Nop);
end;

constructor TAnonymousObserver<T>.Create(const onNext: Action<T>;
  const onCompleted: Action);
begin
  Create(onNext, Stubs.Throw, onCompleted);
end;

procedure TAnonymousObserver<T>.OnNextCore(const value: T);
begin
  fOnNext(value);
end;

procedure TAnonymousObserver<T>.OnErrorCore(const error: Exception);
begin
  fOnError(error);
end;

procedure TAnonymousObserver<T>.OnCompletedCore;
begin
  fOnComplected();
end;

{$ENDREGION}


end.
