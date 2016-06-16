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

unit Spring.Reactive.Testing.MockObserver;

interface

uses
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Notification,
  Spring.Reactive.Testing,
  Spring.Reactive.Testing.Recorded,
  Spring.Reactive.Testing.TestScheduler;

type
  TMockObserver<T> = class(TInterfacedObject, ITestableObserver<T>)
  private
    fScheduler: TTestScheduler;
    fMessages: IList<TRecorded<INotification<T>>>;
    function GetMessages: IList<TRecorded<INotification<T>>>;
  public
    constructor Create(const scheduler: TTestScheduler);
    procedure Dispose;

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;
  end;

implementation

uses
  Spring;

{$REGION 'TMockObserver<T>'}

constructor TMockObserver<T>.Create(const scheduler: TTestScheduler);
begin
  Guard.CheckNotNull(scheduler, 'scheduler');
  inherited Create;
  fScheduler := scheduler;
  fMessages := TCollections.CreateList<TRecorded<INotification<T>>>;
end;

procedure TMockObserver<T>.Dispose;
begin
end;

function TMockObserver<T>.GetMessages: IList<TRecorded<INotification<T>>>;
begin
  Result := fMessages;
end;

procedure TMockObserver<T>.OnNext(const value: T);
begin
  fMessages.Add(TRecorded<INotification<T>>.Create(fScheduler.Clock, TNotification.CreateOnNext<T>(value)));
end;

procedure TMockObserver<T>.OnError(const error: Exception);
begin
  fMessages.Add(TRecorded<INotification<T>>.Create(fScheduler.Clock, TNotification.CreateOnError<T>(error)));
end;

procedure TMockObserver<T>.OnCompleted;
begin
  fMessages.Add(TRecorded<INotification<T>>.Create(fScheduler.Clock, TNotification.CreateOnCompleted<T>));
end;

{$ENDREGION}


end.
