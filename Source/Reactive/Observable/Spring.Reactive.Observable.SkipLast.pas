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

unit Spring.Reactive.Observable.SkipLast;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSkipLast<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
//    fDuration: TTimeSpan; // TODO implement interval
//    fScheduler: IScheduler;
    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TSkipLast<T>;
        fQueue: IQueue<T>;
      public
        constructor Create(const parent: TSkipLast<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: T);
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; const count: Integer);
  end;

implementation


{$REGION 'TSkipLast<T>'}

constructor TSkipLast<T>.Create(const source: IObservable<T>;
  const count: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
end;

function TSkipLast<T>.Run(const observer: IObserver<T>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: IObserver<T>;
begin
//  if not Assigned(fScheduler) then
//  begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := fSource.Subscribe(sink);
//  end
//  else
//  begin
//
//  end;
end;

{$ENDREGION}


{$REGION 'TSkipLast<T>.TSink'}

constructor TSkipLast<T>.TSink.Create(const parent: TSkipLast<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fQueue := TCollections.CreateQueue<T>;
end;

destructor TSkipLast<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipLast<T>.TSink.OnNext(const value: T);
begin
  fQueue.Enqueue(value);
  if fQueue.Count > fParent.fCount then
    Observer.OnNext(fQueue.Dequeue);
end;

{$ENDREGION}


end.
