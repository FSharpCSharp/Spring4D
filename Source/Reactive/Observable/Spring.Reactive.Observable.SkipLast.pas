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

unit Spring.Reactive.Observable.SkipLast;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSkipLast<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fCount: Integer;
//    fDuration: TTimeSpan; // TODO implement interval
//    fScheduler: IScheduler;
    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TSkipLast<TSource>;
        fQueue: IQueue<TSource>;
      public
        constructor Create(const parent: TSkipLast<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; const count: Integer);
  end;

implementation


{$REGION 'TSkipLast<TSource>'}

constructor TSkipLast<TSource>.Create(const source: IObservable<TSource>;
  const count: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
end;

function TSkipLast<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TSkipLast<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TSkipLast<TSource>.TSink'}

constructor TSkipLast<TSource>.TSink.Create(const parent: TSkipLast<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fQueue := TCollections.CreateQueue<TSource>;
end;

destructor TSkipLast<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipLast<TSource>.TSink.OnNext(const value: TSource);
begin
  fQueue.Enqueue(value);
  if fQueue.Count > fParent.fCount then
    Observer.OnNext(fQueue.Dequeue);
end;

{$ENDREGION}


end.
