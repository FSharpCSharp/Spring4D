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

unit Spring.Reactive.Concurrency.Synchronization.ObserveOn;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer;

type
  TObserveOn<TSource> = class
  public type
    TScheduler = class(TProducer<TSource>)
    private
      fSource: IObservable<TSource>;
      fScheduler: IScheduler;
    protected
      function CreateSink(const observer: IObserver<TSource>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>; const scheduler: IScheduler);
    end;
  end;

implementation

uses
  Spring.Reactive.Internal.ScheduledObserver;


{$REGION 'TObserveOn<TSource>.TScheduler'}

constructor TObserveOn<TSource>.TScheduler.Create(
  const source: IObservable<TSource>; const scheduler: IScheduler);
begin
  inherited Create;
  fSource := source;
  fScheduler := scheduler;
end;

function TObserveOn<TSource>.TScheduler.CreateSink(
  const observer: IObserver<TSource>; const cancel: IDisposable): TObject;
begin
  Result := TObserveOnObserver<TSource>.Create(fScheduler, observer, cancel);
end;

function TObserveOn<TSource>.TScheduler.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TObserveOnObserver<TSource>(sink));
end;

{$ENDREGION}


end.
