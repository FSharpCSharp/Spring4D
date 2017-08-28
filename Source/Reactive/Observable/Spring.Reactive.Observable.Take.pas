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

unit Spring.Reactive.Observable.Take;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TTake<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fCount: Integer;
//    fDuration: TTimeSpan;
//    fScheduler: IScheduler;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TTake<TSource>;
        fRemaining: Integer;
      public
        constructor Create(const parent: TTake<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; count: Integer);
  end;

implementation


{$REGION 'TTake<TSource>'}

constructor TTake<TSource>.Create(const source: IObservable<TSource>; count: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
end;

function TTake<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TTake<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TTake<TSource>.TSink'}

constructor TTake<TSource>.TSink.Create(const parent: TTake<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fRemaining := fParent.fCount;
end;

destructor TTake<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TTake<TSource>.TSink.OnNext(const value: TSource);
begin
  if fRemaining > 0 then
  begin
    Dec(fRemaining);
    Observer.OnNext(value);
    if fRemaining = 0 then
    begin
      Observer.OnCompleted;
      Dispose;
    end;
  end;
end;

{$ENDREGION}


end.
