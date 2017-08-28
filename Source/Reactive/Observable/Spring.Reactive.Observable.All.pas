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

unit Spring.Reactive.Observable.All;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TAll<TSource> = class(TProducer<Boolean>)
  private
    fSource: IObservable<TSource>;
    fPredicate: Predicate<TSource>;

    type
      TSink = class(TSink<Boolean>, IObserver<TSource>)
      private
        fPredicate: Predicate<TSource>;
      public
        constructor Create(const predicate: Predicate<TSource>;
          const observer: IObserver<Boolean>; const cancel: IDisposable);
        procedure OnNext(const value: TSource);
        procedure OnCompleted;
      end;
  protected
    function CreateSink(const observer: IObserver<Boolean>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; const predicate: Predicate<TSource>);
  end;

implementation


{$REGION 'TAll<TSource>'}

constructor TAll<TSource>.Create(const source: IObservable<TSource>;
  const predicate: Predicate<TSource>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

function TAll<TSource>.CreateSink(const observer: IObserver<Boolean>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fPredicate, observer, cancel);
end;

function TAll<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TAll<TSource>.TSink'}

constructor TAll<TSource>.TSink.Create(const predicate: Predicate<TSource>;
  const observer: IObserver<Boolean>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fPredicate := predicate;
end;

procedure TAll<TSource>.TSink.OnNext(const value: TSource);
var
  res: Boolean;
begin
  res := False;
  try
    res := fPredicate(value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if not res then
  begin
    Observer.OnNext(False);
    Observer.OnCompleted;
    Dispose;
  end;
end;

procedure TAll<TSource>.TSink.OnCompleted;
begin
  Observer.OnNext(True);
  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


end.
