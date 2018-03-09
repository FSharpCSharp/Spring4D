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

unit Spring.Reactive.Observable.Select;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSelect<TSource, TResult> = class
  public type
    TSelector = class(TProducer<TResult>)
    private
      fSource: IObservable<TSource>;
      fSelector: Func<TSource,TResult>;

      type
        TSink = class(TSink<TResult>, IObserver<TSource>)
        private
          fSelector: Func<TSource,TResult>;
        public
          constructor Create(const selector: Func<TSource,TResult>;
            const observer: IObserver<TResult>; const cancel: IDisposable);
          procedure OnNext(const value: TSource);
        end;
    protected
      function CreateSink(const observer: IObserver<TResult>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>;
        const selector: Func<TSource, TResult>);
    end;
  end;

implementation


{$REGION 'TSelect<TSource, TResult>.TSelector'}

constructor TSelect<TSource, TResult>.TSelector.Create(
  const source: IObservable<TSource>; const selector: Func<TSource, TResult>);
begin
  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelect<TSource, TResult>.TSelector.CreateSink(
  const observer: IObserver<TResult>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fSelector, observer, cancel);
end;

function TSelect<TSource, TResult>.TSelector.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TSelect<TSource, TResult>.TSelector.TSink'}

constructor TSelect<TSource, TResult>.TSelector.TSink.Create(
  const selector: Func<TSource,TResult>; const observer: IObserver<TResult>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fSelector := selector;
end;

procedure TSelect<TSource, TResult>.TSelector.TSink.OnNext(const value: TSource);
var
  result: TResult;
begin
  try
    result := fSelector(value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnNext(result);
end;

{$ENDREGION}


end.
