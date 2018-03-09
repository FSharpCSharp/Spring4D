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

unit Spring.Reactive.Observable.DistinctUntilChanged;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TDistinctUntilChanged<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
//    fKeySelector: Func<TSource,TKey>;
    fComparer: IEqualityComparer<TSource>;//TKey

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
//        fKeySelector: Func<TSource,TKey>;
        fComparer: IEqualityComparer<TSource>;//TKey
        fCurrentKey: TSource;//TKey
        fHasCurrentKey: Boolean;
      public
        constructor Create(const parent: TDistinctUntilChanged<TSource>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        procedure OnNext(const value: TSource);
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    // TODO keySelector and comparer
    constructor Create(const source: IObservable<TSource>);
  end;

implementation


{$REGION 'TDistinctUntilChanged<TSource>'}

constructor TDistinctUntilChanged<TSource>.Create(const source: IObservable<TSource>);
begin
  inherited Create;
  fSource := source;
  fComparer := TEqualityComparer<TSource>.Default;
end;

function TDistinctUntilChanged<TSource>.CreateSink(
  const observer: IObserver<TSource>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TDistinctUntilChanged<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TDistinctUntilChanged<TSource>.TSink'}

constructor TDistinctUntilChanged<TSource>.TSink.Create(
  const parent: TDistinctUntilChanged<TSource>; const observer: IObserver<TSource>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
//  fKeySelector := parent.fKeySelector;
  fComparer := parent.fComparer;

  fCurrentKey := Default(TSource);
  fHasCurrentKey := False;
end;

procedure TDistinctUntilChanged<TSource>.TSink.OnNext(const value: TSource);
var
  comparerEquals: Boolean;
begin
  if fHasCurrentKey then
  try
    comparerEquals := fComparer.Equals(fCurrentKey, value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if not fHasCurrentKey or not comparerEquals then
  begin
    fHasCurrentKey := True;
    fCurrentKey := value;
    Observer.OnNext(value);
  end;
end;

{$ENDREGION}


end.
