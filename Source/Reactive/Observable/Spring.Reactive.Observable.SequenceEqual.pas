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

unit Spring.Reactive.Observable.SequenceEqual;

interface

uses
  Generics.Defaults,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSequenceEqual<TSource> = class
  public type
    TObservable = class // TODO
    end;

    TEnumerable = class(TProducer<Boolean>)
    private
      fFirst: IObservable<TSource>;
      fSecond: IEnumerable<TSource>;
      fComparer: IEqualityComparer<TSource>;

      type
        TSink = class(TSink<Boolean>, IObserver<TSource>)
        private
          fComparer: IEqualityComparer<TSource>;
          fEnumerator: IEnumerator<TSource>;
        public
          constructor Create(
            const comparer: IEqualityComparer<TSource>;
            const observer: IObserver<Boolean>;
            const cancel: IDisposable);
          function Run(const parent: TEnumerable): IDisposable;
          procedure OnNext(const value: TSource);
          procedure OnError(const error: Exception);
          procedure OnCompleted;
        end;
    protected
      function CreateSink(
        const observer: IObserver<Boolean>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(
        const first: IObservable<TSource>;
        const second: IEnumerable<TSource>;
        const comparer: IEqualityComparer<TSource>);
    end;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TSequenceEqual<TSource>.TEnumerable'}

constructor TSequenceEqual<TSource>.TEnumerable.Create(
  const first: IObservable<TSource>; const second: IEnumerable<TSource>;
  const comparer: IEqualityComparer<TSource>);
begin
  inherited Create;
  fFirst := first;
  fSecond := second;
  fComparer := comparer;
end;

function TSequenceEqual<TSource>.TEnumerable.CreateSink(
  const observer: IObserver<Boolean>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fComparer, observer, cancel);
end;

function TSequenceEqual<TSource>.TEnumerable.Run(
  const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self);
end;

{$ENDREGION}


{$REGION 'TSequenceEqual<TSource>.TEnumerable.TSink'}

constructor TSequenceEqual<TSource>.TEnumerable.TSink.Create(
  const comparer: IEqualityComparer<TSource>;
  const observer: IObserver<Boolean>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fComparer := comparer;
end;

function TSequenceEqual<TSource>.TEnumerable.TSink.Run(
  const parent: TEnumerable): IDisposable;
begin
  try
    fEnumerator := parent.fSecond.GetEnumerator;
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit(Disposable.Empty);
    end;
  end;

  Result := TStableCompositeDisposable.Create(
    parent.fFirst.Subscribe(Self),
    Disposable.Create(procedure begin fEnumerator := nil end)); // TODO: consider making IEnumerator inherit or implement IDisposable
end;

procedure TSequenceEqual<TSource>.TEnumerable.TSink.OnNext(const value: TSource);
var
  equal: Boolean;
  current: TSource;
begin
  equal := False;

  try
    if fEnumerator.MoveNext then
    begin
      current := fEnumerator.Current;
      equal := fComparer.Equals(value, current);
    end;
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if not equal then
  begin
    Observer.OnNext(False);
    Observer.OnCompleted;
    Dispose;
  end;
end;

procedure TSequenceEqual<TSource>.TEnumerable.TSink.OnError(
  const error: Exception);
begin
  Observer.OnError(error);
  Dispose;
end;

procedure TSequenceEqual<TSource>.TEnumerable.TSink.OnCompleted;
var
  hasNext: Boolean;
begin
  hasNext := False;

  try
    hasNext := fEnumerator.MoveNext;
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnNext(not hasNext);
  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


end.
