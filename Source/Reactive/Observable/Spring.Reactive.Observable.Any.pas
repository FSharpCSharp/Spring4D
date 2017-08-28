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

unit Spring.Reactive.Observable.Any;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TAny<TSource> = class
  public type
    TCount = class(TProducer<Boolean>)
    private
      fSource: IObservable<TSource>;

      type
        TSink = class(TSink<Boolean>, IObserver<TSource>)
        public
          procedure OnNext(const value: TSource);
          procedure OnCompleted;
        end;
    protected
      function CreateSink(const observer: IObserver<Boolean>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
    public
      constructor Create(const source: IObservable<TSource>);
    end;

    TPredicate = class(TProducer<Boolean>)
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
      constructor Create(const source: IObservable<TSource>;
        const predicate: Predicate<TSource>); overload;
    end;
  end;

implementation


{$REGION 'TAny<TSource>.TCount'}

constructor TAny<TSource>.TCount.Create(const source: IObservable<TSource>);
begin
  inherited Create;
  fSource := source;
end;

function TAny<TSource>.TCount.CreateSink(const observer: IObserver<Boolean>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TAny<TSource>.TCount.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TAny<TSource>.TCount.TSink'}

procedure TAny<TSource>.TCount.TSink.OnNext(const value: TSource);
begin
  Observer.OnNext(True);
  Observer.OnCompleted;
  Dispose;
end;

procedure TAny<TSource>.TCount.TSink.OnCompleted;
begin
  Observer.OnNext(False);
  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


{$REGION 'TAny<TSource>.TPredicate'}

constructor TAny<TSource>.TPredicate.Create(const source: IObservable<TSource>;
  const predicate: Predicate<TSource>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

function TAny<TSource>.TPredicate.CreateSink(const observer: IObserver<Boolean>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(fPredicate, observer, cancel);
end;

function TAny<TSource>.TPredicate.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TAny<TSource>.TPredicate.TSink'}

constructor TAny<TSource>.TPredicate.TSink.Create(
  const predicate: Predicate<TSource>; const observer: IObserver<Boolean>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fPredicate := predicate;
end;

procedure TAny<TSource>.TPredicate.TSink.OnNext(const value: TSource);
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

  if res then
  begin
    Observer.OnNext(True);
    Observer.OnCompleted;
    Dispose;
  end;
end;

procedure TAny<TSource>.TPredicate.TSink.OnCompleted;
begin
  Observer.OnNext(False);
  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


end.
