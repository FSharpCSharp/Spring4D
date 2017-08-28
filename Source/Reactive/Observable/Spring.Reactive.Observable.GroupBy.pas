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

unit Spring.Reactive.Observable.GroupBy;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TGroupBy<TSource, TKey, TElement> = class(TProducer<IGroupedObservable<TKey, TElement>>)
  private
    fSource: IObservable<TSource>;
    fKeySelector: Func<TSource, TKey>;
    fElementSelector: Func<TSource, TElement>;
    fCapacity: Integer; // Nullable in C# - using 0 as null here
    fComparer: IEqualityComparer<TKey>;
    fGroupDisposable: ICompositeDisposable;
    fRefCountDisposable: IRefCountDisposable;

    type
      TSink = class(TSink<IGroupedObservable<TKey, TElement>>, IObserver<TSource>)
      private
        fKeySelector: Func<TSource, TKey>;
        fElementSelector: Func<TSource, TElement>;
        fMap: IDictionary<TKey, ISubject<TElement>>;
        fRefCountDisposable: IRefCountDisposable;
        fNull: ISubject<TElement>;
      public
        constructor Create(const parent: TGroupBy<TSource, TKey, TElement>;
          const observer: IObserver<IGroupedObservable<TKey, TElement>>;
          const cancel: IDisposable);
        function Run(const source: IObservable<TSource>): IDisposable;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function CreateSink(const observer: IObserver<IGroupedObservable<TKey, TElement>>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const capacity: Integer; const comparer: IEqualityComparer<TKey>);
  end;

implementation

uses
  Spring.Reactive.Disposables,
  Spring.Reactive.GroupedObservable,
  Spring.Reactive.Subjects.Subject;


{$REGION 'TGroupBy<TSource, TKey, TElement>'}

constructor TGroupBy<TSource, TKey, TElement>.Create(
  const source: IObservable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>; const capacity: Integer;
  const comparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fCapacity := capacity;
  fComparer := comparer;
end;

function TGroupBy<TSource, TKey, TElement>.CreateSink(
  const observer: IObserver<IGroupedObservable<TKey, TElement>>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TGroupBy<TSource, TKey, TElement>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fSource);
end;

{$ENDREGION}


{$REGION 'TGroupBy<TSource, TKey, TElement>.TSink'}

constructor TGroupBy<TSource, TKey, TElement>.TSink.Create(
  const parent: TGroupBy<TSource, TKey, TElement>;
  const observer: IObserver<IGroupedObservable<TKey, TElement>>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fKeySelector := parent.fKeySelector;
  fElementSelector := parent.fElementSelector;
  fMap := TCollections.CreateDictionary<TKey, ISubject<TElement>>(parent.fCapacity, parent.fComparer);
end;

function TGroupBy<TSource, TKey, TElement>.TSink.Run(
  const source: IObservable<TSource>): IDisposable;
var
  sourceSubscription: ISingleAssignmentDisposable;
begin
  sourceSubscription := TSingleAssignmentDisposable.Create;
  fRefCountDisposable := TRefCountDisposable.Create(sourceSubscription);
  sourceSubscription.Disposable := source.Subscribe(Self);
  Result := fRefCountDisposable;
end;

procedure TGroupBy<TSource, TKey, TElement>.TSink.OnNext(const value: TSource);
var
  key: TKey;
  fireNewMapEntry: Boolean;
  writer: ISubject<TElement>;
  group: IGroupedObservable<TKey, TElement>;
  element: TElement;
begin
  try
    key := fKeySelector(value);
  except
    on e: Exception do
    begin
      OnError(e);
      Exit;
    end;
  end;

  fireNewMapEntry := False;
  try
    if not fMap.TryGetValue(key, writer) then
    begin
      writer := TSubject<TElement>.Create;
      fMap.Add(key, writer);
      fireNewMapEntry := True;
    end;
  except
    on e: Exception do
    begin
      OnError(e);
      Exit;
    end;
  end;

  if fireNewMapEntry then
  begin
    group := TGroupedObservable<TKey, TElement>.Create(key, writer, fRefCountDisposable);
    Observer.OnNext(group);
  end;

  try
    element := fElementSelector(value)
  except
    on e: Exception do
    begin
      OnError(e);
      Exit;
    end;
  end;

  writer.OnNext(element);
end;

procedure TGroupBy<TSource, TKey, TElement>.TSink.OnCompleted;
var
  w: ISubject<TElement>;
begin
  if Assigned(fNull) then
    fNull.OnCompleted;

  for w in fMap.Values do
    w.OnCompleted;

  Observer.OnCompleted;
  Dispose;
end;

procedure TGroupBy<TSource, TKey, TElement>.TSink.OnError(
  const error: Exception);
var
  w: ISubject<TElement>;
begin
  if Assigned(fNull) then
    fNull.OnError(error);

  for w in fMap.Values do
    w.OnError(error);

  Observer.OnError(error);
  Dispose;
end;

{$ENDREGION}


end.
