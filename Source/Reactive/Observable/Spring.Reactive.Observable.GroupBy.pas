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
        fParent: TGroupBy<TSource, TKey, TElement>;
        fMap: IDictionary<TKey, ISubject<TElement>>;
        fNull: ISubject<TElement>;
      public
        constructor Create(const parent: TGroupBy<TSource, TKey, TElement>;
          const observer: IObserver<IGroupedObservable<TKey, TElement>>;
          const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<IGroupedObservable<TKey, TElement>>;
      const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable; override;
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

function TGroupBy<TSource, TKey, TElement>.Run(
  const observer: IObserver<IGroupedObservable<TKey, TElement>>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  fGroupDisposable := TCompositeDisposable.Create([]);
  fRefCountDisposable := TRefCountDisposable.Create(fGroupDisposable);
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  fGroupDisposable.Add(fSource.Subscribe(sink));
  Result := fRefCountDisposable;
end;

{$ENDREGION}


{$REGION 'TGroupBy<TSource, TKey, TElement>.TSink'}

constructor TGroupBy<TSource, TKey, TElement>.TSink.Create(
  const parent: TGroupBy<TSource, TKey, TElement>;
  const observer: IObserver<IGroupedObservable<TKey, TElement>>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fMap := TCollections.CreateDictionary<TKey, ISubject<TElement>>(fParent.fCapacity, fParent.fComparer);
end;

destructor TGroupBy<TSource, TKey, TElement>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
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
    key := fParent.fKeySelector(value);
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
    group := TGroupedObservable<TKey, TElement>.Create(key, writer, fParent.fRefCountDisposable);
    fObserver.OnNext(group);
  end;

  try
    element := fParent.fElementSelector(value)
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

  fObserver.OnCompleted;
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

  fObserver.OnError(error);
  Dispose;
end;

{$ENDREGION}


end.
