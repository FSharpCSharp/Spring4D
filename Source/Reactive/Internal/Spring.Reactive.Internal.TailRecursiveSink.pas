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

unit Spring.Reactive.Internal.TailRecursiveSink;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Reactive,
  Spring.Reactive.Internal.Sink;

type
  TTailRecursiveSink<TSource> = class abstract(TSink<TSource>, IObserver<TSource>)
  private
    fIsDisposed: Boolean;
    fSubscription: ISerialDisposable;
//    fGate: AsyncLock;
    fStack: IStack<IEnumerator<IObservable<TSource>>>;
    fLength: IStack<Integer>;
    fRecurse: Action;
    function TryGetEnumerator(const sources: TArray<IObservable<TSource>>;
      out e: IEnumerator<IObservable<TSource>>): Boolean;
  public
    procedure Dispose; override;
    function Run(const sources: TArray<IObservable<TSource>>): IDisposable;
    procedure OnNext(const value: TSource); virtual; abstract;
  end;

  TArrayEnumerator<T> = class(TEnumeratorBase<T>)
  private
    fSource: TArray<T>;
    fIndex: Integer;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(const source: TArray<T>);
    function MoveNext: Boolean; override;
  end;

implementation

uses
  Spring.Reactive.Concurrency.SchedulerDefaults,
  Spring.Reactive.Disposables;


{$REGION 'TTailRecursiveSink<TSource>'}

procedure TTailRecursiveSink<TSource>.Dispose;
var
  e: IEnumerator<IObservable<TSource>>;
begin
  while fStack.Count > 0 do
  begin
    e := fStack.Pop;
    fLength.Pop;

    e := nil;
  end;

  fIsDisposed := True;
end;

function TTailRecursiveSink<TSource>.Run(
  const sources: TArray<IObservable<TSource>>): IDisposable;
var
  e: IEnumerator<IObservable<TSource>>;
  cancelable: IDisposable;
begin
  fIsDisposed := False;
  fSubscription := TSerialDisposable.Create;
//  fGate := TAsyncLock.Create;
  fStack := TCollections.CreateStack<IEnumerator<IObservable<TSource>>>;
  fLength := TCollections.CreateStack<Integer>;
  e := nil;
  if not TryGetEnumerator(sources, e) then
    Exit(Disposable.Empty);

  fStack.Push(e);
  fLength.Push(Length(sources));

  cancelable := SchedulerDefaults.TailRecursion.Schedule(
    procedure(const _self: Action)
    begin
      fRecurse := _self;
//      fGate.Wait(MoveNext);
    end);

  Result := TStableCompositeDisposable.Create([
    fSubscription, cancelable, Disposable.Create(
    procedure
    begin
//      fGate.Wait(Dispose);
    end)]);
end;

function TTailRecursiveSink<TSource>.TryGetEnumerator(
  const sources: TArray<IObservable<TSource>>;
  out e: IEnumerator<IObservable<TSource>>): Boolean;
begin
  e := TArrayEnumerator<IObservable<TSource>>.Create(sources);
  Result := True;
end;

{$ENDREGION}


{$REGION 'TArrayEnumerator<T>' }

constructor TArrayEnumerator<T>.Create(const source: TArray<T>);
begin
  inherited Create;
  fSource := source;
  fIndex := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := fSource[fIndex];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Result := fIndex < High(fSource);
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


end.
