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

unit Spring.Reactive.Observable.Distinct;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TDistinct<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fHashSet: ISet<TSource>;
      public
        constructor Create(const parent: TDistinct<TSource>;
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


{$REGION 'TDistinct<TSource>'}

constructor TDistinct<TSource>.Create(const source: IObservable<TSource>);
begin
  inherited Create;
  fSource := source;
end;

function TDistinct<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TDistinct<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TDistinct<TSource>.TSink'}

constructor TDistinct<TSource>.TSink.Create(const parent: TDistinct<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
//  fKeySelector := parent.fKeySelector;
  fHashSet := TCollections.CreateSet<TSource>//(parent.fComparer);
end;

procedure TDistinct<TSource>.TSink.OnNext(const value: TSource);
var
  hasAdded: Boolean;
begin
  // TODO keySelector and comparer
  try
    hasAdded := fHashSet.Add(value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if hasAdded then
    Observer.OnNext(value);
end;

{$ENDREGION}


end.
