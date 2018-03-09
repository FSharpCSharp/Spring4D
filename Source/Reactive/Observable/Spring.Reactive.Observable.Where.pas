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

unit Spring.Reactive.Observable.Where;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TWhere<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fPredicate: Predicate<TSource>;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      private
        fParent: TWhere<TSource>;
      public
        constructor Create(const parent: TWhere<TSource>; const observer: IObserver<TSource>;
          const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>; const predicate: Predicate<TSource>);
  end;


implementation


{$REGION 'TWhere<TSource>'}

constructor TWhere<TSource>.Create(const source: IObservable<TSource>;
  const predicate: Predicate<TSource>);
begin
  inherited Create;
  fSource := source;
  fPredicate := predicate;
end;

function TWhere<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TWhere<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TWhere<TSource>.TSink'}

constructor TWhere<TSource>.TSink.Create(const parent: TWhere<TSource>;
  const observer: IObserver<TSource>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TWhere<TSource>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TWhere<TSource>.TSink.OnNext(const value: TSource);
var
  shouldRun: Boolean;
begin
  shouldRun := False;
  try
    shouldRun := fParent.fPredicate(value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  if shouldRun then
    Observer.OnNext(value);
end;

{$ENDREGION}


end.
