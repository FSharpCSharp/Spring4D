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

unit Spring.Reactive.Observable.IgnoreElements;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TIgnoreElements<TSource> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;

    type
      TSink = class(TSink<TSource>, IObserver<TSource>)
      public
        procedure OnNext(const value: TSource);
      end;
  protected
      function CreateSink(const observer: IObserver<TSource>;
        const cancel: IDisposable): TObject; override;
      function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>);
  end;

implementation


{$REGION 'TIgnoreElements<TSource>'}

constructor TIgnoreElements<TSource>.Create(const source: IObservable<TSource>);
begin
  inherited Create;
  fSource := source;
end;

function TIgnoreElements<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TIgnoreElements<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := fSource.Subscribe(TSink(sink));
end;

{$ENDREGION}


{$REGION 'TIgnoreElements<TSource>.TSink'}

procedure TIgnoreElements<TSource>.TSink.OnNext(const value: TSource);
begin
end;

{$ENDREGION}


end.
