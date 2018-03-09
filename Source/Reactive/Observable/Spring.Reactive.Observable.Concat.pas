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

unit Spring.Reactive.Observable.Concat;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.ConcatSink,
  Spring.Reactive.Internal.Producer;

type
  TConcat<TSource> = class(TProducer<TSource>, IConcatenatable<TSource>)
  private
    fSources: IEnumerable<IObservable<TSource>>;

    type
      TSink = class(TConcatSink<TSource>)
      public
        procedure OnNext(const value: TSource); override;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const sources: IEnumerable<IObservable<TSource>>);
    function GetSources: IEnumerable<IObservable<TSource>>;
  end;

implementation


{$REGION 'TConcat<TSource>'}

constructor TConcat<TSource>.Create(
  const sources: IEnumerable<IObservable<TSource>>);
begin
  fSources := sources
end;

function TConcat<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TConcat<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fSources);
end;

function TConcat<TSource>.GetSources: IEnumerable<IObservable<TSource>>;
begin
  Result := fSources;
end;

{$ENDREGION}


{$REGION 'TConcat<TSource>.TSink'}

procedure TConcat<TSource>.TSink.OnNext(const value: TSource);
begin
  Observer.OnNext(value);
end;

{$ENDREGION}


end.
