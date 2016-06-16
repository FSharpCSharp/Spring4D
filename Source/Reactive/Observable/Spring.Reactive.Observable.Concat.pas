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

unit Spring.Reactive.Observable.Concat;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Internal.ConcatSink,
  Spring.Reactive.Internal.Producer;

type
  TConcat<TSource> = class(TProducer<TSource>)
  private
    fSources: TArray<IObservable<TSource>>;

    type
      TSink = class(TConcatSink<TSource>)
      end;
  protected
    function Run(const observer: IObserver<TSource>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const sources: array of IObservable<TSource>);
  end;

implementation


{$REGION 'TConcat<TSource>'}

constructor TConcat<TSource>.Create(
  const sources: array of IObservable<TSource>);
begin
  fSources := TArray.Copy<IObservable<TSource>>(sources);
end;

function TConcat<TSource>.Run(const observer: IObserver<TSource>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(observer, cancel);
  setSink(sink);
  Result := sink.Run(fSources);
end;

{$ENDREGION}


end.
