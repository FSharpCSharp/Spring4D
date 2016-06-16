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

unit Spring.Reactive.Observable.CombineLatest;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer;

type
  TCombineLatest<TFirst, TSecond, TResult> = class(TProducer<TResult>)
  private
    fFirst: IObservable<TFirst>;
    fSecond: IObservable<TSecond>;
    fResultSelector: Func<TFirst, TSecond, TResult>;
  public
    constructor Create(const first: IObservable<TFirst>;
      const second: IObservable<TSecond>;
      const resultSelector: Func<TFirst, TSecond, TResult>);
  end;

implementation


{$REGION 'TCombineLatest<TFirst, TSecond, TResult>'}

constructor TCombineLatest<TFirst, TSecond, TResult>.Create(
  const first: IObservable<TFirst>; const second: IObservable<TSecond>;
  const resultSelector: Func<TFirst, TSecond, TResult>);
begin
  inherited Create;
  fFirst := first;
  fSecond := second;
  fResultSelector := resultSelector;
end;

{$ENDREGION}


end.
