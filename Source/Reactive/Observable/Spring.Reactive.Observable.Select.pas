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

unit Spring.Reactive.Observable.Select;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSelect<TSource, TResult> = class(TProducer<TResult>)
  private
    fSource: IObservable<TSource>;
    fSelector: Func<TSource,TResult>;

    type
      TSink = class(TSink<TResult>, IObserver<TSource>)
      private
        fParent: TSelect<TSource, TResult>;
      public
        constructor Create(const parent: TSelect<TSource, TResult>;
          const observer: IObserver<TResult>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: TSource);
        procedure OnError(const error: Exception);
        procedure OnCompleted;
      end;
  protected
    function Run(const observer: IObserver<TResult>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>;
      const selector: Func<TSource, TResult>);
  end;

implementation


{$REGION 'TSelect<TSource, TResult>'}

constructor TSelect<TSource, TResult>.Create(const source: IObservable<TSource>;
  const selector: Func<TSource, TResult>);
begin
  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelect<TSource, TResult>.Run(const observer: IObserver<TResult>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  if Assigned(fSelector) then
  begin
    sink := TSink.Create(Self, observer, cancel);
    setSink(sink);
    Result := fSource.Subscribe(sink);
  end;
end;

{$ENDREGION}


{$REGION 'TSelect<TSource, TResult>.TSink'}

constructor TSelect<TSource, TResult>.TSink.Create(
  const parent: TSelect<TSource, TResult>; const observer: IObserver<TResult>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSelect<TSource, TResult>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSelect<TSource, TResult>.TSink.OnNext(const value: TSource);
var
  result: TResult;
begin
  try
    result := fParent.fSelector(value);
  except
    on e: Exception do
    begin
      Observer.OnError(e);
      Dispose;
      Exit;
    end;
  end;

  Observer.OnNext(result);
end;

procedure TSelect<TSource, TResult>.TSink.OnError(const error: Exception);
begin
  Observer.OnError(error);
  Dispose;
end;

procedure TSelect<TSource, TResult>.TSink.OnCompleted;
begin
  Observer.OnCompleted;
  Dispose;
end;

{$ENDREGION}


end.
