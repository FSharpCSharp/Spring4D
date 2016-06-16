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

unit Spring.Reactive.Observable.Skip;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSkip<T> = class(TProducer<T>)
  private
    fSource: IObservable<T>;
    fCount: Integer;
//    fDuration: TTimeSpan;
//    fScheduler: IScheduler;

    type
      TSink = class(TSink<T>, IObserver<T>)
      private
        fParent: TSkip<T>;
        fRemaining: Integer;
      public
        constructor Create(const parent: TSkip<T>;
          const observer: IObserver<T>; const cancel: IDisposable);
        destructor Destroy; override;
        procedure OnNext(const value: T);
      end;
  protected
    function Run(const observer: IObserver<T>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(const source: IObservable<T>; count: Integer);
  end;

implementation


{$REGION 'TSkip<T>'}

constructor TSkip<T>.Create(const source: IObservable<T>; count: Integer);
begin
  inherited Create;
  fSource := source;
  fCount := count;
end;

function TSkip<T>.Run(const observer: IObserver<T>; const cancel: IDisposable;
  const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  // TODO implement SubscribeSafe
  Result := fSource.Subscribe(sink);
end;

{$ENDREGION}


{$REGION 'TSkip<T>.TSink'}

constructor TSkip<T>.TSink.Create(const parent: TSkip<T>;
  const observer: IObserver<T>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
  fRemaining := fParent.fCount;
end;

destructor TSkip<T>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkip<T>.TSink.OnNext(const value: T);
begin
  if fRemaining <= 0 then
    fObserver.OnNext(value)
  else
    Dec(fRemaining);
end;

{$ENDREGION}


end.
