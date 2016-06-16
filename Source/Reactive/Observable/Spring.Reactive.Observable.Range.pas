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

unit Spring.Reactive.Observable.Range;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TRange = class(TProducer<Integer>)
  private
    fStart: Integer;
    fCount: Integer;
    fScheduler: IScheduler;

    type
      TSink = class(TSink<Integer>)
      private
        fParent: TRange;
        procedure LoopRec(const state: TValue; const recurse: Action<TValue>);
      public
        constructor Create(const parent: TRange;
          const observer: IObserver<Integer>; const cancel: IDisposable);
        function Run: IDisposable;
      end;
  protected
    function Run(const observer: IObserver<Integer>; const cancel: IDisposable;
      const setSink: Action<IDisposable>): IDisposable; override;
  public
    constructor Create(start, count: Integer; const scheduler: IScheduler);
  end;

implementation


{$REGION 'TRange'}

constructor TRange.Create(start, count: Integer; const scheduler: IScheduler);
begin
  inherited Create;
  fStart := start;
  fCount := count;
  fScheduler := scheduler;
end;

function TRange.Run(const observer: IObserver<Integer>;
  const cancel: IDisposable; const setSink: Action<IDisposable>): IDisposable;
var
  sink: TSink;
begin
  sink := TSink.Create(Self, observer, cancel);
  setSink(sink);
  Result := sink.Run;
end;

{$ENDREGION}


{$REGION 'TRange.TSink'}

constructor TRange.TSink.Create(const parent: TRange;
  const observer: IObserver<Integer>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
end;

procedure TRange.TSink.LoopRec(const state: TValue; const recurse: Action<TValue>);
var
  i: Integer;
begin
  i := state.AsInteger;
  if i < fParent.fCount then
  begin
    fObserver.OnNext(fParent.fStart + i);
    recurse(i + 1);
  end
  else
  begin
    fObserver.OnCompleted;
    Dispose;
  end;
end;

function TRange.TSink.Run: IDisposable;
begin
  Result := fParent.fScheduler.Schedule(0, LoopRec);
end;

{$ENDREGION}


end.

