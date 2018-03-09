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
        fStart: Integer;
        fCount: Integer;
        procedure LoopRec(const i: Integer; const recurse: Action<TValue>);
      public
        constructor Create(const parent: TRange;
          const observer: IObserver<Integer>; const cancel: IDisposable);
        function Run(const scheduler: IScheduler): IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<Integer>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
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

function TRange.CreateSink(const observer: IObserver<Integer>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TRange.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(fScheduler);
end;

{$ENDREGION}


{$REGION 'TRange.TSink'}

constructor TRange.TSink.Create(const parent: TRange;
  const observer: IObserver<Integer>; const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fStart := parent.fStart;
  fCount := parent.fCount;
end;

procedure TRange.TSink.LoopRec(const i: Integer; const recurse: Action<TValue>);
begin
  if i < fCount then
  begin
    Observer.OnNext(fStart + i);
    recurse(i + 1);
  end
  else
  begin
    Observer.OnCompleted;
    Dispose;
  end;
end;

function TRange.TSink.Run(const scheduler: IScheduler): IDisposable;
var
  guard: IInterface;
begin
  guard := Self; // make sure that self is kept alive by capturing it
  Result := scheduler.Schedule(0,
    procedure (const state: TValue; const recurse: Action<TValue>)
    begin
      if Assigned(guard) then
        LoopRec(state.AsInteger, recurse);
    end);
end;

{$ENDREGION}


end.

