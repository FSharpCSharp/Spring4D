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

unit Spring.Reactive.Concurrency.SchedulerQueue;

interface

uses
  Spring.Reactive.Concurrency.ScheduledItem,
  Spring.Reactive.Internal.PriorityQueue;

type
  TSchedulerQueue<TAbsolute> = class
  private
    fQueue: TPriorityQueue<IScheduledItem<TAbsolute>>;
    function GetCount: Integer;
  public
    constructor Create(capacity: Integer = 1024);
    destructor Destroy; override;
    procedure Enqueue(const scheduledItem: IScheduledItem<TAbsolute>);
    function Remove(const scheduledItem: IScheduledItem<TAbsolute>): Boolean;
    function Dequeue: IScheduledItem<TAbsolute>;
    function Peek: IScheduledItem<TAbsolute>;
    property Count: Integer read GetCount;
  end;

implementation


{$REGION 'TSchedulerQueue<TAbsolute>'}

constructor TSchedulerQueue<TAbsolute>.Create(capacity: Integer);
begin
  fQueue := TPriorityQueue<IScheduledItem<TAbsolute>>.Create(capacity);
end;

function TSchedulerQueue<TAbsolute>.Dequeue: IScheduledItem<TAbsolute>;
begin
  Result := fQueue.Dequeue;
end;

destructor TSchedulerQueue<TAbsolute>.Destroy;
begin
  fQueue.Free;
  inherited;
end;

procedure TSchedulerQueue<TAbsolute>.Enqueue(
  const scheduledItem: IScheduledItem<TAbsolute>);
begin
  fQueue.Enqueue(scheduledItem);
end;

function TSchedulerQueue<TAbsolute>.GetCount: Integer;
begin
  Result := fQueue.Count;
end;

function TSchedulerQueue<TAbsolute>.Peek: IScheduledItem<TAbsolute>;
begin
  Result := fQueue.Peek;
end;

function TSchedulerQueue<TAbsolute>.Remove(
  const scheduledItem: IScheduledItem<TAbsolute>): Boolean;
begin
  Result := fQueue.Remove(scheduledItem);
end;

{$ENDREGION}


end.
