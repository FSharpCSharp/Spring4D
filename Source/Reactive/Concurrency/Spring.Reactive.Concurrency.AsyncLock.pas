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

unit Spring.Reactive.Concurrency.AsyncLock;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive;

type
  IAsyncLock = interface(IDisposable)
    procedure Wait(const action: Action);
  end;

  TAsyncLock = class(TInterfacedObject, IAsyncLock)
  private
    fQueue: IQueue<Action>;
    fIsAquired: Boolean;
    fHasFaulted: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Dispose;

    procedure Wait(const action: Action);
  end;

implementation


{$REGION 'TAsyncLock'}

constructor TAsyncLock.Create;
begin
  inherited Create;
  fQueue := TCollections.CreateQueue<Action>;
end;

destructor TAsyncLock.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TAsyncLock.Dispose;
begin
  MonitorEnter(fQueue.AsObject);
  try
    fQueue.Clear;
    fHasFaulted := True;
  finally
    MonitorExit(fQueue.AsObject);
  end;
end;

procedure TAsyncLock.Wait(const action: Action);
var
  isOwner: Boolean;
  work: Spring.Action;
begin
  Guard.CheckNotNull(Assigned(action), 'action');

  isOwner := False;
  MonitorEnter(fQueue.AsObject);
  try
    if not fHasFaulted then
    begin
      fQueue.Enqueue(action);
      isOwner := not fIsAquired;
      fIsAquired := True;
    end;
  finally
    MonitorExit(fQueue.AsObject);
  end;

  if isOwner then
  begin
    while True do
    begin
      MonitorEnter(fQueue.AsObject);
      try
        if fQueue.Count > 0 then
          work := fQueue.Dequeue
        else
        begin
          fIsAquired := False;
          Break;
        end;
      finally
        MonitorExit(fQueue.AsObject);
      end;

      try
        work();
      except
        MonitorEnter(fQueue.AsObject);
        try
          fQueue.Clear;
          fHasFaulted := True;
        finally
          MonitorExit(fQueue.AsObject);
        end;
        raise;
      end;
    end;
  end;
end;

{$ENDREGION}


end.
