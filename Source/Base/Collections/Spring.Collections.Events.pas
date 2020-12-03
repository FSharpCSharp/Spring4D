{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2020 Spring4D Team                           }
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

unit Spring.Collections.Events;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Events.Base;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TCollectionChangedEventImpl<T> = class(TEventBase, IEvent, ICollectionChangedEvent<T>)
  public
    procedure AfterConstruction; override;
{$IFNDEF AUTOREFCOUNT}
    procedure Free;
{$ENDIF}
    procedure Add(handler: TCollectionChangedEvent<T>); overload; inline;
    procedure Remove(handler: TCollectionChangedEvent<T>); overload; inline;
    procedure Invoke(Sender: TObject; const Item: T; Action: TCollectionChangedAction);
  end;

implementation

uses
  Spring.HazardEra;


{$REGION 'TCollectionChangedEventImpl<T>'}

procedure TCollectionChangedEventImpl<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  TCollectionChangedEvent<T>(fInvoke) := Invoke;
{$IFNDEF AUTOREFCOUNT}
  _AddRef;
{$ENDIF}
end;

{$IFNDEF AUTOREFCOUNT}
procedure TCollectionChangedEventImpl<T>.Free;
begin
  if Assigned(Self) then
    _Release;
end;
{$ENDIF}

procedure TCollectionChangedEventImpl<T>.Add(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Add(TMethod(handler));
end;

procedure TCollectionChangedEventImpl<T>.Invoke(Sender: TObject;
  const Item: T; Action: TCollectionChangedAction);
var
  handlers: PMethodArray;
  i: Integer;
begin
  // If you get exception at this location on NextGen and the handler is nil
  // it is highly possible that the owner of the collection already released
  // its weak references. To fix this, free the collection prior to freeing
  // the object owning it (even if you're using interfaces).
  if CanInvoke then
  begin
    handlers := GetHandlers;
    try
      for i := 0 to DynArrayHigh(handlers) do
        TCollectionChangedEvent<T>(handlers[i])(Sender, Item, Action);
    finally
      ReleaseGuard;
    end;
  end;
end;

procedure TCollectionChangedEventImpl<T>.Remove(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


end.
