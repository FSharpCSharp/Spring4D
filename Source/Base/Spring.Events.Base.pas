{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
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

unit Spring.Events.Base;

{$I Spring.inc}

interface

uses
  Classes,
  Generics.Collections,
  Spring;

type
  ///	<summary>
  ///	  Base class for multicast event implementation
  ///	</summary>
  TEventBase = class(TInterfacedObject, IEvent)
  private
    type
      TMethodList = class(TList<TMethod>)
      {$IFDEF DELPHI2010}
        function ToArray: TArray<TMethod>;
      {$ENDIF}
      end;
  private
    fEnabled: Boolean;
    fHandlers: TMethodList;
    fOnChanged: TNotifyEvent;
    fNotificationHandler: TNotificationHandler;

    {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: TMethod;
    function GetIsEmpty: Boolean;
    function GetOnChanged: TNotifyEvent;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    {$ENDREGION}
  protected
    fInvoke: TMethod;
    procedure HandleNotification(Component: TComponent;
      Operation: TOperation);
    procedure Notify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification); virtual;
    property Handlers: TMethodList read fHandlers;
  public
    constructor Create;
    destructor Destroy; override;

    {$REGION 'IEvent Methods'}
    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    procedure ForEach(const action: TAction<TMethod>);
    {$ENDREGION}

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethod read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

implementation

function IsValid(AObject: TObject): Boolean;
{$IFDEF DELPHI2010}
type
  PNativeInt = ^NativeInt;
{$ENDIF}
begin
  Result := False;
  if Assigned(AObject) then
  try
    if PNativeInt(AObject)^ > $FFFF then
      Result := PNativeInt(AObject)^ = PNativeInt(PNativeInt(AObject)^ + vmtSelfPtr)^;
  except
  end;
end;


{$REGION 'TEventBase'}

constructor TEventBase.Create;
begin
  inherited Create;
  fEnabled := True;
  fHandlers := TMethodList.Create;
  fHandlers.OnNotify := Notify;
end;

destructor TEventBase.Destroy;
begin
  fNotificationHandler.Free;
  fHandlers.Free;
  inherited;
end;

procedure TEventBase.Add(const handler: TMethod);
begin
  fHandlers.Add(handler);
end;

procedure TEventBase.Clear;
begin
  fHandlers.Clear;
end;

procedure TEventBase.ForEach(const action: TAction<TMethod>);
var
  handler: TMethod;
begin
  for handler in fHandlers.ToArray do
    action(handler);
end;

function TEventBase.GetCount: Integer;
begin
  Result := fHandlers.Count;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TEventBase.GetInvoke: TMethod;
begin
  Result := fInvoke;
end;

function TEventBase.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

procedure TEventBase.HandleNotification(Component: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    for i := fHandlers.Count - 1 downto 0 do
      if fHandlers[i].Data = Component then
        fHandlers.Delete(i);
  end;
end;

procedure TEventBase.Notify(Sender: TObject; const Item: TMethod;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
    begin
      if IsValid(Item.Data) and (TObject(Item.Data) is TComponent) then
      begin
        if fNotificationHandler = nil then
        begin
          fNotificationHandler := TNotificationHandler.Create(nil);
          fNotificationHandler.OnNotification := HandleNotification;
        end;
        fNotificationHandler.FreeNotification(TComponent(Item.Data));
      end;
    end;
    cnRemoved:
    begin
      if IsValid(Item.Data) and (TObject(Item.Data) is TComponent) then
      begin
        if fNotificationHandler <> nil then
          fNotificationHandler.RemoveFreeNotification(TComponent(Item.Data));
      end;
    end;
  end;

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.Remove(const handler: TMethod);
begin
  fHandlers.Remove(handler);
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  for i := fHandlers.Count - 1 downto 0 do
    if fHandlers[i].Data = instance then
      fHandlers.Delete(i);
end;

procedure TEventBase.SetEnabled(const value: Boolean);
begin
  fEnabled := value;
end;

procedure TEventBase.SetOnChanged(const value: TNotifyEvent);
begin
  fOnChanged := value;
end;

{$ENDREGION}


{$REGION 'TEventBase.TMethodList'}

{$IFDEF DELPHI2010}
function TEventBase.TMethodList.ToArray: TArray<TMethod>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Items[i];
end;
{$ENDIF}

{$ENDREGION}


end.
