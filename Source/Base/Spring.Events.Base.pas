{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2021 Spring4D Team                           }
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

unit Spring.Events.Base;

interface

uses
  Classes,
  Spring;

type
  TEventBaseClass = class of TEventBase;

  /// <summary>
  ///   Base class for multicast event implementation
  /// </summary>
  TEventBase = class(TRefCountedObject)
  public type
    TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  private
    function GetRefCount: Integer; inline;
  strict protected
    fHandlers: PMethodArray;
    fNotificationHandler: TNotificationHandler;
    fOnChanged: TNotifyEvent;
  const
    DisabledFlag = Integer($40000000);
    RefCountMask = Integer($3FFFFFFF);
  {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean; inline;
    function GetEnabled: Boolean; inline;
    function GetHandlers: Pointer;
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean; inline;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    procedure SetUseFreeNotification(const value: Boolean);
  {$ENDREGION}
    procedure EnsureNotificationHandler; inline;
    procedure HandleNotification(component: TComponent; operation: TOperation);
  protected
    fInvoke: TMethodPointer;
    procedure Notify(sender: TObject; const item: TMethod;
      action: TCollectionNotification); virtual;
    function _Release: Integer; stdcall;
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property RefCount: Integer read GetRefCount;

  {$REGION 'Implements IEvent'}
    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
  {$ENDREGION}

    property CanInvoke: Boolean read GetCanInvoke;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethodPointer read fInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
    property UseFreeNotification: Boolean read GetUseFreeNotification write SetUseFreeNotification;
  end;

implementation

uses
  TypInfo,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Spring.HazardEra;

function IsValidObj(p: PPointer): Boolean;
{$IFDEF MSWINDOWS}
var
  memInfo: TMemoryBasicInformation;
{$ENDIF}

  function IsValidPtr(address: Pointer): Boolean;
  begin
    // Must be above 64k and 4 byte aligned
    if (UIntPtr(address) > $FFFF) and (UIntPtr(address) and 3 = 0) then
    begin
{$IFDEF MSWINDOWS}
      // do we need to recheck the virtual memory?
      if (UIntPtr(memInfo.BaseAddress) > UIntPtr(address))
        or ((UIntPtr(memInfo.BaseAddress) + memInfo.RegionSize) < (UIntPtr(address) + SizeOf(Pointer))) then
      begin
        // retrieve the status for the pointer
        memInfo.RegionSize := 0;
        VirtualQuery(address, memInfo, SizeOf(memInfo));
      end;
      // check the readability of the memory address
      Result := (memInfo.RegionSize >= SizeOf(Pointer))
        and (memInfo.State = MEM_COMMIT)
        and (memInfo.Protect and (PAGE_READONLY or PAGE_READWRITE
          or PAGE_WRITECOPY or PAGE_EXECUTE or PAGE_EXECUTE_READ
          or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0)
        and (memInfo.Protect and PAGE_GUARD = 0);
{$ELSE}
      Result := True;
{$ENDIF}
    end
    else
      Result := False;
  end;

begin
  Result := False;
  if Assigned(p) then
  try
{$IFDEF MSWINDOWS}
    memInfo.RegionSize := 0;
{$ENDIF}
    if IsValidPtr(PByte(p) + vmtSelfPtr)
      // not a class pointer - they point to themselves in the vmtSelfPtr slot
      and (p <> PPointer(PByte(p) + vmtSelfPtr)^) then
      if IsValidPtr(p) and IsValidPtr(PByte(p^) + vmtSelfPtr)
        // looks to be an object, it points to a valid class pointer
        and (p^ = PPointer(PByte(p^) + vmtSelfPtr)^) then
        Result := True;
  except
  end; //FI:W501
end;

function SafeIsClass(p: Pointer; cls: TClass): Boolean; inline;
begin
  Result := IsValidObj(p) and (TObject(p) is cls);
end;


{$REGION 'TEventBase'}

destructor TEventBase.Destroy;
begin
  if NativeUInt(fNotificationHandler) > 1 then
    fNotificationHandler.Free;
  NativeInt(fNotificationHandler) := 0;
  if Assigned(fHandlers) then
    Clear;
end;

procedure TEventBase.EnsureNotificationHandler;
begin
  if fNotificationHandler = nil then
  begin
    fNotificationHandler := TNotificationHandler.Create(nil);
    fNotificationHandler.OnNotification := HandleNotification;
  end;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fRefCount and DisabledFlag = 0;
end;

function TEventBase.GetCanInvoke: Boolean;
begin
  {$B+}
  Result := Enabled and Assigned(fHandlers);
  {$B-}
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

function TEventBase.GetRefCount: Integer;
begin
  Result := fRefCount and RefCountMask;
end;

function TEventBase.GetUseFreeNotification: Boolean;
begin
  Result := NativeInt(fNotificationHandler) <> 1;
end;

procedure TEventBase.Add(const handler: TMethod);
var
  handlers, new: PMethodArray;
  count: Integer;
begin
  if not Assigned(handler.Code) then
    Exit;

  new := nil;
  repeat
    handlers := AcquireGuard(fHandlers);
    count := DynArrayLength(handlers);
    EraArraySetLength(new, count + 1, TypeInfo(TMethod));
    EraArrayCopy(new, handlers);
    new[count] := handler;
  until AtomicCmpExchange(Pointer(fHandlers), new, handlers) = handlers;

  ReleaseGuard;
  EraArrayClear(handlers);
  Notify(Self, handler, cnAdded);
end;

procedure TEventBase.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

procedure TEventBase.Clear;
var
  handlers: PMethodArray;
  i: Integer;
begin
  if fHandlers = nil then Exit;

  repeat
    handlers := AcquireGuard(fHandlers);
  until AtomicCmpExchange(Pointer(fHandlers), nil, handlers) = handlers;

  try
    for i := 0 to DynArrayHigh(handlers) do
      Notify(Self, handlers[i], cnRemoved);
  finally
    ReleaseGuard;
    EraArrayClear(handlers);
  end;
end;

function TEventBase.GetHandlers: Pointer;
begin
  Result := AcquireGuard(fHandlers);
end;

procedure TEventBase.HandleNotification(component: TComponent;
  operation: TOperation);
begin
  if operation = opRemove then
    RemoveAll(component);
end;

procedure TEventBase.Notify(sender: TObject; const item: TMethod;
  action: TCollectionNotification);
var
  data: Pointer;
begin
  data := item.Data;
  if UseFreeNotification and SafeIsClass(data, TComponent) then
    case action of //FI:W535
      cnAdded:
      begin
        EnsureNotificationHandler;
        fNotificationHandler.FreeNotification(TComponent(data));
      end;
      cnRemoved:
        if fNotificationHandler <> nil then
          fNotificationHandler.RemoveFreeNotification(TComponent(data));
    end;

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.Remove(const handler: TMethod);
var
  handlers, new: PMethodArray;
  count, index, i: Integer;
begin
  if not Assigned(handler.Code) then
    Exit;

  new := nil;
  repeat
    handlers := AcquireGuard(fHandlers);
    count := DynArrayLength(handlers);
    index := -1;
    for i := 0 to count - 1 do
      if TMethod(handlers[i]) = TMethod(handler) then
      begin
        index := i;
        Break;
      end;
    if index = -1 then
    begin
      ReleaseGuard;
      Exit;
    end;
    if count > 1 then
    begin
      EraArraySetLength(new, count, TypeInfo(TMethod));
      EraArrayCopy(new, handlers);
      EraArrayDelete(new, index);
    end;
  until AtomicCmpExchange(Pointer(fHandlers), new, handlers) = handlers;

  ReleaseGuard;
  EraArrayClear(handlers);
  Notify(Self, handler, cnRemoved);
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  handlers, new: PMethodArray;
  oldItems: TArray<TMethod>;
  count, i, index: Integer;
begin
  new := nil;
  repeat
    handlers := AcquireGuard(fHandlers);
    count := DynArrayLength(handlers);

    EraArraySetLength(new, count, TypeInfo(TMethod));
    EraArrayCopy(new, handlers);
    SetLength(oldItems, count);

    index := 0;
    for i := count - 1 downto 0 do
      if TMethod(handlers[i]).Data = instance then
      begin
        oldItems[index] := handlers[i];
        Inc(index);
        EraArrayDelete(new, i);
      end;

    if index = 0 then
    begin
      ReleaseGuard;
      EraArrayClear(new);
      Exit;
    end;
  until AtomicCmpExchange(Pointer(fHandlers), new, handlers) = handlers;
  ReleaseGuard;
  EraArrayClear(handlers);
  for i := index - 1 downto 0 do
    Notify(Self, oldItems[i], cnRemoved);
end;

procedure TEventBase.SetEnabled(const value: Boolean);
var
  bitMask: Integer;
  oldRefCount: Integer;
begin
  bitMask := Integer(value) shl 30;
  repeat
    oldRefCount := fRefCount;
  until AtomicCmpExchange(fRefCount, (oldRefCount or DisabledFlag) xor bitMask, oldRefCount) = oldRefCount;

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.SetOnChanged(const value: TNotifyEvent);
begin
  fOnChanged := value;
end;

procedure TEventBase.SetUseFreeNotification(const value: Boolean);
var
  data: Pointer;
  handler: PMethodArray;
begin
  MonitorEnter(Self);
  try
    case NativeInt(fNotificationHandler) of
      0: // UseFreeNotification is True but no handler assigned yet ...
        if not value then // ... it can only be turned False
          NativeInt(fNotificationHandler) := 1;
      1: // UseFreeNotification is False ...
        if value then // ... it can only be turned True
        begin
          NativeInt(fNotificationHandler) := 0;
          handler := AcquireGuard(fHandlers);
          try
            if Assigned(handler) and Assigned(handler.Code) then
            repeat
              data := handler.Data;
              if SafeIsClass(data, TComponent) then
              begin
                EnsureNotificationHandler;
                fNotificationHandler.FreeNotification(TComponent(data));
              end;
              Inc(handler);
            until not Assigned(handler.Code);
          finally
            ReleaseGuard;
          end;
        end;
    else // UseFreeNotification is True and handler is already assigned ...
      if not value then // ... it can only be turned False
      begin
        fNotificationHandler.Free;
        NativeInt(fNotificationHandler) := 1;
      end;
    end;
  finally
    MonitorExit(Self);
  end;
end;

function TEventBase._Release: Integer;
begin
  Result := AtomicDecrement(fRefCount) and not DisabledFlag;
  if Result = 0 then
  begin
    __MarkDestroying(Self);
    Destroy;
  end;
end;

{$ENDREGION}


end.
