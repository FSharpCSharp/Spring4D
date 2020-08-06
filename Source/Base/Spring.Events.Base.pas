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

unit Spring.Events.Base;

interface

uses
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Spring;

type
  /// <summary>
  ///   Base class for multicast event implementation
  /// </summary>
  TEventBase = class(TInterfacedObject, IEvent)
  public type
    TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  strict protected
    fHandlers: TArray<TMethodPointer>;
    fCount: Integer;
    fNotificationHandler: TNotificationHandler;
    fOnChanged: TNotifyEvent;
    fLock: {$IFDEF MSWINDOWS}PRTLCriticalSection{$ELSE}TObject{$ENDIF};
    const DisabledFlag = Integer($80000000);
  {$REGION 'Property Accessors'}
    function GetCanInvoke: Boolean; inline;
    function GetCount: Integer; inline;
    function GetEnabled: Boolean; inline;
    function GetHandlers: TArray<TMethodPointer>;
    function GetInvoke: TMethodPointer; inline;
    function GetOnChanged: TNotifyEvent;
    function GetThreadSafe: Boolean; inline;
    function GetUseFreeNotification: Boolean; inline;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    procedure SetThreadSafe(const value: Boolean);
    procedure SetUseFreeNotification(const value: Boolean);
  {$ENDREGION}
    procedure Delete(index: Integer);
    procedure EnsureNotificationHandler; inline;
    procedure HandleNotification(component: TComponent; operation: TOperation);
    procedure LockEnter; inline;
    procedure LockLeave; inline;
  protected
    fInvoke: TMethodPointer;
    procedure Notify(sender: TObject; const item: TMethodPointer;
      action: TCollectionNotification); virtual;
    property Count: Integer read GetCount;
    property Handlers: TArray<TMethodPointer> read GetHandlers;
  public
    constructor Create(const threadSafe: Boolean = True);
    destructor Destroy; override;

  {$REGION 'Implements IEvent'}
    procedure Add(const handler: TMethodPointer);
    procedure Remove(const handler: TMethodPointer);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
  {$ENDREGION}

    property CanInvoke: Boolean read GetCanInvoke;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethodPointer read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
    property ThreadSafe: Boolean read GetThreadSafe write SetThreadSafe;
    property UseFreeNotification: Boolean read GetUseFreeNotification write SetUseFreeNotification;
  end;

  TEventBase<T> = class(TEventBase, IEvent<T>)
  protected
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
  {$ENDREGION}
  public
  {$REGION 'Implements IEvent<T>'}
    procedure Add(handler: T);
    procedure Remove(handler: T);
  {$ENDREGION}
  end;

implementation

uses
  TypInfo;

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

constructor TEventBase.Create(const threadSafe: Boolean);
begin
  if threadSafe then
  begin
  {$IFDEF MSWINDOWS}
    New(fLock);
    InitializeCriticalSection(fLock^);
  {$ELSE}
    fLock := TObject.Create;
  {$ENDIF}
  end;
end;

destructor TEventBase.Destroy;
begin
  if UseFreeNotification then
    fNotificationHandler.Free;
  NativeInt(fNotificationHandler) := 0;
  Clear;
{$IFDEF MSWINDOWS}
  if Assigned(fLock) then
  begin
    DeleteCriticalSection(fLock^);
    Dispose(fLock);
  end;
{$ELSE}
  fLock.Free;
{$ENDIF}
end;

procedure TEventBase.EnsureNotificationHandler;
begin
  if fNotificationHandler = nil then
  begin
    fNotificationHandler := TNotificationHandler.Create(nil);
    fNotificationHandler.OnNotification := HandleNotification;
  end;
end;

function TEventBase.GetCanInvoke: Boolean;
begin
  Result := fCount > 0;
end;

function TEventBase.GetCount: Integer;
begin
  Result := fCount and not DisabledFlag;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fCount >= 0;
end;

function TEventBase.GetInvoke: TMethodPointer;
begin
  Result := fInvoke;
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

function TEventBase.GetThreadSafe: Boolean;
begin
  Result := Assigned(Pointer(fLock));
end;

function TEventBase.GetUseFreeNotification: Boolean;
begin
  Result := NativeInt(fNotificationHandler) <> 1;
end;

procedure TEventBase.LockEnter;
begin
  if Assigned(fLock) then
{$IFDEF MSWINDOWS}
    EnterCriticalSection(fLock^);
{$ELSE}
    TMonitor.Enter(fLock);
{$ENDIF}
end;

procedure TEventBase.LockLeave;
begin
  if Assigned(fLock) then
{$IFDEF MSWINDOWS}
    LeaveCriticalSection(fLock^);
{$ELSE}
    TMonitor.Exit(fLock);
{$ENDIF}
end;

procedure TEventBase.Add(const handler: TMethodPointer);
var
  i: Integer;
begin
  if not Assigned(handler) then
    Exit;
  LockEnter;
  try
    i := Count;
    if Length(fHandlers) <= i then
      if i = 0 then
        SetLength(fHandlers, 1)
      else
        SetLength(fHandlers, i * 2);
    fHandlers[i] := handler;
    AtomicIncrement(fCount);
    Notify(Self, handler, cnAdded);
  finally
    LockLeave;
  end;
end;

procedure TEventBase.Clear;
var
  i: Integer;
begin
  LockEnter;
  try
    for i := 0 to Count - 1 do
      Notify(Self, fHandlers[i], cnRemoved);
    fCount := fCount and DisabledFlag;
    fHandlers := nil;
  finally
    LockLeave;
  end;
end;

procedure TEventBase.Delete(index: Integer);
var
  oldItem: TMethodPointer;
  i: Integer;
begin
  oldItem := fHandlers[index];
  AtomicDecrement(fCount);
  for i := index to Count - 1 do
    fHandlers[i] := fHandlers[i + 1];
  fHandlers[Count] := nil;
  Notify(Self, oldItem, cnRemoved);
end;

function TEventBase.GetHandlers: TArray<TMethodPointer>;
begin
  LockEnter;
  try
    Result := Copy(fHandlers, 0, Count);
  finally
    LockLeave;
  end;
end;

procedure TEventBase.HandleNotification(component: TComponent;
  operation: TOperation);
begin
  if operation = opRemove then
    RemoveAll(component);
end;

procedure TEventBase.Notify(sender: TObject; const item: TMethodPointer;
  action: TCollectionNotification);
var
  data: Pointer;
begin
  data := TMethod(item).Data;
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

procedure TEventBase.Remove(const handler: TMethodPointer);
var
  i: Integer;
begin
  LockEnter;
  try
    for i := 0 to Count - 1 do
      if TMethod(fHandlers[i]) = TMethod(handler) then
      begin
        Delete(i);
        Break;
      end;
  finally
    LockLeave;
  end;
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  LockEnter;
  try
    for i := Count - 1 downto 0 do
      if TMethod(fHandlers[i]).Data = instance then
        Delete(i);
  finally
    LockLeave;
  end;
end;

procedure TEventBase.SetEnabled(const value: Boolean);
var
  bitMask: Integer;
  oldCount: Integer;
begin
  bitMask := Integer(value) shl 31;
  repeat
    oldCount := fCount;
  until AtomicCmpExchange(fCount, (oldCount or DisabledFlag) xor bitMask, oldCount) = oldCount;

  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.SetOnChanged(const value: TNotifyEvent);
begin
  fOnChanged := value;
end;

procedure TEventBase.SetThreadSafe(const value: Boolean);
begin
  if value <> Assigned(fLock) then
    if Assigned(fLock) then
    begin
    {$IFDEF MSWINDOWS}
      DeleteCriticalSection(fLock^);
      Dispose(fLock);
    {$ELSE}
      fLock.Free;
    {$ENDIF}
      fLock := nil;
    end
    else
    begin
    {$IFDEF MSWINDOWS}
      New(fLock);
      InitializeCriticalSection(fLock^);
    {$ELSE}
      fLock := TObject.Create;
    {$ENDIF}
    end;
end;

procedure TEventBase.SetUseFreeNotification(const value: Boolean);
var
  i: Integer;
  data: Pointer;
begin
  LockEnter;
  try
    case NativeInt(fNotificationHandler) of
      0: // UseFreeNotification is True but no handler assigned yet ...
        if not value then // ... it can only be turned False
          NativeInt(fNotificationHandler) := 1;
      1: // UseFreeNotification is False ...
        if value then // ... it can only be turned True
        begin
          NativeInt(fNotificationHandler) := 0;
          for i := 0 to Count - 1 do // check every handler
          begin
            data := TMethod(fHandlers[i]).Data;
            if SafeIsClass(data, TComponent) then
            begin
              EnsureNotificationHandler;
              fNotificationHandler.FreeNotification(TComponent(data));
            end;
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
    LockLeave;
  end;
end;

{$ENDREGION}


{$REGION 'TEventBase<T>'}

procedure TEventBase<T>.Add(handler: T);
begin
  if TType.Kind<T> = tkInterface then
    inherited Add(MethodReferenceToMethodPointer(handler))
  else
    inherited Add(PMethodPointer(@handler)^);
end;

function TEventBase<T>.GetInvoke: T;
begin
  if TType.Kind<T> = tkInterface then
    IInterface(PPointer(@Result)^) := MethodPointerToMethodReference(inherited Invoke)
  else
    PMethodPointer(@Result)^ := inherited Invoke;
end;

procedure TEventBase<T>.Remove(handler: T);
begin
  if TType.Kind<T> = tkInterface then
    inherited Remove(MethodReferenceToMethodPointer(handler))
  else
    inherited Remove(PMethodPointer(@handler)^);
end;

{$ENDREGION}


end.
