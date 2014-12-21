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

{$I Spring.inc}

unit Spring.Events.Base;

interface

uses
  Classes,
  Generics.Collections,
  SyncObjs,
  Spring;

type
  PMethod = ^TMethod;

  ///	<summary>
  ///	  Base class for multicast event implementation
  ///	</summary>
  TEventBase = class(TInterfacedObject, IEvent)
  private
    fEnabled: Boolean;
    fHandlers: TList<TMethod>;
    fLock: TCriticalSection;
    fOnChanged: TNotifyEvent;
    fNotificationHandler: TNotificationHandler;

    {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetHandlers: TArray<TMethod>;
    function GetInvoke: TMethod;
    function GetIsEmpty: Boolean;
    function GetIsInvokable: Boolean;
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
    property Handlers: TArray<TMethod> read GetHandlers;
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

  TEventBase<T> = class(TEventBase, IEvent<T>)
  private
    function GetInvoke: T;
    procedure Add(handler: T);
    procedure Remove(handler: T);
    procedure ForEach(const action: TAction<T>);
  end;

implementation

uses
  TypInfo;

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
  fHandlers := TList<TMethod>.Create;
  fHandlers.OnNotify := Notify;
  fLock := TCriticalSection.Create;
end;

destructor TEventBase.Destroy;
begin
  fNotificationHandler.Free;
  fNotificationHandler := nil;
  fHandlers.Free;
  fLock.Free;
  inherited;
end;

procedure TEventBase.Add(const handler: TMethod);
begin
  fLock.Enter;
  try
    fHandlers.Add(handler);
  finally
    fLock.Leave;
  end;
end;

procedure TEventBase.Clear;
begin
  fLock.Enter;
  try
    fHandlers.Clear;
  finally
    fLock.Leave;
  end;
end;

procedure TEventBase.ForEach(const action: TAction<TMethod>);
var
  handler: TMethod;
begin
  for handler in Handlers do
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

function TEventBase.GetHandlers: TArray<TMethod>;
var
  i: Integer;
begin
  fLock.Enter;
  try
    SetLength(Result, fHandlers.Count);
    for i := 0 to fHandlers.Count - 1 do
      Result[i] := fHandlers[i];
  finally
    fLock.Leave;
  end;
end;

function TEventBase.GetInvoke: TMethod;
begin
  Result := fInvoke;
end;

function TEventBase.GetIsEmpty: Boolean;
begin
  Result := fHandlers.Count = 0;
end;

function TEventBase.GetIsInvokable: Boolean;
begin
  Result := fEnabled and (fHandlers.Count <> 0);
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

procedure TEventBase.HandleNotification(Component: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    RemoveAll(Component);
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
  fLock.Enter;
  try
    fHandlers.Remove(handler);
  finally
    fLock.Leave;
  end;
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  fLock.Enter;
  try
    for i := fHandlers.Count - 1 downto 0 do
      if fHandlers[i].Data = instance then
        fHandlers.Delete(i);
  finally
    fLock.Leave;
  end;
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


{$REGION 'TEventBase<T>'}

procedure TEventBase<T>.Add(handler: T);
begin
  if {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} = tkInterface then
    inherited Add(MethodReferenceToMethodPointer(handler))
  else
    inherited Add(PMethod(@handler)^);
end;

procedure TEventBase<T>.ForEach(const action: TAction<T>);
var
  handler: TMethod;
begin
  for handler in Handlers do
    if {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} = tkInterface then
      TAction<IInterface>(action)(MethodPointerToMethodReference(handler))
    else
      TAction<TMethod>(action)(handler);
end;

function TEventBase<T>.GetInvoke: T;
begin
  if {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} = tkInterface then
    IInterface(PPointer(@Result)^) := MethodPointerToMethodReference(inherited Invoke)
  else
    PMethod(@Result)^ := inherited Invoke;
end;

procedure TEventBase<T>.Remove(handler: T);
begin
  if {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} = tkInterface then
    inherited Remove(MethodReferenceToMethodPointer(handler))
  else
    inherited Remove(PMethod(@handler)^);
end;

{$ENDREGION}


end.
