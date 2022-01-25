{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2022 Spring4D Team                           }
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

unit Spring.Events;

{$IFNDEF ASSEMBLER}
  {$DEFINE USE_RTTI_FOR_PROXY}
{$ENDIF}

interface

uses
  Classes,
  Rtti,
  SysUtils,
  Spring,
  Spring.Events.Base,
  TypInfo;

type

  {$REGION 'TEvent'}

  TEvent = class(TEventBase)
  private
    fTypeInfo: PTypeInfo;
  {$IFDEF USE_RTTI_FOR_PROXY}
    fProxy: Pointer;
  protected
    procedure InternalInvokeMethod(UserData: Pointer;
      const Args: TArray<TValue>; out Result: TValue); virtual;
    procedure InternalInvokeDelegate(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue); virtual;
  {$ELSE}
  private
    const
      paEAX = Word(0);
      paEDX = Word(1);
      paECX = Word(2);
      paStack = Word(3);

    type
      PParameterInfos = ^TParameterInfos;
      TParameterInfos = array[0..255] of ^PTypeInfo;

      PParameters = ^TParameters;
      TParameters = packed record
      public
{$IFNDEF CPUX64}
        Registers: array[paEDX..paECX] of Cardinal;
        EAXRegister: Cardinal;
        ReturnAddress: Pointer;
{$ENDIF}
        Stack: array[0..1023] of Byte;
      end;

      PMethodInfo = ^TMethodInfo;
      TMethodInfo = record
        ParamInfos: PParameterInfos;
        StackSize: Integer;
{$IFDEF CPUX64}
        RegisterFlag: Integer;
{$ENDIF}
        constructor Create(typeData: PTypeData);
      end;

  private
    fMethodInfo: TMethodInfo;
    fMethodInvoke: Pointer;
    procedure InvokeEventHandlerStub;
    class procedure InvokeMethod(const Method: TMethod; Parameters: PParameters; StackSize: Integer); static;
  protected
    procedure Invoke;
    procedure InternalInvoke(Params: Pointer; StackSize: Integer); virtual;
  {$ENDIF}
    procedure Notify(Sender: TObject; const Item: TMethod;
      Action: TEventBase.TCollectionNotification); override;
  public
    constructor Create(typeInfo: PTypeInfo);
    destructor Destroy; override;
  end;

  {$ENDREGION}


  {$REGION 'TNotifyEventImpl'}

  TNotifyEventImpl = class(TEventBase, IEvent,
    IEvent<TNotifyEvent>, IInvokableEvent<TNotifyEvent>)
  private
    function GetInvoke: TNotifyEvent; overload;
  public
    procedure AfterConstruction; override;
    procedure Add(handler: TNotifyEvent); overload;
    procedure Remove(handler: TNotifyEvent); overload;
    procedure Invoke(sender: TObject);
  end;

  IInvokableNotifyEvent = IInvokableEvent<TNotifyEvent>;

  {$ENDREGION}


  {$REGION 'TNotifyEventImpl<T>'}

  TNotifyEventImpl<T> = class(TEventBase, IEvent,
    INotifyEvent<T>, IInvokableNotifyEvent<T>)
  private
    function GetInvoke: TNotifyEvent<T>; overload;
  public
    procedure AfterConstruction; override;
    procedure Add(handler: TNotifyEvent<T>); overload;
    procedure Remove(handler: TNotifyEvent<T>); overload;
    procedure Invoke(sender: TObject; const item: T);
  end;

  {$ENDREGION}


  {$REGION 'TPropertyChangedEventImpl'}

  TPropertyChangedEventImpl = class(TEventBase, IEvent,
    IEvent<TPropertyChangedEvent>, IInvokableEvent<TPropertyChangedEvent>)
  private
    function GetInvoke: TPropertyChangedEvent; overload;
  public
    procedure AfterConstruction; override;
    procedure Add(handler: TPropertyChangedEvent); overload;
    procedure Remove(handler: TPropertyChangedEvent); overload;
    procedure Invoke(Sender: TObject;
      const EventArgs: IPropertyChangedEventArgs);
  end;

  {$ENDREGION}


  {$REGION 'EventHelper'}

  EventHelper = record
  private type
    IMethodEventInternal = interface(IInvokableEvent<TMethodPointer>)
      procedure GetInvoke(var result);
      procedure Add(const handler);
      procedure Remove(const handler);
    end;

    IDelegateEventInternal = interface(IInvokableEvent<IInterface>)
      procedure GetInvoke(var result);
      procedure Add(const handler);
      procedure Remove(const handler);
    end;

    TMethodEvent = class(TEvent, IMethodEventInternal, IEvent)
    private
      function GetInvoke: TMethodPointer; overload;
      procedure Add(handler: TMethodPointer); overload;
      procedure Remove(handler: TMethodPointer); overload;

      procedure GetInvoke(var result); overload;
      procedure Add(const handler); overload;
      procedure Remove(const handler); overload;
    end;

    TDelegateEvent = class(TEvent, {$IFNDEF USE_RTTI_FOR_PROXY}TProc,{$ENDIF} IDelegateEventInternal, IEvent)
    private
      function GetInvoke: IInterface; overload;
      procedure Add(handler: IInterface); overload;
      procedure Remove(handler: IInterface); overload;

      procedure GetInvoke(var result); overload;
      procedure Add(const handler); overload;
      procedure Remove(const handler); overload;
    end;
  private
    fInstance: IMethodEventInternal;
    procedure CreateEventHandler(typeInfo: PTypeInfo);
  public
    function GetCanInvoke: Boolean;
    function GetEnabled: Boolean;
    procedure GetInvoke(var result; typeInfo: PTypeInfo);
    function GetOnChanged: TNotifyEvent;
    function GetUseFreeNotification: Boolean;
    procedure SetEnabled(const value: Boolean; typeInfo: PTypeInfo);
    procedure SetOnChanged(const value: TNotifyEvent; typeInfo: PTypeInfo);
    procedure SetUseFreeNotification(const value: Boolean; typeInfo: PTypeInfo);

    procedure Add(const handler; typeInfo: PTypeInfo);
    procedure Remove(const handler);
    procedure Clear;
    procedure RemoveAll(instance: Pointer);
    procedure EnsureInstance(var result; typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Spring.HazardEra,
{$IFDEF USE_RTTI_FOR_PROXY}
  Spring.VirtualInterface,
{$ENDIF}
  Spring.ResourceStrings;

const
  PointerSize = SizeOf(Pointer);


{$REGION 'Proxy generators'}

{$IFNDEF USE_RTTI_FOR_PROXY}

procedure GetMethodTypeData(Method: TRttiMethod; var TypeData: PTypeData);

  procedure WriteByte(var Dest: PByte; b: Byte);
  begin
    Dest[0] := b;
    Inc(Dest);
  end;

  procedure WritePackedShortString(var Dest: PByte; const s: string);
{$IFNDEF NEXTGEN}
  begin
    PShortString(Dest)^ := ShortString(s);
    Inc(Dest, Dest[0] + 1);
  end;
{$ELSE}
  var
    buffer: TBytes;
  begin
    buffer := TEncoding.ANSI.GetBytes(s);
    if (Length(buffer) > 255) then SetLength(buffer, 255);
    Dest^ := Length(buffer);
    Inc(Dest);
    Move(buffer[0], Dest^, Length(buffer));
    Inc(Dest, Length(buffer));
  end;
{$ENDIF}

  procedure WritePointer(var Dest: PByte; p: Pointer);
  begin
    PPointer(Dest)^ := p;
    Inc(Dest, PointerSize);
  end;

var
  params: TArray<TRttiParameter>;
  i: Integer;
  p: PByte;
begin
  TypeData.MethodKind := Method.MethodKind;
  params := Method.GetParameters;
  TypeData.ParamCount := Length(params);
  p := @TypeData.ParamList;
  for i := Low(params) to High(params) do
  begin
    WriteByte(p, Byte(params[i].Flags));
    WritePackedShortString(p, params[i].Name);
    WritePackedShortString(p, params[i].ParamType.Name);
  end;
  if method.MethodKind = mkFunction then
  begin
    WritePackedShortString(p, method.ReturnType.Name);
    WritePointer(p, method.ReturnType.Handle);
  end;
  WriteByte(p, Byte(method.CallingConvention));
  for i := Low(params) to High(params) do
    WritePointer(p, Pointer(NativeInt(params[i].ParamType.Handle) - PointerSize));
end;

class procedure TEvent.InvokeMethod(const Method: TMethod;
  Parameters: PParameters; StackSize: Integer);
{$IFNDEF CPUX64}
asm
  push ebx                        // preserve ebx
  push esi                        // preserve esi
  mov ebx,edx                     // ebx = Parameters
  mov esi,eax                     // esi = Method

  test ecx,ecx                    // if StackSize > 0
  jz @@call_method

  sub esp,ecx                     // allocate stack space
  lea eax,[ebx].TParameters.Stack // put stack buffer as first parameter
  mov edx,esp                     // put stack address as second parameter
  call Move                       // third parameter StackSize was already in place

@@call_method:
  mov ecx,[ebx].TParameters.Registers.dword[4]  // ecx = Parameters.Registers[paECX]
  mov edx,[ebx].TParameters.Registers.dword[0]  // edx = Parameters.Registers[paEDX]
  mov eax,[esi].TMethod.Data                    // eax = Method.Data
  call [esi].TMethod.Code                       // call Method.Code

  pop esi
  pop ebx
end;
{$ELSE}
asm
  push rbp                        // preserve rbp
  mov rbp,rsp                     // set rbp to current stack pointer for fixed offset
  sub rsp,r8                      // allocate stack space
  mov [rbp+$10],Method            // preserve Method
  mov [rbp+$18],Parameters        // preserve Parameters

  sub r8,32                       // if StackSize > 32
  jz @@call_method

  lea rcx,[rdx+32]                // put stack buffer after first 4 parameters as first parameter
  lea rdx,[rsp+32]                // put stack address after first 4 parameters as second parameter
  call Move

@@call_method:
  mov rax,[rbp+$18]
  mov rcx,[rax].TParameters.Stack.qword[0]      // rcx = Parameters.Stack[0]
  mov rdx,[rax].TParameters.Stack.qword[8]      // rdx = Parameters.Stack[8]
  mov r8,[rax].TParameters.Stack.qword[16]      // r8 = Parameters.Stack[16]
  mov r9,[rax].TParameters.Stack.qword[24]      // r9 = Parameters.Stack[24]

  movsd xmm0,[rax].TParameters.Stack.qword[0]   // xmm0 = Parameters.Stack[0]
  movsd xmm1,[rax].TParameters.Stack.qword[8]   // xmm1 = Parameters.Stack[8]
  movsd xmm2,[rax].TParameters.Stack.qword[16]  // xmm2 = Parameters.Stack[16]
  movsd xmm3,[rax].TParameters.Stack.qword[24]  // xmm3 = Parameters.Stack[24]

  mov rax,[rbp+$10]
  mov rcx,[rax].TMethod.Data      // rcx = Method.Data
  call [rax].TMethod.Code         // call Method.Data

  lea rsp,[rbp]                   // restore rsp - deallocate stack space
  pop rbp                         // restore
end;
{$ENDIF}

constructor TEvent.TMethodInfo.Create(typeData: PTypeData);

  function AdditionalInfoOf(TypeData: PTypeData): Pointer;
  var
    P: PByte;
    I: Integer;
  begin
    P := @TypeData^.ParamList;
    // Skip parameter names and types
    for I := 1 to TypeData^.ParamCount do //FI:W528
    begin
      Inc(P, 1 + P[1] + 1);
      Inc(P, P[0] + 1 );
    end;
    if TypeData^.MethodKind = mkFunction then
      // Skip return type name and info
      Inc(P, P[0] + 1 + 4);
    Result := P;
  end;

  function PassByRef(typeInfo: PTypeInfo; paramFlags: TParamFlags): Boolean;
  begin
    Result := (paramFlags * [pfVar, pfAddress, pfReference, pfOut] <> [])
      and not (typeInfo.Kind in [tkFloat, tkMethod, tkInt64]);
  end;

  function Align4(Value: Integer): Integer;
  begin
    {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
    // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
    Result := (Value + 3) and not 3;
  end;

var
  P: PByte;
  i: Integer;
{$IFNDEF CPUX64}
  curReg: Integer;
  Size: Integer;
{$ENDIF}
begin
  P := AdditionalInfoOf(typeData);
  if TCallConv(PByte(p)^) <> ccReg then
    raise EInvalidOperationException.CreateRes(@SUnsupportedCallingConvention);
  ParamInfos := PParameterInfos(P + 1);

{$IFNDEF CPUX64}
  curReg := paEDX;
  StackSize := 0;
{$ELSE}
  StackSize := PointerSize; // Self in stack
{$ENDIF}

  P := @typeData.ParamList;

  for i := 0 to typeData.ParamCount - 1 do
  begin
    if not Assigned(ParamInfos[i]) then
      raise EInvalidOperationException.CreateRes(@SNoTypeInfo);
{$IFNDEF CPUX64}
    if PassByRef(ParamInfos[i]^, TParamFlags(P[0])) then
    begin
      if curReg < paStack then
        Inc(curReg)
      else
        Inc(StackSize, PointerSize);
    end
    else
    begin
      Size := GetTypeSize(ParamInfos[i]^);
      if (curReg < paStack) and (Size in [1, 2, 4]) and (ParamInfos[i]^.Kind <> tkFloat) then
        Inc(curReg)
      else
        Inc(StackSize, Align4(Size));
    end;
{$ELSE}
    // pass first 3 parameters in XMM1-XMM3 if floating point (Self was already passed before them in RCX)
    if (i < 3) and (ParamInfos[i]^.Kind = tkFloat) then
      RegisterFlag := RegisterFlag or (1 shl (i + 1));
    Inc(StackSize, PointerSize);
{$ENDIF}
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1);
  end;

{$IFDEF CPUX64}
  if StackSize < 32 then
    StackSize := 32;
{$ENDIF}
end;

procedure TEvent.InvokeEventHandlerStub;
{$IFNDEF CPUX64}
asm
  // push registers - order is important, they are part of the TParameters record
  push eax
  push ecx
  push edx

  bt [eax].fRefCount,30                 // if Enabled then
  jc @@return

  mov edx,esp                           // put address to stack into Params
  mov ecx,[eax].fMethodInfo.StackSize   // put StackSize
  call [eax].fMethodInvoke

  pop edx                               // pop registers
  pop ecx                               // don't care for preserving EAX
  pop eax                               // as we don't support result

  mov ecx,[eax].fMethodInfo.StackSize
  test ecx,ecx        // if StackSize > 0
  jnz @@cleanup_stack
  ret

@@cleanup_stack:
  // clean up the stack - like the "ret n" instruction does
  mov eax,[esp]       // load the return address
  add esp,ecx         // pop from the stack
  mov [esp],eax       // write return address for ret
  ret
@@return:
  add esp,12
end;
{$ELSE}
asm
  // check DisabledFlag
  bt [rcx].fRefCount,30
  jc @@return

  // allocate stackframe
  push rbp
  sub rsp,$20
  mov rbp,rsp

  mov eax,[rcx].fMethodInfo.RegisterFlag

  // first parameter is always pointer
  mov [rsp+$30],rcx

  // second parameter: save rdx or xmm1
  test al,2
  jnz @@save_xmm1
  mov [rsp+$38],rdx
  jmp @@third
@@save_xmm1:
  movsd [rsp+$38],xmm1

  // third parameter: save r8 or xmm2
@@third:
  test al,4
  jnz @@save_xmm2
  mov [rsp+$40],r8
  jmp @@fourth
@@save_xmm2:
  movsd [rsp+$40],xmm2

  // fourth parameter: save r9 or xmm3
@@fourth:
  test al,8
  jnz @@save_xmm3
  mov [rsp+$48],r9
  jmp @@call
@@save_xmm3:
  movsd [rsp+$48],xmm3

@@call:
  lea rdx,[rsp+$30]                     // put stack address into Params
  mov r8d,[rcx].fMethodInfo.StackSize   // pass StackSize
  call [rcx].fMethodInvoke

  // restore stack pointer
  lea rsp,[rbp+$20]
  pop rbp

@@return:
end;
{$ENDIF}
{$ENDIF}

{$ENDREGION}


{$REGION 'TEvent'}

constructor TEvent.Create(typeInfo: PTypeInfo);
var
  method: TRttiMethod;
{$IFNDEF USE_RTTI_FOR_PROXY}
  typeData: PTypeData;
  invokeEvent: procedure(Params: Pointer; StackSize: Integer) of object;
{$ENDIF}
begin
  fTypeInfo := typeInfo;
  if not Assigned(typeInfo) then
    raise EInvalidOperationException.CreateRes(@SNoTypeInfo);

  inherited Create;

  case typeInfo.Kind of
    tkMethod:
    begin
{$IFDEF USE_RTTI_FOR_PROXY}
      TMethodImplementation(fProxy) := TRttiInvokableType(typeInfo.RttiType)
        .CreateImplementation(nil, InternalInvokeMethod);
      TMethod(fInvoke) := TMethodImplementation(fProxy).AsMethod;
{$ELSE}
      typeData := typeInfo.TypeData;
      fMethodInfo := TMethodInfo.Create(typeData);
      invokeEvent := InternalInvoke;
      fMethodInvoke := TMethod(invokeEvent).Code;
      fInvoke := InvokeEventHandlerStub;
{$ENDIF}
    end;
    tkInterface:
    begin
      method := typeInfo.RttiType.GetMethod('Invoke');
      if not Assigned(method) then
        raise EInvalidOperationException.CreateResFmt(@STypeParameterContainsNoRtti, [typeInfo.Name]);
{$IFDEF USE_RTTI_FOR_PROXY}
      TVirtualInterface.Create(typeInfo, InternalInvokeDelegate)
        .QueryInterface(typeInfo.TypeData.Guid, fProxy);
{$ELSE}
      New(typeData);
      try
        GetMethodTypeData(method, typeData);
        fMethodInfo := TMethodInfo.Create(typeData);
        invokeEvent := InternalInvoke;
        fMethodInvoke := TMethod(invokeEvent).Code;
        fInvoke := InvokeEventHandlerStub;
      finally
        Dispose(typeData);
      end;
{$ENDIF}
    end
  else
    raise EInvalidOperationException.CreateResFmt(@STypeParameterShouldBeMethod, [typeInfo.Name]);
  end;
end;

destructor TEvent.Destroy;
begin
{$IFDEF USE_RTTI_FOR_PROXY}
  case fTypeInfo.Kind of
    tkMethod: TMethodImplementation(fProxy).Free;
    tkInterface: IInterface(fProxy) := nil;
  end;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF USE_RTTI_FOR_PROXY}
procedure TEvent.InternalInvokeMethod(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
var
  argsWithoutSelf: TArray<TValue>;
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: Integer;
  value: TValue;
begin
  if CanInvoke then
  begin
    argsWithoutSelf := Copy(Args, 1);
    guard := AcquireGuard(fHandlers);
    handlers := guard;
    try
      for i := 0 to DynArrayHigh(handlers) do
      begin
        TValue.Make(@TMethod(handlers[i]), TRttiInvokableType(UserData).Handle, value);
        TRttiInvokableType(UserData).Invoke(value, argsWithoutSelf);
      end;
    finally
      guard.Release;
    end;
  end;
end;

procedure TEvent.InternalInvokeDelegate(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  argsWithoutSelf: TArray<TValue>;
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: Integer;
  reference: IInterface;
  value: TValue;
begin
  if CanInvoke then
  begin
    argsWithoutSelf := Copy(Args, 1);
    guard := AcquireGuard(fHandlers);
    handlers := guard;
    try
      for i := 0 to DynArrayHigh(handlers) do
      begin
        reference := MethodToMethodReference(handlers[i]);
        TValue.Make(@reference, TypeInfo(IInterface), value);
        method.Invoke(value, argsWithoutSelf);
      end;
    finally
      guard.Release;
    end;
  end;
end;
{$ELSE}

procedure TEvent.InternalInvoke(Params: Pointer; StackSize: Integer);
var
  guard: GuardedPointer;
  handlers: PMethod;
  i: Integer;
begin
  guard := AcquireGuard(fHandlers);
  handlers := guard;
  if handlers <> nil then
  try
    {$POINTERMATH ON}
    for i := 1 to PNativeInt(handlers)[-1] do
    {$POINTERMATH OFF}
    begin
      InvokeMethod(handlers^, Params, StackSize);
      Inc(handlers);
    end;
  except
    guard.Release;
    raise;
  end;
  guard.Release;
end;

procedure TEvent.Invoke;
asm
{$IFDEF CPUX64}
  mov rax,[rcx].fInvoke.TMethod.Code
  mov rcx,[rcx].fInvoke.TMethod.Data
  jmp rax
{$ELSE}
  push [eax].fInvoke.TMethod.Code
  mov eax,[eax].fInvoke.TMethod.Data
{$ENDIF}
end;
{$ENDIF}

procedure TEvent.Notify(Sender: TObject; const Item: TMethod;
  Action: TEventBase.TCollectionNotification);
begin
  inherited Notify(Sender, Item, Action);
  if fTypeInfo.Kind = tkInterface then
    case Action of //FI:W535
      cnAdded: IInterface(Item.Data)._AddRef;
      cnRemoved: IInterface(Item.Data)._Release;
    end;
end;

{$ENDREGION}


{$REGION 'TNotifyEventImpl'}

procedure TNotifyEventImpl.AfterConstruction;
begin
  inherited AfterConstruction;
  TNotifyEvent(fInvoke) := Invoke;
end;

procedure TNotifyEventImpl.Add(handler: TNotifyEvent);
begin
  inherited Add(TMethod(handler));
end;

function TNotifyEventImpl.GetInvoke: TNotifyEvent;
begin
  Result := TNotifyEvent(inherited Invoke);
end;

procedure TNotifyEventImpl.Invoke(sender: TObject);
var
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: Integer;
begin
  if Enabled then
  begin
    guard := GetHandlers;
    handlers := guard;
    try
      for i := 0 to DynArrayHigh(handlers) do
        TNotifyEvent(handlers[i])(sender);
    finally
      guard.Release;
    end;
  end;
end;

procedure TNotifyEventImpl.Remove(handler: TNotifyEvent);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


{$REGION 'TNotifyEventImpl<T>'}

procedure TNotifyEventImpl<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  TNotifyEvent<T>(fInvoke) := Invoke;
end;

procedure TNotifyEventImpl<T>.Add(handler: TNotifyEvent<T>);
begin
  inherited Add(TMethod(handler));
end;

function TNotifyEventImpl<T>.GetInvoke: TNotifyEvent<T>;
begin
  Result := TNotifyEvent<T>(inherited Invoke);
end;

procedure TNotifyEventImpl<T>.Invoke(sender: TObject; const item: T);
var
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: Integer;
begin
  if Enabled then
  begin
    guard := GetHandlers;
    handlers := guard;
    try
      for i := 0 to DynArrayHigh(handlers) do
        TNotifyEvent<T>(handlers[i])(sender, item);
    finally
      guard.Release;
    end;
  end;
end;

procedure TNotifyEventImpl<T>.Remove(handler: TNotifyEvent<T>);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


{$REGION 'TPropertyChangedEventImpl'}

procedure TPropertyChangedEventImpl.AfterConstruction;
begin
  inherited AfterConstruction;
  TPropertyChangedEvent(fInvoke) := Invoke;
end;

procedure TPropertyChangedEventImpl.Add(handler: TPropertyChangedEvent);
begin
  inherited Add(TMethod(handler));
end;

function TPropertyChangedEventImpl.GetInvoke: TPropertyChangedEvent;
begin
  Result := TPropertyChangedEvent(inherited Invoke);
end;

procedure TPropertyChangedEventImpl.Invoke(Sender: TObject;
  const EventArgs: IPropertyChangedEventArgs);
var
  guard: GuardedPointer;
  handlers: PMethodArray;
  i: Integer;
begin
  if Enabled then
  begin
    guard := GetHandlers;
    handlers := guard;
    try
      for i := 0 to DynArrayHigh(handlers) do
        TPropertyChangedEvent(handlers[i])(Sender, EventArgs);
    finally
      guard.Release;
    end;
  end;
end;

procedure TPropertyChangedEventImpl.Remove(handler: TPropertyChangedEvent);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


{$REGION 'EventHelper'}

procedure EventHelper.CreateEventHandler(typeInfo: PTypeInfo);
begin
  if typeInfo.Kind = tkMethod then
    IMethodEventInternal(fInstance) := TMethodEvent.Create(typeInfo)
  else
    IDelegateEventInternal(fInstance) := TDelegateEvent.Create(typeInfo);
end;

procedure EventHelper.Add(const handler; typeInfo: PTypeInfo);
begin
  if not Assigned(fInstance) then
    CreateEventHandler(typeInfo);
  fInstance.Add(handler);
end;

procedure EventHelper.Clear;
begin
  if Assigned(fInstance) then
    fInstance.Clear;
end;

procedure EventHelper.EnsureInstance(var result; typeInfo: PTypeInfo);
begin
  if not Assigned(fInstance) then
    CreateEventHandler(typeInfo);
  IInterface(result) := fInstance;
end;

function EventHelper.GetCanInvoke: Boolean;
begin
  if Assigned(fInstance) then
    Result := fInstance.CanInvoke
  else
    Result := False;
end;

function EventHelper.GetEnabled: Boolean;
begin
  if Assigned(fInstance) then
    Result := fInstance.Enabled
  else
    Result := True
end;

procedure EventHelper.GetInvoke(var result; typeInfo: PTypeInfo);
begin
  if not Assigned(fInstance) then
    CreateEventHandler(typeInfo);
  fInstance.GetInvoke(result);
end;

function EventHelper.GetOnChanged: TNotifyEvent;
begin
  if Assigned(fInstance) then
    Result := fInstance.OnChanged
  else
    Result := nil;
end;

function EventHelper.GetUseFreeNotification: Boolean;
begin
  if Assigned(fInstance) then
    Result := fInstance.UseFreeNotification
  else
    Result := True
end;

procedure EventHelper.Remove(const handler);
begin
  if Assigned(fInstance) then
    fInstance.Remove(handler);
end;

procedure EventHelper.RemoveAll(instance: Pointer);
begin
  if Assigned(fInstance) then
    fInstance.RemoveAll(instance);
end;

procedure EventHelper.SetEnabled(const value: Boolean; typeInfo: PTypeInfo);
begin
  if not Assigned(fInstance) then
    CreateEventHandler(typeInfo);
  fInstance.Enabled := value;
end;

procedure EventHelper.SetOnChanged(const value: TNotifyEvent; typeInfo: PTypeInfo);
begin
  if not Assigned(fInstance) then
    CreateEventHandler(typeInfo);
  fInstance.OnChanged := value;
end;

procedure EventHelper.SetUseFreeNotification(const value: Boolean; typeInfo: PTypeInfo);
begin
  if not Assigned(fInstance) then
    CreateEventHandler(typeInfo);
  fInstance.UseFreeNotification := value;
end;

{$ENDREGION}


{$REGION 'EventHelper.TMethodEvent'}

function EventHelper.TMethodEvent.GetInvoke: TMethodPointer;
begin
  Result := Invoke;
end;

procedure EventHelper.TMethodEvent.Add(handler: TMethodPointer);
begin
  inherited Add(TMethod(handler));
end;

procedure EventHelper.TMethodEvent.Remove(handler: TMethodPointer);
begin
  inherited Remove(TMethod(handler));
end;

procedure EventHelper.TMethodEvent.GetInvoke(var result);
begin
  TMethodPointer(result) := fInvoke;
end;

procedure EventHelper.TMethodEvent.Add(const handler);
begin
  inherited Add(TMethod(handler));
end;

procedure EventHelper.TMethodEvent.Remove(const handler);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


{$REGION 'EventHelper.TDelegateEvent'}

function EventHelper.TDelegateEvent.GetInvoke: IInterface;
begin
{$IFDEF USE_RTTI_FOR_PROXY}
  PInterface(@Result)^ := IInterface(fProxy);
{$ELSE}
  TProc(PPointer(@Result)^) := Self;
{$ENDIF}
end;

procedure EventHelper.TDelegateEvent.Add(handler: IInterface);
begin
  inherited Add(MethodReferenceToMethod(handler));
end;

procedure EventHelper.TDelegateEvent.Remove(handler: IInterface);
begin
  inherited Remove(MethodReferenceToMethod(handler));
end;

procedure EventHelper.TDelegateEvent.GetInvoke(var result);
begin
{$IFDEF USE_RTTI_FOR_PROXY}
  IInterface(result) := IInterface(fProxy);
{$ELSE}
  TProc(result) := Self;
{$ENDIF}
end;

procedure EventHelper.TDelegateEvent.Add(const handler);
begin
  inherited Add(MethodReferenceToMethod(handler));
end;

procedure EventHelper.TDelegateEvent.Remove(const handler);
begin
  inherited Remove(MethodReferenceToMethod(handler));
end;

{$ENDREGION}


end.
