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

unit Spring.Coroutines;

interface

uses
{$IFDEF USE_WINDOWS_FIBERS}
  Windows,
{$ENDIF}
  Classes,
  Rtti,
  SyncObjs,
  SysUtils,
  Spring;

type
  TSpinEvent = class
  private
    fEvent: TEvent;
    fSignaled: Boolean;
    fSpinCount: Word;
    const DefaultSpinCount = 10;
  public
    constructor Create(spinCount: Word = DefaultSpinCount);
    destructor Destroy; override;
    procedure SetEvent;
    procedure WaitFor;
  end;

{$IFDEF USE_WINDOWS_FIBERS}
  TFiber = class
  private
    fFatalException: TObject;
    fFinished: Boolean;
    fFreeOnTerminate: Boolean;
    fHandle: Pointer;
    fInvoker: Pointer;
    fTerminated: Boolean;
    procedure FiberProc; stdcall;
  private class threadvar
    fCoroutineCount: Integer;
    fThreadFiber: Pointer;
  protected
    procedure Execute; virtual; abstract;
    procedure Yield; inline;
  public
    constructor Create;
    destructor Destroy; override;

    class function CurrentFiber: TFiber; static;

    procedure Continue;
    procedure Invoke;

    procedure Terminate; inline;

    property FatalException: TObject read fFatalException;
    property Finished: Boolean read fFinished;
    property FreeOnTerminate: Boolean
      read fFreeOnTerminate write fFreeOnTerminate;
    property Terminated: Boolean read fTerminated;
  end;
{$ELSE}
  TFiber = class(TThread)
  private
    // DO NOT CHANGE ORDER!!!
    fEvent: TSpinEvent;
    fInvoker: TFiber;
    fMainFiber: TFiber;
  private class threadvar
    fCoroutineCount: Integer;
    fThreadFiber: TFiber;
    procedure SwitchToFiber;
  protected
    procedure DoTerminate; override;
    procedure Yield; inline;
  public
    constructor Create;
    destructor Destroy; override;

    class function CurrentFiber: TFiber; static;

    procedure Continue;
    procedure Invoke; inline;
  end;
{$ENDIF}

  TCoroutine = class(TFiber, IInterface, TProc, Action)
  private
{$IFNDEF AUTOREFCOUNT}
    fRefCount: Integer;
{$ENDIF}
    fInvoking: Boolean;
    fIsLooping: Boolean;
    fAction: Action;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    class function GetCurrentCoroutine: TCoroutine; static; inline;
  protected
    procedure Execute; override;
    function MoveNext: Boolean;
    procedure Reset;

    /// <summary>
    ///   Returns execution to the caller that invoked the coroutine by calling
    ///   <see cref="Spring.Coroutines|TCoroutine.Invoke">Invoke</see> or to
    ///   the main program if it was invoked by calling <see cref="Spring.Coroutines|TCoroutine.Continue">
    ///   Continue</see>.
    /// </summary>
    procedure Yield; overload; inline;
    procedure Yield(const value); overload; virtual;
    procedure Yield(const value: TValue); overload; virtual;
  public
    constructor Create(const action: Action);

    /// <summary>
    ///   Passes execution to the coroutine without attaching the caller.
    ///   Calling <see cref="Spring.Coroutines|TCoroutine.Yield">Yield</see>
    ///   will return execution to the main program.
    /// </summary>
    procedure Continue; inline;

    /// <summary>
    ///   Passes execution to the coroutine and attaches it to the caller.
    ///   Calling <see cref="Spring.Coroutines|TCoroutine.Yield">Yield</see>
    ///   will return execution to that caller.
    /// </summary>
    procedure Invoke;

    class procedure Yield<T>(const value: T); overload; inline;

    property IsLooping: Boolean read fIsLooping write fIsLooping;
    class property CurrentCoroutine: TCoroutine read GetCurrentCoroutine;
  end;

  TCoroutine<T> = class(TCoroutine)
  private
    fCurrent: T;
  protected
    function GetCurrent: T;
    procedure Yield(const value); overload; override;
    procedure Yield(const value: TValue); overload; override;
    procedure Yield(const value: T); overload;
  public
    function Invoke: T;
    property Current: T read GetCurrent;
  end;

  TCoroutine<T1, TResult> = class(TCoroutine<TResult>, Func<T1, TResult>)
  private
    fArg1: T1;
  public
    constructor Create(const action: Action<T1>); overload;
    function Bind(const arg1: T1): Func<TResult>;
    function Invoke(const arg1: T1): TResult;
  end;

  TCoroutine<T1, T2, TResult> = class(TCoroutine<TResult>,
    Func<T1, T2, TResult>)
  private
    fArg1: T1;
    fArg2: T2;
  public
    constructor Create(const action: Action<T1, T2>);
    function Bind(const arg1: T1; const arg2: T2): Func<TResult>;
    function Invoke(const arg1: T1; const arg2: T2): TResult;
  end;

  TCoroutine<T1, T2, T3, TResult> = class(TCoroutine<TResult>,
    Func<T1, T2, T3, TResult>)
  private
    fArg1: T1;
    fArg2: T2;
    fArg3: T3;
  public
    constructor Create(const action: Action<T1, T2, T3>);
    function Bind(const arg1: T1; const arg2: T2; const arg3: T3): Func<TResult>;
    function Invoke(const arg1: T1; const arg2: T2; const arg3: T3): TResult;
  end;

  TCoroutine<T1, T2, T3, T4, TResult> = class(TCoroutine<TResult>,
    Func<T1, T2, T3, T4, TResult>)
  private
    fArg1: T1;
    fArg2: T2;
    fArg3: T3;
    fArg4: T4;
  public
    constructor Create(const action: Action<T1, T2, T3, T4>);
    function Bind(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4): Func<TResult>;
    function Invoke(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4): TResult;
  end;

  ECoroutineException = class(Exception);

  Yield<T> = record
  private
    function GetValue: T; inline;
  public
    class operator Implicit(const value: T): Yield<T>; inline;
    class operator Implicit(const value: Yield<T>): T; inline;
    property Value: T read GetValue;
  end;


{$REGION 'Yield routines'}

procedure Yield; overload; inline;
procedure Yield(const value); overload; inline;

procedure Yield(const value: Integer); overload; inline;
procedure Yield(const value: string); overload; inline;

{$ENDREGION}


{$REGION 'Fiber routines'}

{$IFDEF USE_WINDOWS_FIBERS}
function ConvertFiberToThread: Pointer; stdcall; external kernel32;
function ConvertThreadToFiber(lpParameter: Pointer): Pointer; stdcall; external kernel32;
function CreateFiber(dwStackSize: Cardinal; lpStartAddress: Pointer;
  lpParameter: Pointer): Pointer; stdcall; external kernel32;
procedure DeleteFiber(lpFiber: Pointer); stdcall; external kernel32;
procedure SwitchToFiber(lpFiber: Pointer); stdcall; external kernel32;
{$ENDIF}

{$ENDREGION}


resourcestring
  SCoroutineNotRunning = 'coroutine not running';
  SCoroutineFinished = 'coroutine finished';
  SCoroutineTerminated = 'coroutine terminated';
  SCoroutineCalledRecursively = 'coroutine called recursively';

implementation


{$REGION 'TThreadHelper'}

type
  TThreadHelper = class helper for TThread
  class var
    FStartedOffset: Integer;
    FFinishedOffset: Integer;
    FExternalThreadOffset: Integer;
    class procedure Init;
    procedure InitExternalThread; inline;
    procedure SetFinished(value: Boolean); inline;
  end;

class procedure TThreadHelper.Init;
var
  ctx: TRttiContext;
begin
  with ctx.GetType(TThread) do
  begin
{$IFDEF DELPHIXE5_UP}
    FStartedOffset := GetField('FStarted').Offset;
{$ENDIF}
    FFinishedOffset := GetField('FFinished').Offset;
    FExternalThreadOffset := GetField('FExternalThread').Offset;
  end;
end;

procedure TThreadHelper.InitExternalThread;
begin
  PBoolean(PByte(Self) + FExternalThreadOffset)^ := True;
{$IFDEF DELPHIXE5_UP}
  PBoolean(PByte(Self) + FStartedOffset)^ := True;
{$ENDIF}
end;

procedure TThreadHelper.SetFinished(value: Boolean);
begin
  PBoolean(PByte(Self) + FFinishedOffset)^ := True;
end;

{$ENDREGION}


{$REGION 'Yield routines'}

procedure Yield;
begin
  TCoroutine.CurrentCoroutine.Yield;
end;

procedure Yield(const value);
begin
  TCoroutine.CurrentCoroutine.Yield(value);
end;

procedure Yield(const value: Integer);
begin
  TCoroutine.Yield<Integer>(value);
end;

procedure Yield(const value: string);
begin
  TCoroutine.Yield<string>(value);
end;

{$ENDREGION}


{$REGION 'Fiber routines'}

{$IFDEF USE_WINDOWS_FIBERS}
function GetCurrentFiber: Pointer;
asm
{$IFDEF CPUX64}
  mov rax,gs:[abs $20]
{$ELSE}
  mov eax,fs:[$10]
{$ENDIF}
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'Yield<T>'}

function Yield<T>.GetValue: T;
begin
  Result := TCoroutine<T>(TCoroutine.CurrentCoroutine).Current;
end;

class operator Yield<T>.Implicit(const value: T): Yield<T>;
begin
  TCoroutine.Yield<T>(value);
end;

class operator Yield<T>.Implicit(const value: Yield<T>): T;
begin
  Result := TCoroutine<T>(TCoroutine.CurrentCoroutine).Current;
end;

{$ENDREGION}


{$REGION 'TSpinEvent' }

constructor TSpinEvent.Create(spinCount: Word);
begin
  inherited Create;
  fEvent := TEvent.Create(nil, True, False, '');
  fSpinCount := spinCount;
end;

destructor TSpinEvent.Destroy;
begin
  fEvent.Free;
  inherited;
end;

procedure TSpinEvent.SetEvent;
begin
  Assert(not fSignaled);
  fSignaled := True;
  fEvent.SetEvent;
end;

procedure TSpinEvent.WaitFor;
const
  SpinCountBeforeYield = 10;
  YieldCountPerSleep0 = 5;
  YieldCountPerSleep1 = 20;
var
  i: Integer;
begin
  try
    if fSignaled then
      Exit;
    for i := 0 to fSpinCount - 1 do
    begin
      if fSignaled then
        Exit;
      if i < SpinCountBeforeYield then
        if i = SpinCountBeforeYield / 2 then
          TThread.Yield
        else
          TThread.SpinWait(TThread.ProcessorCount * (4 shl i))
      else if i mod YieldCountPerSleep1 = 0 then
        TThread.Sleep(1)
      else if i mod YieldCountPerSleep0 = 0 then
        TThread.Sleep(0)
      else
        TThread.Yield;
    end;
    fEvent.WaitFor(INFINITE);
  finally
    fSignaled := False;
    fEvent.ResetEvent;
  end;
end;

{$ENDREGION}


{$REGION 'TFiber'}

{$IFDEF USE_WINDOWS_FIBERS}
constructor TFiber.Create;
begin
  if not Assigned(fThreadFiber) then
    fThreadFiber := ConvertThreadToFiber(nil);
  Inc(fCoroutineCount);
  fInvoker := fThreadFiber;
  fHandle := CreateFiber(0, @TFiber.FiberProc, Self);
end;

destructor TFiber.Destroy;
begin
  if not Finished then
  begin
    Terminate;
    SwitchToFiber(fHandle);
  end;
  Dec(fCoroutineCount);
  if not FreeOnTerminate then
  begin
    DeleteFiber(fHandle);
    if Assigned(fThreadFiber) and (fCoroutineCount = 0) then
    begin
      ConvertFiberToThread;
      fThreadFiber := nil;
    end;
  end;
  inherited Destroy;
  fFatalException.Free;
end;

procedure TFiber.Continue;
var
  current: TFiber;
begin
  if not Finished then
    SwitchToFiber(fHandle);
  // make sure that coroutines do not continue after being terminated
  current := CurrentFiber;
  if Assigned(current) and current.Terminated then
    Abort;
end;

class function TFiber.CurrentFiber: TFiber;
begin
  if fCoroutineCount > 0 then
    Result := PPointer(GetCurrentFiber)^
  else
    Result := nil;
end;

procedure TFiber.FiberProc;
var
  mainFiber: Pointer;
begin
  try
    if not fTerminated then
    try
      Execute;
    except
      on EAbort do;
      on Exception do
        fFatalException := AcquireExceptionObject;
    end;
  finally
    mainFiber := fInvoker;
    fFinished := True;
    if fFreeOnTerminate then
{$IFNDEF AUTOREFCOUNT}
      Destroy;
{$ELSE}
      DisposeOf;
{$ENDIF}
    SwitchToFiber(mainFiber);
  end;
end;

procedure TFiber.Invoke;
begin
  fInvoker := GetCurrentFiber;
  try
    Continue;
  finally
    fInvoker := fThreadFiber;
  end;
end;

procedure TFiber.Terminate;
begin
  fTerminated := True;
end;

procedure TFiber.Yield;
begin
  SwitchToFiber(fInvoker);
  if Terminated then
    Abort;
end;

{$ELSE}

type
  TBaseFiber = class(TThread)
  private
    fEvent: TSpinEvent;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TBaseFiber.Create;
begin
  InitExternalThread;
  inherited Create(False);
  fEvent := TSpinEvent.Create;
end;

destructor TBaseFiber.Destroy;
begin
  inherited Destroy;
  fEvent.Free;
end;

procedure TBaseFiber.Execute;
begin
end;

constructor TFiber.Create;
begin
  if TInterlocked.Increment(fCoroutineCount) = 1 then
    fThreadFiber := TFiber(TBaseFiber.Create);
  fMainFiber := fThreadFiber;
  fInvoker := fMainFiber;
  fEvent := TSpinEvent.Create;
  inherited Create(True);
end;

destructor TFiber.Destroy;
begin
  if (ThreadID <> 0) and not Finished then
  begin
    Terminate;
    SwitchToFiber;
  end;
  inherited Destroy;
  if TInterlocked.Decrement(fCoroutineCount) = 0 then
    FreeAndNil(fThreadFiber);
  fEvent.Free;
end;

procedure TFiber.Continue;
var
  current: TFiber;
begin
  if not Finished then
    SwitchToFiber;

  current := CurrentFiber;
  if Assigned(current) and current.Terminated then
    Abort;
end;

class function TFiber.CurrentFiber: TFiber;
var
  currentThread: TThread;
begin
  currentThread := TThread.CurrentThread;
  if currentThread is TFiber then
    Result := TFiber(currentThread)
  else
    Result := fThreadFiber;
end;

procedure TFiber.DoTerminate;
begin
  inherited DoTerminate;
  SetFinished(True);
  fInvoker.fEvent.SetEvent;
end;

procedure TFiber.Invoke;
begin
  fInvoker := CurrentFiber;
  try
    Continue;
  finally
    fInvoker := fMainFiber;
  end;
end;

procedure TFiber.SwitchToFiber;
begin
  if Suspended then
    Start
  else
    fEvent.SetEvent;
  TFiber.CurrentFiber.fEvent.WaitFor;
end;

procedure TFiber.Yield;
begin
  fInvoker.SwitchToFiber;
  if Terminated then
    Abort;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TCoroutine'}

constructor TCoroutine.Create(const action: Action);
begin
  inherited Create;
  fAction := action;
  fIsLooping := True;
end;

procedure TCoroutine.Continue;
begin
  inherited Continue;
end;

procedure TCoroutine.Execute;
begin
  repeat
    fAction;
  until not fIsLooping or Terminated;
end;

class function TCoroutine.GetCurrentCoroutine: TCoroutine;
begin
  Result := TCoroutine(CurrentFiber);
  if Result = nil then
    raise ECoroutineException.CreateRes(@SCoroutineNotRunning);
end;

procedure TCoroutine.Invoke;
begin
  if fInvoking then
    raise ECoroutineException.CreateRes(@SCoroutineCalledRecursively);
  fInvoking := True;
  try
    inherited Invoke;
    if Assigned(FatalException) then
      if FatalException is Exception then
        raise ECoroutineException.Create(Exception(FatalException).Message);
  finally
    fInvoking := False;
  end;
end;

function TCoroutine.MoveNext: Boolean;
begin
  try
    Invoke;
    Result := not Finished and not Terminated;
  except
    on E: EAbort do
      Result := False
    else
      raise;
  end;
end;

function TCoroutine.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TCoroutine.Reset;
begin
end;

procedure TCoroutine.Yield;
begin
  inherited Yield;
end;

procedure TCoroutine.Yield(const value);
begin
  inherited Yield;
end;

procedure TCoroutine.Yield(const value: TValue);
begin
  inherited Yield;
end;

class procedure TCoroutine.Yield<T>(const value: T);
var
  fiber: TFiber;
begin
  fiber := TCoroutine.CurrentFiber;
  if fiber = nil then
    raise ECoroutineException.CreateRes(@SCoroutineNotRunning);
  TCoroutine<T>(fiber).Yield(value);
end;

function TCoroutine._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := TInterlocked.Increment(fRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TCoroutine._Release: Integer;
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := TInterlocked.Decrement(fRefCount);
  if Result = 0 then
    Destroy;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TCoroutine<T>'}

function TCoroutine<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TCoroutine<T>.Invoke: T;
begin
  inherited Invoke;
  Result := fCurrent;
end;

procedure TCoroutine<T>.Yield(const value);
begin
  Yield(T(value));
end;

procedure TCoroutine<T>.Yield(const value: TValue);
begin
  Yield(value.AsType<T>);
end;

procedure TCoroutine<T>.Yield(const value: T);
begin
  if not Terminated then
    fCurrent := value
  else
    Abort;
  inherited Yield;
end;

{$ENDREGION}


{$REGION 'TCoroutine<T1, TResult>'}

constructor TCoroutine<T1, TResult>.Create(const action: Action<T1>);
begin
  inherited Create(
    procedure
    begin
      action(fArg1);
    end);
end;

function TCoroutine<T1, TResult>.Bind(const arg1: T1): Func<TResult>;
var
  func: Func<T1, TResult>;
begin
  func := Self;
  Result :=
    function: TResult
    begin
      Result := func(arg1);
    end;
end;

function TCoroutine<T1, TResult>.Invoke(const arg1: T1): TResult;
begin
  fArg1 := arg1;
  Result := inherited Invoke;
end;

{$ENDREGION}


{$REGION 'TCoroutine<T1, T2, TResult>'}

constructor TCoroutine<T1, T2, TResult>.Create(const action: Action<T1, T2>);
begin
  inherited Create(
    procedure
    begin
      action(fArg1, fArg2);
    end);
end;

function TCoroutine<T1, T2, TResult>.Bind(const arg1: T1;
  const arg2: T2): Func<TResult>;
var
  func: Func<T1, T2, TResult>;
begin
  func := Self;
  Result :=
    function: TResult
    begin
      Result := func(arg1, arg2);
    end;
end;

function TCoroutine<T1, T2, TResult>.Invoke(const arg1: T1;
  const arg2: T2): TResult;
begin
  fArg1 := arg1;
  fArg2 := arg2;
  Result := inherited Invoke;
end;

{$ENDREGION}


{$REGION 'TCoroutine<T1, T2, T3, TResult>'}

constructor TCoroutine<T1, T2, T3, TResult>.Create(
  const action: Action<T1, T2, T3>);
begin
  inherited Create(
    procedure
    begin
      action(fArg1, fArg2, fArg3);
    end);
end;

function TCoroutine<T1, T2, T3, TResult>.Bind(const arg1: T1; const arg2: T2;
  const arg3: T3): Func<TResult>;
var
  func: Func<T1, T2, T3, TResult>;
begin
  func := Self;
  Result :=
    function: TResult
    begin
      Result := func(arg1, arg2, arg3);
    end;
end;

function TCoroutine<T1, T2, T3, TResult>.Invoke(const arg1: T1; const arg2: T2;
  const arg3: T3): TResult;
begin
  fArg1 := arg1;
  fArg2 := arg2;
  fArg3 := arg3;
  Result := inherited Invoke;
end;

{$ENDREGION}


{$REGION 'TCoroutine<T1, T2, T3, T4, TResult>'}

constructor TCoroutine<T1, T2, T3, T4, TResult>.Create(
  const action: Action<T1, T2, T3, T4>);
begin
  inherited Create(
    procedure
    begin
      action(fArg1, fArg2, fArg3, fArg4);
    end);
end;

function TCoroutine<T1, T2, T3, T4, TResult>.Bind(const arg1: T1;
  const arg2: T2; const arg3: T3; const arg4: T4): Func<TResult>;
var
  func: Func<T1, T2, T3, T4, TResult>;
begin
  func := Self;
  Result :=
    function: TResult
    begin
      Result := func(arg1, arg2, arg3, arg4);
    end;
end;

function TCoroutine<T1, T2, T3, T4, TResult>.Invoke(const arg1: T1;
  const arg2: T2; const arg3: T3; const arg4: T4): TResult;
begin
  fArg1 := arg1;
  fArg2 := arg2;
  fArg3 := arg3;
  fArg4 := arg4;
  Result := inherited Invoke;
end;

{$ENDREGION}


{$IFNDEF USE_WINDOWS_FIBERS}
initialization
  TThread.Init;
{$ENDIF}

end.
