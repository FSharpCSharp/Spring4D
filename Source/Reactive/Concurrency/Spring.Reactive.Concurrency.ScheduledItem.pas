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

unit Spring.Reactive.Concurrency.ScheduledItem;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Reactive;

type
  IScheduledItem<TAbsolute> = interface
    function GetDueTime: TAbsolute;
    function GetIsCanceled: Boolean;

    procedure Cancel;
    procedure Invoke;

    property DueTime: TAbsolute read GetDueTime;
    property IsCanceled: Boolean read GetIsCanceled;
  end;

  IScheduledItem<TAbsolute, TValue> = interface(IScheduledItem<TAbsolute>)
  end;

  TScheduledItem<TAbsolute> = class abstract(TInterfacedObject, IScheduledItem<TAbsolute>, IComparable)
  private
    fDisposable: ISingleAssignmentDisposable;
    fDueTime: TAbsolute;
    fComparer: IComparer<TAbsolute>;
    function GetDueTime: TAbsolute;
    function GetIsCanceled: Boolean;
  strict protected
    constructor Create(const dueTime: TAbsolute; const comparer: IComparer<TAbsolute>);
    function InvokeCore: IDisposable; virtual; abstract;
  public
    procedure Cancel;
    procedure Invoke;

    function CompareTo(const other: TObject): Integer;

    property DueTime: TAbsolute read GetDueTime;
    property IsCanceled: Boolean read GetIsCanceled;
  end;

  TScheduledItem<TAbsolute, TValue> = class(TScheduledItem<TAbsolute>, IScheduledItem<TAbsolute, TValue>)
  public
//    [Unsafe]
    fScheduler: IScheduler;
    fState: TValue;
    fAction: Func<IScheduler, TValue, IDisposable>;
  protected
    function InvokeCore: IDisposable; override;
  public
    constructor Create(const scheduler: IScheduler; const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>;
      const dueTime: TAbsolute; const comparer: IComparer<TAbsolute>); overload;
    constructor Create(const scheduler: IScheduler; const state: TValue;
      const action: Func<IScheduler, TValue, IDisposable>;
      const dueTime: TAbsolute); overload;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TScheduledItem<TAbsolute>'}

constructor TScheduledItem<TAbsolute>.Create(const dueTime: TAbsolute;
  const comparer: IComparer<TAbsolute>);
begin
  inherited Create;
  fDisposable := TSingleAssignmentDisposable.Create;
  fDueTime := dueTime;
  fComparer := comparer;
end;

procedure TScheduledItem<TAbsolute>.Cancel;
begin
  fDisposable.Dispose;
end;

function TScheduledItem<TAbsolute>.GetDueTime: TAbsolute;
begin
  Result := fDueTime;
end;

function TScheduledItem<TAbsolute>.GetIsCanceled: Boolean;
begin
  Result := fDisposable.IsDisposed;
end;

procedure TScheduledItem<TAbsolute>.Invoke;
begin
  if not fDisposable.IsDisposed then
    fDisposable.Disposable := InvokeCore;
end;

function TScheduledItem<TAbsolute>.CompareTo(const other: TObject): Integer;
begin
  Result := fComparer.Compare(DueTime, TScheduledItem<TAbsolute>(other).DueTime);
end;

{$ENDREGION}


{$REGION 'TScheduledItem<TAbsolute, TValue>'}

constructor TScheduledItem<TAbsolute, TValue>.Create(
  const scheduler: IScheduler; const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>; const dueTime: TAbsolute;
  const comparer: IComparer<TAbsolute>);
begin
  inherited Create(dueTime, comparer);
  fScheduler := scheduler;
  fState := state;
  fAction := action;
end;

constructor TScheduledItem<TAbsolute, TValue>.Create(
  const scheduler: IScheduler; const state: TValue;
  const action: Func<IScheduler, TValue, IDisposable>;
  const dueTime: TAbsolute);
var
  comparer: IComparer<TAbsolute>;
begin
  if TypeInfo(TAbsolute) = TypeInfo(TTimeSpan) then
    TComparison<TTimeSpan>(comparer) :=
      function(const left, right: TTimeSpan): Integer
      begin
        if left.Ticks < right.Ticks then
          Result := -1
        else if left.Ticks > right.Ticks then
          Result := 1
        else
          Result := 0;
      end
  else
    comparer := TComparer<TAbsolute>.Default;
  Create(scheduler, state, action, dueTime, comparer);
end;

function TScheduledItem<TAbsolute, TValue>.InvokeCore: IDisposable;
begin
  Result := fAction(fScheduler, fState);
end;

{$ENDREGION}


end.
