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

unit Spring.Collections.Sequences;

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Coroutines;

type
  TSequence = class(TCoroutine, IEnumerator)
  strict private
    fCurrent: TValue;
  protected
    function GetCurrent: TValue;
    procedure Yield(const value: TValue); override;
  public
    constructor Create(const action: Action);
  end;

  TSequence<TResult> = class(TEnumerableBase<TResult>, IInterface, IEnumerable<TResult>)
  private
    fAction: Action;
    type
      TEnumerator = class(TCoroutine<TResult>, IEnumerator, IEnumerator<TResult>)
      protected
        function GetCurrent: TValue; overload;
      end;
  public
    constructor Create(const action: Action);
    function GetEnumerator: IEnumerator<TResult>;
  end;

  TSequence<T1, TResult> = class(TSequence<TResult>,
    Func<T1, IEnumerable<TResult>>)
  private
    fArg1: Nullable<T1>;
    function Func<T1, IEnumerable<TResult>>.Invoke = Bind;
  public
    constructor Create(const action: Action<T1>);
    function Bind(const arg1: T1): IEnumerable<TResult>;
  end;

  TSequence<T1, T2, TResult> = class(TSequence<TResult>,
    Func<T1, T2, IEnumerable<TResult>>)
  private
    fArg1: Nullable<T1>;
    fArg2: Nullable<T2>;
    function Func<T1, T2, IEnumerable<TResult>>.Invoke = Bind;
  public
    constructor Create(const action: Action<T1, T2>);
    function Bind(const arg1: T1; const arg2: T2): IEnumerable<TResult>;
  end;

  TSequence<T1, T2, T3, TResult> = class(TSequence<TResult>,
    Func<T1, T2, T3, IEnumerable<TResult>>)
  private
    fArg1: Nullable<T1>;
    fArg2: Nullable<T2>;
    fArg3: Nullable<T3>;
    function Func<T1, T2, T3, IEnumerable<TResult>>.Invoke = Bind;
  public
    constructor Create(const action: Action<T1, T2, T3>);
    function Bind(const arg1: T1; const arg2: T2; const arg3: T3): IEnumerable<TResult>;
  end;

  TSequence<T1, T2, T3, T4, TResult> = class(TSequence<TResult>,
    Func<T1, T2, T3, T4, IEnumerable<TResult>>)
  private
    fArg1: Nullable<T1>;
    fArg2: Nullable<T2>;
    fArg3: Nullable<T3>;
    fArg4: Nullable<T4>;
    function Func<T1, T2, T3, T4, IEnumerable<TResult>>.Invoke = Bind;
  public
    constructor Create(const action: Action<T1, T2, T3, T4>);
    function Bind(const arg1: T1; const arg2: T2; const arg3: T3; const arg4: T4): IEnumerable<TResult>;
  end;

implementation


{$REGION 'TSequence'}

constructor TSequence.Create(const action: Action);
begin
  inherited Create(action);
  IsLooping := False;
end;

function TSequence.GetCurrent: TValue;
begin
  Result := fCurrent;
end;

procedure TSequence.Yield(const value: TValue);
begin
  fCurrent := value;
  inherited Yield;
end;

{$ENDREGION}


{$REGION 'TSequence<TResult>'}

constructor TSequence<TResult>.Create(const action: Action);
begin
  inherited Create;
  fAction := action;
end;

function TSequence<TResult>.GetEnumerator: IEnumerator<TResult>;
var
  enumerator: TEnumerator;
begin
  enumerator := TEnumerator.Create(fAction);
  enumerator.IsLooping := False;
  Result := enumerator;
end;

{$ENDREGION}


{$REGION 'TSequence<TResult>.TEnumerator'}

function TSequence<TResult>.TEnumerator.GetCurrent: TValue;
begin
  Result := TValue.From<TResult>(Current);
end;

{$ENDREGION}


{$REGION 'TSequence<T1, TResult>'}

constructor TSequence<T1, TResult>.Create(const action: Action<T1>);
begin
  inherited Create(
    procedure
    begin
      if not fArg1.HasValue then
        raise ECoroutineException.Create('arg1 not bound');
      action(fArg1);
    end);
end;

function TSequence<T1, TResult>.Bind(const arg1: T1): IEnumerable<TResult>;
begin
  fArg1 := arg1;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSequence<T1, T2, TResult>'}

constructor TSequence<T1, T2, TResult>.Create(const action: Action<T1, T2>);
begin
  inherited Create(
    procedure
    begin
      if not fArg1.HasValue then
        raise ECoroutineException.Create('arg1 not bound');
      if not fArg2.HasValue then
        raise ECoroutineException.Create('arg2 not bound');
      action(fArg1, fArg2);
    end);
end;

function TSequence<T1, T2, TResult>.Bind(const arg1: T1;
  const arg2: T2): IEnumerable<TResult>;
begin
  fArg1 := arg1;
  fArg2 := arg2;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSequence<T1, T2, T3, TResult>'}

constructor TSequence<T1, T2, T3, TResult>.Create(
  const action: Action<T1, T2, T3>);
begin
  inherited Create(
    procedure
    begin
      if not fArg1.HasValue then
        raise ECoroutineException.Create('arg1 not bound');
      if not fArg2.HasValue then
        raise ECoroutineException.Create('arg2 not bound');
      if not fArg3.HasValue then
        raise ECoroutineException.Create('arg3 not bound');
      action(fArg1, fArg2, fArg3);
    end);
end;

function TSequence<T1, T2, T3, TResult>.Bind(const arg1: T1; const arg2: T2;
  const arg3: T3): IEnumerable<TResult>;
begin
  fArg1 := arg1;
  fArg2 := arg2;
  fArg3 := arg3;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSequence<T1, T2, T3, T4, TResult>'}

constructor TSequence<T1, T2, T3, T4, TResult>.Create(
  const action: Action<T1, T2, T3, T4>);
begin
  inherited Create(
    procedure
    begin
      if not fArg1.HasValue then
        raise ECoroutineException.Create('arg1 not bound');
      if not fArg2.HasValue then
        raise ECoroutineException.Create('arg2 not bound');
      if not fArg3.HasValue then
        raise ECoroutineException.Create('arg3 not bound');
      if not fArg4.HasValue then
        raise ECoroutineException.Create('arg4 not bound');
      action(fArg1, fArg2, fArg3, fArg4);
    end);
end;

function TSequence<T1, T2, T3, T4, TResult>.Bind(const arg1: T1;
  const arg2: T2; const arg3: T3; const arg4: T4): IEnumerable<TResult>;
begin
  fArg1 := arg1;
  fArg2 := arg2;
  fArg3 := arg3;
  fArg4 := arg4;
  Result := Self;
end;

{$ENDREGION}


end.
