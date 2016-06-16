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

unit Spring.Reactive.Disposables.RefCountDisposable;

interface

uses
  Spring,
  Spring.Reactive;

type
  TRefCountDisposable = class(TInterfacedObject,
    IDisposable, ICancelable, IRefCountDisposable)
  private
    fRaiseWhenDisposed: Boolean;
    fDisposable: IDisposable;
    fIsPrimaryDisposed: Boolean;
    fCount: Integer;
    function GetIsDisposed: Boolean;
    procedure Release;

    type
      TInnerDisposable = class(TInterfacedObject, IDisposable)
      private
        fParent: TRefCountDisposable;
      public
        constructor Create(const parent: TRefCountDisposable);
        destructor Destroy; override;
        procedure Dispose;
      end;
  public
    constructor Create(const disposable: IDisposable; raiseWhenDisposed: Boolean = False);
    procedure Dispose;
    function GetDisposable: IDisposable;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TRefCountDisposable'}

constructor TRefCountDisposable.Create(const disposable: IDisposable;
  raiseWhenDisposed: Boolean);
begin
  inherited Create;
  fDisposable := disposable;
  fRaiseWhenDisposed := raiseWhenDisposed;
end;

procedure TRefCountDisposable.Dispose;
var
  disposable: IDisposable;
begin
  disposable := nil;
  MonitorEnter(Self);
  try
    if Assigned(fDisposable) then
    begin
      if not fIsPrimaryDisposed then
      begin
        fIsPrimaryDisposed := True;

        if fCount = 0 then
        begin
          disposable := fDisposable;
          fDisposable := nil;
        end;
      end;
    end;
  finally
    MonitorExit(Self);
  end;

  if Assigned(disposable) then
    disposable.Dispose;
end;

function TRefCountDisposable.GetDisposable: IDisposable;
begin
  MonitorEnter(Self);
  try
    if fDisposable = nil then
    begin
      if fRaiseWhenDisposed then
        raise EObjectDisposedException.Create('RefCountDisposable');
      Result := Disposable.Empty;
    end
    else
    begin
      Inc(fCount);
      Result := TInnerDisposable.Create(Self);
    end;
  finally
    MonitorExit(Self);
  end;
end;

function TRefCountDisposable.GetIsDisposed: Boolean;
begin
  Result := fDisposable = nil;
end;

procedure TRefCountDisposable.Release;
var
  disposable: IDisposable;
begin
  MonitorEnter(Self);
  try
    if Assigned(fDisposable) then
    begin
      Dec(fCount);
      Assert(fCount >= 0);

      if fIsPrimaryDisposed then
      begin
        if fCount = 0 then
        begin
          disposable := fDisposable;
          fDisposable := nil;
        end;
      end;
    end;
  finally
    MonitorExit(Self);
  end;

  if Assigned(disposable) then
    disposable.Dispose;
end;

{$ENDREGION}


{$REGION 'TRefCountDisposable.TInnerDisposable'}

constructor TRefCountDisposable.TInnerDisposable.Create(
  const parent: TRefCountDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
end;

destructor TRefCountDisposable.TInnerDisposable.Destroy;
var
  parent: TRefCountDisposable;
begin
  parent := AtomicExchange(Pointer(fParent), nil);
  if Assigned(parent) then
    parent._Release;
  inherited;
end;

procedure TRefCountDisposable.TInnerDisposable.Dispose;
var
  parent: TRefCountDisposable;
begin
  inherited;
  parent := AtomicExchange(Pointer(fParent), nil);
  if Assigned(parent) then
  begin
    parent.Release;
    parent._Release;
  end;
end;

{$ENDREGION}


end.
