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

unit Spring.Reactive.Disposables.StableCompositeDisposable;

interface

uses
  Spring,
  Spring.Reactive;

type
  TStableCompositeDisposable = class(TInterfacedObject, IDisposable, ICancelable)
  protected
    function GetIsDisposed: Boolean; virtual; abstract;
  public
    class function Create(const disposable1, disposable2: IDisposable): ICancelable; overload; static;
    class function Create(const disposables: array of IDisposable): ICancelable; overload; static;
    procedure Dispose; virtual; abstract;
  end;

implementation

uses
  Spring.Collections;

type
  TBinary = class(TStableCompositeDisposable)
  private
    fDisposable1: IDisposable;
    fDisposable2: IDisposable;
  protected
    function GetIsDisposed: Boolean; override;
  public
    constructor Create(const disposable1, disposable2: IDisposable);
    destructor Destroy; override;
    procedure Dispose; override;
  end;

  TNAry = class(TStableCompositeDisposable)
  private
    fDisposables: IList<IDisposable>;
  protected
    function GetIsDisposed: Boolean; override;
  public
    constructor Create(const disposables: array of IDisposable);
    procedure Dispose; override;
  end;


{$REGION 'TStableCompositeDisposable'}

class function TStableCompositeDisposable.Create(const disposable1,
  disposable2: IDisposable): ICancelable;
begin
  Guard.CheckNotNull(disposable1, 'disposable1');
  Guard.CheckNotNull(disposable2, 'disposable2');

  Result := TBinary.Create(disposable1, disposable2);
end;

class function TStableCompositeDisposable.Create(
  const disposables: array of IDisposable): ICancelable;
begin
  Result := TNAry.Create(disposables);
end;

{$ENDREGION}


{$REGION 'TBinary'}

constructor TBinary.Create(const disposable1, disposable2: IDisposable);
begin
  inherited Create;
  fDisposable1 := disposable1;
  fDisposable2 := disposable2;
end;

destructor TBinary.Destroy;
begin
  fDisposable1 := nil;
  fDisposable2 := nil;
  inherited;
end;

procedure TBinary.Dispose;
var
  old1, old2: IDisposable;
begin
  old1 := TInterlocked.Exchange<IDisposable>(fDisposable1, nil);
  if Assigned(old1) then
    old1.Dispose;
  old2 := TInterlocked.Exchange<IDisposable>(fDisposable2, nil);
  if Assigned(old2) then
    old2.Dispose;
end;

function TBinary.GetIsDisposed: Boolean;
begin
  Result := Assigned(fDisposable1);
end;

{$ENDREGION}


{$REGION 'TNAry'}

constructor TNAry.Create(const disposables: array of IDisposable);
begin
  inherited Create;
  fDisposables := TCollections.CreateInterfaceList<IDisposable>(disposables);
end;

procedure TNAry.Dispose;
var
  old: IList<IDisposable>;
  d: IDisposable;
begin
  old := TInterlocked.Exchange<IList<IDisposable>>(fDisposables, nil);
  if Assigned(old) then
    for d in old do
      d.Dispose;
end;

function TNAry.GetIsDisposed: Boolean;
begin
  Result := fDisposables = nil;
end;

{$ENDREGION}


end.
