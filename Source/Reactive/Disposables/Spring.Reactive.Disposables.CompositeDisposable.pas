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

unit Spring.Reactive.Disposables.CompositeDisposable;

interface

uses
  Spring.Collections,
  Spring.Reactive;

type
  TCompositeDisposable = class(TInterfacedObject,
    IDisposable, ICancelable, ICompositeDisposable)
  private
    fGate: TObject;
    fDisposed: Boolean;
    fDisposables: IList<IDisposable>;
    function GetIsDisposed: Boolean;
    function GetCount: Integer;
  public
    constructor Create(const disposables: array of IDisposable);
    destructor Destroy; override;
    procedure Dispose;
    procedure Add(const item: IDisposable);
    function Remove(const item: IDisposable): Boolean;
  end;

implementation


{$REGION 'TCompositeDisposable'}

constructor TCompositeDisposable.Create(
  const disposables: array of IDisposable);
begin
  inherited Create;
  fGate := TObject.Create;
  fDisposables := TCollections.CreateInterfaceList<IDisposable>(disposables);
end;

procedure TCompositeDisposable.Add(const item: IDisposable);
var
  shouldDispose: Boolean;
begin
  MonitorEnter(fGate);
  try
    shouldDispose := fDisposed;
    if not fDisposed then
    begin
      fDisposables.Add(item);
//      Inc(fCount);
    end;
  finally
    MonitorExit(fGate);
  end;
  if shouldDispose then
    item.Dispose;
end;

destructor TCompositeDisposable.Destroy;
begin
//  Dispose;
  fGate.Free;
  inherited;
end;

procedure TCompositeDisposable.Dispose;
var
  currentDisposables: TArray<IDisposable>;
  d: IDisposable;
begin
  MonitorEnter(fGate);
  try
    if not fDisposed then
    begin
      fDisposed := True;
      currentDisposables := fDisposables.ToArray;
      fDisposables.Clear;
    end;
  finally
    MonitorExit(fGate);
  end;

  for d in currentDisposables do
    d.Dispose;
end;

function TCompositeDisposable.GetCount: Integer;
begin
  Result := fDisposables.Count;
end;

function TCompositeDisposable.GetIsDisposed: Boolean;
begin
  Result := fDisposed;
end;

function TCompositeDisposable.Remove(const item: IDisposable): Boolean;
var
  shouldDispose: Boolean;
  i: Integer;
begin
  shouldDispose := False;
  MonitorEnter(fGate);
  try
    if not fDisposed then
    begin
      i := fDisposables.IndexOf(item);
      if i >= 0 then
      begin
        shouldDispose := True;
        fDisposables.Delete(i);
//        Dec(fCount);
      end;
    end;
  finally
    MonitorExit(fGate);
  end;
  if shouldDispose then
    item.Dispose;
  Result := shouldDispose;
end;

{$ENDREGION}


end.
