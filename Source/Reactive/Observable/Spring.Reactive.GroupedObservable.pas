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

unit Spring.Reactive.GroupedObservable;

interface

uses
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TGroupedObservable<TKey, TElement> = class(TObservableBase<TElement>, IGroupedObservable<TKey, TElement>)
  private
    fKey: TKey;
    fSubject: IObservable<TElement>;
    fRefCount: IRefCountDisposable;
    function GetKey: TKey;
  protected
    function Subscribe(const observer: IObserver<TElement>): IDisposable; override;
  public
    constructor Create(const key: TKey; const subject: ISubject<TElement>;
      const refCount: IRefCountDisposable); overload;
    constructor Create(const key: TKey; const subject: ISubject<TElement>); overload;
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TGroupedObservable<TKey, TElement>'}

constructor TGroupedObservable<TKey, TElement>.Create(const key: TKey;
  const subject: ISubject<TElement>; const refCount: IRefCountDisposable);
begin
  inherited Create;
  fKey := key;
  fSubject := subject;
  fRefCount := refCount;
end;

constructor TGroupedObservable<TKey, TElement>.Create(const key: TKey;
  const subject: ISubject<TElement>);
begin
  inherited Create;
  fKey := key;
  fSubject := subject;
end;

function TGroupedObservable<TKey, TElement>.GetKey: TKey;
begin
  Result := fKey;
end;

function TGroupedObservable<TKey, TElement>.Subscribe(
  const observer: IObserver<TElement>): IDisposable;
var
  release: IDisposable;
  subscription: IDisposable;
begin
  if Assigned(fRefCount) then
  begin
    release := fRefCount.GetDisposable;
    subscription := fSubject.Subscribe(observer);
    Result := TStableCompositeDisposable.Create(release, subscription);
  end
  else
    Result := fSubject.Subscribe(observer);
end;

{$ENDREGION}


end.
