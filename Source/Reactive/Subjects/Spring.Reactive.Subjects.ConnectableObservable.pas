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

unit Spring.Reactive.Subjects.ConnectableObservable;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.ObservableBase;

type
  TConnectableObservable<TSource, TResult> = class(TObservableBase<TResult>,
    IConnectableObservable<TResult>)
  private type
    TConnection = class(TInterfacedObject, IDisposable)
    private
      fParent: TConnectableObservable<TSource, TResult>;
      fSubscription: IDisposable;
    public
      constructor Create(const parent: TConnectableObservable<TSource, TResult>;
        const subscription: IDisposable);
      destructor Destroy; override;
      procedure Dispose;
    end;
  private
    fSubject: ISubject<TSource, TResult>;
    fSource: IObservable<TSource>;
    fConnection: TConnection;
  public
    constructor Create(const source: IObservable<TSource>; const subject: ISubject<TSource, TResult>);
    function Connect: IDisposable;
    function Subscribe(const observer: IObserver<TResult>): IDisposable; override;
  end;

implementation


{$REGION 'TConnectableObservable<TSource, TResult>'}

constructor TConnectableObservable<TSource, TResult>.Create(
  const source: IObservable<TSource>;
  const subject: ISubject<TSource, TResult>);
begin
  inherited Create;
  fSubject := subject;
  fSource := source;//.AsObservable;
end;

function TConnectableObservable<TSource, TResult>.Connect: IDisposable;
var
  subscription: IDisposable;
begin
  Lock(Self);
  if fConnection = nil then
  begin
    subscription := fSource.Subscribe(fSubject as IObserver<TSource>);
    fConnection := TConnection.Create(Self, subscription);
  end;
  Result := fConnection;
end;

function TConnectableObservable<TSource, TResult>.Subscribe(
  const observer: IObserver<TResult>): IDisposable;
begin
  Guard.CheckNotNull(observer, 'observer');

  Result := fSubject.Subscribe(observer);
end;

{$ENDREGION}


{$REGION 'TConnectableObservable<TSource, TResult>.TConnection'}

constructor TConnectableObservable<TSource, TResult>.TConnection.Create(
  const parent: TConnectableObservable<TSource, TResult>;
  const subscription: IDisposable);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fSubscription := subscription;
end;

destructor TConnectableObservable<TSource, TResult>.TConnection.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TConnectableObservable<TSource, TResult>.TConnection.Dispose;
var
  parent: IInterface;
begin
  parent := fParent; // ensure parent to live until method ends
  Lock(fParent);
  if Assigned(fSubscription) then
  begin
    fSubscription.Dispose;
    fSubscription := nil;
    fParent.fConnection := nil;
  end;
end;

{$ENDREGION}


end.
