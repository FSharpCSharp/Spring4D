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

unit Spring.Reactive.Observable.Amb;

interface

uses
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TAmb<TSource> = class(TProducer<TSource>)
  private
    fLeft: IObservable<TSource>;
    fRight: IObservable<TSource>;

    type
      TSink = class(TSink<TSource>)
      private type TAmbState = (Left, Right, Neither);
      private
        fChoice: TAmbState;

        type
          TAmbObserver = class(TInterfacedObject, IObserver<TSource>)
          private
            fTarget: IObserver<TSource>;
            fDisposable: IDisposable;
          public
            procedure Dispose;
            procedure OnNext(const value: TSource);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;

          TDecisionObserver = class(TInterfacedObject, IObserver<TSource>)
          private
            fParent: TSink;
            fme: TAmbState;
            fSubscription: IDisposable;
            fOtherSubscription: IDisposable;
            fObserver: TAmbObserver;
          public
            constructor Create(const parent: TSink; const me: TAmbState;
              const subscription, otherSubscription: IDisposable;
              const observer: TAmbObserver);
            destructor Destroy; override;
            procedure Dispose;
            procedure OnNext(const value: TSource);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
          end;
      public
        function Run(const parent: TAmb<TSource>): IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>; const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const left, right: IObservable<TSource>);
  end;

implementation

uses
  Spring.Reactive.Disposables;


{$REGION 'TAmb<TSource>'}

constructor TAmb<TSource>.Create(const left, right: IObservable<TSource>);
begin
  inherited Create;
  fLeft := left;
  fRight := right;
end;

function TAmb<TSource>.CreateSink(const observer: IObserver<TSource>;
  const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(observer, cancel);
end;

function TAmb<TSource>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run(Self);
end;

{$ENDREGION}


{$REGION 'TAmb<TSource>.TSink'}

function TAmb<TSource>.TSink.Run(const parent: TAmb<TSource>): IDisposable;
var
  ls, rs: ISingleAssignmentDisposable;
  d: IDisposable;
  lo, ro: TAmbObserver;
begin
  ls := TSingleAssignmentDisposable.Create;
  rs := TSingleAssignmentDisposable.Create;
  d := TStableCompositeDisposable.Create(ls, rs);

  lo := TAmbObserver.Create;
  lo.fDisposable := d;
  lo.fTarget := TDecisionObserver.Create(Self, TAmbState.Left, ls, rs, lo);

  ro := TAmbObserver.Create;
  ro.fDisposable := d;
  ro.fTarget := TDecisionObserver.Create(Self, TAmbState.Right, rs, ls, ro);

  fChoice := TAmbState.Neither;

  ls.Disposable := parent.fLeft.Subscribe(lo);
  rs.Disposable := parent.fRight.Subscribe(ro);

  Result := d;
end;

{$ENDREGION}


{$REGION 'TAmb<TSource>.TSink.TAmbObserver'}

procedure TAmb<TSource>.TSink.TAmbObserver.Dispose;
begin
end;

procedure TAmb<TSource>.TSink.TAmbObserver.OnNext(const value: TSource);
begin
  fTarget.OnNext(value);
end;

procedure TAmb<TSource>.TSink.TAmbObserver.OnError(const error: Exception);
begin
  fTarget.OnError(error);
  fDisposable.Dispose;
end;

procedure TAmb<TSource>.TSink.TAmbObserver.OnCompleted;
begin
  fTarget.OnCompleted;
  fDisposable.Dispose;
end;

{$ENDREGION}


{$REGION 'TAmb<TSource>.TSink.TDecisionObserver'}

constructor TAmb<TSource>.TSink.TDecisionObserver.Create(const parent: TSink;
  const me: TAmbState; const subscription, otherSubscription: IDisposable;
  const observer: TAmbObserver);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fme := me;
  fSubscription := subscription;
  fOtherSubscription := otherSubscription;
  fObserver := observer;
end;

destructor TAmb<TSource>.TSink.TDecisionObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TAmb<TSource>.TSink.TDecisionObserver.Dispose;
begin
end;

procedure TAmb<TSource>.TSink.TDecisionObserver.OnNext(const value: TSource);
begin
  Lock(fParent);

  if fParent.fChoice = TAmbState.Neither then
  begin
    fParent.fChoice := fme;
    fOtherSubscription.Dispose;
    fObserver.fDisposable := fSubscription;
    fObserver.fTarget := fParent.Observer;
  end;

  if fParent.fChoice = fme then
    fParent.Observer.OnNext(value);
end;

procedure TAmb<TSource>.TSink.TDecisionObserver.OnError(const error: Exception);
begin
  Lock(fParent);

  if fParent.fChoice = TAmbState.Neither then
  begin
    fParent.fChoice := fme;
    fOtherSubscription.Dispose;
    fObserver.fDisposable := fSubscription;
    fObserver.fTarget := fParent.Observer;
  end;

  if fParent.fChoice = fme then
  begin
    fParent.Observer.OnError(error);
    fParent.Dispose;
  end;
end;

procedure TAmb<TSource>.TSink.TDecisionObserver.OnCompleted;
begin
  Lock(fParent);

  if fParent.fChoice = TAmbState.Neither then
  begin
    fParent.fChoice := fme;
    fOtherSubscription.Dispose;
    fObserver.fDisposable := fSubscription;
    fObserver.fTarget := fParent.Observer;
  end;

  if fParent.fChoice = fme then
  begin
    fParent.Observer.OnCompleted;
    fParent.Dispose;
  end;
end;

{$ENDREGION}


end.
