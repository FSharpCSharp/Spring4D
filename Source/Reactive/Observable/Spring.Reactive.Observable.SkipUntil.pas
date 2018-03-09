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

unit Spring.Reactive.Observable.SkipUntil;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer,
  Spring.Reactive.Internal.Sink;

type
  TSkipUntil<TSource, TOther> = class(TProducer<TSource>)
  private
    fSource: IObservable<TSource>;
    fOther: IObservable<TOther>;

    type
      TSink = class(TSink<TSource>)
      private
        fParent: TSkipUntil<TSource, TOther>;

        type
          TSourceObserver = class(TInterfacedObject, IDisposable, IObserver<TSource>)
          private
            fParent: TSink;
            fObserver: IObserver<TSource>;
            fSubscription: ISingleAssignmentDisposable;
            procedure SetDisposable(const Value: IDisposable);
          public
            constructor Create(const parent: TSink);
            destructor Destroy; override;
            procedure Dispose;
            procedure OnNext(const value: TSource);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
            property Disposable: IDisposable write SetDisposable;
          end;

          TOtherObserver = class(TInterfacedObject, IDisposable, IObserver<TOther>)
          private
            fParent: TSink;
            fSourceObserver: TSourceObserver;
            fSubscription: ISingleAssignmentDisposable;
            procedure SetDisposable(const Value: IDisposable);
          public
            constructor Create(const parent: TSink; const sourceObserver: TSourceObserver);
            destructor Destroy; override;
            procedure Dispose;
            procedure OnNext(const value: TOther);
            procedure OnError(const error: Exception);
            procedure OnCompleted;
            property Disposable: IDisposable write SetDisposable;
          end;
      public
        constructor Create(const parent: TSkipUntil<TSource, TOther>;
          const observer: IObserver<TSource>; const cancel: IDisposable);
        destructor Destroy; override;
        function Run: IDisposable;
      end;
  protected
    function CreateSink(const observer: IObserver<TSource>;
      const cancel: IDisposable): TObject; override;
    function Run(const sink: TObject): IDisposable; override;
  public
    constructor Create(const source: IObservable<TSource>;
      const other: IObservable<TOther>);
  end;

implementation

uses
  Spring.Reactive.Disposables,
  Spring.Reactive.Internal.Observers;


{$REGION 'TSkipUntil<TSource, TOther>'}

constructor TSkipUntil<TSource, TOther>.Create(
  const source: IObservable<TSource>; const other: IObservable<TOther>);
begin
  inherited Create;
  fSource := source;
  fOther := other;
end;

function TSkipUntil<TSource, TOther>.CreateSink(
  const observer: IObserver<TSource>; const cancel: IDisposable): TObject;
begin
  Result := TSink.Create(Self, observer, cancel);
end;

function TSkipUntil<TSource, TOther>.Run(const sink: TObject): IDisposable;
begin
  Result := TSink(sink).Run;
end;

{$ENDREGION}


{$REGION 'TSkipUntil<TSource, TOther>.TSink'}

constructor TSkipUntil<TSource, TOther>.TSink.Create(
  const parent: TSkipUntil<TSource, TOther>; const observer: IObserver<TSource>;
  const cancel: IDisposable);
begin
  inherited Create(observer, cancel);
  fParent := parent;
  fParent._AddRef;
end;

destructor TSkipUntil<TSource, TOther>.TSink.Destroy;
begin
  fParent._Release;
  inherited;
end;

function TSkipUntil<TSource, TOther>.TSink.Run: IDisposable;
var
  sourceObserver: TSourceObserver;
  otherObserver: TOtherObserver;
  sourceSubscription: IDisposable;
  otherSubscription: IDisposable;
begin
  sourceObserver := TSourceObserver.Create(Self);
  otherObserver := TOtherObserver.Create(Self, sourceObserver);

  sourceSubscription := fParent.fSource.Subscribe(sourceObserver);
  otherSubscription := fParent.fOther.Subscribe(otherObserver);

  sourceObserver.Disposable := sourceSubscription;
  otherObserver.Disposable := otherSubscription;

  Result := TStableCompositeDisposable.Create(sourceSubscription, otherSubscription);
end;

{$ENDREGION}


{$REGION 'TSkipUntil<TSource, TOther>.TSink.TSourceObserver'}

constructor TSkipUntil<TSource, TOther>.TSink.TSourceObserver.Create(
  const parent: TSink);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fObserver := TNopObserver<TSource>.Instance;
  fSubscription := TSingleAssignmentDisposable.Create;
end;

destructor TSkipUntil<TSource, TOther>.TSink.TSourceObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TSourceObserver.Dispose;
begin
end;

procedure TSkipUntil<TSource, TOther>.TSink.TSourceObserver.OnCompleted;
begin
  fObserver.OnCompleted;
  fSubscription.Dispose;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TSourceObserver.OnError(
  const error: Exception);
begin
  fParent.Observer.OnError(error);
  fParent.Dispose;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TSourceObserver.OnNext(
  const value: TSource);
begin
  fObserver.OnNext(value);
end;

procedure TSkipUntil<TSource, TOther>.TSink.TSourceObserver.SetDisposable(
  const value: IDisposable);
begin
  fSubscription.Disposable := value;
end;

{$ENDREGION}


{$REGION 'TSkipUntil<TSource, TOther>.TSink.TOtherObserver'}

constructor TSkipUntil<TSource, TOther>.TSink.TOtherObserver.Create(
  const parent: TSink; const sourceObserver: TSourceObserver);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
  fSourceObserver := sourceObserver;
  fSubscription := TSingleAssignmentDisposable.Create;
end;

destructor TSkipUntil<TSource, TOther>.TSink.TOtherObserver.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TOtherObserver.Dispose;
begin
end;

procedure TSkipUntil<TSource, TOther>.TSink.TOtherObserver.OnCompleted;
begin
  fSubscription.Dispose;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TOtherObserver.OnError(
  const error: Exception);
begin
  fParent.Observer.OnError(error);
  fParent.Dispose;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TOtherObserver.OnNext(
  const value: TOther);
begin
  fSourceObserver.fObserver := fParent.Observer;
  fSubscription.Dispose;
end;

procedure TSkipUntil<TSource, TOther>.TSink.TOtherObserver.SetDisposable(
  const value: IDisposable);
begin
  fSubscription.Disposable := value;
end;

{$ENDREGION}


end.
