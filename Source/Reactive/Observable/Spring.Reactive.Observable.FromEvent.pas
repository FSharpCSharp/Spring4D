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

unit Spring.Reactive.Observable.FromEvent;

interface

uses
  Spring,
  Spring.Reactive,
  Spring.Reactive.Internal.Producer;

type
  TEventProducer<TArgs> = class(TProducer<TArgs>)
  private type
    TSession = class(TDisposableObject)
    private
      fParent: TEventProducer<TArgs>;
//      fSubject: ISubject<TArgs>;
//      fCount: Integer;
    public
      constructor Create(const parent: TEventProducer<TArgs>);
      destructor Destroy; override;
      procedure Dispose; override;
    end;
  private
    fScheduler: IScheduler;
    fEventType: PTypeInfo;
//    fSession: TSession;

    function CreateDelegate(const onNext: Action<TArgs>): TMethod;
  protected
    function GetHandler(const onNext: Action<TArgs>): TMethod;
  public
    constructor Create(const scheduler: IScheduler; eventType: PTypeInfo);
  end;

implementation

uses
  Rtti;


{$REGION 'TEventProducer<TArgs>'}

constructor TEventProducer<TArgs>.Create(
  const scheduler: IScheduler; eventType: PTypeInfo);
begin
  inherited Create;
  fScheduler := scheduler;
  fEventType := eventType
end;

function TEventProducer<TArgs>.CreateDelegate(const onNext: Action<TArgs>): TMethod;
var
  fields: TArray<TRttiField>;
  conversion: Func<TArray<TValue>,TArgs>;
begin
{$IFDEF DELPHIXE2_UP}
  fields := TType.GetType(TypeInfo(TArgs)).GetFields;
  conversion :=
    function(const args: TArray<TValue>): TArgs
    var
      i: Integer;
    begin
      for i := 0 to High(fields) do
        fields[i].SetValue(@Result, Args[i+1]);
    end;

  PMethod(@Result)^ := (TType.GetType(fEventType) as TRttiInvokableType).CreateImplementation(nil,
    procedure(UserData: Pointer; const Args: TArray<TValue>; out Result: TValue)
    begin
      onNext(conversion(Args));
    end).AsMethod;
{$ENDIF}
end;

function TEventProducer<TArgs>.GetHandler(
  const onNext: Action<TArgs>): TMethod;
begin
  Result := CreateDelegate(onNext);
end;

{$ENDREGION}


{$REGION 'TEventProducer<TArgs>.TSession'}

constructor TEventProducer<TArgs>.TSession.Create(
  const parent: TEventProducer<TArgs>);
begin
  inherited Create;
  fParent := parent;
  fParent._AddRef;
end;

destructor TEventProducer<TArgs>.TSession.Destroy;
begin
  fParent._Release;
  inherited;
end;

procedure TEventProducer<TArgs>.TSession.Dispose;
begin

end;

{$ENDREGION}


end.
