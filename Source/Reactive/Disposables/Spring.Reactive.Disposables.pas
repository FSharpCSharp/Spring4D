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

unit Spring.Reactive.Disposables;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Disposables.AnonymousDisposable,
  Spring.Reactive.Disposables.BooleanDisposable,
  Spring.Reactive.Disposables.CompositeDisposable,
  Spring.Reactive.Disposables.DefaultDisposable,
  Spring.Reactive.Disposables.RefCountDisposable,
  Spring.Reactive.Disposables.ScheduledDisposable,
  Spring.Reactive.Disposables.SerialDisposable,
  Spring.Reactive.Disposables.SingleAssignmentDisposable,
  Spring.Reactive.Disposables.StableCompositeDisposable;

type
  TAnonymousDisposable = Spring.Reactive.Disposables.AnonymousDisposable.TAnonymousDisposable;
  TBooleanDisposable = Spring.Reactive.Disposables.BooleanDisposable.TBooleanDisposable;
  TCompositeDisposable = Spring.Reactive.Disposables.CompositeDisposable.TCompositeDisposable;
  TDefaultDisposable = Spring.Reactive.Disposables.DefaultDisposable.TDefaultDisposable;
  TRefCountDisposable = Spring.Reactive.Disposables.RefCountDisposable.TRefCountDisposable;
  TScheduledDisposable = Spring.Reactive.Disposables.ScheduledDisposable.TScheduledDisposable;
  TSerialDisposable = Spring.Reactive.Disposables.SerialDisposable.TSerialDisposable;
  TSingleAssignmentDisposable = Spring.Reactive.Disposables.SingleAssignmentDisposable.TSingleAssignmentDisposable;
  TStableCompositeDisposable = Spring.Reactive.Disposables.StableCompositeDisposable.TStableCompositeDisposable;

  Disposable = record
  strict private
    class function GetEmpty: IDisposable; static;
  public
    class function Create(const dispose: Action): IDisposable; overload; static;
    class function Create(const dispose: IDisposable): IDisposable; overload; static;
    class property Empty: IDisposable read GetEmpty;
  end;

implementation


{$REGION 'Disposable'}

class function Disposable.Create(const dispose: Action): IDisposable;
begin
  Result := TAnonymousDisposable.Create(dispose);
end;

class function Disposable.Create(const dispose: IDisposable): IDisposable;
begin
  Result := TAnonymousDisposable.Create(
    procedure
    begin
      dispose.Dispose;
    end);
end;

class function Disposable.GetEmpty: IDisposable;
begin
  Result := TDefaultDisposable.Instance;
end;

{$ENDREGION}


end.
