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

unit Spring.Reactive.Testing.ReactiveTest;

interface

uses
  Spring.Testing,
  Spring.Reactive,
  Spring.Reactive.Notification,
  Spring.Reactive.Testing.Recorded;

type
  TReactiveTest = class(TTestCase)
  public
    const Created: Int64 = 100;
    const Subscribed: Int64 = 200;
    const Disposed: Int64 = 1000;
    class function OnNext<T>(const ticks: Int64; const value: T): TRecorded<INotification<T>>; overload; static;
    class function OnNext(const ticks: Int64; const value: Integer): TRecorded<INotification<Integer>>; overload; static; inline;
    class function OnCompleted<T>(const ticks: Int64): TRecorded<INotification<T>>; overload; static; inline;
    class function OnError<T>(const ticks: Int64; const error: Exception; freeException: Boolean = False): TRecorded<INotification<T>>; overload; static; inline;
  end;

implementation


{$REGION 'TReactiveTest'}

class function TReactiveTest.OnNext(const ticks: Int64;
  const value: Integer): TRecorded<INotification<Integer>>;
begin
  Result := OnNext<Integer>(ticks, value);
end;

class function TReactiveTest.OnNext<T>(const ticks: Int64;
  const value: T): TRecorded<INotification<T>>;
begin
  Result := TRecorded<INotification<T>>.Create(ticks, TNotification.CreateOnNext<T>(value));
end;

class function TReactiveTest.OnCompleted<T>(
  const ticks: Int64): TRecorded<INotification<T>>;
begin
  Result := TRecorded<INotification<T>>.Create(ticks, TNotification.CreateOnCompleted<T>);
end;

class function TReactiveTest.OnError<T>(const ticks: Int64;
  const error: Exception; freeException: Boolean): TRecorded<INotification<T>>;
begin
  Result := TRecorded<INotification<T>>.Create(ticks, TNotification.CreateOnError<T>(error, freeException));
end;

{$ENDREGION}


end.
