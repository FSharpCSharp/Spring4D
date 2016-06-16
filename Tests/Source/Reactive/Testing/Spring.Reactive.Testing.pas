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

unit Spring.Reactive.Testing;

interface

uses
  Spring.Collections,
  Spring.Reactive,
  Spring.Reactive.Notification,
  Spring.Reactive.Testing.Recorded,
  Spring.Reactive.Testing.Subscription;

type
  ITestableObservable<T> = interface(IObservable<T>)
    function GetSubscriptions: IList<TSubscription>;
    function GetMessages: IList<TRecorded<INotification<T>>>;
    property Subscriptions: IList<TSubscription> read GetSubscriptions;
    property Messages: IList<TRecorded<INotification<T>>> read GetMessages;
  end;

  ITestableObserver<T> = interface(IObserver<T>)
    function GetMessages: IList<TRecorded<INotification<T>>>;
    property Messages: IList<TRecorded<INotification<T>>> read GetMessages;
  end;

implementation

end.
