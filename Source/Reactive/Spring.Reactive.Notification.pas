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

unit Spring.Reactive.Notification;

interface

uses
  Spring,
  Spring.Reactive;

type
  TNotificationKind = (
    OnNext,
    OnError,
    OnCompleted
  );

  INotification<T> = interface
    function GetValue: T;
    function GetHasValue: Boolean;
    function GetException: Exception;
    function GetNotificationKind: TNotificationKind;

    procedure Accept(const observer: IObserver<T>);

    property Value: T read GetValue;
    property HasValue: Boolean read GetHasValue;
    property Exception: Exception read GetException;
    property Kind: TNotificationKind read GetNotificationKind;
  end;

  TNotification<T> = class abstract
  private type
    TOnNextNotification = class(TInterfacedObject, IEquatable, INotification<T>)
    private
      fValue: T;
      function GetValue: T;
      function GetHasValue: Boolean;
      function GetException: Exception;
      function GetNotificationKind: TNotificationKind;
    public
      constructor Create(const value: T);
      function Equals(other: TObject): Boolean; override;
      procedure Accept(const observer: IObserver<T>);
    end;

    TOnErrorNotification = class(TInterfacedObject, IEquatable, INotification<T>)
    private
      fException: Exception;
      fFreeException: Boolean;
      function GetValue: T;
      function GetHasValue: Boolean;
      function GetException: Exception;
      function GetNotificationKind: TNotificationKind;
    public
      constructor Create(const exception: Exception; freeException: Boolean = False);
      destructor Destroy; override;
      function Equals(other: TObject): Boolean; override;
      procedure Accept(const observer: IObserver<T>);
    end;

    TOnCompletedNotification = class(TInterfacedObject, IEquatable, INotification<T>)
    private
      function GetValue: T;
      function GetHasValue: Boolean;
      function GetException: Exception;
      function GetNotificationKind: TNotificationKind;
    public
      function Equals(other: TObject): Boolean; override;
      procedure Accept(const observer: IObserver<T>);
    end;
  public
  end;

  TNotification = record
  public
    class function CreateOnNext<T>(const value: T): INotification<T>; static;
    class function CreateOnError<T>(const error: Exception; freeException: Boolean = False): INotification<T>; static;
    class function CreateOnCompleted<T>: INotification<T>; static;
  end;

implementation

uses
  Generics.Defaults;


{$REGION 'TNotification<T>.TOnNextNotification'}

constructor TNotification<T>.TOnNextNotification.Create(const value: T);
begin
  inherited Create;
  fValue := value;
end;

function TNotification<T>.TOnNextNotification.Equals(other: TObject): Boolean;
begin
  if Self = other then
    Exit(True);
  Result := (other is TOnNextNotification)
    and TEqualityComparer<T>.Default.Equals(fValue, TOnNextNotification(other).fValue);
end;

function TNotification<T>.TOnNextNotification.GetException: Exception;
begin
  Result := nil;
end;

function TNotification<T>.TOnNextNotification.GetHasValue: Boolean;
begin
  Result := True;
end;

function TNotification<T>.TOnNextNotification.GetNotificationKind: TNotificationKind;
begin
  Result := OnNext;
end;

function TNotification<T>.TOnNextNotification.GetValue: T;
begin
  Result := fValue;
end;

procedure TNotification<T>.TOnNextNotification.Accept(
  const observer: IObserver<T>);
begin
  Guard.CheckNotNull(observer, 'observer');
  observer.OnNext(fValue);
end;

{$ENDREGION}


{$REGION 'TNotification<T>.TOnErrorNotification'}

constructor TNotification<T>.TOnErrorNotification.Create(
  const exception: Exception; freeException: Boolean);
begin
  inherited Create;
  fException := exception;
  fFreeException := freeException;
end;

destructor TNotification<T>.TOnErrorNotification.Destroy;
begin
  if fFreeException then
    fException.Free;
  inherited;
end;

function TNotification<T>.TOnErrorNotification.Equals(other: TObject): Boolean;
begin
  if Self = other then
    Exit(True);
  Result := (other is TOnErrorNotification)
    and (fException = TOnErrorNotification(other).fException);
end;

function TNotification<T>.TOnErrorNotification.GetException: Exception;
begin
  Result := fException;
end;

function TNotification<T>.TOnErrorNotification.GetHasValue: Boolean;
begin
  Result := False;
end;

function TNotification<T>.TOnErrorNotification.GetNotificationKind: TNotificationKind;
begin
  Result := OnError;
end;

function TNotification<T>.TOnErrorNotification.GetValue: T;
begin
  fFreeException := False;
  raise fException;
end;

procedure TNotification<T>.TOnErrorNotification.Accept(
  const observer: IObserver<T>);
begin
  Guard.CheckNotNull(observer, 'observer');
  observer.OnError(fException);
end;

{$ENDREGION}


{$REGION 'TNotification<T>.TOnCompletedNotification'}

function TNotification<T>.TOnCompletedNotification.Equals(
  other: TObject): Boolean;
begin
  if Self = other then
    Exit(True);
  Result := other is TOnCompletedNotification;
end;

function TNotification<T>.TOnCompletedNotification.GetException: Exception;
begin
  Result := nil;
end;

function TNotification<T>.TOnCompletedNotification.GetHasValue: Boolean;
begin
  Result := False;
end;

function TNotification<T>.TOnCompletedNotification.GetNotificationKind: TNotificationKind;
begin
  Result := OnCompleted;
end;

function TNotification<T>.TOnCompletedNotification.GetValue: T;
begin
  raise EInvalidOperationException.Create('OnCompleted notification doesn''t have a value.');
end;

procedure TNotification<T>.TOnCompletedNotification.Accept(
  const observer: IObserver<T>);
begin
  Guard.CheckNotNull(observer, 'observer');
  observer.OnCompleted;
end;

{$ENDREGION}


{$REGION 'TNotification'}

class function TNotification.CreateOnNext<T>(const value: T): INotification<T>;
begin
  Result := TNotification<T>.TOnNextNotification.Create(value);
end;

class function TNotification.CreateOnError<T>(const error: Exception;
  freeException: Boolean): INotification<T>;
begin
  Result := TNotification<T>.TOnErrorNotification.Create(error, freeException);
end;

class function TNotification.CreateOnCompleted<T>: INotification<T>;
begin
  Result := TNotification<T>.TOnCompletedNotification.Create;
end;

{$ENDREGION}


end.
