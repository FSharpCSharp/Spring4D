{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
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

unit Spring.DesignPatterns;

{$I Spring.inc}

interface

uses
  Classes,
  Contnrs,
  Windows,
  SysUtils,
  SyncObjs,
  TypInfo,
  Generics.Collections,
  Spring.System;

type
  {$REGION 'Singleton Pattern'}

  /// <summary>
  /// Provides a portal to get the single instance of a concrete class which
  /// must have a default constructor. It also keeps track of the lifetime
  /// of the instances and will free them in reversed order.
  /// </summary>
  /// <remarks>
  /// This class just demonstrates how to apply the classical Singleton Pattern.
  /// It's recommended to use the Spring IoC container which is more flexible.
  /// </remarks>
  TSingleton = record
  strict private
    class var
      fMappings: TDictionary<TClass, TObject>;
      fInstances: TObjectList;
      fCriticalSection: TCriticalSection;
    class constructor Create;
    {$HINTS OFF}
    class destructor Destroy;
    {$HINTS ON}
  public
    class function GetInstance<T: class, constructor>: T; static;
  end;

  {$ENDREGION}


  {$REGION 'Observer Pattern'}

  /// <summary>
  /// Represents an observable subject.
  /// </summary>
  IObservable<T> = interface
    procedure AddListener(const listener: T);
    procedure RemoveListener(const listener: T);
    procedure NotifyListeners(callback: TProc<T>);
  end;

  TListenerNotification = (
    lnAdded,
    lnRemoved
  );

  /// <summary>
  /// TObservable<T>
  /// </summary>
  TObservable<T> = class(TInterfacedObject, IObservable<T>, IInterface)
  private
    fListeners: TList<T>;
  protected
    procedure Validate(const listener: T); virtual;
    procedure DoListenerAdded(const listener: T); virtual;
    procedure DoListenerRemoved(const listener: T); virtual;
    procedure Notify(const listener: T; action: TListenerNotification); virtual;
    function Contains(const listener: T): Boolean; virtual;
    function GetListeners: TList<T>; virtual;
    property Listeners: TList<T> read GetListeners;
  public
    destructor Destroy; override;
    procedure AddListener(const listener: T); virtual;
    procedure RemoveListener(const listener: T); virtual;
    procedure NotifyListeners(callback: TProc<T>); virtual;
  end;

  /// <summary>
  /// TSynchronizedObservable<T>
  /// </summary>
  TSynchronizedObservable<T> = class(TObservable<T>, IObservable<T>, IInterface)
  protected
    fListenersLock: IReadWriteSync;
    function Contains(const listener: T): Boolean; override;
  public
    constructor Create;
    procedure AddListener(const listener: T); override;
    procedure RemoveListener(const listener: T); override;
    procedure NotifyListeners(callback: TProc<T>); override;
  end;

//  TAsyncObservable<T> = class(TInterfacedObject, IObservable<T>, IInterface)
//
//  end;

  {$ENDREGION}


  {$REGION 'Specification Pattern (Experimental)'}

  /// <summary>
  /// ISpecification<T>
  /// </summary>
  /// <remarks>
  /// Consider: how to work with ORM?
  /// </remarks>
  ISpecification<T> = interface
    function IsSatisfiedBy(const obj: T): Boolean;
  end;

  /// <summary>
  /// Provides the easy-going specification holder with operator overloads.
  /// </summary>
  TSpecification<T> = record
  private
    fSpecification: ISpecification<T>;
  public
    function IsSatisfiedBy(const obj: T): Boolean;
    class operator Implicit(const specification: ISpecification<T>): TSpecification<T>;
    class operator Implicit(const specification: TSpecification<T>): ISpecification<T>;
    class operator Implicit(const specification: TSpecification<T>): TPredicate<T>;
    class operator Explicit(const specification: ISpecification<T>): TSpecification<T>;
    class operator Explicit(const specification: TSpecification<T>): ISpecification<T>;
    class operator LogicalAnd(const left, right: TSpecification<T>): TSpecification<T>;
    class operator LogicalOr(const left, right: TSpecification<T>): TSpecification<T>;
    class operator LogicalNot(const value:TSpecification<T>) : TSpecification<T>;
  end;

  /// <summary>
  /// Provides the abstract base class for Specification
  /// </summary>
  TSpecificationBase<T> = class abstract(TInterfacedObject, ISpecification<T>, TPredicate<T>, IInterface)
  protected
    { TPredicate<T> }
    function Invoke(const arg: T): Boolean; virtual;
  public
    function IsSatisfiedBy(const obj: T): Boolean; virtual; abstract;
  end;

  TUnarySpecification<T> = class abstract(TSpecificationBase<T>)
  protected
    fSpecification: ISpecification<T>;
  public
    constructor Create(const specification: ISpecification<T>);
  end;

  TLogicalNotSpecification<T> = class sealed(TUnarySpecification<T>)
  public
    function IsSatisfiedBy(const obj: T): Boolean; override;
  end;

  TBinarySpecification<T> = class abstract(TSpecificationBase<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TLogicalAndSpecification<T> = class sealed(TBinarySpecification<T>)
  public
    function IsSatisfiedBy(const obj: T): Boolean; override;
  end;

  TLogicalOrSpecification<T> = class sealed(TBinarySpecification<T>)
  public
    function IsSatisfiedBy(const obj: T): Boolean; override;
  end;

  {$ENDREGION}

implementation

uses
  Spring.ResourceStrings;


{$REGION 'TSingleton'}

class constructor TSingleton.Create;
begin
  fMappings := TDictionary<TClass, TObject>.Create(4);
  fInstances := TObjectList.Create(True);
  fCriticalSection := TCriticalSection.Create;
end;

class destructor TSingleton.Destroy;
begin
  fCriticalSection.Free;
  fInstances.Free;
  fMappings.Free;
end;

class function TSingleton.GetInstance<T>: T;
begin
  fCriticalSection.Enter;
  try
    if not fMappings.TryGetValue(T, TObject(Result)) then
    begin
      Result := T.Create;
      fMappings.Add(T, Result);
      fInstances.Add(Result);
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

{$ENDREGION}


{$REGION 'TObservable<T>'}

destructor TObservable<T>.Destroy;
begin
  fListeners.Free;
  inherited Destroy;
end;

function TObservable<T>.Contains(const listener: T): Boolean;
begin
  Result := Listeners.Contains(listener);
end;

procedure TObservable<T>.Validate(const listener: T);
begin
end;

procedure TObservable<T>.DoListenerAdded(const listener: T);
begin
end;

procedure TObservable<T>.DoListenerRemoved(const listener: T);
begin
end;

procedure TObservable<T>.Notify(const listener: T;
  action: TListenerNotification);
begin
  case action of
    lnAdded:
    begin
      Listeners.Add(listener);
      DoListenerAdded(listener);
    end;
    lnRemoved:
    begin
      Listeners.Remove(listener);
      DoListenerRemoved(listener);
    end;
  end;
end;

procedure TObservable<T>.AddListener(const listener: T);
begin
  Validate(listener);
  if not Contains(listener) then
  begin
    Notify(listener, lnAdded);
  end;
end;

procedure TObservable<T>.RemoveListener(const listener: T);
begin
  Validate(listener);
  if Contains(listener) then
  begin
    Notify(listener, lnRemoved);
  end;
end;

procedure TObservable<T>.NotifyListeners(callback: TProc<T>);
var
  listener: T;
begin
  TArgument.CheckNotNull(Assigned(callback), 'callback');
  for listener in Listeners do
  begin
    callback(listener);
  end;
end;

function TObservable<T>.GetListeners: TList<T>;
begin
  if fListeners = nil then
  begin
    fListeners := TList<T>.Create;
  end;
  Result := fListeners;
end;

{$ENDREGION}


{$REGION 'TSynchronizedObservable<T>'}

constructor TSynchronizedObservable<T>.Create;
begin
  inherited Create;
  fListenersLock := TMREWSync.Create;
  fListeners := TList<T>.Create;
end;

procedure TSynchronizedObservable<T>.AddListener(const listener: T);
begin
  fListenersLock.BeginWrite;
  try
    inherited AddListener(listener);
  finally
    fListenersLock.EndWrite;
  end;
end;

procedure TSynchronizedObservable<T>.RemoveListener(const listener: T);
begin
  fListenersLock.BeginWrite;
  try
    inherited RemoveListener(listener);
  finally
    fListenersLock.EndWrite;
  end;
end;

procedure TSynchronizedObservable<T>.NotifyListeners(callback: TProc<T>);
begin
  fListenersLock.BeginRead;
  try
    inherited NotifyListeners(callback);
  finally
    fListenersLock.EndRead;
  end;
end;

function TSynchronizedObservable<T>.Contains(const listener: T): Boolean;
begin
  fListenersLock.BeginRead;
  try
    Result := inherited Contains(listener);
  finally
    fListenersLock.EndRead;
  end;
end;

{$ENDREGION}


{$REGION 'TSpecificationBase<T>'}

function TSpecificationBase<T>.Invoke(const arg: T): Boolean;
begin
  Result := IsSatisfiedBy(arg);
end;

{$ENDREGION}


{$REGION 'TSpecification<T>'}

function TSpecification<T>.IsSatisfiedBy(const obj: T): Boolean;
begin
  Result := (fSpecification <> nil) and fSpecification.IsSatisfiedBy(obj)
end;

class operator TSpecification<T>.Implicit(
  const specification: ISpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := specification;
end;

class operator TSpecification<T>.Implicit(
  const specification: TSpecification<T>): ISpecification<T>;
begin
  Result := specification.fSpecification;
end;

class operator TSpecification<T>.Implicit(
  const specification: TSpecification<T>): TPredicate<T>;
var
  internalSpecification: ISpecification<T>;
begin
  internalSpecification := specification;
  if internalSpecification is TSpecificationBase<T> then
  begin
    Result := TSpecificationBase<T>(internalSpecification);
  end
  else if internalSpecification <> nil then
  begin
    Result :=
      function(const arg: T): Boolean
      begin
        Result := internalSpecification.IsSatisfiedBy(arg);
      end;
  end
  else
  begin
    Result := nil;
  end;
end;

class operator TSpecification<T>.Explicit(
  const specification: ISpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := specification;
end;

class operator TSpecification<T>.Explicit(
  const specification: TSpecification<T>): ISpecification<T>;
begin
  Result := specification.fSpecification;
end;

class operator TSpecification<T>.LogicalAnd(const left,
  right: TSpecification<T>): TSpecification<T>;
var
  specification: ISpecification<T>;
begin
  specification := TLogicalAndSpecification<T>.Create(ISpecification<T>(left),
    ISpecification<T>(right));
  Result := TSpecification<T>(specification);
end;

class operator TSpecification<T>.LogicalOr(const left,
  right: TSpecification<T>): TSpecification<T>;
var
  specification: ISpecification<T>;
begin
  specification := TLogicalOrSpecification<T>.Create(ISpecification<T>(left),
    ISpecification<T>(right));
  Result := TSpecification<T>(specification);
end;

class operator TSpecification<T>.LogicalNot(
  const value: TSpecification<T>): TSpecification<T>;
var
  specification: ISpecification<T>;
begin
  specification := TLogicalNotSpecification<T>.Create(ISpecification<T>(value));
  Result := TSpecification<T>(specification);
end;

{$ENDREGION}


{$REGION 'Logical Specifications'}

{ TUnarySpecification<T> }

constructor TUnarySpecification<T>.Create(
  const specification: ISpecification<T>);
begin
  inherited Create;
  fSpecification := specification;
end;

{ TBinarySpecification<T> }

constructor TBinarySpecification<T>.Create(const left,
  right: ISpecification<T>);
begin
  inherited Create;
  fLeft := left;
  fRight := right;
end;

{ TLogicalAndSpecification<T> }

function TLogicalAndSpecification<T>.IsSatisfiedBy(const obj: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(obj) and fRight.IsSatisfiedBy(obj);
end;

{ TLogicalOrSpecification<T> }

function TLogicalOrSpecification<T>.IsSatisfiedBy(const obj: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(obj) or fRight.IsSatisfiedBy(obj);
end;

{ TLogicalNotSpecification<T> }

function TLogicalNotSpecification<T>.IsSatisfiedBy(const obj: T): Boolean;
begin
  Result := not fSpecification.IsSatisfiedBy(obj);
end;

{$ENDREGION}

end.
