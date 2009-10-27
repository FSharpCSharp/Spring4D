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

{ TODO: Design Command Pattern with Undo/Redo }
{ TODO: Redesign Memento Pattern }

interface

uses
  Classes,
  Contnrs,
  Windows,
  SysUtils,
  SyncObjs,
  TypInfo,
  Generics.Defaults,
  Generics.Collections,
  Spring.System;

type
  {$REGION 'TSingleton'}

  /// <summary>
  /// Provides a portal to get the single instance of a class. It also keeps track
  /// of the lifetime of the instances and will free them in reversed order.
  /// </summary>
  TSingleton = record
  strict private
    class var
      fMappings: TDictionary<TClass, TObject>;
      fInstances: TObjectList;
      fCriticalSection: TCriticalSection;
    class constructor Create;
    class destructor Destroy;
  public
    class function GetInstance<T: class, constructor>: T; static;
  end;

  {$ENDREGION}


  {$REGION 'Observer Pattern'}

  /// <summary>
  /// Represents an observable subject.
  /// </summary>
  IObservable<T> = interface
    procedure AddObserver(const observer: T);
    procedure RemoveObserver(const observer: T);
    procedure NotifyObservers(callback: TProc<T>);
  end;

  TObserverNotification = (
    onAdded,
    onRemoved
  );

  /// <summary>
  /// TObservable<T>
  /// </summary>
  TObservable<T> = class(TInterfacedObject, IObservable<T>, IInterface)
  private
    fObservers: TList<T>;
  protected
    procedure Validate(const observer: T); virtual;
    procedure DoObserverAdded(const observer: T); virtual;
    procedure DoObserverRemoved(const observer: T); virtual;
    procedure Notify(const observer: T; action: TObserverNotification); virtual;
    function Contains(const observer: T): Boolean; virtual;
    function GetObservers: TList<T>; virtual;
    property Observers: TList<T> read GetObservers;
  public
    destructor Destroy; override;
    procedure AddObserver(const observer: T); virtual;
    procedure RemoveObserver(const observer: T); virtual;
    procedure NotifyObservers(callback: TProc<T>); virtual;
  end;

  /// <summary>
  /// TSynchronizedObservable<T>
  /// </summary>
  /// <remarks>
  /// </remarks>
  TSynchronizedObservable<T> = class(TObservable<T>, IObservable<T>, IInterface)
  protected
    fObserversLock: IReadWriteSync;
    function Contains(const observer: T): Boolean; override;
  public
    constructor Create;
    procedure AddObserver(const observer: T); override;
    procedure RemoveObserver(const observer: T); override;
    procedure NotifyObservers(callback: TProc<T>); override;
  end;

  {$ENDREGION}


  {$REGION 'Memento or Snapshot Pattern (Experimental)'}

  /// <summary>
  /// IRestorable<T>
  /// </summary>
  IRestorable<T> = interface
    function CreateSnapshot: T;
    procedure Restore(const snapshot: T);
  end;

  /// <summary>
  /// ISnapshot
  /// </summary>
  ISnapshot = interface
  end;

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
    class operator Explicit(const specification: ISpecification<T>): TSpecification<T>;
    class operator Explicit(const specification: TSpecification<T>): ISpecification<T>;
    class operator LogicalAnd(const left, right: TSpecification<T>): TSpecification<T>;
    class operator LogicalOr(const left, right: TSpecification<T>): TSpecification<T>;
    class operator LogicalNot(const specification:TSpecification<T>) : TSpecification<T>;
  end;

  /// <summary>
  /// Provides the abstract base class for Specification
  /// </summary>
  TSpecificationBase<T> = class abstract(TInterfacedObject, ISpecification<T>)
  public
    function IsSatisfiedBy(const obj: T): Boolean; virtual; abstract;
  end;

  TBinarySpecification<T> = class abstract(TSpecificationBase<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TUnarySpecification<T> = class abstract(TSpecificationBase<T>)
  protected
    fSpecification: ISpecification<T>;
  public
    constructor Create(const specification: ISpecification<T>);
  end;

  TLogicalAndSpecification<T> = class sealed(TBinarySpecification<T>)
  public
    function IsSatisfiedBy(const obj: T): Boolean; override;
  end;

  TLogicalOrSpecification<T> = class sealed(TBinarySpecification<T>)
  public
    function IsSatisfiedBy(const obj: T): Boolean; override;
  end;

  TLogicalNotSpecification<T> = class sealed(TUnarySpecification<T>)
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
  fObservers.Free;
  inherited Destroy;
end;

function TObservable<T>.Contains(const observer: T): Boolean;
begin
  Result := Observers.Contains(observer);
end;

procedure TObservable<T>.Validate(const observer: T);
begin
  TArgument.CheckNotNull<T>(observer, 'observer');
end;

procedure TObservable<T>.DoObserverAdded(const observer: T);
begin
end;

procedure TObservable<T>.DoObserverRemoved(const observer: T);
begin
end;

procedure TObservable<T>.Notify(const observer: T;
  action: TObserverNotification);
begin
  case action of
    onAdded:
    begin
      Observers.Add(observer);
      DoObserverAdded(observer);
    end;
    onRemoved:
    begin
      Observers.Remove(observer);
      DoObserverRemoved(observer);
    end;
  end;
end;

procedure TObservable<T>.AddObserver(const observer: T);
begin
  Validate(observer);
  if not Contains(observer) then
    Notify(observer, onAdded);
end;

procedure TObservable<T>.RemoveObserver(const observer: T);
begin
  Validate(observer);
  if Contains(observer) then
    Notify(observer, onRemoved);
end;

procedure TObservable<T>.NotifyObservers(callback: TProc<T>);
var
  observer: T;
begin
  for observer in Observers do
  begin
    callback(observer);
  end;
end;

function TObservable<T>.GetObservers: TList<T>;
begin
  if fObservers = nil then
  begin
    fObservers := TList<T>.Create;
  end;
  Result := fObservers;
end;

{$ENDREGION}


{$REGION 'TSynchronizedObservable<T>'}

constructor TSynchronizedObservable<T>.Create;
begin
  inherited Create;
  fObserversLock := TMREWSync.Create;
  fObservers := TList<T>.Create;
end;

procedure TSynchronizedObservable<T>.AddObserver(const observer: T);
begin
  fObserversLock.BeginWrite;
  try
    inherited AddObserver(observer);
  finally
    fObserversLock.EndWrite;
  end;
end;

procedure TSynchronizedObservable<T>.RemoveObserver(const observer: T);
begin
  fObserversLock.BeginWrite;
  try
    inherited RemoveObserver(observer);
  finally
    fObserversLock.EndWrite;
  end;
end;

procedure TSynchronizedObservable<T>.NotifyObservers(callback: TProc<T>);
begin
  fObserversLock.BeginRead;
  try
    inherited NotifyObservers(callback);
  finally
    fObserversLock.EndRead;
  end;
end;

function TSynchronizedObservable<T>.Contains(const observer: T): Boolean;
begin
  fObserversLock.BeginRead;
  try
    Result := inherited Contains(observer);
  finally
    fObserversLock.EndRead;
  end;
end;

{$ENDREGION}


{$REGION 'Logical Specifications'}

{ TBinarySpecification<T> }

constructor TBinarySpecification<T>.Create(const left,
  right: ISpecification<T>);
begin
  inherited Create;
  fLeft := left;
  fRight := right;
end;

{ TUnarySpecification<T> }

constructor TUnarySpecification<T>.Create(
  const specification: ISpecification<T>);
begin
  inherited Create;
  fSpecification := specification;
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


{$REGION 'TSpecification<T>'}

function TSpecification<T>.IsSatisfiedBy(const obj: T): Boolean;
begin
  Result := fSpecification.IsSatisfiedBy(obj)
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
  specification := TLogicalAndSpecification<T>.Create(ISpecification<T>(left), ISpecification<T>(right));
  Result := TSpecification<T>(specification);
end;

class operator TSpecification<T>.LogicalOr(const left,
  right: TSpecification<T>): TSpecification<T>;
var
  specification: ISpecification<T>;
begin
  specification := TLogicalOrSpecification<T>.Create(ISpecification<T>(left), ISpecification<T>(right));
  Result := TSpecification<T>(specification);
end;

class operator TSpecification<T>.LogicalNot(
  const specification: TSpecification<T>): TSpecification<T>;
var
  spec: ISpecification<T>;
begin
  spec := TLogicalNotSpecification<T>.Create(ISpecification<T>(spec));
  Result := TSpecification<T>(spec);
end;

{$ENDREGION}

end.
