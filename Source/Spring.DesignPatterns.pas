{***************************************************************************}
{                                                                           }
{               Delphi Spring Framework                                     }
{                                                                           }
{               Copyright (C) 2008-2009 Zuo Baoquan                         }
{                                                                           }
{               http://www.zuobaoquan.com (Simplified Chinese)              }
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

unit Spring.DesignPatterns experimental;

{$I Spring.inc}

{ TODO: Command Pattern with Undo/Redo }
{ TODO: Memento Pattern }

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
  {$REGION 'Singleton Pattern'}

  /// <summary>
  /// Provides a simple, fast and thread-safe Singleton Pattern implementation.
  /// </summary>
  /// <description>
  /// Singleton Pattern is defined as:
  /// Ensure a class only has one instance, and provide a global point of access to it.
  /// </description>
  /// <remarks>
  /// 1. Use Instance class property to get the singleton instance.
  /// 2. Concrete Singleton Classes may override DoCreate/DoDestroy if necessary.
  /// 3. Do not call Create/Free methods, otherwise an EInvalidOp exception will be raised.
  /// </remarks>
  /// <example>
  /// <code>
  ///   TApplicationContext = class(TSingleton<TApplicationContext>)
  ///   protected
  ///     procedure DoCreate; override;
  ///     procedure DoDestroy; override;
  ///   end;
  /// </code>
  /// </example>
  TSingleton<T: class> = class(TInterfaceBase)
  strict private
    class var fInstance: T;
    class destructor Destroy;
    class procedure FreeSingleton(var obj: T); static;
    class function CreateSingleton: T; static;
    class function GetInstance: T; static;
  protected
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    class property Instance: T read GetInstance;
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

  TObservable<T> = class(TInterfaceBase, IObservable<T>, IInterface)
  strict private
    fObservers: TList<T>;
    function GetObservers: TList<T>;
  protected
    procedure Validate(const observer: T); virtual;
    procedure DoObserverAdded(const observer: T); virtual;
    procedure DoObserverRemoved(const observer: T); virtual;
    procedure Notify(const observer: T; action: TCollectionNotification); virtual;
    property Observers: TList<T> read GetObservers;
  public
    destructor Destroy; override;
    procedure AddObserver(const observer: T);
    procedure RemoveObserver(const observer: T);
    procedure NotifyObservers(callback: TProc<T>); virtual;
  end;

  {$ENDREGION}


  {$REGION 'Command Pattern (NOT READY)'}

  (*

  ICommand = interface
    procedure Execute;
  end;

  IUndoable = interface
    procedure Undo;
    procedure Redo;
  end;

  IUndoRedoManager = interface

  end;

  TCommand = class
  public
    procedure Execute; virtual; abstract;
  end;

  TCommandManager = class
  private
  public
//    procedure AddCommand(const command: ICommand);
//    procedure ExecuteCommand(const command: ICommand);
//    procedure Undo;
//    procedure Redo;
  end;

  //*)

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

  ISpecification<T> = interface
    function IsSatisfiedBy(const obj: T): Boolean;
  end;

  /// <summary>
  /// Provides the easy-going specification holder with operator overloading.
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


  {$REGION 'Registry Pattern (Experimental)'}

  /// <summary>
  /// Repsents type-handler mapping registry
  /// </summary>
  TRegistry<TType, THandler> = class(TSingleton<TRegistry<TType, THandler>>)
  strict private
    fOwnsType: Boolean;
    fOwnsHandler: Boolean;
    function GetTypes: TEnumerable<TType>;
  protected
    fDictionary: TDictionary<TType, THandler>;
    property Dictionary: TDictionary<TType, THandler> read fDictionary;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure DoKeyNotify(sender: TObject; const item: TType; action: TCollectionNotification);
    procedure DoValueNotify(sender: TObject; const item: THandler; action: TCollectionNotification);
  public
    function Contains(const &type: TType): Boolean; virtual;
    function TryGetHandler(const &type: TType; out handler: THandler): Boolean; virtual;
    function GetHandler(const &type: TType): THandler; virtual;
    procedure &Register(const &type: TType; const handler: THandler); virtual;
    procedure Unregister(const &type: TType); overload; virtual;
    procedure Unregister(const handler: THandler); overload; virtual;
    procedure UnregisterAll;
    property Types: TEnumerable<TType> read GetTypes;
    property OwnsType: Boolean read fOwnsType write fOwnsType;
    property OwnsHandler: Boolean read fOwnsHandler write fOwnsHandler;
  end;

  { TClassRegistry, match most specific class type }
  TClassRegistry<TClassType, THandler> = class(TRegistry<TClassType, THandler>)
  public
    function Contains(const &type: TClassType): Boolean; override;
    function TryGetHandler(const &type: TClassType; out handler: THandler): Boolean; override;
  end;

  {$ENDREGION}


implementation

uses
  Spring.ResourceStrings;

{$REGION 'TSingleton<T>'}

class destructor TSingleton<T>.Destroy;
begin
  FreeSingleton(fInstance);
end;

class procedure TSingleton<T>.FreeSingleton(var obj: T);
begin
  if obj <> nil then
  begin
    TSingleton<T>(obj).DoDestroy;
    TSingleton<T>(obj).FreeInstance;
    obj := nil;
  end;
end;

class function TSingleton<T>.CreateSingleton: T;
begin
  Result := T(T.NewInstance);
  TSingleton<T>(Result).DoCreate;
end;

class function TSingleton<T>.GetInstance: T;
var
  obj: T;
begin
  if fInstance = nil then
  begin
    obj := CreateSingleton;
    if InterlockedCompareExchangePointer(PPointer(@fInstance)^, PPointer(@obj)^, nil) <> nil then
    begin
      FreeSingleton(obj);
    end;
  end;
  Result := fInstance;
end;

constructor TSingleton<T>.Create;
begin
  raise EInvalidOperation.Create(SInvalidOperation_SingletonCreate);
end;

destructor TSingleton<T>.Destroy;
begin
  if ExceptObject = nil then
    raise EInvalidOperation.Create(SInvalidOperation_SingletonDestroy);
end;

procedure TSingleton<T>.DoCreate;
begin
end;

procedure TSingleton<T>.DoDestroy;
begin
end;


{$ENDREGION}


{$REGION 'TObservable<T>'}

destructor TObservable<T>.Destroy;
begin
  fObservers.Free;
  inherited Destroy;
end;

procedure TObservable<T>.Validate(const observer: T);
var
  p: Pointer;
begin
  p := PPointer(@observer)^;
  TArgument.CheckNotNull(p, 'observer');
end;

procedure TObservable<T>.DoObserverAdded(const observer: T);
begin
end;

procedure TObservable<T>.DoObserverRemoved(const observer: T);
begin
end;

procedure TObservable<T>.Notify(const observer: T;
  action: TCollectionNotification);
begin
  case action of
    cnAdded:
    begin
      Observers.Add(observer);
      DoObserverAdded(observer);
    end;
    cnRemoved:
    begin
      Observers.Remove(observer);
      DoObserverRemoved(observer);
    end;
  end;
end;

procedure TObservable<T>.AddObserver(const observer: T);
begin
  Validate(observer);
  if not Observers.Contains(observer) then
    Notify(observer, cnAdded);
end;

procedure TObservable<T>.RemoveObserver(const observer: T);
begin
  Validate(observer);
  if Observers.Contains(observer) then
    Notify(observer, cnRemoved);
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


{$REGION 'TRegistry<TType, THandler>'}

{$IFDEF SUPPORTS_GENERICS}

procedure TRegistry<TType, THandler>.DoCreate;
begin
  inherited DoCreate;
  fDictionary := TDictionary<TType, THandler>.Create;
  fDictionary.OnKeyNotify := DoKeyNotify;
  fDictionary.OnValueNotify := DoValueNotify;
  fOwnsType := TRtti.GetTypeKind<TType> = tkClass;
  fOwnsHandler := TRtti.GetTypeKind<THandler> = tkClass;
end;

procedure TRegistry<TType, THandler>.DoDestroy;
begin
  fDictionary.Free;
  inherited DoDestroy;
end;

procedure TRegistry<TType, THandler>.DoKeyNotify(sender: TObject; const item: TType; action: TCollectionNotification);
begin
  if (action = cnRemoved) and fOwnsType then
  begin
    TRtti.CheckTypeKind<TType>(tkClass);
    PObject(@item)^.Free;
  end;
end;

procedure TRegistry<TType, THandler>.DoValueNotify(sender: TObject; const item: THandler; action: TCollectionNotification);
begin
  if (action = cnRemoved) and fOwnsHandler then
  begin
    TRtti.CheckTypeKind<THandler>(tkClass);
    PObject(@item)^.Free;
  end;
end;

function TRegistry<TType, THandler>.GetTypes: TEnumerable<TType>;
begin
  Result := fDictionary.Keys;
end;

function TRegistry<TType, THandler>.Contains(const &type: TType): Boolean;
begin
  Result := fDictionary.ContainsKey(&type);
end;

function TRegistry<TType, THandler>.TryGetHandler(const &type: TType;
  out handler: THandler): Boolean;
begin
  Result := fDictionary.TryGetValue(&type, handler);
end;

function TRegistry<TType, THandler>.GetHandler(const &type: TType): THandler;
var
  typeKind: TypInfo.TTypeKind;
  name: string;
begin
  typeKind := TRtti.GetTypeKind<TType>;
  case typeKind of
    tkEnumeration:
      name := TEnum.GetName<TType>(&type);
    tkClass:
      name := PObject(@&type)^.ClassName;
    tkClassRef:
      name := TClass(&type).ClassName;
    else
    begin
      name := TRtti.GetTypeName<TType>;
    end;
  end;
  if not TryGetHandler(&type, Result) then
    raise EArgumentException.CreateFmt(STypeNotRegistered, [name]);
end;

procedure TRegistry<TType, THandler>.&Register(const &type: TType;
  const handler: THandler);
begin
  fDictionary.AddOrSetValue(&type, handler);
end;

procedure TRegistry<TType, THandler>.Unregister(const &type: TType);
begin
  fDictionary.Remove(&type);
end;

procedure TRegistry<TType, THandler>.Unregister(const handler: THandler);
var
  i: Integer;
begin
  for i := 0 to fDictionary.Values.Count - 1 do
  begin
//    fDictionary.RemoveValues(handler);
  end;
end;

procedure TRegistry<TType, THandler>.UnregisterAll;
begin
  fDictionary.Clear;
end;

{$ENDIF ~SUPPORTS_GENERICS}

{$ENDREGION}


{$REGION 'TClassRegistry<TType, THandler>'}

function TClassRegistry<TClassType, THandler>.Contains(const &type: TClassType): Boolean;
var
  classType: TClass;
begin
  TRtti.CheckTypeKind<TClassType>(tkClassRef);
  Result := False;
  classType := TClass(&type);
  while not Result and (classType <> nil) do
  begin
    Result := inherited Contains(TClassType(classType));
    classType := classType.ClassParent;
  end;
end;

function TClassRegistry<TClassType, THandler>.TryGetHandler(const &type: TClassType;
  out handler: THandler): Boolean;
var
  classType: TClass;
begin
  TRtti.CheckTypeKind<TClassType>(tkClassRef);
  Result := False;
  classType := TClass(&type);
  while not Result and (classType <> nil) do
  begin
    Result := inherited TryGetHandler(TClassType(classType), handler);
    classType := classType.ClassParent;
  end;
end;

{$ENDREGION}


end.
