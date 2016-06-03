unit Spring.Reactive.Observers;

interface

uses
  SysUtils,
  Spring,
  Spring.Reactive;

type
  TNopObserver<T> = class(TDisposableObject, IObserver<T>)
  private
    class var fInstance: IObserver<T>;
  public
    class constructor Create;
    class destructor Destroy;

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;

    class property Instance: IObserver<T> read fInstance;
  end;

  TDoneObserver<T> = class(TDisposableObject, IObserver<T>)
  private
    fError: Exception;
    class var fCompleted: IObserver<T>;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const error: Exception = nil);

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;

    class property Completed: IObserver<T> read fCompleted;
    property Error: Exception read fError write fError;
  end;

  TDisposedObserver<T> = class(TDisposableObject, IObserver<T>)
  private
    class var fInstance: IObserver<T>;
  public
    class constructor Create;
    class destructor Destroy;

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;

    class property Instance: IObserver<T> read fInstance;
  end;

  TObserver<T> = class(TDisposableObject, IObserver<T>)
  private
    fObservers: TArray<IObserver<T>>; // immutable
  public
    constructor Create(const observers: array of IObserver<T>);

    procedure OnNext(const value: T);
    procedure OnError(const error: Exception);
    procedure OnCompleted;

    function Add(const observer: IObserver<T>): IObserver<T>;
    function Remove(const observer: IObserver<T>): IObserver<T>;
  end;

  EObjectDisposedException = class(EInvalidOperationException); // TODO: move to Spring.pas

implementation


{$REGION 'TNopObserver<T>'}

class constructor TNopObserver<T>.Create;
begin
  fInstance := TNopObserver<T>.Create;
end;

class destructor TNopObserver<T>.Destroy;
begin
  fInstance := nil;
end;

procedure TNopObserver<T>.OnCompleted;
begin
end;

procedure TNopObserver<T>.OnError(const error: Exception);
begin
end;

procedure TNopObserver<T>.OnNext(const value: T);
begin
end;

{$ENDREGION}


{$REGION 'TDoneObserver<T>'}

class constructor TDoneObserver<T>.Create;
begin
  fCompleted := TDoneObserver<T>.Create;
end;

class destructor TDoneObserver<T>.Destroy;
begin
  fCompleted := nil;
end;

constructor TDoneObserver<T>.Create(const error: Exception);
begin
  inherited Create;
  fError := error;
end;

procedure TDoneObserver<T>.OnCompleted;
begin
end;

procedure TDoneObserver<T>.OnError(const error: Exception);
begin
end;

procedure TDoneObserver<T>.OnNext(const value: T);
begin
end;

{$ENDREGION}


{$REGION 'TDisposedObserver<T>'}

class constructor TDisposedObserver<T>.Create;
begin
  fInstance := TDisposedObserver<T>.Create;
end;

class destructor TDisposedObserver<T>.Destroy;
begin
  fInstance := nil;
end;

procedure TDisposedObserver<T>.OnCompleted;
begin
  raise EObjectDisposedException.Create('');
end;

procedure TDisposedObserver<T>.OnError(const error: Exception);
begin
  raise EObjectDisposedException.Create('');
end;

procedure TDisposedObserver<T>.OnNext(const value: T);
begin
  raise EObjectDisposedException.Create('');
end;

{$ENDREGION}


{$REGION 'TObserver<T>'}

constructor TObserver<T>.Create(const observers: array of IObserver<T>);
begin
  inherited Create;
  fObservers := TArray.Copy<IObserver<T>>(observers);
end;

procedure TObserver<T>.OnCompleted;
var
  observer: IObserver<T>;
begin
  for observer in fObservers do
    observer.OnCompleted;
end;

procedure TObserver<T>.OnError(const error: Exception);
var
  observer: IObserver<T>;
begin
  for observer in fObservers do
    observer.OnError(error);
end;

procedure TObserver<T>.OnNext(const value: T);
var
  observer: IObserver<T>;
begin
  for observer in fObservers do
    observer.OnNext(value);
end;

function TObserver<T>.Add(const observer: IObserver<T>): IObserver<T>;
var
  observers: TArray<IObserver<T>>;
  n: Integer;
begin
  observers := fObservers;
  n := Length(observers);
  SetLength(observers, n + 1);
  observers[n] := observer;
  Result := TObserver<T>.Create(observers);
end;

function TObserver<T>.Remove(const observer: IObserver<T>): IObserver<T>;
var
  i: Integer;
begin
  i := TArray.IndexOf<IObserver<T>>(fObservers, observer);
  if i < 0 then
    Exit(Self);

  if Length(fObservers) = 2 then
    Result := fObservers[1 - i]
  else
    Result := TObserver<T>.Create(TArray.Concat<IObserver<T>>([Copy(fObservers, 0, i), Copy(fObservers, i + 1)]));
//    Result := TObserver<T>.Create(Copy(fObservers, 0, i) + Copy(fObservers, i + 1));
end;

{$ENDREGION}


end.
