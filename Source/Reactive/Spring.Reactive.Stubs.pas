unit Spring.Reactive.Stubs;

interface

uses
  SysUtils,
  Spring,
  Spring.Reactive;

type
  Stubs = record
  strict private class var
    fNop: Action;
    fThrow: Action<Exception>;
  public
    class constructor Create;
    class destructor Destroy;
    class property Nop: Action read fNop;
    class property Throw: Action<Exception> read fThrow;
  end;

  Stubs<T> = record
  strict private class var
    fIgnore: Action<T>;
  public
    class constructor Create;
    class destructor Destroy;
    class property Ignore: Action<T> read fIgnore;
  end;

implementation


{$REGION 'Stubs'}

class constructor Stubs.Create;
begin
  fNop := procedure begin end;
  fThrow := procedure(const e: Exception) begin raise e; end;
end;

class destructor Stubs.Destroy;
begin
  fNop := nil;
  fThrow := nil;
end;

{$ENDREGION}


{$REGION 'Stubs<T>'}

class constructor Stubs<T>.Create;
begin
  fIgnore := procedure(const _: T) begin end;
end;

class destructor Stubs<T>.Destroy;
begin
  fIgnore := nil;
end;

{$ENDREGION}


end.
