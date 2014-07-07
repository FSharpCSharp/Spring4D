unit StorageInterceptor;

interface

uses
  Spring,
  Spring.Interception,
  Interfaces;

type
  TStorageInterceptor = class(TInterfacedObject, IInterceptor)
  private
    fSecondaryStorage: IStorage;
    procedure ChangeToSecondaryStorage(const invocation: IInvocation);
  public
    constructor Create(const secondaryStorage: IStorage);
    procedure Intercept(const invocation: IInvocation);
  end;

implementation

uses
  Storage;

{ TStorageInterceptor }

procedure TStorageInterceptor.ChangeToSecondaryStorage(
  const invocation: IInvocation);
var
  changeProxyTarget: IChangeProxyTarget;
begin
  changeProxyTarget := invocation as IChangeProxyTarget;
  changeProxyTarget.ChangeInvocationTarget(TValue.From(fSecondaryStorage));
end;

constructor TStorageInterceptor.Create(const secondaryStorage: IStorage);
begin
  fSecondaryStorage := secondaryStorage;
end;

procedure TStorageInterceptor.Intercept(const invocation: IInvocation);
var
  primaryStorage: TPrimaryStorage;
begin
  primaryStorage := invocation.Target.AsInterface as TPrimaryStorage;
  if not primaryStorage.IsUp then
    ChangeToSecondaryStorage(invocation);
  invocation.Proceed;
end;

end.
