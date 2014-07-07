unit StorageFactory;

interface

uses
  Spring.Interception,
  Interfaces;

type
  TStorageFactory = class
  private
    fPrimaryStorage: IStorage;
    fSecondaryStorage: IStorage;
    fGenerator: TProxyGenerator;
  public
    constructor Create(const primaryStorage: IStorage);
    destructor Destroy; override;
    function GetStorage: IStorage;
    property SecondaryStorage: IStorage read fSecondaryStorage write fSecondaryStorage;
  end;

implementation

uses
  StorageInterceptor;

{ TStorageFactory }

constructor TStorageFactory.Create(const primaryStorage: IStorage);
begin
  fPrimaryStorage := primaryStorage;
  fGenerator := TProxyGenerator.Create;
end;

destructor TStorageFactory.Destroy;
begin
  fGenerator.Free;
  inherited;
end;

function TStorageFactory.GetStorage: IStorage;
var
  interceptor: IInterceptor;
begin
  interceptor := TStorageInterceptor.Create(fSecondaryStorage);
  Result := fGenerator.CreateInterfaceProxyWithTarget<IStorage>(
    fPrimaryStorage, interceptor);
end;

end.
