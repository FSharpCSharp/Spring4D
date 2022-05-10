program DynamicAbstractFactory;

{$APPTYPE CONSOLE}

uses
  Spring.Collections,
  Spring.Container;

type
  IOrderShipper = interface
    ['{F4653C0C-2C05-4348-A744-3288E520F586}']
    procedure Ship;
  end;

  IOrderShipperFactory = interface(IInvokable)
    ['{F632D1FB-9C34-48FD-BD72-6BBC436D1B47}']
    function Create: IOrderShipper; overload;
    function Create(const name: string): IOrderShipper; overload;
  end;

  IOrderShipperListFactory = interface(IInvokable)
    ['{E5A4BC4D-68F2-41AA-A60F-B5884C268094}']
    function Create: IList<IOrderShipper>;
  end;

  TOrderShipper = class(TInterfacedObject, IOrderShipper)
  private
    fName: string;
  public
    constructor Create; overload;
    constructor Create(const name: string); overload;

    procedure Ship;
  end;

constructor TOrderShipper.Create;
begin
  Create('default');
end;

constructor TOrderShipper.Create(const name: string);
begin
  inherited Create;
  fName := name
end;

procedure TOrderShipper.Ship;
begin
  Writeln('shipped by: ', fName);
end;

procedure Main;
var
  factory: IOrderShipperFactory;
  service: IOrderShipper;
  services: IList<IOrderShipper>;
begin
  ReportMemoryLeaksOnShutdown := True;

  GlobalContainer.RegisterType<IOrderShipper, TOrderShipper>('default').AsDefault;
  GlobalContainer.RegisterType<IOrderShipper, TOrderShipper>('test').InjectConstructor(['test']);
  GlobalContainer.RegisterFactory<IOrderShipperFactory>;
  GlobalContainer.RegisterFactory<IOrderShipperListFactory>;
  GlobalContainer.Build;

  factory := GlobalContainer.Resolve<IOrderShipperFactory>;
  service := factory.Create;
  service.Ship;

  service := factory.Create('test');
  service.Ship;

  services := GlobalContainer.Resolve<IOrderShipperListFactory>.Create;
  for service in services do
    service.Ship;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  Main;
  Readln;
end.
