unit DelphiUnit;

interface

uses
  Spring.Container.CppWrapper;

function InitApp : TContainer;
procedure RunApp;

implementation

uses
  Spring.Container,
  Spring.Container.Common,
  Fmx.Forms,
  Services,
  MainFrm;

var Container : Spring.Container.CppWrapper.TContainer;

type
  TDelphiImpl2WithCppDep = class(TInterfacedObject, IService2)
  private
    [Inject]
    FDep: IService1;
  public
    procedure Bar;
  end;

function InitApp : Spring.Container.CppWrapper.TContainer;
begin
  Assert(Container = nil);
  Container := Spring.Container.CppWrapper.TContainer.Create(
    Spring.Container.TContainer.Create);
  Result := Container;
end;

procedure RunApp;
begin
  Container.RegisterType<TDelphiImpl2WithCppDep>;
  Container.Build;
  Container.Resolve<IService1>.Foo;
  Container.Resolve<IService2>.Bar;
  Container.Resolve<IService1>('cppdep').Foo;
  if (not IsConsole) then
  begin
    Application.CreateForm(TMainForm, MainForm);
    Application.Initialize;
    Application.Run;
  end;
end;

{ TService2WithCppDep }

procedure TDelphiImpl2WithCppDep.Bar;
begin
  if (IsConsole) then
    Writeln('TDelphiImpl2WithCppDep.Bar');
  FDep.Foo;
end;

initialization
finalization
  Container.Free;

end.

