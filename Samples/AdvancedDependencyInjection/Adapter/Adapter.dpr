program Adapter;

{$APPTYPE CONSOLE}

uses
  Spring,
  Spring.Container,
  System.SysUtils;

type
  ICommand = interface
    ['{570FE027-09A1-4BE9-BFDF-A0E6EC41460C}']
  end;

  TSaveCommand = class(TInterfacedObject, ICommand)
  end;

  TOpenCommand = class(TInterfacedObject, ICommand)
  end;

  TToolButton = class
  private
    fCommand: ICommand;
  public
    constructor Create(const command: ICommand);
    property Command: ICommand read fCommand;
  end;

{ TToolButton }

constructor TToolButton.Create(const command: ICommand);
begin
  fCommand := command;
end;

type
  TAdapterFactory<TService, TAdapter> = reference to function(const service: TService): TAdapter;
  TContainerHelper = class helper for TContainer
  public
    procedure RegisterAdapter<TService, TAdapter>; overload;
    procedure RegisterAdapter<TService, TAdapter>(const delegate: TAdapterFactory<TService, TAdapter>); overload;
  end;

{ TContainerHelper }

procedure TContainerHelper.RegisterAdapter<TService, TAdapter>;
begin
  RegisterType<TAdapter>;
  RegisterAdapter<TService, TAdapter>(
    function(const service: TService): TAdapter
    begin
      Result := Resolve<TAdapter>(TValue.From(service));
    end);
end;

procedure TContainerHelper.RegisterAdapter<TService, TAdapter>(
  const delegate: TAdapterFactory<TService, TAdapter>);
begin
  RegisterType<TArray<TAdapter>>(
    function: TArray<TAdapter>
    var
      services: TArray<TService>;
      i: Integer;
    begin
      services := ResolveAll<TService>;
      SetLength(Result, Length(services));
      for i := 0 to High(services) do
        Result[i] := delegate(services[i]);
    end);
end;

procedure Main;
var
  buttons: TArray<TToolButton>;
begin
  GlobalContainer.RegisterType<ICommand,TSaveCommand>('save');
  GlobalContainer.RegisterType<ICommand,TOpenCommand>('open');
  GlobalContainer.RegisterAdapter<ICommand, TToolButton>;
  GlobalContainer.Build;

  buttons := GlobalContainer.Resolve<TArray<TToolButton>>;
  Assert(Length(buttons) = 2);
  Assert(buttons[0].Command is TSaveCommand);
  Assert(buttons[1].Command is TOpenCommand);
end;

begin
  Main;
end.
