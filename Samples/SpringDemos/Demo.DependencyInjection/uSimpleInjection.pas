unit uSimpleInjection;

interface

uses
      Spring;

type

    IMakeNoise = interface
      ['{2CBCF71D-C5C0-4642-B47C-E0FC56857DE8}']
      procedure MakeNoise;
    end;

    IPetNoiseMaker = interface
    ['{156FEFE7-1CD1-4D58-995C-3B9D69A05931}']
      procedure MakePetNoises; overload;
      procedure MakePetNoises(aAnimalName: string); overload;
    end;

implementation

uses
      Spring.Services
    , Spring.Container
    ;


type

    TPetNoiseProvider = class(TInterfacedObject, IPetNoiseMaker)
    private
      [Injection('Dog')]
      FPet: IMakeNoise;
    public
      procedure MakePetNoises; overload;
      procedure MakePetNoises(aAnimalName: string); overload;
    end;

  TDog = class(TInterfacedObject, IMakeNoise)
      procedure MakeNoise;
    end;

  TCat = class(TInterfacedObject, IMakeNoise)
    procedure MakeNoise;
  end;

  TCow = class(TInterfacedObject, IMakeNoise)
    procedure MakeNoise;
  end;

{ TDog }

procedure TDog.MakeNoise;
begin
  WriteLn('Woof!!');
end;

{ TCat }

procedure TCat.MakeNoise;
begin
  WriteLn('Meow!!');
end;

{ TCow }

procedure TCow.MakeNoise;
begin
  WriteLn('Moo!');
end;


{ TPetNoiseProvider }

procedure TPetNoiseProvider.MakePetNoises;
begin
  FPet.MakeNoise;
end;

procedure TPetNoiseProvider.MakePetNoises(aAnimalName: string);
begin
  FPet := ServiceLocator.GetService<IMakeNoise>(aAnimalName);
  FPet.MakeNoise;
end;

initialization
  GlobalContainer.RegisterComponent<TCat>.Implements<IMakeNoise>('Cat');
  GlobalContainer.RegisterComponent<TDog>.Implements<IMakeNoise>('Dog');
  GlobalContainer.RegisterComponent<TCow>.Implements<IMakeNoise>('Cow');

  GlobalContainer.RegisterComponent<TPetNoiseProvider>.Implements<IPetNoiseMaker>;

end.
