unit Pet;

interface

type
  {$RTTI EXPLICIT METHODS([vcProtected..vcPublished])}
  TPet = class
  private
    fName: string;
    fAge: Integer;
    fDeceased: Boolean;
  protected
    function GetAge: Integer; virtual;
    function GetDeceased: Boolean; virtual;
    function GetName: string; virtual;
    procedure SetAge(const Value: Integer); virtual;
    procedure SetDeceased(const Value: Boolean); virtual;
    procedure SetName(const Value: string); virtual;
  public
    function ToString: string; override;

    procedure Test(var i: Integer); virtual;

    property Name: string read GetName write SetName;
    property Age: Integer read GetAge write SetAge;
    property Deceased: Boolean read GetDeceased write SetDeceased;
  end;

  TPetWithNonVirtualMethod = class(TPet)
  public
    procedure NonVirtualMethod;
  end;

  TPetWithNonVirtualSetter = class(TPet)
  protected
    function GetNonVirtualProperty: Integer;
    procedure SetNonVirtualProperty(const Value: Integer);
  public
    property NonVirtualProperty: Integer read GetNonVirtualProperty
      write SetNonVirtualProperty;
  end;

implementation

uses
  SysUtils;

{ TPet }

function TPet.GetAge: Integer;
begin
  Result := fAge;
end;

function TPet.GetDeceased: Boolean;
begin
  Result := fDeceased;
end;

function TPet.GetName: string;
begin
  Result := fName;
end;

procedure TPet.SetAge(const Value: Integer);
begin
  fAge := Value;
end;

procedure TPet.SetDeceased(const Value: Boolean);
begin
  fDeceased := Value;
end;

procedure TPet.SetName(const Value: string);
begin
  fName := Value;
end;

procedure TPet.Test(var i: Integer);
begin
  Inc(i);
end;

function TPet.ToString: string;
begin
  Result := Format('Name: %1:d, Age: %1:d, Deceased: %2:s', [Name, Age, BoolToStr(Deceased)]);
end;

{ TPetWithNonVirtualMethod }

procedure TPetWithNonVirtualMethod.NonVirtualMethod;
begin
end;

{ TPetWithNonVirtualSetter }

function TPetWithNonVirtualSetter.GetNonVirtualProperty: Integer;
begin
  Result := 0;
end;

procedure TPetWithNonVirtualSetter.SetNonVirtualProperty(
  const Value: Integer);
begin
end;

end.
