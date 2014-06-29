unit Core.Comparers;

interface

uses
  Generics.Defaults
  ;

type
  TStringCaseInsensitiveComparer = class(TEqualityComparer<string>)
  public
    function Equals(const Left, Right: string): Boolean;
      reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer;
      reintroduce; overload; override;
  end;

implementation

uses
  SysUtils
  ,StrUtils
  ;

{ TStringCaseInsensitiveComparer }

function TStringCaseInsensitiveComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := SameText(Left, Right);
end;

function TStringCaseInsensitiveComparer.GetHashCode(const Value: string): Integer;
var
  LValue: string;
begin
  LValue := UpperCase(Value);
  Result := BobJenkinsHash(LValue[1], Length(LValue) * SizeOf(LValue[1]), 0);
end;

end.
