unit Registration.EnvironmentVariable;

interface

uses
  SysUtils, Classes;

type

  TBDSVersionNumber = (bdsDelphi2009, bdsDelphi2010, bdsDelphiXE,
    bdsDelphiXE2, bdsDelphiXE3, bdsDelphiXE4, bdsDelphiXE5, bdsDelphiXE6
    ,bdsDelphiXE7, bdsDelphiXE8, bdsDelphiXE9);



  TRegistration = class
  private
    FORMDirectory: string;
    FBDSVersion: TBDSVersionNumber;
    FBDSVersionsList: TStrings;
    FLastErrorMsg: string;
  protected
    procedure PopulateBDSVersionsList(); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function BDSVersionToString(): string;

    function Execute(): Boolean; virtual;

    function Validate(): Boolean;  

    property BDSVersionsList: TStrings read FBDSVersionsList;
    property BDSVersion: TBDSVersionNumber read FBDSVersion write FBDSVersion;
    property ORMDirectory: string read FORMDirectory write FORMDirectory;

    property LastErrorMsg: string read FLastErrorMsg;
  end;

const
  BDSVersionData : array[TBDSVersionNumber] of Byte =
    ({bdsDelphi2009 =} 6, {bdsDelphi2010 =} 7, {bdsDelphiXE =} 8,
    {bdsDelphiXE2 =} 9, {bdsDelphiXE3 =} 10, {bdsDelphiXE4 =} 11, {bdsDelphiXE5 =} 12, {bdsDelphiXE6 =} 13
    ,{bdsDelphiXE7 =} 14, {bdsDelphiXE8 =} 15, {bdsDelphiXE9 =} 16
    );

implementation

uses
  Constants
  ,Registry
  ,Windows
  ,TypInfo
  ;

{ TRegistration }

function TRegistration.BDSVersionToString: string;
begin
  case FBDSVersion of
    bdsDelphi2009, bdsDelphi2010: Result := Format('CodeGear\\BDS\%0:D.0', [BDSVersionData[FBDSVersion]])
  else
    Result := Format('Embarcadero\\BDS\%0:D.0', [BDSVersionData[FBDSVersion]]);
  end;
end;

constructor TRegistration.Create;
begin
  inherited Create;
  FBDSVersion := bdsDelphiXE;
  FBDSVersionsList := TStringList.Create;
  PopulateBDSVersionsList();
end;

destructor TRegistration.Destroy;
begin
  FBDSVersionsList.Free;
  inherited Destroy;
end;

function TRegistration.Execute: Boolean;
var
  LRegistry: TRegistry;
begin
  if not Validate then
    Exit(False);

  Result := False;

  try
    LRegistry := TRegistry.Create(KEY_WRITE);
    try
      LRegistry.RootKey := HKEY_CURRENT_USER;
      if LRegistry.OpenKey(Format('Software\\%0:S\\Environment Variables\\'
        , [BDSVersionToString]), True) then
      begin
        LRegistry.WriteString(ORM_ENV_VARIABLE_NAME, FORMDirectory);
        Result := True;
      end;
    finally
      LRegistry.Free;
    end;
  except
    on E:Exception do
    begin
      Result := False;
      FLastErrorMsg := E.Message;
    end;
  end;
end;

procedure TRegistration.PopulateBDSVersionsList;
var
  LEnum: TBDSVersionNumber;
begin
  FBDSVersionsList.Clear;
  for LEnum := Low(TBDSVersionNumber) to High(TBDSVersionNumber) do
  begin
    FBDSVersionsList.Add(GetEnumName(System.TypeInfo(TBDSVersionNumber), Ord(LEnum)));
  end;
end;

function TRegistration.Validate: Boolean;
begin
  FLastErrorMsg := '';
  Result := (FORMDirectory <> '') and (DirectoryExists(FORMDirectory));
  if not Result then
    FLastErrorMsg := 'Invalid ORM directory path';
end;

end.
