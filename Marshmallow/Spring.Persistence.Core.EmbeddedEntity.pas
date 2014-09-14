unit Spring.Persistence.Core.EmbeddedEntity;

interface

uses
  Spring.Persistence.Core.Interfaces
  ,Spring.Collections
  ;

type

  TEmbeddedEntity = class(TInterfacedObject, IDBResultset)
  protected
    function IsObject(): Boolean; virtual; abstract;
    function IsArray(): Boolean; virtual; abstract;

    function IsEmpty(): Boolean; virtual; abstract;
    function Next(): Boolean;
    function FieldnameExists(const AFieldName: string): Boolean; virtual; abstract;
    function GetFieldValue(AIndex: Integer): Variant; overload; virtual; abstract;
    function GetFieldValue(const AFieldname: string): Variant; overload; virtual; abstract;
    function GetFieldCount(): Integer; virtual; abstract;
    function GetFieldName(AIndex: Integer): string; virtual; abstract;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function GetValue(const AFieldName: string): Variant; virtual; abstract;
    procedure AddValue(const AFieldname: string; const AValue: Variant); virtual; abstract;
  end;

  TFieldKey = string;

  TEmbeddedObjectEntity = class(TEmbeddedEntity)
  private
    FValues: IDictionary<string, Variant>;
    FIndexedValues: IList<string>;
    FCurrent: Integer;
  protected
    function GetKey(AFieldname: string): TFieldKey; virtual;

    function IsEmpty(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  public
    constructor Create(); override;
    destructor Destroy; override;

    function IsArray(): Boolean; override;
    function IsObject(): Boolean; override;

    function GetValue(const AFieldName: string): Variant; override;
    procedure AddValue(const AFieldname: string; const AValue: Variant); override;

  end;

  TEmbeddedArrayEntity = class(TEmbeddedEntity)
  private
    FValues: IList<Variant>;
    FCurrent: Integer;
  protected
    function IsEmpty(): Boolean; override;
    function FieldnameExists(const AFieldName: string): Boolean; override;
    function GetFieldValue(AIndex: Integer): Variant; overload; override;
    function GetFieldValue(const AFieldname: string): Variant; overload; override;
    function GetFieldCount(): Integer; override;
    function GetFieldName(AIndex: Integer): string; override;
  public
    constructor Create(); override;

    function IsArray(): Boolean; override;
    function IsObject(): Boolean; override;

    function GetValue(const AFieldName: string): Variant; override;
    procedure AddValue(const AFieldname: string; const AValue: Variant); override;
  end;


implementation

uses
  Variants
  ,SysUtils
  ,Spring.Persistence.Core.Utils
  ;

{ TEmbeddedEntity }

constructor TEmbeddedEntity.Create;
begin
  inherited Create;
end;

destructor TEmbeddedEntity.Destroy;
begin

  inherited Destroy;
end;

function TEmbeddedEntity.Next: Boolean;
begin
  Result := True;
end;

{ TEmbeddedObjectEntity }

constructor TEmbeddedObjectEntity.Create;
begin
  inherited Create;
  FValues := TCollections.CreateDictionary<string, Variant>();
  FIndexedValues := TCollections.CreateList<string>();
  FCurrent := -1;
end;

destructor TEmbeddedObjectEntity.Destroy;
begin
  inherited Destroy;
end;

function TEmbeddedObjectEntity.FieldnameExists(
  const AFieldName: string): Boolean;
begin
  Result := FValues.ContainsKey(GetKey(AFieldName));
end;

function TEmbeddedObjectEntity.GetFieldCount: Integer;
begin
  Result := FIndexedValues.Count;
end;

function TEmbeddedObjectEntity.GetFieldName(AIndex: Integer): string;
begin
  Result := FIndexedValues[AIndex];
end;

function TEmbeddedObjectEntity.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := GetValue(GetFieldName(AIndex));
end;

function TEmbeddedObjectEntity.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := GetValue(AFieldname);
end;

function TEmbeddedObjectEntity.GetKey(AFieldname: string): TFieldKey;
begin
  Result := UpperCase(AFieldname);
end;

function TEmbeddedObjectEntity.GetValue(const AFieldName: string): Variant;
begin
  Result := FValues[GetKey(AFieldName)];
end;

function TEmbeddedObjectEntity.IsArray: Boolean;
begin
  Result := False;
end;

function TEmbeddedObjectEntity.IsEmpty: Boolean;
begin
  Result := FCurrent >= FIndexedValues.Count;
  FCurrent := FIndexedValues.Count;
end;

function TEmbeddedObjectEntity.IsObject: Boolean;
begin
  Result := True;
end;

procedure TEmbeddedObjectEntity.AddValue(const AFieldname: string;
  const AValue: Variant);
begin
  FValues.AddOrSetValue(GetKey(AFieldname), AValue);
  FIndexedValues.Add(GetKey(AFieldname));
end;

{ TEmbeddedArrayEntity }

procedure TEmbeddedArrayEntity.AddValue(const AFieldname: string;
  const AValue: Variant);
begin
  FValues.Add(AValue);
end;

constructor TEmbeddedArrayEntity.Create;
begin
  inherited;
  FValues := TCollections.CreateList<Variant>();
  FCurrent := -1;
end;

function TEmbeddedArrayEntity.FieldnameExists(
  const AFieldName: string): Boolean;
begin
  Result := False;
end;

function TEmbeddedArrayEntity.GetFieldCount: Integer;
begin
  Result := 0;
end;

function TEmbeddedArrayEntity.GetFieldName(AIndex: Integer): string;
begin
  Result := '';
end;

function TEmbeddedArrayEntity.GetFieldValue(AIndex: Integer): Variant;
begin
  Result := FValues[AIndex];
end;

function TEmbeddedArrayEntity.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := GetValue(AFieldname);
end;

function TEmbeddedArrayEntity.GetValue(const AFieldName: string): Variant;
var
  LResultset: IDBResultset;
begin
  Result := FValues[FCurrent];
  if VarType(Result) = varUnknown then
  begin
    LResultset := TUtils.GetResultsetFromVariant(Result);
    Result := LResultset.GetFieldValue(AFieldName);
  end;
end;

function TEmbeddedArrayEntity.IsArray: Boolean;
begin
  Result := True;
end;

function TEmbeddedArrayEntity.IsEmpty: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent >= FValues.Count;
end;

function TEmbeddedArrayEntity.IsObject: Boolean;
begin
  Result := False;
end;

end.
