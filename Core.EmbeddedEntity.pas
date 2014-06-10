unit Core.EmbeddedEntity;

interface

uses
  Core.Interfaces
  ;

type

  TEmbeddedEntity = class(TInterfacedObject, IEmbeddedEntity)
  protected
    function IsObject(): Boolean; virtual; abstract;
    function IsArray(): Boolean; virtual; abstract;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function GetValue(const AFieldName: string): Variant; virtual;

  end;

  TEmbeddedObjectEntity = class(TEmbeddedEntity)
  public
    function IsArray(): Boolean; override;
    function IsObject(): Boolean; override;
  end;

  TEmbeddedArrayEntity = class(TEmbeddedEntity)
  public
    function IsArray(): Boolean; override;
    function IsObject(): Boolean; override;
  end;


implementation

uses
  Variants
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

function TEmbeddedEntity.GetValue(const AFieldName: string): Variant;
begin
  Result := Null;
end;

{ TEmbeddedObjectEntity }

function TEmbeddedObjectEntity.IsArray: Boolean;
begin
  Result := False;
end;

function TEmbeddedObjectEntity.IsObject: Boolean;
begin
  Result := True;
end;

{ TEmbeddedArrayEntity }

function TEmbeddedArrayEntity.IsArray: Boolean;
begin
  Result := True;
end;

function TEmbeddedArrayEntity.IsObject: Boolean;
begin
  Result := False;
end;

end.
