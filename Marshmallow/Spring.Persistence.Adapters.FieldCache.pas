unit Spring.Persistence.Adapters.FieldCache;

interface

uses
  DB, Spring.Collections;

type
  IFieldCache = interface(IInvokable)
    ['{11B51ABB-0C29-40CA-A2C1-623CBFF86F4F}']
    function FieldnameExists(const AFieldName: string): Boolean;
    function GetFieldValue(const AFieldname: string): Variant;
  end;

  TFieldCache = class(TInterfacedObject, IFieldCache)
  private
    FValues: IDictionary<string,TField>;
    FDataset: TDataSet;
  protected
    procedure Build(); virtual;
    function FieldnameExists(const AFieldName: string): Boolean; virtual;
    function GetFieldValue(const AFieldname: string): Variant; virtual;
  public
    constructor Create(ADataset: TDataset); virtual;

  end;


implementation

uses
  Spring.Persistence.Core.Comparers
  ;

{ TFieldCache }

procedure TFieldCache.Build;
var
  i: Integer;
begin
  if FValues.Count = 0 then
  begin
    for i := 0 to FDataset.FieldCount - 1 do
    begin
      FValues.Add(FDataset.Fields[i].FieldName, FDataset.Fields[i]);
    end;
  end;
end;

constructor TFieldCache.Create(ADataset: TDataset);
begin
  inherited Create();
  FDataset := ADataset;
  FValues := TCollections.CreateDictionary<string, TField>(TStringCaseInsensitiveComparer.Create);
  Build;
end;

function TFieldCache.FieldnameExists(const AFieldName: string): Boolean;
begin
  Result := FValues.ContainsKey(AFieldName);
end;

function TFieldCache.GetFieldValue(const AFieldname: string): Variant;
begin
  Result := FValues[AFieldname].Value;
end;

end.
