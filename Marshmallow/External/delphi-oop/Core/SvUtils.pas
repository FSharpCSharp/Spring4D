unit SvUtils;

interface

  function iif(Value: Boolean; const ATrue: string; const AFalse: string = ''): string; inline; overload;
  function iif(Value: Boolean; const ATrue: TObject; const AFalse: TObject = nil): TObject; inline; overload;

implementation

function iif(Value: Boolean; const ATrue: string; const AFalse: string = ''): string;
begin
  if Value then
    Result := ATrue
  else
    Result := AFalse;
end;

function iif(Value: Boolean; const ATrue: TObject; const AFalse: TObject): TObject;
begin
  if Value then
    Result := ATrue
  else
    Result := AFalse;
end;

end.
