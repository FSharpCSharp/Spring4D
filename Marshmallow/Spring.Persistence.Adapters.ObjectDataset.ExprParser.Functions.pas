unit Spring.Persistence.Adapters.ObjectDataset.ExprParser.Functions;

interface

uses
  Generics.Collections
  ,SysUtils
  ;

type
  TFunctionGetValueProc = reference to function(const Args: Variant): Variant;

  TFilterFunctions = class sealed
  private
    class var
      FFunctions: TDictionary<string,TFunctionGetValueProc>;
      FIsoFormatSettings: TFormatSettings;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterFunction(const AName: string; AGetValueFunc: TFunctionGetValueProc);
    class procedure UnregisterFunction(const AName: string);
    class function TryGetFunction(const AName: string; out AGetValueFunc: TFunctionGetValueProc): Boolean;
  end;

implementation

uses
  Generics.Defaults
  ,DateUtils
  ,Variants
  ,Math
  ,StrUtils
  ;

{ TFilterFunctions }

class constructor TFilterFunctions.Create;
var
  LCaseInsensitiveComparer: IEqualityComparer<string>;
begin
  LCaseInsensitiveComparer := TEqualityComparer<string>.Construct(
    function(const Left, Right: string): Boolean
    begin
      Result := SameText(Left, Right);
    end,
    function(const Value: string): Integer
    var s: string;
    begin
      s := UpperCase(Value);
      Result := BobJenkinsHash(s[1], Length(s) * SizeOf(s[1]), 0);
    end
  );
  FIsoFormatSettings := TFormatSettings.Create;
  FIsoFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FIsoFormatSettings.DateSeparator := '-';
  FIsoFormatSettings.DecimalSeparator := '.';
  FFunctions := TDictionary<string,TFunctionGetValueProc>.Create(50, LCaseInsensitiveComparer);
end;

class destructor TFilterFunctions.Destroy;
begin
  FFunctions.Free;
end;

class procedure TFilterFunctions.RegisterFunction(const AName: string; AGetValueFunc: TFunctionGetValueProc);
begin
  FFunctions.AddOrSetValue(AName, AGetValueFunc);
end;

class function TFilterFunctions.TryGetFunction(const AName: string; out AGetValueFunc: TFunctionGetValueProc): Boolean;
begin
  Result := FFunctions.TryGetValue(AName, AGetValueFunc);
end;

class procedure TFilterFunctions.UnregisterFunction(const AName: string);
begin
  FFunctions.Remove(AName);
end;

function VarArrayLength(AValue: Variant): Integer;
begin
  Result := VarArrayHighBound(AValue, 1);
  if Result >= 0 then
    Inc(Result)
  else
  begin
    Result := 0;
  end;
end;

procedure RegisterMainFunctions();
begin
  TFilterFunctions.RegisterFunction('today',
    function(const Args: Variant): Variant
    begin
      Assert((VarArrayLength(Args) = 0), 'Today does not require any argument');
      Result := Today;
    end
    );
  TFilterFunctions.RegisterFunction('getdate',
    function(const Args: Variant): Variant
    begin
      Result := Now;
    end
    );
  TFilterFunctions.RegisterFunction('IsNull',
    function(const Args: Variant): Variant
    begin
      Assert((VarArrayLength(Args) = 2), 'IsNull requires 2 arguments');
      if VarIsNull(Args[0]) then
        Result := Args[1]
      else
        Result := Args[0];
    end
    );
  TFilterFunctions.RegisterFunction('Abs',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Abs requires 1 argument');
      Result := Abs(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('IsNotNull',
    function(const Args: Variant): Variant
    begin
      Result := 1;
    end
    );
  TFilterFunctions.RegisterFunction('lower',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'lower requires 1 argument');
      Result := AnsiLowerCase(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('upper',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'upper requires 1 argument');
      Result := AnsiUpperCase(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('coalesce',
    function(const Args: Variant): Variant
    var
      i: Integer;
    begin
      Assert(VarArrayLength(Args) >= 2, 'coalesce requires at least 2 arguments');
      for i := VarArrayLowBound(Args, 1) to VarArrayHighBound(Args, 1) do
      begin
        if not VarIsNull(Args[i]) then
        begin
          Exit(Args[i]);
        end;
      end;
      Result := Null;
    end
    );
  TFilterFunctions.RegisterFunction('Random',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Random requires 1 argument');
      Result := Random(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('Round',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Round requires 1 argument');
      Result := Round(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('Trim',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Trim requires 1 argument');
      Result := Trim(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('substr',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 3, 'substr requires 3 arguments');
      Assert( (VarType(Args[1]) in [varInteger, varSmallint, varInt64, varLongWord, varUInt64, varWord, varByte, varShortInt]), 'Index argument must be Integer' ) ;
      Assert( (VarType(Args[2]) in [varInteger, varSmallint, varInt64, varLongWord, varUInt64, varWord, varByte, varShortInt]), 'Length argument must be Integer' ) ;
      Result := Copy(Args[0], Integer(Args[1]), Integer(Args[2]));
    end
    );
  TFilterFunctions.RegisterFunction('Quote',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Quote requires 1 argument');
      Result := QuotedStr(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('Max',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'Max requires 2 arguments');
      Result := Max(Args[0], Args[1]);
    end
    );
  TFilterFunctions.RegisterFunction('Min',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'Min requires 2 arguments');
      Result := Min(Args[0], Args[1]);
    end
    );
  TFilterFunctions.RegisterFunction('Length',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Length requires 1 argument');
      Result := Length(Args[0]);
    end
    );
  TFilterFunctions.RegisterFunction('Instr',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 2, 'Instr requires 2 arguments');
      Result := PosEx(Args[0], Args[1]);
    end
    );
  TFilterFunctions.RegisterFunction('Replace',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 3, 'Replace requires 3 arguments');
      Result := ReplaceText(Args[0], Args[1], Args[2]);
    end
    );
  TFilterFunctions.RegisterFunction('Date',
    function(const Args: Variant): Variant
    begin
      Assert(VarArrayLength(Args) = 1, 'Date requires 1 argument');
      Result := StrToDate(Args[0], TFilterFunctions.FIsoFormatSettings);
    end
    );

end;

initialization
  RegisterMainFunctions();

end.
