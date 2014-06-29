{*******************************************************}
{                                                       }
{       SvBindings Validation                           }
{                                                       }
{       Copyright (C) 2011 "Linas Naginionis"           }
{                                                       }
{*******************************************************}

unit SvBindings.Validation;

interface

uses
  SysUtils, DSharp.Core.Validations, Rtti;

type
  TFutureDateRule = class(TValidationRule)
  public
    function Validate(const Value: TValue): IValidationResult; override;
  end;

  TDelegateRule = class(TValidationRule)
  private
    FDelegate: TFunc<TValue, IValidationResult>;
  public
    constructor Create(ADelegate: TFunc<TValue, IValidationResult>); reintroduce;
    function Validate(const Value: TValue): IValidationResult; override;
  end;

implementation

{ TFutureDateRule }

function TFutureDateRule.Validate(const Value: TValue): IValidationResult;
begin
  // invalid if date is not in the future
  if Value.AsType<TDate> <= Now() then
  begin
    Result := TValidationResult.Create(False, 'Date must be in the future');
  end
  else
  begin
    // make sure to return a valid result if everything is ok
    Result := TValidationResult.ValidResult;
  end;
end;

{ TDelegateRule }

constructor TDelegateRule.Create(ADelegate: TFunc<TValue, IValidationResult>);
begin
  FDelegate := ADelegate;
end;

function TDelegateRule.Validate(const Value: TValue): IValidationResult;
begin
  Result := FDelegate(Value);
end;

end.
