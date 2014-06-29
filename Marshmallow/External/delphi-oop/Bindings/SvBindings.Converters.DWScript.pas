(* SvBindings.Converters.DWScript.pas
* Created: 2012-10-31
* Copyright (c) 2011, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net or linas@vikarina.lt
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SvBindings.Converters.DWScript;

interface

uses
  DSharp.Core.DataConversion.Expressions
  ,DSharp.DelphiWebScript.Expression
  ,SvBindings
  ,Rtti
  ;

type
  TDWScriptConverter = class(TExpressionConverter)
  private
    FAttribute: BindExpressionAttribute;
    FSource: TObject;
    FTarget: TObject;
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

implementation

uses
  DSharp.Core.DataConversion
  ;


procedure RegisterDefaultConverters();
begin
  TDataBindManager.RegisterConverter(bctDWScriptExpression,
    function(AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter
    var
      LConverter: TDWScriptConverter;
    begin
      LConverter := TDWScriptConverter.Create;
      LConverter.FAttribute := AAtribute as BindExpressionAttribute;
      LConverter.FSource := ASource;
      LConverter.FTarget := ATarget;
      Result := LConverter;
    end );
end;

{ TDWScriptConverter }

function TDWScriptConverter.Convert(const Value: TValue): TValue;
begin
  if (FAttribute.SourceExpression = '') then
    Result := Value
  else
  begin
    SourceToTargetExpression := ScriptExpression(FAttribute.SourceExpression, FSource);
    Result := inherited Convert(Value);
  end;
end;

function TDWScriptConverter.ConvertBack(const Value: TValue): TValue;
begin
  if (FAttribute.TargetExpression = '') then
    Result := Value
  else
  begin
    TargetToSourceExpression := ScriptExpression(FAttribute.TargetExpression, FTarget);
    Result := inherited ConvertBack(Value);
  end;
end;

initialization
  RegisterDefaultConverters();

end.
