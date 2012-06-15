(* SvBindings.pas
* Created: 2012-03-26 14:23:01
* Copyright (c) 2012, Linas Naginionis
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

unit SvBindings;

interface

uses
  SysUtils, DSharp.Bindings, DSharp.Core.DataConversion, SvDesignPatterns, SvBindings.Converters,
  Generics.Collections, Rtti, DSharp.Bindings.Notifications, Classes;

type
  TBindConverterType = (bctNone = 0, bctEnumFromString, bctListFromString, bctVariantFromString,
    bctDateTimeFirstDayOfTheMonthFromDateTime);

  BindAttribute = class(TCustomAttribute)
  private
    FSourcePropertyName: string;
    FTargetPropertyName: string;
    FBindingMode: TBindingMode;
    FConverter: IValueConverter;

  public
    constructor Create(const ASourcePropertyName: string = ''; const ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault; AConverterType: Integer = 0); overload;

    function GetSourcePropertyNameDef(const ADef: string = ''): string;
    function GetTargetPropertyNameDef(const ADef: string = ''): string;

    property SourcePropertyName: string read FSourcePropertyName;
    property BindingMode: TBindingMode read FBindingMode;
    property Converter: IValueConverter read FConverter;
  end;


  IBindableView = interface
    ['{98861A3F-AFC5-4E32-A81E-18061A964F75}']
    function GetBinder: TBindingGroup;
    property Binder: TBindingGroup read GetBinder;
  end;

  /// <remarks>
  /// Data binder manager
  /// Do not forget to add DSharp.Bindings.VCLControls to the interface uses clause of the View unit as the last item
  /// </remarks>
  TDataBindManager = class abstract
  private
    class function GetBinderAttribute(AMember: TRttiMember): BindAttribute;
    class procedure AddSourceNotification(const APropName: string; ABinding: TBinding; ASource: TObject);
  protected
    class procedure SourceObjUpdated(ASender: TObject;
        APropertyName: string; AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    class procedure BindView(ABindableView, ASource: TObject; ABinder: TBindingGroup);
  end;


var
  BindConverters: TMultiton<Integer,IValueConverter> = nil;
 // BindManager: TDataBindManager = nil;

implementation

procedure RegisterDefaultConverters();
begin
  BindConverters.RegisterFactoryMethod(Ord(bctEnumFromString),
    function: IValueConverter begin Result := TEnumConverter.Create; end);
  BindConverters.RegisterFactoryMethod(Ord(bctListFromString),
    function: IValueConverter begin Result := TListConverter.Create; end);
  BindConverters.RegisterFactoryMethod(Ord(bctVariantFromString),
    function: IValueConverter begin Result := TVariantConverter.Create; end);
  BindConverters.RegisterFactoryMethod(Ord(bctDateTimeFirstDayOfTheMonthFromDateTime),
    function: IValueConverter begin Result := TDateTimeFirstDayConverter.Create; end);
end;

{ BindAttribute }

constructor BindAttribute.Create(const ASourcePropertyName, ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverterType: Integer);
begin
  inherited Create;
  FSourcePropertyName := ASourcePropertyName;
  FTargetPropertyName := ATargetPropertyName;
  FBindingMode := ABindingMode;

  if AConverterType <> 0 then
  begin
    FConverter := BindConverters.GetInstance(AConverterType);
  end;
end;


function BindAttribute.GetSourcePropertyNameDef(const ADef: string): string;
begin
  Result := FSourcePropertyName;
  if Result = '' then
    Result := ADef;
end;

function BindAttribute.GetTargetPropertyNameDef(const ADef: string): string;
begin
  Result := FTargetPropertyName;
  if Result = '' then
    Result := ADef;
end;

{ TDataBindManager }

class procedure TDataBindManager.AddSourceNotification(const APropName: string; ABinding: TBinding;
  ASource: TObject);
var
  rTypeSource: TRttiType;
  rPropNew: TRttiProperty;
  rFieldNew: TRttiField;
begin
  rTypeSource := TRttiContext.Create.GetType(ASource.ClassInfo);
  rPropNew := rTypeSource.GetProperty(APropName);
  if Assigned(rPropNew) then
  begin
    if rPropNew.PropertyType.IsInstance then
    begin
      ABinding.NotifyOnSourceUpdated := True;
      ABinding.OnSourceUpdated := SourceObjUpdated;
    end;
  end
  else
  begin
    rFieldNew := rTypeSource.GetField(APropName);
    if Assigned(rFieldNew) then
    begin
      if rFieldNew.FieldType.IsInstance then
      begin
        ABinding.NotifyOnSourceUpdated := True;
        ABinding.OnSourceUpdated := SourceObjUpdated;
      end;
    end;
  end;
end;

class procedure TDataBindManager.BindView(ABindableView, ASource: TObject; ABinder: TBindingGroup);
var
  rTypeView: TRttiType;
  rProp: TRttiProperty;
  rField: TRttiField;
  LAttr: BindAttribute;
  LVal: TValue;
  LBinding: TBinding;
begin
  Assert(Assigned(ABindableView), 'ABindableView must be assigned');
  Assert(Assigned(ASource), 'ASource must be assigned');
  Assert(Assigned(ABinder), 'ABinder must be assigned');

  rTypeView := TRttiContext.Create.GetType(ABindableView.ClassInfo);

  //enumerate properties and fields
  for rField in rTypeView.GetFields do
  begin
    LAttr := GetBinderAttribute(rField);
    if Assigned(LAttr) then
    begin
      LVal := rField.GetValue(ABindableView);
      Assert(LVal.IsObject, 'Bindable property must be TObject');
      LBinding := ABinder.AddBinding(ASource, LAttr.GetSourcePropertyNameDef(rField.Name), LVal.AsObject,
        LAttr.GetTargetPropertyNameDef('Text'), LAttr.BindingMode, LAttr.Converter);
      //if source object's property is not managed, then assign event on which it will be freed
      AddSourceNotification(LAttr.GetSourcePropertyNameDef(rField.Name), LBinding, ASource);
    end;
  end;

  for rProp in rTypeView.GetProperties do
  begin
    LAttr := GetBinderAttribute(rProp);
    if Assigned(LAttr) then
    begin
      LVal := rProp.GetValue(ABindableView);
      Assert(LVal.IsObject, 'Bindable property must be TObject');
      LBinding := ABinder.AddBinding(ASource, LAttr.GetSourcePropertyNameDef(rProp.Name), LVal.AsObject,
        LAttr.GetTargetPropertyNameDef('Text'), LAttr.BindingMode, LAttr.Converter);
      //if source object's property is not managed, then assign event on which it will be freed
      AddSourceNotification(LAttr.GetSourcePropertyNameDef(rProp.Name), LBinding, ASource);
    end;
  end;
end;

class function TDataBindManager.GetBinderAttribute(AMember: TRttiMember): BindAttribute;
var
  LAttr: TCustomAttribute;
begin
  for LAttr in AMember.GetAttributes do
  begin
    if LAttr is BindAttribute then
    begin
      Exit(BindAttribute(LAttr));
    end;
  end;
  Result := nil;
end;

class procedure TDataBindManager.SourceObjUpdated(ASender: TObject; APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
var
  rProp: TRttiProperty;
  rField: TRttiField;
  AValue: TValue;
  ABinding: TBinding;
  ASource: TObject;
begin
  case AUpdateTrigger of
    utPropertyChanged:
    begin
      Assert(ASender is TPersistent, 'Sender must be derived at least from TPersistent');
      ABinding := GetBindingForComponent(ASender as TPersistent);
      if Assigned(ABinding) then
      begin
        ASource := ABinding.Source;
        //get property by name and free it
        rProp := TRttiContext.Create.GetType(ASource.ClassInfo).GetProperty(ABinding.SourcePropertyName);
        if Assigned(rProp) then
        begin
          AValue := rProp.GetValue(ASource);
          if AValue.IsObject then
          begin
            AValue.AsObject.Free;
          end;
        end
        else
        begin
          rField := TRttiContext.Create.GetType(ASource.ClassInfo).GetField(ABinding.SourcePropertyName);
          if Assigned(rField) then
          begin
            AValue := rField.GetValue(ASource);
            if AValue.IsObject then
            begin
              AValue.AsObject.Free;
            end;
          end;
        end;
      end;
    end;
    utLostFocus:;
    utExplicit:;
  end;
end;

initialization
  BindConverters := TMultiton<Integer,IValueConverter>.Create(False);
  RegisterDefaultConverters();
finalization
  BindConverters.Free;
end.
