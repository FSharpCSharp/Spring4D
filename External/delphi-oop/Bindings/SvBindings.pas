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
  SysUtils, DSharp.Bindings, DSharp.Core.DataConversion, SvBindings.Converters,
  Generics.Collections, Rtti, DSharp.Bindings.Notifications, Classes;

type
  TBindConverterType = (bctNone = 0, bctEnumFromString, bctListFromString, bctVariantFromString,
    bctDateTimeFirstDayOfTheMonthFromDateTime, bctDWScriptExpression);

  BindAttribute = class(TCustomAttribute)
  private
    FSourcePropertyName: string;
    FTargetPropertyName: string;
    FBindingMode: TBindingMode;
    FConverterType: TBindConverterType;
  public
    constructor Create(const ASourcePropertyName: string = ''; const ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault; AConverterType: Integer = 0); overload;

    function GetSourcePropertyNameDef(const ADef: string = ''): string;
    function GetTargetPropertyNameDef(const ADef: string = ''): string;
    function GetConverter(ASource, ATarget: TObject): IValueConverter; virtual;

    property SourcePropertyName: string read FSourcePropertyName;
    property BindingMode: TBindingMode read FBindingMode;
  end;
  /// <remarks>
  /// Can write DWScript expressions. DWScript must be present in the sources
  /// </remarks>
  BindExpressionAttribute = class(BindAttribute)
  private
    FSourceExpression: string;
    FTargetExpression: string;
  public
    constructor Create(const ASourcePropertyName: string; const ATargetPropertyName: string;
      const ASourceToTargetExpression: string; const ATargetToSourceExpression: string;
      ABindingMode: TBindingMode = BindingModeDefault); overload;

    property SourceExpression: string read FSourceExpression write FSourceExpression;
    property TargetExpression: string read FTargetExpression write FTargetExpression;
  end;

  BindEventAttribute = class(BindAttribute)
  public
    ControlName: string;
    EventName: string;
  public
    constructor Create(const AControlName: string; const AEventName: string); overload;
  end;


  IBindableView = interface
    ['{98861A3F-AFC5-4E32-A81E-18061A964F75}']
    function GetBinder: TBindingGroup;
    property Binder: TBindingGroup read GetBinder;
  end;

  TGetInstanceFunc = reference to function(AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter;

  /// <remarks>
  /// Data binder manager
  /// Do not forget to add DSharp.Bindings.VCLControls to the interface uses clause of the View unit as the last item
  /// </remarks>
  TDataBindManager = class abstract
  strict private
    class var FBinder: TBindingGroup;
    class var FConverters: TDictionary<TBindConverterType,TGetInstanceFunc>;
  private
    class constructor Create;
    class destructor Destroy;

    class procedure AddSourceNotification(const APropName: string; ABinding: TBinding; ASource: TObject);
  protected
    class procedure SourceObjUpdated(ASender: TObject;
        APropertyName: string; AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    class procedure RegisterConverter(AKey: TBindConverterType; AGetInstanceFunc: TGetInstanceFunc);
    class function GetConverter(AKey: TBindConverterType; AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter;

    class function AddBinding(ASource: TObject = nil; ASourcePropertyName: string = '';
      ATarget: TObject = nil; ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault;
      AConverter: IValueConverter = nil): TBinding;
    class function GetBindingForTarget(ATarget: TObject): TBinding;
    class function GetBinderAttribute(AMember: TRttiMember): BindAttribute;
    class function GetPropertyValueByName(AObject: TObject; const APropertyname: string): TValue;
    class procedure UpdateTargets();
    class procedure UpdateSources();

    class procedure BindView(ABindableView, ASource: TObject; ABinder: TBindingGroup = nil);
  end;


implementation

procedure RegisterDefaultConverters();
begin
  TDataBindManager.RegisterConverter(bctEnumFromString,
    function(AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter
    begin
      Result := TEnumConverter.Create;
    end );

  TDataBindManager.RegisterConverter(bctListFromString,
    function(AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter
    begin
      Result := TListConverter.Create;
    end );

  TDataBindManager.RegisterConverter(bctVariantFromString,
    function(AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter
    begin
      Result := TVariantConverter.Create;
    end );

  TDataBindManager.RegisterConverter(bctDateTimeFirstDayOfTheMonthFromDateTime,
    function(AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter
    begin
      Result := TDateTimeFirstDayConverter.Create;
    end );
end;

{ BindAttribute }

constructor BindAttribute.Create(const ASourcePropertyName, ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverterType: Integer);
begin
  inherited Create;
  FSourcePropertyName := ASourcePropertyName;
  FTargetPropertyName := ATargetPropertyName;
  FBindingMode := ABindingMode;
  FConverterType := TBindConverterType(AConverterType);
end;


function BindAttribute.GetConverter(ASource, ATarget: TObject): IValueConverter;
begin
  Result := nil;
  if FConverterType <> bctNone then
    Result := TDataBindManager.GetConverter(FConverterType, Self, ASource, ATarget);
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

class function TDataBindManager.AddBinding(ASource: TObject; ASourcePropertyName: string;
  ATarget: TObject; ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverter: IValueConverter): TBinding;
begin
  Result := FBinder.AddBinding(ASource, ASourcePropertyName, ATarget, ATargetPropertyName, ABindingMode, AConverter);
end;

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
  LBinder: TBindingGroup;
begin
  Assert(Assigned(ABindableView), 'ABindableView must be assigned');
  Assert(Assigned(ASource), 'ASource must be assigned');
  //Assert(Assigned(ABinder), 'ABinder must be assigned');
  LBinder := ABinder;
  if not Assigned(LBinder) then
    LBinder := FBinder;

  rTypeView := TRttiContext.Create.GetType(ABindableView.ClassInfo);

  //enumerate properties and fields
  for rField in rTypeView.GetFields do
  begin
    LAttr := GetBinderAttribute(rField);
    if Assigned(LAttr) then
    begin
      LVal := rField.GetValue(ABindableView);
      Assert(LVal.IsObject, 'Bindable property must be TObject');
      LBinding := LBinder.AddBinding(ASource, LAttr.GetSourcePropertyNameDef(rField.Name), LVal.AsObject,
        LAttr.GetTargetPropertyNameDef('Text'), LAttr.BindingMode, LAttr.GetConverter(ASource, LVal.AsObject));
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
      LBinding := LBinder.AddBinding(ASource, LAttr.GetSourcePropertyNameDef(rProp.Name), LVal.AsObject,
        LAttr.GetTargetPropertyNameDef('Text'), LAttr.BindingMode, LAttr.GetConverter(ASource, LVal.AsObject));
      //if source object's property is not managed, then assign event on which it will be freed
      AddSourceNotification(LAttr.GetSourcePropertyNameDef(rProp.Name), LBinding, ASource);
    end;
  end;
end;

class constructor TDataBindManager.Create;
begin
  FBinder := TBindingGroup.Create(nil);
  FConverters := TDictionary<TBindConverterType,TGetInstanceFunc>.Create();
end;

class destructor TDataBindManager.Destroy;
begin
  FBinder.Free;
  FConverters.Free;
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

class function TDataBindManager.GetBindingForTarget(ATarget: TObject): TBinding;
begin
  Result := FBinder.GetBindingForTarget(ATarget);
end;

class function TDataBindManager.GetConverter(AKey: TBindConverterType;
  AAtribute: BindAttribute; ASource, ATarget: TObject): IValueConverter;
var
  LResult: TGetInstanceFunc;
begin
  if FConverters.TryGetValue(AKey, LResult) then
  begin
    Result := LResult(AAtribute, ASource, ATarget);
  end
  else
    raise Exception.Create('Converter not registered');
end;

class function TDataBindManager.GetPropertyValueByName(AObject: TObject;
  const APropertyname: string): TValue;
var
  LType: TRttiType;
  LField: TRttiField;
  LProp: TRttiProperty;
begin
  Result := TValue.Empty;
  LType := TRttiContext.Create.GetType(AObject.ClassInfo);
  LField := LType.GetField(APropertyname);
  if Assigned(LField) then
  begin
    Result := LField.GetValue(AObject);
    Exit;
  end;

  LProp := LType.GetProperty(APropertyname);
  if Assigned(LProp) then
  begin
    Result := LProp.GetValue(AObject);
    Exit;
  end;
end;

class procedure TDataBindManager.RegisterConverter(AKey: TBindConverterType;
  AGetInstanceFunc: TGetInstanceFunc);
begin
  FConverters.AddOrSetValue(AKey, AGetInstanceFunc);
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

class procedure TDataBindManager.UpdateSources;
begin
  FBinder.UpdateSources();
end;

class procedure TDataBindManager.UpdateTargets;
begin
  FBinder.UpdateTargets();
end;

{ BindExpressionAttribute }

constructor BindExpressionAttribute.Create(const ASourcePropertyName, ATargetPropertyName,
  ASourceToTargetExpression, ATargetToSourceExpression: string; ABindingMode: TBindingMode);
begin
  inherited Create(ASourcePropertyName, ATargetPropertyName, ABindingMode, Ord(bctDWScriptExpression));
  FSourceExpression := ASourceToTargetExpression;
  FTargetExpression := ATargetToSourceExpression;
end;

{ BindEventAttribute }

constructor BindEventAttribute.Create(const AControlName, AEventName: string);
begin
  inherited Create();
  ControlName := AControlName;
  EventName := AEventName;
end;

initialization
  RegisterDefaultConverters();


end.
