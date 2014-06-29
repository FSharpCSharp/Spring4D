(* MVC Controller.pas
* Created: 2012-11-07 18:23:01
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
unit Controller.Base;

interface

uses
  Rtti
  ,Generics.Collections
  ,DSharp.Bindings
  ,DSharp.Core.DataConversion
  ;

type
  Initializable = interface
    ['{FB868AF7-D9EB-4567-9946-5A010CAFAE0E}']
    procedure Initialize();
  end;

  IController<TModel: class> = interface
    ['{AD4951E2-9AE0-4D45-A11F-BD4C70F19349}']
    function GetModel: TModel;
    function GetAutoFreeModel: Boolean;
    procedure SetAutoFreeModel(const Value: Boolean);
    property AutoFreeModel: Boolean read GetAutoFreeModel write SetAutoFreeModel;
    property Model: TModel read GetModel;
  end;

  TCreateViewFunc<TModel: class> = reference to function: IController<TModel>;

  ///	<summary>
  ///	  Base controller class  
  ///	</summary>
  ///	<typeparam name="TModel">
  ///	  Model class type
  ///	</typeparam>
  ///	<remarks>
  ///	  Controller automatically injects it's fields (marked with Bind
  ///	  attributes) with the same fields from the View. Names of these fields
  ///	  must match.
  ///	</remarks>
  TBaseController<TModel: class> = class(TInterfacedObject, Initializable, IController<TModel>)
  private
    FModel: TModel;
    FView: TObject;
    FAutoFreeModel: Boolean;
    function GetModel: TModel;
    function GetAutoFreeModel: Boolean;
    procedure SetAutoFreeModel(const Value: Boolean);
  protected
    function GetViewComponent(const AComponentName: string): TValue; virtual;
    procedure InjectViewProperties(); virtual;
    procedure BindMethods(); virtual;
    /// <remarks>
    /// Descendants must override and write initialization code here
    /// </remarks>
    procedure Initialize(); virtual; abstract;
  public
    constructor Create(AModel: TModel; AView: TObject); virtual;
    destructor Destroy; override;

    function AddBinding(ASource: TObject = nil; ASourcePropertyName: string = '';
      ATarget: TObject = nil; ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault;
      AConverter: IValueConverter = nil): TBinding;
    function GetBindingForTarget(ATarget: TObject): TBinding;

    procedure UpdateTargets();
    procedure UpdateSources();

    property AutoFreeModel: Boolean read GetAutoFreeModel write SetAutoFreeModel;

    property Model: TModel read GetModel;
    property View: TObject read FView;
  end;

  TControllerFactory<TModel: class> = class
  private
    class var FControllers: TDictionary<TClass,TCreateViewFunc<TModel>>;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterFactoryMethod(AViewClass: TClass; const AMethod: TCreateViewFunc<TModel>);
    class function GetInstance(AViewClass: TClass): IController<TModel>;
  end;

implementation

uses
  SvBindings
  ,SysUtils
  ,TypInfo
  ;

type
  TControllerFactoryException = Exception;

{ TBaseController<TModel> }

function TBaseController<TModel>.AddBinding(ASource: TObject; ASourcePropertyName: string;
  ATarget: TObject; ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverter: IValueConverter): TBinding;
begin
  Result := TDataBindManager.AddBinding(ASource, ASourcePropertyName, ATarget, ATargetPropertyName, ABindingMode, AConverter);
end;

procedure TBaseController<TModel>.BindMethods;
var
  LMethod: TRttiMethod;
  LControlProp: TRttiProperty;
  LType: TRttiType;
  LAttr: BindAttribute;
  LMethodAttr: BindEventAttribute;
  LVal: TValue;
  LNewMethod: TMethod;
  LNewValue: TValue;
begin
  LType := TRttiContext.Create.GetType(Self.ClassInfo);
  for LMethod in LType.GetMethods do
  begin
    LAttr := TDataBindManager.GetBinderAttribute(LMethod);
    if Assigned(LAttr) and (LAttr is BindEventAttribute) then
    begin
      LMethodAttr := BindEventAttribute(LAttr);
      LVal := TDataBindManager.GetPropertyValueByName(Self, LMethodAttr.ControlName);
      if LVal.IsObject then
      begin
        LControlProp := TRttiContext.Create.GetType(LVal.TypeInfo).GetProperty(LMethodAttr.EventName);
        if Assigned(LControlProp) then
        begin
          LNewMethod.Code := LMethod.CodeAddress;
          LNewMethod.Data := Self;
          TValue.Make(@LNewMethod, LControlProp.PropertyType.Handle, LNewValue);
          LControlProp.SetValue(LVal.AsObject, LNewValue);
        end;
      end;
    end;
  end;
end;

constructor TBaseController<TModel>.Create(AModel: TModel; AView: TObject);
begin
  inherited Create();
  FModel := AModel;
  FView := AView;
  InjectViewProperties();
  Initialize();
  TDataBindManager.BindView(Self, AModel);
  BindMethods();
end;

destructor TBaseController<TModel>.Destroy;
begin
  if FAutoFreeModel then
    FModel.Free;

  inherited Destroy;
end;

function TBaseController<TModel>.GetAutoFreeModel: Boolean;
begin
  Result := FAutoFreeModel;
end;

function TBaseController<TModel>.GetBindingForTarget(ATarget: TObject): TBinding;
begin
  Result := TDataBindManager.GetBindingForTarget(ATarget);
end;

function TBaseController<TModel>.GetModel: TModel;
begin
  Result := FModel;
end;

function TBaseController<TModel>.GetViewComponent(
  const AComponentName: string): TValue;
var
  LType: TRttiType;
  LField: TRttiField;
begin
  Result := TValue.Empty;

  LType := TRttiContext.Create.GetType(FView.ClassType);
  LField := LType.GetField(AComponentName);
  if Assigned(LField) then
  begin
    Result := LField.GetValue(FView);
  end;
end;

procedure TBaseController<TModel>.InjectViewProperties;
var
  LType: TRttiType;
  LField: TRttiField;
  LAttr: TCustomAttribute;
begin
  LType := TRttiContext.Create.GetType(Self.ClassType);
  for LField in LType.GetFields do
  begin
    for LAttr in LField.GetAttributes do
    begin
      if LAttr is BindAttribute then
      begin
        LField.SetValue(Self, GetViewComponent(LField.Name));
        Break;
      end;
    end;
  end;
end;

procedure TBaseController<TModel>.SetAutoFreeModel(const Value: Boolean);
begin
  FAutoFreeModel := Value;
end;

procedure TBaseController<TModel>.UpdateSources;
begin
  TDataBindManager.UpdateSources();
end;

procedure TBaseController<TModel>.UpdateTargets;
begin
  TDataBindManager.UpdateTargets();
end;

{ TControllerFactory }

class constructor TControllerFactory<TModel>.Create;
begin
  FControllers := TDictionary<TClass,TCreateViewFunc<TModel>>.Create();
end;

class destructor TControllerFactory<TModel>.Destroy;
begin
  FControllers.Free;
end;

class function TControllerFactory<TModel>.GetInstance(AViewClass: TClass): IController<TModel>;
var
  LResult: TCreateViewFunc<TModel>;
begin
  if not FControllers.TryGetValue(AViewClass, LResult) then
    raise TControllerFactoryException.CreateFmt('Controller class "%S" not registered', [AViewClass.ClassName]);

  Result := LResult();
end;

class procedure TControllerFactory<TModel>.RegisterFactoryMethod(
  AViewClass: TClass; const AMethod: TCreateViewFunc<TModel>);
begin
  FControllers.AddOrSetValue(AViewClass, AMethod);
end;

end.
