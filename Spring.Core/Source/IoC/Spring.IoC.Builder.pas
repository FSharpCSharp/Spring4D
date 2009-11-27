{***************************************************************************}
{                                                                           }
{           Delphi Spring Framework                                         }
{                                                                           }
{           Copyright (C) 2009-2010 Delphi Spring Framework                 }
{                                                                           }
{           http://delphi-spring-framework.googlecode.com                   }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.IoC.Builder;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Generics.Collections,
  Spring.System,
  Spring.IoC.Core;

type
  TComponentBuilder = class(TInterfacedObject, IComponentBuilder)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
    fInspectors: TList<IBuilderInspector>;
  public
    constructor Create(context: IContainerContext; registry: IComponentRegistry);
    destructor Destroy; override;
    procedure AddInspector(const inspector: IBuilderInspector);
    procedure RemoveInspector(const inspector: IBuilderInspector);
    procedure ClearInspectors;
    procedure Build(model: TComponentModel);
    procedure BuildAll;
  end;

  TInspectorBase = class abstract(TInterfacedObject, IBuilderInspector)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); virtual; abstract;
  public
    procedure ProcessModel(const context: IContainerContext; model: TComponentModel);
  end;

  TLifetimeInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TComponentActivatorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TInjectionInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TConstructorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TPropertyInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TMethodInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

  TFieldInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const context: IContainerContext; model: TComponentModel); override;
  end;

implementation

uses
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Reflection,
  Spring.IoC.Injection,
  Spring.IoC.ComponentActivator,
  Spring.Helpers;


{$REGION 'TComponentBuilder'}

constructor TComponentBuilder.Create(
  context: IContainerContext; registry: IComponentRegistry);
begin
  TArgument.CheckNotNull(context, 'context');
  TArgument.CheckNotNull(registry, 'registry');
  inherited Create;
  fContext := context;
  fRegistry := registry;
  fInspectors := TList<IBuilderInspector>.Create;
end;

destructor TComponentBuilder.Destroy;
begin
  fInspectors.Free;
  inherited Destroy;
end;

procedure TComponentBuilder.AddInspector(const inspector: IBuilderInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Add(inspector);
end;

procedure TComponentBuilder.RemoveInspector(const inspector: IBuilderInspector);
begin
  TArgument.CheckNotNull(inspector, 'inspector');
  fInspectors.Remove(inspector);
end;

procedure TComponentBuilder.ClearInspectors;
begin
  fInspectors.Clear;
end;

procedure TComponentBuilder.Build(model: TComponentModel);
var
  inspector: IBuilderInspector;
begin
  for inspector in fInspectors do
  begin
    inspector.ProcessModel(fContext, model);
  end;
end;

procedure TComponentBuilder.BuildAll;
var
  model: TComponentModel;
begin
  for model in fRegistry.FindAll do
  begin
    Build(model);
  end;
end;

{$ENDREGION}


{$REGION 'TInspectorBase'}

procedure TInspectorBase.ProcessModel(
  const context: IContainerContext; model: TComponentModel);
begin
  TArgument.CheckNotNull(context, 'context');
  TArgument.CheckNotNull(model, 'model');
  DoProcessModel(context, model);
end;

{$ENDREGION}


{$REGION 'TLifetimeInspector'}

procedure TLifetimeInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  attribute: TLifetimeAttributeBase;
begin
  if model.LifetimeManager <> nil then
  begin
    model.LifetimeType := ltCustom;
    Exit;
  end;
  if model.LifetimeType = ltUnknown then
  begin
    if model.ComponentType.TryGetCustomAttribute<TLifetimeAttributeBase>(attribute) then
    begin
      model.LifetimeType := attribute.LifetimeType;
    end
    else
    begin
      model.LifetimeType := ltTransient;
    end;
  end;
  model.LifetimeManager := context.CreateLifetimeManager(model);
end;

{$ENDREGION}


{$REGION 'TConstructorInspector'}

procedure TConstructorInspector.DoProcessModel(
  const context: IContainerContext; model: TComponentModel);
var
  predicate: TPredicate<TRttiMethod>;
  injection: IInjection;
  method: TRttiMethod;
begin
  if not model.ConstructorInjections.IsEmpty then Exit;  // TEMP
  predicate := TMethodFilters.IsConstructor and
    not TMethodFilters.HasParameterFlags([pfVar, pfOut]);
  for method in model.ComponentType.GetMethods.Where(predicate) do
  begin
    injection := context.InjectionFactory.CreateConstructorInjection(model);
    injection.Initialize(method);
    model.ConstructorInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TMethodInspector'}

procedure TMethodInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  condition: TPredicate<TRttiMethod>;
  method: TRttiMethod;
  injection: IInjection;
begin
  condition := TMethodFilters.IsInstanceMethod and
    TMethodFilters.HasAttribute(InjectionAttribute) and
    not TMethodFilters.HasParameterFlags([pfOut, pfVar]);
  for method in model.ComponentType.GetMethods.Where(condition) do
  begin
    injection := context.InjectionFactory.CreateMethodInjection(model);
    injection.Initialize(method);
    model.MethodInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TPropertyInspector'}

procedure TPropertyInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  condition: TPredicate<TRttiProperty>;
  propertyMember: TRttiProperty;
  injection: IInjection;
begin
  condition := TPropertyFilters.IsInvokable and
    TPropertyFilters.HasAttribute(InjectionAttribute);
  for propertyMember in model.ComponentType.GetProperties.Where(condition) do
  begin
    injection := context.InjectionFactory.CreatePropertyInjection(model);
    injection.Initialize(propertyMember);
    model.PropertyInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TFieldInspector'}

procedure TFieldInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  condition: TPredicate<TRttiField>;
  field: TRttiField;
  injection: IInjection;
begin
  condition := TFieldFilters.HasAttribute(InjectionAttribute);
  for field in model.ComponentType.GetFields.Where(condition) do
  begin
    injection := context.InjectionFactory.CreateFieldInjection(model);
    injection.Initialize(field);
    model.FieldInjections.Add(injection);
  end;
end;

{$ENDREGION}


{$REGION 'TComponentActivatorInspector'}

procedure TComponentActivatorInspector.DoProcessModel(
  const context: IContainerContext; model: TComponentModel);
begin
  if model.ComponentActivator = nil then
  begin
    if not Assigned(model.ActivatorDelegate) then
    begin
      model.ComponentActivator := TReflectionComponentActivator.Create(model, context.DependencyResolver);
    end
    else
    begin
      model.ComponentActivator := TDelegateComponentActivator.Create(model);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TInjectionInspector'}

{ TODO: TInjectionInspector.DoProcessModel }
procedure TInjectionInspector.DoProcessModel(const context: IContainerContext;
  model: TComponentModel);
var
  injection: IInjection;
  injectionInfo: TInjectionInfo;
  arguments: TArray<TValue>;
//  argument: TValue;
  method: TRttiMethod;
//  member: TRttiMember;
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
  matched: Boolean;
  i: Integer;
begin
  for injection in model.ConstructorInjections do
  begin
    if not injection.HasTarget then
    begin
      if model.InjectionArguments.TryGetValue(injection, injectionInfo) then
      begin
        arguments := injectionInfo.Arguments;
      end
      else
      begin
        SetLength(arguments, 0);
      end;
      matched := False;
      for method in model.ComponentType.GetMethods do
      begin
        if not method.IsConstructor then Continue;
        parameters := method.GetParameters;
        matched := Length(parameters) = Length(arguments);
        if matched then
        begin
          for i := 0 to Length(parameters) - 1 do
          begin
            parameter := parameters[i];
            if parameter.ParamType.IsClassOrInterface then
            begin
              matched := arguments[i].IsType<string>;
              matched := matched and context.HasService(parameter.ParamType.Handle, arguments[i].AsString);
            end
            else
            begin
              matched := arguments[i].IsType(parameter.ParamType.Handle);
            end;
          end;
          if matched then
          begin
            injection.Initialize(method);
            Break;
          end;
        end;
      end;
      if not matched then
      begin
        raise EContainerException.Create('Injection is invalid.');
      end;
    end;
  end;

  for injection in model.MethodInjections do
  begin
    if not injection.HasTarget then
    begin
      if model.InjectionArguments.TryGetValue(injection, injectionInfo) then
      begin
        arguments := injectionInfo.Arguments;
      end
      else
      begin
        SetLength(arguments, 0);
      end;
      matched := False;
      for method in model.ComponentType.GetMethods do
      begin
        if not SameText(method.Name, injectionInfo.TargetName) then
        begin
          Continue;
        end;
        parameters := method.GetParameters;
        matched := Length(parameters) = Length(arguments);
        if matched then
        begin
          for i := 0 to Length(parameters) - 1 do
          begin
            parameter := parameters[i];
            if parameter.ParamType.IsClassOrInterface then
            begin
              matched := arguments[i].IsType<string>;
              matched := matched and context.HasService(parameter.ParamType.Handle, arguments[i].AsString);
            end
            else
            begin
              matched := arguments[i].IsType(parameter.ParamType.Handle);
            end;
          end;
          if matched then
          begin
            injection.Initialize(method);
            Break;
          end;
        end;
      end;
      if not matched then
      begin
        raise EContainerException.Create('Injection is invalid.');
      end;
    end;
  end;
  { TODO: Handle Property & Field Injections }
end;

{$ENDREGION}

end.
