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

/// <summary>
/// The Data Binding of the Delphi Spring Framework provides a flexible and
/// consistent way to bind two bindable elements which could be a native or
/// complex property as well as a list or array.
/// </summary>
unit Spring.Binding;

interface

uses
  Classes,
  SysUtils,
//  Controls,
  DB,
//  DBCtrls,
  Rtti, TypInfo,
  Generics.Collections,
  Spring.System,
  Spring.Collections,
  Spring.Notifications;

type
  /// <summary>
  /// Defines the direction of the data flow between a binding source and target.
  /// </summary>
  TBindingMode = (
    /// <summary>
    /// Source -> Target (Read-Only)
    /// </summary>
    bmOneWayToTarget,
    /// <summary>
    /// Source <- Target
    /// </summary>
    bmOneWayToSource,
    /// <summary>
    /// Source <-> Target
    /// </summary>
    bmTwoWay
  );

  /// <summary>
  /// Defines when and how to update the binding source.
  /// </summary>
  TUpdateSourceTrigger = (
    /// <summary>
    /// The default value for most properties is usPropertyChanged,
    /// while some properties of controls have a default value of LostFocus.
    /// </summary>
    usDefault,
    /// <summary>
    /// Updates the binding source when the bound property of the binding
    /// target has been changed.
    /// </summary>
    usPropertyChanged,
    /// <summary>
    /// Updates the binding source when the target component lost focus.
    /// </summary>
    usLostFocus,
    /// <summary>
    /// Updates the binding source when the UpdateSource method of
    /// the binding was explicitly invoked.
    /// </summary>
    usExplicit
  );

  {$REGION 'Spring.System'}

  TNotifyEventHandler = reference to procedure (sender: TObject);

  /// <summary>
  /// IValueProvider
  /// </summary>
  IValueProvider = interface
    ['{392A1E2F-CCA1-4CBB-9306-29AA402927D6}']
  {$REGION 'Property Getters and Setters'}
    function GetValue: TValue;
    function GetValueType: PTypeInfo;
    function GetIsReadOnly: Boolean;
  {$ENDREGION}
    /// <summary>
    /// Sets the value of the provider.
    /// </summary>
    /// <exception cref="EInvalidOperation">Raised if the value is read-only.</exception>
    procedure SetValue(const value: TValue);
    /// <summary>
    /// Gets the value of the provider.
    /// </summary>
    property Value: TValue read GetValue;
    /// <summary>
    /// Gets the type info of the value.
    /// </summary>
    property ValueType: PTypeInfo read GetValueType;
    /// <summary>
    /// Gets a value that indicates whether the value is read-only.
    /// </summary>
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  /// <summary>
  /// INullable
  /// </summary>
  INullable = interface
    ['{D3BB1062-8B2C-4F65-A719-8EFDD8CC3364}']
  {$REGION 'Property Getters and Setters'}
    function GetIsNull: Boolean;
  {$ENDREGION}
    property IsNull: Boolean read GetIsNull;
  end;

//  IValueConverter = interface
//    ['{20793D22-60E4-4BDA-9BDA-E511719D454E}']
//    function ConvertFrom(const targetValue: TValue): TValue;
//    function ConvertTo(const sourceValue: TValue): TValue;
//  end;

  {$ENDREGION}


  /// <summary>
  /// IBindableList
  /// </summary>
  IBindableList = interface(IList<TValue>)
    ['{5E4FF122-11B0-4406-8D06-C04376A7E088}']

  end;

  /// <summary>
  /// Represents a bindable element.
  /// </summary>
  IBindable = interface(IValueProvider)
    ['{79DA2BB1-2763-4859-BBA8-5913FE49B265}']
  {$REGION 'Property Getters and Setters'}
    (*
    function GetBindingExpression: string;
    /// <summary>
    /// Gets the binding expression.
    /// </summary>
    property BindingExpression: string read GetBindingExpression;
    //*)
  {$ENDREGION}
    /// <summary>
    /// Returns the bindable element that was specified by the expression/path.
    /// </summary>
    /// <param name="expression">
    /// Specifies the path of the bindable element. It could be the name of a
    /// property, e.g. 'Name', 'Person.Address.City', 'DBEdit1' and 'Edit1.Text',
    /// as well as a list, array or enumerable.
    /// </param>
//    function GetChild(const expression: string): IBindable;
//    function GetValue(const instance: TValue; const expression: string): TValue;
    /// <summary>
    /// Notifies listeners that the bindable object has been changed.
    /// </summary>
//    procedure NotifyUpdated;
//    property HasParent: Boolean;
//    property Parent: IBindable;
//    property NotifyOnChanged: Boolean;  // NotifyOnChanged
//    property IsCollection: Boolean;
  end;

  /// <summary>
  /// Represents the link between a binding source and a bindable target.
  /// </summary>
  IBinding = interface(IInitializable)
    ['{156EE2E8-E475-414F-B77F-B0DF2D82D32B}']
  {$REGION 'Property Getters and Setters'}
    function GetSource: IBindable;
    function GetTarget: IBindable;
    function GetColumns: TStrings;
//    function GetMode: TBindingMode;
//    function GetUpdateSourceTrigger: TUpdateSourceTrigger;
//    procedure SetMode(const value: TBindingMode);
//    procedure SetUpdateSourceTrigger(const value: TUpdateSourceTrigger);
  {$ENDREGION}
    procedure UpdateSource;
    procedure UpdateTarget;
    property Source: IBindable read GetSource;
    property Target: IBindable read GetTarget;
    property Columns: TStrings read GetColumns;
//    property ValueConverter: IValueConverter;
    // SourceExpression, TargetExpression?
//    procedure NotifySourceUpdated;
//    procedure NotifyTargetUpdated;
//    property OnSourceUpdated: IDelegate<TNotifyEventHandler>;
//    property OnTargetUpdated: IDelegate<TNotifyEventHandler>;
//    property Mode: TBindingMode read GetMode write SetMode;
//    property UpdateSourceTrigger: TUpdateSourceTrigger read GetUpdateSourceTrigger write SetUpdateSourceTrigger;
  end;

  /// <summary>
  /// Defines the interface for a binding context.
  /// </summary>
  IBindingContext = interface
    ['{CE09224F-2720-4AA8-B67E-719FA9D1E8CD}']
  {$REGION 'Property Getters and Setters'}
    function GetDataSource: TValue;
    function GetIsActive: Boolean;
    function GetBindings: IList<IBinding>;
    procedure SetDataSource(const value: TValue);
    procedure SetIsActive(const value: Boolean);
  {$ENDREGION}
    function AddBinding(const targetExpression, sourceExpression: string): IBinding; overload;
    function FindComponent(const componentName: string): TComponent;
//    property Owner: TComponent read GetOwner;
    property DataSource: TValue read GetDataSource write SetDataSource;
//    property Mode: TBindingMode read GetMode write SetMode;
//    property UpdateSourceTrigger: TUpdateSourceTrigger read GetUpdateSourceTrigger write SetUpdateSourceTrigger;
    property Bindings: IList<IBinding> read GetBindings;
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

  IBindableFactory = interface
    ['{5198DF60-20C6-47F0-A84C-7DE545238BCE}']
    function CreateBindable(const context: IBindingContext; const expression: string): IBindable;
  end;

  /// <summary>
  /// ISupportUpdate
  /// </summary>
  ISupportUpdate = interface
    ['{C5054C83-A399-4274-9054-98AA8696C996}']
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  // IEditable ?

  (*
  /// <summary>
  /// Bindable Component (Visual Element).
  /// </summary>
  IBindableComponent = interface(IBindable)
    ['{514ED690-DB4A-4663-993F-794F0A3F26A3}']
  {$REGION 'Property Getters and Setters'}
    function GetComponent: TObject;
  {$ENDREGION}
    procedure SetIsReadOnly(value: Boolean);
    property Component: TObject read GetComponent;
    // TODO: ? Event Handlers (OnEnter/OnExit/OnLostFocus/OnChanged)
  end;

  /// <summary>
  /// Data-aware control.
  /// </summary>
  IDataAwareControl = interface(IBindableComponent)
    ['{C2EE962B-A459-428B-ACF4-B16093BED06C}']
  {$REGION 'Property Getters and Setters'}
    function GetDataSource: TDataSource;
    procedure SetDataSource(const value: TDataSource);
  {$ENDREGION}
    procedure UpdateSource;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;
  //*)

  /// <summary>
  /// TBindableMember
  /// </summary>
  TBindableMember = class(TInterfacedObject, IBindable, IValueProvider, IInitializable)
  private
    fInstance: TValue;
//    fElements: TArray<string>;
//    fElementTypes: TArray<TRttiType>;
    fPropertyMember: TRttiProperty;
    fBindingExpression: string;
    fIsInitialized: Boolean;
    function GetIsReadOnly: Boolean;
    function GetValue: TValue;
    function GetValueType: PTypeInfo;
    function GetTypeKind: TTypeKind;
    function GetBindingExpression: string;
  protected
    function GetPropertyMember: TRttiProperty;
    // BindableMember
    property PropertyMember: TRttiProperty read GetPropertyMember;
    property BindingExpression: string read GetBindingExpression;
  protected
    { IInitializable }
    procedure Initialize;
  public
    constructor Create(const instance: TValue; const bindingExpression: string);
    procedure SetValue(const value: TValue);
    function GetType: TRttiType;
    property Value: TValue read GetValue;
    property ValueType: PTypeInfo read GetValueType;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  /// <summary>
  /// TBinding
  /// </summary>
  /// <remarks>
  /// By default, the binding mode is inherited from the binding context.
  /// </remarks>
  TBinding = class(TInterfacedObject, IBinding, IInitializable)
  private
    fContext: Pointer; // Weak Reference
    fSource: IBindable;
    fTarget: IBindable;
    fMode: TNullable<TBindingMode>;
    fColumns: TStrings;
    fUpdateSourceTrigger: TNullable<TUpdateSourceTrigger>;
    fOnCollectionChanged: TCollectionChangedEventHandler;
    fOnPropertyChanged: TPropertyNotificationEventHandler;
    function GetSource: IBindable;
    function GetTarget: IBindable;
    function GetColumns: TStrings;
    function GetMode: TBindingMode;
    function GetUpdateSourceTrigger: TUpdateSourceTrigger;
    procedure SetMode(const value: TBindingMode);
    procedure SetUpdateSourceTrigger(const value: TUpdateSourceTrigger);
  protected
    procedure Initialize;
    procedure DoPropertyChanged(sender: TObject; const e: TPropertyNotificationEventArgs);
    procedure DoCollectionChanged(sender: TObject; const e: TCollectionChangedEventArgs);
    procedure UpdateValue(const source, target: IBindable); virtual;
  public
    constructor Create(const context: IBindingContext; const source, target: IBindable);
    destructor Destroy; override;
    procedure UpdateSource;
    procedure UpdateTarget;
    property Source: IBindable read GetSource;
    property Target: IBindable read GetTarget;
    property Columns: TStrings read GetColumns;
    property Mode: TBindingMode read GetMode write SetMode;
    property UpdateSourceTrigger: TUpdateSourceTrigger read GetUpdateSourceTrigger write SetUpdateSourceTrigger;
  end;
  
  /// <summary>
  /// Binding Context.
  /// </summary>
  TBindingContext = class(TInterfacedObject, IBindingContext)
  private
    fOwner: TComponent;
    fDataSource: TValue;
    fIsActive: Boolean;
    fBindings: IList<IBinding>;
    fMappings: IDictionary<string, IBinding>;
    fOnPropertyChanged: TPropertyNotificationEventHandler;
    function GetDataSource: TValue;
    function GetIsActive: Boolean;
    function GetBindings: IList<IBinding>;
    procedure SetIsActive(const value: Boolean);
    procedure SetDataSource(const value: TValue);
  protected
    procedure DoPropertyChanged(sender: TObject; const e: TPropertyNotificationEventArgs);
    procedure InitializeBindings; virtual;
  public
    constructor Create(owner: TComponent);
    destructor Destroy; override;
    function AddBinding(const sourceExpression: string; const target: TValue;
      const targetExpression: string): IBinding; overload;
    function AddBinding(const targetExpression, sourceExpression: string): IBinding; overload;
    function FindComponent(const componentName: string): TComponent;
    property DataSource: TValue read GetDataSource write SetDataSource;
    property Bindings: IList<IBinding> read GetBindings;
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

  (*
  TVclControlPropertyNotification = class(TPropertyNotification, IPropertyNotification)
  private
    fControl: TControl;
    fOldProc: TWndMethod;
  protected
    procedure WndProc(var message: TMessage); virtual;
  public
    constructor Create(control: TControl);
  end;
  //*)

resourcestring
  SInvalidBindingExpression = 'Invalid binding expression: "%s".';

implementation

uses
  Spring.Reflection,
  Spring.Helpers;

// Parse a string, for example: "A|B|C|D|E|F"?
// Result := Parse('|', "A|B|C|D|E|F") => Result = A, expression = B|C|D|E|F
// Result = B, expression = C|D|E|F
// ...
function ExtractNextElement(var expression: string; const delimiter: string): string;
var
  p: Integer;
begin
  p := Pos(delimiter, expression);
  if p = 0 then
  begin
    Exit(expression);
  end
  else
  begin
    Result := Copy(expression, 1, p-1);
    Delete(expression, 1, p);
  end;
end;

// TEMP
function ConvertToString(const value: TValue): string;
var
  v: TValue;
begin
  if value.IsEmpty then Exit('')
  else if value.IsObject then Exit(value.AsObject.ToString)
  else if value.TypeInfo.Name = 'TNullable<System.string>' then
  begin
    TValue.Make(value.GetReferenceToRawData, TypeInfo(TNullable<string>), v);
    Exit(v.AsType<TNullable<string>>.Value);
  end;
end;

{$REGION 'TBindableMember'}

constructor TBindableMember.Create(const instance: TValue;
  const bindingExpression: string);
begin
  inherited Create;
  fInstance := instance;
  fBindingExpression := bindingExpression;
end;

/// Explores Rtti information and initializes the instance.
procedure TBindableMember.Initialize;
var
  instanceType: TRttiType;
  names: TStringDynArray;
  name: string;
  i: Integer;
begin
  if not fIsInitialized then
  begin
    instanceType := TType.GetType(fInstance);
    names := SplitString(BindingExpression, ['.']);
    for i := 0 to Length(names) - 1 do
    begin
      name := names[i];
      fPropertyMember := instanceType.GetProperty(name);
      if fPropertyMember = nil then
      begin
        raise Exception.CreateResFmt(@SInvalidBindingExpression, [fBindingExpression]);
      end;
      if i < Length(names) - 1 then
      begin
        fInstance := fPropertyMember.GetValue(fInstance);
        instanceType := fPropertyMember.PropertyType;
      end;
    end;
//    fHasParent := fPropertyMember.Parent <> nil;
    fIsInitialized := True;
  end;
end;

function TBindableMember.GetIsReadOnly: Boolean;
begin
  Result := not PropertyMember.IsWritable;
end;

function TBindableMember.GetPropertyMember: TRttiProperty;
begin
  Initialize;
  Result := fPropertyMember;
end;

function TBindableMember.GetType: TRttiType;
begin
  Result := PropertyMember.PropertyType;
end;

function TBindableMember.GetTypeKind: TTypeKind;
begin
  Result := ValueType.Kind;
end;

function TBindableMember.GetBindingExpression: string;
begin
  Result := fBindingExpression;
end;

function TBindableMember.GetValue: TValue;
begin
  Result := PropertyMember.GetValue(fInstance);
end;

function TBindableMember.GetValueType: PTypeInfo;
begin
  Result := PropertyMember.PropertyType.Handle;
end;

procedure TBindableMember.SetValue(const value: TValue);
begin
  PropertyMember.SetValue(fInstance, value);
end;

{$ENDREGION}


{$REGION 'TBinding'}

constructor TBinding.Create(const context: IBindingContext; const source,
  target: IBindable);
begin
  inherited Create;
  fContext := Pointer(context);
  fSource := source;
  fTarget := target;
  fOnPropertyChanged := DoPropertyChanged;
  fOnCollectionChanged := DoCollectionChanged;
end;

destructor TBinding.Destroy;
begin
  fColumns.Free;
  inherited Destroy;
end;

procedure TBinding.DoCollectionChanged(sender: TObject;
  const e: TCollectionChangedEventArgs);
var
  strings: TStrings;
  i: Integer;
  obj: TObject;
  displayName: string;
begin
  if not Target.Value.TryAsType<TStrings>(strings) then Exit;
  obj := e.Item.AsObject;
  case e.Action of
    caAdd:
    begin
      if Columns.Count > 0 then  // IsNotEmpty
      begin
        displayName := ConvertToString(TType.GetType(obj).GetProperty(Columns[0]).GetValue(obj));
      end
      else
      begin
        displayName := obj.ToString;
      end;
      strings.InsertObject(e.Index, displayName, obj);
    end;
    caRemove:
    begin
      for i := 0 to strings.Count - 1 do
      begin
        if strings.Objects[i] = obj then
        begin
          strings.Delete(i);
          Break;
        end;
      end;
    end;
  end;
end;

procedure TBinding.DoPropertyChanged(sender: TObject;
  const e: TPropertyNotificationEventArgs);
var
  strings: TStrings;
  index: Integer;
  displayName: string;
begin
  if not Target.Value.TryAsType<TStrings>(strings) then Exit;
  if Columns.IndexOf(e.PropertyName) > -1 then
  begin
    index := strings.IndexOfObject(sender);
    if index > -1 then
    begin
      displayName := ConvertToString(e.NewValue);
      strings[index] := displayName;
    end;
  end;
end;

procedure TBinding.Initialize;
var
  notification: INotifyCollectionChanged;
  pn: ISupportPropertyNotification;
begin
  // BindingMode IsReadOnly, etc.
  if TryGetInterface(Source.Value, INotifyCollectionChanged, notification) then
  begin
    notification.OnCollectionChanged.AddHandler(fOnCollectionChanged);
  end;
  if TryGetInterface(Source.Value, ISupportPropertyNotification, pn) then
  begin
    pn.OnPropertyChanged.AddHandler(fOnPropertyChanged);
  end;
  UpdateTarget;
end;

procedure TBinding.UpdateSource;
begin
  UpdateValue(Target, Source);
end;

procedure TBinding.UpdateTarget;
begin
  UpdateValue(Source, Target);
end;

procedure TBinding.UpdateValue(const source, target: IBindable);
var
  v: TValue;
  obj: TObject;
  enumerable: IEnumerableAware;
  strings: TStrings;
  displayName: string;
begin
  // TEMP (Hard-Coded)  Support Nullable
  if source.Value.TryCast(target.ValueType, v) then
  begin
    target.SetValue(v);
  end
  else if (source.ValueType.Kind = tkInteger) and (target.ValueType.Kind = tkUString) then
  begin
    target.SetValue(IntToStr(source.Value.AsInteger)); // TEMP
  end
  else if (source.ValueType.Name = 'TNullable<System.string>') and
    (target.ValueType.Kind = tkUString) then
  begin
    TValue.Make(source.Value.GetReferenceToRawData, TypeInfo(TNullable<string>), v);
    target.SetValue(v.AsType<TNullable<string>>.Value);
  end
  else if (source.ValueType.Name = 'TNullable<System.Integer>') and
    (target.ValueType.Kind = tkUString) then
  begin
    TValue.Make(source.Value.GetReferenceToRawData, TypeInfo(TNullable<Integer>), v);
    target.SetValue(IntToStr(v.AsType<TNullable<Integer>>.Value));
  end
  else if TryGetInterface(source.Value, IEnumerableAware, enumerable) and
    target.Value.TryAsType<TStrings>(strings) then
  begin
    with strings do
    begin
      BeginUpdate;
      try
        for obj in enumerable.GetEnumerable do
        begin
          if Columns.Count > 0 then  // IsNotEmpty
          begin
            displayName := ConvertToString(TType.GetType(obj).GetProperty(Columns[0]).GetValue(obj));
          end
          else
          begin
            displayName := obj.ToString;
          end;
          AddObject(displayName, obj);
        end;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

function TBinding.GetSource: IBindable;
begin
  Result := fSource;
end;

function TBinding.GetTarget: IBindable;
begin
  Result := fTarget;
end;

function TBinding.GetColumns: TStrings;
begin
  if fColumns = nil then
  begin
    fColumns := TStringList.Create;
  end;
  Result := fColumns;
end;

function TBinding.GetMode: TBindingMode;
begin
  Result := fMode;
//  Result := IIf(fMode.HasValue, fMode, fContext.Mode);
//  if fMode.HasValue then
//    Result := fMode
//  else
//    Result := fContext.Mode;
end;

function TBinding.GetUpdateSourceTrigger: TUpdateSourceTrigger;
begin
  Result := fUpdateSourceTrigger;
end;

procedure TBinding.SetMode(const value: TBindingMode);
begin
  fMode := value;
end;

procedure TBinding.SetUpdateSourceTrigger(const value: TUpdateSourceTrigger);
begin
  fUpdateSourceTrigger := value;
end;

{$ENDREGION}


{$REGION 'TBindingContext'}

constructor TBindingContext.Create(owner: TComponent);
begin
  inherited Create;
  fOwner := owner;
  fMappings := TCollections.CreateDictionary<string, IBinding>;
  fOnPropertyChanged := DoPropertyChanged;
end;

destructor TBindingContext.Destroy;
begin
  if fBindings <> nil then
  begin
    fBindings.Clear;
//    fCollectionNotifications.Clear;
    fMappings.Clear;
  end;
  inherited Destroy;
end;

procedure TBindingContext.DoPropertyChanged(sender: TObject;
  const e: TPropertyNotificationEventArgs);
var
  binding: IBinding;
begin
  if fMappings.TryGetValue(e.PropertyName, binding) then
  begin
    binding.UpdateTarget;
  end;
end;

procedure TBindingContext.InitializeBindings;
var
  binding: IBinding;
begin
  for binding in Bindings do
  begin
    binding.Initialize;
  end;
end;

function TBindingContext.FindComponent(const componentName: string): TComponent;
begin
  if fOwner <> nil then
    Result := fOwner.FindComponent(componentName)
  else
    Result := nil;
end;

function TBindingContext.AddBinding(const sourceExpression: string;
  const target: TValue; const targetExpression: string): IBinding;
var
  bindingSource: IBindable;
  bindingTarget: IBindable;
begin
  bindingSource := TBindableMember.Create({Self} fDataSource, sourceExpression);
  bindingTarget := TBindableMember.Create(target, targetExpression);
  Result := TBinding.Create(Self, bindingSource, bindingTarget);
  Bindings.Add(Result);
  fMappings.Add(sourceExpression, Result);
end;

function TBindingContext.AddBinding(const targetExpression,
  sourceExpression: string): IBinding;
var
  target: TComponent;
  targetName: string;
  expression: string;
begin
  expression := targetExpression;
  targetName := ExtractNextElement(expression, '.');
  target := fOwner.FindComponent(targetName);
  Result := AddBinding(sourceExpression, target, expression);
end;

function TBindingContext.GetBindings: IList<IBinding>;
begin
  if fBindings = nil then
  begin
    fBindings := TCollections.CreateList<IBinding>;
  end;
  Result := fBindings;
end;

function TBindingContext.GetDataSource: TValue;
begin
  Result := fDataSource;
end;

function TBindingContext.GetIsActive: Boolean;
begin
  Result := fIsActive;
end;

procedure TBindingContext.SetDataSource(const value: TValue);
var
  notification: IPropertyNotification;
begin
  if TryGetInterface(fDataSource, IPropertyNotification, notification) then
  begin
    notification.OnPropertyChanged.RemoveHandler(fOnPropertyChanged);
  end;
  fDataSource := value;
  if TryGetInterface(fDataSource, IPropertyNotification, notification) then
  begin
    notification.OnPropertyChanged.AddHandler(fOnPropertyChanged);
  end;
end;

procedure TBindingContext.SetIsActive(const value: Boolean);
begin
  if fIsActive <> value then
  begin
    InitializeBindings;
    fIsActive := value;
  end;
end;

{$ENDREGION}

end.
