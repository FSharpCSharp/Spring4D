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

unit Spring.Binding;

interface

uses
  Classes,
  Controls,
  DB,
  DBCtrls,
  Rtti,
  Generics.Collections,
  Spring.System,
  Spring.Notifications,
  Spring.Binding.DataList;

type
  // Interfaces //
  IBindable = interface;

  IBinding = interface
    ['{3451A90A-0D16-4C9B-9361-109E6F65A7C0}']
    procedure SetBindable(bindable: IBindable);
    function GetBindable: IBindable;
    procedure SetObjectToPresent(objectToPresent: TValue);
    function GetObjectToPresent: TValue;
    property Bindable: IBindable read GetBindable write SetBindable;
    property ObjectToPresent: TValue read GetObjectToPresent write SetObjectToPresent;
  end;

  IDataProvider = interface
    ['{F8F18742-15E2-47CE-AB54-2F10196227E6}']
    function GetDataSource: TDataSource;
    function GetDataSet: TObjectDataSet;
    function GetObjectToPresent: IDataHolder;
    property DataSource: TDataSource read GetDataSource;
    property DataSet: TObjectDataSet read GetDataSet;
    property ObjectToPresent: IDataHolder read GetObjectToPresent;
  end;

  IBindable = interface
    ['{99A3C0DD-B12D-417C-BD6B-DBECFC9103B8}']
    procedure SetExpression(expression: string);
    function GetExpression: string;
    procedure SetComponent(component: TComponent);
    function GetComponent: TComponent;
    property Component: TComponent read GetComponent write SetComponent;
    property Expression: string read GetExpression write SetExpression;
  end;

  IDatawareBindable = interface(IBindable)
    ['{01E453FC-402F-42A0-A211-E3F0C7D5D3F8}']
    procedure SetDataProvider(dataProvider: IDataProvider);
    function GetDataProvider: IDataProvider;
    property DataProvider: IDataProvider read GetDataProvider write SetDataProvider;
  end;

  // Classes //
  TBinding = class(TInterfacedObject, IBinding)
  private
    fBindable: IBindable;
    fObjectToPresent: TValue;
  protected
    procedure SetBindable(bindable: IBindable);
    function GetBindable: IBindable;
    procedure SetObjectToPresent(objectToPresent: TValue);
    function GetObjectToPresent: TValue;
    property Bindable: IBindable read GetBindable write SetBindable;
  end;

  TBindable = class abstract(TNotifiableObject, IBindable)
  private
    fComponent: TComponent;
    fExpression: string;
  protected
    procedure SetExpression(expression: string);
    function GetExpression: string;
    procedure SetComponent(component: TComponent);
    function GetComponent: TComponent;
    property Component: TComponent read GetComponent write SetComponent;
    property Expression: string read GetExpression write SetExpression;
  end;

  TBindableClass = class of TBindable;

  TDatawareBindable = class(TBindable, IDatawareBindable)
  private
    fDataProvider: IDataProvider;
  protected
    procedure SetDataProvider(dataProvider: IDataProvider); virtual;
    function GetDataProvider: IDataProvider;
  end;

  /// <summary>
  ///  Keeps a reference to the object or collection to be presented
  /// </summary>
  /// <remarks>
  ///  This class contains a custom object dataset able to understand objects
  ///  and object collections. When a binding is made between a control
  ///  and Dataware an attribute, the attribute name is placed in the
  ///  DataField of control and its datasource points to a
  ///  DataProvider.Datasource.
  ///  Only a DataProvider is created for each object or object collection.  ///
  ///  If typeinfo is passed then the model is a collection, in this
  ///  case the dataset's columns will be created based on this typeinfo.
  ///  Otherwise mount the fields based on the attributes of the
  ///  ObjectModel itself.
  /// </remarks>
  TDataProvider = class(TInterfacedObject, IDataProvider)
  private
    fDataSource: TDataSource;
    fDataSet: TObjectDataSet;
  protected
    function GetDataSource: TDataSource;
    function GetDataSet: TObjectDataSet;
    function GetObjectToPresent: IDataHolder;
  public
    constructor Create(objectToPresent: TObject; typeInfo: TRttiType = nil);
    destructor Destroy; override;
    property DataSource: TDataSource read GetDataSource;
    property DataSet: TObjectDataSet read GetDataSet;
    property ObjectToPresent: IDataHolder read GetObjectToPresent;
  end;

  IBinder = interface
    ['{DFB4B858-5DAF-484C-87AF-E19E22A5C143}']
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function Add(const TargetExpression, SourceExpression: string): IBinding;
    property Active: Boolean read GetActive write SetActive;
  end;

  TBinder = class(TInterfacedObject, IBinder)
  private
    fActive: Boolean;
    fObjectToPresent: TObject;
    fGuiForm: TComponent;
    fDataProviders: TDictionary<string, IDataProvider>;
    fBindings: TDictionary<string, TList<IBinding>>;
    function CreateBindable(component: TComponent; dataProvider: IDataProvider;
      propertyName: string): IBindable;
    function GetContext: TObject;
    function FindComponentOnGui(targetExpression: string): TComponent;
    function GetDataProvider(objectName: string;
      objectReference: TValue): IDataProvider;
    function GetBindable(controlClass: TClass): IBindable;
    function GetObjectFromExpression(obj: TValue;
      const expression: string; var leftPart, rightPart: string): TValue;
    function Parse(const delimiter: string; var stringToParse: string): string;
  protected
    function GetActive: Boolean;
    procedure SetActive(value: Boolean);
  public
    constructor Create(form: TComponent; data: TObject);
    destructor Destroy; override;
    function Add(const targetExpression, sourceExpression: string): IBinding;
    property Active: Boolean read GetActive write SetActive;
    property Context: TObject read GetContext;
  end;

function GetBindableMapping: TDictionary<TClass, TBindableClass>;

implementation

uses
  SysUtils,
  Spring.Helpers,
  Spring.DesignPatterns,
  Spring.Reflection,
  Spring.Core.ResourceStrings;

type
  /// <summary>
  /// Maintains a mapping between components and bindable classes.
  /// </summary>
  /// <remarks>
  /// This dictionary contains a mapping between visual components and
  /// bindable classes.
  /// The bindable are created based on the type of component.
  /// If the type of component has a bindable not related, bindable
  /// related to the nearest ancestor of the control is returned.
  /// ie:
  /// TDBEdit <=> TDatawareBindable;
  /// TDBcheckBox <=> TDatawareBindable;
  /// TDBImage <=> TDatawareBindable;
  /// TDBGrid => TDatawareGridBindable;
  /// TDBListBox => TDatawareListBoxBindable;
  /// </remarks>
  TBindableMapping = class
  private
    fDictionary: TDictionary<TClass, TBindableClass>;
  public
    constructor Create;
    property Dictionary: TDictionary<TClass, TBindableClass> read fDictionary;
  end;

var
  rttiCtx: TRttiContext;

function GetBindableMapping: TDictionary<TClass, TBindableClass>;
begin
  Result := TSingleton.GetInstance<TBindableMapping>.Dictionary;
end;

{ TBinder }

// form: The Gui Form with visual controls
// data: The object to be presented, generaly a ViewModel.
constructor TBinder.Create(form: TComponent; data: TObject);
begin
  inherited Create;
  fGuiForm := form;
  fObjectToPresent := data;
  fDataProviders := TDictionary<string, IDataProvider>.Create;
  fBindings := TDictionary<string, TList<IBinding>>.Create;
end;

destructor TBinder.Destroy;
begin
  fDataProviders.Free;
  fBindings.Free;
  inherited;
end;

function TBinder.CreateBindable(component: TComponent; dataProvider: IDataProvider;
  propertyName: string): IBindable;
var
  bindableClass: TBindableClass;
  datawareBindable: IDatawareBindable;
begin
  if GetBindableMapping.TryGetValue(component.ClassType, bindableClass) then
    Result := bindableClass.Create
  else
    raise Exception.Create(SNoThereIsNoSuchMapping);
  Result.Component := component;
  Result.Expression := propertyName;
  if Supports(Result, IDatawareBindable, datawareBindable) then
    datawareBindable.DataProvider := dataProvider;
end;

{
  ex.:
  Binder.Add('DBedit1', 'Id'); => Person.Id
  Binder.Add('DBedit2', 'Name'); => Person.Name
  Binder.Add('DBedit3', 'Address.City'); => Person.Address.City
}
function TBinder.Add(const targetExpression, sourceExpression: string): IBinding;
var
  component: TComponent;
  dataProvider: IDataProvider;
  targetBindable: IBindable;
  propertyName, objectName: string;
  objectReference: TValue;
begin
  // find component on FView and raise exception if not found control
  component := FindComponentOnGui(TargetExpression);

  // Get the ObjectName from SourceExpression, find this ObjectName in fDataProviders,
  // if not found create a new DataProvider and add it to fDataProviders by key ObjectName,
  // split SourceExpression and introduce PropertyName which is ObjectModel
  objectReference := GetObjectFromExpression(Context, sourceExpression,
    objectName, propertyName);
  if objectName = EmptyStr then
    objectname := Context.ClassName;

  dataProvider := GetDataProvider(objectName, objectReference);

  // set datasource of control, set propertyname on datafield of control
  // raise exception if bindable couldn't be created
  targetBindable := CreateBindable(component, dataProvider, propertyName);

  // *** need create binidng and put 2 Bindables or 1 DatawareBindable
end;

function TBinder.GetBindable(controlClass: TClass): IBindable;
var
  bindableClass: TBindableClass;
begin
  if not Assigned(controlClass) then
    Result := nil
  else
  begin
    if not GetBindableMapping.TryGetValue(controlClass,
      bindableClass) then
      Result := GetBindable(controlClass.ClassParent);
  end;
end;

function TBinder.GetContext: TObject;
begin
  Result := fObjectToPresent;
end;

//  FindComponentOnView('DBEdit1');
function TBinder.FindComponentOnGui(targetExpression: string): TComponent;
begin
  Result := fGuiForm.FindComponent(TargetExpression);
  if not Assigned(Result) then
    raise Exception.Create(Format(SNoSuchComponent, [TargetExpression]));
end;

// Binder.Add(DBEdit1, Person.ID)    // create a =>  ['person', (datasource, dataset, referencetopersonobject)]
// Binder.Add(DBEdit2, Person.Name)  // use same dataprovider a.dataset on dbedit2
// now if we have
// Binder.Add(DBEdit3, Person.Address.CityName) // create b =>  ['person.address', (datasource, dataset, referencetopersonadressobject)]
function TBinder.GetDataProvider(objectName: string;
  objectReference: TValue): IDataProvider;
begin
  if not fDataProviders.TryGetValue(objectName, Result) then
  begin
    Result := TDataProvider.Create(objectReference.AsObject);
    fDataProviders.Add(objectName, Result);
  end;
end;

// Parse a string, for example: "A|B|C|D|E|F"?
// Result := Parse('|', "A|B|C|D|E|F") => Result = A, stringToParse = B|C|D|E|F
// Result = B, stringToParse = C|D|E|F
// ...
function TBinder.Parse(const delimiter: string;
  var stringToParse: string): string;
begin
  if Copy(stringToParse, Length(stringToParse) - 1) <> delimiter then
    stringToParse := stringToParse + delimiter;
  Result := Copy(stringToParse, 0, Pos(delimiter, stringToParse) - 1);
  stringToParse := Copy(stringToParse, Pos(delimiter, stringToParse) + 1, Length(stringToParse)-1);
end;

// this routine return the context or the object of property defined on
// expression. when finish leftPart contain fullpath to object
// and rightPart have the property name.
function TBinder.GetObjectFromExpression(obj: TValue;
  const expression: string; var leftPart, rightPart: string): TValue;
var
  first, stringToParse: string;
  memberType: TRttiType;
  memberValue: TValue;
begin
  if expression = EmptyStr then
    // *** define a constant here
    raise Exception.Create('SouceExpression not defined');
  stringToParse := expression;
  first := Parse('.', stringToParse);
  if stringToParse = EmptyStr then
  begin
    rightPart := first;
    Result := Obj;
  end
  else
  begin
    if LeftPart <> EmptyStr then
      leftPart := leftPart + '.' + first
    else
      leftPart := first;
    memberType := rttiCtx.GetType(Obj.TypeInfo);
    memberValue := memberType.GetProperty(first).GetValue(obj.AsObject);
    Result := GetObjectFromExpression(
      memberValue, stringToParse, leftPart, rightPart);
  end;
end;

function TBinder.GetActive: Boolean;
begin
  Result := fActive;
end;

procedure TBinder.SetActive(Value: Boolean);
var
  provider: IDataProvider;
begin
  for provider in fDataProviders.Values do
    provider.DataSet.Active := True;
end;

{ TDataProvider }

constructor TDataProvider.Create(objectToPresent: TObject; typeInfo: TRttiType = nil);
var
  objectDataHolder: IDataHolder;
begin
  fDataSet := TObjectDataSet.Create(nil);
  fDataSource := TDataSource.Create(nil);
  fDataSource.DataSet := fDataSet;
  if not Supports(objectToPresent, IDataHolder, objectDataHolder) then
  begin
    objectDataHolder := TNotifiableObject.Create;
    objectDataHolder.SetObject(objectToPresent);
  end;
  (fDataSet as TObjectDataset).DataHolder := objectDataHolder;
end;

destructor TDataProvider.Destroy;
begin
  fDataSource.Free;
  fDataSet.Free;
  inherited;
end;

function TDataProvider.GetDataSet: TObjectDataSet;
begin
  Result := fDataSet;
end;

function TDataProvider.GetDataSource: TDataSource;
begin
  Result := fDataSource;
end;

function TDataProvider.GetObjectToPresent: IDataHolder;
begin
  Result := fDataSet.DataHolder;
end;

{ TBindable }

function TBindable.GetComponent: TComponent;
begin
  Result := fComponent;
end;

function TBindable.GetExpression: string;
begin
  Result := fExpression;
end;

procedure TBindable.SetComponent(component: TComponent);
begin
  fComponent := component;
end;

procedure TBindable.SetExpression(expression: string);
begin
  fExpression := expression;
end;

{ TDatawareBindable }

function TDatawareBindable.GetDataProvider: IDataProvider;
begin
  Result := fDataProvider;
end;

procedure TDatawareBindable.SetDataProvider(dataProvider: IDataProvider);
var
  componentType: TRttiType;
  memberProperty: TRttiProperty;
begin
  fDataProvider := dataProvider;
  componentType := rttiCtx.GetType(Component.ClassType);
  memberProperty := componentType.GetProperty('DataField');
  if Assigned(memberProperty) then
    memberProperty.SetValue(Component, TValue.From<WideString>(Expression));
  memberProperty := componentType.GetProperty('DataSource');
  if Assigned(memberProperty) then
    memberProperty.SetValue(Component, fDataProvider.DataSource);
end;

{ TBinding }

function TBinding.GetBindable: IBindable;
begin
  Result := fBindable;
end;

function TBinding.GetObjectToPresent: TValue;
begin
  Result := fObjectToPresent;
end;

procedure TBinding.SetBindable(bindable: IBindable);
begin
  fBindable := bindable;
end;

procedure TBinding.SetObjectToPresent(objectToPresent: TValue);
begin
  fObjectToPresent := objectToPresent;
end;

{ TBindableMapping }

constructor TBindableMapping.Create;
begin
  fDictionary := TDictionary<TClass, TBindableClass>.Create;
end;

initialization
  rttiCtx.Create;

finalization
  rttiCtx.Free; { do not remove }

end.
