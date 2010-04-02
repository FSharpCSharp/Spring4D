=====================================================================================
Delphi Spring Framework - Binding
=====================================================================================

1. Overview
-----------

The Data Binding of the Delphi Spring Framework provides a flexible and consistent way 
to bind two bindable elements. A bindable element wrap:

- a primitive-type
- a complex type
- a list or array
- a property of a component visual
- a dataware component
- a non dataware componente visual

The Data Binding provides a way for developers to create a read/write link between the 
viasual controls on a form and the data in their application (their data model).

2. Objectives
-------------

- Support to binding between dataware controls and any objects or list;
- Support to binding between non dbaware controls such as Edit, List, ComboBox, etc 
  and any objects or list;
- Supports standard VCL controls;
- Supports Data-Aware controls;
- Supports UCL controls;
- BindingMode (OneWayToTarget, OneWayToSource, TwoWay);
- UpdateSourceTrigger (PropertyChanged, LostFocus, Explicit);
- List/Array/Enumerable;
- Property/Collection Changed Notifications;
- Flexible;
- Extensible;
- create a model-view-viewmodel pattern framework that use binding and separate gui 
  from gui logic;
- create a expert to binding controls to class on design-time;
- be fast, reliable and consistent;


3. Crucial elements
-------------------

There are several elements that are crucial for Spring Binding, the main are:

- Binder: kind of Manager that manages all of Bindings elements
- Binding: is a mediator between two Bindable items. Can be
  * Binding - binding to simple types
  * BindingList - binding to lists or arrays
  * BindingTable - binding to lists with columns
- Bindable: is one side of binding. It can be the Source (source of data) or 
  Target (where data is presented or replicated)
- DataProvider: holder object for DataSet and DataSource of any DBAwareControl 

//-------------------------------------------------------------------------------------------------------------------

REVISED TO HERE

//-------------------------------------------------------------------------------------------------------------------

3. User Stories

The following user stories may be modified depends on the requirements analysis.

    * A user can bind a primitive-type property (e.g. string, Integer, Boolean) to a data-aware control such as TDBEdit.

    * A user can bind a nullable property (such as TNullable<string>) to a data-aware control.

    * The binding target should be updated when the binding source has been changed. (OneWayToTarget/TwoWay)

    * The binding source should be updated when the bindging target has been changed and the UpdateSourceTrigger is satisifed. (OneWayToSource/TwoWay, UpdateSourceTrigger)


    * A user can bind a primitive-type property to a property of a non-data-aware control such as 'TEdit.Text'.

    * A user can bind a nullable property to a non-data-aware control.

    * A user can bind a property to some special properties of vcl controls, e.g. Caption, Text, Enabled, Visible, Color, ReadOnly


    * A user can specify the display format for a  binding source (property).

    * A user can bind a collection/array/enumerable to a list control such as TListBox, and can specify the data template.

    * A user can specify the value converter for a data binding.


4. Questions (Key Problems)

* How to define nullable properties for primitive types?

  Spring.System.TNullable<T>

* How to define property changed notification?

  Spring.Notifications.IPropertyNotification

* How to define collection changed notification?

  Spring.Notifications.ICollectionChangedNotification

* How to reuse the current data-binding mechanism of data-aware controls?



//-------------------------------------------------------------------------------------------------------------------
// The following code will be removed.
//-------------------------------------------------------------------------------------------------------------------

TAddress = class(TNotifiableObject)
// StreetName, City, etc.
end;

TPerson = class(TNotifableObject)
private
  fName: TNullableString;  // TNullable<string>
  fAddresses: IList<TAddress>;
  //...
public
  property Name: TNullableString read fName write SetName;
  property Addresses: IList<TAddress> read GetAddresses;
end;

procedure TPerson.SetName(value: TNullableString);
begin
  SetProperty<TNullableString>('Name', fName, value);
end;

constructor TPerson.Create;
begin
  fAddresses := TObjectNotifableCollection<TAddress>.Create;
end;

function TPerson.GetAddresses: IList<TAddress>;
begin
  Result := fAddresses; // or use lazy-initialization
end;

//-------------------------------------------------------------------------------------------------------------------


Now look, we need bind this property with a dbcombobox.

DBCombobox1.Items should have the names of street
DBCombobox1.Datasource point do BindindDataprovider 'Addresses'
DBCombobox1.DataField point do BindindDataprovider 'StreetName'
this is a way that we fill a dbcombox to work nativaly on delphi ok?????

Then we need 2 binding here. and i think that we need a new type of binding:

Binder.AddList(TargetExpression, SourceExpression): IBindingList
IBinderList.Add(SourceExpression)

then how we can do:

bl := Binder1.Add('DBComboBox1' , 'Addresses')
bl.Add('StreetName');

So the

marcos, when I review the data-aware controls, I think there will not any problem since we use datasource/dataset.

- neither all controls dataware will use datasource and datafield. dblookupcombox for sample
- I mean we can create a TBindingSource component that inherits from the TDataSouce. and then set the datasource and the datafield properties of db-aware controls.  And then everything will be so easy, for data-aware controls. That's why I asked the following question. We just need to bind data-object/list to the dataset, not the data-aware controls themselves.

e.g. TDBGrid.Columns, The TColumn actually binds to a field. and then don't need to be cared. other examples: TDBEdit, etc.

how we can define that on collumn[0] from dbgrid i want show name?
- If we creates the BindingSource, then it'll be the same as before.


5. Core Types

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
//    usDefault,
    /// <summary>
    /// The binding source will be updated when the property of the binding
    /// target has been changed.
    /// </summary>
    usPropertyChanged,
    /// <summary>
    /// The binding source will be updated when the target component lost focus.
    /// </summary>
    usLostFocus,
    /// <summary>
    /// The binding source won't be updated util the UpdateSource method of
    /// the binding was explicitly invoked.
    /// </summary>
    usExplicit
  );

  // IBinding, IBindable, etc.

