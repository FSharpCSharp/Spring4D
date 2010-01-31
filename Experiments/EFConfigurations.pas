{*******************************************************}
{                                                       }
{       Research & Development Library                  }
{                                                       }
{       Copyright (C) 2008 Zuo Baoquan                  }
{                                                       }
{*******************************************************}

unit EFConfigurations;

interface

uses
  Classes, Windows, SysUtils, DB;

type
//  TPropertyType = TFieldType; // PTypeInfo

  { Name, Type, Value, Children }
  IProperty = interface
  (*
    function GetName: string;
    function GetDescription: string;
    function GetIsReadOnly: Boolean;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property AsBCD: TBcd read GetAsBCD write SetAsBCD;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: UnicodeString read GetAsWideString write SetAsWideString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    //*)
  end;

  // IPropertyInspector  , IPropertyChangeListener

  TPropertyCollectionEnumerator = class;

  IPropertyCollection = interface
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): IProperty;
    function Contains(const item: IProperty): Boolean;
    function IndexOf(const item: IProperty): Integer;
    function GetEnumerator: TPropertyCollectionEnumerator;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Items[index: Integer]: IProperty read GetItem; default;
  end;

  IPropertyProvider = interface
    function FindProperty(const name: string): IProperty;
  end;

  { Consider DesignIntf unit, IProperty, IPropertyEditor, etc. }

  TPropertyCollectionEnumerator = class

  end;




  EConfigurationException = class(Exception);

  IConfiguration = interface(IProperty)
//    function GetAttributes: IPropertyCollection;
//    function GetChildren: IConfigurationCollection;
    function GetLocation: string;
    function GetNamespace: string;
    function GetPrefix: string;
    function GetValue: string;
    property Namespace: string read GetNamespace;
    property Location: string read GetLocation;
    property Prefix: string read GetPrefix;
    property Value: string read GetValue;
  end;

  IConfigurationListener = interface

  end;

  IConfigurationProvider = interface

  end;

  IConfigurationCollection_ = interface
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(index: Integer): IConfiguration;
    function Contains(item: IConfiguration): Boolean;
    function IndexOf(Item: IConfiguration): Integer;
//    function GetEnumerator: TConfigurationListEnumerator;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Items[index: Integer]: IConfiguration read GetItem; default;
  end;

  IConfigurable = interface
    procedure Configure(config: IConfiguration);
  end;

//  TBaseConfiguration = class(TInterfacedObject, IConfiguration)
//
//  end;

implementation

end.
