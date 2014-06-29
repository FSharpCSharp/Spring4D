unit SvSerializerFactory;

interface

uses
  SvSerializerAbstract, Generics.Collections, SvSerializer, SysUtils
  ;

type
  TSerializerFactory = class sealed
  private
    class var
      FRegisteredSerializers: TDictionary<TSvSerializeFormat,TSvAbstractNonGenericSerializerClass>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class function GetInstance(AOwner: TSvSerializer; ASerializerFormat: TSvSerializeFormat): ISerializer;
    class procedure RegisterSerializer(ASerializerFormat: TSvSerializeFormat; AClass: TSvAbstractNonGenericSerializerClass);
  end;

  ESerializerFactoryException = class(Exception);

implementation

uses
  TypInfo
  ;


{ TSerializerFactory }

class constructor TSerializerFactory.Create;
begin
  FRegisteredSerializers := TDictionary<TSvSerializeFormat,TSvAbstractNonGenericSerializerClass>.Create();
end;

class destructor TSerializerFactory.Destroy;
begin
  FRegisteredSerializers.Free;
end;

class function TSerializerFactory.GetInstance(AOwner: TSvSerializer;
  ASerializerFormat: TSvSerializeFormat): ISerializer;
var
  LSerializerClass: TSvAbstractNonGenericSerializerClass;
begin
  if not FRegisteredSerializers.TryGetValue(ASerializerFormat, LSerializerClass) then
    raise ESerializerFactoryException.CreateFmt('Serializer not registered: "%S".',
      [GetEnumName(TypeInfo(TSvSerializeFormat), Ord(ASerializerFormat))]);

  Result := LSerializerClass.Create(AOwner);
end;

class procedure TSerializerFactory.RegisterSerializer(ASerializerFormat: TSvSerializeFormat;
  AClass: TSvAbstractNonGenericSerializerClass);
begin
  FRegisteredSerializers.AddOrSetValue(ASerializerFormat, AClass);
end;

end.
