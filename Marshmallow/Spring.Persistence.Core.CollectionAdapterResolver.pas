unit Spring.Persistence.Core.CollectionAdapterResolver;

interface

uses
  Spring.Persistence.Core.Interfaces, Rtti;

const
  IID_SPRING_COLLECTION: TGUID = '{9BFD9B06-45CD-4C80-B145-01B09D432CF0}';

type
  TCollectionAdapterResolver = class
  public
    class function Resolve<T: class, constructor>(const ACollection: TValue): ICollectionAdapter<T>;
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,TypInfo
  ,Spring.Collections
  ,Spring.Persistence.Core.RttiCollectionAdapter
  ,Spring.Persistence.Core.SpringCollectionAdapter
  ,SysUtils
  ;



{ TCollectionAdapterResolver }

class function TCollectionAdapterResolver.Resolve<T>(
  const ACollection: TValue): ICollectionAdapter<T>;
begin
  if ACollection.IsEmpty then
    raise EORMContainerItemTypeNotSupported.Create('Collection type is empty');

  if (ACollection.Kind = tkInterface) and (Supports(ACollection.AsInterface, IID_SPRING_COLLECTION)) then
  begin
    Exit(TSpringCollectionAdapter<T>.Create(ACollection));
  end;

  Result := TRttiCollectionAdapter<T>.Create(ACollection);
end;

end.
