unit TestCollectionsAdapterResolver;

interface

uses
  TestFramework, Rtti, Spring.Persistence.Core.RttiCollectionAdapter, uModels
  , Spring.Persistence.Core.CollectionAdapterResolver, Spring.Persistence.Core.Interfaces;

type
  TestTCollectionAdapterResolver = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WhenPassEmptyRaiseException();
    procedure WhenPassIListWrapIntoIListAdapter();
    procedure WhenPassTListWrapIntoRttiAdapter();
  end;

implementation

uses
  Spring.Persistence.Core.Exceptions
  ,Spring.Collections
  ,Generics.Collections
  ,SysUtils
  ;

{ TestTCollectionAdapterResolver }

procedure TestTCollectionAdapterResolver.SetUp;
begin
  inherited;

end;

procedure TestTCollectionAdapterResolver.TearDown;
begin
  inherited;

end;

procedure TestTCollectionAdapterResolver.WhenPassEmptyRaiseException;
var
  LAdapter: ICollectionAdapter<TProduct>;
begin
  try
    LAdapter := TCollectionAdapterResolver.Resolve<TProduct>(TValue.Empty);
    CheckFalse(True);
  except
    on E: EORMContainerItemTypeNotSupported do
    begin
      CheckTrue(True);
    end;
  end;
end;

procedure TestTCollectionAdapterResolver.WhenPassIListWrapIntoIListAdapter;
var
  LAdapter: ICollectionAdapter<TProduct>;
  LCollection: IList<TProduct>;
begin
  LCollection := TCollections.CreateObjectList<TProduct>();
  LCollection.Add(TProduct.Create);
  LAdapter := TCollectionAdapterResolver.Resolve<TProduct>(TValue.From(LCollection));
  CheckEquals(1, LAdapter.Count);
end;

procedure TestTCollectionAdapterResolver.WhenPassTListWrapIntoRttiAdapter;
var
  LAdapter: ICollectionAdapter<TProduct>;
  LCollection: TObjectList<TProduct>;
begin
  LCollection := TObjectList<TProduct>.Create();
  LCollection.Add(TProduct.Create);
  LAdapter := TCollectionAdapterResolver.Resolve<TProduct>(LCollection);
  CheckEquals(1, LAdapter.Count);
  LCollection.Free;
end;

initialization
  RegisterTest(TestTCollectionAdapterResolver.Suite);

end.
