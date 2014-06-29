unit Core.Collections;

interface

uses
  Rtti
  ,Core.Interfaces
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent adapter of any enumerable collection. Can be used if there is
  ///	  a need to load results into different collection types.
  ///	</summary>
  {$ENDREGION}
  TCollectionAdapter<T: class, constructor> = class(TInterfacedObject, ICollectionAdapter<T>)
  private
    FCollection: TValue;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProp: TRttiProperty;
    FCountMethod: TRttiMethod;
  protected
    constructor Create(const ACollection: TValue); virtual;
    procedure GetMethodsFromRtti(); virtual;
  public
    class function Wrap(const ACollection: TValue): ICollectionAdapter<T>;

    procedure Add(AEntity: T);
    procedure Clear();
    function Count: Integer;
    function GetEnumerator(): ICollectionEnumerator<T>;

    function IsAddSupported(): Boolean;
  end;

implementation

uses
  Mapping.RttiExplorer
  ,Core.Exceptions
  ,Core.Collections.Enumerator
  ;


{ TCollectionAdapter<T> }

procedure TCollectionAdapter<T>.Add(AEntity: T);
begin
  FAddMethod.Invoke(FCollection, [AEntity]);
end;

procedure TCollectionAdapter<T>.Clear;
begin
  FClearMethod.Invoke(FCollection, []);
end;

function TCollectionAdapter<T>.Count: Integer;
begin
  if Assigned(FCountMethod) then
    Result := FCountMethod.Invoke(FCollection, []).AsInteger
  else if Assigned(FCountProp) then
    Result := FCountProp.GetValue(TRttiExplorer.GetRawPointer(FCollection)).AsInteger
  else
    raise EORMContainerDoesNotHaveCountMethod.CreateFmt('Count method not found for container type "%S"', [FCollection.ToString]);
end;

constructor TCollectionAdapter<T>.Create(const ACollection: TValue);
begin
  inherited Create();
  FCollection := ACollection;
  FAddMethod := nil;
  FClearMethod := nil;
  FCountProp := nil;
  FCountMethod := nil;
  GetMethodsFromRtti();
end;

function TCollectionAdapter<T>.GetEnumerator: ICollectionEnumerator<T>;
begin
  Result := TCollectionEnumerator<T>.Create(FCollection);
end;

procedure TCollectionAdapter<T>.GetMethodsFromRtti;
begin
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'Add', FAddMethod, 1);
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'Clear', FClearMethod, 0);
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'GetCount', FCountMethod, 0);
  FCountProp := TRttiContext.Create.GetType(FCollection.TypeInfo).GetProperty('Count');
end;

function TCollectionAdapter<T>.IsAddSupported: Boolean;
begin
  Result := Assigned(FAddMethod) and Assigned(FClearMethod);
end;

class function TCollectionAdapter<T>.Wrap(const ACollection: TValue): ICollectionAdapter<T>;
begin
  Result := TCollectionAdapter<T>.Create(ACollection);
end;

end.
