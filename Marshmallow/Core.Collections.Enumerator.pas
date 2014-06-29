unit Core.Collections.Enumerator;

interface

uses
  Core.Interfaces
  ,Rtti
  ;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represent enumerator for collection adapter.
  ///	</summary>
  {$ENDREGION}
  TCollectionEnumerator<T: class, constructor> = class(TInterfacedObject, ICollectionEnumerator<T>)
  private
    FCollection: TValue;
    FGetEnumeratorMethod: TRttiMethod;
    FEnumerator: TValue;
    FMoveNextMethod: TRttiMethod;
    FGetCurrentMethod: TRttiMethod;
    FCurrentProp: TRttiProperty;
  protected
    constructor Create(const ACollection: TValue); virtual;
  public
    destructor Destroy; override;

    function GetCurrent: T;
    function MoveNext(): Boolean;
    property Current: T read GetCurrent;
  end;

implementation

uses
  Mapping.RttiExplorer
  ;


{ TCollectionEnumerator<T> }

constructor TCollectionEnumerator<T>.Create(const ACollection: TValue);
begin
  inherited Create;
  FCollection := ACollection;
  FGetEnumeratorMethod := nil;
  TRttiExplorer.TryGetMethod(FCollection.TypeInfo, 'GetEnumerator', FGetEnumeratorMethod, 0);
  if Assigned(FGetEnumeratorMethod) then
  begin
    FEnumerator := FGetEnumeratorMethod.Invoke(FCollection, []);
    TRttiExplorer.TryGetMethod(FEnumerator.TypeInfo, 'MoveNext', FMoveNextMethod, 0);
    TRttiExplorer.TryGetMethod(FEnumerator.TypeInfo, 'GetCurrent', FGetCurrentMethod, 0);
    FCurrentProp := TRttiContext.Create.GetType(FEnumerator.TypeInfo).GetProperty('Current');
  end;
end;

destructor TCollectionEnumerator<T>.Destroy;
begin
  if FEnumerator.IsObject then
    FEnumerator.AsObject.Free;
  inherited Destroy;
end;

function TCollectionEnumerator<T>.GetCurrent: T;
begin
  if Assigned(FGetCurrentMethod) then
    Result := FGetCurrentMethod.Invoke(FEnumerator, []).AsType<T>
  else if Assigned(FCurrentProp) then
    Result := FCurrentProp.GetValue(TRttiExplorer.GetRawPointer(FEnumerator)).AsType<T>;
end;

function TCollectionEnumerator<T>.MoveNext: Boolean;
begin
  Result := False;
  if Assigned(FMoveNextMethod) then
    Result := FMoveNextMethod.Invoke(FEnumerator, []).AsBoolean;
end;

end.
