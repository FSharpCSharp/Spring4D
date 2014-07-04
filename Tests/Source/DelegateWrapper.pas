unit DelegateWrapper;

interface

uses
  Spring;

type
  TDelegateWrapper = record
    class function WrapAs<T: IInterface>(delegate: PInterface): T; overload; static;
    class function WrapAs<T: IInterface>(delegates: array of PInterface): T; overload; static;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring.Interception,
  DelegateSelector,
  MethodInterceptor;

{ TDelegateWrapper }

class function TDelegateWrapper.WrapAs<T>(delegate: PInterface): T;
var
  generator: TProxyGenerator;
  proxy: TObject;
begin
  generator := TProxyGenerator.Create;
  try
    proxy := generator.CreateInterfaceProxyWithoutTarget(TypeInfo(T), TMethodInterceptor.Create(delegate^));
    Supports(proxy, GetTypeData(TypeInfo(T)).Guid, Result);
  finally
    generator.Free;
  end;
end;

class function TDelegateWrapper.WrapAs<T>(delegates: array of PInterface): T;
var
  generator: TProxyGenerator;
  options: TProxyGenerationOptions;
  proxy: TObject;
  interceptors: TArray<IInterceptor>;
  i: Integer;
begin
  generator := TProxyGenerator.Create;
  try
    options.Selector := TDelegateSelector.Create;
    SetLength(interceptors, Length(delegates));
    for i := Low(delegates) to High(delegates) do
      interceptors[i] := TMethodInterceptor.Create(delegates[i]^);
    proxy := generator.CreateInterfaceProxyWithoutTarget(TypeInfo(T),
      options, interceptors);
    Supports(proxy, GetTypeData(TypeInfo(T)).Guid, Result);
  finally
    generator.Free;
  end;
end;

end.
