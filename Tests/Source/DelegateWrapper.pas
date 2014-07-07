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
begin
  generator := TProxyGenerator.Create;
  try
    Result := generator.CreateInterfaceProxyWithoutTarget<T>(
      TMethodInterceptor.Create(delegate^));
  finally
    generator.Free;
  end;
end;

class function TDelegateWrapper.WrapAs<T>(delegates: array of PInterface): T;
var
  generator: TProxyGenerator;
  options: TProxyGenerationOptions;
  interceptors: TArray<IInterceptor>;
  i: Integer;
begin
  generator := TProxyGenerator.Create;
  try
    options.Selector := TDelegateSelector.Create;
    SetLength(interceptors, Length(delegates));
    for i := Low(delegates) to High(delegates) do
      interceptors[i] := TMethodInterceptor.Create(delegates[i]^);
    Result := generator.CreateInterfaceProxyWithoutTarget<T>(
      options, interceptors);
  finally
    generator.Free;
  end;
end;

end.
