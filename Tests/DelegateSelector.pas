unit DelegateSelector;

interface

uses
  Rtti,
  Spring.Collections,
  Spring.Interception;

type
  TDelegateSelector = class(TInterfacedObject, IInterceptorSelector)
  public
    function SelectInterceptors(const method: TRttiMethod;
      const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
  end;

implementation

uses
  MethodInterceptor;

{ TDelegateSelector }

function TDelegateSelector.SelectInterceptors(const method: TRttiMethod;
  const interceptors: IEnumerable<IInterceptor>): IEnumerable<IInterceptor>;
begin
  Result := interceptors.Where(
    function(const interceptor: IInterceptor): Boolean
    var
      methodInterceptor: TMethodInterceptor;
    begin
      methodInterceptor := interceptor as TMethodInterceptor;
      Result := Assigned(methodInterceptor);
    end).Skip(method.VirtualIndex - 3).Take(1);
end;

end.
