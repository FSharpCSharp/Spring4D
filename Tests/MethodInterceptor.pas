unit MethodInterceptor;

interface

uses
  Spring.Interception;

type
  TMethodInterceptor = class(TInterfacedObject, IInterceptor)
  private
    fDelegate: IInterface;
  public
    constructor Create(const delegate: IInterface);
    procedure Intercept(const invocation: IInvocation);
    property Delegate: IInterface read fDelegate;
  end;

implementation

uses
  Rtti,
  Spring.Helpers;

{ TMethodInterceptor }

constructor TMethodInterceptor.Create(const delegate: IInterface);
begin
  fDelegate := delegate;
end;

procedure TMethodInterceptor.Intercept(const invocation: IInvocation);
type
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..3] of Pointer;
var
  arguments: TArray<TValue>;
  method: TRttiMethod;
  params: TArray<TRttiParameter>;
  args: TArray<TValue>;
  i: Integer;
  codeAddress: Pointer;
begin
  arguments := invocation.Arguments;
  method := invocation.Method;
  params := method.GetParameters;
  SetLength(args, Length(arguments) + 1);
  args[0] := TValue.From(fDelegate);

  // convert arguments for Invoke call (like done in the DispatchInvoke methods
  for i := Low(arguments) to High(arguments) do
    PassArg(params[i], arguments[i], args[i + 1], method.CallingConvention);

  codeAddress := PPVtable(fDelegate)^^[3];

  invocation.Result := Rtti.Invoke(codeAddress, args, method.CallingConvention, method.ReturnTypeHandle);
end;

end.
