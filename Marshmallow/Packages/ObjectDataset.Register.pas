unit ObjectDataset.Register;

interface

uses
  Adapters.ObjectDataset,
  Classes;

  procedure Register;

implementation

{$R 'ObjectDataset.dcr'}

procedure Register;
begin
  RegisterComponents('Spring Persistence', [TObjectDataset]);
end;

end.
