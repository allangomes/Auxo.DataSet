unit Auxo.DataSet.Register;

interface

procedure register;

implementation

uses
  Auxo.DataSet, System.Classes;

procedure register;
begin
  RegisterComponents('Auxo', [TAuxoDataSet]);
end;

end.
