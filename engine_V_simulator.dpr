program engine_V_simulator;

uses
  Vcl.Forms,
  ev_sim_unit in 'ev_sim_unit.pas' {Form1},
  mcode_unit in 'mcode_unit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
