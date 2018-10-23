program engine_V_simulator;

uses
  Vcl.Forms,
  eV_sim_unit in 'eV_sim_unit.pas' {Form1},
  ev_sim in 'ev_sim.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
