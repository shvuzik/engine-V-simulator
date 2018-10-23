unit eV_sim_unit;

interface

uses
  eV_sim,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Imaging.jpeg, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    l: TMemo;
    Button2: TButton;
    r: TMemo;
    s: TMemo;
    Label2: TLabel;
    Button4: TButton;
    Edit2: TEdit;
    sb: TStatusBar;
    ref: TMemo;
    Label1: TLabel;
    Button3: TButton;
    c: TMemo;
    Button1: TButton;
    Button6: TButton;
    EditBreak: TEdit;
    Label3: TLabel;
    Button7: TButton;
    Label4: TLabel;
    Image1: TImage;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    terminate: boolean;
    function check_signature: boolean;

  public
    { Public declarations }
    MC: TMcode;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Terminate := true;


end;

procedure TForm1.Button2Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  s,
  fp,
  fn: string;
begin
  l.Lines.Clear;

  sb.Panels[0].Text := 'Starting test..';

  fp := ExtractFilePath(ParamStr(0));
  fn := fp + '\images\I-'+Edit1.Text+'-01'+'.bin';
  MC.LoadRAM(fn);
  MC.ram_top := $7FFF;

  fn := fp + '\references\I-'+Edit1.Text+'-01'+'.reference_output';
  ref.Lines.LoadFromFile(fn);

  //Label1.Caption :=Inttostr(MC.RAM_Loaded_Size );

  MC.PC := 0;
  MC.running := true;

  repeat
    l.Lines.Add(
      inttohex(MC.PC,4) + ':'+
      inttohex(MC.get_ram_word(MC.PC), 8)+' '+
      MC.disasm_at(mc.PC));
    MC.sim_step;

    //l.Lines.Add(inttohex(MC.get_reg32(3),8));

  until not MC.running;

  l.Lines.Add(
      inttohex(MC.PC,4) + ':'+
      inttohex(MC.get_ram_word(MC.PC), 8)+' '+
      MC.disasm_at(mc.PC));

//  sb.Panels.Items[0].Text := inttohex(MC.PC,8) + ' ' +inttohex(MC.get_ram_word(MC.PC), 8);


  if MC.get_ram_word(MC.PC)=$0000006F then
  begin
    if check_signature then
      s := 'Signature compare match > PASSED' else s:= 'Signature bad - FAILED!';

    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' (forever loop detected) ' + s;

  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  s,
  fp,
  fn: string;
  a0,a1,a2,a3,
  tohost,
  iw: cardinal;
  instructions: integer;
begin
  terminate := false;

  l.Lines.Clear;

  sb.Panels[0].Text := 'Running Dhrystone..';

  fp := ExtractFilePath(ParamStr(0));
  fn := fp + '\images\dhrystone.bin';
  MC.LoadRAM(fn);
  MC.ram_top := $FFFF;
  MC.uart_base := $C000;
  MC.iss_status := 0;
  MC.iss_break := strtoint('$'+ editBreak.Text);

  MC.PC := 0;
  MC.running := true;

  instructions := 0;
  repeat

    iw := MC.get_ram_word(MC.PC);

    //l.Lines.Add(      inttohex(MC.PC,4) + ':'+      inttohex(iw, 8)+' '+      MC.disasm_at(mc.PC));

    MC.sim_step;
    instructions := instructions + 1;

    if MC.iss_status <> 0 then
    begin
      case MC.iss_status of
        SIM_UART_WRITE: begin
           c.Text := c.Text + char(MC.iss_data and $FF);
           if (MC.iss_data and $FF)=$0A then c.Lines.Add('');

           //l.Lines.Add(inttohex(MC.iss_data and $FF, 2));
           MC.iss_status := 0;
          // MC.running := false;
        end;
        SIM_BUS_FAULT: begin
           l.Lines.Add('BUS FAULT at ' + inttohex(MC.iss_addr, 8));

        end;
      end;
    end;

//    if iw=$0ff0000f then
//    begin
//      tohost := MC.get_ram_word($800);
//      a0 := MC.get_ram_word(tohost);
//      a1 := MC.get_ram_word(tohost+4);
//      a2 := MC.get_ram_word(tohost+8);
//      a3 := MC.get_ram_word(tohost+12);
//
//      //l.Lines.Add('FENCE');
//      //l.Lines.Add(inttohex(tohost,8));
//
//      s := '';
//      for i := 0 to a3-1 do
//      begin
//        s := s + char(MC.get_ram_word(a2+i) and $FF);
//      end;
//      c.Lines.Add(s);
//    end;

    if MC.PC = MC.iss_break then
    begin
      MC.running := false;
      l.Lines.Add('BREAK');
    end;


    //l.Lines.Add(inttohex(MC.get_reg32(3),8));

    Application.ProcessMessages;
  until ((not MC.running) or terminate);

//  until (not MC.running) or (instructions > 1000);

  s := '';
  l.Lines.Add(
      inttohex(MC.PC,4) + ':'+
      inttohex(MC.get_ram_word(MC.PC), 8)+' '+
      MC.disasm_at(mc.PC));

  if MC.get_ram_word(MC.PC)=$0000006F then
  begin
    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' (forever loop detected) ' + s;
  end;

  r.Lines.Clear;
  r.Lines.Add('pc ' + inttohex(MC.PC,8));
  for I := 0 to 31 do
    begin
      r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.get_reg32(i),8));
    end;

  //check_signature;

end;

procedure TForm1.Button4Click(Sender: TObject);
var
  fn: string;
begin
  fn := ExtractFilePath(ParamStr(0));
  fn := fn + '\signatures   \I-'+Edit1.Text+'-01'+'.signature';
  s.Lines.SaveToFile(fn);
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  s,
  fp,
  fn: string;
  a0,a1,a2,a3,
  tohost,
  iw: cardinal;
  instructions: integer;
begin
  terminate := false;

  l.Lines.Clear;

  sb.Panels[0].Text := 'Starting zephyr..';

  fp := ExtractFilePath(ParamStr(0));
  fn := fp + '\images\zephyr.bin';
  MC.LoadRAM(fn);
  MC.ram_top := $7FFF;
  MC.uart_base := $C000;
  MC.iss_break := strtoint('$'+ editBreak.Text);


  MC.PC := 0;
  MC.running := true;
  MC.iss_status := 0;

  instructions := 0;
  repeat

    iw := MC.get_ram_word(MC.PC);

//    l.Lines.Add(      inttohex(MC.PC,4) + ':'+      inttohex(iw, 8)+' '+      MC.disasm_at(mc.PC));

    MC.sim_step;
    instructions := instructions + 1;

    if MC.iss_status <> 0 then
    begin
      case MC.iss_status of
        SIM_UART_WRITE: begin
           c.Text := c.Text + char(MC.iss_data and $FF);
        end;
        SIM_BUS_FAULT: begin
           l.Lines.Add('BUS FAULT at ' + inttohex(MC.iss_addr, 8));

        end;
      end;
      MC.iss_status := 0;
    end;

    if MC.PC = MC.iss_break then
    begin
      MC.running := false;
      l.Lines.Add('BREAK');
    end;

//    if MC.PC = $D48 then
//    begin
//      c.Lines.Text := c.Lines.Text + char(     MC.get_reg32(10) and $FF     );
//    end;

    if iw=$0ff0000f then
    begin
      c.Lines.Add(s);
    end;

    //l.Lines.Add(inttohex(MC.get_reg32(3),8));

    Application.ProcessMessages;
  until ((not MC.running) or terminate);

//  until (not MC.running) or (instructions > 1000);

  s := '';
  l.Lines.Add(
      inttohex(MC.PC,4) + ':'+
      inttohex(MC.get_ram_word(MC.PC), 8)+' '+
      MC.disasm_at(mc.PC));

  r.Lines.Clear;
  r.Lines.Add('pc ' + inttohex(MC.PC,8));
  for I := 0 to 31 do
    begin
      r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.get_reg32(i),8));
    end;


  if MC.get_ram_word(MC.PC)=$0000006F then
  begin
    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' (forever loop detected) ' + s;
  end;

  //check_signature;


end;

procedure TForm1.Button7Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  s,
  fp,
  fn: string;
  a0,a1,a2,a3,
  tohost,
  iw: cardinal;
  instructions: integer;
begin
  //l.Lines.Clear;

  MC.iss_status := 0;

  instructions := 0;

    iw := MC.get_ram_word(MC.PC);

    l.Lines.Add(      inttohex(MC.PC,4) + ':'+      inttohex(iw, 8)+' '+      MC.disasm_at(mc.PC));

    MC.sim_step;
    instructions := instructions + 1;

    if MC.iss_status <> 0 then
    begin
      case MC.iss_status of
        SIM_UART_WRITE: begin
           c.Text := c.Text + char(MC.iss_data and $FF);
        end;
        SIM_BUS_FAULT: begin
           l.Lines.Add('BUS FAULT at ' + inttohex(MC.iss_addr, 8));

        end;
      end;
    end;

    if MC.PC = MC.iss_break then
    begin
      MC.running := false;
      l.Lines.Add('BREAK');
    end;


    if iw=$0ff0000f then
    begin
      c.Lines.Add(s);
    end;

    //l.Lines.Add(inttohex(MC.get_reg32(3),8));

    Application.ProcessMessages;

//  until (not MC.running) or (instructions > 1000);

//  s := '';
//  l.Lines.Add(
//      inttohex(MC.PC,4) + ':'+
//      inttohex(MC.get_ram_word(MC.PC), 8)+' '+
//      MC.disasm_at(mc.PC));

  r.Lines.Clear;
  r.Lines.Add('pc ' + inttohex(MC.PC,8));
  for I := 0 to 31 do
    begin
      r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.get_reg32(i),8));
    end;


  if MC.get_ram_word(MC.PC)=$0000006F then
  begin
    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' (forever loop detected) ' + s;
  end;

  //check_signature;




end;

function TForm1.check_signature: boolean;
var
  sig_beg_addr,
  sig_end_addr,
  i: integer;

begin
  result := false;

  r.Lines.Clear;

  r.Lines.Add('pc ' + inttohex(MC.PC,8));
  for I := 0 to 31 do
    begin
      r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.REGS32[i],8));
    end;

  //
  i := $FF0;
  sig_beg_addr := 0;
  repeat
    if MC.get_ram_word(i)=$BBBBBBBB then
    begin
      //r.Lines.Add('SIG1 '+ inttohex(i,8));

      if sig_beg_addr=0 then
        sig_beg_addr := i and $FFF0+$10;
    end;
    i := i + $10;
  until
    (
    (MC.get_ram_word(i)=$EEEEEEEE) and
    (MC.get_ram_word(i+4)=$EEEEEEEE)
    ) or (i > $2000);

  //r.Lines.Add('SIG2'+ inttohex(i,8));

  if (sig_end_addr - sig_beg_addr) > 128 then exit;


  sig_end_addr := i and $FFF0;
  label2.Caption := 'Signature located at: ' + inttohex(sig_beg_addr,4) +  ' ' + inttohex(sig_end_addr,4);

  i := sig_beg_addr;
  s.Lines.Clear;
  repeat
    s.Lines.Add(
      lowercase(
        inttohex(MC.get_ram_word(i+12),8) +
        inttohex(MC.get_ram_word(i+8),8) +
        inttohex(MC.get_ram_word(i+4),8) +
        inttohex(MC.get_ram_word(i),8)
        )
    );
    i := i + 16;
  until i >= sig_end_addr;
  //
  if (s.Lines.Count=ref.Lines.Count) and (s.Lines.Count>0) then
  begin
    result := s.Lines.Text = ref.Lines.Text;

  end;


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MC := TMCode.Create(self);

end;

end.
