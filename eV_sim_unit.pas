unit ev_sim_unit;

interface

uses
  mcode_unit,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    l: TMemo;
    Button2: TButton;
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
    Button5: TButton;
    Button6: TButton;
    EditBreak: TEdit;
    Label3: TLabel;
    Button7: TButton;
    Panel1: TPanel;
    mr: TMemo;
    ml: TMemo;
    Button9: TButton;
    cbList: TCheckBox;
    cbListMicro: TCheckBox;
    log: TMemo;
    memoTests: TMemo;
    Button8: TButton;
    Button10: TButton;
    memoTests2: TMemo;
    log2: TMemo;
    dump: TMemo;
    Button11: TButton;
    Timer1: TTimer;
    cbConsole: TCheckBox;
    r: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    terminate: boolean;
    sig_check: boolean;
    function check_signature: boolean;
    procedure update_regs;
    procedure mc_dump(addr: cardinal);

  public
    { Public declarations }
    MC: TMcode;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
var
  passed,
  failed,
  i: integer;

begin
  log2.Lines.Clear;

  passed := 0;
  for i := 0 to memoTests.Lines.Count-1 do
    begin
       Edit1.Text := memoTests.Lines[i];
       Button5.OnClick(self);
       if sig_check then begin
          log2.Lines.Add(inttostr(i+1)+' OK: '+memoTests.Lines[i]);
          passed := passed + 1;
       end else begin
          log2.Lines.Add(inttostr(i+1)+' FAILED: '+memoTests.Lines[i]);
          failed := failed + 1;
       end;
    end;
    log2.Lines.Add('Passed # '+inttostr(passed) + ' Failed: '+inttostr(failed));


end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  mc_dump($F200);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Terminate := true;


end;

procedure TForm1.Button2Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  disasm,
  s,
  fp,
  fn: string;
begin
  l.Lines.Clear;
  MC.iss_debug := false;

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
  MC.uart_base := $C000;

  repeat
   if cbList.Checked then
   begin
    l.Lines.Add(
      inttohex(MC.PC,4) + ':'+
      inttohex(MC.get_ram_word(MC.PC), 8)+' '+
      MC.disasm_at(mc.PC));
      check_signature;

      r.Lines.Clear;
      r.Lines.Add('pc ' + inttohex(MC.PC,8));
      for I := 0 to 31 do
      begin
        r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.get_reg32(i),8));
      end;
    end;

    if (pos('JAL', disasm) > 0) and (mc.PC > $4) then
    begin
//       l.Lines.Add(          inttohex(MC.PC,4) + ':'+      inttohex(MC.get_ram_word(MC.PC), 8)+' '+      MC.disasm_at(mc.PC));
    end;


    MC.sim_step;



      disasm := MC.disasm_at(mc.PC);
      if (pos('ExxCALL', disasm) > 0) and (mc.PC > $4) then
      //if (pos('JALR', disasm) > 0) and (mc.PC > $4) then
      begin
        ml.Lines.Add('***************************');
        MC.running := false;
        check_signature;
      end;

      if (mc.PC = $1368) then
      begin
        l.Lines.Add('>>>>');
        //terminate := true;
        MC.running := false;
        check_signature;
      end;



    if MC.iss_debug  then
    begin
      l.Lines.Add(MC.iss_dbgmsg);
      MC.iss_debug := false;
    end;

    if MC.iss_status <> 0 then
    begin
      case MC.iss_status of
        SIM_UART_WRITE: begin
           if cbConsole.Checked then
           begin
             if (MC.iss_data and $FF)=$0a then c.Lines.Add('');
             c.Text := c.Text + char(MC.iss_data and $FF);
           end;
        end;
        SIM_BUS_FAULT: begin
           l.Lines.Add('BUS FAULT at ' + inttohex(MC.iss_addr, 8));
        end;

        SIM_WFI: begin
           l.Lines.Add('WFI-----');
           //if mc.mtimer_expired then
           begin
              mc.mepc := mc.PC;
              mc.PC   := mc.mtvec;
              mc.mcause := $80000007; // machine Timer interrupt
              mc.mip := $80;
              mc.mtimer_expired := false;
           end;
        end;

        SIM_CSR_READ: begin
           l.Lines.Add('CSR READ ' + inttohex(MC.iss_addr, 8));
        end;
        SIM_CSR_WRITE: begin
           l.Lines.Add('CSR WRITE ' + inttohex(MC.iss_addr, 8) + ' ' + inttohex(MC.iss_data, 8) );
        end else begin
           l.Lines.Add('ISS Status ' + inttostr(mc.iss_status) );
           mc.running := false;
        end;

      end;
      MC.iss_status := 0;
    end;


    //l.Lines.Add(inttohex(MC.get_reg32(3),8));
       Application.ProcessMessages;
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

procedure TForm1.Button5Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  disasm,
  s,
  fp,
  fn: string;
begin
  caption := 'running';

  ml.Lines.Clear;
  l.Lines.Clear;

  fp := ExtractFilePath(ParamStr(0));
  fn := fp + '\images\I-'+Edit1.Text+'-01'+'.bin';

 // fn := fp + '\images\test.bin';

  for i := 0 to 31 do
      MC.REGS8[i] := 0;

  for i := 0 to 255 do
    MC.RAM[$FF00 + i] := 0; //$AA;

  MC.LoadRAM(fn);
  MC.ram_top := $7FFF;

  fn := fp + '\references\I-'+Edit1.Text+'-01'+'.reference_output';
  ref.Lines.LoadFromFile(fn);

  fn := fp + '\images\test.mem';
  MC.LoadMCODE(fn);
  sb.Panels[0].Text := 'Loaded ' +inttostr(MC.mcode_size)+ ' words of microcode';

  MC.PC := 0;
  MC.mc_PC := 0;
  MC.mc_RISC_PC := 0;
  mc.uart_base := $C000;
  MC.mc_running := true;
  terminate := false;

  repeat
    if MC.mc_C then s := 'C' else s := 'c';

    if MC.iss_status <> 0 then
    begin
      case MC.iss_status of
        SIM_UART_WRITE: begin
           if cbConsole.Checked then
           begin
             if (MC.iss_data and $FF)=$0a then c.Lines.Add('');
             c.Text := c.Text + char(MC.iss_data and $FF);
           end;
        end;
        SIM_BUS_FAULT: begin
           l.Lines.Add('BUS FAULT at ' + inttohex(MC.iss_addr, 8));
        end;

        SIM_CSR_READ: begin
           l.Lines.Add('CSR READ ' + inttohex(MC.iss_addr, 8));
        end;
        SIM_CSR_WRITE: begin
           l.Lines.Add('CSR WRITE ' + inttohex(MC.iss_addr, 8) + ' ' + inttohex(MC.iss_data, 8) );
        end else begin
           l.Lines.Add('ISS Status ' + inttostr(mc.iss_status) );
           mc.running := false;
        end;
      end;
      MC.iss_status := 0;
    end;

     if MC.iss_debug  then
    begin
      l.Lines.Add(MC.iss_dbgmsg);
      MC.iss_debug := false;
    end;



    if cbListMicro.Checked then
      ml.Lines.Add(      inttohex(MC.mc_PC,4) + ':'+      inttohex(MC.get_mcode_word(MC.mc_PC), 4)+' '+      MC.mc_disasm_at(mc.mc_PC) + ' ' + s       );

    //l.Lines.Add(inttohex(MC.get_reg32(3),8));
    if MC.mc_PC = $02f then
    begin
      disasm := MC.disasm_at(mc.mc_RISC_PC);

      if (pos('CSxR', disasm) > 0) then
      begin
        l.Lines.Add(        inttohex(MC.mc_RISC_PC,4) + ':'+        inttohex(MC.get_ram_word(MC.mc_RISC_PC), 8)+' '+       MC.disasm_at(mc.mc_RISC_PC));       update_regs;
      end;

      if (pos('LxW ', disasm) > 0) then
      begin
        l.Lines.Add(        inttohex(MC.mc_RISC_PC,4) + ':'+        inttohex(MC.get_ram_word(MC.mc_RISC_PC), 8)+' '+       MC.disasm_at(mc.mc_RISC_PC));       update_regs;
        terminate := true;

      end;



//      if (mc.mc_RISC_PC > $18) then terminate := true;
//      if (pos('JALR', disasm) > 0) and (mc.mc_RISC_PC > $4) then
      if (pos('WFI', disasm) > 0) and (mc.mc_RISC_PC >= $0) then
      begin
       //ml.Lines.Add('********** WFI *****************');
       //l.Lines.Add(        inttohex(MC.mc_RISC_PC,4) + ':'+        inttohex(MC.get_ram_word(MC.mc_RISC_PC), 8)+' '+       MC.disasm_at(mc.mc_RISC_PC));       update_regs;

        //terminate := true;
        //check_signature;
        //update_regs;
      end;

      if (mc.mc_RISC_PC = $0848) then
      begin
        //ml.Lines.Add('>>>>');
        //l.Lines.Add('>>>>');
        //terminate := true;

        //c.Text := c.Text + char(MC.mc_get_reg32(10) and $FF);
      end;

      if MC.mc_get_reg32(0) <>0 then begin
        ml.Lines.Add('>>>>');
        l.Lines.Add('>>>>');
        terminate := true;
        check_signature;
      end;

      if cbList.Checked then
      begin
        l.Lines.Add(        inttohex(MC.mc_RISC_PC,4) + ':'+        inttohex(MC.get_ram_word(MC.mc_RISC_PC), 8)+' '+       MC.disasm_at(mc.mc_RISC_PC));       update_regs;
       // check_signature;
       update_regs;
      end;

      //terminate := true;

      if MC.mc_RISC_PC > 0 then
      begin
        if MC.get_ram_word(MC.mc_RISC_PC)=$0000006F then
        begin
          l.Lines.Add('FOREVER @' + inttohex(MC.mc_RISC_PC, 4));
          terminate := true;
        end;
//      if MC.get_ram_word(MC.mc_RISC_PC) = $C0001073 then MC.mc_running := false;
    end;

    end;



    //l.Lines.Add('>>' + inttohex(MC.mc_RISC_PC, 4));
    MC.mc_sim_step;
    //l.Lines.Add('**>>' + inttohex(MC.mc_RISC_PC, 4));

//    if MC.get_ram_word(MC.mc_RISC_PC-4) = $10500073 then MC.mc_running := false;
//    if MC.get_ram_word(MC.mc_RISC_PC) = 0 then MC.mc_running := false;


    Application.ProcessMessages;
  until not MC.mc_running or Terminate;

  update_regs;


   if check_signature then
      s := 'Signature compare match > PASSED' else s:= 'Signature bad - FAILED!';
    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' ' + s;

  if MC.get_ram_word(MC.mc_RISC_PC-4)=$0000006F then
  begin
    if check_signature then
      s := 'Signature compare match > PASSED' else s:= 'Signature bad - FAILED!';
    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' (forever loop detected) ' + s;
  end;

  caption := 'stopped';
//  l.Lines.Add(
//      inttohex(MC.mc_PC,4) + ':'+
//      inttohex(MC.get_mcode_word(MC.mc_PC), 4)+' '+
//      MC.mc_disasm_at(mc.mc_PC));
//

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

  check_signature;




end;

procedure TForm1.Button8Click(Sender: TObject);
var
  passed,
  failed,
  i: integer;

begin
  log.Lines.Clear;

  passed := 0;
  for i := 0 to memoTests.Lines.Count-1 do
    begin
       Edit1.Text := memoTests.Lines[i];
       Button2.OnClick(self);
       if sig_check then begin
          log.Lines.Add(inttostr(i+1)+' OK: '+memoTests.Lines[i]);
          passed := passed + 1;
       end else begin
          log.Lines.Add(inttostr(i+1)+' FAILED: '+memoTests.Lines[i]);
          failed := failed + 1;
       end;
    end;
    log.Lines.Add('Passed # '+inttostr(passed) + ' Failed: '+inttostr(failed));
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  addr: integer;
  i: Integer;
  disasm,
  s,
  fp,
  fn: string;
begin
//  ml.Lines.Clear;


//  l.Lines.Clear;

  MC.mc_running := true;
  Terminate := false;

  repeat
    if MC.get_ram_word(MC.mc_RISC_PC-4) = $10500073 then MC.mc_running := false;
    if MC.get_ram_word(MC.mc_RISC_PC) = 0 then MC.mc_running := false;

    if MC.mc_C then s := 'C' else s := 'c';

    ml.Lines.Add(      inttohex(MC.mc_PC,4) + ':'+      inttohex(MC.get_mcode_word(MC.mc_PC), 4)+' '+      MC.mc_disasm_at(mc.mc_PC) + ' ' + s       );

    //l.Lines.Add(inttohex(MC.get_reg32(3),8));
    if MC.mc_PC = $24 then
    begin

      disasm := MC.disasm_at(mc.mc_RISC_PC);
      if pos('AUxIPC', disasm) > 0 then
      begin
        ml.Lines.Add('***************************');
      end;

      l.Lines.Add(        inttohex(MC.mc_RISC_PC,4) + ':'+        inttohex(MC.get_ram_word(MC.mc_RISC_PC), 8)+' '+       MC.disasm_at(mc.mc_RISC_PC));
      ml.Lines.Add('****** '+  inttohex(MC.mc_RISC_PC,4) + ':'+   inttohex(MC.get_ram_word(MC.mc_RISC_PC), 8)+' '+       MC.disasm_at(mc.mc_RISC_PC));

      update_regs;
      terminate := true;
    end;

    MC.mc_sim_step;

    Application.ProcessMessages;
  until not MC.mc_running or Terminate or (MC.mc_PC = $012);

  update_regs;


   if check_signature then
      s := 'Signature compare match > PASSED' else s:= 'Signature bad - FAILED!';
    sb.Panels.Items[0].Text := 'Halt at: '+inttohex(MC.PC,4)+' (forever loop detected) ' + s;

//  l.Lines.Add(
//      inttohex(MC.mc_PC,4) + ':'+
//      inttohex(MC.get_mcode_word(MC.mc_PC), 4)+' '+
//      MC.mc_disasm_at(mc.mc_PC));
//

end;

function TForm1.check_signature: boolean;
var
  sig_beg_addr,
  sig_end_addr,
  i: integer;

begin
  sig_check := false;
  result := false;
(*
  r.Lines.Clear;
  r.Lines.Add('pc ' + inttohex(MC.PC,8));
  for I := 0 to 31 do
    begin
      r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.REGS32[i],8));
    end;
*)
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
        inttohex(MC.get_ram_word(i+8),8)  +
        inttohex(MC.get_ram_word(i+4),8)  +
        inttohex(MC.get_ram_word(i),8)
        )
    );
    i := i + 16;
  until i >= sig_end_addr;
  //
  if (s.Lines.Count=ref.Lines.Count) and (s.Lines.Count>0) then
  begin
    result := s.Lines.Text = ref.Lines.Text;
    sig_check := result;
  end;


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MC := TMCode.Create(self);
end;

procedure TForm1.mc_dump(addr: cardinal);
var
  i, j: integer;
  s: string;

begin
  dump.Lines.Clear;
  for I := 0 to 15 do
    begin
       s := inttohex(i*16,2) + ' ';
       for j := 0 to 15 do
       begin
          s := s + inttohex(MC.RAM[addr + i*16+j],2)
       end;


      dump.Lines.Add(s);
    end;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  mc.mtimer_expired := true;
end;

procedure TForm1.update_regs;
var i: integer;

begin
    mr.Lines.Clear;
  for I := 0 to 31 do
    begin
      mr.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.REGS8[i],2));
    end;

  r.Lines.Clear;
  r.Lines.Add('pc ' + inttohex(MC.mc_RISC_PC,8));
  for I := 0 to 31 do
    begin
      r.Lines.Add('r'+inttostr(i) + ' ' + inttohex(MC.mc_get_reg32(i),8));
    end;
end;

end.
