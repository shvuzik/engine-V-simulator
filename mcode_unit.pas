unit mcode_unit;

interface

uses
  Windows, SysUtils, System.Classes;

const
  SIM_BUS_FAULT  = 3;
  SIM_UART_WRITE = 4;
  SIM_UART_READ  = 5;
  SIM_CSR_WRITE  = 6;
  SIM_CSR_READ   = 7;
  SIM_WFI        = 8;


type
  TMCode = class(TComponent)
  private
    FPC: word;
    Frunning: boolean;
    Fmc_PC: integer;
    Fmc_C: boolean;
    Fmc_Z: boolean;
    Fmc_running: boolean;
 //   Fmc_RISC_PC: cardinal;
    procedure SetPC(const Value: word);
    procedure Setrunning(const Value: boolean);
    procedure Setmc_PC(const Value: integer);
    procedure Setmc_C(const Value: boolean);
    procedure Setmc_Z(const Value: boolean);
    procedure Setmc_running(const Value: boolean);
    procedure Setmc_RISC_PC(const Value: cardinal);
    function Getmc_RISC_PC: cardinal;
    { Private declarations }
  public
    { Public declarations }


    mtimer_enabled,
    mtimer_expired: boolean;


    // Microcode engine simulation
    REGS8: Array[0..$1F] of byte;   // R0..R31
    MCODE: Array[0..$FFF] of word;  // microcode ROM
    mcode_size: word; // how many words loaded

    mode_iss: boolean;
    // RISCV ISS related variables

    iss_status: integer;
    iss_debug: boolean;
    iss_dbgmsg: string;

    iss_break,
    iss_addr,
    iss_data,
    uart_base,
    ram_top: cardinal;

    RAM_Loaded_Size: integer;
    RAM: Array[0..$FFFF] of byte;
    REGS32: Array[0..$1F] of cardinal;

    cycle: uint64;

    mip,mie,
    mepc,
    mtvec,
    mtval,
    mcause,
    mscratch: cardinal;

    // Microcode
    property mc_PC: integer read Fmc_PC write Setmc_PC;
    property mc_C: boolean read Fmc_C write Setmc_C;
    property mc_Z: boolean read Fmc_Z write Setmc_Z;

    property mc_RISC_PC: cardinal read Getmc_RISC_PC write Setmc_RISC_PC;



    property mc_running: boolean read Fmc_running write Setmc_running;

    // ISS
    property PC: word read FPC write SetPC;
    property running: boolean read Frunning write Setrunning;


    // microcode
    procedure LoadMCODE(FileName: string);
    procedure mc_sim_step;
    function mc_disasm(instruction: cardinal): string;
    function mc_disasm_at(address: cardinal): string;
    function get_mcode_word(addr: word): word;

    function get_imm8(instruction: word): byte;
    function get_reg_r_D0(instruction: word): byte;

    procedure check_carry_and_zero(w: word);
    procedure check_zero(w: word);
    //procedure set_reg32(reg: integer; value: cardinal);
    function  mc_get_reg32(reg: integer): cardinal;

    // ISS
    procedure LoadRAM(FileName: string);
    procedure sim_step;
    function disasm(instruction: cardinal): string;
    function disasm_at(address: cardinal): string;

    function get_ram_word(addr: cardinal): cardinal;
    procedure set_ram_byte(addr: cardinal; value: cardinal);
    procedure set_ram_word16(addr: cardinal; value: cardinal);
    procedure set_ram_word(addr: cardinal; value: cardinal);

    procedure set_reg32(reg: integer; value: cardinal);
    function  get_reg32(reg: integer): cardinal;
    procedure set_CSR(csr: integer; value: cardinal);
    function  get_CSR(csr: integer): cardinal;

    function  rd_from_instr(instr: cardinal): integer;
    function  rs1_from_instr(instr: cardinal): integer;
    function  rs2_from_instr(instr: cardinal): integer;

    function  funct3_from_instr(instr: cardinal): integer;
    function  imm_31_12_from_instr(instr: cardinal): integer;
    function  imm_11_0_from_instr(instr: cardinal): integer;
    function  imm_jal_from_instr(instr: cardinal): integer;
    function  imm_branch_from_instr(instr: cardinal): integer;
    function  imm_store_from_instr(instr: cardinal): integer;

    function  csr_name(addr: cardinal): string;

  end;

implementation

{ TMCode }

procedure TMCode.check_carry_and_zero(w: word);
begin
  if (w and $100) <> 0 then
  begin
    mc_C := true;
  end else begin
    mc_C := false;
  end;

  if (w and $ff) = 0 then
  begin
    mc_Z := true;
  end else begin
    mc_Z := false;
  end;

end;

procedure TMCode.check_zero(w: word);
begin
  if (w and $ff) = 0 then
  begin
    mc_Z := true;
  end else begin
    mc_Z := false;
  end;

end;

function TMCode.csr_name(addr: cardinal): string;
begin
    result := '[' + inttohex(addr,3)+']';
    case addr of
      $B00: result := 'mcycle';
      $B02: result := 'minstret';
      $340: result := 'mscratch';
      $341: result := 'mepc';
      $342: result := 'mcause';
      $343: result := 'mtval';
      $305: result := 'mtvec';
    end;
end;

function TMCode.disasm(instruction: cardinal): string;
var
  funct3,
  opcode: byte;
  m: string;
begin
  result := 'ILLEGAL ++++++++++++++++++++++++++++++++++++';

  opcode := instruction and $7C; // we must lower bits too

  case opcode of
    // x00 000 xx
    $00: begin
       m := 'LOAD ?';
       funct3 := funct3_from_instr(instruction);
       case funct3 of
         0: begin
           m := 'LB';
         end;
         1: begin
           m := 'LH';
         end;
         2: begin
           m := 'LW';
         end;
         4: begin
           m := 'LBU';
         end;
         5: begin
           m := 'LHU';
         end;

       end;
       result := m + ' r'+
         inttostr(rd_from_instr(instruction)) + ', r' +
         inttostr(rs1_from_instr(instruction)) + ', ' +
         inttostr(imm_11_0_from_instr(instruction)) + ' >>' +
         inttostr(funct3);
    end;
    $0C: begin
       // FENCE
       result := 'FENCE';
    end;

    // x00 100 xx
    $10: begin
       funct3 := funct3_from_instr(instruction);
       m := 'OP-IMM ?';
       case funct3 of
         0: begin
           // ADDI
           m := 'ADDI';
         end;
         1: begin
           m := 'SLLI';
         end;
         2: begin
           m := 'SLTI';
         end;
         3: begin
           m := 'SLTIU';
         end;
         4: begin
           m := 'XORI';
         end;
         5: begin
           if (instruction and $40000000)<>0 then
           begin
              m := 'SRAI';
           end else begin
              m := 'SRLI';
           end;
         end;
         6: begin
           m := 'ORI';
         end;
         7: begin
           m := 'ANDI';
         end;
       end;

       result := m + ' r'+
         inttostr(rd_from_instr(instruction)) + ', r' +
         inttostr(rs1_from_instr(instruction)) + ', ' +
         inttostr(imm_11_0_from_instr(instruction)) + ' >>' +
         inttostr(funct3);
    end;
    // x00 101 xx
    $14: begin
       //result := 'AUIPC r'+ inttostr(rd_from_instr(instruction)) +',0x0';
       result := 'AUIPC' + ' r'+
         inttostr(rd_from_instr(instruction)) + ', ' +
         inttohex(instruction and $FFFFF000, 4);

    end;

    // x01 000 xx
    $20: begin
       result := 'STORE - ILLEGAL';
       case funct3_from_instr(instruction) of
          0: begin
            m := 'SB';
          end;
          1: begin
            m := 'SH';
          end;
          2: begin
            m := 'SW';
          end;
       end;
         result := m + ' r'+
             inttostr(rs1_from_instr(instruction)) + ', r' +
             inttostr(rs2_from_instr(instruction)) + ', ' +
             inttostr(imm_store_from_instr(instruction));
    end;
    // x01 101 xx
    $34: begin
       result := 'LUI' + ' r'+
         inttostr(rd_from_instr(instruction)) + ', ' +
         inttohex(instruction and $FFFFF000, 8);
    end;
    // x01 100 xx
    $30: begin
       funct3 := funct3_from_instr(instruction);
       m := 'OP ?';
       case funct3 of
         0: begin
           if (instruction and $40000000)<>0 then
           begin
              m := 'SUB';
           end else begin
              m := 'ADD';
           end;
         end;
         1: begin
           m := 'SLL';
         end;
         2: begin
           m := 'SLT';
         end;
         3: begin
           m := 'SLTU';
         end;
         4: begin
           m := 'XOR';
         end;
         5: begin
           if (instruction and $40000000)<>0 then
           begin
              m := 'SRA';
           end else begin
              m := 'SRL';
           end;
         end;
         6: begin
           m := 'OR';
         end;
         7: begin
           m := 'AND';
         end;
       end;

       result := m + ' r'+
         inttostr(rd_from_instr(instruction)) + ', r' +
         inttostr(rs1_from_instr(instruction)) + ', r' +
         inttostr(rs2_from_instr(instruction)) ;

    end;

    // x11 000 xx
    $60: begin
       m := 'BRANCH - ILLEGAL';
       case funct3_from_instr(instruction) of
          0: begin
            m := 'BEQ ';
          end;
          1: begin
            m := 'BNE ';
          end;
          4: begin
            m := 'BLT ';
          end;
          5: begin
            m := 'BGE ';
          end;
          6: begin
            m := 'BLTU ';
          end;
          7: begin
            m := 'BGEU ';
          end;
       end;
       //
       result := m +' r' +
          inttostr(rs1_from_instr(instruction)) + ', r' +
          inttostr(rs2_from_instr(instruction)) + ', ' +
          inttostr(imm_branch_from_instr(instruction))+' [' +inttohex(imm_branch_from_instr(instruction)+PC,4)+']';


    end;
    // x11 001 xx
    $64: begin
       result := 'JALR r' +
          inttostr(rd_from_instr(instruction)) + ', r' +
          inttostr(rs1_from_instr(instruction)) + ', ' +
          inttostr(imm_11_0_from_instr(instruction));

    end;
    // x11 011 xx
    $6C: begin
       result := 'JAL r' +
          inttostr(rd_from_instr(instruction)) + ', ' +
          inttostr(imm_jal_from_instr(instruction))+' [' +inttohex(imm_jal_from_instr(instruction)+PC,4)+']';

    end;
    // x11 100 xx
    $70: begin
       m := 'SYSTEM - ILLEGAL';
       case funct3_from_instr(instruction) of
          0: begin
            if (instruction and $FFF00000) = $00100000 then
            begin
               m := 'EBREAK';
            end;
            if (instruction and $FFF00000) = $0 then
            begin
               m := 'ECALL';
            end;
            if (instruction and $FFF00000) = $30200000 then
            begin
               m := 'MRET';
            end;
            if (instruction and $FFF00000) = $10500000 then
            begin
               m := 'WFI';
            end;

            result := m;
          end;
          1: begin
            m := 'CSRRW r' +
                   inttostr(rd_from_instr(instruction)) + ', r' +
                   inttostr(rs1_from_instr(instruction)) + ', ' +
                   csr_name(instruction shr 20);
//                   '[' +inttohex(instruction shr 20,4)+']';
          end;
          2: begin
            m := 'CSRRS r' +
                   inttostr(rd_from_instr(instruction)) + ', r' +
                   inttostr(rs1_from_instr(instruction)) + ', ' +
                   csr_name(instruction shr 20);
//                   '[' +inttohex(instruction shr 20,4)+']';
          end;
          3: begin
            m := 'CSRRC r' +
                   inttostr(rd_from_instr(instruction)) + ', r' +
                   inttostr(rs1_from_instr(instruction)) + ', ' +
                   csr_name(instruction shr 20);
//                   '[' +inttohex(instruction shr 20,4)+']';
          end;
          5: begin
            m := 'CSRRWI r' +
                   inttostr(rd_from_instr(instruction)) + ', ' +
                   inttostr(rs1_from_instr(instruction)) + ', ' +
                   csr_name(instruction shr 20);
//                   '[' +inttohex(instruction shr 20,4)+']';
          end;
          6: begin
            m := 'CSRRSI r' +
                   inttostr(rd_from_instr(instruction)) + ', ' +
                   inttostr(rs1_from_instr(instruction)) + ', ' +
                   csr_name(instruction shr 20);
//                   '[' +inttohex(instruction shr 20,4)+']';
          end;
          7: begin
            m := 'CSRRCI r' +
                   inttostr(rd_from_instr(instruction)) + ', ' +
                   inttostr(rs1_from_instr(instruction)) + ', ' +
                   csr_name(instruction shr 20);
//                   '[' +inttohex(instruction shr 20,4)+']';
          end;
       end;
       result := m;
    end;


  end;


end;

function TMCode.disasm_at(address: cardinal): string;
begin
  result := disasm(get_ram_word(address));
end;

function TMCode.funct3_from_instr(instr: cardinal): integer;
begin
  result := (instr shr 12) and $7;
end;

function TMCode.Getmc_RISC_PC: cardinal;
begin
  result := REGS8[11] shl 8 or REGS8[10];

end;

function TMCode.get_CSR(csr: integer): cardinal;
begin
  result := 0;
  if not mode_iss then
  begin
    // fetch from shared RAM area !!



  end else begin
    case csr of
      $B00: result := cycle;
      $340: result := mscratch;
      $341: result := mepc;
      $342: result := mcause;
      $343: result := mtval;
      $305: result := mtvec;
    end;
  end;

end;

function TMCode.get_imm8(instruction: word): byte;
begin
  result := (instruction shr 4 and $F0) or (instruction and $F);
end;

function TMCode.get_mcode_word(addr: word): word;
begin
  result := MCODE[addr];
end;

function TMCode.get_ram_word(addr: cardinal): cardinal;
var
  W: cardinal;
  B: byte;
begin
  if addr > $7FFF then
  begin
    //running := false;
    //iss_addr := addr;
    //iss_status := SIM_BUS_FAULT;
    //result := 0;

    iss_debug := true;
    iss_dbgmsg := 'LD outside memory space @' + inttohex(addr,4);

    exit;
  end;

//addr := addr and $7FFF;
  addr := addr  and $FFFF;

  W :=      RAM[addr+3]; W := W shl 8;
  W := W or RAM[addr+2]; W := W shl 8;
  W := W or RAM[addr+1]; W := W shl 8;
  W := W or RAM[addr];
  result := W;
end;

function TMCode.get_reg32(reg: integer): cardinal;
begin
   result := REGS32[reg and $1F];
   if reg = 0 then result := 0;

end;

function TMCode.get_reg_r_D0(instruction: word): byte;
begin
  result := (instruction shr 5 and $10) or (instruction and $F);
end;

function TMCode.imm_11_0_from_instr(instr: cardinal): integer;
var
  c: cardinal;
begin
  c := (instr shr 20) and $FFF; // sign extend?
  if (c and $800) <> 0 then c := c or $FFFFF000;
  result := integer(c);
end;

function TMCode.imm_31_12_from_instr(instr: cardinal): integer;
begin
  result := (instr shr 12) and $FFFF; // sign extend?
end;

function TMCode.imm_jal_from_instr(instr: cardinal): integer;
var
  c: cardinal;
  i: integer;
begin
  //c := 0;
  c :=        instr         and $FF000;  //
  c := c or ((instr shr 20) and $07FE);   // 10:1 - 30:21
  c := c or ((instr shr  9) and $0800);   // 11 - 20
  c := c or ((instr shr 11) and $100000); // 20 - 31
  if (c and $100000) <> 0 then c := c or $FFE00000;
  result := integer(c);
end;

function TMCode.imm_branch_from_instr(instr: cardinal): integer;
var
  c: cardinal;
  i: integer;
begin
  // 4:1   < 11:8
  // 10:5  < 30:25
  // 11    < 7
  // 12    < 31
  c :=       (instr shr  7) and $001E;  // 4:1 - 11:8
  c := c or ((instr shr 20) and $07E0); // 10:5 - 30:25
  c := c or ((instr shl  4) and $0800); // 11 - 7
  c := c or ((instr shr 19) and $1000); // 12 - 31
  if (c and $1000) <> 0 then c := c or $FFFFE000;
  result := integer(c);

end;

function TMCode.imm_store_from_instr(instr: cardinal): integer;
var
  c: cardinal;
begin
  c := (instr shr 20) and $FE0; //
  c := c or ((instr shr 7) and $1F);
  if (c and $800) <> 0 then c := c or $FFFFF000;
  result := integer(c);
end;

procedure TMCode.LoadMCODE(FileName: string);
var
  SL: TStringList;
  i: integer;
  addr,
  code: word;

begin
  //
  //for I := $F000 to $FFFF do RAM[i] := 0;

  SL := TStringList.Create;
  SL.LoadFromFile(FileName);
  for I := 0 to SL.Count-1 do
    begin
       MCODE[i] := StrToInt('$' + SL[i]);
    end;
  mcode_size := SL.Count;

  SL.Destroy;
end;

procedure TMCode.LoadRAM(FileName: string);
var
  F: TFileStream;

begin
  //
  F := TFileStream.Create(FileName, fmOpenRead);
  RAM_Loaded_Size := F.Size;
  RAM_Loaded_Size := F.Read(RAM, $10000);

  F.Destroy;
end;

function TMCode.mc_disasm(instruction: cardinal): string;
var
  opcode: byte;
  m: string;
begin
  result := 'ILLEGAL +++++++++++';
  m := 'not implemented';

  if (instruction and $F000) = $C000 then
  begin
    if (instruction and $0800) <> 0 then
       m := 'RJMP ' + inttohex(integer(instruction and $FFF or $FFFFF000) + mc_PC+1, 4)
    else
       m := 'RJMP ' + inttohex(instruction and $FFF + mc_PC+1, 4);
  end;

  if (instruction and $FE00) = $FC00 then
  begin
     m := 'SBRC r' + inttostr(instruction shr 4 and $1F) + ', ' +inttostr(instruction and $7);
  end;
  if (instruction and $FE00) = $FE00 then
  begin
     m := 'SBRS r' + inttostr(instruction shr 4 and $1F) + ', ' +inttostr(instruction and $7);
  end;

  if (instruction and $F000) = $E000 then
  begin
     m := 'LDI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);
  end;

  if (instruction and $FC00) = $2C00 then
  begin
     m := 'MOV r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $F800) = $B800 then
  begin
     m := 'OUT r' + inttostr(instruction shr 4 and $1F);
  end;
  // #1

  // RETI
  if (instruction and $FFFF) = $9518 then
  begin
     m := 'SYSTEM';
  end;

  if (instruction and $F000) = $7000 then
  begin
     m := 'ANDI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);
  end;

  if (instruction and $F000) = $6000 then
  begin
     m := 'ORI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);
  end;
(*
  if (instruction and $F000) = $3000 then
  begin
     m := 'CPI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);
  end;
*)


  if (instruction and $FC00) = $0C00 then
  begin
     m := 'ADD r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FC00) = $1C00 then
  begin
     m := 'ADC r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FC00) = $2800 then
  begin
     m := 'OR r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FC00) = $2400 then
  begin
     m := 'XOR r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FC00) = $2000 then
  begin
     m := 'AND r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FC00) = $0800 then
  begin
     m := 'SBC r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FC00) = $1800 then
  begin
     m := 'SUB r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));
  end;

  if (instruction and $FE0F) = $9407 then
  begin
     m := 'ROR r' + inttostr(instruction shr 4 and $1F);
  end;
  if (instruction and $FE0F) = $9406 then
  begin
     m := 'LSR r' + inttostr(instruction shr 4 and $1F);
  end;
  if (instruction and $FE0F) = $9405 then
  begin
     m := 'ASR r' + inttostr(instruction shr 4 and $1F);
  end;

  if (instruction and $FE0F) = $940A then
  begin
     m := 'DEC r' + inttostr(instruction shr 4 and $1F);
  end;
//  if (instruction and $FE0F) = $9403 then
//  begin
//     m := 'INC r' + inttostr(instruction shr 4 and $1F);
//  end;

  if (instruction and $FE0F) = $9402 then
  begin
     m := 'SWAP r' + inttostr(instruction shr 4 and $1F);
  end;


  // BREQ
  if (instruction and $FC07) = $F001 then
  begin
    if (instruction and $0200) <> 0 then
       m := 'BREQ ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
    else
       m := 'BREQ ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
  end;

  // BRNE
  if (instruction and $FC07) = $F401 then
  begin
    if (instruction and $0200) <> 0 then
       m := 'BRNE ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
    else
       m := 'BRNE ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
  end;

  // BRCC
  if (instruction and $FC07) = $F400 then
  begin
    if (instruction and $0200) <> 0 then
       m := 'BRCC ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
    else
       m := 'BRCC ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
  end;

  // BRCS
  if (instruction and $FC07) = $F000 then
  begin
    if (instruction and $0200) <> 0 then
       m := 'BRCS ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
    else
       m := 'BRCS ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
  end;


  // not check needed bits!
  if (instruction and $D200) = $8200 then
  begin
     m := 'ST r' + inttostr(instruction shr 4 and $1F);
  end;

  // not check needed bits!
  if (instruction and $D200) = $8000 then
  begin
     m := 'LD r' + inttostr(instruction shr 4 and $1F);
  end;



  result := m;

end;

function TMCode.mc_disasm_at(address: cardinal): string;
begin
  result := mc_disasm(get_mcode_word(address));
end;

function TMCode.mc_get_reg32(reg: integer): cardinal;
var
  c: cardinal;
begin
  //
  c :=      RAM[$F103+(reg and $1F shl 2)]; c := c shl 8;
  c := c or RAM[$F102+(reg and $1F shl 2)]; c := c shl 8;
  c := c or RAM[$F101+(reg and $1F shl 2)]; c := c shl 8;
  c := c or RAM[$F100+(reg and $1F shl 2)];

  result := c;
end;

procedure TMCode.mc_sim_step;
var
  Zreg,
  w, r,
  instruction: word;
begin
  mode_iss := false;
  cycle := cycle + 1;

  //set_ram_word($F200, cycle);
  RAM[$F204]   := cycle;
  RAM[$F204+1] := cycle shr 8;
  RAM[$F204+2] := cycle shr 16;
  RAM[$F204+3] := cycle shr 24;


  instruction := MCODE[mc_PC];

  if (instruction and $F000) = $C000 then
  begin
    if (instruction and $0800) <> 0 then
    begin
      mc_PC := integer(instruction and $FFF or $FFFFF000) + mc_PC + 1
    end else begin
      mc_PC := instruction and $FFF + mc_PC + 1
    end;
    //if (instruction and $0800) <> 0 then
    //    m := 'RJMP ' + inttohex(integer(instruction and $FFF or $FFFFF000) + mc_PC+1, 4)
    // else
    //    m := 'RJMP ' + inttohex(instruction and $FFF + mc_PC+1, 4);

    exit;
  end;




  if (instruction and $FE00) = $FC00 then
  begin
     //m := 'SBRC r' + inttostr(instruction shr 4 and $1F) + ', ' +inttostr(instruction and $7);

     mc_PC := mc_PC + 1;
     if (REGS8[instruction shr 4 and $1F] and (1 shl (instruction and $7))) = 0 then mc_PC := mc_PC + 1; // SKIP

     exit;
   end;
  if (instruction and $FE00) = $FE00 then
  begin
     //m := 'SBRS r' + inttostr(instruction shr 4 and $1F) + ', ' +inttostr(instruction and $7);
     mc_PC := mc_PC + 1;
     if (REGS8[instruction shr 4 and $1F] and (1 shl (instruction and $7))) <> 0 then  mc_PC := mc_PC + 1; // SKIP

     exit;
   end;

  if (instruction and $F000) = $E000 then
  begin
     //m := 'LDI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);

     REGS8[instruction shr 4 and $F + 16] := get_imm8(instruction);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FC00) = $2C00 then
  begin
     //m := 'MOV r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));


     REGS8[instruction shr 4 and $1F] := REGS8[get_reg_r_D0(instruction)];
     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $F800) = $B800 then
  begin
     //m := 'OUT r' + inttostr(instruction shr 4 and $1F);
     mc_PC := mc_PC + 1;
     exit;
  end;
  // #1

  if (instruction and $F000) = $7000 then
  begin
     //m := 'ANDI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);
     w := REGS8[instruction shr 4 and $F + 16] and get_imm8(instruction);
     REGS8[instruction shr 4 and $F + 16] := w;
     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $F000) = $6000 then
  begin
     //m := 'ORI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);
     REGS8[instruction shr 4 and $F + 16] := REGS8[instruction shr 4 and $F + 16] or get_imm8(instruction);

     mc_PC := mc_PC + 1;
     exit;
  end;
(*
  if (instruction and $F000) = $3000 then
  begin
     //m := 'CPI r' + inttostr(instruction shr 4 and $F + 16) + ', 0x' +inttohex(get_imm8(instruction),2);

     mc_Z := REGS8[instruction shr 4 and $F + 16] = get_imm8(instruction); // Set Zero flag

     mc_PC := mc_PC + 1;
     exit;
  end;
*)
  if (instruction and $FC00) = $0C00 then
  begin
     //m := 'ADD r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] + REGS8[get_reg_r_D0(instruction)];
     REGS8[instruction shr 4 and $1F] := w;
     check_carry_and_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FC00) = $1C00 then
  begin
     //m := 'ADC r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] + REGS8[get_reg_r_D0(instruction)];
     if mc_C then w := w + 1;
     REGS8[instruction shr 4 and $1F] := w;


     check_carry_and_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;
(*
  if (instruction and $FC00) = $0800 then
  begin
     //m := 'SBC r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     // error
     if not mc_C then
     begin
       w := REGS8[instruction shr 4 and $1F] - REGS8[get_reg_r_D0(instruction)];
       REGS8[instruction shr 4 and $1F] := w;
     end else begin
       w := REGS8[instruction shr 4 and $1F] - REGS8[get_reg_r_D0(instruction)] - 1;
       REGS8[instruction shr 4 and $1F] := w;
     end;
     //w := w xor $100;
     check_carry_and_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FC00) = $1800 then
  begin
     //m := 'SUB r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] - REGS8[get_reg_r_D0(instruction)];
     REGS8[instruction shr 4 and $1F] := w;
     //w := w xor $100;
     check_carry_and_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;
*)

  if (instruction and $FC00) = $0800 then
  begin
     //m := 'SBC r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] and $FF;
     r := REGS8[get_reg_r_D0(instruction)];
     if mc_C then
     begin
       r := r + 1;
     end;
     REGS8[instruction shr 4 and $1F] := w - r;

     if r>w then
     begin
       w := w or $100;
     end else begin
       w := w or $ff;
     end;

     //w := w xor $100;
     check_carry_and_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FC00) = $1800 then
  begin
     //m := 'SUB r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] - REGS8[get_reg_r_D0(instruction)];
     REGS8[instruction shr 4 and $1F] := w;
     //w := w xor $100;
     check_carry_and_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;



  if (instruction and $FC00) = $2800 then
  begin
     //m := 'OR r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] or REGS8[get_reg_r_D0(instruction)];
     REGS8[instruction shr 4 and $1F] := w;

     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FC00) = $2400 then
  begin
     //m := 'XOR r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] xor REGS8[get_reg_r_D0(instruction)];
     REGS8[instruction shr 4 and $1F] := w;

     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

   if (instruction and $FC00) = $2000 then
  begin
     //m := 'AND r' + inttostr(instruction shr 4 and $1F) + ', r' +inttostr(get_reg_r_D0(instruction));

     w := REGS8[instruction shr 4 and $1F] and REGS8[get_reg_r_D0(instruction)];
     REGS8[instruction shr 4 and $1F] := w;

     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FE0F) = $9407 then
  begin
     //m := 'ROR r' + inttostr(instruction shr 4 and $1F);
     w := REGS8[instruction shr 4 and $1F] and $FF;
     if mc_C then w := w or $100;
     mc_C := (w and 1) <> 0;
     w := w shr 1;
     REGS8[instruction shr 4 and $1F] := w;
     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;


  if (instruction and $FE0F) = $9406 then
  begin
     //m := 'LSR r' + inttostr(instruction shr 4 and $1F);
     w := REGS8[instruction shr 4 and $1F] and $FF;
     mc_C := (w and 1) <> 0;
     w := w shr 1;
     REGS8[instruction shr 4 and $1F] := w;
     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  if (instruction and $FE0F) = $9405 then
  begin
     //m := 'ASR r' + inttostr(instruction shr 4 and $1F);
     w := REGS8[instruction shr 4 and $1F] and $FF;
     mc_C := (w and 1) <> 0;
     if (w and $80) <> 0 then w := w or $100;
     w := w shr 1;
     REGS8[instruction shr 4 and $1F] := w;
     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;

  (*
  if (instruction and $FE0F) = $940A then
  begin
     //m := 'DEC r' + inttostr(instruction shr 4 and $1F);

     w := REGS8[instruction shr 4 and $1F] - 1;
     REGS8[instruction shr 4 and $1F] := w;
     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;
  *)

(*
  if (instruction and $FE0F) = $9403 then
  begin
     //m := 'INC r' + inttostr(instruction shr 4 and $1F);

     w := REGS8[instruction shr 4 and $1F] + 1;
     REGS8[instruction shr 4 and $1F] := w;
     check_zero(w);

     mc_PC := mc_PC + 1;
     exit;
  end;
*)
  if (instruction and $FE0F) = $9402 then
  begin
     //m := 'SWAP r' + inttostr(instruction shr 4 and $1F);

     REGS8[instruction shr 4 and $1F] := (REGS8[instruction shr 4 and $1F] shl 4) or (REGS8[instruction shr 4 and $1F] shr 4);

     mc_PC := mc_PC + 1;
     exit;
  end;




  if (instruction and $FC07) = $F001 then
  begin
//    if (instruction and $0200) <> 0 then
//       m := 'BREQ ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
//    else
//       m := 'BREQ ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);

    if mc_Z then
    begin
      if (instruction and $0200) <> 0 then
      begin
         mc_PC := integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC + 1
      end else begin
         mc_PC := (instruction shr 3 and $7F) + mc_PC + 1
      end;
      exit;
    end else begin
     mc_PC := mc_PC + 1;
     exit;
    end;
  end;

  if (instruction and $FC07) = $F401 then
  begin
//    if (instruction and $0200) <> 0 then
//       m := 'BRNE ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
//    else
//       m := 'BRNE ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
    if not mc_Z then
    begin
      if (instruction and $0200) <> 0 then
      begin
         mc_PC := integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC + 1
      end else begin
         mc_PC := (instruction shr 3 and $7F) + mc_PC + 1
      end;
      exit;
    end else begin
     mc_PC := mc_PC + 1;
     exit;
    end;
  end;

  if (instruction and $FC07) = $F400 then
  begin
//    if (instruction and $0200) <> 0 then
//       m := 'BRCC ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
//    else
//       m := 'BRCC ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
    if not mc_C then
    begin
      if (instruction and $0200) <> 0 then
      begin
         mc_PC := integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC + 1
      end else begin
         mc_PC := (instruction shr 3 and $7F) + mc_PC + 1
      end;
      exit;
    end else begin
     mc_PC := mc_PC + 1;
     exit;
    end;
  end;

  if (instruction and $FC07) = $F000 then
  begin
//    if (instruction and $0200) <> 0 then
//       m := 'BRCS ' + inttohex(integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC+1, 4)
//    else
//       m := 'BRCS ' + inttohex(instruction shr 3 and $7F + mc_PC+1, 4);
    if mc_C then
    begin
      if (instruction and $0200) <> 0 then
      begin
         mc_PC := integer(instruction shr 3 and $7F or $FFFFFF80) + mc_PC + 1
      end else begin
         mc_PC := (instruction shr 3 and $7F) + mc_PC + 1
      end;
      exit;
    end else begin
     mc_PC := mc_PC + 1;
     exit;
    end;
  end;


  // ST
//  if (instruction and $FE0F) = $8200 then
  if (instruction and $D200) = $8200 then
  begin
     //m := 'ST r' + inttostr(instruction shr 4 and $1F);
     Zreg := REGS8[31] shl 8 or REGS8[30];

     Zreg := Zreg xor (Instruction and $3);

     if Zreg = uart_base  then
     begin
      //
      iss_status := SIM_UART_WRITE;
      iss_data := REGS8[instruction shr 4 and $1F];
     end;


//ZREG := ZREG and $7FFF;
     RAM[Zreg] := REGS8[instruction shr 4 and $1F];

     if (zreg and $FF00) = $F200 then
     begin
//       iss_debug := true;
//       iss_dbgmsg := 'WR [' + inttohex(Zreg, 4)+']='+inttohex(REGS8[instruction shr 4 and $1F],2);
     end;


     mc_PC := mc_PC + 1;
     exit;
  end;

  //if (instruction and $FE0F) = $8000 then
  // 10x0 xx0x xxxx xxxx
  if (instruction and $D200) = $8000 then
  begin
     //m := 'LD r' + inttostr(instruction shr 4 and $1F);
     Zreg := REGS8[31] shl 8 or REGS8[30];
//ZREG := ZREG and $7FFF;
     Zreg := Zreg xor (Instruction and $3);

     REGS8[instruction shr 4 and $1F] := RAM[Zreg];

     if (zreg and $FF00) = $F700 then
     begin
//       iss_debug := true;
//       iss_dbgmsg := 'RD [' + inttohex(Zreg, 4)+']='+inttohex(REGS8[instruction shr 4 and $1F],2);
     end;


     mc_PC := mc_PC + 1;
     exit;
  end;






  mc_running := false;
end;

function TMCode.rd_from_instr(instr: cardinal): integer;
begin
  result := (instr shr 7) and $1F;
end;

function TMCode.rs1_from_instr(instr: cardinal): integer;
begin
  result := (instr shr 15) and $1F;
end;

function TMCode.rs2_from_instr(instr: cardinal): integer;
begin
  result := (instr shr 20) and $1F;
end;

procedure TMCode.Setmc_C(const Value: boolean);
begin
  Fmc_C := Value;
end;

procedure TMCode.Setmc_PC(const Value: integer);
begin
  Fmc_PC := Value;
end;

procedure TMCode.Setmc_RISC_PC(const Value: cardinal);
begin
  //Fmc_RISC_PC := Value;
end;

procedure TMCode.Setmc_running(const Value: boolean);
begin
  Fmc_running := Value;
end;

procedure TMCode.Setmc_Z(const Value: boolean);
begin
  Fmc_Z := Value;
end;

procedure TMCode.SetPC(const Value: word);
begin
  FPC := Value;
end;

procedure TMCode.Setrunning(const Value: boolean);
begin
  Frunning := Value;
end;

procedure TMCode.set_CSR(csr: integer; value: cardinal);
begin

  case csr of

    $304: begin
      mie := value;
      if (value and $80) = 0 then
      begin
        //disable machine timer
        mtimer_enabled := false;
      end else begin
        // enable machine timer
        mtimer_enabled := true;
      end;
      if (value and $FFFFFF7F) <> 0 then
      begin
        // enabling unsupported interrupt
        iss_status := SIM_CSR_WRITE;
        iss_addr := csr;
        iss_data := value;
        running := false;
      end;
    end;


    $340: mscratch := value;
    $341: mepc := value;
    $342: mcause := value;
    $343: mtval := value;
    $305: mtvec := value;
  end;
end;

procedure TMCode.set_ram_byte(addr, value: cardinal);
begin
  if addr = uart_base then
  begin
    //running := false;

    iss_status := SIM_UART_WRITE;
    iss_data := value;
    exit;
  end;
//addr := addr and $7FFF;

  if addr > $F000 then
  begin
    running := false;
    iss_addr := addr;
    iss_data := value;
    iss_status := SIM_BUS_FAULT;
    exit;
  end;

  //
//  if addr > ram_top then
//  begin
//    running := false;
//    iss_addr := addr;
//    iss_data := value;
//    iss_status := SIM_BUS_FAULT;
//  end;

  addr := addr and $FFFF;
  RAM[addr]   := value;
  //RAM[addr+1] := value shr 8;
  //RAM[addr+2] := value shr 16;
  //RAM[addr+3] := value shr 24;
end;

procedure TMCode.set_ram_word(addr, value: cardinal);
begin
//addr := addr and $7FFF;
  if addr > $f000 then
  begin
    iss_addr := addr;
    iss_data := value;

    running := false;
    iss_status := SIM_BUS_FAULT;
    exit;
  end;


//  if addr > ram_top then
//  begin
//    running := false;
//    iss_status := SIM_BUS_FAULT;
//  end;

  //
  addr := addr and $FFFF;
  RAM[addr]   := value;
  RAM[addr+1] := value shr 8;
  RAM[addr+2] := value shr 16;
  RAM[addr+3] := value shr 24;
end;

procedure TMCode.set_ram_word16(addr, value: cardinal);
begin
//addr := addr and $7FFF;

  if addr > $f000 then
  begin
    running := false;
    iss_addr := addr;
    iss_data := value;

    iss_status := SIM_BUS_FAULT;
    exit;
  end;

//  if addr > ram_top then
//  begin
//    running := false;
//    iss_status := SIM_BUS_FAULT;
//  end;

  //
  addr := addr and $FFFF;
  RAM[addr]   := value;
  RAM[addr+1] := value shr 8;
  //RAM[addr+2] := value shr 16;
  //RAM[addr+3] := value shr 24;
end;

procedure TMCode.set_reg32(reg: integer; value: cardinal);
begin
   REGS32[reg and $1F] := value;
   REGS32[0] := 0;
end;

procedure TMCode.sim_step;
var
  t64: int64;
  misalign_ld,
  misalign_st,
  do_branch: boolean;
  funct3,
  opcode: byte;
  i: integer;
  ram_addr,
  rs1,rs2,rd,
  old_pc,
  c,
  instruction: cardinal;
begin
  mode_iss := true;

  instruction := get_ram_word(FPC);
  opcode := instruction and $7C; // we must lower bits too
  old_pc := FPC;
  misalign_ld := false;
  misalign_st := false;

  case opcode of
    // x00 000 xx
    $00: begin
       //result := 'LOAD';
       funct3 := funct3_from_instr(instruction);
       case funct3 of
         // LB
         0: begin
            c := get_ram_word( get_reg32(rs1_from_instr(instruction)) + imm_11_0_from_instr(instruction)  );
            if (c and $80)<>0 then c := c or $FFFFFF00 else c := c and $ff;
            set_reg32(rd_from_instr(instruction), c);
         end;
         // LH
         1: begin
            ram_addr := get_reg32(rs1_from_instr(instruction)) + imm_11_0_from_instr(instruction);
            if (ram_addr and $0001) <> 0 then
            begin
//              iss_debug := true;
              iss_dbgmsg := 'misaligned LH @' + inttohex(PC,4) + ' Load from: ' + inttohex(ram_addr,4);
              misalign_ld := true;
            end else begin

              c := get_ram_word( ram_addr  );
              if (c and $8000)<>0 then c := c or $FFFF0000 else c := c and $ffff;
              set_reg32(rd_from_instr(instruction), c);
            end;
         end;
         // LW
         2: begin
            ram_addr := get_reg32(rs1_from_instr(instruction)) + imm_11_0_from_instr(instruction);
            if (ram_addr and $0003) <> 0 then
            begin
//              iss_debug := true;
              iss_dbgmsg := 'misaligned LW @' + inttohex(PC,4) + ' Load from: ' + inttohex(ram_addr,4);
              misalign_ld := true;
            end else begin

              set_reg32(
                rd_from_instr(instruction),
                get_ram_word(
                  ram_addr
                )
              );
            end;
         end;
         // LBU
         4: begin

            set_reg32(
              rd_from_instr(instruction),
              get_ram_word(
                get_reg32(rs1_from_instr(instruction)) + imm_11_0_from_instr(instruction)
              ) and $FF
            );
         end;
         // LHU
         5: begin
            ram_addr := get_reg32(rs1_from_instr(instruction)) + imm_11_0_from_instr(instruction);
            if (ram_addr and $0001) <> 0 then
            begin
//              iss_debug := true;
              iss_dbgmsg := 'misaligned LHU @' + inttohex(PC,4) + ' Load from: ' + inttohex(ram_addr,4);
              misalign_ld := true;
            end else begin
                set_reg32(
                rd_from_instr(instruction),
                get_ram_word(
                    ram_addr
                ) and $FFFF
              );
            end;
         end;



       end;
       //result := m + ' r'+
       //  inttostr(rd_from_instr(instruction)) + ', r' +
       //  inttostr(rs1_from_instr(instruction)) + ', ' +
       //  inttostr(imm_11_0_from_instr(instruction)) + ' >>' +
       //  inttostr(funct3);


       FPC := FPC + 4; // ?
    end;

    $0C: begin
       // FENCE

       //running := false;

       FPC := FPC + 4; // ?
    end;

    // x00 100 xx
    // OP-IMM
    $10: begin
       funct3 := funct3_from_instr(instruction);
       rs1 := get_reg32(rs1_from_instr(instruction));
       rs2 := imm_11_0_from_instr(instruction);

       case funct3 of
         0: begin
           // ADDI
           rd := rs1 + rs2;
         end;
         1: begin
           //m := 'SLLI';
            rd := rs1 shl (rs2 and $1F);
         end;
         2: begin
           //m := 'SLTI';
//           if integer(rs1) < integer(rs2) then rd := 1 else rd := 0;

           if (rs1+$80000000) < (rs2+$80000000) then rd := 1 else rd := 0;

         end;
         3: begin
           // 'SLTIU'; OK
           // positive values only
           if rs1 < rs2 then rd := 1 else rd := 0;



         end;
         4: begin
           //m := 'XORI';
           rd := rs1 xor rs2;
         end;
         5: begin
           if (instruction and $40000000)<>0 then
           begin
              //m := 'SRAI';
              t64 := rs1;
              if(t64 and $80000000) <>0 then t64 := t64 or $FFFFFFFF00000000;
              //rd := t64 shr (rs2 and $1F);
              rd := t64 shr rs2;
           end else begin
              //m := 'SRLI';
              rd := rs1 shr rs2;
           end;
         end;
         6: begin
           //m := 'ORI';
           rd := rs1 or rs2;
         end;
         7: begin
           //m := 'ANDI';
           rd := rs1 and rs2;
         end;
       end;

       set_reg32(  rd_from_instr(instruction), rd  );

       //result := 'OP-IMM';
       FPC := FPC + 4; // ?
    end;

    // x01 000 xx
    // STORE
    $20: begin
       //result := 'SW';

       case funct3_from_instr(instruction) of
          0: begin
            //result := 'SB';
           set_ram_byte(
             get_reg32(rs1_from_instr(instruction)) + imm_store_from_instr(instruction)
           , get_reg32(rs2_from_instr(instruction))
           )

          end;
          1: begin
            //result := 'SH';

            ram_addr := get_reg32(rs1_from_instr(instruction)) + imm_store_from_instr(instruction);
            if (ram_addr and $0001) <> 0 then
            begin
//              iss_debug := true;
              iss_dbgmsg := 'misaligned SH @' + inttohex(PC,4) + ' Load from: ' + inttohex(ram_addr,4);
              misalign_st := true;
            end else begin
               set_ram_word16(
               ram_addr
               , get_reg32(rs2_from_instr(instruction))
               );
            end;


//           set_ram_word16(
//             get_reg32(rs1_from_instr(instruction)) + imm_store_from_instr(instruction)
//           , get_reg32(rs2_from_instr(instruction))
//           );


          end;
          2: begin
            //result := 'SW';
           //result := m + ' r'+
           //  inttostr(rs1_from_instr(instruction)) + ', r' +
           //  inttostr(rs2_from_instr(instruction)) + ', ' +
           //  inttostr(imm_store_from_instr(instruction));

            ram_addr := get_reg32(rs1_from_instr(instruction)) + imm_store_from_instr(instruction);
            if (ram_addr and $0003) <> 0 then
            begin
//              iss_debug := true;
              iss_dbgmsg := 'misaligned SW @' + inttohex(PC,4) + ' Load from: ' + inttohex(ram_addr,4);

               misalign_st := true;
            end else begin
               set_ram_word(
                 ram_addr
               , get_reg32(rs2_from_instr(instruction))
               );
               //iss_debug := true;
               //iss_dbgmsg := 'SW [' + inttohex(ram_addr, 4)+']='+inttohex(get_reg32(rs2_from_instr(instruction)),4);
            end;


          end;
       end;

       FPC := FPC + 4; // ?
    end;
    // x01 101 xx
    $34: begin
       //result := 'LUI';
       set_reg32(rd_from_instr(instruction), (instruction and $FFFFF000)); //
       FPC := FPC + 4; // ?
    end;
    // x00 101 xx
    $14: begin
       //result := 'AUIPC';
       // works
       set_reg32(rd_from_instr(instruction), (instruction and $FFFFF000) + PC);
       set_reg32(rd_from_instr(instruction), (instruction and $FFFF0000) or ((PC + (instruction and $0000F000)) and $FFFF));


///       iss_debug := true;
///       iss_dbgmsg := 'AUIPC @' + inttohex(PC, 4);

       //       set_reg32(rd_from_instr(instruction), (PC + (instruction and $0000F000)) and $FFFF);

       FPC := FPC + 4; // ?
    end;

    // x01 100 xx
    $30: begin
       funct3 := funct3_from_instr(instruction);

       rs1 := get_reg32( rs1_from_instr(instruction));
       rs2 := get_reg32( rs2_from_instr(instruction));

       case funct3 of
         0: begin
           if (instruction and $40000000)<>0 then
           begin
              //m := 'SUB';
              rd := rs1 - rs2;
           end else begin
              //m := 'ADD';
              rd := rs1 + rs2;
           end;
         end;
         1: begin
           //m := 'SLL';
           rd := rs1 shl (rs2 and $1F);
         end;
         2: begin
           //m := 'SLT';
           if integer(rs1) < integer(rs2) then rd := 1 else rd := 0;
         end;
         3: begin
           //m := 'SLTU';
           if rs1 < rs2 then rd := 1 else rd := 0;
         end;
         4: begin
           //m := 'XOR';
           rd := rs1 xor rs2;
         end;
         5: begin
           if (instruction and $40000000)<>0 then
           begin
              //m := 'SRA';
              t64 := rs1;
              if(t64 and $80000000) <>0 then t64 := t64 or $FFFFFFFF00000000;
              rd := t64 shr (rs2 and $1F);
           end else begin
              //m := 'SRL';
              rd := rs1 shr rs2;
           end;
         end;
         6: begin
           //m := 'OR';
           rd := rs1 or rs2;
         end;
         7: begin
           //m := 'AND';
           rd := rs1 and rs2;
         end;
       end;

       set_reg32(rd_from_instr(instruction), rd); //


       FPC := FPC + 4; // ?
    end;


    // x11 000 xx
    $60: begin
       //result := 'BRANCH';
       do_branch := false;
       rs1 := get_reg32( rs1_from_instr(instruction));
       rs2 := get_reg32( rs2_from_instr(instruction));

       case funct3_from_instr(instruction) of
          0: begin
            //m := 'BEQ ';
            do_branch := rs1 = rs2;
          end;
          1: begin
            //m := 'BNE ';
            do_branch := rs1 <> rs2;
          end;
          4: begin
            //m := 'BLT ';
            do_branch := integer(rs1) < integer(rs2);
          end;
          5: begin
            //m := 'BGE ';
            do_branch := integer(rs1) >= integer(rs2);
          end;
          6: begin
            //m := 'BLTU ';
            do_branch := rs1 < rs2;
          end;
          7: begin
            //m := 'BGEU ';
            do_branch := rs1 >= rs2;
          end;
       end;
       //
       //result := m +' r' +
       //   inttostr(rs1_from_instr(instruction)) + ', r' +
       //   inttostr(rs2_from_instr(instruction)) + ', ' +
       //   inttostr(imm_branch_from_instr(instruction))+' [' +inttohex(imm_branch_from_instr(instruction)+PC,4)+']';

       if do_branch then
       begin
          FPC := imm_branch_from_instr(instruction)+FPC; // ?
       end else begin
         FPC := FPC + 4; // ?
       end;



    end;
    // x11 001 xx
    // JALR
    $64: begin
       rs1 := get_reg32( rs1_from_instr(instruction)) + imm_11_0_from_instr(instruction);
       // needed
       rs1 := rs1 and $FFFFFFFE;

//       rs1 := rs1 and $FFFFFFFC;

       rd := FPC + 4;
       set_reg32(rd_from_instr(instruction), rd); //
       FPC := rs1;
    end;
    // x11 011 xx
    $6C: begin
       //result := 'JAL';
       rd := FPC + 4;
       set_reg32(rd_from_instr(instruction), rd); //
       FPC := imm_jal_from_instr(instruction) + FPC;
    end;
    // x11 100 xx


    $70: begin
       //m := 'SYSTEM - ILLEGAL';
       case funct3_from_instr(instruction) of
          0: begin
            if (instruction and $FFF00000) = $00100000 then
            begin
               //m := 'EBREAK';
               set_CSR($341, PC); // mepc
               set_CSR($342, 3);  // mcause = breakpoint
               PC := get_CSR($305); // mtvec
            end;
            if (instruction and $FFF00000) = $0 then
            begin
               //m := 'ECALL';
               set_CSR($341, PC); // mepc
               set_CSR($342, 11);  // mcause = ECALL from M-Mode
               PC := get_CSR($305); // mtvec
iss_debug := true;
iss_dbgmsg := '>>>>>>>>>ecall';

            end;
            if (instruction and $FFF00000) = $30200000 then
            begin
               //m := 'MRET';
               PC := get_CSR($341); // mepc
iss_debug := true;
iss_dbgmsg := '>>>>>>>>>mret';
            end;

            if (instruction and $FFF00000) = $10500000 then
            begin
               //m := 'WFI';
               //running := false;
               iss_status := SIM_WFI;
               FPC := FPC + 4; // ?

            end;

          end;
          // CSRRW
          1: begin
              c := get_CSR(instruction shr 20);
              set_CSR ((instruction shr 20),          get_reg32(rs1_from_instr(instruction)) );
              set_reg32(rd_from_instr(instruction),   c);
              FPC := FPC + 4; // ?
          end;
          // CSRRS
          2: begin
              c := get_CSR(instruction shr 20);
              set_CSR ((instruction shr 20),          c or get_reg32(rs1_from_instr(instruction)) );
              set_reg32(rd_from_instr(instruction),   c);
              FPC := FPC + 4; // ?
          end;
          // CSSRC
          3: begin
              c := get_CSR(instruction shr 20);
              set_CSR ((instruction shr 20),          c and (get_reg32(rs1_from_instr(instruction)) xor $FFFFFFFF) );
              set_reg32(rd_from_instr(instruction),   c);
              FPC := FPC + 4; // ?
          end;
          // CSRRWI
          5: begin
              c := get_CSR(instruction shr 20);
              set_CSR ((instruction shr 20),          rs1_from_instr(instruction) );
              set_reg32(rd_from_instr(instruction),   c);
              FPC := FPC + 4; // ?
          end;
          // CSRRSI
          6: begin
              c := get_CSR(instruction shr 20);
              set_CSR ((instruction shr 20),          c or rs1_from_instr(instruction) );
              set_reg32(rd_from_instr(instruction),   c);
              FPC := FPC + 4; // ?
          end;
          // CSRRCI
          7: begin
              c := get_CSR(instruction shr 20);
              set_CSR ((instruction shr 20),          c and (rs1_from_instr(instruction) xor $FFFFFFFF) );
              set_reg32(rd_from_instr(instruction),   c);
              FPC := FPC + 4; // ?
          end else begin
            running := false;
            FPC := FPC + 4; // ?
          end;
       end;

       //result := m;

    end else begin
      // Illegal..
      running := false;
      FPC := FPC + 4; // ?
    end;
  end;

  if (FPC and $3) <> 0 then
  begin
     set_CSR($341, old_pc); // mepc ??
     set_CSR($343, FPC);    // mtval
     set_CSR($342, 0);      // mcause = misalgin jmp
     PC := get_CSR($305);   // mtvec
  end;

  if misalign_ld then begin
     set_CSR($341, old_pc); // mepc ??
     set_CSR($343, ram_addr);    // mtval
     set_CSR($342, 4);      // mcause = misalgin ld
     PC := get_CSR($305);   // mtvec
  end;

  if misalign_st then begin
     set_CSR($341, old_pc); // mepc ??
     set_CSR($343, ram_addr);    // mtval
     set_CSR($342, 6);      // mcause = misalgin st
     PC := get_CSR($305);   // mtvec
  end;


  // sanity
  if (instruction and $3) <> 3 then running := false;
  if (instruction = $6f) then running := false;
  //if FPC = $0800 then running := false;

  inc(cycle, 1);

end;

end.
