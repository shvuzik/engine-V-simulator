object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 
    'RISC-V SoftCPU Contest 2018 - Simulator v 1.0 (License Apache 2.' +
    '0)'
  ClientHeight = 951
  ClientWidth = 1321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 903
    Top = 551
    Width = 101
    Height = 13
    Caption = 'Signature located at:'
  end
  object Label1: TLabel
    Left = 903
    Top = 742
    Width = 99
    Height = 13
    Caption = 'Reference Signature'
  end
  object Label3: TLabel
    Left = 327
    Top = 14
    Width = 51
    Height = 13
    Caption = 'Breakpoint'
  end
  object Edit1: TEdit
    Left = 8
    Top = 37
    Width = 90
    Height = 21
    TabOrder = 0
    Text = 'misalign_ldst'
  end
  object l: TMemo
    Left = 8
    Top = 64
    Width = 329
    Height = 672
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 136
    Top = 33
    Width = 185
    Height = 25
    Caption = 'Execute Compliance Test'
    TabOrder = 2
    OnClick = Button2Click
  end
  object s: TMemo
    Left = 903
    Top = 570
    Width = 304
    Height = 166
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object Button4: TButton
    Left = 1212
    Top = 568
    Width = 101
    Height = 25
    Caption = 'Save Signature'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Edit2: TEdit
    Left = 104
    Top = 37
    Width = 26
    Height = 21
    TabOrder = 5
    Text = '01'
  end
  object sb: TStatusBar
    Left = 0
    Top = 932
    Width = 1321
    Height = 19
    Panels = <
      item
        Text = 'IDLE'
        Width = 50
      end>
  end
  object ref: TMemo
    Left = 903
    Top = 761
    Width = 304
    Height = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object Button3: TButton
    Left = 327
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Dhrystone'
    TabOrder = 8
    OnClick = Button3Click
  end
  object c: TMemo
    Left = 343
    Top = 594
    Width = 554
    Height = 142
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object Button1: TButton
    Left = 1230
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 10
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 136
    Top = 8
    Width = 185
    Height = 25
    Caption = 'Run engine-V'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 408
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Zephyr'
    TabOrder = 12
    OnClick = Button6Click
  end
  object EditBreak: TEdit
    Left = 408
    Top = 6
    Width = 75
    Height = 21
    TabOrder = 13
    Text = 'FFF0'
  end
  object Button7: TButton
    Left = 489
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Step'
    TabOrder = 14
    OnClick = Button7Click
  end
  object Panel1: TPanel
    Left = 696
    Top = 64
    Width = 609
    Height = 481
    TabOrder = 15
    object mr: TMemo
      Left = 130
      Top = 16
      Width = 57
      Height = 457
      TabOrder = 0
    end
    object dump: TMemo
      Left = 193
      Top = 45
      Width = 408
      Height = 428
      OEMConvert = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object Button11: TButton
      Left = 193
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Dump'
      TabOrder = 2
      OnClick = Button11Click
    end
    object r: TMemo
      Left = 16
      Top = 16
      Width = 108
      Height = 457
      TabOrder = 3
    end
  end
  object ml: TMemo
    Left = 343
    Top = 67
    Width = 347
    Height = 521
    ScrollBars = ssVertical
    TabOrder = 16
  end
  object Button9: TButton
    Left = 489
    Top = 8
    Width = 75
    Height = 25
    Caption = 'u Step'
    TabOrder = 17
    OnClick = Button9Click
  end
  object cbList: TCheckBox
    Left = 593
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Listing'
    TabOrder = 18
  end
  object cbListMicro: TCheckBox
    Left = 593
    Top = 31
    Width = 97
    Height = 17
    Caption = 'Microcode Listing'
    TabOrder = 19
  end
  object log: TMemo
    Left = 136
    Top = 753
    Width = 241
    Height = 173
    ScrollBars = ssVertical
    TabOrder = 20
  end
  object memoTests: TMemo
    Left = 8
    Top = 784
    Width = 122
    Height = 142
    Lines.Strings = (
      'ADD'
      'ADDI'
      'AND'
      'ANDI'
      'AUIPC'
      'BEQ'
      'BGE'
      'BGEU'
      'BLT'
      'BLTU'
      'BNE'
      'CSRRC'
      'CSRRCI'
      'CSRRS'
      'CSRRSI'
      'CSRRW'
      'CSRRWI'
      'DELAY_SLOTS'
      'EBREAK'
      'ECALL'
      'ENDIANESS'
      'FENCE.I'
      'IO'
      'JAL'
      'JALR'
      'LB'
      'LBU'
      'LH'
      'LHU'
      'LUI'
      'LW'
      'MISALIGN_JMP'
      'MISALIGN_LDST'
      'NOP'
      'OR'
      'ORI'
      'RF_size'
      'RF_width'
      'RF_x0'
      'SB'
      'SH'
      'SLL'
      'SLLI'
      'SLT'
      'SLTI'
      'SLTIU'
      'SLTU'
      'SRA'
      'SRAI'
      'SRL'
      'SRLI'
      'SUB'
      'SW'
      'XOR'
      'XORI')
    ScrollBars = ssVertical
    TabOrder = 21
  end
  object Button8: TButton
    Left = 8
    Top = 753
    Width = 122
    Height = 25
    Caption = 'EXEC ISS'
    TabOrder = 22
    OnClick = Button8Click
  end
  object Button10: TButton
    Left = 383
    Top = 753
    Width = 122
    Height = 25
    Caption = 'EXEC MF8A18'
    TabOrder = 23
    OnClick = Button10Click
  end
  object memoTests2: TMemo
    Left = 383
    Top = 784
    Width = 122
    Height = 142
    Lines.Strings = (
      'ADD'
      'ADDI'
      'AND'
      'ANDI'
      'AUIPC'
      'BEQ'
      'BGE'
      'BGEU'
      'BLT'
      'BLTU'
      'BNE'
      'CSRRC'
      'CSRRCI'
      'CSRRS'
      'CSRRSI'
      'CSRRW'
      'CSRRWI'
      'DELAY_SLOTS'
      'EBREAK'
      'ECALL'
      'ENDIANESS'
      'FENCE.I'
      'IO'
      'JAL'
      'JALR'
      'LB'
      'LBU'
      'LH'
      'LHU'
      'LUI'
      'LW'
      'MISALIGN_JMP'
      'MISALIGN_LDST'
      'NOP'
      'OR'
      'ORI'
      'RF_size'
      'RF_width'
      'RF_x0'
      'SB'
      'SH'
      'SLL'
      'SLLI'
      'SLT'
      'SLTI'
      'SLTIU'
      'SLTU'
      'SRA'
      'SRAI'
      'SRL'
      'SRLI'
      'SUB'
      'SW'
      'XOR'
      'XORI')
    ScrollBars = ssVertical
    TabOrder = 24
  end
  object log2: TMemo
    Left = 511
    Top = 753
    Width = 241
    Height = 173
    ScrollBars = ssVertical
    TabOrder = 25
  end
  object cbConsole: TCheckBox
    Left = 712
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Console'
    Checked = True
    State = cbChecked
    TabOrder = 26
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 992
    Top = 16
  end
end
