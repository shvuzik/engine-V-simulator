# engine-V-simulator
Simulator for engine-V

Designed from scratch on saturday, simulator to pass the RISCV requirements as set by the RISCV SoftCPU Contest 2018.

Compiled with Delphi XE7, should compile under Tokyo too, 

Get free [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter/free-download) if you want to try out.

Test binaries included https://github.com/micro-FPGA/engine-V-simulator/tree/master/Win32/Debug/images

Status: 
* passes all RV32I tests except misaligned load-store (not implemented)
* runs Dhrystone
* runs Zephyr Hello World

machine timer interrupt not implemented

Binary files are loaded to address 0

Console UART is byte write to address 0xC000 (C as in Console)
