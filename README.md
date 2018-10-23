# engine-V-simulator
Simulator for engine-V

Designed from scratch on saturday, simulator to pass the RISCV requirements as set by the RISCV SoftCPU Contest 2018.

Compiled with Delphi XE7, should compile under Tokyo too, but not tested. Test binaries included.

Status: 
passes all RV32I tests except misaligned load-store (not implemented in simulator)
runs Dhrystone
runs Zephyr Hello World

machine interrupt not implemented. Binary files are loaded to address 0, console UART is byte write to address 0xC000
