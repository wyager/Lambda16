This is my Cλash/Haskell implementation of a microprocessor for UT's CS 350C Advanced Architecture class.

The CPU is defined in CPU/CPU.hs.

Features:
* 4-stage pipeline (Fetch, Decode, Wait, Writeback)
* Flexible microcode rewriting (to support the `Ldr` instruction, which has both RAM and register dependencies)
* Dynamic opportunistic instruction rewriting
(e.g. `Add a b c` becomes `Mov (r[a] + r[b]) c` as soon as `r[a]` and `r[b]` are known)
* Opportunistic jumping (`Jmp` happens immediately, `Jeq` happens when `r[a]` and `r[b]` are known)
* Fully associative LRU memory and register write caches (size tunable by type parameter, default 8)
* Fully simulatable without ever generating an HDL
* Can target VHDL, Verilog, SystemVerilog

To generate an HDL:

    clash --<HDL name> CPU/Adapter.hs

Adapter.hs is a file that converts my ADTs into simpler wire types 
(not strictly necessary, but makes things a bit clearer) and throws
away the debug port.

To simulate CPU:

    clash Main.hs
    ./Main

This will load "mem.hex" into RAM, dump the program in human-readable format,
and simulate the CPU. Formatted output comes from
Testbed.hs. If you get an error about "Prelude.head: empty list" that means
the program tried to read from unititialized RAM. 