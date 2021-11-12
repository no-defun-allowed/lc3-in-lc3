# Metacircular LC-3 implementation

![](demo.gif)

This is a joke gone *way* too far. Unfortunately it allows you to run
LC3 programs in a VM written in LC-3. I don't know of any practical
uses for this.

The `macro-assembler` directory has the macro assembler, which roughly
brings up a LC-3 assembler (I tested with laser) to nasm-ish
standards. The `vm` directory has a VM implementation based on [this
guide](https://justinmeiners.github.io/lc3-vm/), specifically the C++
template part. However, as code density is important with LC-3 and its
tiny PC-relative ranges, we instead have each "part" be compiled exactly
once, with threaded "microcode" sequencing parts.
