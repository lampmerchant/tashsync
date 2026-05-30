# TashSync

Macintosh and Apple IIgs video sync signal converters/generators for PIC10F320.


## Project Status

**In limbo.**  The provided firmware may work in some cases, but cases have been found where it produces unacceptable jitter, and a fundamental redesign is necessary (and hopefully forthcoming).


## Technical Details

### Building Firmware

Building the firmware requires Microchip MPASM, which is included with their development environment, MPLAB. Note that you must use MPLAB X version 5.35 or earlier or MPLAB 8 as later versions of MPLAB X have removed MPASM.

The firmware can also be built using gpasm, an MPASM-compatible assembler that is part of the [gputils](https://gputils.sourceforge.io/) project.  Please note, however, that compatibility with gpasm is not tested.
