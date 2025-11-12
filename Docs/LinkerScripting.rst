Linker Scripting
================

Numinous linker is unlike other linkers as it does not create object files but instead is used to map contents. The Linker Script must be written to abide the specified ``toml`` template described in docs here.

Example:

.. code-block:: toml

    [dynamic]
        [dynamic.system]
            offset = 0          # address is derived from offset with bss regions (it does not contribute to file out)
            width  = 0x800      # width of memory hunk (physical)

            [dynamic.system.direct]
                offset = 0      # relative offset to dynamic.system
                width  = 256    # direct refers to 'zero page' or the first page

            [dynamic.system.stack]
                offset = 256    # relative offset to dynamic.system
                width  = 256    # the stack lies from 0x100 to 0x200


    [static]
        [static.prgrom]
            address = 0x8000    # with the NES in mind, ROM begins at 0x8000
            offset  = 0         # in file, this is where the content begins
            width   = 0x8000    # it is 32KiB in size

            [static.prgrom.rodata]
                offset = 0      # the rodata begins at the first ROM page
                width  = 256    # page aligned data is faster to access

            [static.prgrom.vectors]
                offset = 0x7ffa # the vectors may only lie in the last
                width  = 6      # 6 bytes of CPU

        [static.chrrom]
            address = 0         # required by linker, nothing else
            offset = 0x8000     # NES ROMs demand the CHR to be stored after PRG
            width  = 0x2000     # the minimum is 8KiB

    # ordered in order of link
    [rules]
        direct  = ["dynamic.system.direct"]
        fast    = ["dynamic.system.direct", "dynamic.system"]
        proc    = ["static.prgrom", "static.prgrom.rodata"]
        table   = ["static.prgrom.rodata", "static.prgrom"]
        slow    = ["dynamic.system", "dynamic.system.direct"]

Segments and Memory are unified with ``uhla`` separated by constant or deferred allocation. Top level segments do **not** overlap each other and are not considered to share the same space to allow representing entirely different physical locations.

The offset of a segment refers to the position it takes up relative to its parent segment, the width describes the size of the segment in bytes and is absolute such that all child segments must fit within the specified size.

Static top level segments contain an additional ``address`` field which is added to the final absolute offset of any static segment.


To allocate memory to a specified segment, the user simply types:

.. code-block::

    direct u8 MyFoo         // will use 'direct' segment
    fast u8 MyBar           // uses rule 'fast' trying to use 'direct' but uses 'ram' if fails

To write a procedure to a specified segment, the user simply types:

.. code-block::

    .boot init {
        // example code for 'boot' segment here
    }

    .coproc cop_init {
        // example code to offload to a coprocessor (eg)
    }

To reserve non-variable information for a specified segment, the user simply types:

.. code-block::

    rodata u8[] Table {
        // const information for the rodata table
    }

    coproc i8[] CoTable {
        // const information that follows the 'coproc' information allocation rule
    }

    // a ptr
    rodata u16 ptr

    // a hook to a coroutine
    coproc i16 hook

It's known to be ``const`` without specifying as the ``memory`` specified in the ``segment`` has the write flag clear.


To provide information without specifying a target, simply use the following:

.. code-block::

    .proc MyProcedure {
        // Procedure that can be in any block
        // proc will simply search for non-writable segment
    }

    const u8[] Table {
        // Table that can be in any block
        // const will simply find a non-writable segment
    }

    // constant that can be in any block
    // const will simply find a non-writable segment
    const u16 ptr

Platform Linker Script Intelligence (PLSI)
##########################################

Architectures leverage the linker script for the linker script provided directives, but certain platforms use the Linker Script for additional purposes.

NMOS 6502
*********
