Linker Scripting
================

Numinous linker is unlike other linkers as it does not create object files but instead is used to map contents. The Linker Script must be written to abide the specified ``toml`` template described in docs here.

Example:

.. code-block:: toml

    [memory]
        ram         = [1, 0x0000, 0x8000]
        flash       = [0, 0x8000, 0x8000]

    [segments]
        direct      = ["ram",   0x0000, 0x0100]
        ram         = ["ram",   0x0100, 0x0700]
        rodata      = ["flash", 0x0000, 0x0140]
        boot        = ["flash", 0xf000, 0x1000]
        vectors     = ["flash", 0x7ffa, 6]

    [rules]
        fast        = ["direct", "ram"]
        slow        = ["ram", "direct"]
        coproc      = ["proc", "rodata"]

``memory`` contains the physical locations for information. Each array in ``memory`` acts as a struct: the first element is a 'write flag', the second element is the final pointer base offset and the third and final element is the run length (or size) of the memory component.

``segments`` contains the 'segments' of information hunks, all information given to a segment begins *from* the first pointer base offset (or the index of the segment) and must collectively fit inside the segment run length (or segment size). The names of components in ``memory`` are not special and are never visible in source.

``rules`` contain the 'flow' of information allocation attempts, if a member cannot fit within the first segment specified it will attempt the next in sequence with no default but total failure if none of the segments in the sequence have enough space to contain the information.

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
