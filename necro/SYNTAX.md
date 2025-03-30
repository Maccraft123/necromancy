
# Directives
## `cpu`

Declare the CPU using a compatible string, for example:
`.cpu "intel,8080"`

Tells the assembler that CPU ISA is compatible with Intel 8080

## `os`

Declare the OS using a compatible string, for example:
`.os "dr,cp/m-2.2"

Tells the assembler that the OS is a CP/M v2.2

For CP/M it does the following:
- Sets the string termination for OS-defined string to `$`
- Adds a constant symbol `bdos` with addr of `0x0005`
- Adds constant values corresponding to BDOS calls of CP/M 
- Puts the code section at `0x0100`
- Puts a jump to `entry` symbol if code section doesn't begin at `0x0100`

## `import <name>`

Adds a `<name>` symbol to list of symbols that will be resolved by the linker.

## `export <name>`

Adds `<name>` symbol to list of symbols that can be used in other object files.
`entry` label is automatically added.

## `define_sym <name> <addr>`

Adds a `<name>` symbol with address of `<addr>`. For example `define_sym bdos 0x5` allows usage of
`call bdos` in code.

## `section <name>`

Begins a section named `<name>`. Note that the name is used only for permissions and flags:
- `code` -> read and execute
- `data` -> read and write
- `bss` -> read and write, don't store the section data on disk

## Strings

Strings can be defined using a variety of directives, and their representation depends on the
directive and OS/ABI.

String escape sequences roughly match what is seen in C, with limitation as to what escape
sequences are supported:
- `\n`, `\r`, `\t`, `\\` matching definition in C

### `.byte_str <data>`

Defines a string that is just a bunch of ASCII characters, without any termination or length data.

### `.os_str <data>`

Defines a string with `<data>` that is defined by the OS ABI.
- CP/M puts a `$` character at the end
- Various Unixes and Unix-like OSes put a `\0` byte at the end
- Classic Mac Os prefixes the string with its lenght

In the case of char-terminated string, the assembler checks if the characters are present in the
string and aborts the assemly process if they are present.

## `<name>:`

Defines a label with `<name>`. Labels are symbols with value such that jumping to a label value
causes the processor to execute instructions beginning after the label.
Labels can be exported, and `entry` label is automatically exported.

# Instructions:

Instructions are generally ISA-defined, with some notes:
Number radix is defined by the number suffix or prefix.
If there is neither a suffix, nor a prefix the number is base 10.
When parsing numbers, underscore characters are ignored.

Prefixes:
- Hexadecimal numbers: `0x` or `$`
- Octal numbers: `0o`
- Binary numbers: `0b`

Suffixes:
- Hexadecimal numbers: `h`
