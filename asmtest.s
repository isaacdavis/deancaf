#
# This program runs in 32-bit protected mode.
#  build: nasm -f elf -F stabs name.asm
#  link:  ld -o name name.o
#
# In 64-bit long mode you can use 64-bit registers (e.g. rax instead of eax, rbx instead of ebx, etc.)
# Also change "-f elf " for "-f elf64" in build command.

.data
_V$Object:
_V$String: .long _$DecafMain

.text
.globl _V$String
.globl _$DecafMain

_$DecafMain:
	enter $0x64, $0x0
	mov 8(%ebp), %eax
	add $0x4, %eax
	push (%eax)
	call IO$putString
	mov $0, %eax
	leave
	ret
