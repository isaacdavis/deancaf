#
# This program runs in 32-bit protected mode.
#  build: nasm -f elf -F stabs name.asm
#  link:  ld -o name name.o
#
# In 64-bit long mode you can use 64-bit registers (e.g. rax instead of eax, rbx instead of ebx, etc.)
# Also change "-f elf " for "-f elf64" in build command.

#include <stdio.h>

.data
str:     .string "Hello world!\n"
str_len = . - str

.text
.globl main

main:
	mov	$0x4, %eax
	mov	$0x1, %ebx
	mov	$str, %ecx
	mov	$str_len, %edx
	int	$0x80
	mov	$0x1, %eax
	mov	$0x2, %ebx
	int	$0x80
	ret
