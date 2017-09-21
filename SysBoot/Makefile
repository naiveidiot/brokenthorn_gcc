include ./Stage1/Makefile
Stage1/Boot1.bin:Stage1/Boot1.asm
	nasm -f bin Stage1/Boot1.asm -o Stage1/Boot1.bin

include ./Stage2/Makefile
Stage2/KRNLDR.SYS:Stage2/Stage2.asm
	nasm -i Stage2/ -f bin Stage2/Stage2.asm -o Stage2/KRNLDR.SYS
