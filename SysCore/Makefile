ARCH=ARCH_X86
KERNEL=KRNL32.ELF
INCDIR= -I ./Include -I ./Kernel

include ./Kernel/config.make
include ./Kernel/Makefile
include ./Lib/Makefile

FLAG :=$(FLAG) -D__$(ARCH)__

all: $(KERNEL)

$(KERNEL): $(OBJS)
	$(LD) $(LDFLAG) -o $@ $^
	#cp $(KERNEL) $(SDKDIR)/bootdisk/

help:
	@echo "Makefile for Kernel."
	@echo "Please see COPYING for licensing information."
	@echo "Output should be: "$(KERNEL)
	@echo "Usage: make [ all | clean] "

%.o: %.cc
	$(SC) $(FLAG) -c $< -o  $@

%.o: %.S
	$(SC) $(FLAG) -c $< -o  $@

%.o: %.asm
	$(ASM) $(ASMFLAG)  -c $< -o  $@


clean:
	rm -f $(OBJS)  $(KERNEL)
