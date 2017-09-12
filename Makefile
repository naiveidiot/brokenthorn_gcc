help:
	@echo "Makefile for Building Brokenthorn OS Tutorials."
	@echo "Usage: make [ all | clean | help | build | run] "
	@echo ""
	@echo

all:
	@echo "Building Kernel"
	make -C ./SysCore
	#@echo "Building Bootloader"
	#make -C ./SysBoot

clean:
	make -C ./SysCore clean
	#make -C ./SysBoot clean
