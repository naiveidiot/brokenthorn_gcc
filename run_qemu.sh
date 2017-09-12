#!/bin/bash

# run_qemu.sh

qemu-system-i386 -drive format=raw,file=floppy.img,index=0,if=floppy -serial mon:stdio
