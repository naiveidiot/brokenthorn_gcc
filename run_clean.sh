#!/bin/bash

# run.sh

make clean
make all
nasm -f bin ./SysBoot/Stage1/Boot1.asm -o ./SysBoot/Stage1/Boot1.bin
nasm -i./SysBoot/Stage2/ -f bin ./SysBoot/Stage2/Stage2.asm -o ./SysBoot/Stage2/KRNLDR.SYS

./update_image.sh
./run_qemu.sh
