#!/bin/bash
dd if=/dev/zero of=floppy.img bs=1024 count=1440
dd if=./SysBoot/Stage1/Boot1.bin of=floppy.img bs=1 count=512 conv=notrunc

sudo /sbin/losetup /dev/loop0 floppy.img
sudo mount /dev/loop0 /mnt
sudo cp ./SysBoot/Stage2/KRNLDR.SYS /mnt/KRNLDR.SYS
sudo cp ./SysCore/KRNL32.EXE /mnt/KRNL32.EXE
sudo umount /dev/loop0
sudo /sbin/losetup -d /dev/loop0
