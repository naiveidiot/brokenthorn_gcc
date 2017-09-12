#!/bin/bash

sudo /sbin/losetup /dev/loop0 floppy.img
sudo mount /dev/loop0 /mnt
sudo cp ./SysCore/KRNL32.ELF /mnt/kernel
sudo umount /dev/loop0
sudo /sbin/losetup -d /dev/loop0
