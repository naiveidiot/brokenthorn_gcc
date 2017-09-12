#!/bin/bash

# run.sh

make clean
make all
./update_image.sh
./run_qemu.sh
