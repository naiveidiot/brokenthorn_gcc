#!/bin/bash

# run.sh
make all
./update_image.sh
./run_qemu.sh
