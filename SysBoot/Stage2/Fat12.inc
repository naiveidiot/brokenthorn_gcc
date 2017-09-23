
;*******************************************************************************
;  Fat12.inc
;    FAT12 filesystem for 3-1/2 floppies
;
;  OS Development Series
;*******************************************************************************

%ifndef __FAT12_INC_67343546FDCC56AAB872_INCLUDED__
%define __FAT12_INC_67343546FDCC56AAB872_INCLUDED__

bits  16

%include "Floppy16.inc"          ; the erm.. floppy driver

%define ROOT_OFFSET 0x2e00
%define FAT_SEG 0x2c0
%define ROOT_SEG 0x2e0

;*******************************************************************************
; LoadRoot
; ========
; DESC: Loads the root directory at 0x2E00
; PRE :
; POST: The root directory table is loaded at 0x2E00
; IN  :
; OUT :
; NOTE: Size of root directory = 224(bpbRootEntries) x 32 = 7168 bytes. The table takes 0x2E00 to 0x4A00
;*******************************************************************************

LoadRoot:

  pusha
  push   es

  ; compute size of root directory and store in "CX"
  xor    cx, cx
  xor    dx, dx
  mov    ax, 32                        ; 32 byte directory entry
  mul    WORD [bpbRootEntries]          ; total size of directory
  div    WORD [bpbBytesPerSector]      ; sectors used by directory
  xchg   ax, cx            ; CX = number of sctors used by root directory, i.e, number of sectors to read into buffer

  ; compute starting location of root directory and store in "ax"
  mov    al, BYTE [bpbNumberOfFATs]      ; number of FATs
  mul    WORD [bpbSectorsPerFAT]          ; sectors used by FATs
  add    ax, WORD [bpbReservedSectors]
  ; store starting sector of data region
  mov    WORD [datasector], ax            ; starting sector of root directory
  add    WORD [datasector], cx            ; add size of root directory to get starting sector of data region

  ; load root directory into memory starting at 0x2E00
  push  word ROOT_SEG
  pop    es
  mov    bx, 0
  call   ReadSectors
  pop    es
  popa
  ret

;*******************************************************************************
; LoadFAT
; =======
; DESC: Load FATs to 0x2C00
; PRE :
; POST:
; IN  : ES:BX points to the beginning of the buffer address
; OUT :
; NOTE:
;*******************************************************************************

LoadFAT:
  pusha
  push   es

  ; compute size of FATs and store in "CX"
  xor    ax, ax
  mov    al, BYTE [bpbNumberOfFATs]
  mul    WORD [bpbSectorsPerFAT]
  mov    cx, ax                          ; number of sectors used by FATs
  mov    ax, WORD [bpbReservedSectors]   ; first sector of FAT

  ; load FATs into memory starting at 0x2C00
  push   word FAT_SEG
  pop    es
  xor    bx, bx
  call   ReadSectors
  pop    es
  popa                                    ; restore registers and return
  ret

;*******************************************************************************
; FindFile
; ========
; DESC: Search for filename in root table
; PRE :
; POST:
; IN  : DS:SI => File name
; OUT : ES:DI points to the entry found. AX = -1 if not found,
; NOTE:
;*******************************************************************************


FindFile:

  push   cx                             ; store registers
  push   dx
  push   bx
  mov    bx, si                         ; copy filename for later

  ; browse root directory for binary image
  mov    cx, WORD [bpbRootEntries]      ; load loop counter
  mov    di, ROOT_OFFSET
  cld

.LOOP:
  push   cx
  mov    cx, 11               ; eleven character name. Image name is in SI
  mov    si, bx               ; image name is in BX
  push   di
  rep    cmpsb                ; test for entry match
  pop    di
  je     .Found
  pop    cx
  add    di, 32               ; queue next directory entry
  loop   .LOOP

.NotFound:
  pop    bx                   ; restore registers and return
  pop    dx
  pop    cx
  mov    ax, -1               ; set error code
  ret

.Found:
  pop    ax                   ; return value into AX contains entry of file
  pop    bx                   ; restore registers and return
  pop    dx
  pop    cx
  ret

;*******************************************************************************
; LoadFile
; ========
; DESC: Load the file into memory
; PRE :
; POST: The file is loaded in the buffer.
; IN  : DS:SI points to the file name, EBX:BP points to the starting address of the buffer
; OUT : AX = -1 on error, 0 on success, ECX = total number of sectors read, i.e size of the file, BX = the address of the last byte of the file.
; NOTE:
;*******************************************************************************

LoadFile:
  ; ECX stores the total number of sectors loaded
  xor    ecx, ecx
  push   ecx
  .FIND_FILE:
  mov    es, bx
  mov    bx, bp
  push   es
  push   bx
  call   FindFile      ; DS:SI => File name ; ret AX = -1 if not found ; ret ES:DI points to the entry found
  cmp    ax, -1
  jne    .LOAD_IMAGE_PRE
  pop    bp
  pop    bx
  pop    ecx           ; ECX = 0, no sectors is read
  mov    ax, -1
  ret

  .LOAD_IMAGE_PRE:

  mov    dx, WORD [di + 0x001A]    ; DI is the offset to file entry from the start of the root entry, bytes 26th-27th is the first cluster of the file
  mov    WORD [cluster], dx        ; file's first cluster
  call   LoadFAT         ; load FATs to starting 0x2C00

  .LOAD_IMAGE:
  ; load the cluster
  mov    ax, WORD [cluster]                 ; cluster to read
  ; restore ES:BX, the image buffer's address
  pop    bx
  pop    es

  call   ClusterLBA
  xor    cx, cx
  mov    cl, BYTE [bpbSectorsPerCluster]

  call   ReadSectors
  ; inc number of sectors read and save on the stack
  pop    ecx
  inc    ecx
  push   ecx

  push   es
  push   bx

  mov    ax, FAT_SEG
  mov    es, ax
  xor    bx, bx
  mov    ax, WORD [cluster]
  mov    cx, ax
  mov    dx, ax
  shr    dx, 0x0001
  add    cx, dx

  mov    bx, 0
  add    bx, cx
  mov    dx, WORD [es:bx]
  test   ax, 0x0001                         ; test for odd or even cluster
  jnz    .ODD_CLUSTER

  .EVEN_CLUSTER:
  and    dx, 0000_1111_1111_1111b           ; take low 12 bits
  jmp    .DONE

  .ODD_CLUSTER:
  shr    dx, 0x0004                         ; take high 12 bits

  .DONE:
  mov    WORD [cluster], dx
  cmp    dx, 0x0ff0                         ; test for end of file marker
  jb     .LOAD_IMAGE

  .SUCCESS:
  pop    bx
  pop    es
  pop    ecx                                ; ECX = total numbers of sectors read
  xor    ax, ax
  ret

%endif    ;__FAT12_INC_67343546FDCC56AAB872_INCLUDED__
