
;*******************************************************************************
; Boot1.asm
; =========
; DESC: A simple 1st stage boot loader for FAT12 floppy disk
;*******************************************************************************


bits  16                ; we are in 16 bit real mode

org    0                ; we will set regisers later

start:
  jmp     main          ; jump to start of bootloader

;*******************************************************************************
; BIOS Parameter Block
; ===================
; DESC:
; PRE :
; POST:
; IN  :
; OUT :
; NOTE:
;*******************************************************************************

bpbOEM:                   DB "My OS   "
bpbBytesPerSector:        DW 512
bpbSectorsPerCluster:     DB 1
bpbReservedSectors:       DW 1
bpbNumberOfFATs:          DB 2
bpbRootEntries:           DW 224
bpbTotalSectors:          DW 2880
bpbMedia:                 DB 0xf0  ;; 0xF1
bpbSectorsPerFAT:         DW 9
bpbSectorsPerTrack:       DW 18
bpbHeadsPerCylinder:      DW 2
bpbHiddenSectors:         DD 0
bpbTotalSectorsBig:       DD 0
bsDriveNumber:            DB 0
bsUnused:                 DB 0
bsExtBootSignature:       DB 0x29
bsSerialNumber:           DD 0xa0a1a2a3
bsVolumeLabel:            DB "MOS FLOPPY "
bsFileSystem:             DB "FAT12   "

;*******************************************************************************
; Print
; ======
; DESC: Print a zero terminated string to vga out.
; PRE :
; POST: The string is printed on the screen.
; IN  : DS:SI point to the zero terminated string
; OUT :
; NOTE:
;*******************************************************************************

Print:
  lodsb                 ; load next byte from string from SI to AL
  or      al, al        ; Does AL=0?
  jz      PrintDone     ; Yep, null terminator found-bail out
  mov     ah, 0eh       ; Nope-Print the character
  int     10h
  jmp     Print         ; Repeat until null terminator found
  PrintDone:
  ret                   ; we are done, so return


absoluteSector: db 0x00
absoluteHead:   db 0x00
absoluteTrack:  db 0x00

;*******************************************************************************
; ClusterLBA
; ==========
; DESC: Convert cluster number to logical block address (LBA)
; PRE : WORD [datasector] must contain the first sector of data area
; POST:
; IN  : AX contains cluster number
; OUT : AX contains LBA of the cluster
; NOTE: Cluster number to LBA conversion formula:
;         LBA = (cluster no - 2) * sectors per cluster + first data sector
;*******************************************************************************

ClusterLBA:
  sub     ax, 0x0002                          ; zero base cluster number
  xor     cx, cx
  mov     cl, BYTE [bpbSectorsPerCluster]     ; convert byte to word
  mul     cx
  add     ax, WORD [datasector]               ; base data sector
  ret

;*******************************************************************************
; LBACHS
; ======
; DESC: convert logical block address to cylinder head sector numbers
; PRE :
; POST:
; IN  : AX = LBA to convert
; OUT : absoluteTrack, absoluteHead and absoluteSector variables are set with cylinder, head and secotrs numbers respectively
; NOTE: LBA to CHS coonversion formula
;          Sector = (LBA % SectorsPerTrack) + 1
;          Head = (LBA / SectorsPerTrack) % NumberOfHeads
;          Cylinder = (LBA /SectorsPerTrack) / NumberOfHeads
;          Source:http://www.osdever.net/tutorials/view/lba-to-chs
;*******************************************************************************

LBACHS:
  xor     dx, dx
  div     WORD [bpbSectorsPerTrack]
  ;  al contains the quotient, dl contains the remainder
  ;  i.e. AL = LBA / SectorsPerTrack
  ;       DL = LBA % SectorsPerTrack
  inc     dl                                  ; adjust for sector 0
  mov     BYTE [absoluteSector], dl
  xor     dx, dx
  div     WORD [bpbHeadsPerCylinder]
  ; Now, al = (LBA / SectorsPerTrack) / NumberOfHeads = Cylinder
  ;      dl = (LBA / SectorsPerTrack) % NumberOfHeads = Head
  mov     BYTE [absoluteHead] , dl
  mov     BYTE [absoluteTrack], al
  ret

;*******************************************************************************
; ReadSectors
; ======
; DESC: read sequencially series of sectors in a cluster into memory
; PRE :
; POST:
; IN  : AX = LBA, CX = Number of sectors to read, ES:BX points of the starting memory address to read the sectors to to
; OUT :
; NOTE: Used to load sectors in a cluster into memory
;*******************************************************************************

ReadSectors:
  .MAIN:
  mov     di, 0x0005                          ; five retries for error read a sector until it's successfully read
  .SECTORLOOP:
    push    ax
    push    bx
    push    cx
    call    LBACHS                              ; convert starting sector to CHS
    mov     ah, 0x02                            ; read sector function
    mov     al, 0x01                            ; read one sector
    mov     ch, BYTE [absoluteTrack]            ; track
    mov     cl, BYTE [absoluteSector]           ; sector
    mov     dh, BYTE [absoluteHead]             ; head
    mov     dl, BYTE [bsDriveNumber]            ; drive
    int     0x13                                ; invoke BIOS
    jnc     .SUCCESS                            ; test for read error.
    ;We are here because reading the sector was failed. So, retry.
    xor     ax, ax                              ; BIOS reset disk
    int     0x13                                ; invoke BIOS
    dec     di                                  ; decrement error counter
    pop     cx
    pop     bx
    pop     ax
    jnz     .SECTORLOOP                         ; retry to read the same
    int     0x18                                ; quit in case the sector can't be read after 5 retries
  .SUCCESS:
    mov     si, msgProgress
    call    Print
    pop     cx
    pop     bx
    pop     ax
    add     bx, WORD [bpbBytesPerSector]        ; queue next buffer
    inc     ax                                  ; queue next sector
    loop    .MAIN                               ; read next sector
    ret

main:

  ;----------------------------------------------------
  ; code located at 0000:7C00, adjust segment registers
  ;----------------------------------------------------

  cli                       ; disable interrupts
  mov     ax, 0x07C0        ; setup registers to point to our segment
  mov     ds, ax
  mov     es, ax
  mov     fs, ax
  mov     gs, ax

  ;----------------------------------------------------
  ; create stack
  ;----------------------------------------------------

  mov     ax, 0x0000        ; set the stack
  mov     ss, ax
  mov     sp, 0xFFFF
  sti            ; restore interrupts

  ;----------------------------------------------------
  ; Display loading message
  ;----------------------------------------------------

  mov     si, msgLoading
  call    Print

  ;-----------------------------------------------------------------------------
  ; DESC:  Load the root directory after this boot loader, i.e. starting at 7E00
  ; PRE :
  ; POST: The root directory is loaded at 0x7E00
  ; IN  :
  ; OUT :
  ; Note: Sectors before FAT1 are taken as reserved sectors. We only have one,
  ;        which is the boot sector. That's why "bpbReservedSectors:   DW 1"
  ; ||Boot Sector...|FAT1...|FAT2...|Root Dir...|Data Region... ...
  ;-----------------------------------------------------------------------------

  LOAD_ROOT:

    ; compute size of root directory and store in CX
    xor     cx, cx
    xor     dx, dx
    mov     ax, 0x0020                           ; 32 byte directory entry
    mul     WORD [bpbRootEntries]                ; total size of directory
    div     WORD [bpbBytesPerSector]             ; sectors used by directory
    xchg    ax, cx                               ; size of root dir in CX

    ; compute location of root directory and store in "ax"
    mov     al, BYTE [bpbNumberOfFATs]            ; number of FATs
    mul     WORD [bpbSectorsPerFAT]               ; sectors used by FATs
    add     ax, WORD [bpbReservedSectors]         ; adjust for bootsector
    ; save starting sector of data area
    mov     WORD [datasector], ax                 ; base of root directory
    add     WORD [datasector], cx                 ; add size of root dir

    ; Now that CX => Size of root dir (in sectors), AX => Starting sector of   root   dir
    ; we can read root directory into memory (0x7C00:0x0200 = 0x7e00)
    mov     bx, 0x0200                            ; copy root dir above bootcode
    call    ReadSectors

    ;----------------------------------------------------
    ; Find stage 2
    ;----------------------------------------------------

    ; CMPSB: Both source operands are located in memory. The address of the first
    ; source operand is read from DS:SI, DS:ESI or RSI (depending on the address
    ; -size attribute of the instruction is 16, 32, or 64, respectively). The     address
    ; of the second source operand is read from ES:DI, ES:EDI or RDI (again     depending
    ; on the address-size attribute of the instruction)
    ; After the comparison, the (E/R)SI and (E/R)DI registers increment or   decrement
    ; automatically according to the setting of the DF flag in the EFLAGS   register.
    ; Logic:   CMP (DS:SI), (ES:DI)           ; Sets flags only
    ;             if DF = 0
    ;                 SI    SI + 1
    ;                 DI    DI + 1
    ;             else
    ;                 SI    SI - 1
    ;                 DI    DI - 1


  ; browse root directory for binary image
  FIND_FILE:
    mov     cx, WORD [bpbRootEntries]             ; load loop counter
    mov     di, 0x0200                            ; locate first root entry
    .LOOP:
    push    cx
    mov     cx, 0x000B                            ; eleven character name
    mov     si, ImageName                         ; image name to find
    push    di
    rep     cmpsb                                 ; test for entry match
    pop     di                                    ; start address of entry
    je      LOAD_FAT
    pop     cx
    add     di, 0x0020                            ; queue next directory entry
    loop    .LOOP
    jmp     FAILURE

  ;-----------------------------------------------------------------------------
  ; LOAD_FAT
  ; ========
  ; DESC: Load the FAT's after boot loader, i.e. at 0x7E00. Override the root   directory previously loaded there.
  ; PRE : ES:DI points to the the root directory entry of the image file
  ; POST:
  ; IN  :
  ; OUT :
  ; NOTE:
  ;-----------------------------------------------------------------------------

  LOAD_FAT:
    ;--------------------------------------------------
    ; Save starting cluster of boot image
    ; ES:DI = start address of entry, byte number 26 and 27 store the first
    ; cluster of the file
    ;--------------------------------------------------
    mov     dx, WORD [di + 0x001A]
    mov     WORD [cluster], dx                  ; file's first cluster

    xor     ax, ax
    mov     al, BYTE [bpbNumberOfFATs]          ; number of FATs
    mul     WORD [bpbSectorsPerFAT]             ; sectors used by FATs
    mov     cx, ax

    ; compute location of FAT and store in "ax"

    mov     ax, WORD [bpbReservedSectors]       ; adjust for bootsector

    ; Read FAT into memory (7C00:0200). Remember that we've set ES to 0x7C00. So, ES:BX = 0x7C00:0x200 =0x7E00, which is right after this boot loader.

    mov     bx, 0x0200                          ; copy FAT above bootcode
    call    ReadSectors

;----------------------------------------------------
; Load Stage 2
;----------------------------------------------------

; read image file into memory (0050:0000)

;*******************************************************************************
; LOAD_IMAGE
; ==========
; DESC: Load second stage boot loader into memory starting 0x0500
; PRE : FATs are already loaded at 0x7E00, after 1st stage boot loader
; POST: 2nd stage boot loader is loaded at 0x500
; IN  :
; OUT :
; NOTE:
;*******************************************************************************

  LOAD_IMAGE:
    mov     ax, 0x0050
    mov     es, ax                              ; destination for image
    mov     bx, 0x0000                          ; destination for image
    push    bx

    mov     ax, WORD [cluster]                  ; cluster to read
    pop     bx                                  ; buffer to read into
    call    ClusterLBA                          ; convert cluster to LBA
    xor     cx, cx
    mov     cl, BYTE [bpbSectorsPerCluster]     ; no of sectors to read
    call    ReadSectors
    push    bx

    ; compute next cluster

    ; Note: Cluster index number and starting address relationship is as follows:
    ; FAT entry size is 12 bits (one and a half bytes)
    ; byte add: 0   1   3   4   6   7   9   10  12  13  15 ...
    ; cluster : 0   1   2   3   4   5   6   7   8   9   10 ...
    ; So, we can see that
    ;      byteAddress = clusterIndex + clusterIndex / 2
    mov     ax, WORD [cluster]                  ; identify current cluster
    mov     cx, ax                              ; copy current cluster
    mov     dx, ax                              ; copy current cluster
    shr     dx, 0x0001                          ; divide by two
    add     cx, dx
    ; cx = byte address of cluster
    mov     bx, 0x0200                          ; location of FAT in memory
    add     bx, cx                              ; index into FAT
    mov     dx, WORD [bx]                       ; read two bytes from FAT
    test    ax, 0x0001
    jnz     .ODD_CLUSTER

    .EVEN_CLUSTER:
      and     dx, 0000_1111_1111_1111b               ; take low twelve bits
      jmp      .DONE
    .ODD_CLUSTER:
      shr     dx, 0x0004                          ; take high twelve bits
    .DONE:
      mov     WORD [cluster], dx                  ; store new cluster
      cmp     dx, 0x0FF0                          ; test for end of file
      jb      LOAD_IMAGE
    DONE:
      mov     si, msgCRLF
      call    Print
      push    WORD 0x0050
      push    WORD 0x0000
      retf
    FAILURE:
      mov     si, msgFailure
      call    Print
      mov     ah, 0x00
      int     0x16                                ; await keypress
      int     0x19                                ; warm boot computer

datasector  dw 0x0000
cluster     dw 0x0000
ImageName   db "KRNLDR  SYS"
msgLoading  db 0x0D, 0x0A, "Loading Boot Image ", 0x00
msgCRLF     db 0x0D, 0x0A, 0x00
msgProgress db ".", 0x00
msgFailure  db 0x0D, 0x0A, "MISSING OR CURRUPT KRNLDR. Press Any Key to Reboot", 0x0D, 0x0A, 0x00

TIMES 510-($-$$) DB 0
DW 0xAA55
