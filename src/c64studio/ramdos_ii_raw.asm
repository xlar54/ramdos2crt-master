;
; change list after v1.0
;
;version .macro      ; declare 3 char version here
;  .byte "4.3"
;  .endm
;
; when     who  version what
; --------   ---  ----  ---------------------------------------------
; 11/08/18  scott hutter V4.3b   This version is a RAW memory dump of the
; code, disassembled.  It has many flaws but does work and will 
; compile.  The original v4.3 source code has apparently been
; lost in time. I am using the original c128devpak ramdos code
; to recreate this code with labels for c64studio.
;
; You can load and run the code with this basic program
;
;  0 if a=0 then a=1:sys65418:load"ramdos.bin",8,1
;  5 u=15:p=207:ml=25344:l=6:m=0
;  40 poke 780,u
;  50 poke 781,p
;  60 sys ml+l+m
;  70 print "ramdisk loaded as device 15"
;
;  it can also be modified and loaded to run in 128 mode

; 11/13/87   fab  V4.3  Added patch to correctly handle CLOSE for
;       C128 opration.  Patch added to match posted
;       version of RAMDOS128.BIN.
;
; 10/26/87   fab  V4.2  RBASIN did not check for prior status error-
;       added code at DISK_IO to exit if bad status.
;       This caused a problem especially for Relative
;       file reads of empty records.
;
;       EOF_CHECK did not preserve prior status-
;       added  ora status/ sta status.  This caused a
;       problem for file read loops expecting some kind
;       of error status when past end of file.
;
;       READ_BYTE did not set TIMEOUT status bit, and
;       now it does (eg, read past EOF & you get ST=66).
;
;       Relative file writes did not report OVERFLOW
;       if given too much data for one record, and
;       instead placed the excess data into subsequent
;       records.  Changed write_byte_rel to properly
;       update current record only and report error.
;       Fix assumes that a chkin/out implies a prior
;       clrchn was performed.
;
;       CLOSEing the command channel now closes all
;       other user channels on disk side, as it should.
;       Also, for C128 mode only, the status of carry
;       is important as it should be (i.e., c=1 means
;       not a real CLOSE, just remove crap from tables).
;
; 8/26/87    fab  V4.1  Added NEW command.  Simply sets disk_end=start.
;
;       Added range check to set_unit_number (4-30).
;
;       F access command string failed when a parameter
;       was equal to <CR>.  It's okay now.
;
;       Added code to strip trailing <CR> if any from
;       any filename processed by init_get_filename.
;
; 8/24/87    hcd  V4.0  added 'F' access ( as opposed to 'RWMA' ).
;
;       F access allows both reading and writing a
;       file like sequential read and sequential
;       write access. The file pointer merely points to
;       the byte to operate on for reads or writes.
;
;       F access is legal for SEQ or PRG files only.
;
;       F access allows the POSITION command to be used
;       to position the r/w head at any byte in the 
;       file. Positioning the head past the end of the
;       file causes the file to be expanded, $FF is the
;       padding char.
;
;       The format for the F access version of the
;       position command is:
;
;         P:<channel><lpage><hpage>[<byte>]
;
;       where:
;
;         <channel> is the file's channel
;         <hpage><lpage><byte> is a three byte
;         pointer into the file. Three nulls
;         would point the the first byte in the 
;         file.
;
;         <byte> is optional, if omitted it
;         defaults to zero.
;
;         Note the odd order for the arguements.
;
;
;
; 8/20/87    hcd  V3.6    added flush block call to read_byte_default.
;       This is required before all DMAs directly
;       to disk ram. Its omission had caused strange
;       happenings when a file was opened for write,
;       and a previously open read file was then opened.
;
;       The rule is that any system calls which 
;       access disk memory directly ( as opposed to
;       accessing disk memory via the default page ),
;       must flush the default block before execution,
;       and must unflush the block after execution.
;
;       added flush block to do_load also.
;
;       Corrected "FILE TOO LARGE" mispelling.
;
;       Fixed bug with cleanup command write call
;       to interpret command. Bad commands would
;       shown errors because the command would be
;       interpreted ( via cleanup ), then the commands
;       cr would be interpreted as an ok command
;       clearing the error channel. Fix was implemented
;       by causeing commands of a single <cr> to
;       have no effect on error channel.
;
;       Caused serial buss timeout bit to be set when
;       an attempt to read past the end of a file
;       occurs. This is to accomodate BASIC7.0 DOS
;       input command which is not satisfied by a
;       simple EOF status.
;
; 7/20/87    hcd  V3.5    BA ( the system bank variable ) was corrected
;       to reflect the proper address. It was $cb.
;       It is $c6.
;
;       Corrected "FILE NAME EXSISTS" mispelling.
;
;       Corrected "FILES SCRATCHED" error message
;       where the number of files scratched was
;       incorrect.
;
;       Removed enhancement allowing for files 
;       > 10K block long. This causes the
;       directory format display to be correct.
;       ( filenames were shifted one char right )
;
;       Caused cleanup_command_write to force any
;       command in buffer to interpreted. This is
;       in line with serial buss standard that clrch
;       can terminate a command.
;
; 5/27/87    hcd  V3.4  corrected save bug (saved 1 too many bytes
;       causing load to load 1 too many)
;
; 4/8/87     hcd  V3.3  corrected bug in sniff_disk_size which
;       smelled 512k when only 256k exsisted
;       on some ramdisk units. (erp)
;
; 11/12/86   hcd  V3.2  added copyright message to jump vectors.
;       corrected error 73 text to include Vx.x 
;       correctly from version macro.
;
; 11/11/86   hcd  V3.1  added USR files.
;       corrected error in M-W for unit change.
;       corrected pattern matching bug where ending *
;       in filename may mean 0-n chars.
;
;
c64  = 1          ; define this flag to force c65 assy
rel_flag =1       ; define this flag to enable rel file code....
position_flag =1  ; define this flag to allow position command on
                  ; program and relative files.
;
;
;!ifdef position_flag {
;  !ifndef rel_flag {
;  *** error *** illegal to assemble with position and no rel files
;  }
;}
;
!ifdef  c64 {
  ;.nam  C64 RAMDISK DOS
  default_unit_number = $08       ; unit 8
  default_interface_page  = $cf   ; place for interface page
  swapped_dos_base  = $6000       ; install the dos here....
} else {
  ;.nam  C128 RAMDISK DOS
  default_unit_number = $09       ; unit 9
  default_interface_page  = $0e   ; place for interface page
  down_load_area    = $3e4        ; start of down load area
  swapped_dos_base  = $2000       ; install the dos here....
}

curzpg  = $fe
curram  = swapped_dos_base
code_start = swapped_dos_base+$300
swapped_code_start = swapped_dos_base
swapped_code_size = $1fff

curzpg  = $fe
curram  = swapped_dos_base
code_start = swapped_dos_base+$300
swapped_code_start = swapped_dos_base
swapped_code_size = $1fff

* = swapped_dos_base
data_block
!fill 256, $00

;
dir_filelen   = data_block      ; number of blocks for this file
dir_access    = data_block+2    ; access char if open, null otherwise ( R,W,L,$ )
dir_filetype  = data_block+3    ; type char for file ( S,P,L )
dir_last_byte = data_block+4    ; pointer to last byte
dir_end_record= data_block+5    ; two bytes indicating number of rel file records
dir_record_len= data_block+7    ; record length for rel files
dir_filename  = data_block+8
;       use rest of file for data
dir_data_start        = dir_filename+18
dir_data_offset       = dir_data_start-data_block
dir_load_addr         = dir_data_offset+data_block
dir_load_data_offset  = dir_data_offset+2
;
first_block
!fill 2, $00                    ; pointer to location of first data
                                ; on disk after dos code and ram
disk_end
!fill 2, $00                    ; pointer to one past last data block in disk
                                ; ( if disk_end == first_block then disk empty )
disk_max
!fill 2, $00                    ; highest legal value for disk_end
                                ; this is the number of blocks on the disk
channel_blocks
!fill 2, $00                    ; this stores the pointer to channel
                                ; storage on disk ( less than first block )
default_channel_number  
!byte $00                       ; current channel in default_channel
;
cleanup_vector
!fill 2, $00                    ; pointer to cleanup routine for fastop

default_channel = curram

channel_access
!fill 1, $00                    ; ( R,W,L,$ ) read/write/relative/directory
directory_block
!fill 2, $00                    ; ( first_block - 1 )
current_byte
!byte $00
current_block
!fill 2, $00                    ; point directly to next byte
end_byte
!byte $00
end_block
!fill 2, $00                    ; point directly to last byte

!ifdef rel_flag {
current_record_byte
!byte $00                       ; rel file, current byte in record
current_record
!fill 2, $00                    ; index of current record
current_record_len
!byte $00                       ; length of current record
                                ; ram rel_write_flag  flag for interface to disk_unlsn for rel only
end_record
!fill 2, $00                    ; index of last record in rel file
record_len    
!byte $00                       ; length of physical record
}
default_channel_end
channel_len = curram-default_channel ; used to allocate channels

;
default_block
!fill 2, $00                    ; current block in the data_block buffer
;
pntr
!fill 2, $00                    ; all I want is a pointer to use
pntr_save
!fill 2, $00                    ; save this too shithead....
;
;
eof_flag
!byte $00                       ; internal eof flag
data_byte
!byte $00                       ; data byte buffer
interface_page
!byte $00                       ; page number of the dma interface block
disk_fa
!byte $00                       ; our unit number ( can you say 9 )
alt_filename
!fill 17, $00                   ; alternate filename for copy/rename
;

;
;************************************************************************
;   DMA DECLARES
;************************************************************************
;
;
!ifndef c64 {
mmucr   = $ff00     ; mmu configuration 
mmurcr  = $d506
}
;
dma = $df00         ; base of dma unit
vicspeed = $d030    ; must do for c64 mode on c128
;
dma_status  = dma   ; dma status
;   b7  - irq pending
;   b6  - dma complete
;   b5  - block verify error
;   b4  - size register
;   b3-0  - version
;
dma_cmd = dma+1     ; dma command
; b7 =1   arm transfer
; b6  
; b5 =1 autoload enable
; b4 =1 disable $ff00 decode
; b3  
; b2  
; b1:b0
;   00  write c128 --> disk
;   01  read  c128 <-- disk
;   10  swap  c128 <-> disk
;   11  compare c128 == disk
;
dma_immediate_write   = %10110000
dma_immediate_read    = %10110001
dma_immediate_swap    = %10110010
dma_immediate_compare = %10110011
;
dma_banked_write      = %10100000
dma_banked_read       = %10100001
dma_banked_swap       = %10100010
dma_banked_compare    = %10100011
;
dma_fastop_write      = %10010000
dma_fastop_read       = %10010001
;
dma_cpu_addr          = dma+2   ; c128 addr
dma_disk_addr         = dma+4   ; disk low order adder
dma_disk_block        = dma+5   ; disk block ( two bytes )
dma_disk_bank         = dma+6   ; disk bank
dma_len               = dma+7   ; two bytes for length of transfer
dma_ifr               = dma+9   ; interupt mask register
dma_acr               = dma+10  ; address_control_register

;************************************************************************
;   CODE START
;************************************************************************
;

* = code_start

JMP $7D99
JMP $7DDE
JMP $7D9D
JMP $7DE2
JMP $630F

;************************************************************************
;   EQUATES
;************************************************************************
;
cr  = $0d
;
; kernal_declares
;
status  = $90
svxt    = $92
verck   = $93
ldtnd   = $98
dfltn   = $99
dflto   = $9a
eal     = $ae
eah     = $af
fnlen   = $b7
la      = $b8
sa      = $b9
fa      = $ba
fnadr   = $bb
stal    = $c1
stah    = $c2
memuss  = $c3

!ifndef c64 {
ba      = $c6
fnbank  = $c7
}

inmi    = $0318
iopen   = $031a
iclose  = $031c
ichkin  = $031e
ickout  = $0320
iclrch  = $0322
ibasin  = $0324
ibsout  = $0326
; istop
igetin  = $032a
iclall  = $032c
; exmon
iload   = $0330
isave   = $0332
;
d1prb   = $dc01 ; key scan port
d1ddrb  = $dc03 ; key scan port ddr
d2icr   = $dd0d ; icr for nmni 6526


!ifdef c64 {
fat         = $0263
;
print       = $e716 ; direct entry to screen printer.
;
cheap_open  = $f34a
jxrmv       = $f2f2 ; remove lat fat sat entry whose index is in .a
lookup      = $f30f   
jltlk       = $f314
getlfs      = $f31f ; jz100
;
_luking     = $f5af ; print looking for filename
loding      = $f5d2 ; print loading
_saving     = $f68f ; print saving filename
;
no_restor_restore = $fe69 ; run stop restore less the restor....
fake_nmi    = $fe72 ; calls nmi232, and does a prend...
;
kernal_error= $f715 ; kernal error handler
ud60        = $f6bc
;
} else {
;
fat         = $036c
;
system_vector= $0a00
;
print       = $c00c ; direct entry into screen print...
;
cheap_open  = $efbd
jxrmv       = $f1e5 ; remove lat fat sat entry whose index is in .a
lookup      = $f202
jltlk       = $f207
getlfs      = $f212
;
_luking     = $f50f ; print looking for filename
loding      = $f533 ; print loading
_saving     = $f5bc ; print saving filename
;
no_restor_restore = $fa56 ; run stop restore less the restor....
fake_nmi    = $fa5f ; calls nmi232, and does a prend...
;
kernal_error= $f699 ; kernal error handler
fnadry      = $f7ae ; indirect load file name address
getcfg      = $ff6b ; .a <= mmu setting for config .x
ud60        = $f63d
;
}
;
cint    = $ff81
ioinit  = $ff84
restor  = $ff8a
clrch   = $ffcc
stop    = $ffe1
;
;
;************************************************************************
;   COPYRIGHT MESSAGE
;************************************************************************
;
copyright_message
        LDX #$00
l_10    TXA 
        PHA 
        LDA $6321,X
        JSR $E716
        PLA 
        TAX 
        INX 
        CPX #$45
        BNE $6311
        RTS 
;;
l_6321 !byte $0d,$28,$43,$29,$20,$31,$39,$38,$36,$20,$43,$4f,$4d,$4d,$4f,$44,$4f,$52,$45,$20,$45,$4c,$45,$43 ;  .(C) 1986 COMMODORE ELEC
l_6339 !byte $54,$52,$4f,$4e,$49,$43,$53,$0d,$0d,$32,$4d,$42,$20,$50,$41,$54,$43,$48,$20,$42,$59,$20,$41,$4e ;  TRONICS..2MB PATCH BY AN
l_6351 !byte $44,$52,$45,$57,$20,$45,$2e,$20,$4d,$49,$4c,$45,$53,$4b,$49,$20,$31,$39,$39,$31,$0d,$00,$4f,$4b ;  DREW E. MILESKI 1991..OK
l_6369 !byte $00,$01,$46,$49,$4c,$45,$53,$20,$53,$43,$52,$41,$54,$43,$48,$45,$44,$00,$0d,$44,$4f,$53,$20,$43 ;  ..FILES SCRATCHED..DOS C
l_6381 !byte $4f,$4e,$46,$55,$53,$45,$44,$00,$1e,$53,$59,$4e,$54,$41,$58,$20,$45,$52,$52,$4f,$52,$00,$1f,$53 ;  ONFUSED..SYNTAX ERROR..S
l_6399 !byte $59,$4e,$54,$41,$58,$20,$45,$52,$52,$4f,$52,$00,$20,$53,$59,$4e,$54,$41,$58,$20,$45,$52,$52,$4f ;  YNTAX ERROR. SYNTAX ERRO
l_63b1 !byte $52,$00,$21,$53,$59,$4e,$54,$41,$58,$20,$45,$52,$52,$4f,$52,$00,$22,$53,$59,$4e,$54,$41,$58,$20 ;  R.!SYNTAX ERROR."SYNTAX
l_63c9 !byte $45,$52,$52,$4f,$52,$00,$32,$52,$45,$43,$4f,$52,$44,$20,$4e,$4f,$54,$20,$50,$52,$45,$53,$45,$4e ;  ERROR.2RECORD NOT PRESEN
l_63e1 !byte $54,$00,$33,$4f,$56,$45,$52,$46,$4c,$4f,$57,$20,$49,$4e,$20,$52,$45,$43,$4f,$52,$44,$00,$34,$46 ;  T.3OVERFLOW IN RECORD.4F
l_63f9 !byte $49,$4c,$45,$20,$54,$4f,$4f,$20,$4c,$41,$52,$47,$45,$00,$3c,$46,$49,$4c,$45,$20,$4f,$50,$45,$4e ;  ILE TOO LARGE.<FILE OPEN
l_6411 !byte $00,$3d,$46,$49,$4c,$45,$20,$4e,$4f,$54,$20,$4f,$50,$45,$4e,$00,$3e,$46,$49,$4c,$45,$20,$4e,$4f ;  .=FILE NOT OPEN.>FILE NO
l_6429 !byte $54,$20,$46,$4f,$55,$4e,$44,$00,$3f,$46,$49,$4c,$45,$20,$45,$58,$49,$53,$54,$53,$00,$40,$46,$49 ;  T FOUND.?FILE EXISTS.@FI
l_6441 !byte $4c,$45,$20,$54,$59,$50,$45,$20,$4d,$49,$53,$4d,$41,$54,$43,$48,$00,$42,$49,$4c,$4c,$45,$47,$41 ;  LE TYPE MISMATCH.BILLEGA
l_6459 !byte $4c,$20,$54,$52,$41,$43,$4b,$20,$41,$4e,$44,$20,$53,$45,$43,$54,$4f,$52,$00,$46,$4e,$4f,$20,$43 ;  L TRACK AND SECTOR.FNO C
l_6471 !byte $48,$41,$4e,$4e,$45,$4c,$00,$48,$44,$49,$53,$4b,$20,$46,$55,$4c,$4c,$00,$49,$43,$42,$4d,$20,$44 ;  HANNEL.HDISK FULL.ICBM D
l_6489 !byte $4f,$53,$20,$56,$34,$2e,$33,$20,$31,$37,$58,$58,$00,$00,$42,$41,$44,$20,$45,$52,$52,$4f,$52,$20 ;  OS V4.3 17XX..BAD ERROR
l_64a1 !byte $4e,$55,$4d,$42,$45,$52,$00;  NUMBER.
 
CLC 
ADC $6169
STA $6169
RTS 
LDX $6169
CPX $616A
BCC $64BF
JSR $655F
CLC 
JMP $7995
LDA #$A8
LDX #$64
STA $6109
STX $610A
LDA #$37
LDX #$01
CLC 
ADC $6169
BCC $64D4
INX 
STA $DF04
STX $DF05
LDA #$00
JSR $7EB0
LDA $616A
SEC 
SBC $6169
LDX $62F5
LDY $62F6
SEC 
JMP $7AE2
STA $616B
STX $616C
LDY #$01
JSR $6580
LDA #$00
STA $616F
LDA #$10
LDX #$27
JSR $6523
LDA #$E8
LDX #$03
JSR $6523
LDA #$64
LDX #$00
JSR $6523
LDA $616B
JSR $65A6
JSR $65D4
LDA #$00
JMP $65A6
STA $616D
STX $616E
LDA $616F
AND #$F0
STA $616F
LDA $616B
SEC 
SBC $616D
TAX 
LDA $616C
SBC $616E
BCC $6553
STA $616C
STX $616B
LDA $616F
ADC #$00
ORA #$30
STA $616F
BNE $6531
LDA $616F
BEQ $655B
JSR $65D6
RTS 


LDY #$49
BIT $00A0
LDA #$00
TAX 
JMP $656E
TAY 
LDA $610F
LDX $6110
JSR $6580
LDA $6135
JSR $65A6
JSR $65D4
LDA $6136
JMP $65A6
STY $6134
STX $6135
STA $6136
LDA #$00
STA $6169
STA $616A
LDA $6134
JSR $65A6
JSR $65D4
LDA #$20
JSR $65D6
JSR $65EA
JSR $65D4
RTS 


CMP #$64
BCC $65C0
CMP #$C8
BCC $65B6
SBC #$C8
PHA 
LDA #$02
JMP $65BC
SEC 
SBC #$64
PHA 
LDA #$01
JSR $65D1
PLA 
LDX #$FF
INX 
SEC 
SBC #$0A
BCS $65C2
PHA 
TXA 
JSR $65D1
PLA 
CLC 
ADC #$0A
ORA #$30
BIT $2CA9
LDX $616A
STA $6137,X
CPX #$30
BEQ $65E3
INC $616A
LDA #$00
STA $6138,X
CLC 
RTS 


LDA #$66
LDX #$63
STA $FE
STX $FF
LDA $6134
LDX #$00
CMP ($FE,X)
BEQ $6600
JSR $660D
BNE $65F2
JSR $6612
BEQ $660B
JSR $65D6
JMP $6600
CLC 
RTS 


JSR $6612
BNE $660D
INC $FE
BNE $6618
INC $FF
LDX #$00
LDA ($FE,X)
RTS 


LDX #$10
TXA 
JSR $663F
JSR $662D
LDX $6108
DEX 
BPL $661F
RTS 


LDX #$0F
LDA #$00
STA $610B,X
DEX 
BPL $6631
RTS 


LDA $B9
AND #$0F
BIT $10A9
CMP $6108
BEQ $6670
PHA 
LDA #$0B
LDX #$61
STA $DF02
STX $DF03
LDA $6107
JSR $7EB0
LDA #$10
LDX #$00
STA $DF07
STX $DF08
LDA $6108
LDY #$B0
JSR $6678
PLA 
STA $6108
LDY #$B1
JSR $6678
LDA $6108
LDX $610B
CLC 
RTS 


ASL A
TAX 
LDA $668D,X
STA $DF04
LDA $668E,X
ADC $6106
STA $DF05
STY $DF01
RTS 

!byte $00,$00
BPL $6691
JSR $3000
!byte $00
RTI 
!byte $00
BVC $6699
RTS 
!byte $00
BVS $669D
!byte $80, $00 
BCC $66A1
LDY #$00
BCS $66A5
CPY #$00
BNE $66A9
CPX #$00
BEQ $66AD
!byte $00, $01, $8D, $0f, $61, $8E
BPL $6716
LDA $610F
LDX $6110
CPX $611C
BNE $66C3
CMP $611B
BEQ $66EB
PHA 
TXA 
PHA 
JSR $66D7
PLA 
TAX 
PLA 
STA $611B
STX $611C
LDY #$B1
BIT $B0A0
JSR $66ED
LDA $611B
LDX $611C
STA $DF05
JSR $7EB7
STY $DF01
CLC 
RTS 


LDX #$0A
LDA $66F9,X
STA $DF02,X
DEX 
BPL $66EF
RTS 

!byte $00, $60, $00,$00,$00,$00,$01,$00,$00,$00,$00
JSR $6638
CMP #$0F
BNE $670E
JMP $64B0
CPX #$00
BNE $671B
LDA #$46
STA $90
LDA #$46
JMP $7992
CPX #$24
BNE $673A
JMP $7458
TAY 
BEQ $6739
CLC 
ADC $610E
STA $610E
BCC $6736
INC $610F
BNE $6736
INC $6110
JSR $66D4
RTS 


LDA $610B
CMP #$52
BEQ $6751
CMP #$46
BEQ $6751
CMP #$4C
BNE $674C
JMP $6A0E
LDA #$3C
JMP $7992
SEC 
LDA $6111
SBC $610E
TAY 
LDA $6112
SBC $610F
TAX 
LDA $6113
SBC $6110
BCS $676B
JMP $7989
BNE $6770
TXA 
BEQ $6772
LDY #$FF
TYA 
PHA 
JSR $66D7
PLA 
TAY 
BNE $6780
LDA #$40
STA $90
INY 
LDA #$22
LDX #$67
SEC 
JMP $7AC7
JSR $6638
CMP #$0F
BNE $6792
JMP $7184
CPX #$57
BEQ $67DA
CPX #$46
BEQ $67DA
CPX #$4C
BNE $67A1
JMP $6A43
LDA #$46
JMP $7992
TAY 
BEQ $67D7
DEY 
TYA 
CLC 
ADC $610E
STA $610E
LDX $6110
CPX $6113
BNE $67C0
LDX $610F
CPX $6112
BNE $67CA
CMP $6111
BCC $67CA
STA $6111
INC $610E
BNE $67D7
INC $610F
BNE $67D7
INC $6110
JMP $66D4
JSR $67FB
BCS $67EA
LDA #$00
SEC 
SBC $610E
CMP #$00
BNE $67ED
CLC 
JMP $79A3
PHA 
JSR $66D7
PLA 
TAY 
LDA #$A6
LDX #$67
CLC 
JMP $7AC7
LDA $6113
CMP $6110
BNE $6811
LDA $6112
CMP $610F
BNE $6811
LDA $6111
CMP $610E
BCS $686F
LDA $6111
CMP #$FF
BNE $6858
LDA $610F
LDX $6110
PHA 
TXA 
PHA 
LDA $6112
LDX $6113
CLC 
ADC #$01
BCC $682F
INX 
JSR $6B1A
TAY 
PLA 
TAX 
PLA 
STA $610F
STX $6110
TYA 
BCS $6889
INC $6112
BNE $6847
INC $6113
LDA $610C
LDX $610D
JSR $66BB
INC $6000
BNE $6858
INC $6001
INC $6111
LDA $6112
LDX $6113
JSR $66BB
LDA #$FF
LDY $6111
STA $6000,Y
JMP $67FB
JSR $66B5
LDY $610E
LDA $6120
STA $6000,Y
INC $610E
BNE $6888
INC $610F
BNE $6888
INC $6110
CLC 
RTS 


CLC 
LDA #$1A
ADC $6114
STA $610E
LDA #$00
LDX #$00
ADC $610C
PHA 
TXA 
ADC $610D
TAX 
PLA 
STA $610F
STX $6110
LDY $610B
CPY #$46
BNE $68C4
CLC 
ADC $6115
PHA 
TXA 
ADC $6116
TAX 
PLA 
BCS $68C1
STA $610F
STX $6110
LDA #$34
RTS 


LDA #$00
STA $6172
LDA $6115
LDX $6116
STA $6170
STX $6171
LDA $611A
LSR A
PHA 
BCC $68F8
CLC 
LDA $6170
ADC $610E
STA $610E
LDA $6171
ADC $610F
STA $610F
LDA $6172
ADC $6110
STA $6110
ASL $6170
ROL $6171
ROL $6172
PLA 
BNE $68D8
LDA #$32
LDX $6116
CPX $6119
BNE $6914
LDX $6115
CPX $6118
RTS 


LDA $6115
LDX $6116
PHA 
TXA 
PHA 
LDA $6114
PHA 
LDA $6120
PHA 
JSR $6940
BCS $692C
CLC 
TAY 
PLA 
STA $6120
PLA 
STA $6114
PLA 
TAX 
PLA 
STA $6115
STX $6116
TYA 
RTS 


LDA $6118
LDX $6119
CPX #$FF
BNE $694C
CMP #$FF
BCS $6977
STA $6115
STX $6116
LDA #$00
STA $6114
JSR $688A
LDA #$FF
STA $6120
JSR $67FB
BCS $6976
INC $6114
JSR $697B
BCS $6976
INC $6118
BNE $6976
INC $6119
RTS 


LDA #$34
SEC 
RTS 


LDA $6114
CMP $611A
BCS $6992
LDA #$00
STA $6120
JSR $67FB
BCS $6993
INC $6114
BNE $697B
CLC 
RTS 


LDA $6114
PHA 
LDA #$00
STA $6114
STA $6117
JSR $688A
BCS $69D6
LDA $6114
CMP $611A
BCS $69D0
JSR $66B5
LDY $610E
LDA $6000,Y
BEQ $69BE
LDA $6114
STA $6117
INC $610E
BNE $69CB
INC $610F
BNE $69CB
INC $6110
INC $6114
BNE $69A5
PLA 
STA $6114
CLC 
RTS 


TAY 
PLA 
TYA 
RTS 


JSR $688A
BCS $69E9
JSR $66B5
LDX $610E
LDA $6000,X
CLC 
RTS 


JSR $6A04
JSR $688A
JSR $697B
BCS $6A02
INC $6115
BNE $69FD
INC $6116
LDA #$00
STA $6114
CLC 
RTS 


CLC 
ADC $6114
STA $6114
JMP $66D4
LDA #$04
LDX #$6A
STA $6109
STX $610A
JSR $6994
BCS $6A2D
JSR $688A
SEC 
LDA $6117
SBC $6114
BEQ $6A30
SEC 
JMP $6A7F
JMP $7992
JSR $69DA
LDX #$00
STX $6114
INC $6115
BNE $6A40
INC $6116
JMP $799A
CLC 
JSR $6AA1
BNE $6A69
LDA #$EA
LDX #$69
STA $6109
STX $610A
LDX $6116
CPX $6119
BNE $6A61
LDX $6115
CPX $6118
BCC $6A6E
JSR $6915
BCC $6A53
BIT $33A9
JMP $7992
LDA $611A
SEC 
SBC $6114
BCC $6A69
BEQ $6A69
PHA 
JSR $6AA1
PLA 
CLC 
PHP 
PHA 
JSR $688A
JSR $66B5
JSR $66D7
LDA $610E
STA $DF04
LDA $610F
LDX $6110
STA $DF05
JSR $7EB7
PLA 
PLP 
JMP $7AE2
LDA $6121
STA $FF
LDA #$01
STA $FE
LDY #$00
LDA ($FE),Y
BCC $6AB3
ROL A
STA ($FE),Y
RTS 


JSR $6D58
BCS $6B13
TYA 
AND #$0F
JSR $663F
LDA $610B
CMP #$46
BEQ $6ACA
CMP #$4C
BNE $6B10
JSR $6D58
TYA 
BCS $6B13
STA $6115
JSR $6D58
TYA 
BCS $6B13
STA $6116
LDA #$00
STA $6114
JSR $6D58
TYA 
BCS $6AFD
LDY $610B
CPY #$46
BEQ $6AFA
CMP #$00
BEQ $6AFD
TAY 
DEY 
TYA 
CMP $611A
BCS $6B16
STA $6114
LDA $6115
BNE $6B0A
LDA $6116
BEQ $6B0D
DEC $6116
DEC $6115
JMP $688A
LDA #$46
BIT $1EA9
BIT $33A9
SEC 
RTS 


STA $6173
STX $6174
LDA $6102
LDX $6103
CLC 
ADC #$01
BNE $6B2C
INX 
CPX $6105
BNE $6B34
CMP $6104
BCC $6B39
LDA #$48
RTS 


LDA #$01
LDX #$00
JSR $6B74
LDA $6173
LDX $6174
JSR $66BB
INC $6173
BNE $6B51
INC $6174
JSR $66ED
LDA $6173
LDX $6174
STA $DF05
JSR $7EB7
CPX $6103
BNE $6B68
CMP $6102
BCS $6B72
LDA #$B2
STA $DF01
JMP $6B49
CLC 
RTS 


STA $6175
STX $6176
LDA $6108
PHA 
LDX #$10
TXA 
JSR $663F
LDA $610B
BEQ $6BB6
LDA $6112
LDX $6113
JSR $6BD1
STA $6112
STX $6113
LDA $610F
LDX $6110
JSR $6BD1
STA $610F
STX $6110
LDA $610C
LDX $610D
JSR $6BD1
STA $610C
STX $610D
LDX $6108
DEX 
BPL $6B80
PLA 
JSR $663F
LDA $6102
LDX $6103
JSR $6BD1
STA $6102
STX $6103
CLC 
RTS 


CPX $6174
BNE $6BD9
CMP $6173
BCC $6BE6
CLC 
ADC $6175
PHA 
TXA 
ADC $6176
TAX 
PLA 
RTS 


LDA $611B
LDX $611C
STA $6173
STX $6174
LDA $6000
LDX $6001
PHA 
TXA 
PHA 
LDA $6000
LDX $6001
EOR #$FF
TAY 
TXA 
EOR #$FF
TAX 
TYA 
JSR $6B74
PLA 
TAX 
PLA 
STA $6175
STX $6176
LDA $6173
LDX $6174
SEC 
ADC $6175
PHA 
TXA 
ADC $6176
TAX 
PLA 
JSR $66BB
LDA $6173
LDX $6174
STA $611B
STX $611C
INC $6173
BNE $6C3E
INC $6174
LDA $6103
CMP $6174
BNE $6C4C
LDA $6102
CMP $6173
BCS $6C16
RTS 


CMP #$40
BCC $6C5F
CMP #$80
BCC $6C5B
CMP #$C0
BCC $6C5F
AND #$1F
ORA #$40
CLC 
RTS 

!byte $01,$02,$04,$08
BPL $6C87
RTI 
!byte $80,$20,$3D,$66,$AD,$00,$61,$AE,$01,$61
STA $610F
STX $6110
CPX $6103
BNE $6C80
CMP $6102
BCC $6C8B
BEQ $6C87
LDA #$0D
BIT $3EA9
SEC 
RTS 


JSR $66B5
BCS $6C89
JSR $6CF6
BCS $6C9C
LDA $610F
LDX $6110
RTS 


LDA $6000
LDX $6001
SEC 
ADC $610F
PHA 
TXA 
ADC $6110
TAX 
PLA 
JMP $6C72
JSR $663D
LDA $6100
LDX $6101
STA $610F
STX $6110
CPX $6103
BNE $6CC7
CMP $6102
BCC $6CD2
BEQ $6CCE
LDA #$0D
BIT $3EA9
SEC 
RTS 


JSR $66B5
LDA $6002
BEQ $6CE2
LDA $610F
LDX $6110
CLC 
RTS 


LDA $6000
LDX $6001
SEC 
ADC $610F
PHA 
TXA 
ADC $6110
TAX 
PLA 
JMP $6CB9
LDX #$FF
INX 
LDA $6008,X
BNE $6D09
LDA $6177,X
BEQ $6D07
CMP #$2A
BNE $6D1B
CLC 
RTS 


LDA $6177,X
BEQ $6D1B
CMP $6008,X
BEQ $6CF8
CMP #$3F
BEQ $6CF8
CMP #$2A
BEQ $6D07
SEC 
RTS 


LDA #$00
STA $6189
STA $618A
STA $618B
STA $618C
STA $618D
JSR $7A01
LDY $B7
LDA $6191,Y
LDX #$00
BEQ $6D42
LDX #$01
LDY $6292
LDA $6292,Y
CMP #$0D
BNE $6D47
DEY 
STX $6191
STY $6190
LDA #$00
STA $618F
CLC 
RTS 


DEC $618F
RTS 


LDY $618F
CPY $6190
BCS $6D75
LDA $6191
BNE $6D6B
LDA $6192,Y
JMP $6D6E
LDA $6293,Y
INC $618F
JSR $6D76
CLC 
RTS 


LDX #$07
CMP $6D95,X
BEQ $6D84
DEX 
BNE $6D78
TAY 
LDA #$00
RTS 


TAY 
LDA $6C61,X
CMP #$04
BEQ $6D90
CMP #$02
BNE $6D93
STY $618B
CLC 
RTS 


JSR $2A3F
!byte $22
RTI 
AND $2C24,X
LDX #$FF
INX 
LDA #$00
STA $6177,X
JSR $6D58
BCS $6DC4
AND #$A0
BNE $6DC1
LDX #$FF
INX 
CPX #$10
BEQ $6DC1
LDA $6177,X
BNE $6DB0
TYA 
STA $6177,X
JMP $6D9F
JSR $6D54
CLC 
RTS 


JSR $6DCC
JSR $6DCC
JSR $6D58
BCS $6E04
CPY #$2C
BNE $6E01
JSR $6D58
BCS $6E01
TYA 
JSR $6C4F
CMP #$50
BEQ $6E28
CMP #$53
BEQ $6E28
CMP #$55
BEQ $6E28
CMP #$52
BEQ $6E31
CMP #$57
BEQ $6E31
CMP #$41
BEQ $6E31
CMP #$46
BEQ $6E31
CMP #$4C
BEQ $6E06
JSR $6D54
JSR $6D54
CLC 
RTS 


JSR $6E28
BCS $6E24
JSR $6D58
BCS $6E22
CPY #$2C
BNE $6E1F
JSR $6D58
BCS $6E24
STY $618D
JMP $6E37
JSR $6D54
CLC 
RTS 


LDA #$1E
SEC 
RTS 


LDY $618A
STA $618A
JMP $6E37
LDY $6189
STA $6189
TYA 
PHA 
JSR $6D58
BCS $6E45
AND #$A0
BEQ $6E39
JSR $6D54
CLC 
PLA 
BEQ $6E4C
LDA #$1E
SEC 
RTS 


JSR $6D58
BCS $6E66
CPY #$3A
BEQ $6E66
CPY #$30
BNE $6E63
JSR $6D58
BCS $6E66
CPY #$3A
BEQ $6E66
JSR $6D54
CLC 
RTS 


JSR $6D1D
JSR $6D58
BCS $6EA7
CPY #$40
BNE $6E86
STY $618C
JSR $6D58
BCS $6E93
CPY #$40
BEQ $6E74
JSR $6D54
JMP $6E93
CPY #$24
BNE $6E90
STY $6189
JMP $6E93
JSR $6D54
JSR $6E4D
JSR $6D9D
JSR $6DC6
BCS $6EA5
JSR $6D58
BCS $6EA7
LDA #$1E
SEC 
RTS 


CLC 
RTS 


JSR $6638
CMP #$0F
BNE $6EB3
JMP $7143
CPX #$00
BEQ $6EBA
JSR $70EA
JSR $655F
JSR $6E68
BCS $6F14
LDA $6189
CMP #$24
BNE $6ECC
JMP $742D
LDA #$22
LDX $6177
BEQ $6F14
LDA #$00
STA $618E
LDA $6189
BNE $6EE2
LDA #$52
STA $6189
JSR $6C69
BCS $6F19
PHA 
TXA 
PHA 
JSR $6638
PLA 
TAX 
PLA 
STA $610F
STX $6110
JSR $66B5
BCS $6F14
LDA #$3C
LDX $6002
BNE $6F14
LDX $6003
LDA $618A
STX $618A
BEQ $6F16
CMP $6003
BEQ $6F16
LDA #$40
SEC 
RTS 


INC $618E
LDA $618A
BNE $6F23
LDA #$53
STA $618A
CMP #$4C
BNE $6F2A
JMP $7091
LDA $6189
CMP #$52
BEQ $6F3F
CMP #$41
BEQ $6FB2
CMP #$46
BNE $6F3C
JMP $6FEB
JMP $7010
LDA #$1E
LDX $618C
BNE $6FB0
LDA #$3E
LDX $618E
BEQ $6FB0
JSR $66B5
BCS $6FB0
JSR $6638
LDA $611B
LDX $611C
STA $610C
STX $610D
STA $610F
STX $6110
CLC 
ADC $6000
PHA 
TXA 
ADC $6001
TAX 
PLA 
STA $6112
STX $6113
LDA #$1A
STA $610E
LDA $6004
STA $6111
LDA $6007
STA $611A
LDA $6005
LDX $6006
STA $6118
STX $6119
LDA #$00
LDX #$00
STA $6115
STX $6116
STA $6117
STA $6114
LDA $6189
STA $610B
STA $6002
CLC 
RTS 


SEC 
RTS 


LDA #$57
STA $6189
LDX $618E
BEQ $7010
LDA #$21
LDX $618B
BNE $6FE9
JSR $6F3F
BCS $6FE9
LDA $6111
STA $610E
LDA $6112
LDX $6113
STA $610F
STX $6110
INC $610E
BNE $6FE7
INC $610F
BNE $6FE7
INC $6110
CLC 
RTS 


SEC 
RTS 


LDA #$21
LDX $618B
BNE $700E
LDA #$1E
LDX $618A
CPX #$53
BEQ $7001
CPX #$50
BEQ $7001
SEC 
RTS 


LDA $618E
BEQ $7010
LDA $618C
BNE $7010
JMP $6F3F
SEC 
RTS 


LDA #$21
LDX $618B
BEQ $701A
JMP $708F
LDX $618E
BEQ $702C
LDA #$3F
LDX $618C
BEQ $708F
JSR $66B5
JSR $6BE7
JSR $6638
JSR $662D
LDA $6102
LDX $6103
STA $610F
STX $6110
STA $610C
STX $610D
STA $6112
STX $6113
JSR $6B1A
BCS $708F
JSR $6638
JSR $66B5
LDY #$11
LDA $6177,Y
STA $6008,Y
DEY 
BPL $7057
LDA $618A
STA $6003
LDA $618D
STA $6007
STA $611A
LDA #$00
LDX #$00
STA $6000
STX $6001
LDA #$1A
STA $6004
STA $6111
STA $610E
LDA $6189
STA $6002
STA $610B
CLC 
RTS 


SEC 
RTS 


LDA #$4C
STA $6189
STA $618A
LDX $618E
BEQ $70B9
JSR $66B5
LDA $6007
LDX $618D
STA $618D
BEQ $70B1
CPX $618D
BNE $70CD
LDX $618C
BNE $70B9
JMP $6F3F
LDA $618D
BEQ $70CD
LDX $618E
BNE $70CA
LDA #$21
LDX $618B
BNE $70CF
JMP $7010
LDA #$32
SEC 
RTS 


LDX #$10
TXA 
PHA 
JSR $663F
JSR $70ED
PLA 
TAX 
DEX 
BPL $70D3
CLC 
RTS 


LDA $B9
AND #$0F
CMP #$0F
BEQ $70D1
JSR $6638
LDA $6108
LDX $610B
BEQ $7141
CMP #$0F
BEQ $713C
CPX #$24
BEQ $713C
LDA $610C
LDX $610D
STA $610F
STX $6110
JSR $66B5
BCS $7142
LDA $6111
STA $6004
LDA $6112
LDX $6113
SEC 
SBC $610C
PHA 
TXA 
SBC $610D
TAX 
PLA 
STA $6000
STX $6001
LDA $6118
LDX $6119
STA $6005
STX $6006
LDA #$00
STA $6002
LDA #$00
STA $610B
CLC 
RTS 


LDA #$0F
JSR $6638
LDA #$57
STA $610B
JSR $6D2E
LDY $B7
CLC 
BEQ $715B
CPY #$28
BCC $715C
LDA #$20
RTS 


STY $6292
DEY 
LDA $6192,Y
STA $6293,Y
DEY 
BPL $7160
JSR $71FB
LDX #$00
STX $6292
RTS 


CLC 
ADC $6292
STA $6292
BEQ $7183
JSR $7169
BCC $7183
JSR $6567
RTS 


LDY $6120
LDA #$28
SEC 
SBC $6292
BCS $719D
CPY #$0D
BNE $7198
LDA #$00
STA $6292
LDA #$20
JMP $7992
CPY #$0D
BNE $71AC
JSR $71FB
LDX #$00
STX $6292
JMP $79A3
TAY 
LDA #$72
LDX #$71
STA $6109
STX $610A
LDA #$93
LDX #$02
CLC 
ADC $6292
BCC $71C2
INX 
STA $DF04
STX $DF05
LDA #$00
JSR $7EB0
TYA 
CLC 
JMP $7AE2
!byte $53, $72
AND $7252,X
CMP $7243,X
SBC ($4E),Y
!byte $72,$CF,$4D,$73,$B5,$49,$73,$77
!byte $55,$73,$7D,$56,$72
LDA $6A50,Y 
!byte $b3, $00
JSR $71F4
JSR $71F4
INC $FE
BNE $71FA
INC $FF
RTS 


JSR $655F
JSR $6D1D
JSR $6D3A
LDA #$D2
LDX #$71
STA $FE
STX $FF
LDA $6292
BEQ $7231
JSR $6D58
TYA 
TAX 
LDY #$FD
INY 
INY 
INY 
LDA ($FE),Y
BNE $7223
LDA #$1F
SEC 
RTS 


TXA 
CMP ($FE),Y
BNE $7218
INY 
LDA ($FE),Y
PHA 
INY 
LDA ($FE),Y
PHA 
NOP 
CLC 
RTS 


JSR $6D58
BCS $723C
CPY #$3A
BNE $7233
CLC 
RTS 


JSR $7233
JSR $6D9D
BCS $7271
LDA #$00
LDX #$00
STA $62BC
STX $62BD
JSR $6C69
BCC $725B
CMP #$3E
BEQ $7273
SEC 
RTS 


LDA $610F
LDX $6110
JSR $727C
INC $62BC
BNE $726C
INC $62BD
JSR $6BE7
BCC $7250
SEC 
RTS 


LDA $62BC
LDX $62BD
JMP $64F0
STA $62BE
STX $62BF
LDA $6108
STA $62C0
LDA #$10
JSR $663F
BEQ $72AC
LDA $62BE
LDX $62BF
CPX $610D
BNE $729D
CMP $610C
BNE $72AC
LDA $6108
CMP $62C0
BEQ $72AC
LDA #$00
STA $610B
LDA $6108
CLC 
ADC #$FF
BCS $728A
LDA $62C0
JMP $663F
JSR $70D1
JSR $655F
JSR $6CB0
BCS $72CA
JSR $6BE7
BCC $72C0
CMP #$3E
BNE $72CF
CLC 
RTS 


LDA $6100
LDX $6101
STA $6102
STX $6103
CLC 
RTS 


JSR $73E7
BCS $72F0
LDX #$FF
INX 
LDA $6123,X
STA $6008,X
BNE $72E5
CLC 
RTS 


SEC 
RTS 


JSR $73E7
BCS $7370
LDA $6000
LDX $6001
SEC 
ADC $6102
PHA 
TXA 
ADC $6103
TAX 
PLA 
CPX $6105
BNE $7310
CMP $6104
BEQ $7314
BCS $736E
STA $62C1
STX $62C2
LDA $6102
LDX $6103
PHA 
TXA 
PHA 
LDA $6102
LDX $6103
STA $611B
STX $611C
INC $610F
BNE $7337
INC $6110
INC $6102
BNE $733F
INC $6103
JSR $66B5
LDA $6103
CMP $62C2
BNE $7350
LDA $6102
CMP $62C1
BNE $7323
PLA 
TAX 
PLA 
STA $610F
STX $6110
JSR $66BB
LDX #$FF
INX 
LDA $6123,X
STA $6008,X
BNE $7360
STA $6002
CLC 
RTS 


LDA #$48
SEC 
RTS 


JSR $70D1
JMP $655C
JSR $70D1
JMP $655F
JSR $6D58
BCS $739E
CPY #$3A
BEQ $7372
CPY #$4A
BEQ $7372
CPY #$49
BEQ $7372
CPY #$30
BNE $739E
JSR $6D58
BCS $739E
CPY #$3E
BNE $739E
CLC 
BIT $38
BCS $73B2
JSR $6D58
BCS $73B2
TYA 
CMP #$1F
BCS $73B2
CMP #$04
BCC $73B2
JMP $7E08
LDA #$1F
SEC 
RTS 


JSR $6D58
JSR $6D58
BCS $73E3
CPY #$57
BNE $73E3
JSR $6D58
BCS $73E3
CPY #$77
BEQ $73CF
CPY #$78
BNE $73E3
JSR $6D58
BCS $73E3
CPY #$00
BNE $73E3
JSR $6D58
BCS $73E3
CPY #$00
BEQ $73E3
CLC 
BIT $38
JMP $739F
JSR $7233
JSR $6D9D
BCS $742B
LDA $618B
BNE $7429
JSR $6C69
LDA #$3F
BCC $742B
LDX #$FF
INX 
LDA $6177,X
STA $6123,X
BNE $73FD
JSR $6D58
BCS $742B
CPY #$3D
BNE $7429
JSR $6E4D
JSR $6D9D
BCS $742B
LDA #$22
LDX $6177
BEQ $742B
JSR $6C69
BCS $7426
JMP $66AF
LDA #$3E
BIT $1EA9
SEC 
RTS 


LDA #$24
STA $610B
JSR $663D
LDA #$00
STA $610E
STA $610F
STA $6110
LDA $6177
BNE $744D
STA $6178
LDA #$2A
STA $6177
JMP $74FB
CLC 
ADC $610E
STA $610E
RTS 


JSR $663D
LDY $610E
CPY $6111
BCC $746C
JSR $749D
BCC $746C
CLC 
JMP $7989
LDA #$50
LDX #$74
STA $6109
STX $610A
LDA #$C3
LDX #$02
CLC 
ADC $610E
BCC $7481
INX 
STA $DF04
STX $DF05
LDA #$00
JSR $7EB0
LDA $6111
SEC 
SBC $610E
LDX $62F5
LDY $62F6
SEC 
JMP $7AE2
LDA $610F
ORA $6110
BNE $74AE
LDA $6100
LDX $6101
JMP $74D3
LDA $610F
LDX $6110
CPX $6103
BNE $74BC
CMP $6102
BCC $74BF
RTS 


JSR $66B5
LDA $610F
LDX $6110
SEC 
ADC $6000
PHA 
TXA 
ADC $6001
TAX 
PLA 
STA $610F
STX $6110
CPX $6103
BNE $74E1
CMP $6102
BCC $74E9
JSR $754A
JMP $74F4
JSR $66B5
JSR $6CF6
BCS $749D
JSR $7572
LDA #$00
STA $610E
CLC 
RTS 


LDX #$20
STX $6111
DEX 
LDA $750C,X
STA $62C3,X
DEX 
BPL $7501
CLC 
RTS 

l_750c !byte $01, $10, $01, $10,  $00, $00, $12, $22,  $52, $41, $4d, $44, $49, $53, $4b, $20,  $5d, $5b, $20, $20,  $56, $34, $2e, $33 ; ......."RAMDISK ][  V4.3
l_7524 !byte $22, $20, $52, $44,  $20, $30, $30, $00,  $01, $10, $00, $00, $42, $4c, $4f, $43,  $4b, $53, $20, $46,  $52, $45, $45, $20 ;  " RD 00.....BLOCKS FREE
l_753c !byte $20, $20, $20, $20,  $20, $20, $20, $20,  $20, $20, $20, $20, $00, $00

LDX #$1E
STX $6111
DEX 
LDA $752C,X
STA $62C3,X
DEX 
BPL $7550
LDA $6104
LDX $6105
SEC 
SBC $6102
PHA 
TXA 
SBC $6103
TAX 
PLA 
STA $62C5
STX $62C6
CLC 
RTS 


LDX #$20
STX $6111
LDA #$20
STA $62C3,X
DEX 
CPX #$03
BNE $7579
LDA #$22
STA $62C7
LDX #$00
LDA $6008,X
STA $62C8,X
BEQ $7595
INX 
CPX #$11
BNE $7588
LDA #$22
STA $62C8,X
LDY #$53
LDA #$45
LDX #$51
CPY $6003
BEQ $75CA
LDY #$50
LDA #$52
LDX #$47
CPY $6003
BEQ $75CA
LDY #$55
LDA #$53
LDX #$52
CPY $6003
BEQ $75CA
LDY #$52
LDA #$45
LDX #$4C
CPX $6003
BEQ $75CA
LDA #$3F
TAX 
TAY 
STY $62DA
STA $62DB
STX $62DC
LDA $6002
CMP #$57
BNE $75DF
LDA #$2A
STA $62D9
LDA $6000
LDX $6001
CLC 
ADC #$01
PHA 
TXA 
ADC #$00
TAX 
PLA 
STA $62C5
STX $62C6
CPX #$03
BNE $75FA
CMP #$E8
BCS $7615
CPX #$00
BNE $7602
CMP #$64
BCS $7612
CPX #$00
BNE $760A
CMP #$0A
BCS $760F
JSR $761C
JSR $761C
JSR $761C
LDA #$00
STA $62E2
CLC 
RTS 


LDX #$04
LDA #$20
LDY $62C3,X
STA $62C3,X
TYA 
INX 
CPX #$20
BNE $7620
RTS 


STA $93
LDA $B7
BNE $7637
LDA #$08
BNE $766B
JSR $7676
BCC $765D
CMP #$00
BNE $7650
LDA $93
BNE $7648
LDA #$10
BNE $766B
LDA #$10
ORA $90
STA $90
BNE $765D
JSR $6567
LDA #$02
ORA $90
STA $90
LDA #$04
BNE $766B
LDA #$40
ORA $90
STA $90
LDX $AE
LDY $AF
CLC 
JMP $79BD
TAY 
LDA #$F7
PHA 
LDA #$14
PHA 
TYA 
JMP $79BD
LDA #$00
STA $90
JSR $7A6C
JSR $6E68
BCS $76C6
JSR $6D58
BCC $76C4
LDA $618C
BNE $76C4
LDA $6189
CMP #$24
BNE $7696
JMP $777A
ORA $618A
BNE $76C4
LDA #$22
LDX $6177
BEQ $76C6
JSR $6C69
BCS $76C6
STA $610F
STX $6110
JSR $66B5
LDA #$40
LDX $6003
CPX #$50
BNE $76C6
LDX $6002
BEQ $76C8
LDA #$3C
BIT $3EA9
BIT $1EA9
SEC 
RTS 


JSR $F5D2
JSR $66D7
LDA $C3
LDX $C4
LDY $B9
BEQ $76DC
LDA $601A
LDX $601B
STA $DF02
STX $DF03
LDY #$1C
STY $DF04
LDA $610F
LDX $6110
STA $DF05
JSR $7EB7
SEC 
LDA $6004
SBC #$1B
TAY 
LDA $6000
LDX $6001
SBC #$00
BCS $770E
DEX 
JMP $770E
LDX #$00
TXA 
LDY $6111
STY $DF07
STA $DF08
CPX #$00
PHP 
LDA #$FF
LDX #$EF
SEC 
SBC $DF02
PHA 
TXA 
SBC $DF03
TAX 
PLA 
PLP 
BNE $7733
CPX $DF08
BNE $7731
CMP $DF07
BCS $773F
STA $DF07
STX $DF08
JSR $773F
JMP $7774
LDA $DF02
LDX $DF03
CLC 
ADC $DF07
PHA 
TXA 
ADC $DF08
TAX 
PLA 
STA $AE
STX $AF
LDA $DF07
ORA $DF08
BEQ $7778
LDY #$A1
LDA $93
BEQ $7764
LDY #$A3
JSR $7A2A
LDA $93
BEQ $7778
LDA $DF00
AND #$20
STA $93
BEQ $7778
LDA #$00
SEC 
RTS 


CLC 
RTS 


JSR $F5D2
JSR $742D
LDA $C3
LDX $C4
LDY $B9
BEQ $778C
LDA #$10
LDX #$10
STA $AE
STX $AF
DEC $6111
DEC $6111
LDX #$FF
INX 
LDA $62C5,X
STA $62C3,X
CPX $6111
BCC $7798
JSR $77BD
BCS $77BA
JSR $749D
BCC $77A4
LDX #$00
STX $62C3
INX 
STX $6111
JSR $77BD
LDA #$00
RTS 


LDA #$C3
LDX #$02
STA $DF04
STX $DF05
LDA #$00
JSR $7EB0
LDA $AE
LDX $AF
STA $DF02
STX $DF03
JMP $7708
LDA #$00
STA $90
JSR $7A65
JSR $6E68
BCS $7827
LDA $618A
ORA $6189
ORA $618B
BNE $7825
LDA #$22
LDX $6177
BEQ $7827
JSR $6C69
BCS $781F
STA $610F
STX $6110
JSR $66B5
LDA #$3F
LDX $618C
BEQ $7827
LDA #$40
LDX $6003
CPX #$50
BNE $7827
LDA #$3C
LDX $6002
BNE $7827
JSR $6BE7
JMP $7829
LDA #$3E
BIT $1EA9
SEC 
RTS 


LDA $6102
LDX $6103
CPX $6105
BNE $7837
CMP $6104
BCC $783D
LDA #$48
SEC 
RTS 


JSR $66AF
LDA $C1
LDX $C2
STA $601A
STX $601B
LDA $AE
LDX $AF
SEC 
SBC $C1
PHA 
TXA 
SBC $C2
TAX 
PLA 
TAY 
PHA 
TXA 
PHA 
TYA 
CLC 
ADC #$FF
PHA 
TXA 
ADC #$FF
TAX 
PLA 
CLC 
ADC #$1C
STA $6004
TXA 
ADC #$00
STA $6000
LDA #$00
STA $6001
ASL $6001
STA $6002
LDA #$50
STA $6003
LDX #$FF
INX 
LDA $6177,X
STA $6008,X
BNE $7883
JSR $66D7
PLA 
TAX 
PLA 
STA $DF07
STX $DF08
LDA $C1
LDX $C2
STA $DF02
STX $DF03
LDA $610F
LDX $6110
STA $DF05
JSR $7EB7
LDA #$1C
STA $DF04
LDA $6000
LDX $6001
SEC 
ADC $6102
PHA 
TXA 
ADC $6103
TAX 
PLA 
CPX $6105
BNE $78CC
CMP $6104
BCC $78D1
LDA #$48
RTS 


STA $6102
STX $6103
LDY #$A0
JSR $7A2A
JSR $66D4
CLC 
RTS 


JSR $7A7F
STX $62F5
STY $62F6
STA $6120
BCC $78F9
LDA $90
BNE $78F6
JMP $6704
JMP $7995
JMP $6788
JSR $7A7F
JMP $79BD
BCC $78FC
JSR $7A7F
TXA 
JSR $F2F2
JSR $70E2
JMP $79A3
JSR $7A7F
STA $6120
BCS $791C
JMP $79ED
LDA $BA
CMP $6122
BEQ $7931
LDA $7D25
PHA 
LDA $7D24
PHA 
LDA $6120
JMP $79BD
LDA $BA
PHA 
LDA #$00
STA $BA
JSR $F34A
TAY 
PLA 
STA $BA
TYA 
!byte $b0, $7B
LDA $BA
STA $0263,X
JSR $6EA9
JMP $79A3
PHP 
PHA 
JSR $7A7F
LDA $BA
CMP $6122
BEQ $797C
PLA 
STA $6120
PLP 
BCC $796E
LDA $7D2B
PHA 
LDA $7D2A
PHA 
LDA $6120
JMP $79BD
LDA $7D29
PHA 
LDA $7D28
PHA 
LDA $6120
JMP $79BD
PLA 
PLP 
BCS $7983
JMP $762D
JSR $77D9
JMP $79A3
LDA #$42
STA $90
LDA #$00
JMP $79B6
SEC 
BCS $79A3
LDA #$0D
BIT $00A9
STA $6120
LDA #$FF
STA $611F
CLC 
BCC $79B0
ROL $611F
JSR $6567
LDA #$0D
STA $6120
JSR $79DC
LDA $6120
LDX $62F5
LDY $62F6
CLC 
STA $6120
LDA $6121
PHA 
LDA #$E8
PHA 
LDA $6121
PHA 
LDA #$AF
PHA 
LDA $611D
STA $FE
LDA $611E
STA $FF
LDA $6120
RTS 


LDA $611F
BEQ $79E5
LDA #$40
ORA $90
STA $90
LDA #$00
STA $611F
RTS 


JSR $FF8A
JSR $7D2E
LDA #$FE
PHA 
LDA #$68
PHA 
LDA $6121
PHA 
LDA #$AF
PHA 
RTS 


LDA $BB
LDX $BC
STA $DF02
STX $DF03
LDA $B7
BNE $7A10
RTS 


STA $DF07
LDA #$00
STA $DF08
JSR $7EB0
LDA #$92
LDX #$01
STA $DF04
STX $DF05
LDY #$A0
JMP $7A2A
STY $62F7
LDA $6121
PHA 
LDA #$AF
PHA 
JMP $7A37
PHA 
TXA 
PHA 
LDA $62F7
ORA #$10
PHA 
JMP $7EBE
!byte $00, $DF,$48,$E8
CPX #$0B
BNE $7A42
LDA $6121
PHA 
LDA #$BB
PHA 
LDA $6121
PHA 
LDA #$AF
PHA 
LDA $611D
STA $FE
LDA $611E
STA $FF
CLC 
RTS 


LDA #$8E
LDX #$F6
JMP $7A70
LDA #$AE
LDX #$F5
TAY 
LDA $6121
PHA 
LDA #$AC
PHA 
TXA 
PHA 
TYA 
PHA 
JMP $79BD
PHP 
PHA 
LDA $FE
STA $611D
LDA $FF
STA $611E
LDA $62F8
BEQ $7AC1
TXA 
PHA 
TYA 
PHA 
LDA $6121
STA $FF
LDA #$00
STA $FE
LDA #$2C
LDY #$14
STA ($FE),Y
LDY #$26
STA ($FE),Y
LDA #$60
LDY #$63
STA ($FE),Y
LDY #$00
SEC 
LDA $62F8
SBC ($FE),Y
JSR $7AC4
PLA 
TAY 
PLA 
TAX 
LDA #$00
STA $62F8
PLA 
PLP 
RTS 
JMP ($6109)
STA $6109
STX $610A
LDA $610F
LDX $6110
STA $DF05
JSR $7EB7
LDA $610E
STA $DF04
TYA 
BCC $7AF5
TAX 
LDA $6121
PHA 
LDA #$EE
PHA 
LDA $6121
PHA 
LDA #$CB
PHA 
TXA 
JMP $7AFE
TAX 
LDA $6121
PHA 
LDA #$E8
PHA 
TXA 
STA $62F8
LDA #$00
STA $FE
LDA $6121
STA $FF
LDY #$E2
BCS $7B28
LDA #$90
STA ($FE),Y
LDY #$60
LDA $610B
EOR #$4C
BNE $7B1D
LDY #$EA
TYA 
LDY #$63
STA ($FE),Y
LDA #$2C
LDX #$20
BCC $7B30
LDA #$91
STA ($FE),Y
LDA #$20
LDX #$2C
LDY #$14
STA ($FE),Y
LDY #$26
TXA 
STA ($FE),Y
LDY #$D4
LDA $B9
AND #$0F
STA ($FE),Y
LDY #$00
LDA $62F8
STA ($FE),Y
LDA $6121
STA $DF03
LDA #$E7
STA $DF02
LDA #$00
STA $DF09
LDA #$80
STA $DF0A
LDA #$01
LDX #$00
STA $DF07
STX $DF08
LDX $62F5
LDY $62F6
LDA #$00
STA $62F7
LDA $6120
JMP $7A37

!fill 138, $00 

LDA $99
CMP #$09
BEQ $7C0B
JMP $FFFC
LDA $99
CMP #$09
BEQ $7C14
JMP $FFFC
BIT $7CC9
SEC 
BCS $7C2A
PHA 
LDA $9A
CMP #$09
BEQ $7C25
PLA 
JMP $FFFC
PLA 
BIT $7CC9
CLC 
JSR $7CAD
JMP $78E1
JSR $7C3C
BCS $7C38
JMP $FFFC
STA $99
CLC 
RTS 


LDA #$00
STA $7C01
JSR $F30F
TAX 
JSR $F314
BNE $7C55
JSR $F31F
LDA $BA
CMP #$09
BEQ $7C57
LDA $B8
TAX 
CLC 
RTS 


JSR $7C3C
BCS $7C60
JMP $FFFC
STA $9A
CLC 
RTS 


JSR $7CAD
JMP $7902
JSR $7C45
BCS $7C64
JMP $FFFC
SEC 
BIT $18
JSR $7CAD
JMP $794D
PHA 
TXA 
PHA 
TYA 
PHA 
CLD 
LDA #$7F
STA $DD0D
LDY $DD0D
BMI $7C93
JSR $F6BC
JSR $FFE1
BEQ $7C96
JMP $FE72
CLC 
BIT $38
JSR $7CAD
JMP $7911
!byte $00,$00,$1F,$FF,$00,$00,$00
RTS 
!byte $00, $B2
EOR #$09
BNE $7CC8
JSR $7CF0
PHA 
TXA 
PHA 
LDX #$09
LDA $7C9F,X
PHA 
DEX 
BPL $7CB5
LDX #$09
PLA 
STA $DF01,X
DEX 
BPL $7CBE
PLA 
TAX 
PLA 
RTS 


JSR $7CF0
STA $7CE7
LDA $B9
AND #$0F
CMP #$FF
BNE $7CE6
LDA $7C00
BEQ $7CE6
DEC $7C00
PLA 
PLA 
LDA #$B1
STA $DF01
LDA #$00
CLC 
PHA 
INC $DC03
CLI 
PLA 
RTS 


PHA 
SEI 
LDA #$00
STA $D030
DEC $DC03
PLA 
RTS 
ROL A
BIT $26
ASL $1A20,X
!byte $1C,$30,$32
CLC 
ORA #$12
!byte $23,$36,$5E,$00,$70,$00,$00,$00,$02,$0B,$1A
!byte $30,$58,$98,$6A,$74,$72,$7B,$3D,$F1,$56,$F1
!byte $c9,$f1,$0d,$f2,$4f,$f2,$49,$f3,$90,$f2,$FC
!byte $DE,$F0
DEC $DEF3,X
LDA #$FF
STA $DC01
LDA $6121
STA $FF
LDA #$00
STA $FE
LDX #$09
LDY $7CFC,X
LDA $0301,Y
PHA 
LDA $0300,Y
PHA 
TXA 
ASL A
TAY 
PLA 
CLC 
ADC #$FF
STA $7D1A,Y
PLA 
ADC #$FF
STA $7D1B,Y
LDY $7CFC,X
LDA $0300,Y
LDY $7D06,X
BEQ $7D72
STA ($FE),Y
LDY $7CFC,X
LDA $0301,Y
LDY $7D06,X
INY 
STA ($FE),Y
LDY $7CFC,X
LDA $7D10,X
STA $0300,Y
LDA $FF
STA $0301,Y
DEX 
BPL $7D3E
RTS 


ASL $28,X
BIT $4032
!byte $5A
ROR $6C
!byte $77, $9B, $AF, $B7, $CB
DEC $DED9
ORA $0E
ASL $AA50,X
LDA #$08
LDX #$CF

CLD 
PHA 
TXA 
PHA 
JSR $7CF0
LDX #$00
LDA #$00
STA $6000,X
STA $6100,X
STA $6200,X
INX 
BNE $7DA8
JSR $7E3D
PLA 
TAX 
PLA 
PHA 
TXA 
STA $6121
JSR $7E22
PLA 
JSR $7E08
JSR $7D2E
JSR $655C
JSR $661D
LDA $6121
PHA 
LDA #$E8
PHA 
LDA $6121
PHA 
LDA #$AF
PHA 
RTS 
LDA #$08
LDX #$CF
CLD 
PHA 
TXA 
PHA 
JSR $7CF0
LDX #$08
LDA $7DFF,X
STA $DF02,X
DEX 
BPL $7DEB
LDA #$B2
STA $DF01
PLA 
TAX 
PLA 
JMP $7DBA
BRK 
RTS 


!byte $00,$00,$00,$00, $03, $00,$00
AND #$1F
STA $6122
LDX $6121
STX $FF
LDX #$00
STX $FE
LDX #$04
LDY $7D94,X
STA ($FE),Y
DEX 
BPL $7E18
CLC 
RTS 


STA $FF
LDY #$00
STY $FE
LDA $7C00,Y
STA ($FE),Y
INY 
BNE $7E28
LDX #$0F
LDA $FF
LDY $7D84,X
STA ($FE),Y
DEX 
BPL $7E34
RTS 


LDA #$00
LDX #$00
STA $611B
STX $611C
LDX #$00
TXA 
EOR #$5A
STA $6000,X
DEX 
BNE $7E49
JSR $66D7
INC $611C
BNE $7E52
LDX #$00
TXA 
EOR #$2C
STA $6000,X
DEX 
BNE $7E5C
JSR $66D7
INC $611C
BMI $7E80
JSR $66D4
LDX #$00
TXA 
EOR #$5A
CMP $6000,X;;; 4975
BNE $7E80
DEX 
BNE $7E72
JMP $7E5A
LDA $611B
LDX $611C
STA $6104
STX $6105
LDX #$00
LDA #$20
STA $6106
STX $6107
LDA #$22
STA $6100
STX $6101
STA $6102
STX $6103
LDA #$29
STA $611B
STX $611C
RTS 


LSR $41
!byte $42
STA $7F91
STA $DF06
RTS 
STX $7F91
STX $DF06
RTS 
STA $7F8B
LDA $7F91
AND #$07
CMP #$07
BEQ $7EE7
LDX #$02
LDA $DF00,X
PHA 
INX 
CPX #$06
BNE $7ECC
LDA $7F91
PHA 
LDX #$07
LDA $DF00,X
PHA 
INX 
CPX #$0B
BNE $7EDB
JMP $7A4B
LDA $DF07
SBC #$01
STA $7F8C
LDA $DF08
SBC #$00
STA $7F8D
CLC 
LDA $7F8C
ADC $DF04
STA $7F8E
LDA $7F8D
ADC $DF05
STA $7F8F
LDA $7F91
ADC #$00
STA $7F90
CMP $7F91
BEQ $7ECA
SEC 
LDA #$00
SBC $DF04
STA $DF07
LDA #$00
SBC $DF05
STA $DF08
INC $7F8E
BNE $7F30
INC $7F8F
CLC 
LDA $DF02
ADC $DF07
STA $7F8C
LDA $DF03
ADC $DF08
STA $7F8D
LDA #$7F
PHA 
LDA #$58
PHA 
LDA $6121
PHA 
LDA #$AF
PHA 
PHA 
PHA 
LDA $7F8B
PHA 
JMP $7ECA
LDA #$00
STA $DF04
STA $DF05
LDA $7F8E
STA $DF07
LDA $7F8F
STA $DF08
LDA $7F8C
STA $DF02
LDA $7F8D
STA $DF03
LDX #$02
LDA $DF00,X
PHA 
INX 
CPX #$06
BNE $7F7B
LDA $7F90
PHA 
JMP $7ED9
!byte $00,$00,$00,$00,$00,$00,$00
DEC $DC03
LDA $02A1
AND #$01
BNE $7F95
STA $D030
PLA 
RTS 
