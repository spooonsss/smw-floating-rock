;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Yoshi's Island Floating Slippery Rock Platform
; Programmed by SMWEdit
;
; Uses first extra bit: NO
;
; You will need to patch SMKDan's dsx.asm to your ROM with xkas
; this sprite, like all other dynamic sprites, uses the last 4 rows of sp4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; there's nothing customizable here
prot gfx


        !OFFSET = !1528

        !PLAT_THK = $07		; pixels below stand position to assume standing, higher for steeper slopes
        !PLAT_STICKY = $02	; pixels above stand position to assume standing, higher for steeper slopes
        !PLAT_X1 = $00		; \
        !PLAT_X2 = $02		;  | scratch RAM addresses set before processing
        !PLAT_Y1 = $04		;  | slope. !PLAT_X1 must be less than !PLAT_X2
        !PLAT_Y2 = $06		; /
        !YPOSTMP = $08		; scratch RAM used in slope processing for Y position of standing
        !TMP1 = $0A		; general use scratch RAM
        !TMP2 = $0C

        !PLAT_H = !PLAT_THK+!PLAT_STICKY	; total height of vertical interaction in any given point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INIT and MAIN JSL targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        PRINT "INIT ",pc
        LDA #$0F
        STA !OFFSET,x
        RTL

        PRINT "MAIN ",pc
        PHB
        PHK
        PLB
        JSR SPRITE_ROUTINE
        PLB
        RTL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPRITE_ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

X1DATA:		dw $FFF0,$FFF0,$FFF0,$FFF0
        dw $FFF0,$FFF1,$FFF1,$FFF1
        dw $FFF1,$FFF2,$FFF2,$FFF3
        dw $FFF3,$FFF4,$FFF4,$FFF5
        dw $FFF5,$FFF6,$FFF7,$FFF7
        dw $FFF8,$FFF9,$FFF9,$FFFA
        dw $FFFB,$FFFC,$FFFD,$FFFD
        dw $FFFE,$FFFF,$0000

Y1DATA:		dw $FFF0,$FFEF,$FFEE,$FFED
        dw $FFED,$FFEC,$FFEB,$FFEA
        dw $FFE9,$FFE9,$FFE8,$FFE7
        dw $FFE7,$FFE6,$FFE5,$FFE5
        dw $FFE4,$FFE4,$FFE3,$FFE3
        dw $FFE2,$FFE2,$FFE1,$FFE1
        dw $FFE1,$FFE1,$FFE0,$FFE0
        dw $FFE0,$FFE0,$FFE0

X2DATA:		dw $0000,$0001,$0002,$0003
        dw $0003,$0004,$0005,$0006
        dw $0007,$0007,$0008,$0009
        dw $0009,$000A,$000B,$000B
        dw $000C,$000C,$000D,$000D
        dw $000E,$000E,$000F,$000F
        dw $000F,$000F,$0010,$0010
        dw $0010,$0010,$0010

Y2DATA:		dw $FFE0,$FFE0,$FFE0,$FFE0
        dw $FFE0,$FFE1,$FFE1,$FFE1
        dw $FFE1,$FFE2,$FFE2,$FFE3
        dw $FFE3,$FFE4,$FFE4,$FFE5
        dw $FFE5,$FFE6,$FFE7,$FFE7
        dw $FFE8,$FFE9,$FFE9,$FFEA
        dw $FFEB,$FFEC,$FFED,$FFED
        dw $FFEE,$FFEF,$FFF0

RETURN1:		RTS
NO_STAND1:	JMP NO_STAND

SPRITE_ROUTINE:	JSR SUB_GFX
        LDA !14C8,x		; \  RETURN if
        CMP #$08		;  | sprite status
        BNE RETURN1		; /  is not 8
        LDA $9D			; \ RETURN if
        BNE RETURN1		; / sprites locked
        %SubOffScreen()

        LDA !OFFSET,x		; \  get index to
        ASL A			;  | platform endpoint
        TAY			; /  coords tables
        PHP			; back up processor bits
        REP #%00100000		; 16 bit A/math
        LDA X1DATA,y		; \
        STA !PLAT_X1		;  | set ledge
        LDA X2DATA,y		;  | endpoint
        STA !PLAT_X2		;  | coordinates
        LDA Y1DATA,y		;  |
        STA !PLAT_Y1		;  |
        LDA Y2DATA,y		;  |
        STA !PLAT_Y2		; /
        PLP			; load backed up processor bits

        LDA $187A|!Base2		; \
        BEQ NOYOSHI1		;  | temporarily
        LDA $96			;  | offset Mario's
        CLC			;  | Y position if
        ADC #$10		;  | on Yoshi
        STA $96			;  |
        LDA $97			;  |
        ADC #$00		;  |
        STA $97			; /
NOYOSHI1:

        LDA $7D			; \ no standing if
        BMI NO_STAND1		; / Mario is going up
        LDA !E4,x		; \
        CLC			;  | if Mario
        ADC !PLAT_X1		;  | is before
        STA !TMP1		;  | X1, then
        LDA !14E0,x		;  | don't
        ADC !PLAT_X1+1		;  | stand
        STA !TMP1+1		;  |
        LDA !TMP1		;  |
        CMP $94			;  |
        LDA !TMP1+1		;  |
        SBC $95			;  |
        BPL NO_STAND1		; /
        LDA !E4,x		; \
        CLC			;  | if Mario
        ADC !PLAT_X2		;  | is after
        STA !TMP1		;  | X2, then
        LDA !14E0,x		;  | don't
        ADC !PLAT_X2+1		;  | stand
        STA !TMP1+1		;  |
        LDA !TMP1		;  |
        CMP $94			;  |
        LDA !TMP1+1		;  |
        SBC $95			;  |
        BMI NO_STAND1		; /
        LDA !PLAT_Y1		; \
        SEC			;  | offset Y
        SBC #!PLAT_STICKY	;  | coordinates
        STA !PLAT_Y1		;  | by number
        LDA !PLAT_Y1+1		;  | of pixels
        SBC #$00		;  | to extend
        STA !PLAT_Y1+1		;  | platform
        LDA !PLAT_Y2		;  | interaction
        SEC			;  | upwards
        SBC #!PLAT_STICKY	;  |
        STA !PLAT_Y2		;  |
        LDA !PLAT_Y2+1		;  |
        SBC #$00		;  |
        STA !PLAT_Y2+1		; /
        LDA !E4,x		; \
        CLC			;  | get Mario's
        ADC !PLAT_X1		;  | X position
        STA !TMP1		;  | relative to
        LDA $94			;  | platform start
        SEC			;  |
        SBC !TMP1		; /
if !sa1 == 0
        STA $4202		; set as multiplicand
else
        STZ $2250 ; -----OO    O = operation        00 = Multiplication        01 = Division
        STA $2251
        STZ $2252		; set as multiplicand
endif
        LDA !PLAT_Y2		; \
        CMP !PLAT_Y1		;  | negative slope
        LDA !PLAT_Y2+1		;  | or positive
        SBC !PLAT_Y1+1		;  | slope?
        BMI NEG_SLP		; /
        LDA !PLAT_Y2		; \  POSITIVE:
        SEC			;  | Y2-Y1 is "rise"
        SBC !PLAT_Y1		; /  in slope
if !sa1 == 0
        STA $4203		; set as multiplier
else
        STA $2253
        STZ $2254		; set as multiplier
endif
        LDY #$00		; Y=0 means slope is positive
        BRA STARTDIV		; skip over to division
NEG_SLP:
        LDA !PLAT_Y1		; \  NEGATIVE:
        SEC			;  | Y1-Y2 is "rise" in
        SBC !PLAT_Y2		; /  slope (going other way)
if !sa1 == 0
        STA $4203		; set as multiplier
else
        STA $2253
        STZ $2254		; set as multiplier
endif
        LDY #$01		; Y=1 means slope is negative
if !sa1 == 0
STARTDIV:
        NOP #3 ; 2 cycles from LDY# already

        LDA $4216		; \
        STA $4204		;  | put product
        STA !TMP2
        LDA $4217		;  | into dividend
        STA $4205		; /
        LDA !PLAT_X2		; \  get "run" (X)
        SEC			;  | in slope
        SBC !PLAT_X1		; /
        STA $4206		; set as divisor
        NOP #8

        LDA $4214		; \ quotient goes

else
STARTDIV:
        NOP #2
        LDA $2306		; \
        PHA
        LDA #$01
        STA $2250
        PLA
        STA !TMP2
        STA $2251
        STZ $2252		;  | put product
        LDA $2307		;  | into dividend
        STA $2252		; /
        LDA !PLAT_X2		; \  get "run" (X)
        SEC			;  | in slope
        SBC !PLAT_X1		; /
        STA $2253
        STZ $2254
        NOP
        NOP
        NOP      		; set as divisor
        LDA $2306		; \ quotient goes
endif
        STA !TMP1		; / into scratch RAM
        STZ !TMP1+1		; and zero high byte
        PHP			; back up processor bits
        REP #%00100000		; 16 bit A/math
if !sa1 == 0
        LDA $4216		; \
else
        LDA $2308 ; conversion had LDA $2306		; \
endif
        ASL A			;  | round up if
        ; CMP $4204		;  | remainder >=  original was this, but this is open bus
        CMP !TMP2
        BCC CHKNEG		;  | 1/2 dividend
        INC !TMP1		; /
CHKNEG:		CPY #$00		; \ if Y=0 (not negative)
        BEQ ADDYINT		; / then don't make negative
        LDA !TMP1		; \  inverse plus
        EOR.w #$FFFF		;  | 1 is equivalent
        INC A			;  | to subtracting
        STA !TMP1		; /  from zero
ADDYINT:		LDA !TMP1		; \  since X1 is considered
        CLC			;  | to be the "zero", then
        ADC !PLAT_Y1		; /  Y1 is the Y intercept
        STA !YPOSTMP		; set sum to standing Y position
        PLP			; load backed up processor bits
        LDA !D8,x		; \
        CLC			;  | actual Y
        ADC !YPOSTMP		;  | position
        STA !TMP1		;  | is offset
        LDA !14D4,x		;  | by sprite's
        ADC !YPOSTMP+1		;  | Y position
        STA !TMP1+1		; /
        LDA !TMP1		; \
        CMP $96			;  | if Mario is higher
        LDA !TMP1+1		;  | than this, then
        SBC $97			;  | there's no standing
        BPL NO_STAND		; /
        LDA !TMP1		; \
        CLC			;  | get lower
        ADC #!PLAT_H		;  | standing
        STA !TMP1		;  | boundary
        LDA !TMP1+1		;  |
        ADC #$00		;  |
        STA !TMP1+1		; /
        LDA !TMP1		; \
        CMP $96			;  | if Mario is lower
        LDA !TMP1+1		;  | than this, then
        SBC $97			;  | there's no standing
        BMI NO_STAND		; /
        LDA #$01		; \ set standing
        STA $1471|!Base2		; / mode
        LDA #$06                ; \ set riding
        STA !154C,x             ; / sprite mode
        LDA !D8,x		; \
        CLC			;  | set Mario
        ADC !YPOSTMP		;  | in standing
        STA $96			;  | Y position
        LDA !14D4,x		;  |
        ADC !YPOSTMP+1		;  |
        STA $97			; /
        LDA $96			; \
        CLC			;  | offset
        ADC #!PLAT_STICKY	;  | resulting
        STA $96			;  | position by
        LDA $97			;  | interaction
        ADC #$00		;  | upper extent
        STA $97			; /
        %SubHorzPos()	; \  decide which
        CPY #$00		;  | way to tilt
        BNE DECROT		; /
INCROT:		LDA !OFFSET,x		; \  when it's
        CMP #$1E		;  | clockwise, don't
        BCS ENDINC		; /  go past 1E
        INC !OFFSET,x		; if not 1E, increment rotation offset
ENDINC:		INC $94			; \  make mario
        BNE ENDINC2		;  | slip off
        INC $95			; /  of slope
ENDINC2:		BRA ENDROT		; and skip CCW rotation
DECROT:		LDA !OFFSET,x		; \ when it's CCW, don't go
        BEQ ENDDEC		; / backwards past zero
        DEC !OFFSET,x		; if not zero, decrement rotation offset
ENDDEC:		DEC $94			; \  make mario
        BNE ENDROT		;  | slip off
        DEC $95			; /  of slope
ENDROT:
        BRA ENDFLATROT		; skip over code to rotate back to a flat platform
NO_STAND:
        LDA !OFFSET,x		; \  don't turn
        CMP #$0F		;  | back if
        BEQ ENDFLATROT		; /  already there
        BCS DECFLAT		; if current offset > flat offset, go to rotate CCW
        INC !OFFSET,x		; rotate CW
        BRA ENDFLATROT		; and skip CCW
DECFLAT:		DEC !OFFSET,x		; rotate CCW
ENDFLATROT:

        LDA $187A|!Base2		; \
        BEQ NOYOSHI2		;  | reverse
        LDA $96			;  | temporary
        SEC			;  | Mario offset
        SBC #$10		;  | if on Yoshi
        STA $96			;  |
        LDA $97			;  |
        SBC #$00		;  |
        STA $97			; /
NOYOSHI2:
        JSL $019138		; interact with objects

        LDA !164A,x		; \ in water
        BNE INWATER		; / or not?
INAIR:		INC !AA,x		; increase Y speed for gravity
        LDA !AA,x		; \ skip speed reducer
        BPL ENDFLOAT		; / if Y is positive
        CMP #$FA		; \ > FA (-6) means
        BCS ENDFLOAT		; / skip speed reducer
        LDA #$FA		; \ keep
        STA !AA,x		; / at FA
        BRA ENDFLOAT		; skip in-water code
INWATER:		LDA $14			; \
        AND #%00000001		;  | float up half the speed
        BNE WATERCHKSPD		;  | it would be falling down
        DEC !AA,x		; /
WATERCHKSPD:	LDA !AA,x		; \ skip speed reducer
        BMI ENDFLOAT		; / if Y is negative
        CMP #$06		; \ < 06 means
        BCC ENDFLOAT		; / skip speed reducer
        LDA #$06		; \ keep
        STA !AA,x		; / at 06
ENDFLOAT:

            JSL $01801A             ; Update Y position without gravity
RETURN:		RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRAPHICS ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        !TILESDRAWN = $08	; \ scratch RAM
        !TEMP_FOR_TILE = $03	; / addresses

ROCK_TILES:	db $00,$02,$20,$22
ROCK_XPOS:	db $F8,$08,$F8,$08
ROCK_YPOS:	db $F8,$F8,$08,$08

SUB_GFX:		%GetDrawInfo()
        STZ !TILESDRAWN		; zero tiles drawn
        JSR DRAW_ROCK		; draw rock
        JSR SETTILES		; set tiles / don't draw offscreen
ENDSUB:		RTS

DRAW_ROCK:	LDA !OFFSET,x		; get frame number
        JSR OFF2FRM		; convert to dynamic pointer
        JSR GETSLOT		; call routine to get a slot
        BEQ ENDSUB		; if none left, end
        STA !TEMP_FOR_TILE	; store tile into scratch RAM
        PHX			; back up X
        LDX #$00		; load X with zero
TILELP:		CPX #$04		; end of loop?
        BEQ  RETFRML		; if so, then end
        LDA $00			; get sprite's X position
        CLC			; \ offset by
        ADC ROCK_XPOS,x		; / tile's X
        STA $0300|!Base2,y		; set tile's X position
        LDA $01			; get sprite's Y position
        CLC			; \ offset by
        ADC ROCK_YPOS,x		; / tile's Y
        STA $0301|!Base2,y		; set tile's Y position
        LDA !TEMP_FOR_TILE	; load tile # from scratch RAM
        CLC			; \ shift tile right/down
        ADC ROCK_TILES,x	; / according to which part
        STA $0302|!Base2,y		; set tile #
        PHX			; back up X (index to tile data)
        LDX $15E9|!Base2		; load X with index to sprite
        LDA !15F6,x		; load palette info
        ORA $64			; add in priority bits
        STA $0303|!Base2,y		; set extra info
        PLX			; load backed up X
        INC !TILESDRAWN		; another tile was drawn
        INY #4
        INX			; next tile to draw
        BRA TILELP		; loop
RETFRML:		PLX			; load backed up X
ENDROCK:		RTS

SETTILES:	LDA !TILESDRAWN		; \ don't call sub
        BEQ NODRAW		; / if no tiles
        LDY #$02		; #$02 means 16x16
        DEC A			; A = # tiles - 1
        JSL $01B7B3		; don't draw if offscreen
NODRAW:		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dynamic sprite routine
; Programmed mainly by SMKDan, but based on some of my code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!TEMP = $09

!SLOTPTR = $0660|!Base2		;16bit pointer for source GFX
!SLOTBANK = $0662|!Base2	;bank
!SLOTDEST = $0663|!Base2	;VRAM address
!SLOTSUSED = $06FE|!Base2	;how many SLOTS have been used

!MAXSLOTS = $04		;maximum selected SLOTS

SLOTS: db $CC,$C8,$C4,$C0	;avaliable SLOTS.  Any more transfers and it's overflowing by a dangerous amount.

GETSLOT:
    PHY		;preserve OAM index
    PHA		;preserve frame
    LDA !SLOTSUSED	;test if slotsused == maximum allowed
    CMP #!MAXSLOTS
    BEQ NONEFREE

    PLA		;pop frame
    REP #$20	;16bit A
    AND.w #$00FF	;wipe high
    XBA		;<< 8
    LSR A		;>> 1 = << 7
    STA !TEMP	;back to scratch
    LDA.w #gfx
    CLC
    ADC !TEMP	;add frame offset
    STA !SLOTPTR	;store to pointer to be used at transfer time
    SEP #$20	;8bit store
    LDA.b #bank(gfx)
    STA !SLOTBANK	;store bank to 24bit pointer

    LDY !SLOTSUSED	;calculate VRAM address + tile number
    LDA SLOTS,y	;get tile# in VRAM
    PHA		;preserve for eventual pull
    SEC
    SBC #$C0	;starts at C0h, they start at C0 in tilemap
    REP #$20	;16bit math
    AND.w #$00FF	;wipe high byte
    ASL #5
    CLC
    ADC #$0B44	;add 0B44, base address of buffer
    STA !SLOTDEST	;destination address in the buffer

    JSR DMABUFFER	;ROM -> RAM copy

    SEP #$20	;8bit A
    INC !SLOTSUSED	;one extra slot has been used

    PLA		;RETURN starting tile number
    PLY
    RTS

NONEFREE:
    PLA
    PLY
    LDA #$00	;zero on no free SLOTS
    RTS

;;;;;;;;;;;;;;;;
;Tansfer routine
;;;;;;;;;;;;;;;;

;DMA ROM -> RAM ROUTINE

if !sa1
DMABUFFER:
;set destination RAM address
    REP #$20
    LDY #$C4
    STY $2230
    LDA.w !SLOTDEST
    ADC #$74BC
    STA $2235	;16bit RAM dest

                 ;set 7F as bank

;common DMA settings
                 ;1 reg only
                ;to 2180, RAM write/read


;first line
    LDA !SLOTPTR
    STA $2232	;low 16bits
    LDY !SLOTBANK
    STY $2234	;bank
    LDY #$80	;128 bytes
    STZ $2238
    STY $2238
    LDY #$41
    STY $2237

    LDY $318C
    BEQ $FB
    LDY #$00
    STY $318C
    STY $2230	;transfer

;second line
    LDY #$C4
    STY $2230
    LDA.w !SLOTDEST	;update buffer dest
    CLC
    ADC #$0200	;512 byte rule for sprites
    STA !SLOTDEST	;updated base
    ADC #$74BC
    STA $2235	;updated RAM address

    LDA !SLOTPTR	;update source address
    CLC
    ADC #$0200	;512 bytes, next row
    STA !SLOTPTR
    STA $2232	;low 16bits
    LDY !SLOTBANK
    STY $2234	;bank
    LDY #$80
    STZ $2238
    STY $2238
    LDY #$41
    STY $2237

    LDY $318C
    BEQ $FB
    LDY #$00
    STY $318C
    STY $2230	;transfer

;third line
    LDY #$C4
    STY $2230
    LDA.w !SLOTDEST	;update buffer dest
    CLC
    ADC #$0200	;512 byte rule for sprites
    STA !SLOTDEST	;updated base
    ADC #$74BC
    STA $2235	;updated RAM address

    LDA !SLOTPTR	;update
    CLC
    ADC #$0200
    STA !SLOTPTR
    STA $2232
    LDY !SLOTBANK
    STY $2234
    LDY #$80
    STZ $2238
    STY $2238
    LDY #$41
    STY $2237

    LDY $318C
    BEQ $FB
    LDY #$00
    STY $318C
    STY $2230	;transfer

;fourth line
    LDY #$C4
    STY $2230
    LDA.w !SLOTDEST	;update buffer dest
    CLC
    ADC #$0200	;512 byte rule for sprites
    STA !SLOTDEST	;updated base
    ADC #$74BC
    STA $2235	;updated RAM address

    LDA !SLOTPTR
    CLC
    ADC #$0200
    STA !SLOTPTR
    STA $2232
    LDY !SLOTBANK
    STY $2234
    LDY #$80
    STZ $2238
    STY $2238
    LDY #$41
    STY $2237

    LDY $318C
    BEQ $FB
    LDY #$00
    STY $318C
    STY $2230

    SEP #$20	;8bit A
    RTS		;all done, RETURN
else
DMABUFFER:
;set destination RAM address
    REP #$20
    LDA !SLOTDEST
    STA $2181	;16bit RAM dest
    LDY #$7F
    STY $2183	;set 7F as bank

;common DMA settings
    STZ $4300	;1 reg only
    LDY #$80	;to 2180, RAM write/read
    STY $4301

;first line
    LDA !SLOTPTR
    STA $4302	;low 16bits
    LDY !SLOTBANK
    STY $4304	;bank
    LDY #$80	;128 bytes
    STY $4305
    LDY #$01
    STY $420B	;transfer

;second line
    LDA !SLOTDEST	;update buffer dest
    CLC
    ADC #$0200	;512 byte rule for sprites
    STA !SLOTDEST	;updated base
    STA $2181	;updated RAM address

    LDA !SLOTPTR	;update source address
    CLC
    ADC #$0200	;512 bytes, next row
    STA !SLOTPTR
    STA $4302	;low 16bits
    LDY !SLOTBANK
    STY $4304	;bank
    LDY #$80
    STY $4305
    LDY #$01
    STY $420B	;transfer

;third line
    LDA !SLOTDEST	;update buffer dest
    CLC
    ADC #$0200	;512 byte rule for sprites
    STA !SLOTDEST	;updated base
    STA $2181	;updated RAM address

    LDA !SLOTPTR	;update
    CLC
    ADC #$0200
    STA !SLOTPTR
    STA $4302
    LDY !SLOTBANK
    STY $4304
    LDY #$80
    STY $4305
    LDY #$01
    STY $420B	;transfer

;fourth line
    LDA !SLOTDEST	;update buffer dest
    CLC
    ADC #$0200	;512 byte rule for sprites
    STA !SLOTDEST	;updated base
    STA $2181	;updated RAM address

    LDA !SLOTPTR
    CLC
    ADC #$0200
    STA !SLOTPTR
    STA $4302
    LDY !SLOTBANK
    STY $4304
    LDY #$80
    STY $4305
    LDY #$01
    STY $420B

    SEP #$20	;8bit A
    RTS		;all done, RETURN

endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This will convert a frame offset
;; into a pointer for a dynamic frame
;;   OFFSETS:
;;   00 01 02 03
;;   04 05 06 07
;;   08 09 0A 0B
;;   0C 0D 0E 0F
;;
;;   FRAMES:
;;   00 01 02 03
;;   10 11 12 13
;;   20 21 22 23
;;   30 31 32 33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        !OFF2FRM_TEMP = $04

OFF2FRM:		PHY
        STA !OFF2FRM_TEMP
        AND #%00000011
        TAY
        LDA !OFF2FRM_TEMP
        ASL A
        ASL A
        AND #%11110000
        STY !OFF2FRM_TEMP
        ORA !OFF2FRM_TEMP
        PLY
        RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

incbin floatingrock.bin -> gfx		;included graphics file
