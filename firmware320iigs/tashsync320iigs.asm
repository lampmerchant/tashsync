;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  TashSync IIgs: Apple IIgs Video Sync Signal Converter/Generator
;;;
;


;;; Connections ;;;

;;;                                                              ;;;
;                                PDIP                              ;
;                             .--------.                           ;
;                         nc -|01 \/ 08|- RA3 <-- !MCLR            ;
;                     Supply -|02    07|- Ground                   ;
;     Composite Sync --> RA2 -|03    06|- nc                       ;
;    Horizontal Sync <-- RA1 -|04    05|- RA0 --> Vertical Sync    ;
;                             '--------'                           ;
;                                                                  ;
;                               SOT-23                             ;
;                               .----.                             ;
;        Vertical Sync <-- RA0 -|1o 6|- RA3 <-- !MCLR              ;
;                       Ground -|2  5|- Supply                     ;
;      Horizontal Sync <-- RA1 -|3  4|- RA2 <-- Composite Sync     ;
;                               '----'                             ;
;;;                                                              ;;;


;;; Assembler Directives ;;;

	list		P=PIC10F320, F=INHX32, ST=OFF, MM=OFF, R=DEC, X=ON
	#include	P10F320.inc
	errorlevel	-302	;Suppress "register not in bank 0" messages
	errorlevel	-224	;Suppress TRIS instruction not recommended msgs
	__config	_FOSC_INTOSC & _BOREN_OFF & _WDTE_SWDTEN & _PWRTE_ON & _MCLRE_ON & _CP_OFF &_LVP_OFF & _LPBOR_OFF & _BORV_LO & _WRT_OFF
			;_FOSC_INTOSC	Internal oscillator, I/O on RA1
			;_BOREN_OFF	Brownout reset off
			;_WDTE_SWDTEN	Watchdog timer CONTROLLED BY FIRMWARE
			;_PWRTE_ON	Keep in reset for 64 ms on start
			;_MCLRE_ON	RA3/!MCLR is !MCLR
			;_CP_OFF	Code protection off
			;_LVP_OFF	High-voltage on Vpp to program
			;_LPBOR_OFF	Low power brownout reset disabled
			;_BORV_LO	Brownout reset voltage low trip point
			;_WRT_OFF	Write protection off


;;; Macros ;;;

DNOP	macro
	goto	$+1
	endm


;;; Constants ;;;

PCYCLES	equ	300000	;Number of cycles to spend assessing polarity

;Pin Assignments
VS_PIN	equ	RA0	;Vertical sync output pin
HS_PIN	equ	RA1	;Horizontal sync output pin
CS_PIN	equ	RA2	;Composite sync input pin
SW_PIN	equ	RA3	;Switch pin

;FLAGS:
NEGPULS	equ	7	;Set if horizontal pulses are negative
IHGOTO	equ	6	;Controls the destination of the interrupt handler


;;; Variable Storage ;;;

	cblock	0x40	;Bank 0 registers
	
	FLAGS	;You've got to have flags
	HPERIOD	;Horizontal pulse period
	VPULSES	;Number of vertical sync pulses in a single frame
	TSKIPH	;Value to load up counter with to sleep through horizontal
	TSKIPL	; pulses
	X10
	X9
	X8
	X7
	X6
	X5
	X4	
	X3	;Various purposes
	X2	; "
	X1	; "
	X0	; "
	
	endc


;;; Vectors ;;;

	org	0x00		;Reset vector
	
	movlw	B'01111000'	;16 MHz high-freq internal oscillator
	movwf	OSCCON

	clrf	ANSELA		;All pins digital, not analog

	goto	Init


	org	0x04		;Interrupt vector
	
	;fall through


;;; Interrupt Handler ;;;

Interrupt
	comf	LATA,F		;Toggle vertical sync pin
	bsf	CLC1POL,0	;Make CLC1 latch transparent
	btfss	FLAGS,IHGOTO	;Jump into the appropriate place in the main
	goto	Loop0		; loop depending on the IHGOTO flag (retfie
	goto	Loop2		; would just return us back into the nop field)


;;; Hardware Initialization ;;;

Init
	movlw	B'00010111'	;Watchdog timer resets processor after about
	movwf	WDTCON		; two seconds if not cleared (even in sleep)

	movlw	B'11010000'	;Timer0 counts 1:2 with Fosc (half-microseconds)
	movwf	OPTION_REG

	movlw	B'00000010'	;CLC1 is a transparent latch that passes through
	movwf	CLC1SEL0	; CLC1IN1/RA2 (composite sync) to CLC1/RA1
	clrf	CLC1SEL1	; (horizontal sync) to start with, CLC1IF set on
	clrf	CLC1GLS0	; rising or falling edge of output
	movwf	CLC1GLS1
	clrf	CLC1GLS2
	clrf	CLC1GLS3
	movlw	B'00000001'
	movwf	CLC1POL
	comf	CLC1CON,F

	bsf	LATA,VS_PIN	;Drive vertical sync pin high by default

	movlw	B'00001100'	;Composite sync and switch inputs, vertical and
	movwf	TRISA		; horizontal sync outputs

	clrf	FLAGS		;Initialize key globals

	;fall through


;;; Mainline ;;;

Main
	clrf	X2		;Set the sample high and low up counters to
	clrf	X0		; overflow after PCYCLES / (14 * 256) counts;
	movlw	-(PCYCLES/3584)	; 256 because X2 and X0 are always 0, 14
	movwf	X3		; because the loop below takes 14 cycles per
	movwf	X1		; iteration
Main0	btfss	PORTA,CS_PIN	;If composite sync pin is low, increment X1:0;
	incf	X0,F		; if X1:0 rolls over first, the sync signal is
	btfsc	STATUS,Z	; usually low and thus horizontal pulses are
	incf	X1,F		; positive, so skip setting NEGPULS
	btfsc	STATUS,Z	; "
	goto	Main2		; "
	btfsc	PORTA,CS_PIN	;If composite sync pin is high, increment X3:2;
	incf	X2,F		; if X3:2 rolls over first, the sync signal is
	btfsc	STATUS,Z	; usually high and thus horizontal pulses are
	incf	X3,F		; negative, so set NEGPULS (and clear INTEDG),
	btfss	STATUS,Z	; else loop to keep sampling
	goto	Main0		; "
	bsf	FLAGS,NEGPULS	; "
	bcf	OPTION_REG,INTEDG; "
Main1	call	MostlyUpOrDown	;If horizontal pulses are negative, loop until
	btfsc	STATUS,Z	; we're in an area where composite sync is
	goto	Main1		; mostly positive
Main2	call	MostlyUpOrDown	;Now loop until we're in an area where composite
	btfss	STATUS,Z	; sync is mostly negative
	goto	Main2		; "
Main3	call	MostlyUpOrDown	;Now loop until we're in an area where composite
	btfsc	STATUS,Z	; sync is mostly positive
	goto	Main3		; "
	btfsc	FLAGS,NEGPULS	;If horizontal pulses are negative, we're where
	goto	Main5		; we want to be
Main4	call	MostlyUpOrDown	;If horizontal pulses are positive, loop until
	btfss	STATUS,Z	; we're in an area where composite sync is
	goto	Main4		; mostly negative
Main5	movlw	-6		;Prepare to reset Timer0 with a small margin
	clrf	INTCON		;Wait for a primary edge on composite sync
	btfss	INTCON,INTF	; "
	goto	$-1		; "
	movwf	TMR0		;Reset Timer0
	call	NextEdgeDelta	;Wait for the next edge to establish a time base
	movf	X0,W		;Store the time base as the inter-pulse period
	movwf	HPERIOD		; "
Main6	call	NextEdgeDelta	;Wait for the next edge and update the time base
	subwf	HPERIOD,W	;If the delta is less than the measured period
	btfss	STATUS,C	; (with margin), this is the start of the
	goto	Main6		; vertical sync area
	clrf	VPULSES		;Clear the vertical pulse count
	movlw	1 << INTEDG	;Toggle INTEDG so we capture secondary edges
	xorwf	OPTION_REG,F	; "
	call	NextEdgeDelta	;Wait for the next edge to establish a time base
Main7	incf	VPULSES,F	;Count a vertical pulse
	call	NextEdgeDelta	;Wait for the next edge and update the time base
	subwf	HPERIOD,W	;If the delta is less than the measured period
	btfss	STATUS,C	; (with margin), this is the end of the vertical
	goto	Main7		; sync area
	movlw	1 << INTEDG	;Toggle INTEDG so we capture primary edges again
	xorwf	OPTION_REG,F	; "
	clrf	TSKIPH		;Clear the edge-skip up counter
	clrf	TSKIPL		; "
	call	NextEdgeDelta	;Wait for the next edge to establish a time base
Main8	movf	TSKIPL,F	;Decrement the edge-skip up counter
	btfsc	STATUS,Z	; "
	decf	TSKIPH,F	; "
	decf	TSKIPL,F	; "
	call	NextEdgeDelta	;Wait for the next edge and update the time base
	subwf	HPERIOD,W	;If the delta is less than the measured period
	btfss	STATUS,C	; (with margin), this is the start of the
	goto	Main8		; vertical sync area again
	comf	LATA,F		;Lower vertical sync signal (a bit late, but)
	goto	Loop0		;Jump into loop below

Loop
	bcf	CLC1POL,0	;Latch the idle state of composite sync
	movlw	B'00000001'	;Switch D from composite sync to its inverse
	movwf	CLC1GLS1	; "
	bcf	FLAGS,IHGOTO	;Set interrupt handler to return to Loop0
	movlw	B'10010000'	;Set interrupt handler to fire on primary edge
	movwf	INTCON		; of composite sync
	goto	NopField	;Minimize latency while we wait for interrupt
Loop0	movf	VPULSES,W	;Initialize a countdown of the vertical edges
	movwf	X0		; between here and end of vertical sync
Loop1	movlw	1 << INTE	;Set us to wake on a primary edge
	movwf	INTCON		; "
	sleep			;Sleep until primary edge
	decfsz	X0,F		;Repeat this as many times as there are vertical
	goto	Loop1		; pulses
	bcf	CLC1POL,0	;Latch the active state of composite sync
	movlw	B'00000010'	;Switch D back to non-inverse composite sync
	movwf	CLC1GLS1	; "
	movlw	1 << INTEDG	;Toggle INTEDG so INT interrupt fires on
	xorwf	OPTION_REG,F	; secondary edge of composite sync
	bcf	STATUS,C	;Set Timer0 so it overflows at approximately the
	rrf	HPERIOD,W	; halfway point between horizontal pulses
	xorlw	0xFF		; "
	addlw	20		; "
	movwf	TMR0		; "
	movlw	0		;Copy the NEGPULS flag into W at the position
	btfsc	FLAGS,NEGPULS	; corresponding to the composite sync pin in
	movlw	1 << CS_PIN	; PORTA
	clrf	INTCON		;Wait for Timer0 to overflow
	btfss	INTCON,TMR0IF	; "
	goto	$-1		; "
	xorwf	PORTA,W		;Get the state of the composite sync pin,
	andlw	1 << CS_PIN	; inverting it if NEGPULS was set
	btfsc	STATUS,Z	;If the composite sync pin is inactive now, the
	goto	SignalChange	; signal has changed and we should reanalyze it
	bsf	FLAGS,IHGOTO	;Set interrupt handler to return to Loop2
	movlw	B'10010000'	;Set interrupt handler to fire on secondary edge
	movwf	INTCON		; of composite sync
	goto	NopField	;Minimize latency while we wait for interrupt
Loop2	movf	TSKIPH,W	;Initialize an up counter of the secondary edges
	movwf	X1		; between here and the point before the vertical
	movf	TSKIPL,W	; sync inversion
	movwf	X0		; "
Loop3	movlw	1 << INTE	;Set us to wake on a secondary edge
	movwf	INTCON		; "
	sleep			;Sleep until secondary edge
	incfsz	X0,F		;Increment the up counter, looping until it hits
	goto	Loop3		; zero
	incfsz	X1,F		; "
	goto	Loop3		; "
	movlw	1 << INTEDG	;Toggle INTEDG so INT interrupt fires on primary
	xorwf	OPTION_REG,F	; edge of composite sync
	goto	Loop		;Back pre vertical inversion, repeat main loop

SignalChange
	movlw	B'00000001'	;We have no reset instruction on this PIC so set
	movwf	WDTCON		; the WDT to its shortest interval and loop
	goto	$		; until it resets us

NopField
	nop			;Minimize interrupt latency in an infinite loop;
	nop			; interrupt would be delayed by one cycle if the
	nop			; edge happened in the first half of a goto
	nop			; instruction, but this way the probability of
	nop			; that happening is significantly lowered
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	nop			; "
	goto	NopField	;Loop


;;; Subprograms ;;;

;Determine whether, over a period of ~32 us, the composite sync signal is mostly
; high or mostly low, returning with Z set for low and clear for high.
;Clobbers: X0, W
MostlyUpOrDown
	movlw	16		;Set the counter at the midpoint between the low
	movwf	X0		; trigger (0) and the high trigger (32)
MoUpDn0	btfsc	PORTA,CS_PIN	;If composite sync is high, skip ahead, else
	goto	MoUpDn1		; waste a cycle and proceed so the conditional
	nop			; always takes three cycles
	decf	X0,F		;Composite sync was low, so decrement counter
	btfsc	STATUS,Z	;If counter hit zero, return with Z set
	return			; "
	goto	MoUpDn0		;If not, loop
MoUpDn1	incf	X0,F		;Composite sync was high, so increment counter
	btfsc	X0,5		;If counter hit 32, return with Z clear
	return			; "
	goto	MoUpDn0		;If not, loop

;Wait for an edge on INT, get the value of Timer0, update the time base in X0,
; and return with the delta between Timer0 and the last time base.
;Clobbers: C, DC, Z
NextEdgeDelta
	clrf	INTCON		;Wait for a primary edge on composite sync
	btfss	INTCON,INTF	; "
	goto	$-1		; "
	movf	TMR0,W		;Get the current value of Timer0, update the
	subwf	X0,W		; time base in X0 and leave the delta in W
	sublw	0		; "
	addwf	X0,F		; "
	return			;Done


;;; End of Program ;;;

	end
