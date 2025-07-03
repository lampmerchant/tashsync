;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  TashSync: Video Sync Signal Converter/Generator for Macintosh IIsi
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
	__config	_FOSC_INTOSC & _BOREN_OFF & _WDTE_SWDTEN & _PWRTE_ON & _MCLRE_ON & _CP_OFF &_LVP_ON & _LPBOR_OFF & _BORV_LO & _WRT_OFF
			;_FOSC_INTOSC	Internal oscillator, I/O on RA1
			;_BOREN_OFF	Brownout reset off
			;_WDTE_SWDTEN	Watchdog timer CONTROLLED BY FIRMWARE
			;_PWRTE_ON	Keep in reset for 64 ms on start
			;_MCLRE_ON	RA3/!MCLR is !MCLR
			;_CP_OFF	Code protection off
			;_LVP_ON	Low-voltage programming supported
			;_LPBOR_OFF	Low power brownout reset disabled
			;_BORV_LO	Brownout reset voltage low trip point
			;_WRT_OFF	Write protection off


;;; Macros ;;;

DNOP	macro
	goto	$+1
	endm

IHCALL	macro	lbl
	movlw	low ($+5)
	movwf	IHGOTO
	movlw	B'10010000'
	movwf	INTCON
	goto	lbl
	movf	IHWREG,W
	endm

XCALL	macro	lbl
	movlw	low ($+3)
	movwf	XGOTO
	goto	lbl
	movf	XWREG,W
	endm

XRETURN	macro
	movwf	XWREG
	movf	XGOTO,W
	movwf	PCL
	endm


;;; Constants ;;;

LONGMIN	equ	6	;Pulses longer than LONGMIN*5 cycles are considered long

;Pin Assignments
VS_PIN	equ	RA0	;Vertical sync output pin
HS_PIN	equ	RA1	;Horizontal sync output pin
CS_PIN	equ	RA2	;Composite sync input pin

;FLAGS:
IHVSYNC	equ	7	;If set, interrupt handler toggles vertical sync


;;; Variable Storage ;;;

	cblock	0x40	;Bank 0 registers
	
	FLAGS	;You've got to have flags
	VPULSES	;Number of vertical sync pulses in a single frame
	TSKIPH	;Value to load up counter with to sleep through horizontal
	TSKIPL	; pulses
	IHWREG	;W register at the time of the interrupt handler being called
	IHGOTO	;Where the interrupt handler should return to after finishing
	XWREG	;W register at the time of returning from the XCALL subprogram
	XGOTO	;Where XCALL should return to after finishing
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
	btfsc	FLAGS,IHVSYNC	;Toggle vertical sync pin if IHVSYNC flag set
	comf	LATA,F		; "
	movwf	IHWREG		;Save W register value
	movf	IHGOTO,W	;Go to the designated location (we can't modify
	movwf	PCL		; the stack, so just ignore it)


;;; Hardware Initialization ;;;

Init
	movlw	B'00010111'	;Watchdog timer resets processor after about
	movwf	WDTCON		; two seconds if not cleared (even in sleep)

	movlw	B'00000100'	;Timer2 counts 1:1 with Fosc
	movwf	T2CON

	movlw	B'10010000'	;PWM1 on, active low
	movwf	PWM1CON

	movlw	B'10011111'	;Timer0 counts 1:1 with Fosc, INT pin interrupts
	movwf	OPTION_REG	; on falling edge

	movlw	B'00110010'	;CLC1 is a transparent latch that passes through
	movwf	CLC1SEL0	; CLC1IN1/RA2 (composite sync) to CLC1/RA1
	clrf	CLC1SEL1	; (horizontal sync) to start with, CLC1IF set on
	clrf	CLC1GLS0	; rising or falling edge of output, PWM1
	movlw	B'00000010'	; available as second selection
	movwf	CLC1GLS1
	clrf	CLC1GLS2
	clrf	CLC1GLS3
	movlw	B'00000001'
	movwf	CLC1POL
	comf	CLC1CON,F

	bsf	LATA,VS_PIN	;Drive vertical and horizontal sync pins high by
	bsf	LATA,HS_PIN	; default

	movlw	B'00001100'	;Composite sync and switch inputs, vertical and
	movwf	TRISA		; horizontal sync outputs

	clrf	FLAGS		;Initialize key globals

	;fall through


;;; Mainline ;;;

Main
	call	MostlyUpOrDown	;As horizontal pulses are negative, loop until
	btfsc	STATUS,Z	; we're in an area where composite sync is
	goto	Main		; mostly positive
Main0	call	MostlyUpOrDown	;Now loop until we're in an area where composite
	btfss	STATUS,Z	; sync is mostly negative (the vertical sync
	goto	Main0		; area)
Main1	call	MostlyUpOrDown	;Now loop until we're in an area where composite
	btfsc	STATUS,Z	; sync is mostly positive (just out of the
	goto	Main1		; vertical sync area)
	XCALL	NextEdgeDelta	;Wait for the next edge to establish a time base
	XCALL	NextEdgeDelta	;Get the inter-pulse period and store it so it
	movwf	PR2		; can be used by Timer2 and the PWM module,
	decf	PR2,F		; decremented by one (because period is PR2 + 1)
	btfss	PORTA,CS_PIN	;Wait until composite sync is inactive (high)
	goto	$-1		; "
	bsf	OPTION_REG,INTEDG;Toggle INTEDG so we capture secondary edges
	XCALL	NextEdgeDelta	;Wait for the next edge and update the time base
	subwf	PR2,W		;Subtract the period from the delta to get the
	sublw	-2		; pulse width and store that as the duty cycle
	movwf	PWM1DCH		; to be used by the PWM module
	bcf	OPTION_REG,INTEDG;Toggle INTEDG so we capture primary edges
	clrf	VPULSES		;Start vertical pulses count off at zero
Main2	call	MeasurePulse	;Wait for a long (vertical) pulse
	btfss	STATUS,Z	; "
	goto	Main2		; "
	clrf	TSKIPH		;Start (negated) horizontal pulse count off at
	clrf	TSKIPL		; zero
Main3	incf	VPULSES,F	;Count the vertical pulse
	call	MeasurePulse	;Wait for a short (horizontal) pulse, counting
	btfsc	STATUS,Z	; vertical pulses while we wait
	goto	Main3		; "
Main4	movf	TSKIPL,F	;Decrement the (negated) horizontal pulse count
	btfsc	STATUS,Z	; to count the horizontal pulse
	decf	TSKIPH,F	; "
	decf	TSKIPL,F	; "
	call	MeasurePulse	;Wait for a long (vertical) pulse, counting
	btfss	STATUS,Z	; horizontal pulses backwards while we wait
	goto	Main4		; "
	decf	VPULSES,W	;Count down vertical pulses until we get to
	btfsc	STATUS,Z	; before the first horizontal pulse
	goto	Main6		; "
	movwf	X1		; "
Main5	call	MeasurePulse	; "
	decfsz	X1,F		; "
	goto	Main5		; "
Main6	movf	TSKIPH,W	;Set up an up counter of horizontal pulses
	movwf	X2		; "
	movf	TSKIPL,W	; "
	movwf	X1		; "
	bsf	FLAGS,IHVSYNC	;Interrupt handler should toggle vertical sync
	;fall through

Loop
	movlw	1 << INTE	;Count and sleep us past all the horizontal
	movwf	INTCON		; edges until we're just before the first
	sleep			; vertical one (the first edge we catch will be
	bcf	OPTION_REG,INTEDG; a secondary edge, but all following will be
	incfsz	X1,F		; primary edges)
	goto	Loop		; "
	incfsz	X2,F		; "
	goto	Loop		; "
	movf	PR2,W		;Force PWM1 to go into active state (7 is the
	addlw	-7		; number of cycles between movwf TMR2 here and
	movwf	TMR2		; the first in ClearTimer2)
	bsf	CLC1GLS1,3	;Gate PWM1 into horizontal sync pin
	IHCALL	ClearTimer2	;Repeatedly clear Timer2 until primary edge
	decf	VPULSES,W	;Count through vertical pulses
	btfsc	STATUS,Z	; "
	goto	Loop1		; "
	movwf	X1		; "
Loop0	call	MeasurePulse	; "
	decfsz	X1,F		; "
	goto	Loop0		; "
Loop1	btfsc	PORTA,CS_PIN	;If composite sync is high here, signal has
	goto	SignalChange	; changed and we need to re-analyze
	movf	TSKIPH,W	;Set up an up counter of horizontal pulses
	movwf	X2		; "
	movf	TSKIPL,W	; "
	movwf	X1		; "
	bsf	OPTION_REG,INTEDG;Wait until secondary edge, then flip vertical
	IHCALL	PollTimer0	; sync (we have more of a time cushion this way)
	bcf	CLC1GLS1,3	;Gate PWM1 out of horizontal sync pin
	goto	Loop		;Loop

SignalChange
	movlw	B'00000001'	;We have no reset instruction on this PIC so set
	movwf	WDTCON		; the WDT to its shortest interval and loop
	goto	$		; until it resets us


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

;Measure the next pulse on composite sync and return with Z clear if it's short,
; set if it's long, as determined by the LONGMIN constant.
;Clobbers: X0, W
MeasurePulse
	movlw	LONGMIN		;Initialize a countdown after which a pulse is
	movwf	X0		; considered long
	clrf	INTCON		;Wait for a primary edge on composite sync
	btfss	INTCON,INTF	; "
	goto	$-1		; "
MeaPul0	btfsc	PORTA,CS_PIN	;Pulses are negative, so wait until composite
	goto	MeaPul1		; sync goes high or LONGMIN * 5 cycles elapse
	decfsz	X0,F		; "
	goto	MeaPul0		; "
MeaPul1	movf	X0,W		;Return with Z set for a long pulse, clear for
	return			; a short one

;Wait for an edge on INT, get the value of Timer0, update the time base in X0,
; and return with the delta between Timer0 and the last time base.
;Clobbers: C, DC, Z
NextEdgeDelta
	IHCALL	PollTimer0	;Poll Timer0 until an edge on composite sync,
	subwf	X0,W		; returning its value at interrupt time in W,
	sublw	0		; update the time base in X0 and leave the delta
	addwf	X0,F		; in W
	XRETURN			;Done

PollTimer0
	movf	TMR0,W		;Continually poll Timer0 until an interrupt
	movf	TMR0,W		; breaks us out of the loop
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	movf	TMR0,W		; "
	goto	PollTimer0	;Loop

ClearTimer2
	movlw	1		;Continually clear Timer2 until an interrupt
ClrTm20	movwf	TMR2		; breaks us out of the loop
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	movwf	TMR2		; "
	goto	ClrTm20		;Loop


;;; End of Program ;;;

	end
