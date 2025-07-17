;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  TashSync: Video Sync Signal Converter/Generator for "Toby" NuBus Card
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
PSGUESS	equ	6	;There are at most this many serrated pulses after vsync

;Pin Assignments
VS_PIN	equ	RA0	;Vertical sync output pin
HS_PIN	equ	RA1	;Horizontal sync output pin
CS_PIN	equ	RA2	;Composite sync input pin

;FLAGS:
PREPOST	equ	7	;State flag for vertical sync area
SERRATE	equ	6	;Set if there are serrated pulses


;;; Variable Storage ;;;

	cblock	0x40	;Bank 0 registers
	
	FLAGS	;You've got to have flags
	SERCNT	;Number of serrated pulses before/after vertical sync pulse
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
	call	MeasurePulse	;Look for the long (vertical) pulse
	btfss	STATUS,Z	; "
	goto	Main		; "
	movlw	(PSGUESS+1)*2	;Skip past PSGUESS potentially-serrated pulses,
	movwf	X1		; using the Timer0 time on the next-to-last one
Main2	XCALL	NextEdgeDelta	; to establish a time base and the following one
	decfsz	X1,F		; to measure the inter-pulse period
	goto	Main2		; "
	movwf	PR2		;Store it so it can be used by Timer2 and PWM1,
	decf	PR2,F		; decremented by one (because period is PR2 + 1)
	btfss	PORTA,CS_PIN	;Wait until composite sync is inactive (high)
	goto	$-1		; "
	bsf	OPTION_REG,INTEDG;Toggle INTEDG so we capture secondary edges
	XCALL	NextEdgeDelta	;Wait for the next edge and update the time base
	subwf	PR2,W		;Subtract the period from the delta to get the
	sublw	-2		; pulse width and store that as the duty cycle
	movwf	PWM1DCH		; to be used by the PWM module
	bcf	OPTION_REG,INTEDG;Toggle INTEDG so we capture primary edges
Main3	call	MeasurePulse	;Wait for the long (vertical) pulse
	btfss	STATUS,Z	; "
	goto	Main3		; "
	clrf	TSKIPH		;Start (negated) horizontal pulse count off at
	clrf	TSKIPL		; zero
	call	MeasurePulse	;Get what we hope is a short (horizontal) pulse,
	btfsc	STATUS,Z	; freezing if it's a vertical pulse instead
	goto	$		; "
Main4	movf	TSKIPL,F	;Decrement the (negated) horizontal pulse count
	btfsc	STATUS,Z	; to count the horizontal pulse
	decf	TSKIPH,F	; "
	decf	TSKIPL,F	; "
	call	MeasurePulse	;Wait for a long (vertical) pulse, counting
	btfss	STATUS,Z	; horizontal pulses backwards while we wait
	goto	Main4		; "
	clrf	SERCNT		;Start the serrated pulse count off at zero
	bcf	STATUS,C	;Calculate 0.75 the inter-pulse period to use
	rrf	PR2,W		; as a threshold to discriminate between pulses
	addwf	PR2,W		; and pulse serrations
	movwf	X1		; "
	bcf	STATUS,C	; "
	rrf	X1,F		; "
Main5	XCALL	NextEdgeDelta	;Establish a time base from the next edge
	XCALL	NextEdgeDelta	;Get the distance to the next pulse
	subwf	X1,W		;Compare to the threshold; if it is above (i.e.
	btfss	STATUS,C	; a full pulse instead of a serration), skip
	goto	Main6		; ahead
	incf	SERCNT,F	;Count a serrated pulse
	bsf	FLAGS,SERRATE	;Flag that there are serrated pulses
	movlw	4		;Shorten the number of edges to skip by 4 (two
	addwf	TSKIPL,F	; per serrated pulse, one serrated pulse per
	btfsc	STATUS,C	; side)
	incf	TSKIPH,F	; "
	goto	Main5		;Loop to count more
Main6	btfsc	FLAGS,SERRATE	;If there are no serrated pulses, skip one fewer
	goto	Main7		; pulse because we need something to time the
	incf	TSKIPL,F	; PWM off of
	btfsc	STATUS,Z	; "
	incf	TSKIPH,F	; "
Main7	movf	TSKIPH,W	;Set up an up counter of horizontal pulses,
	movwf	X2		; skipping two fewer because we counted two
	movf	TSKIPL,W	; extra in the course of looking for serrated
	addlw	2		; pulses
	movwf	X1		; "
	btfsc	STATUS,C	; "
	incf	X2,F		; "
	;fall through

Loop
	movlw	1 << INTE	;Count and sleep us past all the ordinary
	movwf	INTCON		; horizontal edges until we're just before the
	sleep			; first serrated one (if there are any)
	incfsz	X1,F		; "
	goto	Loop		; "
	incfsz	X2,F		; "
	goto	Loop		; "
	btfss	PORTA,CS_PIN	;This is a somewhat poor check for signal change
	goto	SignalChange	; but it's the best I think we can do
	movf	SERCNT,W	;Load a down counter with the count of serrated
	movwf	X1		; pulses
	btfss	STATUS,Z	;If there are no serrated pulses, skip ahead to
	goto	Loop0		; time the PWM off of the one before the
	incf	X1,F		; vertical pulse
	goto	Loop2		; "
Loop0	bcf	STATUS,C	;Else, double the down counter
	rlf	X1,F		; "
Loop1	btfsc	X1,0		;If the down counter is odd (we're about to hit
	goto	Loop3		; a pulse we should swallow), skip ahead
Loop2	decf	PR2,W		;Force PWM output to active (low)
	movwf	TMR2		; "
	movf	PR2,W		; "
	addlw	-6		; "
	movwf	TMR2		; "
	IHCALL	ClearTimer2	;Repeatedly clear Timer2 until primary edge
	btfss	FLAGS,SERRATE	;If there are no serrated pulses, skip ahead to
	goto	Loop4		; switch the output to PWM1
	goto	Loop5		;Skip ahead
Loop3	clrf	INTCON		;This is a pulse we should swallow, so don't
	btfss	INTCON,INTF	; time the PWM based on it, just wait for its
	goto	$-1		; primary edge
	decfsz	X1,W		;If this is the last edge before the vertical
	goto	Loop5		; pulse, switch the output to be PWM1
Loop4	movlw	B'00001000'	; "
	movwf	CLC1GLS1	; "
Loop5	btfss	PORTA,CS_PIN	;Whatever kind of pulse it is, wait until it's
	goto	$-1		; passed
Loop6	movlw	1 << LC1OE	;Toggle the output enable (stick horizontal sync
	btfsc	FLAGS,SERRATE	; high if it's off), unless there are no
	xorwf	CLC1CON,F	; serrated pulses
	decfsz	X1,F		;Repeat for each edge
	goto	Loop1		; "
	clrf	INTCON		;We've reached the vertical pulse, almost; wait
	btfss	INTCON,INTF	; for it to begin
	goto	$-1		; "
	btfss	FLAGS,PREPOST	;If this is the first time we're going through
	bcf	LATA,VS_PIN	; this loop, set vertical sync low
	bsf	OPTION_REG,INTEDG;Toggle INTEDG so we capture secondary edges
	clrf	INTCON		;Wait for the vertical pulse to finish
	btfss	INTCON,INTF	; "
	goto	$-1		; "
	btfss	FLAGS,PREPOST	;If this is the first time we're going through
	bsf	LATA,VS_PIN	; this loop, set vertical sync high
	bcf	OPTION_REG,INTEDG;Toggle INTEDG so we capture primary edges
	btfss	PWM1CON,PWM1OUT	;Wait for the last synthesized horizontal sync
	goto	$-1		; pulse to finish
	movlw	B'00000010'	;Switch output back to reflecting composite sync
	movwf	CLC1GLS1	; "
	btfsc	FLAGS,PREPOST	;If this is the second time we've gone through
	goto	Loop7		; this loop, skip ahead
	movf	SERCNT,W	;If not, load the counter for the post-vertical
	movwf	X1		; serrated pulses
	btfsc	STATUS,Z	;If there are none, skip ahead
	goto	Loop7		; "
	bcf	STATUS,C	;Double the counter as before
	rlf	X1,F		; "
	bsf	FLAGS,PREPOST	;Flag that this is our second trip through the
	goto	Loop6		; loop and loop
Loop7	bcf	FLAGS,PREPOST	;Clear the flag for next time
	movf	TSKIPH,W	;Set up the up counter for skipping through the
	movwf	X2		; horizontal edges
	movf	TSKIPL,W	; "
	movwf	X1		; "
	goto	Loop		;Loop

SignalChange
	movlw	B'00000001'	;We have no reset instruction on this PIC so set
	movwf	WDTCON		; the WDT to its shortest interval and loop
	goto	$		; until it resets us


;;; Subprograms ;;;

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
	goto	ClrTm20		;Loop


;;; End of Program ;;;

	end
