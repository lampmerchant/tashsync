;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  TashSync: Macintosh Video Sync Signal Converter/Generator
;;;
;


;;; Connections ;;;

;;;                                                                        ;;;
;                                   .--------.                               ;
;                           Supply -|01 \/ 08|- Ground                       ;
;      Horizontal Sync Out <-- RA5 -|02    07|- RA0 <-- Horizontal Sync In   ;
;   UART Tx/Pushbutton/LED <-> RA4 -|03    06|- RA1 <-- Composite Sync In    ;
;         Vertical Sync In --> RA3 -|04    05|- RA2 --> Vertical Sync Out    ;
;                                   '--------'                               ;
;;;                                                                        ;;;


;;; Assembler Directives ;;;

	list		P=PIC12F1501, F=INHX32, ST=OFF, MM=OFF, R=DEC, X=ON
	#include	P12F1501.inc
	errorlevel	-302	;Suppress "register not in bank 0" messages
	errorlevel	-224	;Suppress TRIS instruction not recommended msgs
	__config	_CONFIG1, _FOSC_INTOSC & _WDTE_SWDTEN & _PWRTE_ON & _MCLRE_OFF & _CP_OFF & _BOREN_OFF & _CLKOUTEN_OFF
			;_FOSC_INTOSC	Internal oscillator, I/O on RA5
			;_WDTE_SWDTEN	Watchdog timer CONTROLLED BY FIRMWARE
			;_PWRTE_ON	Keep in reset for 64 ms on start
			;_MCLRE_OFF	RA3/!MCLR is RA3
			;_CP_OFF	Code protection off
			;_BOREN_OFF	Brownout reset off
			;_CLKOUTEN_OFF	CLKOUT disabled, I/O on RA4
	__config	_CONFIG2, _WRT_OFF & _STVREN_ON & _BORV_LO & _LPBOR_OFF &_LVP_OFF
			;_WRT_OFF	Write protection off
			;_STVREN_ON	Stack over/underflow causes reset
			;_BORV_LO	Brownout reset voltage low trip point
			;_LPBOR_OFF	Low power brownout reset disabled
			;_LVP_OFF	High-voltage on Vpp to program


;;; Macros ;;;

DELAY	macro	value		;Delay 3*W cycles, set W to 0
	movlw	value
	decfsz	WREG,F
	bra	$-1
	endm

DNOP	macro
	bra	$+1
	endm


;;; Constants ;;;

UARTBT	equ	208	;Bit time for pseudo-UART (cycles)
VERTTHR	equ	8	;A pulse with more downtime than this (us) is vertical
SPULSES	equ	8	;Maximum number of serrated pulses we expect to see
POSTVRT	equ	32	;Delay (cycles) from end of vert. to first horiz. pulse
HPWIDTH	equ	3	;Width (us) of synthesized pulses across vertical pulse

;Pin Assignments
TX_PIN	equ	RA4	;UART Tx pin
VS_PIN	equ	RA3	;Vertical sync input pin
CS_PIN	equ	RA1	;Composite sync input pin
HS_PIN	equ	RA0	;Horizontal sync input pin

;FLAGS:
VISFLAT	equ	7	;Set if V pulse on C is long (no H pulses)
VATEANH	equ	6	;Set if the last V pulse eats the first H pulse after


;;; Variable Storage ;;;

	cblock	0x70	;Bank-common registers
	
	FLAGS	;You've got to have flags
	VPULSES	;Number of vertical sync pulses in a single frame
	HTCYCLS	;Horizontal pulse period in cycles
	HTMICRO	;Horizontal pulse period in microseconds
	HPULSEH	;Number of horizontal sync pulses on composite sync in a single
	HPULSEL	; frame
	SERPRE	;Count of serrated periods before vertical sync
	SERPOST	;Count of serrated periods after vertical sync
	VSYNTH	;Number of pulses that should be synthed across vertical sync
	T1SKPH	;Value to load into Timer1 to sleep through horizontal pulses
	T1SKPL	; "
	X4	;Various purposes
	X3	; "
	X2	; "
	X1	; "
	X0	; "
	
	endc


;;; Vectors ;;;

	org	0x000		;Reset vector
	
	movlb	1		;16 MHz high-freq internal oscillator
	movlw	B'01111000'
	movwf	OSCCON
	goto	Init


	org	0x004		;Interrupt vector
	
	;fall through


;;; Interrupt Handler ;;;

Interrupt
	clrf	INTCON		;Prevent immediate reentry of interrupt handler
	movlb	31		;Return to caller's caller
	decf	STKPTR,F	; "
	retfie			; "


;;; Hardware Initialization ;;;

Init
	banksel	WDTCON		;Watchdog timer resets processor after about
	movlw	B'00010111'	; two seconds if not cleared (even in sleep)
	movwf	WDTCON

	banksel	IOCAN		;By default, IOC catches negative edges on
	movlw	1 << CS_PIN	; composite sync
	movwf	IOCAN

	banksel	OPTION_REG	;Weak pullups enabled
	movlw	B'01111111'
	movwf	OPTION_REG

	banksel	WPUA		;Weak pullup enabled on UART Tx/pushbutton only
	movlw	1 << TX_PIN
	movwf	WPUA

	banksel	ANSELA		;All pins digital, not analog
	clrf	ANSELA

	banksel	LATA		;Drive all pins high by default when outputs
	movlw	B'00101111'	; except for UART/pushbutton/LED pin
	movwf	LATA

	banksel	TRISA		;UART/pushbutton/LED input (so we can use weak
	movlw	B'00011011'	; pullup to drive it), sync inputs inputs, sync
	movwf	TRISA		; outputs outputs

	banksel	PIE1		;Make it so INTCON.PEIE means Timer1, we don't
	movlw	1 << TMR1IF	; need any other peripheral interrupts
	movwf	PIE1

	movlw	high IOCAF	;Point FSR1 permanently to IOCAF so it can be
	movwf	FSR1H		; cleared without switching banks
	movlw	low IOCAF
	movwf	FSR1L

	clrf	FLAGS		;Initialize key globals

	;fall through


;;; Mainline ;;;

;Handle the reset cause and output an appropriate message.
ResetCause
	movlb	1		;All non-specifically handled resets should be
	movlw	low SErrorR	; considered errors
	btfss	PCON,NOT_RWDT	;If a WDT reset occurred, let the user know
	movlw	low SWDTR	; "
	btfss	PCON,NOT_POR	;If a power-on reset occurred, say hello
	movlw	low SPOR	; "
	btfss	PCON,NOT_RI	;If a reset-command reset occurred, say it was
	movlw	low SReset	; because of a sync change
	call	UartTxString	;Transmit whatever the appropriate string is
	movlb	1		;Reset reset causes
	movlw	B'00011111'	; "
	movwf	PCON		; "
	movlb	0		;Wait for 16 ms and see if we detect any edges
	clrf	TMR1H		; on composite sync; if we don't, pass through
	clrf	TMR1L		; horizontal and vertical sync
	movlw	B'00000001'	; "
	movwf	T1CON		; "
	clrf	PIR1		; "
	clrf	INDF1		; "
	btfss	PIR1,TMR1IF	; "
	bra	$-1		; "
	movlw	low SPassTh	; "
	btfss	INTCON,IOCIF	; "
	call	UartTxString	; "
	btfss	INTCON,IOCIF	; "
	bra	Passthrough	; "
	call	VerticalTrace	;If we saw composite sync, do a trace of it
	call	SynthesisPrep	;Do synthesis prep
	call	SynthesisReport	;Output a report on synthesis prep
	call	SetupPwm	;Set up PWM
	call	FindStart	;Find the start of the vertical sync area
	btfsc	FLAGS,VISFLAT	;Do either PWM or passthrough synthesis,
	call	PwmSynthesis	; depending on whether vertical sync is flat or
	btfss	FLAGS,VISFLAT	; not
	call	PassSynthesis	; "
	bra	$		;We shouldn't get here

;Pass through horizontal and vertical sync until reset.
Passthrough
	movlb	30		;Set CLC1 to feed vertical sync through from
	clrf	CLC1SEL0	; CLC1IN0/RA3 to CLC1/RA2 unaltered and set
	clrf	CLC1SEL1	; CLC2 to feed horizontal sync through from
	movlw	B'00000001'	; CLC2IN1/RA0 to CLC2/RA5 unaltered, and set
	movwf	CLC2SEL0	; the CLCs to interrupt on either edge
	clrf	CLC2SEL1	; "
	movlw	B'00000010'	; "
	movwf	CLC1GLS0	; "
	movwf	CLC1GLS1	; "
	movwf	CLC1GLS2	; "
	movwf	CLC1GLS3	; "
	movwf	CLC2GLS0	; "
	movwf	CLC2GLS1	; "
	movwf	CLC2GLS2	; "
	movwf	CLC2GLS3	; "
	clrf	CLC1POL		; "
	clrf	CLC2POL		; "
	movlw	B'11011000'	; "
	movwf	CLC1CON		; "
	movwf	CLC2CON		; "
	movlb	1		;Enable CLC1 interrupt
	movlw	1 << CLC1IE	; "
	movwf	PIE3		; "
	movlw	1 << PEIE	; "
	movwf	INTCON		; "
	movlb	0		; "
Passth0	clrf	PIR3		;Clear the interrupt
	sleep			;Back to sleep
	bra	Passth0		;Awakened by interrupt, WDT cleared, loop

;Make up and output a tiny little trace of the vertical sync area.
VerticalTrace
	movlw	low STrace	;Output trace header
	call	UartTxString	; "
	movlw	0x20		;Fill the 48-byte SRAM buffer with 0xFF and put
	movwf	FSR0H		; FSR0 at the beginning of it
	movlw	0x30		; "
	movwf	FSR0L		; "
	movlw	0xFF		; "
VerTra0	movwi	--FSR0		; "
	movf	FSR0L,F		; "
	btfss	STATUS,Z	; "
	bra	VerTra0		; "
	clrf	X1		;Clear count of pulses
	clrf	X0		; "
VerTra1	call	MeasurePulse	;Measure pulses until we find a horizontal one
	btfsc	STATUS,Z	; (in case we happen to be in vertical sync
	bra	VerTra1		; right now)
VerTra2	call	MeasurePulse	;Measure pulses until we find a vertical one
	btfss	STATUS,Z	; "
	bra	VerTra2		; "
VerTra3	incf	X0,F		;Count vertical pulses until we find a
	btfsc	STATUS,Z	; horizontal one
	incf	X1,F		; "
	call	MeasurePulse	; "
	btfsc	STATUS,Z	; "
	bra	VerTra3		; "
VerTra4	incf	X0,F		;Count horizontal pulses until we find a
	btfsc	STATUS,Z	; vertical one, indicating we've counted all
	incf	X1,F		; pulses
	call	MeasurePulse	; "
	btfss	STATUS,Z	; "
	bra	VerTra4		; "
	movlw	12		;Subtract half the size of the buffer (12 since
	subwf	X0,F		; each item in the buffer is a pair of bytes)
	movlw	0		; "
	subwfb	X1,F		; "
	incf	X1,F		;Increment X1 so decfsz works right
VerTra5	call	MeasurePulse	;Count off pulses until we get to a point where
	decfsz	X0,F		; 24 samples (of both up and down time) will
	bra	VerTra5		; about cover all the interesting stuff in the
	decfsz	X1,F		; vertical sync area
	bra	VerTra5		; "
	movlw	0		;Buffer values start from zero
	btfsc	PORTA,CS_PIN	;Wait for composite sync to go low
	bra	$-1		; "
	bra	VerTra7		;Jump into middle of loop
VerTra6	addlw	1		;Increment W for every 1 us that composite sync
	btfsc	PORTA,CS_PIN	; stays high
	bra	VerTra6		; "
	movwi	FSR0++		;Write the value of W to the buffer
	incfsz	INDF0,F		;If the value at this point in the buffer is 0,
	bra	VerTra8		; we've gone past the end, so break out
VerTra7	addlw	1		;Increment W for every 1 us that composite sync
	btfss	PORTA,CS_PIN	; stays low
	bra	VerTra7		; "
	movwi	FSR0++		;Write the value of W to the buffer
	bra	VerTra6		;Loop
VerTra8	clrf	FSR0L		;Adjust recorded measurements so they're based
	movlw	0		; on zero rather than the previous measurement
VerTra9	subwf	INDF0,F		; "
	addwf	INDF0,W		; "
	incf	FSR0L,F		; "
	btfss	FSR0L,6		; "
	bra	VerTra9		; "
	clrf	FSR0L		;Rewind FSR0
VerTr10	moviw	FSR0++		;Transmit next byte
	call	UartTxHex	; "
	call	UartTxSpace	;Transmit a space
	movf	FSR0L,W		;If we're at a 16-byte boundary, transmit a
	andlw	B'00001111'	; newline
	btfss	STATUS,Z	; "
	bra	VerTr11		; "
	movlw	0x0D		; "
	call	UartTx		; "
	movlw	0x0A		; "
	call	UartTx		; "
VerTr11	movf	FSR0L,W		;If we've written out the whole buffer, done,
	xorlw	0x30		; else loop
	btfss	STATUS,Z	; "
	bra	VerTr10		; "
	return			; "

;Common code for all synthesis strategies.
;Error codes returned in W:
; 0 - Success
; 1 - Too few horizontal pulses
; 2 - Uneven serrated pulses
; 3 - Non-serrated pulses between serrated pulses and vertical sync
; 4 - Unexpected horizontal pulse
; 5 - Single vertical pulse that doesn't eat first horizontal pulse
SynthesisPrep
	movlb	30		;Set CLC2 to be a transparent latch with S and
	movlw	B'00110000'	; R, initially feeding through composite sync,
	movwf	CLC2SEL0	; input sources as follows:
	movlw	B'00100000'	; 1 - CLCIN0/RA1 (Composite Sync In)
	movwf	CLC2SEL1	; 2 - T2_match
	clrf	CLC2GLS0	; 3 - LC1_out (Vertical Sync Out)
	movlw	B'00000010'	; 4 - PWM1_out
	movwf	CLC2GLS1	; "
	clrf	CLC2GLS2	; "
	clrf	CLC2GLS3	; "
	movlw	B'00000001'	; "
	movwf	CLC2POL		; "
	movlw	B'11000111'	; "
	movwf	CLC2CON		; "
	movlw	B'01010000'	;Set CLC1 to be a DFF with S and R, initially
	movwf	CLC1SEL0	; set, input sources as follows:
	clrf	CLC1SEL1	; 2 - LC2_out (Horizontal Sync Out)
	clrf	CLC1GLS0	; "
	clrf	CLC1GLS1	; "
	clrf	CLC1GLS2	; "
	clrf	CLC1GLS3	; "
	movlw	B'00001010'	; "
	movwf	CLC1POL		; "
	movlw	B'11000100'	; "
	movwf	CLC1CON		; "
	bcf	CLC1POL,3	; "
Synthe0	call	MeasurePulse	;Measure pulses until we find a horizontal one
	btfsc	STATUS,Z	; (in case we happen to be in vertical sync
	bra	Synthe0		; right now)
Synthe1	call	MeasurePulse	;Measure pulses until we find a vertical one
	btfss	STATUS,Z	; "
	bra	Synthe1		; "
	clrf	VPULSES		;Clear count of vertical pulses
Synthe2	incf	VPULSES,F	;Count vertical pulses including the one we
	call	MeasurePulse	; just found
	btfsc	STATUS,Z	; "
	bra	Synthe2		; "
	movlw	(SPULSES*2)-1	;Use horizontal pulse count register to count
	movwf	HPULSEL		; down potential serrated pulses
Synthe3	call	MeasurePulse	;Count a horizontal pulse and error if for some
	btfsc	STATUS,Z	; reason it's a vertical pulse
	retlw	1		; "
	decfsz	HPULSEL,F	; "
	bra	Synthe3		; "
	movlb	0		;Start Timer2 counting cycles
	movlw	B'00000100'	; "
	movwf	T2CON		; "
	call	TimeNextPulse	;Wait for the next composite sync pulse and
	movwf	HTCYCLS		; store the Timer2 time when it occurred
	call	TimeNextPulse	;Wait for the next composite pulse and store
	subwf	HTCYCLS,W	; the difference in Timer2 time between it and
	sublw	0		; the previous, this is our horizontal pulse
	movwf	HTCYCLS		; period in cycles
	bsf	T2CON,T2CKPS0	;Start Timer2 counting microseconds
	call	TimeNextPulse	;Wait for the next composite sync pulse and
	movwf	HTMICRO		; store the Timer2 time when it occurred
	call	TimeNextPulse	;Wait for the next composite pulse and store
	subwf	HTMICRO,W	; the difference in Timer2 time between it and
	sublw	0		; the previous, this is our horizontal pulse
	movwf	HTMICRO		; period in microseconds
	clrf	HPULSEH		;Start HPULSEH:L at the number of horizontal
	movlw	(SPULSES*2)+3	; pulses we've seen so far minus one
	movwf	HPULSEL		; "
Synthe4	incf	HPULSEL,F	;Count horizontal pulses including the one we
	btfsc	STATUS,Z	; just found until a vertical one is reached
	incf	HPULSEH,F	; "
	call	MeasurePulse	; "
	btfss	STATUS,Z	; "
	bra	Synthe4		; "
	decfsz	VPULSES,W	;If there's only one vertical pulse, proceed,
	bra	Synth13		; else skip ahead to get us to the last one
Synthe5	clrf	INDF1		;Clear any previous edges
	clrf	T1CON		;Stop Timer1 if it's running
	comf	HPULSEH,W	;Set Timer1 to overflow after HPULSEH:L minus
	movwf	TMR1H		; (SPULSES * 2) + 1 rising edges once activated
	comf	HPULSEL,W	; (the extra plus-one is because we're two's
	addlw	(SPULSES*2)+2	; complementing HPULSEH:L)
	movwf	TMR1L		; "
	btfsc	STATUS,C	; "
	incf	TMR1H,F		; "
	bsf	FLAGS,VATEANH	;Set VATEANH flag, we'll clear it if necessary
	btfss	PORTA,CS_PIN	;Wait until the vertical pulse goes high
	bra	$-1		; "
	movlw	POSTVRT/3	;Delay for POSTVRT cycles while we wait to see
	decfsz	WREG,W		; if there is a falling edge on composite sync
	bra	$-1		; "
	btfss	INDF1,CS_PIN	;If there was a falling edge during the delay,
	bra	Synthe6		; that was the first horizontal pulse, so
	bcf	FLAGS,VATEANH	; shorten the interval by one pulse, also clear
	incf	TMR1L,F		; the VATEANH flag; if there was no falling
	btfsc	STATUS,Z	; edge, the last vertical pulse ate the first
	incf	TMR1H,F		; horizontal pulse, so leave the flag set
	btfss	PORTA,CS_PIN	;Make sure (if horizontal pulse wasn't eaten)
	bra	$-1		; that composite sync is high
Synthe6	call	SleepUntilTimer1;Sleep until Timer1 counts enough pulses
	lsrf	HTMICRO,W	;Calculate 3/4 of the period between pulses (in
	movwf	X0		; microseconds) to use as a threshold to tell
	lsrf	WREG,W		; whether a pulse is serrated or not
	addwf	X0,F		; "
	clrf	SERPRE		;Zero count of pre-vertical serrated pulses
	movlw	(SPULSES*2)+1	;Start countdown of total pre-vertical pules
	movwf	SERPOST		; "
Synthe7	call	TimeNextPulse	;Get the Timer2 (microsecond) value of the
	movwf	T1SKPH		; first pulse as a time base
	decfsz	SERPOST,F	;Count down the pulse we just saw and, if the
	bra	Synthe8		; next is the first vertical pulse, something
	retlw	2		; is wrong
Synthe8	call	TimeNextPulse	;Get the Timer2 value of the next pulse
	movwf	T1SKPL		;Save it for later use as the new time base
	subwf	T1SKPH,W	;Get the difference betwen this value and the
	sublw	0		; previous time base
	subwf	X0,W		;Compare it to the serration threshold
	movf	T1SKPL,W	;Set the time base to the new time base
	movwf	T1SKPH		; "
	btfsc	STATUS,C	;If the difference is below the threshold, skip
	bra	Synth15		; ahead
	movf	SERPRE,W	;If the difference is above the threshold, make
	btfss	STATUS,Z	; sure we haven't already counted pre-vertical
	retlw	3		; pulses, and if we have, something is wrong
	decfsz	SERPOST,F	;If the difference is above the threshold,
	bra	Synthe8		; count down the remaining pulses and loop
Synthe9	decf	VPULSES,W	;If there is only one vertical pulse, skip
	btfsc	STATUS,Z	; ahead to time it and find out how many pulses
	bra	Synth16		; we need to synthesize
	clrf	VSYNTH		;Don't need to synthesize any vertical pulses
	movf	VPULSES,W	;Skip past the falling edges of the vertical
	movwf	SERPOST		; pulses, the next falling edge is of the first
Synth10	call	TimeNextPulse	; horizontal pulse
	decfsz	SERPOST,F	; "
	bra	Synth10		; "
Synth11	call	TimeNextPulse	;Get the Timer2 (microsecond) value of the
	movwf	T1SKPH		; first pulse as a time base
Synth12	call	TimeNextPulse	;Get the Timer2 value of the next pulse
	movwf	T1SKPL		;Save it for later use as the new time base
	subwf	T1SKPH,W	;Get the difference betwen this value and the
	sublw	0		; previous time base
	subwf	X0,W		;Compare it to the serration threshold
	movf	T1SKPL,W	;Set the time base to the new time base
	movwf	T1SKPH		; "
	btfsc	STATUS,C	;If the difference is below the threshold, skip
	bra	Synth19		; ahead
	movlb	7		;Switch IOC to catch falling edges again in
	movlw	1 << CS_PIN	; case it was switched
	clrf	IOCAP		; "
	movwf	IOCAN		; "
	movlb	0		; "
	comf	HPULSEH,W	;One's complement HPULSEH:L into T1SKPH:L,
	movwf	T1SKPH		; effectively giving us the count-up value for
	comf	HPULSEL,W	; Timer1 plus one extra edge - it'd take us
	movwf	T1SKPL		; into the first vertical pulse as it stands
	lslf	SERPRE,W	;Shorten the count-up value by (2 * SERPRE) +
	addwf	SERPOST,W	; (2 * SERPOST); with that extra edge, this is
	addwf	SERPOST,W	; the correct value if the last vertical pulse
	btfss	FLAGS,VATEANH	; eats the first horizontal pulse; if that is
	addlw	1		; not the case, shorten it by one more
	addwf	T1SKPL,F	; "
	btfsc	STATUS,C	; "
	incf	T1SKPH,F	; "
	movlw	2		;Take the timer value we just calculated and
	addwf	T1SKPL,W	; shave two more edges off of it (since we ate
	movwf	TMR1L		; two more edges to find out where the end of
	movlw	0		; the serrated pulses was) and sleep to get us
	addwfc	T1SKPH,W	; to the beginning of the vertical sync area
	movwf	TMR1H		; "
	call	SleepUntilTimer1; "
	retlw	0		;Success, return
Synth13	movwf	VSYNTH		;There's more than one vertical pulse, so count
Synth14	call	MeasurePulse	;Measure a pulse; if it isn't vertical, reset
	btfss	STATUS,Z	; because something is wrong
	retlw	4		; "
	decfsz	VSYNTH,F	;Loop until we're at the last vertical pulse,
	bra	Synth14		; then rejoin above
	bra	Synthe5		; "
Synth15	incf	SERPRE,F	;Count the pre-vertical serration we just saw
	decfsz	SERPOST,F	;If there are more pre-vertical pulses to look
	bra	Synthe7		; at, loop
	bra	Synthe9		;If not, proceed to deal with vertical pulse(s)
Synth16	btfss	FLAGS,VATEANH	;If the single vertical pulse doesn't eat the
	retlw	5		; first horizontal pulse, we can't handle this
	bsf	FLAGS,VISFLAT	;By definition, vertical pulse is flat
	movlw	B'00000110'	;Set Timer2 to measure in four-microsecond
	movwf	T2CON		; intervals
	clrf	VSYNTH		;Zero the count of synthetic vertical pulses
	lsrf	HTMICRO,W	;Divide horizontal period by four to get our
	lsrf	WREG,W		; divisor
	movwf	T1SKPL		; "
	call	TimeNextPulse	;Get the starting point of the vertical pulse
	movwf	T1SKPH		; "
	movlb	7		;Switch IOC to catch rising edges instead of
	movlw	1 << CS_PIN	; falling edges
	clrf	IOCAN		; "
	movwf	IOCAP		; "
	call	TimeNextPulse	;Get the ending point of the vertical pulse and
	subwf	T1SKPH,F	; calculate the elapsed time (complemented)
	movf	T1SKPL,W	;Retrieve precalculated divisor
Synth17	addwf	T1SKPH,F	;Add divisor to complemented dividend
	btfsc	STATUS,C	;If overflow, we're done, skip back to count
	bra	Synth18		; post-vertical serrations
	incf	VSYNTH,F	;If no overflow, increment quotient and loop
	bra	Synth17		; "
Synth18	clrf	T1CON		;Stop Timer1 if it's running
	comf	HPULSEH,W	;Set Timer1 to overflow after HPULSEH:L rising
	movwf	TMR1H		; edges once activated (the extra plus-one is
	comf	HPULSEL,W	; because we're two's complementing HPULSEH:L);
	addlw	1		; this leaves us in the space before the
	movwf	TMR1L		; vertical pulse and the next rising edge is
	btfsc	STATUS,C	; that of the vertical pulse (effectively the
	incf	TMR1H,F		; rising edge of the first horizontal pulse)
	call	SleepUntilTimer1;Sleep until Timer1 overflows
	movlw	B'00000101'	;Set Timer2 to count microseconds again
	movwf	T2CON		; "
	bra	Synth11		;Rejoin above
Synth19	incf	SERPOST,F	;Count the post-vertical serration we just saw
	bra	Synth11		;Loop

;Output a report of the synthesis prep.
SynthesisReport
	movlw	low SRpt0	;Output vertical pulse count
	call	UartTxString	; "
	movf	VPULSES,W	; "
	call	UartTxDecShort	; "
	movlw	low SRpt1	;Output horizontal pulse count
	call	UartTxString	; "
	movf	HPULSEH,W	; "
	movwf	X1		; "
	movf	HPULSEL,W	; "
	movwf	X0		; "
	call	UartTxDecimal	; "
	movlw	low SRpt2	;Output horizontal period (in cycles and
	call	UartTxString	; microseconds if under 64 microseconds, just
	btfss	HTMICRO,7	; microseconds if not)
	btfsc	HTMICRO,6	; "
	bra	SynRpt0		; "
	movf	HTCYCLS,W	; "
	call	UartTxDecShort	; "
	movlw	low SRpt3	; "
	call	UartTxString	; "
SynRpt0	movf	HTMICRO,W	; "
	call	UartTxDecShort	; "
	movlw	low SRpt4	;Output pre and post vertical serrated pulses
	call	UartTxString	; if any
	movf	SERPRE,W	; "
	addwf	SERPOST,W	; "
	btfsc	STATUS,Z	; "
	bra	SynRpt1		; "
	movlw	low SRpt5	; "
	call	UartTxString	; "
	movf	SERPRE,W	; "
	call	UartTxDecShort	; "
	movlw	low SRpt6	; "
	call	UartTxString	; "
	movf	SERPOST,W	; "
	call	UartTxDecShort	; "
SynRpt1	btfss	FLAGS,VISFLAT	;If vertical pulse is flat, output count of
	bra	SynRpt2		; synthesized pulses
	movlw	low SRpt7	; "
	call	UartTxString	; "
	movf	VSYNTH,W	; "
	call	UartTxDecShort	; "
SynRpt2	movlw	low SCRLF	; "
	call	UartTxString	; "
	return			;Done

;Set up PWM1 (and thus Timer2) to mimic the horizontal sync pulse frequency.
SetupPwm
	movlb	0		;Start Timer2 ticking off cycles at first
	movlw	B'00000100'	; "
	movwf	T2CON		; "
	btfss	HTMICRO,7	;If the horizontal period in cycles is over
	btfsc	HTMICRO,6	; 255 (64 us or greater, as with the IIgs), set
	bsf	T2CON,T2CKPS0	; Timer2 to use a 1:4 prescaler
	decf	HTCYCLS,W	;Set the period of Timer2 according to whether
	btfsc	T2CON,T2CKPS0	; the 1:4 prescaler is being used or not
	decf	HTMICRO,W	; "
	movwf	PR2		; "
	movlw	HPWIDTH*4	;Set the pulse width according to the HPWIDTH
	btfsc	T2CON,T2CKPS0	; constant and whether the 1:4 prescaler is
	movlw	HPWIDTH		; being used or not
	movlb	12		; "
	movwf	PWM1DCH		; "
	clrf	PWM1DCL		; "
	movlw	B'10010000'	;Turn PWM on, set output to be active-low
	movwf	PWM1CON		; "
	movlb	0		;Make sure PWM registers get loaded by forcing
	decf	PR2,W		; Timer2 to roll over
	movwf	TMR2		; "
	return			;Done

;Find the start of the vertical sync area.
FindStart
	movlb	0		;Set up Timer1 to skip past all the edges that
	clrf	T1CON		; we'll see between just-past-the-beginning-of-
	lslf	SERPOST,W	; first-vertical-pulse and the beginning of the
	addwf	VPULSES,W	; vertical sync area
	btfsc	FLAGS,VATEANH	; "
	addlw	-1		; "
	subwf	T1SKPL,W	; "
	movwf	TMR1L		; "
	movlw	0		; "
	subwfb	T1SKPH,W	; "
	movwf	TMR1H		; "
FindSt0	call	MeasurePulse	;Measure pulses until we find a horizontal one
	btfsc	STATUS,Z	; (in case we happen to be in vertical sync
	bra	FindSt0		; right now)
FindSt1	call	MeasurePulse	;Measure pulses until we find a vertical one
	btfss	STATUS,Z	; "
	bra	FindSt1		; "
	goto	SleepUntilTimer1;Arm Timer1 and sleep until it interrupts

;PWM synthesis for when VISFLAT is set.
PwmSynthesis
	movf	SERPRE,W	;Initialize a counter of pre-vertical serrated
	btfsc	STATUS,Z	; pulses or skip ahead if there aren't any
	bra	PwmSyn1		; "
	movwf	X0		; "
PwmSyn0	clrf	INDF1		;Wait for a falling edge on composite sync
	btfss	INTCON,IOCIF	; "
	bra	$-1		; "
	call	ThreeFourths	;Timer2 to match after 3/4 normal pulse width
	movlb	30		;Latch high state of composite sync to
	bcf	CLC2POL,0	; horizontal sync
	clrf	INDF1		;Clear falling edge and Timer2 flags
	movlb	0		; "
	clrf	PIR1		; "
	btfss	INTCON,IOCIF	;Wait for the serration that we want to eat
	bra	$-1		; "
	btfsc	PIR1,TMR2IF	;If Timer2 matched while we were waiting, the
	reset			; signal has changed, so reset
	movlb	30		;Pass composite sync through to horizontal sync
	bsf	CLC2POL,0	; again
	decfsz	X0,F		;Count down serrated pulses and loop if there
	bra	PwmSyn0		; are more
PwmSyn1	movlb	30		;Gate PWM1 into horizontal sync output (make it
	bsf	CLC2GLS1,7	; low if both PWM1 and composite sync are low)
	bcf	CLC1GLS0,2	;Disconnect horizontal sync from the vert clock
	bsf	CLC1GLS2,2	;Make vertical sync go low when horizontal does
	call	HoldTimer2	;Hold Timer2 until falling edge on composite
	clrf	INDF1		;Clear the falling edge
	movlb	30		;Disconnect the reset input of vertical sync
	bcf	CLC1GLS2,2	; from the complement of horizontal sync
	movlb	0		; "
	movf	VSYNTH,W	;Initialize a counter of pulses to synthesize
	movwf	X0		; inside the flat vertical pulse
PwmSyn2	decf	X0,W		;If this about to be the end of the vertical
	btfss	STATUS,Z	; pulse, set vertical sync to go high on a
	bra	PwmSyn3		; falling edge of horizontal
	movlb	30		; "
	btfss	CLC2CON,LC2OUT	; "
	bra	$-1		; "
	bsf	CLC1GLS0,2	; "
	movlb	0		; "
PwmSyn3	clrf	PIR1		;Wait for Timer2 to match that many times
	btfss	PIR1,TMR2IF	; "
	bra	$-1		; "
	decfsz	X0,F		; "
	bra	PwmSyn2		; "
	call	ThreeFourths	;Timer2 to match after 3/4 normal pulse width
	btfsc	INTCON,IOCIF	;If there was a falling edge on composite sync
	reset			; during vertical pulse, signal has changed
	movlb	30		;Take PWM1 out of gate for horizontal sync
	bcf	CLC2GLS1,7	; "
	movf	SERPOST,W	;Initialize a counter of post-vertical serrated
	btfsc	STATUS,Z	; pulses or skip ahead if there aren't any
	bra	PwmSyn6		; "
	movwf	X0		; "
	bra	PwmSyn5		;Jump into loop since first H pulse was eaten
PwmSyn4	clrf	INDF1		;Wait for a falling edge on composite sync
	btfss	INTCON,IOCIF	; "
	bra	$-1		; "
	call	ThreeFourths	;Timer2 to match after 3/4 normal pulse width
	movlb	30		;Latch high state of composite sync to
PwmSyn5	bcf	CLC2POL,0	; horizontal sync
	clrf	INDF1		;Clear falling edge and Timer2 flags
	movlb	0		; "
	clrf	PIR1		; "
	btfss	INTCON,IOCIF	;Wait for the serration that we want to eat
	bra	$-1		; "
	btfsc	PIR1,TMR2IF	;If Timer2 matched while we were waiting, the
	reset			; signal has changed, so reset
	movlb	30		;Pass composite sync through to horizontal sync
	bsf	CLC2POL,0	; again
	decfsz	X0,F		;Count down serrated pulses and loop if there
	bra	PwmSyn4		; are more
PwmSyn6	movlb	0		;Stop Timer1 if it was running
	clrf	T1CON		; "
	movf	T1SKPH,W	;Set up Timer1 to skip enough rising edges on
	movwf	TMR1H		; composite sync that it takes us to the next
	movf	T1SKPL,W	; vertical sync area
	movwf	TMR1L		; "
	call	SleepUntilTimer1;Sleep until that happens
	bra	PwmSynthesis	;Loop

;Passthrough synthesis for when VISFLAT is clear.
PassSynthesis
	movf	SERPRE,W	;Initialize a counter of pre-vertical serrated
	btfsc	STATUS,Z	; pulses or skip ahead if there aren't any
	bra	PassSy1		; "
	movwf	X0		; "
PassSy0	clrf	INDF1		;Wait for a falling edge on composite sync
	btfss	INTCON,IOCIF	; "
	bra	$-1		; "
	call	ThreeFourths	;Timer2 to match after 3/4 normal pulse width
	movlb	30		;Latch high state of composite sync to
	bcf	CLC2POL,0	; horizontal sync
	clrf	INDF1		;Clear falling edge and Timer2 flags
	movlb	0		; "
	clrf	PIR1		; "
	btfss	INTCON,IOCIF	;Wait for the serration that we want to eat
	bra	$-1		; "
	btfsc	PIR1,TMR2IF	;If Timer2 matched while we were waiting, the
	reset			; signal has changed, so reset
	movlb	30		;Pass composite sync through to horizontal sync
	bsf	CLC2POL,0	; again
	decfsz	X0,F		;Count down serrated pulses and loop if there
	bra	PassSy0		; are more
PassSy1	movlb	30		;Gate PWM1 into horizontal sync output (make it
	bsf	CLC2GLS1,7	; low if both PWM1 and composite sync are low)
	bcf	CLC1GLS0,2	;Disconnect horizontal sync from the vert clock
	bsf	CLC1GLS2,2	;Make vertical sync go low when horizontal does
	call	HoldTimer2	;Hold Timer2 until falling edge on composite
	decf	VPULSES,W	;Initialize a counter of pulses to let pass
	movwf	X0		; "
PassSy2	clrf	PIR1		;Wait for Timer2 to match that many times
	btfss	PIR1,TMR2IF	; "
	bra	$-1		; "
	decfsz	X0,F		; "
	bra	PassSy2		; "
	movlb	0		;Stop Timer1 if it was running and set it up to
	clrf	T1CON		; skip enough rising edges on composite sync
	movf	T1SKPH,W	; that it takes us to the next vertical sync
	movwf	TMR1H		; area - we won't have time enough to do this
	movf	T1SKPL,W	; later, so do it now
	movwf	TMR1L		; "
	movlb	30		;Disconnect the reset input of vertical sync
	bcf	CLC1GLS2,2	; from the complement of horizontal sync
	btfss	CLC2CON,LC2OUT	;Wait for PWM to go high
	bra	$-1		; "
	bsf	CLC1GLS0,2	;Make vertical sync go high when horiz goes low
	movlb	0		;If composite sync isn't low at this point, the
	btfsc	PORTA,CS_PIN	; signal has changed, reset
	reset			; "
	movlb	30		;Latch high state of horizontal sync
	bcf	CLC2POL,0	; "
	bcf	CLC2GLS1,7	;Take PWM1 out of gate for horizontal sync
	movlb	0		;Wait for composite sync to go high
	btfss	PORTA,CS_PIN	; "
	bra	$-1		; "
	movlb	30		;Pass composite sync through to horizontal sync
	bsf	CLC2POL,0	; again
	movf	SERPOST,W	;Initialize a counter of post-vertical serrated
	btfsc	STATUS,Z	; pulses or skip ahead if there aren't any
	bra	PassSy5		; "
	movwf	X0		; "
PassSy3	clrf	INDF1		;Wait for a falling edge on composite sync
	btfss	INTCON,IOCIF	; "
	bra	$-1		; "
	call	ThreeFourths	;Timer2 to match after 3/4 normal pulse width
	movlb	30		;Latch high state of composite sync to
PassSy4	bcf	CLC2POL,0	; horizontal sync
	clrf	INDF1		;Clear falling edge and Timer2 flags
	movlb	0		; "
	clrf	PIR1		; "
	btfss	INTCON,IOCIF	;Wait for the serration that we want to eat
	bra	$-1		; "
	btfsc	PIR1,TMR2IF	;If Timer2 matched while we were waiting, the
	reset			; signal has changed, so reset
	movlb	30		;Pass composite sync through to horizontal sync
	bsf	CLC2POL,0	; again
	decfsz	X0,F		;Count down serrated pulses and loop if there
	bra	PassSy3		; are more
PassSy5	call	SleepUntilTimer1;Sleep until next vertical sync area
	bra	PassSynthesis	;Loop


;;; Subprograms ;;;

;Transmit a space over the pseudo UART.
;Clobbers: X0 (0), BSR (0)
UartTxSpace
	movlw	0x20
	;fall through

;Transmit the byte in W over the pseudo UART.
;Clobbers: X0 (0), BSR (0)
UartTx
	movwf	X0		;Save byte to transmit
	movlb	1		;If the Timer0 clock input isn't Fosc/4 and/or
	btfss	OPTION_REG,TMR0CS; the interrupt flag is set, it's been long
	btfsc	INTCON,TMR0IF	; enough since the last byte was sent, so skip
	bra	UartTx0		; ahead
	btfss	INTCON,TMR0IF	;Otherwise, wait for the pending stop bit to be
	bra	$-1		; over
	movlb	0		;Set Timer0 so it overflows after one bit time
	movlw	258-UARTBT	; modulo a bit of overhead, then skip ahead
	movwf	TMR0		; "
	bra	UartTx1		; "
UartTx0	bcf	OPTION_REG,TMR0CS;Set Timer0 clock input to be Fosc/4
	movlb	0		;Set Timer0 so it overflows after one bit time
	movlw	262-UARTBT	; modulo a bit of overhead
	movwf	TMR0		; "
UartTx1	movlb	1		;Pull Tx low for a start bit
	bcf	TRISA,TX_PIN	; "
	bsf	STATUS,C	;Set carry as an end-of-byte sentinel
UartTx2	bcf	INTCON,TMR0IF	;Clear the Timer0 interrupt flag
	rrf	X0,F		;Rotate the next bit to transmit into carry
	movf	TRISA,W		;Copy carry into the next value to write to
	andlw	~(1 << TX_PIN)	; TRISA
	btfsc	STATUS,C	; "
	iorlw	1 << TX_PIN	; "
	btfss	INTCON,TMR0IF	;Wait for Timer0 to overflow
	bra	$-1		; "
	movwf	TRISA		;Set or clear Tx for the next bit
	movlb	0		; "
	movf	X0,F		;If this was the stop bit, skip ahead
	btfsc	STATUS,Z	; "
	bra	UartTx3		; "
	movlw	268-UARTBT	;Set Timer0 so it overflows after one bit time,
	movwf	TMR0		; modulo a bit of overhead
	bcf	STATUS,C	;Clear carry so we shift zeroes into shift
	movlb	1		; register
	bra	UartTx2		;Loop
UartTx3	movlw	276-UARTBT	;Set Timer0 so if this function is called again
	movwf	TMR0		; soon, the appropriate bit time passes before
	bcf	INTCON,TMR0IF	; the next byte is transmitted
	return			; "

;Unpack the ASCII string pointed to by 0x300 + W and write it to the UART.
;Clobbers: X0 (0), BSR (1)
UartTxString
	movlb	3		;Regress the pointer from W by one and store it
	addlw	-1		; in PMADRL
	movwf	PMADRL		; "
	movlw	0x03		;Strings are all in the 0x300 block of flash,
	movwf	PMADRH		; so set PMADRH to 0x03
UaTxSt0	incf	PMADRL,F	;Advance the program memory pointer
	bsf	PMCON1,RD	;Perform a read of a flash memory word
	nop			; "
	nop			; "
	lslf	PMDATL,W	;Get the high seven bits of the word we read
	rlf	PMDATH,W	; "
	movf	WREG,W		;If they're zero, done
	btfsc	STATUS,Z	; "
	return			; "
	call	UartTx		;Print the character
	movlb	3		; "
	movf	PMDATL,W	;Get the low seven bits of the word we read
	andlw	B'01111111'	; into the SRAM buffer
	btfsc	STATUS,Z	;If they're zero, done
	return			; "
	call	UartTx		;Print the character
	movlb	3		; "
	bra	UaTxSt0		;Loop

;Write the contents of W out as a hex number.
;Clobbers: X1, X0 (0), BSR (0)
UartTxHex
	movwf	X1		;Save W
	swapf	WREG,W		;Isolate the high hex numeral
	andlw	B'00001111'	; "
	addlw	0xF6		;Convert it to ASCII
	btfsc	STATUS,C	; "
	addlw	0x07		; "
	addlw	0x3A		; "
	call	UartTx		;Transmit it
	movf	X1,W		;Isolate the low hex numeral
	andlw	B'00001111'	; "
	addlw	0xF6		;Convert it to ASCII
	btfsc	STATUS,C	; "
	addlw	0x07		; "
	addlw	0x3A		; "
	bra	UartTx		;Transmit it

;Convert the number in W into an ASCII and put it to the UART.
UartTxDecShort
	movwf	X0		;Save the caller a bit of space by writing the
	clrf	X1		; number in W to X0 and clearing X1
	;fall through

;Convert the number in X1:0 into an ASCII number and put it to the UART.
;Clobbers: X4:X0, BSR (0)
UartTxDecimal
	movlb	0		;Clear the BCD space
	clrf	X4		; "
	clrf	X3		; "
	clrf	X2		; "
	bsf	STATUS,C	;Use this 1 as an end sentinel
UaTxDe0	rlf	X0,F		;Rotate
	rlf	X1,F		; "
	rlf	X2,F		; "
	rlf	X3,F		; "
	rlf	X4,F		; "
	rlf	X1,W		;Check if we're done dabbling and skip ahead if
	iorwf	X0,W		; we are
	btfsc	STATUS,Z	; "
	bra	UaTxDe1		; "
	movlw	0x33		;Set high bits of each nibble if they're >= 5
	addwf	X2,F		; "
	addwf	X3,F		; "
	btfsc	X2,3		;If the low nibble's high bit was set, adjust
	addlw	-3		; so we don't subtract from it
	btfsc	X2,7		;If the high nibble's high bit was set, adjust
	addlw	(-3)<<4		; so we don't subtract from it
	subwf	X2,F		;Do the subtraction
	movlw	0x33		;Reload 0x33 into W to adjust the middle reg
	btfsc	X3,3		;If the low nibble's high bit was set, adjust
	addlw	-3		; so we don't subtract from it
	btfsc	X3,7		;If the high nibble's high bit was set, adjust
	addlw	(-3)<<4		; so we don't subtract from it
	subwf	X3,F		;Do the subtraction
	bcf	STATUS,C	;Clear carry so we don't rotate another one in
	bra	UaTxDe0		;Loop
UaTxDe1	movf	X4,W		;Convert the packed BCD representation into
	movwf	X0		; unpacked BCD representation (slightly out of
	movf	X2,W		; order: X0, X3, X2, X1, X4; this way we don't
	andlw	0x0F		; mind that UartTx clobbers X0)
	movwf	X4		; "
	swapf	X2,W		; "
	andlw	0x0F		; "
	movwf	X1		; "
	movf	X3,W		; "
	andlw	0x0F		; "
	movwf	X2		; "
	swapf	X3,W		; "
	andlw	0x0F		; "
	movwf	X3		; "
	movf	X0,W		;Skip ahead as necessary so as not to print
	btfss	STATUS,Z	; leading zeroes
	bra	UaTxDe2		; "
	movf	X3,W		; "
	btfss	STATUS,Z	; "
	bra	UaTxDe3		; "
	movf	X2,W		; "
	btfss	STATUS,Z	; "
	bra	UaTxDe4		; "
	movf	X1,W		; "
	btfss	STATUS,Z	; "
	bra	UaTxDe5		; "
	bra	UaTxDe6		; "
UaTxDe2	movf	X0,W		;Convert each unpacked BCD digit into ASCII and
	iorlw	0x30		; put them to the UART
	call	UartTx		; "
UaTxDe3	movf	X3,W		; "
	iorlw	0x30		; "
	call	UartTx		; "
UaTxDe4	movf	X2,W		; "
	iorlw	0x30		; "
	call	UartTx		; "
UaTxDe5	movf	X1,W		; "
	iorlw	0x30		; "
	call	UartTx		; "
UaTxDe6	movf	X4,W		; "
	iorlw	0x30		; "
	call	UartTx		; "
	return			;Done

;Start Timer1 counting pulses on T1CKI and sleep until it overflows.
;Clobbers: BSR (0)
SleepUntilTimer1
	movlb	0		;Arm Timer1 to count up and then wake us from
	movlw	B'10000101'	; sleep (we should be in the period where
	movwf	T1CON		; composite sync is high before a horizontal
	movlw	1 << PEIE	; pulse, (SPULSES * 2) + 1 pulses before
	movwf	INTCON		; vertical)
	clrf	PIR1		; "
	sleep			;Sleep until Timer1 overflows
	return			;Done

;Set Timer2 so it overflows after three fourths its period.
;Clobbers: BSR (0)
ThreeFourths
	movlb	0		;Divide Timer2 period register by four and set
	lsrf	PR2,W		; Timer2 to it
	lsrf	WREG,W		; "
	movwf	TMR2		; "
	return			;Done

;Roughly measure a pulse on composite sync using threshold VERTTHR.  Return
; with Z clear for a short (horizontal) pulse, set for a long (vertical) pulse.
;Clobbers: BSR (0)
MeasurePulse
	clrf	INDF1		;Wait for a falling edge on composite sync
	btfss	INDF1,CS_PIN	; (INDF1 is IOCAF)
	bra	$-1		; "
	movlw	VERTTHR-1	;Wait VERTTHR - 1 microseconds (the delay in
MeasPu0	nop			; triggering on IOCAF and the delay afterwards
	decfsz	WREG,W		; add up to another microsecond
	bra	MeasPu0		; "
	movlb	0		;Grab the state of the composite sync pin and
	movf	PORTA,W		; return with it in W (Z will be clear if the
	andlw	1 << CS_PIN	; pulse was short and set if it was long)
	return			;Done

;Get the value of Timer2 at the time of the start of the next composite sync
; pulse, returning it in W.
;Clobbers: BSR (0)
TimeNextPulse
	clrf	INDF1		;Clear any past edge
	clrf	INTCON		;Enable only IOC interrupt, going to interrupt
	bsf	INTCON,IOCIE	; handler not yet enabled
	movlb	0		;Grab a first value of Timer2 in case edge was
	movf	TMR2,W		; already triggered
	bsf	INTCON,GIE	;Enable interrupt handler
TiNePu0	movf	TMR2,W		;Repeatedly grab the value of Timer2 until the
	movf	TMR2,W		; interrupt handler triggers and returns us to
	movf	TMR2,W		; caller
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	movf	TMR2,W		; "
	;TODO more?
	bra	TiNePu0		;Loop

;Hold Timer2 at zero (and PWM1 active) until the start of the next composite
; sync pulse.
;Clobbers: BSR (0)
HoldTimer2
	movlb	0		;Set Timer2 to its period so when it ticks, the
	movf	PR2,W		; PWM is in active mode
	movwf	TMR2		; "
	clrf	INDF1		;Clear any past edge
	clrf	INTCON		;Enable only IOC interrupt, going to interrupt
	bsf	INTCON,IOCIE	; handler not yet enabled
	bsf	INTCON,GIE	;Enable interrupt handler
	movlw	1		;Repeatedly clear Timer2 until the interrupt
HoldT20	movwf	TMR2		; handler triggers and returns us to caller
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
	;TODO more?
	bra	HoldT20		; "


;;; Strings ;;;

SPOR	da	"Welcome to TashSync 20250624\r\n\x00"
SWDTR	da	"sync timeout\r\n\x00"
SReset	da	"sync change\r\n\x00"
SErrorR	da	"serious error\r\n\x00"
SPassTh	da	"No composite sync detected, using passthrough mode.\r\n\x00"
STrace	da	"Composite sync detected.\r\nTrace:\r\n\x00"
SRpt0	da	"VPulses: \x00"
SRpt1	da	"\r\nHPulses: \x00"
SRpt2	da	"\r\nHPeriod: \x00"
SRpt3	da	" cyc / \x00"
SRpt4	da	" us\x00"
SRpt5	da	"\r\nSerPre: \x00"
SRpt6	da	"\r\nSerPost: \x00"
SRpt7	da	"\r\nVSynth: \x00"
SCRLF	da	"\r\n"
SNull	da	"\x00"


;;; End of Program ;;;

	end
