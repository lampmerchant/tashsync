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
VERTTHR	equ	14	;A pulse with more downtime than this (us) is vertical

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
	HPULSEH	;Number of horizontal sync pulses on composite sync in a single
	HPULSEL	; frame
	HPERIOD	;Horizontal pulse period
	VPULSES	;Number of vertical sync pulses in a single frame
	SERPRE	;Count of serrated periods before vertical sync
	SERPOST	;Count of serrated periods after vertical sync
	VSYNTH	;Number of pulses that should be synthed across vertical sync
	T1SKPH	;Value to load into Timer1 to sleep through horizontal pulses
	T1SKPL	; "
	X5
	X4	
	X3
	X2	;Various purposes
	X1	; "
	X0	; "
	
	endc

	cblock	0x20
	
	PROF00	;Signal profile registers
	PROF01	; "
	PROF02	; "
	PROF03	; "
	PROF04	; "
	PROF05	; "
	PROF06	; "
	PROF07	; "
	PROF08	; "
	PROF09	; "
	PROF0A	; "
	PROF0B	; "
	PROF0C	; "
	PROF0D	; "
	PROF0E	; "
	PROF0F	; "
	PROF10	; "
	PROF11	; "
	PROF12	; "
	PROF13	; "
	PROF14	; "
	PROF15	; "
	PROF16	; "
	PROF17	; "
	PROF18	; "
	PROF19	; "
	PROF1A	; "
	PROF1B	; "
	PROF1C	; "
	PROF1D	; "
	PROF1E	; "
	PROF1F	; "
	PROF20	; "
	PROF21	; "
	PROF22	; "
	PROF23	; "
	PROF24	; "
	PROF25	; "
	PROF26	; "
	PROF27	; "
	PROF28	; "
	PROF29	; "
	PROF2A	; "
	PROF2B	; "
	PROF2C	; "
	PROF2D	; "
	PROF2E	; "
	PROF2F	; "
	
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
	clrf	INTCON		;Interrupt is just to wake us up, so disable
	retfie			; all interrupt sources and return


;;; Hardware Initialization ;;;

Init
	banksel	WDTCON		;Watchdog timer resets processor after about
	movlw	B'00010111'	; two seconds if not cleared (even in sleep)
	movwf	WDTCON

	banksel	WPUA		;Weak pullup enabled on UART Tx/pushbutton only
	movlw	1 << TX_PIN
	movwf	WPUA

	banksel	OPTION_REG	;Weak pullups enabled
	movlw	B'01111111'
	movwf	OPTION_REG

	banksel	ANSELA		;All pins digital, not analog
	clrf	ANSELA

	banksel	LATA		;Drive all pins high by default when outputs
	movlw	B'00101111'	; except for UART/pushbutton/LED pin
	movwf	LATA

	banksel	TRISA		;UART/pushbutton/LED input (so we can use weak
	movlw	B'00011011'	; pullup to drive it), sync inputs inputs, sync
	movwf	TRISA		; outputs outputs

	clrf	FLAGS		;Initialize key globals

	;fall through


;;; Mainline ;;;

Main
	movlb	1		;All non-specifically handled resets should be
	movlw	low SErrorR	; considered errors
	btfss	PCON,NOT_RWDT	;If a WDT reset occurred, let the user know
	movlw	low SWDTR	; "
	btfss	PCON,NOT_POR	;If a power-on reset occurred, say hello
	movlw	low SPOR	; "
	btfss	PCON,NOT_RI	;If a reset-command reset occurred, say nothing
	movlw	low SReset	; "
	call	UartTxString	;Transmit whatever the appropriate string is
	movlb	1		;Reset reset causes
	movlw	B'00011111'	; "
	movwf	PCON		; "
	
	bra	Synthesis	;TODO determine mode automagically

Passthrough
	call	SetPassthrough	;Set up CLCs for passthrough mode
	movlb	7		;Set the IOC interrupt to interrupt on vertical
	movlw	1 << VS_PIN	; sync negative edges so as long as we keep
	movwf	IOCAN		; getting those, we keep clearing the WDT and
	clrf	IOCAP		; keep passing through vertical and horizontal
	movlw	1 << GIE	; sync until the WDT times out
	movwf	INTCON		; "
Passth0	clrf	IOCAF		;Clear the IOC interrupt
	bsf	INTCON,IOCIE	;Set the IOC interrupt to wake us up and go to
	sleep			; sleep
	bra	Passth0		;Awakened by IOC interrupt, WDT cleared, loop

Synthesis
	call	SetSynthesis	;Set up CLCs for synthesis mode
	movlw	low SProfil	;Let the user know what we're doing
	call	UartTxString	; "
	call	ProfileSignal	;Profile the signal
	call	ParseProfile	;Parse the profile
	movf	WREG,W		;If no error, skip ahead
	btfsc	STATUS,Z	; "
	bra	Synthe0		; "
	call	UartTxHex	;If there was an error, print the code and wait
	bra	$		; for WDT to reset us
Synthe0	call	ConfigTimers	;Configure timers
	movlw	low SProfOk	;Let user know profiling succeeded and dump a
	call	UartTxString	; mini signal trace of the signal to UART
	call	UartTxHexDump	; followed by a newline
	movlw	low SCRLF	; "
	call	UartTxString	; "
	movlw	low SHeader	;Print profile header
	call	UartTxString	; "
	movf	HPULSEH,W	;Print horizontal pulse count
	call	UartTxHex	; "
	movf	HPULSEL,W	; "
	call	UartTxHex	; "
	call	UartTxSpace	;Print space
	movf	HPERIOD,W	;Print horizontal period
	call	UartTxHex	; "
	call	UartTxSpace	;Print space
	movf	VPULSES,W	;Print vertical pulse count
	call	UartTxHex	; "
	call	UartTxSpace	;Print space
	movf	SERPRE,W	;Print pre-vertical serrated pulse count
	call	UartTxHex	; "
	call	UartTxSpace	;Print space
	movf	SERPOST,W	;Print post-vertical serrated pulse count
	call	UartTxHex	; "
	call	UartTxSpace	;Print space
	movlw	low SYes	;Print yes if vertical pulse is flat, else no
	btfss	FLAGS,VISFLAT	; "
	movlw	low SNo		; "
	call	UartTxString	; "
	movlw	low SYes	;Print yes if last vertical pulse eats first
	btfss	FLAGS,VATEANH	; horizontal pulse, else no
	movlw	low SNo		; "
	call	UartTxString	; "
	movlw	low SCRLF	;Two newlines
	call	UartTxString	; "
	movlw	low SCRLF	; "
	call	UartTxString	; "
	call	FindStart	;Find the start of the vertical sync area
	btfss	FLAGS,VISFLAT	;If there is more than one vertical pulse, skip
	bra	Synthe7		; ahead
Synthe1	movf	SERPRE,W	;If there are no pre-vertical serrated pulses,
	btfsc	STATUS,Z	; skip ahead, else loop waiting for a falling
	bra	Synthe7		; edge and eating a pulse
	movwf	X1		; "
Synthe2	call	WaitForLow	; "
	call	EatPulse	; "
	decfsz	X1,F		; "
	bra	Synthe2		; "
	movf	SERPOST,W	;Check the number of post-vertical serrated
	btfsc	STATUS,Z	; pulses; if there are none, skip ahead
	bra	Synthe5		; "
	movwf	X1		;Store it for use as a loop variable
	btfsc	FLAGS,VATEANH	;If the vertical pulse eats first horizontal
	bra	Synthe3		; pulse, skip ahead
	call	SplitSingleFlat	;If there are post-vertical serrated pulses and
	call	WaitForLow	; the vertical pulse does not eat the first
	call	EatPulse	; horizontal pulse, loop waiting for a falling
	decfsz	X1,F		; edge and eating a pulse, then sleep and loop
	bra	Synthe4		; "
	bra	Synthe6		; "
Synthe3	call	SplitSingleFlat	;If there are post-vertical serrated pulses and
	call	EatPulse	; the vertical pulse eats the first horizontal
	decfsz	X1,F		; pulse, do this split loop thing for speed
	bra	Synthe4		; (there's very little time coming out of
	bra	Synthe6		; SplitSingleFlat); it does the same as above
Synthe4	call	WaitForLow	; except the first call to WaitForLow is
	call	EatPulse	; skipped
	decfsz	X1,F		; "
	bra	Synthe4		; "
	bra	Synthe6		; "
Synthe5	call	SplitSingleFlat	;If there are no post-vertical serrated pulses,
Synthe6	call	SleepEdgePrep	; just split the single vertical pulse, sleep,
	call	SleepEdge	; and loop
	bra	Synthe1		; "
Synthe7	decf	VPULSES,W	;If vertical pulses are not flat, for each one
	btfsc	STATUS,Z	; except the last, wait for a falling edge on
	bra	Synthe9		; composite sync, set vertical sync low (if it
	movwf	X1		; isn't already), then force horizontal sync
Synthe8	call	WaitForLow	; high until composite sync goes high
	call	SetVSyncLow	; "
	call	ForceHSyncHigh	; "
	decfsz	X1,F		; "
	bra	Synthe8		; "
Synthe9	call	WaitForLow	;Wait for the final falling edge
	movlb	30		;Connect CLC2's latch enable to CLC1's output
	movlw	B'00100000'	; (vertical sync, currently low)
	movwf	CLC2GLS0	; "
	movlw	B'00000011'	;Momentarily actuate the S line, setting
	movwf	CLC2GLS3	; horizontal sync high
	clrf	CLC2GLS3	; "
	call	SleepEdgePrep2	;Get ready for sleep
	movlb	0		;If composite sync is high at this point, there
	btfsc	PORTA,CS_PIN	; has been a change in sync and we should reset
	reset			; to deal with it
	call	WaitForHigh	;Wait for composite sync to go high
	movlb	30		;Momentarily actuate the S input of the DFF
	movlw	B'00000011'	; that feeds vertical sync, which also sets the
	movwf	CLC1GLS3	; enable on the latch that feeds horizontal
	clrf	CLC1GLS3	; sync
	call	SleepEdge	;Sleep until the next vertical sync area
	bra	Synthe7		;Loop


;;; Subprograms ;;;

;Configure the CLCs so vertical and horizontal sync are passed through.
;Clobbers: BSR (30)
SetPassthrough
	movlb	30		;Set CLC1 to feed vertical sync through from
	clrf	CLC1SEL0	; CLC1IN0/RA3 to CLC1/RA2 unaltered
	clrf	CLC1SEL1	; "
	movlw	B'00000010'	; "
	movwf	CLC1GLS0	; "
	movwf	CLC1GLS1	; "
	movwf	CLC1GLS2	; "
	movwf	CLC1GLS3	; "
	clrf	CLC1POL		; "
	movlw	B'11000000'	; "
	movwf	CLC1CON		; "
	movlw	B'00000001'	;Set CLC2 to feed horizontal sync through from
	movwf	CLC2SEL0	; CLC2IN1/RA0 to CLC2/CLC1IN1/RA5 unaltered
	clrf	CLC2SEL1	; "
	movlw	B'00000010'	; "
	movwf	CLC2GLS0	; "
	movwf	CLC2GLS1	; "
	movwf	CLC2GLS2	; "
	movwf	CLC2GLS3	; "
	clrf	CLC2POL		; "
	movlw	B'11000000'	; "
	movwf	CLC2CON		; "
	return			;Done

;Configure the CLCs so both vertical and horizontal sync are set up to be
; synthesized from composite sync, with composite sync being fed to horizontal
; sync at first
;Clobbers: BSR (30)
SetSynthesis
	movlb	30		;Set CLC2 to be a transparent latch with S and
	movlw	B'00110000'	; R, initially feeding through composite sync,
	movwf	CLC2SEL0	; with Timer2 and CLC1 available as input
	clrf	CLC2SEL1	; sources
	movlw	B'00000011'	; "
	movwf	CLC2GLS0	; "
	movlw	B'00000010'	; "
	movwf	CLC2GLS1	; "
	clrf	CLC2GLS2	; "
	clrf	CLC2GLS3	; "
	clrf	CLC2POL		; "
	movlw	B'11000111'	; "
	movwf	CLC2CON		; "
	movlw	B'00000001'	;Set CLC1 to be a DFF with S and R, initially
	movwf	CLC1SEL0	; set, and ready on demand to (1) be set low or
	clrf	CLC1SEL1	; (2) be set high on a rising edge from
	movlw	B'00000011'	; horizontal (reflecting composite) sync
	movwf	CLC1GLS0	; "
	movwf	CLC1GLS1	; "
	clrf	CLC1GLS2	; "
	movwf	CLC1GLS3	; "
	clrf	CLC1POL		; "
	movlw	B'11000100'	; "
	movwf	CLC1CON		; "
	clrf	CLC1GLS3	; "
	return			;Done

;Set up Timer2's period register to time synthetic pulses and T1SKPH:L up with
; the number of positive edges that must be skipped in between sync generation.
;Clobbers: BSR (0)
ConfigTimers
	movlb	0		;Set Timer2's period so it can be used to time
	decf	HPERIOD,W	; synthetic vertical pulses and start it
	movwf	PR2		; "
	movlw	B'00000100'	; "
	movwf	T2CON		; "
	comf	HPULSEH,W	;One's complement horizontal pulse count to
	movwf	T1SKPH		; start with (we still need to add one to it)
	comf	HPULSEL,W	; "
	movwf	T1SKPL		; "
	lslf	SERPRE,W	;Add the pre and post vertical serrated pulse
	addwf	SERPOST,W	; counts times two (plus one, but only if the
	addwf	SERPOST,W	; vertical pulse doesn't eat a horizontal one,
	btfss	FLAGS,VATEANH	; this way we actually subtract one or don't)
	addlw	1		; to the complemented horizontal pulse count;
	addwf	T1SKPL,F	; this gives us a count which, once it rolls 
	movlw	0		; over, equals the number of rising edges we'll
	addwfc	T1SKPH,F	; need to skip
	return			;Done

;Find the starting point where the sync generation program needs to step in.
;Clobbers: BSR (0)
FindStart
	movlb	7		;Get IOC set up to catch falling edges
	movlw	1 << CS_PIN	; "
	movwf	IOCAN		; "
	clrf	IOCAP		; "
	movlw	1 << GIE	;Make sure interrupt subsystem is on (but not
	movwf	INTCON		; yet the IOC interrupt source)
FindSt0	movlb	7		;Sleep until we see a negative edge on the
	clrf	IOCAF		; composite sync pin
	bsf	INTCON,IOCIE	; "
	sleep			; "
	movlb	0		;If we come out of sleep to find that composite
	btfsc	PORTA,CS_PIN	; sync pin is high again, it's not a vertical
	bra	FindSt0		; pulse, so loop and retry
	clrf	T1CON		;Stop Timer1
	lslf	SERPOST,W	;Lengthen the Timer1 skip value by two times
	addwf	VPULSES,W	; the post-vertical serrated pulses plus the
	btfsc	FLAGS,VATEANH	; number of vertical pulses, minus one if the
	addlw	-1		; last vertical pulse eats a horizontal pulse
	subwf	T1SKPL,W	; "
	movwf	TMR1L		; "
	movlw	0		; "
	subwfb	T1SKPH,W	; "
	movwf	TMR1H		; "
	clrf	PIR1		;Set up interrupts to wake us when Timer1 has
	movlb	1		; seen that many positive edges
	movlw	1 << TMR1IE	; "
	movwf	PIE1		; "
	movlb	0		; "
	movlw	B'11000000'	; "
	movwf	INTCON		; "
	movlw	B'10000101'	; "
	movwf	T1CON		; "
	sleep			;Sleep until that happens
	return			;Done

;Configure CLC1 so the DFF that feeds vertical sync is set low.
;Clobbers: BSR (30)
SetVSyncLow
	movlb	30		;Momentarily actuate the R input of the DFF
	movlw	B'00000011'	; "
	movwf	CLC1GLS2	; "
	clrf	CLC1GLS2	; "
	return			;Done

;Configure CLC1 so the DFF that feeds vertical sync goes high on the next
; rising edge of horizontal sync (which is being fed from composite sync),
; wait until that happens, and return.
;Clobbers: BSR (30)
SetVSyncHighEdge
	movlb	30		;Set the clock input of the DFF to composite
	bcf	CLC1GLS0,0	; sync
	btfss	CLC1CON,LC1OUT	;Wait until a rising edge from composite sync
	bra	$-1		; sets the DFF
	bsf	CLC1GLS0,0	;Hold the DFF clock high
	return			;Done

;Pass composite sync through to horizontal sync and prepare Timer1 to wake us
; up after the correct number of horizontal pulses.
;Clobbers: BSR (0)
SleepEdgePrep
	movlb	30		;Set the transparent latch to feed composite
	movlw	B'00000011'	; sync through to horizontal sync
	movwf	CLC2GLS0	; "
SleepEdgePrep2
	movlw	1 << GIE	;Ready Timer1 to start counting edges as soon
	movwf	INTCON		; as it's started and its interrupt to wake us
	movlb	1		; up
	movlw	1 << TMR1IE	; "
	movwf	PIE1		; "
	movlb	0		; "
	movlw	B'10000100'	; "
	movwf	T1CON		; "
	clrf	PIR1		; "
	bsf	INTCON,PEIE	; "
	movf	T1SKPH,W	;Load Timer1 with a number of rising edges
	movwf	TMR1H		; (negated) that will take us through the
	movf	T1SKPL,W	; horizontal pulses
	movwf	TMR1L		; "
	return			;Done

;Start Timer1 and go to sleep, having it wake us after the preset number of
; edges on composite/horizontal sync.
;Clobbers: BSR (0)
SleepEdge
	movlb	0		;Start Timer1
	bsf	T1CON,TMR1ON	; "
	sleep			;Sleep until awakened by Timer1
	return			;Done

;Wait for composite sync to be low.
;Clobbers: BSR (0)
WaitForLow
	movlb	0		;Wait for composite sync to be low
	btfsc	PORTA,CS_PIN	; "
	bra	$-1		; "
	return			;Done

;Wait for composite sync to be high.
;Clobbers: BSR (0)
WaitForHigh
	movlb	0		;Wait for composite sync to be high
	btfss	PORTA,CS_PIN	; "
	bra	$-1		; "
	return			;Done

;Force horizontal sync high until composite sync goes high, then pass composite
; through to horizontal again.
;Clobbers: BSR (30)
ForceHSyncHigh
	movlb	30		;Set the transparent latch to hold its state
	clrf	CLC2GLS0	; "
	movlw	B'00000011'	;Momentarily actuate the S line
	movwf	CLC2GLS3	; "
	clrf	CLC2GLS3	; "
	movlb	0		;Wait for composite sync to go high
	btfss	PORTA,CS_PIN	; "
	bra	$-1		; "
	movlb	30		;Set the transparent latch to feed composite
	movwf	CLC2GLS0	; sync through again
	return			;Done

;Wait for composite sync to go high, latch it high on horizontal sync, then
; wait for another pulse to pass by on composite sync, then pass composite sync
; through to horizontal sync again.
;Clobbers: BSR (30)
EatPulse
	movlb	7		;Set IOC to catch rising edges on composite
	movlw	1 << CS_PIN	; sync
	movwf	IOCAP		; "
	clrf	IOCAN		; "
	movlb	0		;Wait for composite sync to go high
	btfss	PORTA,CS_PIN	; "
	bra	$-1		; "
	movlb	7		;Clear the rising edge IOC just caught
	clrf	IOCAF		; "
	movlb	30		;Set the transparent latch to hold its state
	clrf	CLC2GLS0	; "
	movlw	B'00000011'	;Prepare to undo the previous
	btfss	INTCON,IOCIF	;Wait for a rising edge on composite sync
	bra	$-1		; "
	movwf	CLC2GLS0	;Feed composite sync through again
	return			;Done

;Split a single flat vertical pulse into multiple correctly-spaced horizontal
; pulses.
;Clobbers: X0 (0), BSR (30)
SplitSingleFlat
	movf	VSYNTH,W	;Set the counter for the number of synthetic
	movwf	X0		; horizontal pulses to create
	movlb	7		;Set IOC to detect premature rise of composite
	movlw	1 << CS_PIN	; sync (which would imply a sync change and
	movwf	IOCAP		; necessitate a reset)
	clrf	IOCAN		; "
	clrf	IOCAF		; "
	movlb	30		;Set vertical sync to go low when horizontal
	clrf	CLC1GLS1	; sync (which is passed through from composite
	bcf	CLC1GLS0,1	; sync) does
	movlb	1		;Set Timer0 so that it overflows on the next
	bsf	OPTION_REG,TMR0CS; falling transition on T0CKI (which happens
	movlb	0		; to also be the vertical sync output)
	movlw	0xFF		; "
	movwf	TMR0		; "
	clrf	T1CON		;Stop Timer1 if it was started and load it with
	movlw	6		; a value that we want to increment in sync
	movwf	TMR1L		; with the arrival of the vertical pulse
	movlw	B'11111001'	;Set the Timer1 gate to start Timer1 as soon as
	movwf	T1GCON		; pulse arrives (passed from composite sync to
	bsf	T1CON,TMR1ON	; horizontal sync to vertical sync to T0CKI)
	btfss	T1GCON,T1GVAL	; "
	bra	$-1		; "
	movf	TMR1L,W		;Set Timer2 from Timer1 and disable the gate so
	clrf	T1GCON		; anything else using Timer1 won't be messed up
	movwf	TMR2		; by it
	movlb	30		;Set horizontal sync DFF to be set low by
	bsf	CLC2GLS2,3	; Timer2
SplSng0	movlw	B'00000011'	;Set vertical sync DFF to hold its clock high
	movwf	CLC1GLS1	; and raise its input in preparation for later
	movwf	CLC1GLS0	; "
	clrf	CLC2GLS0	;Set transparent latch to hold its state then
	movwf	CLC2GLS3	; momentarily actuate the S line so horizontal
	clrf	CLC2GLS3	; sync goes high
	btfsc	INTCON,IOCIF	;If at any time a rising edge was detected on
	reset			; composite sync, signal's changed, reset
	decf	X0,F		;Decrement the counter of synthetic horizontal
	btfsc	STATUS,Z	; pulses that remain; if this is the last one,
	bcf	CLC1GLS0,1	; vertical sync rises when horizontal falls
	btfsc	CLC2CON,LC2OUT	;Wait until horizontal sync DFF has been set
	bra	$-1		; low by Timer2
	movwf	CLC2GLS0	;Pass composite sync through to horizontal sync
	btfss	STATUS,Z	;If this is not the last synthetic horizontal
	bra	SplSng0		; pulse, loop
	movlb	0		;Wait for composite sync to go high
	btfss	PORTA,CS_PIN	; "
	bra	$-1		; "
	movlb	30		;Set vertical sync DFF to hold its clock high
	bsf	CLC1GLS0,1	; "
	bcf	CLC2GLS2,3	;Disconnect Timer2 from horizontal sync DFF
	return			;Done

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

;Write the contents of the SRAM buffer, in hex, to the UART.
;Clobbers: X1, X0 (0), FSR0 (0x2030), BSR (0)
UartTxHexDump
	movlw	0x20		;Put FSR0 at the beginning of the SRAM buffer
	movwf	FSR0H		; "
	clrf	FSR0L		; "
UaTxHD0	moviw	FSR0++		;Transmit next byte
	call	UartTxHex	; "
	movlw	0x20		;Transmit a space
	call	UartTx		; "
	movf	FSR0L,W		;If we're at a 16-byte boundary, transmit a
	andlw	B'00001111'	; newline
	btfss	STATUS,Z	; "
	bra	UaTxHD1		; "
	movlw	0x0D		; "
	call	UartTx		; "
	movlw	0x0A		; "
	call	UartTx		; "
UaTxHD1	movf	FSR0L,W		;If we've written out the whole buffer, done,
	xorlw	0x30		; else loop
	btfss	STATUS,Z	; "
	bra	UaTxHD0		; "
	return			; "

;Profile the vertical pulses and the surrounding area in the composite sync
; signal, loading the 48-byte SRAM buffer with it.  Composite sync must be fed
; through CLC2 (so it's readable as the T1CKI pin) before calling.  Also sets
; HPULSEH:L.
;Clobbers: FSR1 (IOCAF), FSR0 (0x2040), BSR (0)
ProfileSignal
	movlw	0x20		;Point FSR0 to start of 48-byte linear SRAM
	movwf	FSR0H		; buffer
	clrf	FSR0L		; "
	movlw	high IOCAF	;Point FSR1 to IOCAF for ease of clearing
	movwf	FSR1H		; "
	movlw	low IOCAF	; "
	movwf	FSR1L		; "
	clrf	HPULSEH		;Clear the horizontal (short) pulse count
	clrf	HPULSEL		; "
	movlb	1		;Get Timer1 set up to interrupt when peripheral
	movlw	1 << TMR1IE	; interrupts are enabled
	movwf	PIE1		; "
	movlb	0		; "
	clrf	T1CON		; "
	clrf	PIR1		; "
	movlb	7		;Get IOC set up to catch falling edges
	movlw	1 << CS_PIN	; "
	movwf	IOCAN		; "
	clrf	IOCAP		; "
	movlb	0		; "
	movlw	1 << GIE	;Make sure interrupt subsystem is on (but not
	movwf	INTCON		; yet the IOC interrupt source)
Profil0	call	Profil5		;Wait for a short pulse in case we're already
	btfss	PORTA,CS_PIN	; in the middle of vertical sync
	bra	Profil0		; "
Profil1	call	Profil5		;Wait for a long pulse so we can find the
	btfsc	PORTA,CS_PIN	; vertical sync
	bra	Profil1		; "
Profil2	call	Profil5		;Wait for a short pulse again so we can start
	btfss	PORTA,CS_PIN	; counting them
	bra	Profil2		; "
Profil3	incf	HPULSEL,F	;Increment the horizontal pulse count
	btfsc	STATUS,Z	; "
	incf	HPULSEH,F	; "
	call	Profil5		;Wait for a pulse, loop to count it if it's
	btfsc	PORTA,CS_PIN	; short or finish the count if it's long
	bra	Profil3		; "
	movlw	10		;Subtract the horizontal pulse count from a
	movwf	TMR1L		; constant offset to get the number of positive
	clrf	TMR1H		; edges (negated) that Timer1 should count
	movf	HPULSEL,W	; before waking up and profiling the composite
	subwf	TMR1L,F		; sync signal during and surrounding the
	movf	HPULSEH,W	; vertical sync interval
	subwfb	TMR1H,F		; "
	movlw	B'10000101'	;Arm Timer1 to count up and then wake us from
	movwf	T1CON		; sleep (we should be in the period where
	bsf	INTCON,PEIE	; composite sync is high before a horizontal
	sleep			; pulse)
	movlw	0		;Start W off at zero
	btfsc	PORTA,CS_PIN	;Wait until that pulse begins
	bra	$-1		; "
	addlw	1		;Measure and record down cycle of 1st pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 1st pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 2nd pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 2nd pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 3rd pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 3rd pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 4th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 4th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 5th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 5th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 6th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 6th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 7th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 7th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 8th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 8th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 9th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 9th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 10th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 10th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 11th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 11th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 12th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 12th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 13th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 13th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 14th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 14th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 15th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 15th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 16th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 16th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 17th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 17th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 18th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 18th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 19th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 19th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 20th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 20th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 21st pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 21st pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 22nd pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 22nd pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 23rd pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 23rd pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record down cycle of 24th pulse
	btfss	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	addlw	1		;Measure and record up cycle of 24th pulse
	btfsc	PORTA,CS_PIN	; "
	bra	$-2		; "
	movwi	FSR0++		; "
	clrf	FSR0L		;Adjust recorded measurements so they're based
	movlw	0		; on zero rather than the previous measurement
Profil4	subwf	INDF0,F		; "
	addwf	INDF0,W		; "
	incf	FSR0L,F		; "
	btfss	FSR0L,6		; "
	bra	Profil4		; "
	return			;Done
Profil5	btfss	PORTA,CS_PIN	;If we're already in the middle of a pulse,
	bra	$-1		; wait until it's over
	clrf	INDF1		;Sleep while we wait for a negative edge on the
	bsf	INTCON,IOCIE	; composite sync line, then return (typically
	sleep			; 5 us after the change); caller can check the
	return			; line state to see if short or long pulse

;Parse parameters out of the profiled signal that are needed to synthesize a
; horizontal/vertical signal from it.
;Clobbers: X2, X1, X0, FSR0, BSR (0)
;Exit codes:
;0 - success
;1 - no vertical pulses
;2 - serrated pulse followed by non-serrated pulse before vertical pulse(s)
;3 - half serrated pulse followed by vertical pulse
;4 - half serrated pulse followed by horizontal pulse, pre-vertical
;5 - no end of vertical pulses
;6 - no normal horizontal pulse to finish profile
;7 - half serrated pulse followed by horizontal pulse, post-vertical
ParseProfile
	movlw	0x20		;Need this for accessing the profile
	movwf	FSR0H		; "
	movlb	0		;Determine horizontal pulse period from the
	movf	PROF00,W	; average width of the outermost pulses in the
	addwf	PROF01,W	; profile (no need to divide them as the pulses
	addwf	PROF02,W	; in the profile are in microseconds and the
	addwf	PROF03,W	; period needs to be in quarter-microseconds)
	addwf	PROF2C,W	; "
	addwf	PROF2D,W	; "
	addwf	PROF2E,W	; "
	addwf	PROF2F,W	; "
	movwf	HPERIOD		; "
	lsrf	WREG,W		;Multiply horizontal pulse period by 3/4 to get
	movwf	X0		; a threshold we can use to tell the difference
	lsrf	WREG,W		; between serrated and non-serrated horizontal
	addwf	X0,F		; pulses and by 1/4 to get one we can use to
	movwf	X1		; tell if the final (or only) vertical pulse
	lsrf	X0,F		; swallowed the first horizontal pulse
	lsrf	X0,F		; "
	lsrf	X1,F		; "
	lsrf	X1,F		; "
	bcf	FLAGS,VISFLAT	;Clear the single-flat-vertical-pulse flag
	bcf	FLAGS,VATEANH	;Clear the vertical-ate-a-horizontal-pulse flag
	clrf	VPULSES		;Clear the vertical pulses count
	clrf	SERPRE		;Clear the pre-vertical serration pulses count
	clrf	SERPOST		;Clear the post-vertical serration pulses count
	clrf	FSR0L		;Rewind the pointer
ParseP0	movlw	1		;If we've scanned through the profile and not
	call	ParseP9		; found any vertical pulses, error out
	movf	INDF0,W		;If the current pulse is long enough to be a
	sublw	VERTTHR		; vertical pulse, skip ahead to count it as one
	btfss	STATUS,C	; "
	bra	ParseP3		; "
	moviw	FSR0++		;If the current pulse and the uptime following
	addwf	INDF0,W		; it are short enough to indicate this is a
	subwf	X0,W		; serrated pulse, skip ahead to count it as one
	btfsc	STATUS,C	; "
	bra	ParseP2		; "
	movf	SERPRE,W	;If this is a normal (non-serrated) horizontal
	btfss	STATUS,Z	; pulse but we've seen pre-vertical serrated
	retlw	2		; pulses already, error out
ParseP1	addfsr	FSR0,1		;Otherwise, advance to the next pulse and loop
	bra	ParseP0		; "
ParseP2	addfsr	FSR0,1		;We saw half a serrated pulse, make sure the
	movf	INDF0,W		; following pulse is the other half; if it is a
	sublw	VERTTHR		; vertical pulse instead, error out
	btfss	STATUS,C	; "
	retlw	3		; "
	moviw	FSR0++		;If the following pulse is too long to be the
	addwf	INDF0,W		; other half, error out
	subwf	X0,W		; "
	btfss	STATUS,C	; "
	retlw	4		; "
	incf	SERPRE,F	;Otherwise, count this as a serrated pulse and
	bra	ParseP1		; loop
ParseP3	movf	INDF0,W		;This was a vertical pulse, save its width for
	movwf	X2		; later use
	incf	VPULSES,F	;Increment the count of vertical pulses and
	addfsr	FSR0,2		; move on to the next pulse
	movlw	5		;If we ran out of profile before seeing the end
	call	ParseP9		; of vertical pulses, error out
	movf	INDF0,W		;If the current pulse is long enough to be a
	sublw	VERTTHR		; vertical pulse, loop to count it as one
	btfss	STATUS,C	; "
	bra	ParseP3		; "
	moviw	--FSR0		;Before we move on, check if the uptime after
	subwf	X1,W		; last vertical pulse is long enough to suggest
	btfsc	STATUS,C	; that it swallowed the first horizontal pulse
	bra	ParseP4		; following it; if not, skip ahead
	bsf	FLAGS,VATEANH	;If so, set the flag that says so
	moviw	FSR0++		;This is cheating a little, but reckon whether
	addwf	INDF0,W		; the pulse that was swallowed was serrated or
	subwf	X0,W		; not by adding the downtime following the
	btfsc	STATUS,C	; vertical pulse to the uptime following it; if
	bra	ParseP5		; serrated, skip ahead to make sure there's a
	bra	ParseP6		; second half, else skip (further) ahead
ParseP4	addfsr	FSR0,1		;If we ran out of profile before finding one
	movlw	6		; normal horizontal pulse, error out
	call	ParseP9		; "
	moviw	FSR0++		;If the current pulse and the uptime following
	addwf	INDF0,W		; it are short enough to indicate that it's a
	subwf	X0,W		; serrated pulse, proceed to check for the
	btfss	STATUS,C	; other half, else skip ahead
	bra	ParseP6		; "
	addfsr	FSR0,1		;Check whether the following pulse is the other
ParseP5	moviw	FSR0++		; half of the serrated pulse; if it's not,
	addwf	INDF0,W		; error out, else count it and loop
	subwf	X0,W		; "
	btfss	STATUS,C	; "
	retlw	7		; "
	incf	SERPOST,F	; "
	bra	ParseP4		; "
ParseP6	decf	VPULSES,W	;If there's more than one vertical pulse, we're
	btfss	STATUS,Z	; done and successful
	retlw	0		; "
	clrf	VSYNTH		;If not, we need to count the number of pulses
	lsrf	HPERIOD,W	; that there should be, so take the length of
	lsrf	WREG,W		; our one vertical pulse and divide it by the
	movwf	X0		; horizontal period
	lsrf	X0,W		; "
	addwf	X2,F		; "
ParseP7	movf	X0,W		; "
	subwf	X2,F		; "
	btfss	STATUS,C	; "
	bra	ParseP8		; "
	incf	VSYNTH,F	; "
	bra	ParseP7		; "
ParseP8	decf	VSYNTH,W	;If the result is more than one, set the flag
	btfss	STATUS,Z	; to indicate that the vertical pulse is flat
	bsf	FLAGS,VISFLAT	; and we should synthesize horizontal pulses
	retlw	0		; across it; either way, return success
ParseP9	btfsc	FSR0L,4		;If we haven't gone off the end of the profile,
	btfss	FSR0L,5		; everything's okay, return to caller
	return			; "
	movlb	31		;Otherwise, return to caller's caller with the
	decf	STKPTR,F	; exit code in W
	return			; "
	

;;; Strings ;;;

SPOR	da	"Welcome to TashSync 20250607\r\n\x00"
SWDTR	da	"sync timeout\r\n\x00"
SReset	da	"sync change\r\n\x00"
SErrorR	da	"serious error\r\n\x00"
SProfil	da	"Profiling...\x00"
SProfOk	da	"ok.\r\n\r\nTrace:\r\n\x00"
SHeader	da	"HP   HT VP Pr Po VF VE"
SCRLF	da	"\r\n"
SNull	da	"\x00"
SYes	da	"Y  \x00"
SNo	da	"N  \x00"


;;; End of Program ;;;

	end
