;***********************************************************************************
;*                            Stepper Motor Control v1.5                           *
;*                             PIC16F84A & ULN2003                                 *
;*                                                                                 *
;*                         Jugurtha Hadjar & Raouf Moualdi.                        * 
;*                 4ème Année Ingénieur Instrumentation U.S.T.H.B                  *
;*                                                                                 *
;*          Université des Sciences et de la Technologie Houari Boumédiène.        *
;*                                                                                 *
;*               Contact: Remove capital letters from the address.                 *
;*              REMOVEMEjugurtha.haSPAMdjarSPAM@REMOVEMEhotmail.com                *
;*                             http://docs.com/@jhadjar                            *
;***********************************************************************************

;***********************************************************************************
;* Description:                                                                    *
;* TEC 585 Project00: Stepper Motor Control using PIC1684A                         *
;* A ULN2003 (7 Darlington Array) is used as an interface.                         *
;* Two push-buttons to make the motor rotate to the LEFT or to the RIGHT.          *
;*                                                                                 *
;* Original code provided courtesy of my teacher, and put as a comment for memo    *
;* at the end of this file, to keep track of modifications.                        *
;* It contains errors intentionally slipped by the teacher to stimulate learning.  *
;*                                                                                 *
;***********************************************************************************





;***********************************************************************************
;*                                   THE SETUP                                     *
;***********************************************************************************

LIST p=16F84A              ;Which processor ?                                             
#include "p16F84A.inc"	   ;Where's the file?                                             
                                        												  

__CONFIG _CP_OFF & _WDT_OFF & _PWRTE_ON & _RC_OSC       
    
	;Code Protection OFF
														;Watchdog Timer  OFF
														;Power Up Timer  ON
        													;RC Oscillator.
        
	;__CONFIG H'3FF9'



;************************************************************************************
;*                                  EQUATES                                         *
;************************************************************************************

STEP   EQU 0x0C      ; See 2-2 Memory Organization (Datasheet 16F84A)

#define RIGHTBUTTON PORTA,0
#define LEFTBUTTON  PORTA,1

#define RIGHTLED    PORTB,6
#define LEFTLED     PORTB,7



;**********************************
;       Stepper Motor Phases      *
;---------------------------------*
#define SETPHASE0 retlw B'00001001'
#define SETPHASE1 retlw B'00001010'
#define SETPHASE2 retlw B'00000110'
#define SETPHASE3 retlw B'00000101'
;*********************


;**********************************************************************************               ;* TRISB is in the 7th location in Bank1, hence setting RP0 to 1 to access it.    *
;* TRISB is the PORTB data direction register. It's on 8 bits.                    *
;* Clearing TRISB configures the PORTB as an OUTPUT.(Bits can be set/cleared      *
;* individually to configure each I/O port separately. RB0,RB1,...,RB7            *
;**********************************************************************************


 
;******************************************************************************************
;*                             OPTION_REG Description                                     * 
;*----------------------------------------------------------------------------------------*
;* Bit7:RBPU(active low) PORTB Pull-Up Enable Bit (1, Disabled)                           *
;* Bit6:INTEDG, Interrupt Edge Select Bit(0, Interrupt on falling edge B0/INT)            *
;* Bit5:T0CS, TMR0 Clock Source Select Bit (0, Internal Instruction Cycle Clock)          *
;* Bit4:T0SE, TMR0 Source Edge Select Bit(1, Increment on Hi-to-Lo transition on RA4)     *
;* Bit3:PSA,PRESCALER Assignment Bit(0, Prescaler assigned to TMR0 module)                *
;* Bit2-0:PRESCALER Select Bits (110, 1:128)                                              *
;******************************************************************************************

				cblock 0x0C           
		                  
	  				STEP:1         ; The byte at 0x0C:STEP.
                     
    			endc
     
	
		org 0x00              ;Reset Vector.

	    goto ENTRYPOINT

        org 0x04
        ;Supposed to write something here,
        ;Maybe poetry. 

ENTRYPOINT
    
    ;mSelectBank1             ;Select Bank 1, Duh :)
    banksel TRISB                         ;That's why I gave meaningful names in the first place :)
    	
    movlw 0x0                ; Load W with 0x00 
    movwf TRISB              ; All bits to 0. (TRISB=00000000)
                             
                    
    banksel TRISA
    
    movlw 0xFF               ; Load W with 0xFF. 
	movwf TRISA              ; All bits to 1. (TRISA=11111111)  
                              
    movlw 0x96               
	movwf OPTION_REG      ; Configure OPTION_REG (81h in Bank1).

	banksel PORTB
    ;mSelectBank0 	;Getting back to Bank0 
                    ;There was an error in v1.3, I wrote mBankSelect0
                    ;It obviously didn't work, as we couldn't write to PORTB.
                    ;It drove me CRAZY.
	movlw 0x00
    movwf TRISB ;All PORTB pins to 0.
    
	clrf STEP		;Clearing STEP at 0x0C 
	clrf TMR0		;Clearing TMR0
	    
    goto START		;Here we go..


;******************************************************************************************
;*                        MOTOR SEQUENCE: PHASES (In Program Memory)                      *
;******************************************************************************************
        
            ;Computed GOTO. We add w to PCL.        
            ;This is the sequence used to control the motor.
            ;It's a Table in the program memory
			;When a call gets with an offset value loaded in w
			;We then add the Program Counter low 8bits to that value,
			;The result is branching to the new value of PCL.
			;See Application Note 556 "Implementing a Table Read"
			;Stan D'Souza.

SEQUENCE	 addwf PCL,f ; Adds W to the PC Low 8 bits. The location
                         ; of the next instruction is computed that way.
        
	SETPHASE0           ;Return With Literal in W 0x09. With an initial w of 0x00
	SETPHASE1           ;Return With Literal in W 0x0A. With an initial w of 0x01
	SETPHASE2           ;Return With Literal in W 0x06. With an initial w of 0x02
	SETPHASE3           ;Return With Literal in W 0x05. With an initial w of 0x03

;***************************************************************************************

START	btfsc   TMR0,7 ; Bit test f, skip if clear. 
                       ; In the original code, it was BTFSS. Which doesn't make sens.
                       ; Because we've just cleared TMR0, so TMR<7>=0 , so GOTO START
                       ; gets executed a never ending loop.
                       ; Although, I have to figure out this TMR0 thing later. 
                       ; Its purpose, and all.

                       ; Test the 7th bit of TMR0, and skip the goto START if it's 0
				
				goto START ; Only if TMR0<7>=1
				clrf TMR0  ; Clear TMR0 (All bits 0)




;**************************************************************************************
;*                                RIGHT SUBROUTINE                                    *
;**************************************************************************************
;This makes the motor turn right when RA0 is pulled down.(button pushed and kep down) 

RIGHT	
        
        btfsc	RIGHTBUTTON ;Test RA0. If it's Zero, skip to Incf. If it's 1, goto left.
        goto 	LEFT
		incf	STEP,f  ;Increment the value held in STEP (address 0x0C)
		movlw	0x04    ;Move literal to W (0x04-->w)
		subwf	STEP,w  ;Substract w from f, the result goes to w. f-w--->w
		     						
		btfsc	STATUS,C ;Test the Carry bit(flag) of STATUS register
		clrf	STEP     ;Clear STEP
		movf	STEP,w   ;STEP---->w
		call	SEQUENCE ;Wondering what this does ? 

        movwf PORTB     ;w---->PORTB. Outputs whatever value was in w to PORTB pins.
		
        goto	 START

;**************************************************************************************
;*                                 LEFT SUBROUTINE                                    *
;**************************************************************************************
;This makes the motor turn lefht when RA0 is pulled down.(button pushed and kept down)*

LEFT	

        btfsc	LEFTBUTTON ;Test RA1, Goto start if it's 1.
		goto 	START
		decf	STEP,f  ;f-1---->f
		movlw	0x03    ;0x03--->w
		btfsc	STEP,7  ;Test STEP<7> , skip next instruction if clear.
		
        movwf	STEP    ;Why are we doing this ? movwf STEP then movf STEP, w. Killing time ?
 		movf	STEP,w  ;It's because movwf STEP gets executed only the first time
                        ;When STEP=0-1 (decf STEP) = 0xFF = 11111111

		call	SEQUENCE ;We call SEQUENCE, where we'll add the low 8 bits of the 
                         ;Program counter to W to do a computed goto.
        movwf PORTB
                         ;Loads PORTB with the value of w.
		
        goto	START

end	







;****************************************************************************
;                     LOG: Code Updates, Rambling & Rants                   *
;****************************************************************************
;
;
;Tuesday December 8th, 2011:
;I put the RIGHT subroutine as a comment, so that there's only the LEFT subroutine
;that will be executed.
;RA1 is pulled up to 5V by a 10K resistor, and as long as the push-button isn't pushed
;RA1 is logical 1, so the BTFSC PORTA,1
;                         GOTO  START
;It always hooks to START and starts all over again.
;Now We push the push-button (LEFT), we pull down RA1. And the program skips GOTO START
;because RA1 is 0. And the program does its stuff.
;PORTB = 00000101 , 00000110, 00001010, 00001001, then cycles again 00000101,. 

;Then I'll put the LEFT subroutine as a comment, and do the same thing to check it out.
;RIGHT and LEFT subroutine work.



;Friday December 9th, 2011:
;-Added some macros and got rid of 'hardcoded' literals to make the code
; more easily reusable elsewhere.
;-Changed the place of LOG to the end of file for clarity.
;-Defined RA0 as RIGHTBUTTON, and RA1 as LEFTBUTTON.


;Saturday December 10th, 2011
;Some bugs slipped in the code and went undetected.
;The code compiled successfully, but I didn't test it
;if it works. It didn't, but does now.

;-Bugs in v1.3 fixed in v1.4:
; -SEQUENCE was in the wrong place (before GOTO START)
; -mSelectBank1 macro was declared as such 
;  but invoked as mBankSelect1 and didn't work obviously.
; -mOPTION_REGConfigure was invoked as such, but the register
;  in the macro was CONFIG_REG, which doesn't exist as far
;  as I know.

;RULE1: -In macros, Always start with a VERB. This way, if invoking 
;        a macro doesn't start with a verb, I'll immediately know.(crossing fingers)
;        -Macros start with a lower case 'm'.

;Thursday December 13th, 2011: v1.5
; -Macros related to Bank Selection were put as comments, as there already
;  is an instruction to deal with that. "banksel". 
;  Thanks to Jon Wilder on ETO for bringing this to my attention.


;                              ********************************
;                              *      BANK RELATED MACROS     *
;                              ********************************
    
    ;mSelectBank0 macro
	;			 bcf STATUS,RP0
    ;             endm        

	;mSelectBank1 macro
    ;             bsf STATUS,RP0
    ;             endm

;******************************************
;* RP0 is the Bank Select Bit (in STATUS).*
;* We chose the Bank1 by setting it to 1. *
;* And chose Bank0 by setting RP0 to 0  . *
;******************************************











;*****************************************************************************
;*                          ORIGINAL CODE                                    *
;*****************************************************************************

;LIST p=16F84A

;#include p16F84A.inc
;;_CONFIG H'3FF9'
;__CONFIG _CP_OFF & _WDT_OFF & _PWRTE_ON & _RC_OSC

;Pas EQU 0x0C

;	org 0x00
;	bsf STATUS, RP0
;	clrf TRISB


;    movlw B'11111111'
;	 movwf TRISA
;		movlw B'10010110'
;		movwf OPTION_REG
;	bcf STATUS,RP0
;	clrf PORTB
;		clrf Pas
;		clrf TMR0
;		goto Debut

;Table	addwf PCL,f
;			    retlw B'00001001'
;			    retlw B'00001010'
;			    retlw B'00000110'
;               retlw B'00000101'

;Debut	btfss   TMR0,7
;				goto Debut
;				clrf TMR0

;Droite	btfsc	PORTA,0
;		goto 	Gauche
;		incf	Pas,f
;		movlw	0x04
;		subwf	Pas,w
;		btfsc	STATUS,C
;		clrf	Pas
;		movf	Pas,w
;		call	Table
;		movwf	PORTB
;		goto	Debut

;Gauche	btfsc	PORTA,1
;		goto 	Debut
;		decf	Pas,f
;		movlw	0x03
;		btfsc	Pas,7
;		movwf	Pas
;		movf	Pas,w
;		call	Table
;		movwf	PORTB
;		goto	Debut
;end
;****************************************************************************



