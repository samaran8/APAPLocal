* @ValidationCode : MjotMzMwOTY5MzE3OkNwMTI1MjoxNjgwNjAzODI5NjgwOklUU1M6LTE6LTE6LTI2OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 15:53:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : -26
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.METH.PAGO.VAL
*=============================================================================
* Subroutine Type :  VALIDATION ROUTINE
* Attached to     :  REDO.FC.BH.VALIDATIONS ROUTINE THAT COLLED VALIDATIONS TO
*                    REDO.CREATE.ARRANGEMENT
* Attached as     :  ROUTINE
* Primary Purpose :  VALIDATE IF THE USER CHOOSE "NOMINA EXTERNA" IN PAYMENT
*                    METHOD, THEN IS MANDATORY TO INPUT "NOMINA EMPRESA" IN
*                    FORM FIELD
*
* Incoming:
* ---------
*
* Outgoing:
* ---------
*
*-----------------------------------------------------------------------------
* Modification History:
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Bryan Torres (btorresalbornoz@temenos.com) - RTAM
* Date            : 25.08.2011
* Modified by     : Luis Fernando Pazmino (lpazminodiaz@temenos.com) - RTAM
* Date            : 19.12.2011
* Notes           : Minor fixes for code review. MV control for charges
* Modified by     : Jorge Valarezo (jvalarezoulloa@temenos.com) - RTAM
* Date            : 02.01.2011
* Notes           : Change the value to compare according to content of LOCAL.TABLE. 304
*  DATE             WHO                   REFERENCE                  
* 04-APRIL-2023      Harsha                R22 Auto Conversion  - VM to @VM and = to EQ
* 04-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*=============================================================================

******************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.REDO.CREATE.ARRANGEMENT
******************************************************************************

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

*************
INIT:
* Initialise
*************
RETURN

*************
OPEN.FILES:
* Open Files
*************
RETURN

***************
PROCESS:
* Main process
***************
    Y.METH.PAGO = R.NEW(REDO.FC.PAYMT.MHD)
*Change the Value in according to content LOCAL.FIELD 304
    IF Y.METH.PAGO EQ "External Payroll" AND R.NEW(REDO.FC.FORM) EQ "" THEN
        AF = REDO.FC.FORM
        TEXT = 'REDO-FC-METH-PAY'
        M.CONT = DCOUNT(R.NEW(REDO.FC.OVERRIDE),@VM) + 1
        CALL STORE.OVERRIDE(M.CONT)
    END

    Y.CHARGE.AMOUNTS = R.NEW(REDO.FC.CHARG.AMOUNT)
    Y.CHARGE.IDS     = R.NEW(REDO.FC.CHARG.DISC)

* Validate user input
    IF Y.CHARGE.AMOUNTS THEN
        Y.CHARGE.NUM = DCOUNT(Y.CHARGE.AMOUNTS,@VM)
        FOR Y.CHARGE = 1 TO Y.CHARGE.NUM
            Y.CHARGE.AMOUNT = FIELD(Y.CHARGE.AMOUNTS,@VM,Y.CHARGE)
            Y.CHARGE.ID     = FIELD(Y.CHARGE.IDS,@VM,Y.CHARGE)
            IF Y.CHARGE.AMOUNT NE '' AND Y.CHARGE.ID EQ '' THEN
                AF = REDO.FC.CHARG.DISC
                AV = Y.CHARGE
                ETEXT = "EB-REDO-FC-METH.PAY"
                CALL STORE.END.ERROR
            END
        NEXT Y.CHARGE
    END

RETURN

END
