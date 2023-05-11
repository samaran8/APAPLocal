* @ValidationCode : MjotMTU2MDc1ODQ5NTpDcDEyNTI6MTY4MTIwMjM2NDg4MDpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 14:09:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE S.REDO.GET.EB.ERROR.TXT(P.IN.EB.ERROR.ID,P.IN.VARS,P.OUT.MSG)
*-----------------------------------------------------------------------------
*!** Simple SUBROUTINE template
* @author:    vpanchi@temenos.com
* @stereotype subroutine: Routine
* @package:   REDO.CCRG
*!
*-----------------------------------------------------------------------------
*  Routine that permits to show the error message to the user
*  This is used for batch process
*
*  Input Param:
*  ------------
*  P.IN.EB.ERROR.ID:
*     Error message code
*  P.IN.VARS
*     Variables
*
*  Output Param:
*  ------------
*  P.OUT.MSG:
*            User message
*-----------------------------------------------------------------------------
* Modification Details:
*=====================
*
* Date             Who                   Reference      Description
* 11.04.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 11.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*--------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB INITIALISE
    GOSUB CHECK.PRELIM.CONDITIONS

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

RETURN

*** <region name= INITIALISE>
*** <desc>Initialise the variables</desc>
****
INITIALISE:
    PROCESS.GOAHEAD = 1
    Y.MSG           = P.IN.EB.ERROR.ID
*
RETURN
*** </region>

*** <region name= PROCESS>
*** Process the message
PROCESS:
    CALL EB.GET.ERROR.MESSAGE(Y.MSG)
*P.OUT.MSG = Y.MSG<1,1>

* Assign the value to variable
    P.OUT.MSG = Y.MSG
    P.OUT.MSG<2> = P.IN.VARS
    CALL TXT(P.OUT.MSG)
*
RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= CHECK.PRELIM.CONDITIONS>
***
CHECK.PRELIM.CONDITIONS:
    IF NOT(P.IN.EB.ERROR.ID) THEN
        PROCESS.GOAHEAD = 0
    END
RETURN
*** </region>
END
