* @ValidationCode : MjotMjEwNjI2NTgyMDpDcDEyNTI6MTY4MTM3MzIzMTg5NTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:37:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.CONV.UPLOAD.ID
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This is used as Convertion  routine in EB.FILE.UPLOAD enuriy  to form the ID
*-----------------------------------------------------------------------------------------------------
* * Input / Output
*--------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : Sakthi Sellappillai
* PROGRAM NAME : REDO.E.CONV.UPLOAD.ID
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE            WHO                     REFERENCE             DESCRIPTION
*=======          ==============          ==================    ===============
* 07-10-2010      Sakthi Sellappillai     ODR-2010-08-0031      INITIAL CREATION

* 13-APR-2023     Conversion tool    R22 Auto conversion       if condition added
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
* ----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
    GOSUB INIT
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------------------------------
    Y.CUSTOMER.VAL = ''
    Y.DATE.TIME.VAL = ''
    Y.TEMP.VAL  = O.DATA
RETURN
*-----------------------------------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------------------------------
    Y.CUSTOMER.VAL = System.getVariable('EXT.SMS.CUSTOMERS')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - start
        Y.CUSTOMER.VAL = ""
    END					;*R22 Auto conversion- end
    CALL ALLOCATE.UNIQUE.TIME(UNIQUE.TIME)
    Y.TIME.VALUE = UNIQUE.TIME
    Y.DATE.VALUE = TODAY
    Y.UPLOAD.ID = Y.CUSTOMER.VAL:".":Y.DATE.VALUE:".":Y.TIME.VALUE
    O.DATA = Y.UPLOAD.ID
RETURN
*----------------------------------------------------------------------------------------------
END
