* @ValidationCode : MjotMTMyMzI5NTk2NTpDcDEyNTI6MTY4MTM2MzY0NDIyNzpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 10:57:24
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
$PACKAGE APAP.REDOEB
SUBROUTINE MB.E.SDB.REFUND.VAT
* Routine will calculate the refund amount and the VAT on it
* To be used in ENQUIRY
*

*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 13-APR-2023     Conversion tool    R22 Auto conversion      ++ to +=
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*
    REFUND.AMT = 0
    LOOP
        REMOVE REF.AMT FROM O.DATA SETTING REF.POS
    WHILE REF.AMT:REF.POS
        REFUND.AMT += REF.AMT
    REPEAT
    IF REFUND.AMT LT '0' THEN
        REFUND.AMT = REFUND.AMT * -1
    END
    VAT.AMT = REFUND.AMT * (17.5/100)
    O.DATA = REFUND.AMT:'*':VAT.AMT
RETURN
END
