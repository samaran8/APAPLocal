* @ValidationCode : Mjo4OTA4MTcyNjQ6Q3AxMjUyOjE2ODExMDkzMzY3ODM6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 12:18:56
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE AI.REDO.GET.CURRENT.VAR
*-----------------------------------------------------------------------------
*Company   Name    : APAP
*Developed By      : Martin Macias
*Program   Name    : AI.REDO.GET.CURRENT.VAR
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 10-APR-2023     Conversion tool    R22 Auto conversion       IF condition added
* 10-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System

    CURRENT.VAR = O.DATA
    CURRENT.VALUE = System.getVariable(CURRENT.VAR)
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - start
        CURRENT.VALUE = ""
    END					;*R22 Auto conversion - end
    O.DATA = CURRENT.VALUE

RETURN
END
