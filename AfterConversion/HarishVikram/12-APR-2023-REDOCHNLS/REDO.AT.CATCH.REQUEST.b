* @ValidationCode : MjoxMTkzNjQ2NjAzOkNwMTI1MjoxNjgxMjc2MzU5NTkyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:42:39
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
SUBROUTINE REDO.AT.CATCH.REQUEST(ACTUAL.REQUEST)

*--------------------------------------------
* By JP
*-------------------------------------------
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 11-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 12-APR-2023      Harishvikram C   Manual R22 conversion     CALL routine format modified
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    AUXREQUEST = ACTUAL.REQUEST
    USERINFO = FIELD(AUXREQUEST,',',3)

    PASSUSER = FIELD(USERINFO,'/',2)
    SIGNONUSER = FIELD(USERINFO,'/',1)

    NEWPASSUSER = SIGNONUSER
    CALL APAP.SRTN.REDO.S.GET.PASS(NEWPASSUSER);*Manual R22 conversion

    IF NEWPASSUSER THEN
        CHANGE PASSUSER TO NEWPASSUSER IN AUXREQUEST
        ACTUAL.REQUEST = AUXREQUEST
    END


RETURN
END
