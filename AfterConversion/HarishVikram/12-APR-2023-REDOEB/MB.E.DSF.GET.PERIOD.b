* @ValidationCode : MjoxMTg5OTM5MjMzOkNwMTI1MjoxNjgxMjg3ODY1NjIwOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 13:54:25
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
SUBROUTINE MB.E.DSF.GET.PERIOD(Y.DATA)
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 12-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 12-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.MB.SDB.POST

    IF R.NEW(SDB.POST.OFFER.EXPIRY.DATE) GT TODAY THEN
        FROM.DATE = TODAY
    END ELSE
        FROM.DATE = R.NEW(SDB.POST.OFFER.EXPIRY.DATE)
    END

    TO.DATE = R.NEW(SDB.POST.RENEW.FREQUENCY)[1,8]
    Y.DATA = OCONV(ICONV(FROM.DATE,"D"), "D4-E"):" To ":OCONV(ICONV(TO.DATE,"D4"), "D4")

RETURN

END
