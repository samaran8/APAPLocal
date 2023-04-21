*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.GET.FMT.PASSPORT(NO.PASSPORT, NO.FMT.PASSPORT)

    $INSERT I_COMMON
    $INSERT I_EQUATE
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.GET.FORMATED.PASSPORT
* Date           : 2018-05-23
* Item ID        : CN008754
*========================================================================
* Brief description :
* -------------------
* This routine retorned the formated passport (COUNTRY CODE + PASSPORT NUMBER)
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-05-23     Anthony Martinez  Initial Development
* 
* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes
*========================================================================

    GOSUB PROCESS

PROCESS:
*-------
    NO.FMT.PASSPORT = SUBSTRINGS(NO.PASSPORT, LEN(NO.PASSPORT)-1, 2) : SUBSTRINGS(NO.PASSPORT, 0, LEN(NO.PASSPORT)-3)

RETURN
*-------
