*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.LIMITE.CURRENCY.PARAM
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.LIMITE.CURRENCY.PARAM
* Date           : 2019-1017
* Item ID        :
*========================================================================
* Brief description :
* -------------------
* type : ID ROUTINE
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-05-04     Richard HC                Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :
* Auto Increment :
* Views/versions :
* EB record      :
* Routine        :
*========================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CURRENCY


    FN.CURR = "F.CURRENCY"
    F.CURR = ""
    CALL OPF(FN.CURR,F.CURR)

    ID = V$DISPLAY

    CALL CACHE.READ(FN.CURR, ID, R.CURR, ERR)

    IF ERR THEN
        E = 'INGRESE UN TIPO DE MONEDA VALIDO  - Ejemplo: [ USD | EUR | JPY |ETC... ]'
    END

*   CALL System.setVariable('CURRENT.OLD.RECORD.ID',ID)

RETURN


END
