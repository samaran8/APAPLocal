*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.ACC.STA.A.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    FN.AC = "F.ACCOUNT"
    F.AC = ""

    P.ACCOUNT.ID = COMI
   **DEBUG
    CALL F.READ(FN.AC, P.ACCOUNT.ID, R.AC, F.AC, '')

    CALL GET.LOC.REF("ACCOUNT", "L.AC.STATUS",AC.POS)
    Y.ACCOUNT.STATUS = R.AC<AC.LOCAL.REF,AC.POS>
    IF (Y.ACCOUNT.STATUS NE "IM") THEN
       ** DEBUG
        TEXT = "CUENTA INVALIDA, CODIGO ESTATUS DIFERENTE (IM) : " : Y.ACCOUNT.STATUS
        **CALL REM
        **ETEXT = TEXT
        E = TEXT
       ** CALL ERR
        **CALL STORE.END.ERROR
    END
    RETURN
END
