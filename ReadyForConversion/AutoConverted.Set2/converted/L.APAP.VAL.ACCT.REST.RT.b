SUBROUTINE L.APAP.VAL.ACCT.REST.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    FN.AC = "F.ACCOUNT"
    F.AC = ""

    P.ACCOUNT.ID = COMI
*CALL F.READ(FN.AC, P.ACCOUNT.ID, R.AC, F.AC, '')

*CALL GET.LOC.REF("ACCOUNT", "L.AC.STATUS",AC.POS)
*Y.ACCOUNT.STATUS = R.AC<AC.LOCAL.REF,AC.POS>
    T.CANTIDAD.CARACTERES = LEN(P.ACCOUNT.ID )
    IF (T.CANTIDAD.CARACTERES NE 10) THEN

        TEXT = "NUMERO CUENTA INVALIDO "
        E = TEXT
    END
RETURN
END
