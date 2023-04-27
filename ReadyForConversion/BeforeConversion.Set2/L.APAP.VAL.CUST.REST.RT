*-----------------------------------------------------------------------------
* <Rating>-3</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.CUST.REST.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    FN.AC = "F.CUSTOMER"
    F.AC = ""

    P.CUSTOMER.ID = COMI
*CALL F.READ(FN.AC, P.ACCOUNT.ID, R.AC, F.AC, '')

*CALL GET.LOC.REF("ACCOUNT", "L.AC.STATUS",AC.POS)
*Y.ACCOUNT.STATUS = R.AC<AC.LOCAL.REF,AC.POS>
    T.CANTIDAD.CARACTERES = LEN(P.CUSTOMER.ID)
    IF (T.CANTIDAD.CARACTERES LE 3) THEN

        TEXT = "CODIGO CLIENTE INVALIDO "
        E = TEXT
    END
    RETURN
END
