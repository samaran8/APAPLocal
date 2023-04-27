*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.AC.ACCOUNT.SWEEP

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.AC.ACCOUNT.LINK

*DEBUG

    FN.AC = "F.ACCOUNT"
    F.AC = ""
    F.AC2 = ""
    F.CUST.ID.AC1 = ""
    F.CUST.ID.AC2 = ""
    F.ACC1 = R.NEW(AC.LINK.ACCOUNT.TO)
    F.ACC2 = R.NEW(AC.LINK.ACCOUNT.FROM)
    CALL F.READ(FN.AC, F.ACC1 , R.AC, F.AC, '')
    CALL F.READ(FN.AC, F.ACC2 , R.AC2, F.AC2, '')

    F.CUST.ID.AC1 = R.AC<AC.CUSTOMER>
    F.CUST.ID.AC2 = R.AC2<AC.CUSTOMER>

*DEBUG

    IF F.ACC1 EQ F.ACC2 THEN

        TEXT = "AMBAS CUENTAS SON IGUALES"
        CALL REM
        ETEXT = TEXT
        PRINT E
        CALL STORE.END.ERROR

    END

    IF F.CUST.ID.AC1 NE F.CUST.ID.AC2 THEN

        TEXT = "CUENTA INVALIDA, NO PERTENECE AL MISMO CLIENTE"
        CALL REM
        ETEXT = TEXT
        PRINT E
        CALL STORE.END.ERROR
    END
    RETURN
END
