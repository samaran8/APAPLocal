*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.RTN.VAL.PROSPECTO
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER

    Y.CUS.NO = COMI

    IF Y.CUS.NO NE '' THEN
        FN.CUS = "F.CUSTOMER"
        FV.CUS = ""
        CALL OPF(FN.CUS, FV.CUS)

        R.CUS = ""
        CUS.ERR = ""

        CALL F.READ(FN.CUS,Y.CUS.NO,R.CUS, FV.CUS, CUS.ERR)

        Y.GET.CUS = R.CUS<EB.CUS.MNEMONIC>

        IF Y.GET.CUS NE '' THEN
            E = "EL CLIENTE YA EXISTE"
        END

    END

    RETURN
END
