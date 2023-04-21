SUBROUTINE LAPAP.RAD.MON.CUSTID.OPER

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File, INCLUDE to INSERT

*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER ;*R22 Auto conversion - END

    FN.CUSTOMER = 'F.CUSTOMER'; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    Y.INIT = ''
    Y.CUST.CODE = COMI
    CALL F.READ(FN.CUSTOMER, Y.CUST.CODE, R.CUSTOMER, F.CUSTOMER, ERR.CUS)

    IF NOT(ERR.CUS) THEN

        CUS.NATION = R.CUSTOMER<EB.CUS.NATIONALITY>

        CALL DR.REG.GET.CUST.TYPE(R.CUSTOMER, OUT.ARR)
        Y.CUS.ID = EREPLACE(OUT.ARR<2>, "-", "")

        CALL LAPAP.GET.IDENTIFICATION.TYPE(R.CUSTOMER, OUT.ARR2)
        Y.ID.TYPE = OUT.ARR2

        IF Y.ID.TYPE EQ 'PAS' AND CUS.NATION NE '' THEN
            Y.INIT = 1 + LEN(CUS.NATION)
            Y.CUS.ID = Y.CUS.ID[Y.INIT,LEN(Y.CUS.ID)]
        END

        COMI = Y.CUS.ID : '@' : Y.ID.TYPE
    END

RETURN
END
