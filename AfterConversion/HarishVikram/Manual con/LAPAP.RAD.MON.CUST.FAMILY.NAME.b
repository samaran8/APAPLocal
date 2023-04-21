SUBROUTINE LAPAP.RAD.MON.CUST.FAMILY.NAME
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

    Y.CUST.CODE = COMI
    CALL F.READ(FN.CUSTOMER, Y.CUST.CODE, R.CUSTOMER, F.CUSTOMER, ERR.CUS)

    IF NOT(ERR.CUS) THEN

        Y.FAMILY.NAME = ''

        IF R.CUSTOMER<EB.CUS.FAMILY.NAME> THEN
            Y.FAMILY.NAME = R.CUSTOMER<EB.CUS.FAMILY.NAME>
        END ELSE
            Y.FAMILY.NAME = R.CUSTOMER<EB.CUS.NAME.2>
        END

        COMI = Y.FAMILY.NAME
    END

RETURN
END
