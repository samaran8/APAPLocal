SUBROUTINE LAPAP.RAD.MON.CUST.NAME
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

        Y.NAME = ''

        IF R.CUSTOMER<EB.CUS.GIVEN.NAMES> THEN
            Y.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>
        END ELSE
            Y.NAME = R.CUSTOMER<EB.CUS.NAME.1>
        END

        COMI = Y.NAME
    END

RETURN
END
