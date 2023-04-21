SUBROUTINE LAPAP.MON.CALC.DATE.AZ

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT ;*R22 Auto conversion - END

    FN.AZ = "F.AZ.ACCOUNT"
    F.AZ = ""
    CALL OPF(FN.AZ,F.AZ)

    ID = COMI

    CALL F.READ(FN.AZ,ID,R.AZ,F.AZ,ERRZ)
    START.DATE = R.AZ<AZ.VALUE.DATE>
    END.DATE = R.AZ<AZ.MATURITY.DATE>

    IF ERRZ EQ '' THEN
        CALL CDD("",START.DATE,END.DATE,NO.OF.DAYS)
        COMI = NO.OF.DAYS
    END ELSE
        COMI = ""
    END

RETURN

END
