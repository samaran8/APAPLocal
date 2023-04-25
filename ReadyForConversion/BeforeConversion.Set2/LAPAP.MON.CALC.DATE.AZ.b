*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.CALC.DATE.AZ

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AZ.ACCOUNT

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
