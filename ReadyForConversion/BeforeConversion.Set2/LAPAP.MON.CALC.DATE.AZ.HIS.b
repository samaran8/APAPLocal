*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.CALC.DATE.AZ.HIS

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.AZ.ACCOUNT

    FN.AZ = "F.AZ.ACCOUNT$HIS"
    F.AZ = ""
    CALL OPF(FN.AZ,F.AZ)

    ACC = COMI
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    Y.ACC.ID = RES

    CALL F.READ.HISTORY(FN.AZ,Y.ACC.ID,R.HIS,F.AZ,ERRH)
    START.DATE = R.HIS<AZ.VALUE.DATE>
    END.DATE = R.HIS<AZ.MATURITY.DATE>

    IF ERRH EQ '' THEN
        CALL CDD("",START.DATE,END.DATE,NO.OF.DAYS)
        COMI = NO.OF.DAYS
    END ELSE
        COMI = ""
    END

    RETURN
END
