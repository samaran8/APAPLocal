SUBROUTINE LAPAP.MON.DEFINE.PAYMENT(ID,RS,RT)

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File 

*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT ;*R22 Auto conversion - END

*----------------
*Opening tables
*---------------
    FN.AZ.HIS = "F.AZ.ACCOUNT$HIS"
    F.AZ.HIS = ""
    CALL OPF(FN.AZ.HIS,F.AZ.HIS)

    FN.AZ = "F.AZ.ACCOUNT"
    F.AZ = ""
    CALL OPF(FN.AZ,F.AZ)

*---------------
*ACC validation
*---------------
    ACC = ID
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    Y.ACC.ID = RES

*---------------
*Getting result
*---------------

    IF ACC NE Y.ACC.ID THEN

        CALL F.READ.HISTORY(FN.AZ.HIS,Y.ACC.ID,R.HIS,F.AZ.HIS,ERR1)
        CALL GET.LOC.REF("AZ.ACCOUNT","L.AZ.METHOD.PAY",POS)
        CALL GET.LOC.REF("AZ.ACCOUNT","L.AZ.AMOUNT",POSS)
        TYPE = R.HIS<AZ.LOCAL.REF,POS>
        AMOUNT = R.HIS<AZ.LOCAL.REF,POSS>

        IF TYPE EQ '' THEN
            TYPE = ""
            AMOUNT = ""
        END

    END ELSE

        CALL F.READ.HISTORY(FN.AZ.HIS,ACC,R.AZ,F.AZ.HIS,ERR)

        IF ERR THEN
            ACC = ACC[1,10]
            CALL F.READ(FN.AZ,ACC,R.AZ,F.AZ,ERRZ)
        END

        CALL GET.LOC.REF("AZ.ACCOUNT","L.AZ.METHOD.PAY",POS)
        CALL GET.LOC.REF("AZ.ACCOUNT","L.AZ.AMOUNT",POSS)
        TYPE = R.AZ<AZ.LOCAL.REF,POS>
        AMOUNT = R.AZ<AZ.LOCAL.REF,POSS>

        IF TYPE EQ '' THEN
            TYPE = ""
            AMOUNT = ""
        END

    END

    RS = TYPE
    RT = AMOUNT

*DEBUG

END
