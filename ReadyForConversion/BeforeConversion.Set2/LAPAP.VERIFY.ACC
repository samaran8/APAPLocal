*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.VERIFY.ACC(ACC,RES)

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT

    AC = ACC
    FN.ACC = "F.ACCOUNT$HIS"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    CALL F.READ.HISTORY(FN.ACC,AC,R.ACC,F.ACC,ERR);
    CALL GET.LOC.REF("ACCOUNT","L.AC.AZ.ACC.REF",POS);
    AZ.ACC.REF = R.ACC<AC.LOCAL.REF,POS>
    CATEGORY = R.ACC<AC.CATEGORY>

    IF CATEGORY GE 6010 AND CATEGORY LE 6020 THEN
        RES = AZ.ACC.REF
    END ELSE
        RES = AC[1,10]
    END

    RETURN

END
