SUBROUTINE LAPAP.MON.SALD.HIS

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - END

    FN.ACC = "F.ACCOUNT$HIS"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    ACC = COMI
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    Y.ACC.ID = RES

    CALL F.READ.HISTORY(FN.ACC,Y.ACC.ID,R.HIS,F.ACC,ERRH)
    CALL GET.LOC.REF("ACCOUNT","L.AC.AV.BAL",POS)
    SALDO = R.HIS<AC.LOCAL.REF,POS>
    COMI = SALDO

RETURN

END
