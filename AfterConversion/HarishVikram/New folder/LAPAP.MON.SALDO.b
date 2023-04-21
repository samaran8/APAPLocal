SUBROUTINE LAPAP.MON.SALDO

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - END

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    ID = COMI
    CALL F.READ(FN.ACC,ID,R.ACC,F.ACC,ERRCC)
    CALL GET.LOC.REF("ACCOUNT","L.AC.AV.BAL",POS)
    SALDO = R.ACC<AC.LOCAL.REF,POS>
    COMI = SALDO

RETURN

END
