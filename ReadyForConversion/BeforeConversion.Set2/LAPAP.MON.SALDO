*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.SALDO

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT

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
