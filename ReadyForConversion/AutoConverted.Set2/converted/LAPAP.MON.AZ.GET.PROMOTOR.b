SUBROUTINE LAPAP.MON.AZ.GET.PROMOTOR
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT


    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    ID = COMI

    CALL F.READ(FN.ACC,ID,R.ACC,F.ACC,ERRCC)
    PROMOTOR = R.ACC<AC.ACCOUNT.OFFICER>

    COMI = PROMOTOR

RETURN

END