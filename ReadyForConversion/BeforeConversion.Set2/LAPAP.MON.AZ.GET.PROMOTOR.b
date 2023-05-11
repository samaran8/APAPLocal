*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.AZ.GET.PROMOTOR
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT


    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    ID = COMI

    CALL F.READ(FN.ACC,ID,R.ACC,F.ACC,ERRCC)
    PROMOTOR = R.ACC<AC.ACCOUNT.OFFICER>

    COMI = PROMOTOR

    RETURN

END
