SUBROUTINE LAPAP.MON.AZ.GET.PROMOTOR

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP removed in INSERT file

*-----------------------------------------------------------------------------
    $INSERT I_COMMON  ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - END


    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    ID = COMI

    CALL F.READ(FN.ACC,ID,R.ACC,F.ACC,ERRCC)
    PROMOTOR = R.ACC<AC.ACCOUNT.OFFICER>

    COMI = PROMOTOR

RETURN

END
