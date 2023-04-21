SUBROUTINE LAPAP.GET.MON.AZ.AUTH.HIS

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT ;*R22 Auto conversion - END

    FN.AZ = "F.AZ.ACCOUNT$HIS"
    F.AZ = ""
    CALL OPF(FN.AZ,F.AZ)

    ACC = COMI
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    Y.ACC.ID = RES

    CALL F.READ.HISTORY(FN.AZ,Y.ACC.ID,R.HIS,F.AZ,ERRH)
    AUTH = R.HIS<AZ.AUTHORISER>

    REPLACED.AUTH = EREPLACE(AUTH, '_',@VM)
    AUTHORISER = REPLACED.AUTH<1,2>

    COMI = AUTHORISER
RETURN

END
