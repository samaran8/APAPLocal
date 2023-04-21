SUBROUTINE LAPAP.GET.MON.AZ.AUTH

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT ;*R22 Auto conversion - END

    FN.AZ = "F.AZ.ACCOUNT"
    F.AZ = ""
    CALL OPF(FN.AZ,F.AZ)

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    ID = COMI

    CALL F.READ(FN.AZ,ID,R.AZ,F.AZ,ERRZ)
    AUTH = R.AZ<AZ.AUTHORISER>

    IF ERRZ THEN

        CALL F.READ(FN.ACC,ID,R.ACC,F.ACC,ERRC)
        AUTH = R.ACC<AC.AUTHORISER>
        REPLACED.AUTH = EREPLACE(AUTH, '_',@VM)
        AUTHORISER = REPLACED.AUTH<1,2>

        COMI = AUTHORISER

    END ELSE
        REPLACED.AUTH = EREPLACE(AUTH, '_',@VM)
        AUTHORISER = REPLACED.AUTH<1,2>

        COMI = AUTHORISER

    END
END
