*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.GET.MON.AZ.AUTH

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.AZ.ACCOUNT

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
