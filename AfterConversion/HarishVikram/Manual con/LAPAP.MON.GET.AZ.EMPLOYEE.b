SUBROUTINE LAPAP.MON.GET.AZ.EMPLOYEE

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------


    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER ;*R22 Auto conversion - END


    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    FN.ACC.HIS = "F.ACCOUNT$HIS"
    F.ACC.HIS = ""
    CALL OPF(FN.ACC.HIS,F.ACC.HIS)

    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
    CALL OPF(FN.CUS,F.CUS)

    ID = COMI

    CALL F.READ(FN.ACC,ID,R.ACC,F.ACC,ERRCC)
    CUSTOMER  = R.ACC<AC.CUSTOMER>

    IF ERRCC THEN

        CALL F.READ.HISTORY(FN.ACC.HIS,ID,R.ACC.HIS,F.ACC.HIS,ERRH)
        CUSTOMER  = R.ACC.HIS<AC.CUSTOMER>

        CALL F.READ(FN.CUS,CUSTOMER,R.CUS,F.CUS,ERRCUS)
        FAX = R.CUS<EB.CUS.FAX.1>

        IF FAX NE '' THEN
            COMI = "S"
        END ELSE
            COMI = "N"
        END

    END ELSE

        CALL F.READ(FN.CUS,CUSTOMER,R.CUS,F.CUS,ERRCUS)
        FAX = R.CUS<EB.CUS.FAX.1>

        IF FAX NE '' THEN
            COMI = "S"
        END ELSE
            COMI = "N"
        END
    END

RETURN



END
