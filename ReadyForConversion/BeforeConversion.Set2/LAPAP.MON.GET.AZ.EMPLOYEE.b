*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.GET.AZ.EMPLOYEE

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.CUSTOMER


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
