*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.AZ.PAYMET.METHOD2

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AZ.ACCOUNT


    ID = COMI
    CALL LAPAP.MON.DEFINE.PAYMENT(ID,RS,RT)
    IF RS EQ "CHEQUE.DEPOSIT" THEN
        COMI = RT
    END ELSE
        COMI = ""
    END

END
