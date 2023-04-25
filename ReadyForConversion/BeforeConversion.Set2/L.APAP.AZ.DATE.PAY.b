*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.AZ.DATE.PAY

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT


    VAR.VALUE.DATE = R.NEW(AZ.VALUE.DATE)[7,2]

        CALL GET.LOC.REF("AZ.ACCOUNT","PAYMENT.DATE",ACC.POS)

        R.NEW(AZ.LOCAL.REF)<1,ACC.POS> = VAR.VALUE.DATE

END
