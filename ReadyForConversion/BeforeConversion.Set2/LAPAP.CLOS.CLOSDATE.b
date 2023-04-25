*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CLOS.CLOSDATE

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.ACCOUNT.CLOSURE

    FN.ACC = "F.ACCOUNT$HIS"
    FV.ACC = ""
    CALL OPF(FN.ACC,FV.ACC)

    ACC = COMI

    CALL EB.READ.HISTORY.REC(FV.ACC,ACC,R.ACC,ACC.ERROR)
    OPEND = R.ACC<AC.CLOSURE.DATE>
    COMI = OPEND

    RETURN

END
