SUBROUTINE L.APAP.CLOS.CUSTOMER

    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ACCOUNT.CLOSURE

    FN.ACC = "F.ACCOUNT$HIS"
    FV.ACC = ""

    Y.ACC.ID = COMI

    CALL OPF(FN.ACC,FV.ACC)
    CALL EB.READ.HISTORY.REC(FV.ACC,Y.ACC.ID,R.ACC,ACC.ERROR)

    CUSTOMER.ID = R.ACC<AC.CUSTOMER>

    COMI = CUSTOMER.ID
