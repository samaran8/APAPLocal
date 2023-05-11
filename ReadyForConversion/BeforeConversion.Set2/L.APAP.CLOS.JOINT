*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
 SUBROUTINE L.APAP.CLOS.JOINT

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.ACCOUNT.CLOSURE

    FN.ACC = "F.ACCOUNT$HIS"
    FV.ACC = ""

    Y.ACC.ID = COMI

    CALL OPF(FN.ACC,FV.ACC)
    CALL EB.READ.HISTORY.REC(FV.ACC,Y.ACC.ID,R.ACC,ACC.ERROR)

    CUSTOMER.ID = R.ACC<AC.JOINT.HOLDER>

    COMI = CUSTOMER.ID
