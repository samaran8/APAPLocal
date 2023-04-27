*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.CLOS.OPENDATE

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.ACCOUNT.CLOSURE

    FN.ACC = "F.ACCOUNT$HIS"
    FV.ACC = ""
    CALL OPF(FN.ACC,FV.ACC)

    FN.ACL = "F.ACCOUNT"
    F.ACL = ""
    CALL OPF(FN.ACL,F.ACL)

    ACC = COMI
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    Y.ACC.ID = RES



    IF ACC NE Y.ACC.ID THEN

        CALL EB.READ.HISTORY.REC(FV.ACC,Y.ACC.ID,R.ACL,ACC.ERROR)
        OPEND = R.ACL<AC.OPENING.DATE>
        COMI = OPEND

    END ELSE

        CALL EB.READ.HISTORY.REC(FV.ACC,Y.ACC.ID,R.ACC,ACC.ERROR)
        OPEND = R.ACC<AC.OPENING.DATE>
        COMI = OPEND

    END

    RETURN

END
