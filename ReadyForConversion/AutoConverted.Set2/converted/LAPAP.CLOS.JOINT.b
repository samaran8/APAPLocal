SUBROUTINE LAPAP.CLOS.JOINT

    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ACCOUNT.CLOSURE

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

        CALL F.READ(FN.ACL,Y.ACC.ID,R.ACL,F.ACL,ERR.ACL)
        CUSTOMER.ID = R.ACL<AC.JOINT.HOLDER,1>
        COMI = CUSTOMER.ID

    END ELSE

        CALL F.READ(FN.ACL,ACC,R.ACL,F.ACL,ERR.ACL)
        CUSTOMER.ID = R.ACL<AC.JOINT.HOLDER,1>
        COMI = CUSTOMER.ID

*CALL EB.READ.HISTORY.REC(FV.ACC,ACC.ID,R.ACC,ACC.ERROR2)
*CUSTOMER.ID = R.ACC<AC.JOINT.HOLDER,1>
*CRT CUSTOMER.ID ;* COMI = CUSTOMER.ID

    END

RETURN

END
