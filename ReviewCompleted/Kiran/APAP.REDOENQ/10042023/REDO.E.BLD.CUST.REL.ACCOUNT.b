$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.BLD.CUST.REL.ACCOUNT(ENQ.DATA)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 10-APRIL-2023      Conversion Tool       R22 Auto Conversion - FM to @FM and SM to @SM
* 10-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ENQUIRY
    GOSUB PROCESS
    GOSUB PGM.END
RETURN
*------
PROCESS:
*-------
    FN.JOINT.CONTRACTS.XREF='F.JOINT.CONTRACTS.XREF'
    F.JOINT.CONTRACTS.XREF=''
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)

    FN.CUSTOMER.ACCOUNT='F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT=''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

    LOCATE 'CUSTOMER' IN ENQ.DATA<2,1> SETTING POS1 THEN
        LOCATE '@ID' IN ENQ.DATA<2,1> SETTING POS2 THEN
            GOSUB PGM.END
        END
        Y.CUST = ENQ.DATA<4,POS1>
        CALL F.READ(FN.JOINT.CONTRACTS.XREF,Y.CUST,R.JOINT.CUST,F.JOINT.CONTRACTS.XREF,CUST.ERR)
        CALL F.READ(FN.CUSTOMER.ACCOUNT,Y.CUST,R.CUST,F.CUSTOMER.ACCOUNT,CUST.ACC.ERR)
        IF R.JOINT.CUST THEN
            IF R.CUST THEN
                Y.CUST.ACCTS = R.CUST:@FM:R.JOINT.CUST
            END ELSE
                Y.CUST.ACCTS = R.JOINT.CUST
            END
        END ELSE
            Y.CUST.ACCTS = R.CUST
        END
        IF Y.CUST.ACCTS THEN
            Y.CUST.ACCTS = CHANGE(Y.CUST.ACCTS,@FM,@SM)
            ENQ.DATA<2,POS1> = "@ID"
            ENQ.DATA<3,POS1> = "EQ"
            ENQ.DATA<4,POS1> = Y.CUST.ACCTS
        END

    END
RETURN
*-------
PGM.END:
*-------
END
