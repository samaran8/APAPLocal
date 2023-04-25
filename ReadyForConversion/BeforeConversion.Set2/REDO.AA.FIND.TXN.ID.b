*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------

    SUBROUTINE REDO.AA.FIND.TXN.ID

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER

    GOSUB PRODUCT.VALIDATION
    GOSUB MAIN.PROCESS
    RETURN
*
************************************
PRODUCT.VALIDATION:
*
************************************

    FT.VALID = '' ; FT.INSTALLED = '' ; COMP.FT = '' ; ERR.MSG = ''
    CALL EB.VAL.PRODUCT('FT',FT.VALID,FT.INSTALLED,COMP.FT,ERR.MSG)

    TT.VALID = '' ; TT.INSTALLED = '' ; COMP.TT = '' ; ERR.MSG = ''
    CALL EB.VAL.PRODUCT('TT',TT.VALID,TT.INSTALLED,COMP.TT,ERR.MSG)

    RETURN
*
************************************
MAIN.PROCESS:
*
************************************

    TRANSACTION.REFERENCE = TRIM(O.DATA)

    BEGIN CASE
    CASE TRANSACTION.REFERENCE[1,2] EQ "FT" AND FT.VALID AND FT.INSTALLED AND COMP.FT
        IF ENQ.SELECTION<1> EQ "AA.DETAILS.ACTIVITY.LOG.PENDING.FIN" THEN
            FN.FUNDS.TRANSFER = "F.FUNDS.TRANSFER$NAU"
        END ELSE
            IF ENQ.SELECTION<1> EQ "L.APAP.AA.DETAILS.ACTIVITY.LOG.FIN" THEN
                FN.FUNDS.TRANSFER = "F.FUNDS.TRANSFER"
            END
        END
        F.FUNDS.TRANSFER = ""
        CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
        CALL F.READ(FN.FUNDS.TRANSFER,TRANSACTION.REFERENCE,R.FT,F.FUNDS.TRANSFER,FT.ERR)
        IF R.FT<FT.TFS.REFERENCE> THEN
            O.DATA = FIELD(R.FT<FT.TFS.REFERENCE>,'-',1)
        END ELSE
            O.DATA = ""
        END
    CASE TRANSACTION.REFERENCE[1,2] EQ "TT" AND TT.VALID AND TT.INSTALLED AND COMP.TT
        IF ENQ.SELECTION<1> EQ "AA.DETAILS.ACTIVITY.LOG.PENDING.FIN" THEN
            FN.TELLER = "F.TELLER$NAU"
        END ELSE
            IF ENQ.SELECTION<1> EQ "L.APAP.AA.DETAILS.ACTIVITY.LOG.FIN" THEN
                FN.TELLER = "F.TELLER"
            END
        END
        CALL OPF(FN.TELLER,F.TELLER)
        CALL F.READ(FN.TELLER,TRANSACTION.REFERENCE,R.TT,F.TELLER,TT.ERR)
        IF R.TT<TT.TE.TFS.REFERENCE> THEN
            O.DATA = FIELD(R.TT<TT.TE.TFS.REFERENCE>,'-',1)
        END ELSE
            O.DATA = ""
        END
    CASE TRANSACTION.REFERENCE[1,3] EQ "AAA"
        O.DATA = TRANSACTION.REFERENCE
    CASE 1
        O.DATA = ""
    END CASE
*
************************************
    RETURN
*
************************************
END
