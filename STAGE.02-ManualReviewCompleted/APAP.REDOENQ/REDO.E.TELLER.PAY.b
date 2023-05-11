$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.TELLER.PAY(Y.LIST)
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This is a no file enquiry routine for the enquiry REDO.E.TELLER.PAY
*--------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : TXN.ARRAY
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : PRABHU N
* PROGRAM NAME : REDO.E.TELLER.PAY
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                REFERENCE         DESCRIPTION
*28.06.2010      PRABHU           ODR-2010-01-0081    INITIAL CREATION
* 12-APRIL-2023      Conversion Tool       R22 Auto Conversion - FM to @FM and SM to @SM
* 12-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
* -----------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER
    GOSUB INIT
    GOSUB PROCESS
RETURN
INIT:
******

    FN.TELLER='F.TELLER'
    F.TELLER =''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.TELLER.HIS='F.TELLER$HIS'
    F.TELLER.HIS=''
    CALL OPF(FN.TELLER.HIS,F.TELLER.HIS)
RETURN

PROCESS:
*********

    LOCATE "TELLER.ID" IN D.FIELDS<1> SETTING TILL.POS THEN
        VAR.SEL.ID=D.RANGE.AND.VALUE<TILL.POS>
    END
    LOCATE "FROM.DATE" IN D.FIELDS<1> SETTING FROM.POS THEN
        VAR.FROM.DATE=D.RANGE.AND.VALUE<FROM.POS>
    END
    LOCATE "TO.DATE" IN D.FIELDS<1> SETTING TO.POS THEN
        VAR.TO.DATE=D.RANGE.AND.VALUE<TO.POS>
    END
    LOCATE "CO.CODE" IN D.FIELDS<1> SETTING CO.POS THEN
        VAR.COMPANY=D.RANGE.AND.VALUE<TO.POS>
    END
    LOCATE "TRANSACTION.CODE" IN D.FIELDS<1> SETTING VAR.TRANS.POS THEN
        VAR.TRANSACTION.LIST=D.RANGE.AND.VALUE<VAR.TRANS.POS>
        CHANGE @SM TO @FM IN VAR.TRANSACTION.LIST
    END

    IF VAR.FROM.DATE LT TODAY THEN
        SEL.TELLER.CMD    ='SELECT ':FN.TELLER.HIS:' WITH VALUE.DATE.1 GE ':VAR.FROM.DATE :' AND LE ':VAR.TO.DATE
        CALL  EB.READLIST(SEL.TELLER.CMD,SEL.TELLER.LIST,'',TOT.COUNT,ERR)
        IF VAR.SEL.ID NE '' THEN
            GOSUB HIS.PROCESS
        END
        ELSE
            GOSUB HIS.COMP.PROCESS
        END
    END
    IF VAR.TO.DATE EQ TODAY THEN
        SEL.TELLER.CMD    ='SELECT ':FN.TELLER
        CALL  EB.READLIST(SEL.TELLER.CMD,SEL.TELLER.LIST,'',TOT.COUNT,ERR)
        IF VAR.SEL.ID NE '' THEN
            GOSUB LIVE.PROCESS
        END
        ELSE
            GOSUB LIVE.COMP.PROCESS
        END
    END
RETURN
*-----------
LIVE.PROCESS:
*------------
    FOR LIVE.CNT=1 TO TOT.COUNT
        VAR.TXN.ID=SEL.TELLER.LIST<LIVE.CNT>
        CALL F.READ(FN.TELLER,VAR.TXN.ID,R.TELLER,F.TELLER,ERR)
        LOCATE R.TELLER<TT.TE.TRANSACTION.CODE> IN VAR.TRANSACTION.LIST SETTING POS THEN
            VAR.TELLER.ID=R.TELLER<TT.TE.TELLER.ID.1>
            IF VAR.TELLER.ID EQ VAR.SEL.ID THEN
                VAR.AMOUNT=R.TELLER<TT.TE.AMOUNT.LOCAL.1>
                VAR.CREDIT.ACCOUNT=R.TELLER<TT.TE.ACCOUNT.2>
                Y.LIST<-1>=VAR.TXN.ID:'*':VAR.CREDIT.ACCOUNT:'*':VAR.AMOUNT
            END

        END
    NEXT LIVE.CNT
RETURN
*-----------
HIS.PROCESS:
*-----------
    FOR HIS.CNT=1 TO TOT.COUNT
        VAR.TXN.ID=SEL.TELLER.LIST<HIS.CNT>
        VAR.TXN.ID=FIELD(VAR.TXN.ID, ";", 1)
        CALL EB.READ.HISTORY.REC(F.TELLER.HIS,VAR.TXN.ID,R.TELLER.HIS,ERR)
        LOCATE R.TELLER.HIS<TT.TE.TRANSACTION.CODE> IN VAR.TRANSACTION.LIST SETTING POS THEN
            VAR.TELLER.ID=R.TELLER.HIS<TT.TE.TELLER.ID.1>
            IF VAR.TELLER.ID EQ VAR.SEL.ID THEN
                VAR.AMOUNT=R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>
                VAR.CREDIT.ACCOUNT=R.TELLER.HIS<TT.TE.ACCOUNT.2>
                VAR.TXN.ID=FIELD(VAR.TXN.ID, ";", 1)
                Y.LIST<-1>=VAR.TXN.ID:'*':VAR.CREDIT.ACCOUNT:'*':VAR.AMOUNT
            END
        END
    NEXT HIS.CNT
RETURN
*----------------
LIVE.COMP.PROCESS:
*----------------
    FOR LIVE.CNT=1 TO TOT.COUNT
        VAR.TXN.ID=SEL.TELLER.LIST<LIVE.CNT>
        CALL F.READ(FN.TELLER,VAR.TXN.ID,R.TELLER,F.TELLER,ERR)
        LOCATE R.TELLER<TT.TE.TRANSACTION.CODE> IN VAR.TRANSACTION.LIST SETTING POS THEN
            VAR.COMP.ID=R.TELLER<TT.TE.CO.CODE>
            IF VAR.COMP.ID  EQ VAR.COMPANY THEN
                VAR.AMOUNT=R.TELLER<TT.TE.AMOUNT.LOCAL.1>
                VAR.CREDIT.ACCOUNT=R.TELLER<TT.TE.ACCOUNT.2>
                Y.LIST<-1>=VAR.TXN.ID:'*':VAR.CREDIT.ACCOUNT:'*':VAR.AMOUNT
            END

        END
    NEXT LIVE.CNT
RETURN
*----------------
HIS.COMP.PROCESS:
*----------------
    FOR HIS.CNT=1 TO TOT.COUNT
        VAR.TXN.ID=SEL.TELLER.LIST<HIS.CNT>
        VAR.TXN.ID=FIELD(VAR.TXN.ID, ";", 1)
        CALL EB.READ.HISTORY.REC(F.TELLER.HIS,VAR.TXN.ID,R.TELLER.HIS,ERR)
        LOCATE R.TELLER.HIS<TT.TE.TRANSACTION.CODE> IN VAR.TRANSACTION.LIST SETTING POS THEN
            VAR.COMP.ID=R.TELLER.HIS<TT.TE.CO.CODE>
            IF VAR.COMP.ID EQ VAR.COMPANY THEN
                VAR.AMOUNT=R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>
                VAR.CREDIT.ACCOUNT=R.TELLER.HIS<TT.TE.ACCOUNT.2>
                VAR.TXN.ID=FIELD(VAR.TXN.ID, ";", 1)
                Y.LIST<-1>=VAR.TXN.ID:'*':VAR.CREDIT.ACCOUNT:'*':VAR.AMOUNT
            END
        END
    NEXT HIS.CNT
RETURN
END
