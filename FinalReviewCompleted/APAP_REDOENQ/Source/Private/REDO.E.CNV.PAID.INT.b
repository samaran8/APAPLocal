$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.CNV.PAID.INT
*----------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : NAVEENKUMAR N
* Program Name  : REDO.E.CNV.PAID.INT
*----------------------------------------------------------
* Description   : This subroutine is attached as a conversion routine in the Enquiry REDO.REINVESTED.ACCT.STMT
* Linked with   : Enquiry REDO.REINVESTED.ACCT.STMT as conversion routine
* In Parameter  : None
* Out Parameter : None
*-----------------------------------------------------------------------------
* Modification Details:
*=====================
* 24/08/2010 - ODR-2010-08-0192
*  DATE             WHO                   REFERENCE                  
* 11-APRIL-2023      Conversion Tool       R22 Auto Conversion  - ! to * and commented I_F.STMT.ENTRY
* 11-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
*------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.ACCOUNT
*   $INSERT I_F.STMT.ENTRY                   ;*R22 Auto Conversion  - commented I_F.STMT.ENTRY
    $INSERT I_F.REDO.TEMP.VERSION.IDS
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT.STATEMENT
*
    GOSUB INIT
    GOSUB PROCESS
RETURN
*****
INIT:
*****
* Initialising Necessary Variables
*
    FN.STMT.ENTRY  = "F.STMT.ENTRY"
    F.STMT.ENTRY   = ""
    R.STMT.ENTRY   = ""
    E.STMT.ENTRY   = ""
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.ACCOUNT.STATEMENT="F.ACCOUNT.STATEMENT"
    F.ACCOUNT.STATEMENT = ""
    R.ACCOUNT.STATEMENT = ""
    E.ACCOUNT.STATEMENT = ""
    CALL OPF(FN.ACCOUNT.STATEMENT,F.ACCOUNT.STATEMENT)

    FN.TELLER = "F.TELLER"
    F.TELLER = ""
    R.TELLER = ""
    E.TELLER= ""
    CALL OPF(FN.TELLER,F.TELLER)

*
    FN.REDO.TEMP.VERSION.IDS = "F.REDO.TEMP.VERSION.IDS"
    F.REDO.TEMP.VERSION.IDS = ""
    R.REDO.TEMP.VERSION.IDS = ""
    E.REDO.TEMP.VERSION.IDS = ""
    CALL OPF(FN.REDO.TEMP.VERSION.IDS,F.REDO.TEMP.VERSION.IDS)

    FN.FUNDS.TRANSFER = "F.FUNDS.TRANSFER"
    F.FUNDS.TRANSFER = ""
    R.FUNDS.TRANSFER = ""
    E.FUNDS.TRANSFER = ""
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FUNDS.TRANSFER.HIS = "F.FUNDS.TRANSFER$HIS"
    F.FUNDS.TRANSFER.HIS = ""
    R.FUNDS.TRANSFER.HIS = ""
    CALL OPF(FN.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS)

    FN.ACCOUNT     = "F.ACCOUNT"
    F.ACCOUNT      = ""
    R.ACCOUNT      = ""
    E.ACCOUNT      = ""
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ""
    R.AZ.ACCOUNT = ""
    E.AZ.ACCOUNT = ""
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
*
    FN.STMT.ENTRY  = "F.STMT.ENTRY"
    F.STMT.ENTRY   = ""
    R.STMT.ENTRY   = ""
    E.STMT.ENTRY   = ""
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)
*
    TOT.CR.AMOUNTS = ""
    Y.CR.AMOUNT    = ""
    TOT.CR.AMOUNT  = ""
    Y.ID.LIST      = ""
    Y.SE.ID.LIST   = ""
    Y.REV.AMOUNT = " "
    Y.ALL.IDS = ""

    LOC.REF.APPLICATION="ACCOUNT"
    LOC.REF.FIELDS='L.AC.AZ.ACC.REF'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AC.AZ.ACC.REF=LOC.REF.POS<1,1>
RETURN
********
PROCESS:
********
* Main process to fetch the necessary values
*

    Y.INTEREST.LIQ.ACCT = R.RECORD<AZ.INTEREST.LIQU.ACCT>
    CALL F.READ(FN.ACCOUNT,Y.INTEREST.LIQ.ACCT,R.ACCOUNT,F.ACCOUNT,E.ACCOUNT)
    Y.BASE.ACCOUNT = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.AZ.ACC.REF>
    CALL F.READ(FN.ACCOUNT.STATEMENT,Y.BASE.ACCOUNT,R.ACCOUNT.STATEMENT,F.ACCOUNT.STATEMENT,E.ACCOUNT.STATEMENT)
    Y.VALUE.DATE = R.ACCOUNT.STATEMENT<AC.STA.FQU1.LAST.DATE>
    IF NOT(Y.VALUE.DATE) THEN
        CALL F.READ(FN.AZ.ACCOUNT,Y.BASE.ACCOUNT,R.AZ.ACCOUNT,F.AZ.ACCOUNT,E.AZ.ACCOUNT)
        Y.VALUE.DATE =  R.AZ.ACCOUNT<AZ.VALUE.DATE>
    END
*
* Select on stmt entry upon Booking date, Account number, and Crf type and Looping to fetch the total TOT.CR.AMOUNTS
*
    ENQ.SEL = ENQ.SELECTION<2>
    CHANGE @VM TO @FM IN ENQ.SEL
*
    LOCATE "MATURITY.DATE" IN ENQ.SEL SETTING MATURITY.POS THEN
        Y.CLOSE.DATE = ENQ.SELECTION<4,MATURITY.POS>
    END


    SEL.IDS = "SELECT ":FN.REDO.TEMP.VERSION.IDS
    CALL EB.READLIST(SEL.IDS,Y.ID.LIST,'',NO.OF.REC.ARR,SEL.RET.ARR)
    LOOP
        REMOVE Y.ID FROM Y.ID.LIST SETTING ID.POS
    WHILE Y.ID:ID.POS
        CALL F.READ(FN.REDO.TEMP.VERSION.IDS,Y.ID,R.REDO.TEMP.VERSION.IDS,F.REDO.TEMP.VERSION.IDS,E.REDO.TEMP.VERSION.IDS)
        Y.ALL.IDS<-1> = R.REDO.TEMP.VERSION.IDS<REDO.TEM.AUT.TXN.ID>
        CHANGE @VM TO @FM IN Y.ALL.IDS
    REPEAT
*

    GOSUB E.STMT.ENQ.BY.CONCAT

    GOSUB LOOP.GENERATION

    TOT.CR.AMOUNTS = ABS(TOT.CR.AMOUNTS)
    Y.FINAL.AMOUNT = TOT.CR.AMOUNTS - Y.REV.AMOUNT
    O.DATA        = Y.FINAL.AMOUNT
RETURN


****************
E.STMT.ENQ.BY.CONCAT:
*********************

    IF Y.INTEREST.LIQ.ACCT THEN
        Y.SELECTION = ENQ.SELECTION
        D.FIELDS                = 'ACCOUNT':@FM:'BOOKING.DATE'
        IF Y.CLOSE.DATE THEN
            D.RANGE.AND.VALUE   = Y.INTEREST.LIQ.ACCT:@FM:Y.VALUE.DATE:@SM:Y.CLOSE.DATE
        END ELSE
            D.RANGE.AND.VALUE   = Y.INTEREST.LIQ.ACCT:@FM:Y.VALUE.DATE:@SM:TODAY
        END
        D.LOGICAL.OPERANDS      = '1':@FM:'2'
        CALL E.STMT.ENQ.BY.CONCAT(Y.ID.LIST)
        Y.SE.ID.LIST = FIELDS(Y.ID.LIST,'*',2,1)
    END
RETURN
****************
LOOP.GENERATION:
****************
    LOOP
        REMOVE Y.SINGLE FROM Y.SE.ID.LIST SETTING SINGLE.POS
    WHILE Y.SINGLE:SINGLE.POS
        CALL F.READ(FN.STMT.ENTRY,Y.SINGLE,R.STMT.ENTRY,F.STMT.ENTRY,E.STMT.ENTRY)
        Y.CRF.TYPE = R.STMT.ENTRY<AC.STE.CRF.TYPE>
        Y.CCY      = R.STMT.ENTRY<AC.STE.CURRENCY>
*

        IF Y.CCY EQ LCCY THEN
            Y.AMT = R.STMT.ENTRY<AC.STE.AMOUNT.LCY>
            IF Y.AMT LT 0 THEN
                TOT.CR.AMOUNTS += Y.AMT
            END
        END ELSE
            Y.AMT = R.STMT.ENTRY<AC.STE.AMOUNT.FCY>
            IF Y.AMT LT 0 THEN
                TOT.CR.AMOUNTS += Y.AMT
            END
        END

        Y.TRANS.REF= R.STMT.ENTRY<AC.STE.TRANS.REFERENCE>
        GOSUB CHECK.LOCAL.TABLE
    REPEAT
RETURN
************
CHECK.LOCAL.TABLE:
************
    LOCATE Y.TRANS.REF IN Y.ALL.IDS SETTING Y.ID.POS ELSE
        IF Y.TRANS.REF[1,2] EQ 'FT' THEN
            CALL F.READ(FN.FUNDS.TRANSFER,Y.TRANS.REF,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,E.FUNDS.TRANSFER)
            GOSUB CALC.REV.AMOUNT
        END
    END
RETURN
****************
CALC.REV.AMOUNT:
****************
    IF R.FUNDS.TRANSFER THEN
        IF R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT> THEN
            Y.REV.AMOUNT += R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT>
        END ELSE
            Y.REV.AMOUNT += R.FUNDS.TRANSFER<FT.CREDIT.AMOUNT>
        END
    END ELSE
        CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIS,Y.TRANS.REF,R.FUNDS.TRANSFER.HIS,HIS.FT.ERR)
        IF R.FUNDS.TRANSFER.HIS<FT.DEBIT.AMOUNT> THEN
            Y.REV.AMOUNT += R.FUNDS.TRANSFER.HIS<FT.DEBIT.AMOUNT>
        END ELSE
            Y.REV.AMOUNT += R.FUNDS.TRANSFER.HIS<FT.CREDIT.AMOUNT>
        END
    END
RETURN
END
