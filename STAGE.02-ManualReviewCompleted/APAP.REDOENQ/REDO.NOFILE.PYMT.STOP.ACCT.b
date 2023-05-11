$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.PYMT.STOP.ACCT(Y.ARRAY)
*********************************************************************************************************
* Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By      : Temenos Application Management
* Program   Name    : REDO.NOFILE.PYMT.STOP.ACCT
*--------------------------------------------------------------------------------------------------------
* Description       : This routine is used to increase the count based upon the user's input

* Linked With       :
* In  Parameter     : Y.FINAL.ARR
* Out Parameter     : Y.FINAL.ARR
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                  Description
*   ------             -----                 -----------                -----------
* 21-DEC-2010      JEYACHANDRAN S          ODR-2010-03-0159           Initial Creation
* 08-NOV-2012      MARIMUTHU S             PACS00232596
* 13-APRIL-2023      Harsha                R22 Auto Conversion  - FM to @FM and ++ to +=
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.PAYMENT.STOP.ACCOUNT
    $INSERT I_F.ALTERNATE.ACCOUNT

    GOSUB INIT
    GOSUB FORM.SELECT
    GOSUB PROCESS
    GOSUB GOEND

*---------
INIT:

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = '' ;Y.FALG = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.LOCKING='F.LOCKING'
    F.LOCKING=''
    CALL OPF(FN.LOCKING,F.LOCKING)

    FN.REDO.PAYMENT.STOP.ACCOUNT = 'F.REDO.PAYMENT.STOP.ACCOUNT'
    F.REDO.PAYMENT.STOP.ACCOUNT = ''
    CALL OPF(FN.REDO.PAYMENT.STOP.ACCOUNT,F.REDO.PAYMENT.STOP.ACCOUNT)

    FN.ALTERNATE.ACCOUNT = 'F.ALTERNATE.ACCOUNT'
    F.ALTERNATE.ACCOUNT = ''
    CALL OPF(FN.ALTERNATE.ACCOUNT,F.ALTERNATE.ACCOUNT)

RETURN

*-------------
FORM.SELECT:

    LOCATE "ACCOUNT.NUMBER" IN D.FIELDS<1> SETTING ACC.POS THEN
        Y.ACC.NUM =D.RANGE.AND.VALUE<ACC.POS>
        CALL F.READ(FN.ACCOUNT,Y.ACC.NUM,R.ACC,F.ACCOUNT,ACC.ERRR)
        IF NOT(R.ACC) THEN
            CALL F.READ(FN.ALTERNATE.ACCOUNT,Y.ACC.NUM,R.ALT.ACC,F.ALTERNATE.ACCOUNT,ALT.ACC.ERR)
            IF R.ALT.ACC THEN
                Y.ACC.NUM = R.ALT.ACC<AAC.GLOBUS.ACCT.NUMBER>
            END
        END
    END
    LOCATE "PAY.STOP.STATUS" IN D.FIELDS<1> SETTING PAY.POS THEN
        Y.SEL.STATUS = D.RANGE.AND.VALUE<PAY.POS>
    END
    LOCATE "STOP.DATE" IN D.FIELDS<1> SETTING STOP.POS THEN
        Y.SEL.STOP = D.RANGE.AND.VALUE<STOP.POS>
    END
    LOCATE "@ID" IN D.FIELDS<1> SETTING ID.POS THEN
        Y.SEL.ID = D.RANGE.AND.VALUE<ID.POS>
    END
    LOCATE "EXPIRY.DATE" IN D.FIELDS<1> SETTING EXP.POS THEN
        Y.SEL.EXP = D.RANGE.AND.VALUE<EXP.POS>
    END
    LOCATE "CO.CODE" IN D.FIELDS<1> SETTING CODE.POS THEN
        Y.SEL.CODE = D.RANGE.AND.VALUE<CODE.POS>
    END

RETURN
*----------
PROCESS:

    SEL.CMD = "SELECT ":FN.REDO.PAYMENT.STOP.ACCOUNT

    IF Y.ACC.NUM THEN
        SEL.CMD := " WITH ACCOUNT.NUMBER EQ ":Y.ACC.NUM
    END
    IF Y.SEL.STOP THEN
        SEL.CMD := " WITH STOP.DATE EQ ":Y.SEL.STOP
    END
    IF Y.SEL.CODE THEN
        SEL.CMD := " WITH CO.CODE EQ ":Y.SEL.CODE
    END
    IF Y.SEL.EXP THEN
        SEL.CMD := " WITH EXPIRY.DATE EQ ":Y.SEL.EXP
    END
    IF Y.SEL.STATUS THEN
        SEL.CMD := " WITH PAY.STOP.STATUS EQ '":Y.SEL.STATUS:"'"
    END
    IF Y.SEL.ID THEN
        SEL.CMD := " WITH @ID EQ ":Y.SEL.ID
    END

    SEL.CMD := " BY @ID"

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NOF,ERR)
    Y.LIST.CNT = DCOUNT(SEL.LIST,@FM)
    Y.LIST.INIT = 1
    LOOP
        REMOVE Y.ID FROM SEL.LIST SETTING POS
    WHILE Y.LIST.INIT LE Y.LIST.CNT
        R.REC = ''
        CALL F.READ(FN.REDO.PAYMENT.STOP.ACCOUNT,Y.ID,R.REC,F.REDO.PAYMENT.STOP.ACCOUNT,F.ERR)
        IF R.REC THEN
            Y.DUP.ID<1,-1>             = Y.ID
            Y.DUP.LIST<1,-1>           = R.REC<REDO.PS.ACCT.STOP.DATE,1>:Y.ID
        END
        Y.LIST.INIT += 1
    REPEAT
    GOSUB SORT.OUT.ARRAY
RETURN
*---------------
SORT.OUT.ARRAY:
    Y.SORT.ARR = SORT(Y.DUP.LIST)
    Y.DUP.CNT = DCOUNT(Y.SORT.ARR,@FM)
    Y.DUP.INIT = 1
    LOOP
        REMOVE VAR.ARR.ID FROM Y.SORT.ARR SETTING Y.ARR.POS
    WHILE Y.DUP.INIT LE Y.DUP.CNT
        Y.LEN = LEN(VAR.ARR.ID)
        Y.ARR.ID = VAR.ARR.ID[9,Y.LEN-9+1]
        CALL F.READ(FN.REDO.PAYMENT.STOP.ACCOUNT,Y.ARR.ID,R.REC,F.REDO.PAYMENT.STOP.ACCOUNT,F.ERR)
        Y.ACCT.NUM       =R.REC<REDO.PS.ACCT.ACCOUNT.NUMBER>
        Y.CHQ.NO1        = R.REC<REDO.PS.ACCT.CHEQUE.FIRST>
        Y.CHQ.NO2        = R.REC<REDO.PS.ACCT.CHEQUE.LAST>
        Y.ISSUE.DATE     = R.REC<REDO.PS.ACCT.ISSUE.DATE>
        Y.STOP.PYMT.DATE = R.REC<REDO.PS.ACCT.STOP.DATE>
        Y.AMT1           = R.REC<REDO.PS.ACCT.AMT.FROM>
        Y.AMT2           = R.REC<REDO.PS.ACCT.AMT.TO>
        Y.BENIFICIARY    = R.REC<REDO.PS.ACCT.BENIFICIARY>
        Y.STATUS         = R.REC<REDO.PS.ACCT.PAY.STOP.STATUS>
        Y.EXP.DATE       = R.REC<REDO.PS.ACCT.EXPIRY.DATE>
        Y.REASON         = R.REC<REDO.PS.ACCT.PAY.REASON>
        Y.LEAVES         = R.REC<REDO.PS.ACCT.NO.OF.LEAVES>
        Y.WAI.CHG        = R.REC<REDO.PS.ACCT.WAIVE.CHARGES>
        Y.CHG.AMT        = R.REC<REDO.PS.ACCT.CHARGE.AMOUNT>
        Y.REMARK         = R.REC<REDO.PS.ACCT.REMARKS>
        Y.INPUT          = R.REC<REDO.PS.ACCT.INPUTTER>
        Y.CO.CODE        = R.REC<REDO.PS.ACCT.CO.CODE>
        Y.AUTH           = R.REC<REDO.PS.ACCT.AUTHORISER>
        Y.USER2          = FIELD(Y.INPUT,'_',2)
        Y.USER3          = FIELD(Y.AUTH,'_',2)
        Y.OUT.ARRAY<-1>  = Y.ARR.ID:'*':Y.ACCT.NUM:'*':Y.CHQ.NO1:'*':Y.CHQ.NO2:'*':Y.LEAVES:'*':Y.ISSUE.DATE:'*':Y.STOP.PYMT.DATE:'*':Y.EXP.DATE:'*':Y.STATUS:'*':Y.AMT1:'*':Y.BENIFICIARY:'*':Y.REASON:'*':Y.WAI.CHG:'*':Y.CHG.AMT:'*':Y.REMARK:'*':Y.USER2:'*':Y.USER3:'*':Y.CO.CODE
        Y.DUP.INIT += 1
    REPEAT
    Y.ARRAY = Y.OUT.ARRAY
RETURN
*-------------

GOEND:
END
