* @ValidationCode : Mjo3Mzg2MDYwOTE6Q3AxMjUyOjE2ODI1Nzg2MDI4MjU6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Apr 2023 12:26:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.INW.PROCESS(SEL.LIST)
*-----------------------------------------------------------------------------
* Description:
* This routine is a multithreaded routine to select the records in the mentioned applns
*------------------------------------------------------------------------------------------
* * Input / Output
*
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : ganesh r
* PROGRAM NAME : REDO.B.INW.PROCESS
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE         DESCRIPTION
* 21.09.2010  ganesh r            ODR-2010-09-0148  INITIAL CREATION
* 30.08.2011  KAVITHA             PACS00112979      PACS00112979 FIX
* 03.10.2011  JEEVA T             PACS00131732      FIX FOR PACS00131732
* 18.10.2011  KAVITHA             PACS00146623      PACS00146623  FIX
* Date                   who                   Reference
* 11-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND SM TO @SM AND VM TO @VM AND CONVERT TO CHANGE
* 11-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION - CALL RTN METHOD ADDED

*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.LIMIT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.COLLATERAL
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CHEQUE.REGISTER
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_REDO.B.INW.PROCESS.COMMON
    $INSERT I_F.CERTIFIED.CHEQUE.PARAMETER
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
    $INSERT I_F.REDO.ADMIN.CHQ.DETAILS
    $INSERT I_F.REDO.MANAGER.CHQ.PARAM
    $INSERT I_F.REDO.MANAGER.CHQ.DETAILS
    $INSERT I_F.CERTIFIED.CHEQUE.STOCK
    $INSERT I_F.CERTIFIED.CHEQUE.DETAILS ;*Tus Start
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.CHEQUE.REGISTER.SUPPLEMENT ; *Tus End
    $INSERT I_F.CHEQUE.TYPE.ACCOUNT
    $USING APAP.REDOAPAP

    GOSUB INIT
    GOSUB PROCESS
RETURN

*Getting the Local field position
INIT:

    VAR.LOCK.AMT = 0
    VAL.TOT.LOCK.AMT = 0
    R.REDO.APAP.CLEARING.INWARD = ''
    Y.PAID.FLAG = ''
    REFER.FLAG = ''
    REFER.PAID = ''
    VAL.ACCT = ''
    VAL.TAX.AMT = ''
    VAR.PRD.CATEG = ''
    ADMIN.FLAG = ''

    VAL.IMG.ID = ''
    VAL.AMT = ''
    VAR.ACCT.OFF = ''
    VAR.CUSTOMER = ''
    VAR.REF.CHG = ''
    Y.HIS.FLAG = ''
    VAR.CURRENCY = ''
    VAL.TOT.CHG.AMT = ''
    R.ACCOUNT = ''
    VAR.REASON = ''
    RETURN.FLAG = ''
    VAR.REC.STATUS = ''
    VAL.NOR.TAX = ''
    INVALID.ACCT.FLAG = ''
    R.REDO.ADMIN.CHQ.DETAILS = ''
    R.CERTIFIED.CHEQUE.STOCK = ''
    R.CERTIFIED.CHEQUE.DETAILS = ''
    ECB.ERR=''
    CH.STOPPED.ERR=''
    NO.ACC = ''

RETURN

PROCESS:
*Getting the category codes

    REDO.ACCT.ID = SEL.LIST
    INVALID.ACCT.FLAG = ''

    GOSUB GET.DETAILS
    GOSUB CHECK.ACCOUNT3
    IF INVALID.ACCT.FLAG NE "Y" THEN
        GOSUB CALC.AMOUNT
        GOSUB GET.CHG.AMOUNT
        GOSUB CHECK.ACCOUNT1
        GOSUB CHECK.ACCOUNT2
        GOSUB CHECK.ACCOUNT4

        IF VAR.CUSTOMER EQ '' THEN
            GOSUB CHECK.AMOUNT
        END
    END

    GOSUB UPDATE.INWARD
    GOSUB GENERATE.ENTRIES

RETURN
*-------------------------------------------------------------------------------------
GET.DETAILS:
*Get the Details from the record
    CALL F.READU(FN.REDO.APAP.CLEARING.INWARD,REDO.ACCT.ID,R.REDO.APAP.CLEARING.INWARD,F.REDO.APAP.CLEARING.INWARD,CLEAR.INW.ERR,'')
    VAL.AMT = R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.AMOUNT>
    VAL.NOR.TAX = TRIMB(FMT((VAL.TAX * VAL.AMT)/100,'L2,#19'))
    VAL.TAX.AMT = R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.TAX.AMOUNT>
    VAL.ACCT = R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.ACCOUNT.NO>
    VAL.CHEQUE.NUMBER = R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.CHEQUE.NO>
    VAL.CHEQUE.NUMBER = TRIM(VAL.CHEQUE.NUMBER,"0", "L")
    VAL.IMG.ID = R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.IMAGE.REFERENCE>
RETURN

CALC.AMOUNT:
*Check for lower Amount and process

    VAL.TOT.AMT = VAL.TAX.AMT
    LOCATE VAL.ACCT IN VAR.ACCOUNT SETTING ACCT.POS THEN
        VAR.GET.AMT = VAR.AMOUNT<ACCT.POS>
        VAL.TOT.AMT = VAL.TAX.AMT + VAR.GET.AMT
        GOSUB CHECK.LOW.AMOUNT
        IF REFER.FLAG NE 1 THEN
            VAR.AMOUNT<ACCT.POS> = VAL.TOT.AMT
        END
    END
    ELSE
        GOSUB CHECK.LOW.AMOUNT
        IF REFER.FLAG NE 1 THEN
            VAR.ACCOUNT<-1> = VAL.ACCT
            VAR.AMOUNT<-1> = VAL.TOT.AMT
        END
    END
RETURN

CHECK.LOW.AMOUNT:
*Checks for available Amount

    VAR.LIMIT.REF      = R.ACCOUNT<AC.LIMIT.REF>
    VAR.ACCT.OFF       = R.ACCOUNT<AC.ACCOUNT.OFFICER>
*VAR.WORK.BAL       = R.ACCOUNT<AC.WORKING.BALANCE> ;*Tus Start
    VAR.WORK.BAL       = R.ECB<ECB.WORKING.BALANCE> ;*Tus End
    VAR.LOCK.AMT       = R.ACCOUNT<AC.LOCKED.AMOUNT>
    VAR.PRD.CATEG      = R.ACCOUNT<AC.CATEGORY>
    VAR.CURRENCY       = R.ACCOUNT<AC.CURRENCY>
    VAL.AC.STATUS1     = R.ACCOUNT<AC.LOCAL.REF><1,L.AC.STATUS1.POS>
    VAL.AC.STATUS2     = R.ACCOUNT<AC.LOCAL.REF><1,L.AC.STATUS2.POS>
    VAL.AC.NOTIFY      = R.ACCOUNT<AC.LOCAL.REF><1,L.AC.NOTIFY.POS>
    Y.TRANS.AVAIL.AMT  = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.TRAN.AVAIL.POS>
    Y.TRANS.LIMIT.AMT  = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.TRANS.LIM.POS>
    Y.AVAILABLE.BAL    = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.AV.BAL.POS>
    Y.LIMIT.REF        = R.ACCOUNT<AC.LIMIT.REF>
    Y.CUSTOMER         = R.ACCOUNT<AC.CUSTOMER>
*Y.WORKING.BAL      = R.ACCOUNT<AC.WORKING.BALANCE> ;*Tus Start
    Y.WORKING.BAL      = R.ECB<ECB.WORKING.BALANCE> ;*Tus End
    VAR.CUSTOMER       = R.ACCOUNT<AC.CUSTOMER>

    CALL F.READ(FN.CUSTOMER,VAR.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUST.ERR)
    VAR.SEGMENTO       = R.CUSTOMER<EB.CUS.LOCAL.REF><1,L.SEGMENTO.POS>

    IF Y.LIMIT.REF NE '' THEN
        Y.LIMIT.REF    = FMT(Y.LIMIT.REF,'R%10')
        LIMIT.ID       = Y.CUSTOMER : '.' : Y.LIMIT.REF
        CALL F.READ(FN.LIMIT,LIMIT.ID,R.LIMIT,F.LIMIT,LIMIT.ERR)
        IF R.LIMIT THEN
            Y.LIMIT = R.LIMIT<LI.AVAIL.AMT>
        END
    END ELSE
        Y.LIMIT = 0
    END

    Y.L.AC.AVAIL.BAL  = Y.AVAILABLE.BAL
    Y.TRANSIT.BAL     = Y.TRANS.AVAIL.AMT
    Y.TRANSIT.LIMIT   = Y.TRANS.LIMIT.AMT
    Y.OD.LIMIT        = 0       ;* OD Limit
    Y.TRANSIT.BAL.USE = 0
    Y.CHEQUE.AMT      = VAL.AMT

    IF Y.TRANSIT.BAL GT Y.TRANSIT.LIMIT THEN        ;* Avail is greator than limit. so limit amount needs to taken.
        Y.TRANSIT.BAL.USE = Y.TRANSIT.LIMIT
    END ELSE
        Y.TRANSIT.BAL.USE = Y.TRANSIT.BAL
    END


    IF (Y.L.AC.AVAIL.BAL+Y.TRANSIT.BAL.USE+Y.OD.LIMIT+Y.LIMIT) LT Y.CHEQUE.AMT THEN
        IF (Y.L.AC.AVAIL.BAL+Y.TRANSIT.BAL+Y.OD.LIMIT+Y.LIMIT) GE Y.CHEQUE.AMT THEN
            REFER.FLAG     = 1
            VAR.REASON<-1> = 18
            Y.PAID.FLAG    = ''
        END ELSE
            REFER.FLAG     = 1
            VAR.REASON<-1> = 17
            Y.PAID.FLAG    = ''
        END
    END

RETURN

CHECK.ACCOUNT1:


    HIS.REC.STATUS = R.ACCOUNT.HIS<AC.RECORD.STATUS>
    Y.HIS.FLAG = ''
    IF HIS.REC.STATUS EQ 'CLOSED' THEN
        RETURN.FLAG = 1
        VAR.REASON<-1> = 20
        Y.HIS.FLAG = '1'
    END
    SEL.CHQ.CMD = "SELECT ":FN.CHEQUE.REGISTER :" WITH @ID LIKE ...":VAL.ACCT
    CALL EB.READLIST(SEL.CHQ.CMD,CHQ.LIST,'',NO.OF.REC,CHQ.ERR)
    IF NO.OF.REC EQ 0 THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 23
    END
    LOOP
        REMOVE CHQ.ID FROM CHQ.LIST SETTING CHQ.POS
    WHILE CHQ.ID:CHQ.POS
        CALL F.READ(FN.CHEQUE.REGISTER,CHQ.ID,R.CHEQUE.REGISTER,F.CHEQUE.REGISTER,CHQ.ERR)
        VAL.CHEQ.NO = R.CHEQUE.REGISTER<CHEQUE.REG.CHEQUE.NOS>
        STRT.LIM = FIELD(VAL.CHEQ.NO,'-',1)
        END.LIM = FIELD(VAL.CHEQ.NO,'-',2)
        IF VAL.CHEQUE.NUMBER LT STRT.LIM OR VAL.CHEQUE.NUMBER GT END.LIM THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 23
        END
    REPEAT
RETURN

CHECK.ACCOUNT2:
    Y.ABS.CHEQUE.NUMBER = ABS(VAL.CHEQUE.NUMBER)
*VAR.CHEQUE.STOP.ID = VAL.ACCT:'*':Y.ABS.CHEQUE.NUMBER
*CALL F.READ(FN.CHEQUE.STOPPED,VAR.CHEQUE.STOP.ID,R.CHEQUE.STOPPED,F.CHEQUE.STOPPED,CHEQUE.ERR) ;*Tus Start
    CALL F.READ(FN.CHEQUE.TYPE.ACCOUNT,VAL.ACCT,R.CHEQUE.TYPE.ACCOUNT,F.CHEQUE.TYPE.ACCOUNT,CHEQUE.ERR)
    CHQ.TYPE= R.CHEQUE.TYPE.ACCOUNT<CHQ.TYP.CHEQUE.TYPE,1>
    VAR.CHEQUE.STOP.ID= CHQ.TYPE:'.':VAL.ACCT:'.':Y.ABS.CHEQUE.NUMBER
    CALL F.READ(FN.CHEQUE.REGISTER.SUPPLEMENT,VAR.CHEQUE.STOP.ID,R.CHEQUE.REGISTER.SUPPLEMENT,F.CHEQUE.REGISTER.SUPPLEMENT,CH.STOPPED.ERR)
    CHQ.STATUS= R.CHEQUE.REGISTER.SUPPLEMENT<CC.CRS.STATUS>
    IF CHQ.STATUS EQ 'STOPPED' THEN ;* Tus End
*IF R.CHEQUE.STOPPED THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 19
    END
    IF CHQ.STATUS EQ 'PRESENTED' THEN ;*Tus Start
*SEL.PRE.CHQ = "SELECT ":FN.CHEQUES.PRESENTED :" WITH @ID LIKE ...":VAL.ACCT:'-':VAL.CHEQUE.NUMBER
        SEL.PRE.CHQ = "SELECT ":FN.CHEQUE.REGISTER.SUPPLEMENT :" WITH @ID LIKE ...":VAL.ACCT:'.':VAL.CHEQUE.NUMBER ;*Tus End
        CALL EB.READLIST(SEL.PRE.CHQ,PRE.CHQ.LIST,'',PRE.OF.REC,PRE.CHQ.ERR)
        IF PRE.OF.REC GT 0 THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 14
        END
    END
RETURN


CHECK.ACCOUNT3:

    HIS.ACCT.ID = VAL.ACCT
    CALL EB.READ.HISTORY.REC(F.ACCOUNT.HIS,HIS.ACCT.ID,R.ACCOUNT.HIS,ACCT.HIS.ERR)

    CALL F.READ(FN.ACCOUNT.NAU,VAL.ACCT,R.ACCOUNT.NAU,F.ACCOUNT.NAU,ACCT.NAU.ERR)

    CALL F.READ(FN.ACCOUNT,VAL.ACCT,R.ACCOUNT,F.ACCOUNT,ACCT.ERR) ;*Tus Start
    R.ECB='' ; ECB.ERR=''
    CALL EB.READ.HVT("EB.CONTRACT.BALANCES",VAL.ACCT,R.ECB,ECB.ERR) ;*Tus End
    IF R.ACCOUNT.NAU EQ '' AND R.ACCOUNT EQ '' AND R.ACCOUNT.HIS EQ '' THEN
        NO.ACC = 1
    END

*IF R.ACCOUNT EQ '' THEN
*NO.ACC = 1
*END

*PACS00146623 -S
    IF NO.ACC EQ 1 THEN
*REFER.FLAG = 1   ;* Changed for KB PACS00243453
        RETURN.FLAG = 1
        VAR.REASON<-1> = 21
        INVALID.ACCT.FLAG = "Y"
    END ELSE

        VAL.POSTING.REST = R.ACCOUNT<AC.POSTING.RESTRICT>
        IF VAL.POSTING.REST EQ 90 THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 22
        END
        IF VAL.POSTING.REST EQ 1 THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 22
        END
        IF VAL.POSTING.REST EQ 2 THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 22
        END
        IF VAL.POSTING.REST EQ 3 THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 22
        END


    END

*PACS00146623 -E
RETURN

CHECK.ACCOUNT4:
    CHANGE @SM TO @FM IN VAL.AC.STATUS2

*VAR.ONLINE.ACT.BAL = R.ACCOUNT<AC.ONLINE.ACTUAL.BAL> ;*Tus Start
    VAR.ONLINE.ACT.BAL = R.ECB<ECB.ONLINE.ACTUAL.BAL>
*VAR.ONLINE.CLR.BAL = R.ACCOUNT<AC.ONLINE.CLEARED.BAL>
    VAR.ONLINE.CLR.BAL = R.ECB<ECB.ONLINE.CLEARED.BAL> ;*TUs End
    DIFF.ONLINE.BAL = VAR.ONLINE.ACT.BAL - VAR.ONLINE.CLR.BAL

    IF VAL.AC.STATUS1 EQ '6MINACTIVE' THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 25
    END

    IF VAL.AC.STATUS1 EQ '3YINACTIVE' THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 26
    END

    IF VAL.AC.STATUS1 EQ 'ABANDONED' THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 27
    END

    LOCATE 'DECEASED' IN VAL.AC.STATUS2 SETTING STATUS2.POS THEN
        RETURN.FLAG = 1
        VAR.REASON<-1> = 28
    END

    LOCATE 'GARNISHMENT' IN VAL.AC.STATUS2 SETTING STATUS2.POS THEN
        IF NOT(Y.PAID.FLAG) THEN
            REFER.FLAG = 1
            VAR.REASON<-1> = 29
        END
    END

    IF VAL.AC.NOTIFY EQ 'NOTIFY.OFFICER' THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 31
    END
    IF VAL.AC.NOTIFY EQ 'NOTIFY.MGMT.MONEY.LAUNDRY.PREV' THEN
        REFER.FLAG = 1
        VAR.REASON<-1> = 32
    END
*IF DIFF.ONLINE.BAL GT 0 THEN ;* Local fields used to check the transit funds
*REFER.FLAG = 1
*VAR.REASON<-1> = 18
*END
    IF VAR.CUSTOMER THEN
        SEL.COL.CMD = "SELECT ":FN.COLLATERAL :" WITH @ID LIKE ...":VAR.CUSTOMER:"..."
        CALL EB.READLIST(SEL.COL.CMD,SEL.COL.LIST,'',NO.COL.REC,SEL.COL.ERR)

        LOOP
            REMOVE COL.ID FROM SEL.COL.LIST SETTING COL.POS
        WHILE COL.ID:COL.POS
            CALL F.READ(FN.COLLATERAL,COL.ID,R.COLLATERAL,F.COLLATERAL,COL.ERR)
            Y.COLL.APPLICATION.ID = R.COLLATERAL<COLL.APPLICATION.ID>
            IF Y.COLL.APPLICATION.ID EQ VAL.ACCT THEN
                RETURN.FLAG = 1
                VAR.REASON<-1> = 30
            END
        REPEAT
    END
RETURN

CHECK.AMOUNT:
*Check for Cheque Amount with Cheque Number

    GOSUB ADMIN.CHECK
    IF NOT(Y.ADMIN.FLAG) THEN
        GOSUB CERT.CHECK
    END
RETURN

ADMIN.CHECK:

*Y.ADMIN.REF.STATUS = 'PAID':VM:'CANCELLED':VM:'STOP.PAID.CNFRM':VM:'STOP.PAID.NON.CNFRM':VM:'STOP.PAID':VM:'REVERSED'
    Y.ADMIN.FLAG = ''
    Y.ADMIN.ACCTS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
    CHANGE @VM TO @FM IN Y.ADMIN.ACCTS
    LOCATE VAL.ACCT IN Y.ADMIN.ACCTS SETTING ADM.ACT.POS THEN
        CALL F.READ(FN.REDO.ADMIN.CHQ.DETAILS,VAL.CHEQUE.NUMBER,R.REDO.ADMIN.CHQ.DETAILS,F.REDO.ADMIN.CHQ.DETAILS,CHQ.DET.ERR)
        Y.ADMIN.FLAG = 1
    END ELSE
        REFER.FLAG = 1
        ADMIN.FLAG = 1
    END
    IF R.REDO.ADMIN.CHQ.DETAILS THEN
        ADMIN.CHQ.AMT = R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.AMOUNT>
        ADMIN.CHQ.STATUS = R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>
        IF VAL.AMT NE ADMIN.CHQ.AMT THEN
            REFER.FLAG = 1
            ADMIN.FLAG = 1
        END ELSE
            REFER.FLAG = ''
            VAR.REASON = ''
        END
*IF ADMIN.CHQ.STATUS EQ 'PAID' OR ADMIN.CHQ.STATUS EQ 'CANCELLED' OR ADMIN.CHQ.STATUS EQ 'STOP.PAID.CNFRM' OR ADMIN.CHQ.STATUS EQ 'STOP.PAID.NON.CNFRM' OR ADMIN.CHQ.STATUS EQ 'STOP.PAID' THEN
*IF ADMIN.CHQ.STATUS MATCHES Y.ADMIN.REF.STATUS THEN
        IF ADMIN.CHQ.STATUS NE 'ISSUED' THEN
            REFER.FLAG = 1
            ADMIN.FLAG = 1
            IF ADMIN.CHQ.STATUS EQ 'PAID' THEN
                REFER.PAID = 'YES'
            END

        END
    END ELSE
        REFER.FLAG =1
        ADMIN.FLAG = 1
    END


RETURN

CERT.CHECK:

    Y.CERT.ACCOUNTS = R.CERTIFIED.CHEQUE.PARAMETER<CERT.CHEQ.ACCOUNT.NO>
    CHANGE @VM TO @FM IN Y.CERT.ACCOUNTS
    LOCATE VAL.ACCT IN Y.CERT.ACCOUNTS SETTING CER.ACT.POS THEN
        CALL F.READ(FN.CERTIFIED.CHEQUE.STOCK,VAL.CHEQUE.NUMBER,R.CERTIFIED.CHEQUE.STOCK,F.CERTIFIED.CHEQUE.STOCK,CER.CHQ.ERR)
        CALL F.READ(FN.CERTIFIED.CHEQUE.DETAILS,VAL.CHEQUE.NUMBER,R.CERTIFIED.CHEQUE.DETAILS,F.CERTIFIED.CHEQUE.DETAILS,CERTIFIED.CHEQUE.DETAILS.ERR)

    END ELSE
        REFER.FLAG = 1
        CERT.FLAG = 1
    END
    IF  R.CERTIFIED.CHEQUE.DETAILS THEN
        CER.CHQ.AMT = R.CERTIFIED.CHEQUE.DETAILS<CERT.STO.AMOUNT>
        CER.STATUS = R.CERTIFIED.CHEQUE.DETAILS<CERT.STO.STATUS>
        IF VAL.AMT NE CER.CHQ.AMT THEN
            REFER.FLAG = 1
            CERT.FLAG = 1
        END ELSE
            REFER.FLAG = ''
            VAR.REASON = ''
        END
*IF CER.STATUS EQ 'PAID' OR CER.STATUS EQ 'CANCELLED' OR  CER.STATUS EQ 'STOP.PAID.CNFRM' OR CER.STATUS EQ 'STOP.PAID.NON.CNFRM' OR CER.STATUS EQ 'STOP.PAID' THEN
        IF CER.STATUS NE 'ISSUED' THEN
            REFER.FLAG = 1
            CERT.FLAG = 1
            IF CER.STATUS EQ 'PAID' THEN
                REFER.PAID = 'YES'
            END
        END
    END ELSE
        REFER.FLAG = 1
    END

    IF CERT.FLAG EQ 1 THEN
        IF REFER.PAID EQ 'YES' THEN
            VAR.REASON<-1> = 90
        END ELSE
            VAR.REASON<-1> = 35
        END
    END

RETURN

GET.CHG.AMOUNT:

**Get the charge for the Customer to be debited in Statement Entry
    VAR.TOT.CHG.AMT = 0
    VAR.REF.CHG = ''
    LOCATE VAR.SEGMENTO IN SEGMENTO.VAL<1,1> SETTING SEG.POS THEN
        VAR.REF.CHG = REF.CHG.TYPE<1,SEG.POS>
    END
    IF VAR.REF.CHG NE '' THEN
        GOSUB GET.GROUP.DETAILS
    END

RETURN

GET.GROUP.DETAILS:

    CUSTOMER.ID = VAR.CUSTOMER
    DEAL.AMOUNT = VAL.AMT
    DEAL.CURRENCY = VAR.CURRENCY
    CCY.MKT = '1'
    CROSS.RATE = ""
    CROSS.CURRENCY = DEAL.CURRENCY
    DRAWDOWN.CURRENCY = CROSS.CURRENCY
    T.DATA = ""
    TOTAL.FOREIGN.AMT = ""
    TOTAL.LOCAL.AMT = ""
    TOTAL.AMT = ""
    T.DATA<1,1> = VAR.REF.CHG


    CALL CALCULATE.CHARGE(CUSTOMER.ID, DEAL.AMOUNT, DEAL.CURRENCY, CCY.MKT, CROSS.RATE,CROSS.CURRENCY, DRAWDOWN.CURRENCY, T.DATA, '', TOTAL.LOCAL.AMT, TOTAL.FOREIGN.AMT)
    VAR.TOT.CHG.AMT = T.DATA<4,1>

    IF CUSTOMER.ID EQ '' THEN   ;* No tax for Internal accounts
        VAL.NOR.TAX = 0
        VAL.TAX.AMT = 0
    END

RETURN

UPDATE.INWARD:
*Update REDO.APAP.CLEARING.INWARD Table with status

    TEMPTIME = OCONV(TIME(),"MTS")
    TEMPTIME = TEMPTIME[1,5]
    CHANGE ':' TO '' IN TEMPTIME   ;*R22 AUTO CONVERSTION CONVERT TO CHANGE
    CHECK.DATE = DATE()
    DATE.TIME = OCONV(CHECK.DATE,"DY2"):FMT(OCONV(CHECK.DATE,"DM"),"R%2"):FMT(OCONV(CHECK.DATE,"DD"),"R%2"):TEMPTIME

    IF RETURN.FLAG THEN
        R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.STATUS> = 'REJECTED'
        R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.REJECT.TYPE> = 'AUTOMATIC'
    END
    IF RETURN.FLAG NE 1 THEN
        IF REFER.FLAG THEN
            R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.REJECT.TYPE> = ''
            R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.REFER.DATE.TIME> = DATE.TIME
            R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.REFER.NAME> = 'SYSTEM'
            R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.STATUS> = 'REFERRED'
        END
    END
    IF REFER.FLAG EQ '' AND RETURN.FLAG EQ '' THEN
        VAR.REC.STATUS = 1
        R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.STATUS> = 'PAID'
        IF Y.ADMIN.FLAG THEN
            R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS> = 'PAID'
            CALL F.WRITE(FN.REDO.ADMIN.CHQ.DETAILS,VAL.CHEQUE.NUMBER,R.REDO.ADMIN.CHQ.DETAILS)
        END ELSE
            R.CERTIFIED.CHEQUE.DETAILS<CERT.DET.STATUS> = 'PAID'
            CALL F.WRITE(FN.CERTIFIED.CHEQUE.DETAILS,VAL.CHEQUE.NUMBER,R.CERTIFIED.CHEQUE.DETAILS)
        END
    END
    R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.CHG.AMOUNT> = VAR.TOT.CHG.AMT
    R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.CHARGE.TYPE> = VAR.REF.CHG
    CHANGE @FM TO @VM IN VAR.REASON

*------------fix for PACS00131732 for closed account ; starts------
    IF Y.HIS.FLAG THEN
        VAR.REASON = '20'
    END
*------------------------ends-----------------------------
*PACS00146621-S

    IF ADMIN.FLAG EQ 1 THEN
        IF REFER.PAID EQ 'YES' THEN
            VAR.REASON = 90
        END ELSE
            VAR.REASON = 33
        END
    END

*PACS00146621 -E

    R.REDO.APAP.CLEARING.INWARD<CLEAR.CHQ.REASON> = VAR.REASON
    CALL F.WRITE(FN.REDO.APAP.CLEARING.INWARD,REDO.ACCT.ID,R.REDO.APAP.CLEARING.INWARD)
RETURN

GENERATE.ENTRIES:

*CALL APAP.REDOAPAP.REDO.APAP.GENERATE.ENTRIES(MULTI.STMT) ;*MANUAL R22 CODE CONVERSION
    CALL APAP.REDOAPAP.redoApapGenerateEntries(MULTI.STMT) ;*MANUAL R22 CODE CONVERSION
RETURN

END
