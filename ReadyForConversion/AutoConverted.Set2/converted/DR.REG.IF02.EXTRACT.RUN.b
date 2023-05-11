SUBROUTINE DR.REG.IF02.EXTRACT.RUN
*-----------------------------------------------------------------------------
* This is a requirement report. The entity require this report with a letter with an unique reference.
* Must be enter a Customer ID or ID card identity as criteria selection like it is working in the version REDO.CREATE.ARRANGEMENT
*-----------------------------------------------------------------------------
* Modification History :
* ----------------------
*   Date          Author              Modification Description
*
* 30-Jul-2014     V.P.Ashokkumar      PACS00305219 - Fixed to retrive all forex, MM details for Customer
* 15-Sep-2014     V.P.Ashokkumar      PACS00305219 - Added closed account record.
* 06-Dec-2016     V.P.Ashokkumar      R15 Upgrade changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.MM.MONEY.MARKET
    $INSERT I_F.AA.SCHEDULED.ACTIVITY
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.DR.REG.IF02.EXTRACT
    $INSERT I_F.AA.ARRANGEMENT;* PACS00305219
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AA.PRODUCT.GROUP
    $INSERT I_F.AA.ACCOUNT

    OUTPUT.ARR = ''
    GOSUB INIT.PARA
    GOSUB GET.LR.POSN
    IF PROCESS.GO.AHEAD THEN
        GOSUB PROCESS.PARA
    END
RETURN
*------------------------------------------------------------------------------------
PROCESS.PARA:

    ID.CARD.IDENTITY = R.NEW(IF02.ID.CARD.NUMBER)
    CUSTOMER.NO = R.NEW(IF02.CUSTOMER.NUMBER)
*
    IF ID.CARD.IDENTITY AND CUSTOMER.NO THEN
        E = 'Both Customer ID and ID Card ID should not be entered'
        CALL STORE.END.ERROR
    END
*
    IF ID.CARD.IDENTITY AND CUSTOMER.NO EQ '' THEN
        R.CUSTOMER.L.CU.RNC = ''; CUSTOMER.L.CU.RNC.ERR = ''
        CALL F.READ(FN.CUSTOMER.L.CU.RNC,ID.CARD.IDENTITY,R.CUSTOMER.L.CU.RNC,F.CUSTOMER.L.CU.RNC,CUSTOMER.L.CU.RNC.ERR)
        GOSUB GET.ALTERNAT.CUSTID
    END
*
    R.CUSTOMER = ''; CUSTOMER.ERR = ''
    CALL F.READ(FN.CUSTOMER,CUSTOMER.NO,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    IF R.CUSTOMER THEN
        GOSUB RUN.CUSTOMER
    END ELSE
* Populate with Null values and Flag - "N"
        T24.CLIENT = 'N'
        SEQ.NUMB = 1
        GOSUB FMT.FLDS
        OUTPUT.ARR<1> = SEQ.NUMB:'*':CUST.TYPE:'*':CUST.ID:'*':NAMES:'*':NAME.INITIALS:'*':ACC.TYPE:'*':ACC.NUMBER:'*':ACC.STATUS:'*':BALANCE:'*':CCY.TYPE:'*':ACC.OPEN.DATE:'*':CUT.DATE:'*':CLOSE.DATE:'*':COMMENTS:'*':YEAR.OF.CIRCULAR:'*':T24.CLIENT
        GOSUB WRITE.TO.FILE
    END
RETURN

GET.ALTERNAT.CUSTID:
********************
    IF R.CUSTOMER.L.CU.RNC THEN
        CUSTOMER.NO = FIELD(R.CUSTOMER.L.CU.RNC,'*',2)
    END ELSE
        CALL F.READ(FN.CUSTOMER.L.CU.CIDENT,ID.CARD.IDENTITY,R.CUSTOMER.L.CU.CIDENT,F.CUSTOMER.L.CU.CIDENT,CUSTOMER.L.CU.CIDENT.ERR)
        CUSTOMER.NO = FIELD(R.CUSTOMER.L.CU.CIDENT,'*',2)
    END
RETURN

FMT.FLDS:
*-------*
    SEQ.NUMB = FMT(SEQ.NUMB,'R%7')
    CUST.TYPE = FMT(CUST.TYPE,'L#2')
    CUST.ID = FMT(CUST.ID,'L#17')
    NAMES = FMT(NAMES,'L#50')
    NAME.INITIALS = FMT(NAME.INITIALS,'L#30')
*    ACC.TYPE = FMT(ACC.TYPE,'2L')   ;* PACS00305219
    ACC.TYPE = FMT(ACC.TYPE,'R%3')
    ACC.NUMBER = FMT(ACC.NUMBER,'L#27')
    ACC.STATUS = FMT(ACC.STATUS,'L#1')
    BALANCE = FMT(BALANCE,'R2%14')
    CCY.TYPE = FMT(CCY.TYPE,'L#3')
    ACC.OPEN.DATE = FMT(ACC.OPEN.DATE,'L#10')
    CUT.DATE = FMT(CUT.DATE,'L#10')
    CLOSE.DATE = FMT(CLOSE.DATE,'L#10')
    COMMENTS = FMT(COMMENTS,'L#100')
    YEAR.OF.CIRCULAR = FMT(YEAR.OF.CIRCULAR,'R%4')
    T24.CLIENT = FMT(T24.CLIENT,'L#1')
RETURN
*------------------------------------------------------------------------------------
INIT.VARS:
*--------*
    COMMENTS = ''; YEAR.OF.CIRCULAR = ''; CUST.TYPE = ''; CUST.ID = ''; NAMES = ''; NAME.INITIALS = ''
    ACC.TYPE = ''; ACC.NUMBER = ''; ACC.STATUS = ''; BALANCE = ''; CCY.TYPE = ''; ACC.OPEN.DATE = ''
    CUT.DATE = ''; CLOSE.DATE = ''; R.AZ.CUSTOMER = ''; AZ.CUSTOMER.ERR = ''
RETURN
*------------------------------------------------------------------------------------
RUN.CUSTOMER:
*-----------*
    OUT.ARR = ''
    CALL DR.REG.GET.CUST.TYPE(R.CUSTOMER,OUT.ARR)
    CUST.TYPE = OUT.ARR<1>
    CUST.ID = OUT.ARR<2>
    TIPO.CL.POS.VAL = R.CUSTOMER<EB.CUS.LOCAL.REF,TIPO.CL.POS>
    GOSUB GET.NAMES
    GOSUB GET.NAME.INITIALS
    GOSUB GET.USER.VALUES
    GOSUB GET.ACCOUNT.DETAILS
    GOSUB WRITE.TO.FILE
RETURN
*------------------------------------------------------------------------------------
GET.USER.VALUES:
****************
    R.DR.REG.IF02.EXTRACT = ''; DR.REG.IF02.EXTRACT.ERR = ''; TXNACC.POS = ''
    CALL CACHE.READ(FN.DR.REG.IF02.EXTRACT,'SYSTEM',R.DR.REG.IF02.EXTRACT,DR.REG.IF02.EXTRACT.ERR)
    IF R.DR.REG.IF02.EXTRACT THEN
        SEQ.NUMB = R.DR.REG.IF02.EXTRACT<IF02.COMMUNC.NUMBER>
        YEAR.OF.CIRCULAR = R.DR.REG.IF02.EXTRACT<IF02.COMMUNC.YEAR>
        COMMENTS = R.DR.REG.IF02.EXTRACT<IF02.COMMENT>
        YFLD.NAME.ARR = R.DR.REG.IF02.EXTRACT<IF02.IF.FIELD.NAME>
        Y.FIELD.VAL.ARR = R.DR.REG.IF02.EXTRACT<IF02.IF.FIELD.VALUE>
        Y.DISP.TEXT.ARR = R.DR.REG.IF02.EXTRACT<IF02.IF.DISPLAY.VALUE>
    END
    LOCATE "CUR.CATEGORY" IN YFLD.NAME.ARR<1,1> SETTING TXNACC.POS THEN
        Y.TXNACC.VAL.ARR = Y.FIELD.VAL.ARR<1,TXNACC.POS>
        Y.TXNACC.DIS.ARR = Y.DISP.TEXT.ARR<1,TXNACC.POS>
    END
    Y.TXNACC.VAL.ARR = CHANGE(Y.TXNACC.VAL.ARR,@SM,@VM)
    Y.TXNACC.DIS.ARR = CHANGE(Y.TXNACC.DIS.ARR,@SM,@VM)
RETURN

WRITE.TO.FILE:
*------------*
    OPEN.ERR = ''
    IF R.CUSTOMER THEN
        EXTRACT.FILE.ID = 'IF02_':CUSTOMER.NO:'.txt'        ;* Parameterise
    END ELSE
        EXTRACT.FILE.ID = 'IF02_NON.T24.CUS.txt'
    END
**FN.CHK.DIR = '../bnk.interface/REG.REPORTS'
    FN.CHK.DIR = R.NEW(IF02.EXTRACT.PATH)
    OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID TO FV.EXTRACT.FILE THEN
        DELETESEQ FN.CHK.DIR,EXTRACT.FILE.ID THEN
        END ELSE
            NULL    ;* In case if it exisit DELETE, for Safer side
        END
        OPENSEQ FN.CHK.DIR,EXTRACT.FILE.ID TO FV.EXTRACT.FILE THEN
        END ELSE    ;* After DELETE file pointer will be closed, hence reopen the file
            CREATE FV.EXTRACT.FILE THEN
            END ELSE
                OPEN.ERR = 1
            END
        END
    END ELSE
        CREATE FV.EXTRACT.FILE THEN
        END ELSE
            OPEN.ERR = 1
        END
    END

    IF OPEN.ERR THEN
        TEXT = "Unable to Create a File -> ":EXTRACT.FILE.ID
        CALL FATAL.ERROR("DR.REG.IF02.EXTRACT.RUN")
    END
*
    CNT.OUT.ARR = DCOUNT(OUTPUT.ARR,@FM)
    CTR.OUT.ARR = 1
    LOOP
    WHILE CTR.OUT.ARR LE CNT.OUT.ARR
        R.REC = OUTPUT.ARR<CTR.OUT.ARR>
        CHANGE '*' TO '|' IN R.REC
        CRLF = CHARX(013):CHARX(010)
        CHANGE @FM TO CRLF IN R.REC
        WRITESEQ R.REC TO FV.EXTRACT.FILE THEN
        END ELSE
            NULL
        END
        CTR.OUT.ARR += 1
    REPEAT
*
RETURN

*------------------------------------------------------------------------------------
GET.AZ.BALANCE:
***************
    AZ.CCY.TYPE = R.AZ.ACCOUNT<AZ.CURRENCY>
    IF AZ.CCY.TYPE EQ LCCY THEN
        BALANCE = R.AZ.ACCOUNT<AZ.PRINCIPAL>
    END ELSE
        AZ.FAMT = R.AZ.ACCOUNT<AZ.PRINCIPAL>
        AZ.FCY = R.AZ.ACCOUNT<AZ.CURRENCY>
        Y.MARKET = '' ; Y.LAMT = '' ; Y.DIF.AMT = '' ; Y.DIF.PCT = '' ; Y.RATE = ''
        CALL MIDDLE.RATE.CONV.CHECK(AZ.FAMT,AZ.FCY,Y.RATE,Y.MARKET,Y.LAMT,Y.DIF.AMT,Y.DIF.PCT)
        BALANCE = Y.LAMT
    END
    CCY.TYPE = AZ.CCY.TYPE
RETURN

GET.ACCOUNT.DETAILS:
*------------------*
    R.CUSTOMER.ACCOUNT = ''; CUSTOMER.ACCOUNT.ERR = ''
    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUSTOMER.NO,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,CUSTOMER.ACCOUNT.ERR)
    IF NOT(R.CUSTOMER.ACCOUNT) THEN
        RETURN
    END
    R.REDO.CUST.PRD.LIST = ''; ERR.REDO.CUST.PRD.LIST = ''; YCLOSED.ACC.ID = ''
    CALL F.READ(FN.REDO.CUST.PRD.LIST,CUSTOMER.NO,R.REDO.CUST.PRD.LIST,F.REDO.CUST.PRD.LIST,ERR.REDO.CUST.PRD.LIST)
    IF R.REDO.CUST.PRD.LIST THEN
        YACCT.REC = R.REDO.CUST.PRD.LIST<1>
        YACCT.CNT = DCOUNT(YACCT.REC,@VM)
        GOSUB GET.CLOSED.ACCT
    END

    IF YCLOSED.ACC.ID THEN
        R.CUSTOMER.ACCOUNT<-1> = YCLOSED.ACC.ID
    END
    CUSTOMER.ACCOUNT.CNT = DCOUNT(R.CUSTOMER.ACCOUNT,@FM)
    CUSTOMER.ACCOUNT.CTR = 1
    GOSUB ACCT.PROCESS
RETURN

GET.CLOSED.ACCT:
****************
    REC.FND = 0; YCUST.CNT = 0
    LOOP
    UNTIL YACCT.CNT EQ YCUST.CNT
        YCUST.CNT += 1
        YACC.ID = R.REDO.CUST.PRD.LIST<1,YCUST.CNT>
        YAC.TYPE = R.REDO.CUST.PRD.LIST<3,YCUST.CNT>
        YAC.STATUS = R.REDO.CUST.PRD.LIST<2,YCUST.CNT>

        IF YAC.STATUS EQ 'CLOSED' AND YAC.TYPE EQ 'CUSTOMER' THEN
            YCLOSED.ACC.ID<-1> = YACC.ID
        END
    REPEAT
RETURN

ACCT.PROCESS:
*************
    LOOP
    WHILE CUSTOMER.ACCOUNT.CTR LE CUSTOMER.ACCOUNT.CNT
        ACC.TYPE = ''; ERR.ACCOUNT = ''; R.ACCOUNT = ''
        ACCOUNT.ERR = ''; YACC.CCATEG = ''
        GOSUB ACCT.SUB.PROCESS
    REPEAT
RETURN

ALT.ACCT.VALUE:
***************
    Y.ALT.ACCT.TYPE = ''; Y.ALT.ACCT.ID = ''; Y.PREV.ACCOUNT = ''
    Y.ALT.ACCT.TYPE=R.ACCOUNT<AC.ALT.ACCT.TYPE>
    Y.ALT.ACCT.ID=R.ACCOUNT<AC.ALT.ACCT.ID>
    LOCATE 'ALTERNO1' IN Y.ALT.ACCT.TYPE<1,1> SETTING ALT.TYPE.POS THEN
        Y.PREV.ACCOUNT = Y.ALT.ACCT.ID<1,ALT.TYPE.POS>
    END
RETURN

ACCT.SUB.PROCESS:
*****************
    ACCOUNT.VAL = R.CUSTOMER.ACCOUNT<CUSTOMER.ACCOUNT.CTR>
    ACC.NUMBER = ACCOUNT.VAL
    GOSUB READ.ACCOUNT
    GOSUB ALT.ACCT.VALUE
    IF Y.PREV.ACCOUNT THEN
        ACC.NUMBER = Y.PREV.ACCOUNT
    END
    YACC.CCATEG = R.ACCOUNT<AC.CATEGORY>
    TXNACE.POS = ''
    LOCATE YACC.CCATEG IN Y.TXNACC.VAL.ARR<1,1> SETTING TXNACE.POS THEN
        ACC.TYPE= Y.TXNACC.DIS.ARR<1,TXNACE.POS>
        GOSUB GET.ACCOUNT.BALANCE
    END

    IF R.ACCOUNT<AC.ALL.IN.ONE.PRODUCT> NE '' THEN          ;* is deposit
        CALL F.READ(FN.AZ.ACCOUNT,ACCOUNT.VAL,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACCOUNT.ERR)
        GOSUB GET.AZ.VAL
    END
*
    GOSUB ACC.STATUS
    IF CCY.TYPE EQ '' THEN    ;*The account type doesn't macth with category range and is not a deposit product
        GOSUB GET.ACCOUNT.BALANCE
    END

    ACC.OPEN.DATE = R.ACCOUNT<AC.OPENING.DATE>
    ACC.OPEN.DATE = ACC.OPEN.DATE[7,2]:'/':ACC.OPEN.DATE[5,2]:'/':ACC.OPEN.DATE[1,4]
    ARR.ID = ''
    ARR.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
    GOSUB GET.AA.ARRANG.VAL
    CUT.DATE = R.ACCOUNT<AC.DATE.LAST.CR.CUST>
    CLOSE.DATE = R.ACCOUNT<AC.CLOSURE.DATE>

    IF CUT.DATE THEN
        CUT.DATE = CUT.DATE[7,2]:'/':CUT.DATE[5,2]:'/':CUT.DATE[1,4]
    END
    IF CLOSE.DATE THEN
        CLOSE.DATE = CLOSE.DATE[7,2]:'/':CLOSE.DATE[5,2]:'/':CLOSE.DATE[1,4]
    END
    T24.CLIENT = 'S'
    GOSUB FMT.FLDS
    OUTPUT.ARR<-1> = SEQ.NUMB:'*':CUST.TYPE:'*':CUST.ID:'*':NAMES:'*':NAME.INITIALS:'*':ACC.TYPE:'*':ACC.NUMBER:'*':ACC.STATUS:'*':BALANCE:'*':CCY.TYPE:'*':ACC.OPEN.DATE:'*':CUT.DATE:'*':CLOSE.DATE:'*':COMMENTS:'*':YEAR.OF.CIRCULAR:'*':T24.CLIENT
    CUSTOMER.ACCOUNT.CTR += 1
    ACC.TYPE = ''; BALANCE = ''; CCY.TYPE = ''
RETURN

READ.ACCOUNT:
*************
    R.ACCOUNT = ''; ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,ACCOUNT.VAL,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    IF NOT(R.ACCOUNT) THEN
        ACCOUNT.VAL.HST = ACCOUNT.VAL
        CALL EB.READ.HISTORY.REC(F.ACCOUNT.HST,ACCOUNT.VAL.HST,R.ACCOUNT,ERR.ACCOUNT)
    END
RETURN

GET.AZ.VAL:
***********
    IF R.AZ.ACCOUNT THEN
        ACC.TYPE = R.AZ.ACCOUNT<AZ.LOCAL.REF,AZ.L.INV.FACILITY>
        GOSUB GET.AZ.BALANCE
    END
RETURN

GET.AA.ARRANG.VAL:
******************
    IF ARR.ID THEN
        R.AA.SCHEDULED.ACTIVITY = ''; AA.SCHEDULED.ACTIVITY.ERR = ''
        CALL F.READ(FN.AA.SCHEDULED.ACTIVITY,ARR.ID,R.AA.SCHEDULED.ACTIVITY,F.AA.SCHEDULED.ACTIVITY,AA.SCHEDULED.ACTIVITY.ERR)
        CUT.DATE = R.AA.SCHEDULED.ACTIVITY<AA.SCH.NEXT.RUN.DATE>
*PACS00305219
*        IF ACC.STATUS EQ '' THEN
        R.AA.ARRANGEMENT = ''; AA.ARRANGEMENT.ERR = ''
        CALL F.READ(FN.AA.ARRANGEMENT,ARR.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,AA.ARRANGEMENT.ERR)
        GOSUB AA.ARRANG.CASE
*        END
*PACS00305219
    END
RETURN

AA.ARRANG.CASE:
***************
    IF NOT(R.AA.ARRANGEMENT) THEN
        RETURN
    END
    AAR.STATUS.VAL = R.AA.ARRANGEMENT<AA.ARR.ARR.STATUS>
    BEGIN CASE
        CASE AAR.STATUS.VAL EQ 'AUTH' OR AAR.STATUS.VAL EQ 'AUTH.FWD' OR AAR.STATUS.VAL EQ 'CURRENT'
            ACC.STATUS = 'A'
        CASE AAR.STATUS.VAL EQ 'MATURED' OR AAR.STATUS.VAL EQ 'EXPIRED' OR AAR.STATUS.VAL EQ 'REVERSED' OR AAR.STATUS.VAL EQ 'CANCELLED' OR AAR.STATUS.VAL EQ 'CLOSE' OR AAR.STATUS.VAL EQ 'PENDING.CLOSURE'
            ACC.STATUS = 'C'
    END CASE
    STAR.DATE.VAL = R.AA.ARRANGEMENT<AA.ARR.START.DATE>
    PROD.GROUP.ID = R.AA.ARRANGEMENT<AA.ARR.PRODUCT.GROUP>
    GOSUB GET.LOAN.TYPE
    GOSUB GET.LOAN.BALANCE
    ACC.OPEN.DATE = STAR.DATE.VAL[7,2]:'/':STAR.DATE.VAL[5,2]:'/':STAR.DATE.VAL[1,4]
RETURN

GET.ACCOUNT.BALANCE:
********************
    CCY.TYPE = R.ACCOUNT<AC.CURRENCY>
    IF CCY.TYPE EQ LCCY THEN
        BALANCE = R.ACCOUNT<AC.LOCAL.REF,L.AC.AV.BAL.POS>
    END ELSE
        Y.FAMT = R.ACCOUNT<AC.LOCAL.REF,L.AC.AV.BAL.POS>

        Y.FCY = R.ACCOUNT<AC.CURRENCY>
        Y.MARKET = '' ; Y.LAMT = '' ; Y.DIF.AMT = '' ; Y.DIF.PCT = '' ; Y.RATE = ''
        CALL MIDDLE.RATE.CONV.CHECK(Y.FAMT,Y.FCY,Y.RATE,Y.MARKET,Y.LAMT,Y.DIF.AMT,Y.DIF.PCT)
        BALANCE = Y.LAMT
    END
RETURN
*------------------------------------------------------------------------------------
GET.LOAN.BALANCE:
*****************
    START.DATE=TODAY
    END.DATE=START.DATE
    BALANCES.TO.CHECK='CURACCOUNT':@FM:'DE1ACCOUNT':@FM:'DE3ACCOUNT':@FM:'DELACCOUNT':@FM:'NABACCOUNT'
    TOT.BALANCES = DCOUNT(BALANCES.TO.CHECK,@FM)
    REQUEST.TYPE<4>='ECB'
    CNTR = 1
*
    LOOP
    WHILE CNTR LE TOT.BALANCES
        BALANCE.TO.CHECK=BALANCES.TO.CHECK<CNTR>
        CALL AA.GET.PERIOD.BALANCES(ACCOUNT.VAL,BALANCE.TO.CHECK,REQUEST.TYPE,START.DATE,END.DATE,'',BAL.DETAILS, ERROR.MESSAGE)
        BALANCE.AMT = BAL.DETAILS<4>
        TOTAL.BAL += BALANCE.AMT
        CNTR += 1
    REPEAT
    BALANCE = ABS(BALANCE.AMT)
RETURN

GET.LOAN.TYPE:
**************
    ARRANGEMENT.ID = ARR.ID
    PROP.CLASS     = 'ACCOUNT'
    PROP.NAME      = ''; RET.ERR  = ''; returnConditions = ''; R.AA.ACCOUNT  = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.ID,PROP.CLASS,PROP.NAME,'','',returnConditions,RET.ERR)
    R.AA.ACCOUNT = RAISE(returnConditions)
    ACC.TYPE = R.AA.ACCOUNT<AA.AC.LOCAL.REF,AA.INV.FACILITY>
RETURN

ACC.STATUS:
*---------*
    ACC.STATUS = ''; STAT1 = ''; STAT2 = ''
    STAT1 = R.ACCOUNT<AC.LOCAL.REF,AC.STATUS1.POS>
    STAT2 = R.ACCOUNT<AC.LOCAL.REF,AC.STATUS2.POS>
    STAT2.CNT = DCOUNT(STAT2,@SM)
*PACS00305219
    IF R.ACCOUNT<AC.CLOSURE.DATE> NE '' THEN      ;*  (THE ACCOUNT MUST BE IN HISTORY FILE)
        ACC.STATUS = 'C'
        RETURN
    END
*PACS00305219

    IF STAT2.CNT GE 2 THEN
        GOSUB GET.STATUS.MULTIVAL
        RETURN
    END

    BEGIN CASE
        CASE STAT2 EQ 'DECEASED'
            ACC.STATUS = 'F'
        CASE STAT2 EQ 'GARNISHMENT'
            ACC.STATUS = 'E'
        CASE STAT2 EQ 'GUARANTEE.STATUS'
            ACC.STATUS = 'G'
        CASE STAT1 EQ 'ACTIVE' OR STAT1 EQ '' OR STAT1 EQ '6MINACTIVE'
            ACC.STATUS = 'A'
        CASE STAT1 EQ 'ABANDONED'
            ACC.STATUS = 'B'
        CASE STAT1 EQ '3YINACTIVE'
            ACC.STATUS = 'I'
    END CASE
RETURN

GET.STATUS.MULTIVAL:
********************
    D.POSN = ''; G.POSN = ''
    LOCATE 'DECEASED' IN STAT2<1,1,1> SETTING D.POSN THEN
        ACC.STATUS = 'F'
    END ELSE
        GOSUB GET.STATUS.EG
    END
RETURN

GET.STATUS.EG:
**************
    LOCATE 'GARNISHMENT' IN STAT2<1,1,1> SETTING G.POSN THEN
        ACC.STATUS = 'E'
    END ELSE
        ACC.STATUS = 'G'
    END
RETURN

INIT.PARA:
**********
*
    PROCESS.GO.AHEAD =  1
    ID.CARD.IDENTITY = ''; CUSTOMER.NO = ''

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'; F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
*
    FN.AA.SCHEDULED.ACTIVITY = 'F.AA.SCHEDULED.ACTIVITY'; F.AA.SCHEDULED.ACTIVITY = ''
    CALL OPF(FN.AA.SCHEDULED.ACTIVITY,F.AA.SCHEDULED.ACTIVITY)
*
    FN.CUSTOMER.L.CU.RNC = 'F.CUSTOMER.L.CU.RNC'; F.CUSTOMER.L.CU.RNC = ''
    CALL OPF(FN.CUSTOMER.L.CU.RNC,F.CUSTOMER.L.CU.RNC)
*
    FN.CUSTOMER.L.CU.CIDENT = 'F.CUSTOMER.L.CU.CIDENT'; F.CUSTOMER.L.CU.CIDENT = ''
    CALL OPF(FN.CUSTOMER.L.CU.CIDENT,F.CUSTOMER.L.CU.CIDENT)
*
    FN.CUSTOMER = 'F.CUSTOMER'; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)
*
    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'; F.CUSTOMER.ACCOUNT = ''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
*
    FN.ACCOUNT = 'F.ACCOUNT'; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
*
    FN.DR.REG.IF02.EXTRACT = 'F.DR.REG.IF02.EXTRACT'; F.DR.REG.IF02.EXTRACT = ''
    CALL OPF(FN.DR.REG.IF02.EXTRACT,F.DR.REG.IF02.EXTRACT)

    FN.AZ.CUSTOMER = 'F.AZ.CUSTOMER'; F.AZ.CUSTOMER = ''
    CALL OPF(FN.AZ.CUSTOMER,F.AZ.CUSTOMER)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.REDO.CUST.PRD.LIST = 'F.REDO.CUST.PRD.LIST'; F.REDO.CUST.PRD.LIST = ''
    CALL OPF(FN.REDO.CUST.PRD.LIST,F.REDO.CUST.PRD.LIST)

    FN.ACCOUNT.HST = 'F.ACCOUNT$HIS'; F.ACCOUNT.HST = ''
    CALL OPF(FN.ACCOUNT.HST,F.ACCOUNT.HST)

    GOSUB INIT.VARS
RETURN

GET.LR.POSN:
************
    APPL.NAME = 'CUSTOMER':@FM:'ACCOUNT':@FM:'MM.MONEY.MARKET':@FM:'AA.ARR.ACCOUNT':@FM:'AZ.ACCOUNT'
    FLD.NAME = 'L.CU.TIPO.CL':@VM:'L.CU.RNC':@VM:'L.CU.CIDENT':@VM:'L.CU.FOREIGN':@FM:'L.AC.STATUS1':@VM:'L.AC.STATUS2':@VM:'L.INV.FACILITY':@VM:'L.AC.AV.BAL':@FM:'L.INV.FACILITY':@FM:'L.CR.FACILITY':@FM:'L.INV.FACILITY'
    FLD.POS = ''
    CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)
    TIPO.CL.POS = FLD.POS<1,1>
    CIDENT.POS = FLD.POS<1,3>
    RNC.POS = FLD.POS<1,2>
    FOREIGN.POS = FLD.POS<1,4>
    AC.STATUS1.POS = FLD.POS<2,1>
    AC.STATUS2.POS = FLD.POS<2,2>
    AC.INV.FACILITY = FLD.POS<2,3>
    L.AC.AV.BAL.POS = FLD.POS<2,4>
    MM.INV.FACILITY = FLD.POS<3,1>
    AA.INV.FACILITY = FLD.POS<4,1>
    AZ.L.INV.FACILITY = FLD.POS<5,1>
RETURN
*----------------------------------------------------------------------------
GET.NAMES:
*--------*
    NAMES = ''
    BEGIN CASE
        CASE TIPO.CL.POS.VAL EQ "PERSONA FISICA" OR "CLIENTE MENOR"
            NAMES = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
        CASE TIPO.CL.POS.VAL EQ "PERSONA JURIDICA"
            NAMES = R.CUSTOMER<EB.CUS.NAME.1,1>:' ':R.CUSTOMER<EB.CUS.NAME.2,1>
    END CASE
*
RETURN
*---------------------------------------------------------------------------
GET.NAME.INITIALS:
*----------------*
    BEGIN CASE
        CASE TIPO.CL.POS.VAL EQ "PERSONA FISICA" OR "CLIENTE MENOR"
            NAME.INITIALS = R.CUSTOMER<EB.CUS.FAMILY.NAME>
        CASE TIPO.CL.POS.VAL EQ "PERSONA JURIDICA"
            NAME.INITIALS = R.CUSTOMER<EB.CUS.SHORT.NAME,1>
    END CASE
*
RETURN
*---------------------------------------------------------------------------
END
