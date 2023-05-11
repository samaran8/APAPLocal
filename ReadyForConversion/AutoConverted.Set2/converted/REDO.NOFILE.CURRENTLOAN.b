SUBROUTINE REDO.NOFILE.CURRENTLOAN(Y.ARRAY.OUT)
*------------------------------------------------------------------------
*Description : This routine is nofile enquiry routine in order to fetch the loan when it is not CANCELED
*Attached to : ENQUIRY>L.APAP.ENQ.PRESTVIG
*
*------------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------------------------------------------------
*  DATE             WHO         REFERENCE            DESCRIPTION
* 18/08/2017   Raquel P.S.      CI007105     Query with a selection field diferent than Customer, does not
*                                            display records. It ocurred when R9 was upgraded to R15.
*----------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.PRODUCT
    $INSERT I_F.EB.LOOKUP
    $INSERT I_F.USER
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.REDO.CUSTOMER.ARRANGEMENT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY

    GOSUB INIT
    GOSUB PROCESS
    GOSUB PGM.END
RETURN

*------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------
* Variables and files are opened here

    FN.AA.ARRANGEMENT.ACTIVITY = 'F.AA.ARRANGEMENT.ACTIVITY'
    FN.AA.ARRANGEMENT.ACTIVITY<2>='NO.FATAL.ERROR'
    F.AA.ARRANGEMENT.ACTIVITY = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY,F.AA.ARRANGEMENT.ACTIVITY)

    FN.CUSTOMER='F.CUSTOMER'
    F.CUSTOMER=''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT=''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.PRODUCT='F.AA.PRODUCT'
    F.AA.PRODUCT=''
    CALL OPF(FN.AA.PRODUCT,F.AA.PRODUCT)
    FN.REDO.CUSTOMER.ARRANGEMENT='F.REDO.CUSTOMER.ARRANGEMENT'
    F.REDO.CUSTOMER.ARRANGEMENT=''
    CALL OPF(FN.REDO.CUSTOMER.ARRANGEMENT,F.REDO.CUSTOMER.ARRANGEMENT)

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    FN.EB.LOOKUP='F.EB.LOOKUP'
    F.EB.LOOKUP=''
    CALL OPF(FN.EB.LOOKUP,F.EB.LOOKUP)

    FN.CUS.CIDENT = 'F.CUSTOMER.L.CU.CIDENT'
    F.CUS.CIDENT = ''
    CALL OPF(FN.CUS.CIDENT,F.CUS.CIDENT)

    FN.CUS.RNC = 'F.CUSTOMER.L.CU.RNC'
    F.CUS.RNC = ''
    CALL OPF(FN.CUS.RNC,F.CUS.RNC)

    FN.CUS.ACTANAC = 'F.CUSTOMER.L.CU.ACTANAC'
    F.CUS.ACTANAC = ''
    CALL OPF(FN.CUS.ACTANAC,F.CUS.ACTANAC)

    FN.CUS.NOUNICO = 'F.CUSTOMER.L.CU.NOUNICO'
    F.CUS.NOUNICO = ''
    CALL OPF(FN.CUS.NOUNICO,F.CUS.NOUNICO)
RETURN

*------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------
* Here the main process of selecting the customer happens
    LOCATE "@ID" IN D.FIELDS<1> SETTING CUS.POS THEN
        Y.CUST.ID = D.RANGE.AND.VALUE<CUS.POS>
    END

    CNT.FM = DCOUNT(D.RANGE.AND.VALUE,@FM)

    LOCATE "L.CU.CIDENT" IN D.FIELDS<1> SETTING CIDENT.POS THEN
        Y.CIDENT.ID = D.RANGE.AND.VALUE<CIDENT.POS>
        CALL F.READ(FN.CUS.CIDENT,Y.CIDENT.ID,R.CUS.REC,F.CUS.CIDENT,CUS.ERR)
        Y.CUSTOMER.ID = FIELD(R.CUS.REC,'*',2)
    END

    LOCATE "L.CU.RNC" IN D.FIELDS<1> SETTING RNC.POS THEN
        Y.RNC.ID = D.RANGE.AND.VALUE<RNC.POS>
        CALL F.READ(FN.CUS.RNC,Y.RNC.ID,R.CUS.REC,F.CUS.RNC,CUS.ERR)
        Y.CUSTOMER.ID = FIELD(R.CUS.REC,'*',2)
    END

    LOCATE "L.CU.ACTANAC" IN D.FIELDS<1> SETTING ACTANAC.POS THEN
        Y.ACT.ID = D.RANGE.AND.VALUE<ACTANAC.POS>
        CALL F.READ(FN.CUS.ACTANAC,Y.ACT.ID,R.CUS.REC,F.CUS.ACTANAC,CUS.ERR)
        Y.CUSTOMER.ID = FIELD(R.CUS.REC,'*',2)
    END

    LOCATE "L.CU.NOUNICO" IN D.FIELDS<1> SETTING NOUNICO.POS THEN
        Y.NOUN.ID = D.RANGE.AND.VALUE<NOUNICO.POS>
        CALL F.READ(FN.CUS.NOUNICO,Y.NOUN.ID,R.CUS.REC,F.CUS.NOUNICO,CUS.ERR)
        Y.CUSTOMER.ID = FIELD(R.CUS.REC,'*',2)
    END

    GOSUB CHECK.CUST.ID

    R.CUS.ARR = ''

    IF Y.CUSTOMER.ID THEN
        CALL F.READ(FN.REDO.CUSTOMER.ARRANGEMENT,Y.CUSTOMER.ID,R.CUS.ARR,F.REDO.CUSTOMER.ARRANGEMENT,CUS.ARR.ERR)
    END
    IF R.CUS.ARR THEN
        GOSUB GET.ARRANGEMENT
    END
RETURN

*-------------------------------------------------------------------------
CHECK.CUST.ID:
*-------------------------------------------------------------------------
    IF Y.CUSTOMER.ID AND Y.CUST.ID THEN
        IF (Y.CUSTOMER.ID NE Y.CUST.ID) THEN
            GOSUB PGM.END
        END
        Y.CUSTOMER.ID = Y.CUST.ID
    END

    IF NOT(Y.CUSTOMER.ID) AND Y.CUST.ID THEN
        Y.CUSTOMER.ID = Y.CUST.ID
    END
RETURN

*------------------------------------------------------------------------
GET.ARRANGEMENT:
*------------------------------------------------------------------------
* In this part, all arrangement related to that customer are fetched from REDO.CUSTOMER.ARRANGEMENT
    Y.OWNER=R.CUS.ARR<CUS.ARR.OWNER>
    Y.OWNER.CNT=DCOUNT(Y.OWNER,@VM)

    Y.ARRAY=''
    Y.VAR1=1
    LOOP
    WHILE Y.VAR1 LE Y.OWNER.CNT

        Y.ARR.ID=Y.OWNER<1,Y.VAR1>

        IF Y.ARR.ID THEN
            CALL F.READ(FN.AA.ARRANGEMENT,Y.ARR.ID,R.ARR.REC,F.AA.ARRANGEMENT,ARR.ERR)
            Y.PRODUCT=R.ARR.REC<AA.ARR.PRODUCT>
            Y.STATUS = R.ARR.REC<AA.ARR.ARR.STATUS>

            SEL.CMD = "SELECT " :FN.AA.ARRANGEMENT.ACTIVITY: " WITH ARRANGEMENT EQ " : Y.ARR.ID
            CALL EB.READLIST(SEL.CMD, SEL.LIST,"", NO.OF.REC, SEL.ERR)

            LOOP  REMOVE Y.TT.ID FROM SEL.LIST SETTING TT.POS
            WHILE  Y.TT.ID  DO
                CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY,Y.TT.ID,R.ARR.ACT,F.AA.ARRANGEMENT.ACTIVITY,ARR.ERR)
                Y.ACTIVITY =R.ARR.ACT<AA.ARR.ACT.ACTIVITY>

                IF Y.ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF' OR Y.ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF.CHQ'   THEN
                    Y.ID.PAYOFF=Y.ARR.ID
                END
            REPEAT

            IF Y.STATUS EQ 'AUTH' THEN
                Y.VAR1 += 1
                CONTINUE
            END
            IF Y.ARR.ID  NE Y.ID.PAYOFF THEN
                CALL CACHE.READ(FN.AA.PRODUCT, Y.PRODUCT, R.AA.PRD, PRD.ERR)
                GOSUB GET.TERM.AMOUNT
                GOSUB GET.ACCT.NO
                Y.ARRAY:=Y.AA.ACCT.ID

                IF R.USER<EB.USE.LANGUAGE> EQ 2 THEN
                    GOSUB CHECK.COND
                END

                Y.ARRAY:='*':Y.PROD.DESC
                Y.ARRAY:='*'
                Y.ARRAY:='*':Y.TERM.AMOUNT
                CALL REDO.GET.TOTAL.OUTSTANDING.SIN.UNC.UND(Y.ARR.ID,Y.BAL,Y.TOT.BAL)
                Y.BALANCE = Y.BAL<1>
                Y.ARRAY:='*':Y.BALANCE
                GOSUB GET.NEXT.PAYMENT
                Y.ARRAY:='*':Y.NEXTPAY.AMT
                Y.ARRAY:='*':R.ARR.REC<AA.ARR.CURRENCY>
                Y.ARRAY:='*':R.ARR.REC<AA.ARR.START.DATE>:@FM
            END

        END
        Y.VAR1 += 1
    REPEAT


    Y.OTHER=R.CUS.ARR<CUS.ARR.OTHER.PARTY>
    Y.OTHER.CNT=DCOUNT(Y.OTHER,@VM)
    Y.VAR2=1
    LOOP
    WHILE Y.VAR2 LE Y.OTHER.CNT
        Y.ARR.ID=Y.OTHER<1,Y.VAR2>
        Y.ARR.ID=Y.OTHER<1,Y.VAR2>

        IF Y.ARR.ID THEN
            CALL F.READ(FN.AA.ARRANGEMENT,Y.ARR.ID,R.ARR.REC,F.AA.ARRANGEMENT,ARR.ERR)
            Y.PRODUCT=R.ARR.REC<AA.ARR.PRODUCT>
            Y.STATUS = R.ARR.REC<AA.ARR.ARR.STATUS>

            SEL.CMD = "SELECT " :FN.AA.ARRANGEMENT.ACTIVITY: " WITH ARRANGEMENT EQ " : Y.ARR.ID
            CALL EB.READLIST(SEL.CMD, SEL.LIST,"", NO.OF.REC, SEL.ERR)

            LOOP  REMOVE Y.TT.ID FROM SEL.LIST SETTING TT.POS
            WHILE  Y.TT.ID  DO
                CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY,Y.TT.ID,R.ARR.ACT,F.AA.ARRANGEMENT.ACTIVITY,ARR.ERR)
                Y.ACTIVITY =R.ARR.ACT<AA.ARR.ACT.ACTIVITY>

                IF Y.ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF' OR Y.ACTIVITY EQ 'LENDING-APPLYPAYMENT-RP.PAYOFF.CHQ'   THEN
                    Y.ID.PAYOFF=Y.ARR.ID
                END
            REPEAT
            IF Y.STATUS EQ 'AUTH' THEN
                Y.VAR2 += 1
                CONTINUE
            END
            IF Y.ARR.ID NE Y.ID.PAYOFF THEN
                GOSUB GET.ROLE
                GOSUB GET.TERM.AMOUNT
                GOSUB GET.ACCT.NO
                Y.ARRAY:=Y.AA.ACCT.ID
                Y.ARRAY:='*':Y.PRODUCT
                Y.ARRAY:='*':Y.ROLE.CUS
                Y.ARRAY:='*':Y.TERM.AMOUNT
                CALL REDO.GET.TOTAL.OUTSTANDING.SIN.UNC.UND(Y.ARR.ID,Y.BAL,Y.TOT.BAL)
                Y.BALANCE = Y.BAL<1>
                Y.ARRAY:='*':Y.BALANCE
                GOSUB GET.NEXT.PAYMENT
                Y.ARRAY:='*':Y.NEXTPAY.AMT
                Y.ARRAY:='*':R.ARR.REC<AA.ARR.CURRENCY>
                Y.ARRAY:='*':R.ARR.REC<AA.ARR.START.DATE>:@FM
*            Y.ARRAY:='*':R.ARR.ACT<AA.ARR.ACT.ACTIVITY>
            END
        END
        Y.VAR2 += 1
    REPEAT
    GOSUB FINAL.ARRAY
RETURN
*-----------------------------------------------------------------------
CHECK.COND:
*-----------------------------------------------------------------------
    IF R.AA.PRD<AA.PDT.DESCRIPTION,2> THEN
        Y.PROD.DESC=R.AA.PRD<AA.PDT.DESCRIPTION,2>
    END ELSE
        Y.PROD.DESC=R.AA.PRD<AA.PDT.DESCRIPTION,1>
    END
RETURN
*------------------------------------------------------------------------
GET.TERM.AMOUNT:
*------------------------------------------------------------------------
    Y.TERM.AMOUNT=0
    EFF.DATE = ''; PROP.CLASS='TERM.AMOUNT'; PROPERTY = ''; R.CONDITION = ''; ERR.MSG = ''
    CALL REDO.CRR.GET.CONDITIONS(Y.ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.CONDITION,ERR.MSG)
    Y.TERM.AMOUNT=R.CONDITION<AA.AMT.AMOUNT>
RETURN
*------------------------------------------------------------------------
GET.NEXT.PAYMENT:
*------------------------------------------------------------------------
    CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.ARR.ID,R.AA.ACC.DET,F.AA.ACCOUNT.DETAILS,ACT.ERR)
    Y.PAY.START.DATE = R.AA.ACC.DET<AA.AD.PAYMENT.START.DATE>
    IF Y.PAY.START.DATE ELSE
        Y.PAY.START.DATE = TODAY
    END

    Y.NEXTPAY.AMT=''
    Y.AMT = 0
    SIMULATION.REF = ''
    NO.RESET = '1'
    YREGION = ''
    YDATE = TODAY
    Y.YEAR = YDATE[1,4] + 1
    YDAYS.ORIG = Y.YEAR:TODAY[5,4]
    DATE.RANGE = Y.PAY.START.DATE:@FM:YDAYS.ORIG   ;* Date range is passed for 1 years to avoid building schedule for whole loan term

    PAYMENT.DATES = '' ; PAYMENT.TYPES = '' ; TOT.PAYMENT = '' ; PAYMENT.METHOD = '' ; DUE.TYPE.AMTS = ''
    PAYMENT.PROPERTIES = '' ; PAYMENT.PROPERTIES.AMT = '' ; DUE.OUTS = ''
    CALL AA.SCHEDULE.PROJECTOR(Y.ARR.ID, SIMULATION.REF, NO.RESET, DATE.RANGE, TOT.PAYMENT, PAYMENT.DATES, DUE.DEFER.DATES, PAYMENT.TYPES, DUE.METHODS,DUE.TYPE.AMTS, PAYMENT.PROPERTIES, PAYMENT.PROPERTIES.AMT, DUE.OUTS)

    Y.DATES.CNT = DCOUNT(PAYMENT.DATES,@FM)
    CNT = 1
    LOOP

    WHILE CNT LE Y.DATES.CNT
        Y.PAY.DATE = PAYMENT.DATES<CNT>
        IF Y.PAY.DATE GT TODAY THEN
            Y.AMT = TOT.PAYMENT<CNT>
            CNT = Y.DATES.CNT+1
        END
        CNT += 1
    REPEAT
    Y.NEXTPAY.AMT = Y.AMT
RETURN


*------------------------------------------------------------------------
GET.ROLE:
*------------------------------------------------------------------------
    Y.ROLE=''; ERR.MSG = ''
    Y.OTHER.PARTY=''; EFF.DATE = ''; PROP.CLASS='CUSTOMER'; PROPERTY = ''; R.CONDITION.CUSTOMER = ''
    CALL REDO.CRR.GET.CONDITIONS(Y.ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.CONDITION.CUSTOMER,ERR.MSG)
    Y.OTHER.PARTY=R.CONDITION.CUSTOMER<AA.CUS.OTHER.PARTY>
    Y.ROLE=R.CONDITION.CUSTOMER<AA.CUS.ROLE>
    LOCATE Y.CUSTOMER.ID IN Y.OTHER.PARTY<1,1> SETTING POS1 THEN

        Y.ROLE.CUS=Y.ROLE<1,POS1>
        CALL F.READ(FN.EB.LOOKUP,'AA.PARTY.ROLE*':Y.ROLE.CUS,R.EB.LOOKUP,F.EB.LOOKUP,LOOK.ERR)
        IF R.USER<EB.USE.LANGUAGE> EQ 1 THEN
            Y.ROLE.CUS=R.EB.LOOKUP<EB.LU.DESCRIPTION,1>
        END
        IF R.USER<EB.USE.LANGUAGE> EQ 2 THEN
            GOSUB CHECK.COND1
        END
    END
RETURN
*-----------------------------------------------------------------------
CHECK.COND1:
*-----------------------------------------------------------------------
    IF R.EB.LOOKUP<EB.LU.DESCRIPTION,2> THEN
        Y.ROLE.CUS=R.EB.LOOKUP<EB.LU.DESCRIPTION,2>
    END ELSE
        Y.ROLE.CUS=R.EB.LOOKUP<EB.LU.DESCRIPTION,1>
    END
RETURN


*------------------------------------------------------------------------
FINAL.ARRAY:
*------------------------------------------------------------------------
    Y.ARRAY.OUT:=Y.ARRAY
RETURN

*--------------------------------------------------------------------------
GET.ACCT.NO:
*--------------------------------------------------------------------------

    IN.ACC.ID = ''
    IN.ARR.ID = Y.ARR.ID
    OUT.ID    = ''
    ERR.TEXT  = ''
    CALL REDO.CONVERT.ACCOUNT(IN.ACC.ID,IN.ARR.ID,OUT.ID,ERR.TEXT)
    IF NOT(ERR.TEXT) THEN
        Y.AA.ACCT.ID = OUT.ID
    END

RETURN

*---------------------------------------------------------------------------
PGM.END:
*---------------------------------------------------------------------------
END
