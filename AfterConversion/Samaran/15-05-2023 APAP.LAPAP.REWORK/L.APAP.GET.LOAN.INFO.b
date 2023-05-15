* @ValidationCode : MjotMzQwNzM2Mzg1OkNwMTI1MjoxNjg0MTQ3NzY1MTU5OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 15 May 2023 16:19:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.GET.LOAN.INFO(R.DATA)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*-----------------------------------------------------------------------------
* This is an Nofile routine to get some data for enquiry ENQ.L.APAP.GET.LOAN.INFO
* related to C.3 IVR Interface.

* Input/Output:
*--------------
* IN : LINKED.APPL.ID
* OUT : R.DATA (ALL DATA)
*---------------
*-----------------------------------------------------------------------------
* Modification History :
* Date            Who                   Reference               Description
* 30-05-2019      ANTHONY MARTINEZ      APAPMOVIL               CREATION
*  DATE             WHO                   REFERENCE
* 21-APRIL-2023      Harsha                R22 Auto Conversion  - VM to @VM , FM to @FM , SM to @SM , I to I.VAR , F.READ to CACHE.READ , J to J.VAR , ++ to +=1 and K to K.VAR
* 21-APRIL-2023      Harsha                R22 Manual Conversion - CALL ROUTINE FORMAT MODIFIED
*-----------------------------------------------------------------------------
*<region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.CURRENCY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.INTEREST.ACCRUALS
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.OVERDUE
    $INSERT I_F.ACCOUNT
    $USING APAP.TAM
    $USING APAP.AA
   
* </region>
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*****
INIT:
*****

    Y.LINKED.APPL.ID.POS = ""
    Y.LINKED.APPL.ID = ""
    Y.DATACOUNT = ""
    Y.RESULT = ""
    AA.ARRANGEMENT.ERR = ""
    Y.AA.ARRANGEMENT.REC = ""
    Y.AA.ID = ""
    Y.CURRENCY = ""
    Y.CURRENCY.REC = ""
    CURRENCY.ERR = ""
    Y.AA.ACCOUNT.DETAILS.REC = ""
    AA.ACCOUNT.DETAILS.ERR = ""

    LOC.REF.POS = ""
    LOC.REF.APP = "AA.PRD.DES.OVERDUE"
    LOC.REF.FIELD = "L.LOAN.STATUS.1"
    CALL GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)
    POS.L.LOAN.STATUS = LOC.REF.POS<1,1>

RETURN

*********
OPENFILES:
*********
*Open files AA.ARRANGEMENT and AA.ACCOUNT.DETAILS

    FN.AA.ARRANGEMENT = "F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT = ""
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.ACCOUNT.DETAILS = "F.AA.ACCOUNT.DETAILS"
    F.AA.ACCOUNT.DETAILS = ""
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    FN.CURRENCY = "F.CURRENCY"
    F.CURRENCY = ""
    CALL OPF(FN.CURRENCY,F.CURRENCY)

    FN.CUSTOMER = "F.CUSTOMER"
    F.CUSTOMER = ""
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.AA.ACT.HISTORY = "F.AA.ACTIVITY.HISTORY"
    F.AA.ACT.HISTORY  = ""
    CALL OPF(FN.AA.ACT.HISTORY,F.AA.ACT.HISTORY)

    FN.AA.ARR.ACTIVITY = "F.AA.ARRANGEMENT.ACTIVITY"
    F.AA.ARR.ACTIVITY = ""
    CALL OPF(FN.AA.ARR.ACTIVITY,F.AA.ARR.ACTIVITY)

    FN.AA.INT.ACCRUALS = "F.AA.INTEREST.ACCRUALS"
    F.AA.INT.ACCRUALS = ""
    CALL OPF(FN.AA.INT.ACCRUALS, F.AA.INT.ACCRUALS)
    FN.AA.ARR.TERM.AMOUNT = "F.AA.ARR.TERM.AMOUNT"
    F.AA.ARR.TERM.AMOUNT = ""
    CALL OPF(FN.AA.ARR.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT)
    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
RETURN
********
PROCESS:
********
    LOCATE "LINKED.APPL.ID" IN D.FIELDS<1> SETTING LINKED.APPL.ID.POS THEN
        Y.LINKED.APPL.ID = D.RANGE.AND.VALUE<LINKED.APPL.ID.POS>
        COMI = Y.LINKED.APPL.ID
        CALL IN2POSANT(19,'')
        Y.LINKED.APPL.ID = COMI
    END
    R.ACCOUNT = ""; ACC.ERR = ""
    CALL F.READ(FN.ACCOUNT,Y.LINKED.APPL.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    IF R.ACCOUNT THEN
        Y.ARR.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
    END ELSE
        Y.RESULT = "2**************"
        RETURN
    END
    GOSUB GET.ARR.DETAILS
    GOSUB GET.CUST.DETAILS
    GOSUB GET.ACCT.HIS.DETAILS
    Y.AA.RATE = "N/A"
    Y.ARR.ID2 = Y.ARR.ID:"-PRINCIPALINT"
    CALL F.READ(FN.AA.INT.ACCRUALS,Y.ARR.ID2,R.AA.INT.ACCRUALS,F.AA.INT.ACCRUALS,"")
    IF R.AA.INT.ACCRUALS THEN
        Y.AA.RATE = "-"
        Y.AA.RATE = R.AA.INT.ACCRUALS<AA.INT.ACC.RATE,1>
    END
    Y.RESULT := Y.AA.RATE:"*"
    Y.AA.BAL = "N/A"
*CALL REDO.GET.TOTAL.OUTSTANDING.SIN.UNC.UND(Y.ARR.ID,Y.PROP.AMT,Y.TOTAL.AMT)
    CALL APAP.TAM.redoGetTotalOutstandingSinUncUnd(Y.ARR.ID,Y.PROP.AMT,Y.TOTAL.AMT) ;*R22 MANAUAL CODE CONVERSION
    Y.AA.BAL = FIELD(Y.PROP.AMT,@FM,1)
    Y.RESULT := Y.AA.BAL:"*"
    GOSUB GET.PAY.DETAILS
    Y.AA.TERM = "N/A"
    ID.TERM.AMOUNT = Y.ARR.ID:'-COMMITMENT-':Y.START.DATE:'.1'
    CALL F.READ(FN.AA.ARR.TERM.AMOUNT,ID.TERM.AMOUNT,R.AA.ARR.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT,"")

    IF R.AA.ARR.TERM.AMOUNT THEN
        Y.AA.TERM = "-"
        Y.AA.TERM = R.AA.ARR.TERM.AMOUNT<AA.AMT.TERM>
    END

    Y.RESULT := Y.AA.TERM:"*"
    Y.RESULT := Y.AA.BILL.PAY.DATE.FIRST:"*"
    Y.RESULT := Y.AA.BILL.PAY.DATE.LAST:"*"
    Y.RESULT := Y.AA.REPAY.AMT:"*"

    Y.RESULT := Y.AA.BILL.DUE:"*"
    Y.AA.LOAN.STATUS = "N/A"
    PROP.CLASS = 'OVERDUE'
    PROPERTY = ''
    R.Condition = ''
    ERR.MSG = ''
    EFF.DATE = ''
*CALL REDO.CRR.GET.CONDITIONS(Y.ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.Condition,ERR.MSG)
    CALL APAP.AA.redoCrrGetConditions(Y.ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.Condition,ERR.MSG) ;*R22 MANUAL CODE CONVERSION
    Y.AA.LOAN.STATUS = R.Condition<AA.OD.LOCAL.REF,POS.L.LOAN.STATUS>

    CALL GET.LOC.REF("AA.ARR.OVERDUE", "L.LOAN.COND", L.LOAN.COND.POS)

    Y.RESULT := Y.AA.LOAN.STATUS:"*"
    Y.RESULT := Y.AA.PRODUCT:"*"
    Y.RESULT := Y.AA.DATE.LAST.PAY:"*"
    Y.RESULT := R.Condition<AA.OD.LOCAL.REF, L.LOAN.COND.POS> : "*"

    R.DATA = Y.RESULT

RETURN

****************
GET.PAY.DETAILS:
****************
    Y.AA.MNTPAY = "N/A"
    Y.AA.BILL.DUE.AMT = "N/A"
    Y.AA.BILL.PAY.DATE.FIRST = "N/A"
    Y.AA.BILL.PAY.DATE.LAST = "N/A"
    Y.AA.BILL.DUE = "N/A"
    CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.ARR.ID,Y.AA.ACCOUNT.DETAILS.REC,F.AA.ACCOUNT.DETAILS,AA.ACCOUNT.DETAILS.ERR)
    IF Y.AA.ACCOUNT.DETAILS.REC THEN
        Y.BILL.PAY.DATES = Y.AA.ACCOUNT.DETAILS.REC<AA.AD.BILL.PAY.DATE>
        Y.DATACOUNT = DCOUNT(Y.BILL.PAY.DATES,@VM)
        Y.AA.BILL.PAY.DATE.FIRST = FIELD(Y.BILL.PAY.DATES,@VM,1)
        Y.AA.BILL.PAY.DATE.LAST = FIELD(Y.BILL.PAY.DATES,@VM,Y.DATACOUNT)
        GOSUB GET.PAY.DETAILS.2
    END
    Y.RESULT := Y.AA.MNTPAY:"*"
    Y.RESULT := Y.AA.BILL.DUE.AMT:"*"

RETURN
******************
GET.PAY.DETAILS.2:
******************
    Y.AA.MNTPAY = 0
    Y.AA.BILL.DUE = 0
    Y.AA.BILL.DUE.AMT = 0
    I.VAR = 1
    LOOP
    WHILE I.VAR LE Y.DATACOUNT
        BILL.REFERENCE = Y.AA.ACCOUNT.DETAILS.REC<AA.AD.BILL.ID,I.VAR>
        IF Y.AA.ACCOUNT.DETAILS.REC<AA.AD.BILL.TYPE,I.VAR,1> EQ 'PAYMENT' AND Y.AA.ACCOUNT.DETAILS.REC<AA.AD.SET.STATUS,I.VAR,1> EQ 'UNPAID' THEN
            Y.AA.BILL.DUE += 1
            CALL AA.GET.BILL.DETAILS(Y.ARR.ID, BILL.REFERENCE, BILL.DETAILS, RET.ERROR)
            
            
            Y.PROPERTY.TOT = DCOUNT(BILL.DETAILS<AA.BD.PROPERTY>,@VM)
            GOSUB SUM.AMOUNTS
            LOCATE 'PRMORA' IN BILL.DETAILS<AA.BD.PROPERTY,1> SETTING Y.POS THEN
                Y.AA.BILL.DUE.AMT += SUM(BILL.DETAILS<AA.BD.OS.PROP.AMOUNT,Y.POS>)
            END
        END
        I.VAR += 1
    REPEAT
RETURN
************
SUM.AMOUNTS:
************
    K.VAR = 1
    LOOP
    WHILE K.VAR LE Y.PROPERTY.TOT
        Y.AA.MNTPAY += SUM(BILL.DETAILS<AA.BD.OS.PROP.AMOUNT,K.VAR>)
        K.VAR += 1
    REPEAT
RETURN
****************
GET.ARR.DETAILS:
****************
    Y.CURRENCY = "N/A"
    Y.AA.PRODUCT = "N/A"
    IF Y.ARR.ID NE "" THEN
        CALL F.READ(FN.AA.ARRANGEMENT,Y.ARR.ID,Y.AA.ARRANGEMENT.REC,F.AA.ARRANGEMENT,AA.ARRANGEMENT.ERR)
        IF Y.AA.ARRANGEMENT.REC THEN
            Y.RESULT = "1*"
            Y.CURRENCY = Y.AA.ARRANGEMENT.REC<AA.ARR.CURRENCY>
            CALL CACHE.READ(FN.CURRENCY, Y.CURRENCY, Y.CURRENCY.REC, CURRENCY.ERR)	;*R22 Auto Conversion  - F.READ to CACHE.READ
            IF Y.CURRENCY.REC THEN
                Y.CURRENCY = Y.CURRENCY.REC<EB.CUR.CCY.NAME>
            END
            Y.AA.PRODUCT = Y.AA.ARRANGEMENT.REC<AA.ARR.PRODUCT>
            Y.AA.CUSTOMER = Y.AA.ARRANGEMENT.REC<AA.ARR.CUSTOMER>
            Y.START.DATE = Y.AA.ARRANGEMENT.REC<AA.ARR.START.DATE>
        END ELSE
            Y.RESULT = "2**************"
            RETURN
        END
    END ELSE
        Y.RESULT = "2**************"
        RETURN
    END

RETURN
*****************
GET.CUST.DETAILS:
*****************
    Y.RESULT := Y.CURRENCY:"*"
    Y.AA.PRIM.OWNER = "N/A"
    R.CUSTOMER = ""; CUS.ERR = ""
    CALL F.READ(FN.CUSTOMER,Y.AA.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
    IF R.CUSTOMER THEN
        Y.AA.PRIM.OWNER = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:" ":R.CUSTOMER<EB.CUS.FAMILY.NAME>
    END
    Y.RESULT := Y.AA.PRIM.OWNER:"*"
RETURN
*********************
GET.ACCT.HIS.DETAILS:
*********************
    Y.AA.EFFDATE = "N/A"
    Y.ACTIVITY = "LENDING-DISBURSE-COMMITMENT"
    Y.AA.REPAY.AMT = "N/A"
    Y.ACT.REPAY = "APPLYPAYMENT"
    R.AA.ACT.HISTORY = ""
    CALL F.READ(FN.AA.ACT.HISTORY,Y.ARR.ID,R.AA.ACT.HISTORY,F.AA.ACT.HISTORY,"")
    IF R.AA.ACT.HISTORY THEN
        Y.CONT = DCOUNT(R.AA.ACT.HISTORY<AA.AH.EFFECTIVE.DATE>,@VM)
        Y.AA.EFFDATE = "-"
        GOSUB GET.DISBURSE.DATE
        Y.AA.REPAY.AMT = 0
        Y.LAST.PAY = 0
        I.VAR = 1
        LOOP
        WHILE I.VAR LE Y.CONT
            Y.ACT.TO.EVAL.TOT = R.AA.ACT.HISTORY<AA.AH.ACTIVITY,I.VAR>
            Y.ACT.TO.EVAL.CNT = DCOUNT(Y.ACT.TO.EVAL.TOT,@SM)
            GOSUB GET.ACCT.HIS.DETAILS.2
            I.VAR += 1
        REPEAT
    END
    Y.RESULT := Y.AA.EFFDATE:"*"
RETURN
***********************
GET.ACCT.HIS.DETAILS.2:
***********************
    J.VAR = 1
    LOOP
    WHILE J.VAR LE Y.ACT.TO.EVAL.CNT
        Y.ACT.TO.EVAL = FIELD(Y.ACT.TO.EVAL.TOT,@SM,J.VAR)
        Y.ACT.TO.EVAL = FIELD(Y.ACT.TO.EVAL,"-",2)
        IF Y.ACT.TO.EVAL EQ Y.ACT.REPAY THEN
            Y.LAST.PAY += 1
            IF Y.LAST.PAY EQ 1 THEN
                Y.AA.DATE.LAST.PAY = R.AA.ACT.HISTORY<AA.AH.EFFECTIVE.DATE,I.VAR>
                Y.AA.REPAY.AMT = R.AA.ACT.HISTORY<AA.AH.ACTIVITY.AMT,I.VAR,J.VAR>
            END
        END
        J.VAR += 1
    REPEAT
RETURN
*-----------------
GET.DISBURSE.DATE:
*-----------------
    I.VAR = 1
    LOOP
    WHILE I.VAR LE Y.CONT
        Y.ACTIVITY.TOT = R.AA.ACT.HISTORY<AA.AH.ACTIVITY,I.VAR>
        Y.ACTIVITY.TOT.CNT = DCOUNT(Y.ACTIVITY.TOT,@SM)
        J.VAR = 1
        LOOP
        WHILE J.VAR LE Y.ACTIVITY.TOT.CNT
            Y.ACT.TO.EVAL = FIELD(Y.ACTIVITY.TOT,@SM,J.VAR)
            IF Y.ACTIVITY EQ Y.ACT.TO.EVAL THEN
                Y.AA.EFFDATE = R.AA.ACT.HISTORY<AA.AH.EFFECTIVE.DATE,I.VAR>
                RETURN
            END
            J.VAR += 1
        REPEAT
        I.VAR += 1
    REPEAT
    Y.ACTIVITY = "LENDING-TAKEOVER-ARRANGEMENT"
    I.VAR = 1
    LOOP
    WHILE I.VAR LE Y.CONT
        J.VAR = 1
        LOOP
        WHILE J.VAR LE Y.ACTIVITY.TOT.CNT
            Y.ACT.TO.EVAL = FIELD(Y.ACTIVITY.TOT,@SM,J.VAR)
            IF Y.ACTIVITY EQ Y.ACT.TO.EVAL THEN
                Y.AA.ACT.ID = R.AA.ACT.HISTORY<AA.AH.ACTIVITY.REF,I.VAR,J.VAR>
                R.AA.ARR.ACTIVITY = ""; ARR.ERR = ""
                CALL F.READ(FN.AA.ARR.ACTIVITY,Y.AA.ACT.ID,R.AA.ARR.ACTIVITY,F.AA.ARR.ACTIVITY,ARR.ERR)
                IF R.AA.ARR.ACTIVITY THEN
                    Y.AA.EFFDATE = R.AA.ARR.ACTIVITY<AA.ARR.ACT.ORIG.CONTRACT.DATE>
                    RETURN
                END
            END
            J.VAR += 1
        REPEAT
        I.VAR += 1
    REPEAT
RETURN
END
