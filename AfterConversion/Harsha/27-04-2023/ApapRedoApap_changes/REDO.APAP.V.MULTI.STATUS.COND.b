* @ValidationCode : MjoxODkzNzYzODYzOkNwMTI1MjoxNjgyNTc1MTAwNDA4OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 11:28:20
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE  REDO.APAP.V.MULTI.STATUS.COND
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.VAL.CREDIT.ACCOUNT
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is a validation routine for payoff version and this routine is attached as routine to the ACCOUNT.2
*field of teller versions used for payment.  The functionality of this routine is to display the values in TELLER local reference fields
*L.LOAN.STATUS.1 and L.LOAN.COND for the given arrangement id by fetching the values from AA.OVERDUE application
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 3-JUN-2010        Prabhu.N       ODR-2010-01-0081    Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*18-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM, SM to @SM ,VAR.INTEREST.DUE=VAR.INTEREST.DUE+Y.LIM.BALANCE to VAR.INTEREST.DUE += Y.LIM.BALANCE
*18-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------



*-------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.OVERDUE
    $INSERT I_GTS.COMMON
    $INSERT I_EB.TRANS.COMMON
    $USING APAP.TAM

    IF VAL.TEXT EQ '' THEN
        GOSUB INIT
    END
RETURN
*----
INIT:
*----
    VAR.ACCOUNT.ID = ECOMI

    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        LREF.APP='FUNDS.TRANSFER'
        LREF.FIELDS='L.PRINC.AMT.DUE':@VM:'L.INT.AMT.DUE'
        LREF.POS=''
        CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)

        GOSUB PROCESS

        R.NEW(FT.CREDIT.AMOUNT)=VAR.OS.AMOUNT
        R.NEW(FT.LOCAL.REF)<1,LREF.POS<1,1>>=VAR.PRINCIPAL.DUE
        R.NEW(FT.LOCAL.REF)<1,LREF.POS<1,2>>=VAR.INTEREST.DUE
    END
    IF APPLICATION EQ 'TELLER' THEN
        LREF.APP='TELLER'
        LREF.FIELDS='L.PRINC.AMT.DUE':@VM:'L.INT.AMT.DUE'
        LREF.POS=''
        CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)

        GOSUB PROCESS
        R.NEW(TT.TE.AMOUNT.LOCAL.1)=VAR.OS.AMOUNT
        R.NEW(TT.TE.LOCAL.REF)<1,LREF.POS<1,1>>=VAR.PRINCIPAL.DUE
        R.NEW(TT.TE.LOCAL.REF)<1,LREF.POS<1,2>>=VAR.INTEREST.DUE
    END
    GOSUB MULTI.GET.STATUS.COND
    GOSUB GOEND
RETURN
*-------
PROCESS:
*-------
    FN.EB.CONTRACT.BALANCES='F.EB.CONTRACT.BALANCES'
    F.EB.CONTRACT.BALANCES=''
    CALL OPF(FN.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES)

    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT=''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,VAR.ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ERR)
    VAR.AA.ID=R.ACCOUNT<AC.ARRANGEMENT.ID>
    CALL F.READ(FN.AA.ARRANGEMENT,VAR.AA.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,READ.ERR)
    BALANCE.TYPE='DUEACCOUNT'
    GOSUB CALC.PRIN.DUE
    BALANCE.TYPE='DUEPRINCIPALINT'
    GOSUB CALC.INT.DUE
    BALANCE.TYPE='GRCACCOUNT'
    GOSUB CALC.PRIN.DUE
    BALANCE.TYPE='GRCPRINCIPALINT'
    GOSUB CALC.INT.DUE
    BALANCE.TYPE='DELACCOUNT'
    GOSUB CALC.PRIN.DUE
    BALANCE.TYPE='DELPRINCIPALINT'
    GOSUB CALC.INT.DUE
    BALANCE.TYPE='NABACCOUNT'
    GOSUB CALC.PRIN.DUE
    BALANCE.TYPE='NABPRINCIPALINT'
    GOSUB CALC.INT.DUE
    BALANCE.TYPE='DUEPENALTYINT'
    GOSUB CALC.OTHER.BAL
    BALANCE.TYPE='GRCPENALTYINT'
    GOSUB CALC.OTHER.BAL
    BALANCE.TYPE='DELPENALTYINT'
    GOSUB CALC.OTHER.BAL
    BALANCE.TYPE='NABPENALTYINT'
    GOSUB CALC.OTHER.BAL
    BALANCE.TYPE='CURACCOUNT'
    GOSUB CALC.OTHER.BAL
    BALANCE.TYPE='ACCPRINCIPALINT'
    GOSUB CALC.OTHER.BAL
    VAR.OS.AMOUNT=VAR.PRINCIPAL.DUE+VAR.INTEREST.DUE+VAR.OTHER.BAL
RETURN
*-------------
CALC.PRIN.DUE:
*-------------
    Y.BAL.AMOUNT=''
    Y.REQ.TYPE = ''
    START.DATE= R.AA.ARRANGEMENT<AA.ARR.START.DATE>
    END.DATE=TODAY
    CHARGEOFF.TYPE = ''
    EFFECTIVE.DATE = TODAY
    CALL AC.GET.PERIOD.BALANCES(VAR.ACCOUNT.ID,BALANCE.TYPE, Y.REQ.TYPE, START.DATE, END.DATE ,EFFECTIVE.DATE,CHARGEOFF.TYPE, Y.BAL.AMOUNT, ERR.MSG)
    Y.LIM.BALANCE = Y.BAL.AMOUNT<4>
    CHANGE @VM TO @FM IN Y.LIM.BALANCE
    Y.LIM.BALANCE.SIZE=DCOUNT(Y.LIM.BALANCE,@FM)
    Y.LIM.BALANCE=Y.LIM.BALANCE<Y.LIM.BALANCE.SIZE>
    Y.LIM.BALANCE = ABS(Y.LIM.BALANCE)
    VAR.PRINCIPAL.DUE += Y.LIM.BALANCE
    Y.BAL.AMOUNT=''
RETURN
*------------
CALC.INT.DUE:
*------------
    Y.BAL.AMOUNT=''
    Y.REQ.TYPE = ''
    START.DATE= R.AA.ARRANGEMENT<AA.ARR.START.DATE>
    END.DATE=TODAY
    CHARGEOFF.TYPE = ''
    EFFECTIVE.DATE = TODAY
    CALL AC.GET.PERIOD.BALANCES(VAR.ACCOUNT.ID,BALANCE.TYPE, Y.REQ.TYPE, START.DATE, END.DATE ,EFFECTIVE.DATE,CHARGEOFF.TYPE, Y.BAL.AMOUNT, ERR.MSG)
    Y.LIM.BALANCE = Y.BAL.AMOUNT<4>
    CHANGE @VM TO @FM IN Y.LIM.BALANCE
    Y.LIM.BALANCE.SIZE=DCOUNT(Y.LIM.BALANCE,@FM)
    Y.LIM.BALANCE=Y.LIM.BALANCE<Y.LIM.BALANCE.SIZE>
    Y.LIM.BALANCE = ABS(Y.LIM.BALANCE)
    VAR.INTEREST.DUE += Y.LIM.BALANCE ;*R22 AUTO CODE CONVERSION
    Y.BAL.AMOUNT=''
RETURN
*------------
CALC.OTHER.BAL:
*------------
    Y.BAL.AMOUNT=''
    Y.REQ.TYPE = ''
    START.DATE= R.AA.ARRANGEMENT<AA.ARR.START.DATE>
    END.DATE=TODAY
    CHARGEOFF.TYPE = ''
    EFFECTIVE.DATE = TODAY
    CALL AC.GET.PERIOD.BALANCES(VAR.ACCOUNT.ID,BALANCE.TYPE, Y.REQ.TYPE, START.DATE, END.DATE ,EFFECTIVE.DATE, CHARGEOFF.TYPE, Y.BAL.AMOUNT, ERR.MSG)
    Y.LIM.BALANCE = Y.BAL.AMOUNT<4>
    CHANGE @VM TO @FM IN Y.LIM.BALANCE
    Y.LIM.BALANCE.SIZE=DCOUNT(Y.LIM.BALANCE,@FM)
    Y.LIM.BALANCE=Y.LIM.BALANCE<Y.LIM.BALANCE.SIZE>
    Y.LIM.BALANCE = ABS(Y.LIM.BALANCE)
    VAR.OTHER.BAL += Y.LIM.BALANCE
    Y.BAL.AMOUNT=''
RETURN

*----------------------
MULTI.GET.STATUS.COND:


*    IF OFS$OPERATION EQ 'VALIDATE' THEN
*        RETURN
*    END
*Return in Commit stage
    IF cTxn_CommitRequests EQ '1' THEN
        RETURN
    END
    IF MESSAGE EQ 'VAL' THEN
        RETURN
    END

    GOSUB GET.LRF.POS1
    GOSUB PROCESS1
RETURN

*--------------
GET.LRF.POS1:
*----------------------------------------------------------------------
* This section gets the position of the local reference field positions
*----------------------------------------------------------------------

    LR.APP = 'AA.PRD.DES.OVERDUE':@FM:'TELLER'
    LR.FLDS = 'L.LOAN.STATUS.1':@VM:'L.LOAN.COND':@FM
    LR.FLDS := 'L.LOAN.STATUS.1':@VM:'L.LOAN.COND'
    LR.POS = ''
    CALL MULTI.GET.LOC.REF(LR.APP,LR.FLDS,LR.POS)

    OD.LOAN.STATUS.POS = LR.POS<1,1>
    OD.LOAN.COND.POS =  LR.POS<1,2>
    TT.LOAN.STATUS.POS = LR.POS<2,1>
    TT.LOAN.COND.POS =  LR.POS<2,2>

RETURN

*--------------
PROCESS1:

*--------------------------------------------------------------------------------------------------------------------------------------
* This section gets the latest overdue record for the arrangement id and stores the value of loan status and condition in R.NEW of TELLER
*--------------------------------------------------------------------------------------------------------------------------------------

    ARR.ID =  VAR.ACCOUNT.ID
    PROP.CLASS = 'OVERDUE'
    PROPERTY = ''
    R.Condition = ''
    ERR.MSG = ''
    EFF.DATE = ''
    CALL APAP.TAM.redoCrrGetConditions(ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.Condition,ERR.MSG)
    LOAN.STATUS = R.Condition<AA.OD.LOCAL.REF,OD.LOAN.STATUS.POS>
    LOAN.COND = R.Condition<AA.OD.LOCAL.REF,OD.LOAN.COND.POS>
    CHANGE @SM TO @VM IN LOAN.STATUS
    CHANGE @SM TO @VM IN LOAN.COND
    R.NEW(TT.TE.LOCAL.REF)<1,TT.LOAN.STATUS.POS> = LOAN.STATUS
    R.NEW(TT.TE.LOCAL.REF)<1,TT.LOAN.COND.POS> = LOAN.COND
RETURN

*---------
GOEND:
END
