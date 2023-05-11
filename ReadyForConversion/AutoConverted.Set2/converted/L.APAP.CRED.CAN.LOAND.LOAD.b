*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.CRED.CAN.LOAND.LOAD
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INSERT T24.BP I_F.DATES
    $INSERT LAPAP.BP L.APAP.CRED.CAN.LOAND.COMMON
    $INSERT T24.BP I_F.AA.ACTIVITY.HISTORY
    $INSERT T24.BP I_F.AA.OVERDUE
    $INSERT T24.BP I_F.AA.ACCOUNT
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_F.TELLER
    $INSERT LAPAP.BP I_F.REDO.APAP.CREDIT.CARD.DET
    $INSERT T24.BP I_F.AA.LIMIT
    $INSERT T24.BP I_F.AA.INTEREST
    $INSERT T24.BP I_F.AA.TERM.AMOUNT
    $INSERT T24.BP I_F.AA.ACCOUNT.DETAILS
    $INSERT T24.BP I_F.AA.PAYMENT.SCHEDULE
    $INSERT T24.BP I_F.AA.INTEREST.ACCRUALS
    $INSERT T24.BP I_F.AA.CUSTOMER
    $INSERT T24.BP I_F.EB.CONTRACT.BALANCES
    $INSERT BP I_F.ST.LAPAP.REPORTDE16

    GOSUB MAIN.PROCESO

MAIN.PROCESO:
    GOSUB GET.LOARD.TABLE
    GOSUB GET.CONSTANTE.VAR
    GOSUB GET.CAMPOS.LOCALES
    RETURN
GET.LOARD.TABLE:
*****************
    FN.AA.ARRANGEMENT = "F.AA.ARRANGEMENT"; F.AA.ARRANGEMENT  = ""
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.ACCOUNT = "F.ACCOUNT"; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = "F.CUSTOMER"; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    FN.AA.ACTIVITY.HISTORY = "F.AA.ACTIVITY.HISTORY"; F.AA.ACTIVITY.HISTORY  = ""
    CALL OPF(FN.AA.ACTIVITY.HISTORY,F.AA.ACTIVITY.HISTORY)

    FN.AA.ARRANGEMENT.ACTIVITY = "F.AA.ARRANGEMENT.ACTIVITY"; F.AA.ARRANGEMENT.ACTIVITY = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY,F.AA.ARRANGEMENT.ACTIVITY)

    FN.DATES = 'F.DATES'; F.DATES = ''
    CALL OPF(FN.DATES,F.DATES)

    FN.FUNDS.TRANSFER.HST = 'F.FUNDS.TRANSFER$HIS' ; F.FUNDS.TRANSFER.HST = ''
    CALL OPF(FN.FUNDS.TRANSFER.HST,F.FUNDS.TRANSFER.HST)

    FN.TELLERHIS = "F.TELLER$HIS" ; F.TELLERHIS = ''
    CALL OPF(FN.TELLERHIS,F.TELLERHIS)

    FN.REDO.APAP.CREDIT.CARD.DET = 'F.REDO.APAP.CREDIT.CARD.DET'; F.REDO.APAP.CREDIT.CARD.DET = ''
    CALL OPF(FN.REDO.APAP.CREDIT.CARD.DET,F.REDO.APAP.CREDIT.CARD.DET)

    FN.ACCOUNT.HST = 'F.ACCOUNT$HIS'
    F.ACCOUNT.HST = ''
    CALL OPF(FN.ACCOUNT.HST,F.ACCOUNT.HST)

    FN.EB.CONT.BAL = "F.EB.CONTRACT.BALANCES"
    F.EB.CONT.BAL = ""
    CALL OPF(FN.EB.CONT.BAL,F.EB.CONT.BAL)

    FN.LIMIT = "F.LIMIT"
    F.LIMIT  = ""
    CALL OPF(FN.LIMIT,F.LIMIT)

    FN.AA.LIMIT = 'F.AA.ARR.LIMIT'
    F.AA.LIMIT = ''
    R.AA.LIMIT = ''
    CALL OPF(FN.AA.LIMIT,F.AA.LIMIT)

    FN.AA.ARRANGEMENT = "F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT  = ""
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.ARR.TERM.AMOUNT = "F.AA.ARR.TERM.AMOUNT"
    F.AA.ARR.TERM.AMOUNT  = ""
    CALL OPF(FN.AA.ARR.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT)

    FN.AA.PRODUCT.GROUP = "F.AA.PRODUCT.GROUP"
    F.AA.PRODUCT.GROUP  = ""
    CALL OPF(FN.AA.PRODUCT.GROUP,F.AA.PRODUCT.GROUP)

    FN.LAPAP.REPORTDE16 = "F.ST.LAPAP.REPORTDE16"
    F.LAPAP.REPORTDE16 = ''
    CALL OPF(FN.LAPAP.REPORTDE16,F.LAPAP.REPORTDE16)

    RETURN

GET.CONSTANTE.VAR:
    Y.APAP.REP.PARAM.ID = 'REDO.DE16'
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.APAP.REP.PARAM.ID,R.REDO.H.REPORTS.PARAM,REDO.H.REPORTS.PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        Y.OUTPUT.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
        Y.FILE.NAME  = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        Y.FILE.DIR   = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.TEMP.DIR>
        Y.FIELD.NME.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
        Y.FIELD.VAL.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
        Y.DISP.TEXT.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>
    END
    CHANGE VM TO FM IN Y.FIELD.NME.ARR
    CHANGE SM TO FM IN Y.FIELD.NME.ARR
    CHANGE VM TO FM IN Y.FIELD.VAL.ARR
    CHANGE SM TO FM IN Y.FIELD.VAL.ARR
    CHANGE VM TO FM IN Y.DISP.TEXT.ARR
    CHANGE SM TO FM IN Y.DISP.TEXT.ARR

    LOCATE "RP.PAYOFF.CHQ" IN Y.FIELD.VAL.ARR<1> SETTING EYPO.POS THEN
        Y.RP.PAYOFF.CHQ =  Y.DISP.TEXT.ARR<EYPO.POS,1>
    END
    LOCATE "RP.PAYOFF" IN Y.FIELD.VAL.ARR<1> SETTING EYPO.POS THEN
        Y.RP.PAYOFF =  Y.DISP.TEXT.ARR<EYPO.POS,1>
    END
    R.FECHAS = ''; ERR.FECHAS = ''
    CALL F.READ(FN.DATES,'DO0010001',R.FECHAS,FV.DATES,ERR.FECHAS)
    Y.FECHA.CORTE = R.FECHAS<EB.DAT.LAST.WORKING.DAY>
    Y.FECHA1 = Y.FECHA.CORTE[1,6]
    Y.FECHA2 = '01'
    Y.FECHA.INICIO = Y.FECHA1:"":Y.FECHA2
    RETURN
*------------------
GET.CAMPOS.LOCALES:
*------------------
    Y.APPL = "AA.PRD.DES.ACCOUNT":FM:"AA.PRD.DES.OVERDUE":FM:"CUSTOMER"
    Y.FLD  = "L.CR.FACILITY":FM:"L.LOAN.STATUS.1":VM:"L.LOAN.COND":FM:"L.CU.DEBTOR"
    Y.POS  = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)
    Y.L.CR.FACILITY.POS = Y.POS<1,1>
    Y.L.LOAN.STATUS.1.POS = Y.POS<2,1>
    Y.L.LOAN.COND.POST = Y.POS<2,2>
    L.CU.DEBTOR.POS = Y.POS<3,1>
    RETURN
END
