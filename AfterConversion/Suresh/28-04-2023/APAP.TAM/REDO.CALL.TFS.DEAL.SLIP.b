* @ValidationCode : MjotODIxNTg5MDI6Q3AxMjUyOjE2ODI2Nzg2OTk5MTg6MzMzc3U6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 16:14:59
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
 

*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------------
* Modification History:
*
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*04/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*04/04/2023         SURESH           MANUAL R22 CODE CONVERSION        CALL routine format modified
*-----------------------------------------------------------------------------------
* <Rating>-69</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.CALL.TFS.DEAL.SLIP(Y.INP.DEAL)
*-------------------------------------------------------------
*Description: This routine is call routine from deal slip of TFS

*-------------------------------------------------------------
*Input Arg : Y.INP.DEAL
*Out Arg   : Y.INP.DEAL
*Deals With: TFS payement
*-------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CURRENCY
    $INSERT I_TT.COMMON
    $INSERT I_F.TELLER
    $INSERT I_T24.FS.COMMON
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.TRANSACTION
    $INSERT I_REDO.TFS.DEAL.SLIP.COMMON

    GOSUB GET.LOC.REF
    GOSUB PROCESS
RETURN
*-------------------------------------------------------------
GET.LOC.REF:
*-------------------------------------------------------------
    LOC.REF.APPLICATION = "ACCOUNT":@FM:"T24.FUND.SERVICES"
    LOC.REF.FIELDS      = 'L.AC.ALPH.AC.NO':@FM:'L.TT.NO.OF.CHQ':@VM:'L.NCF.NUMBER'
    LOC.REF.POS         = ''
    CALL APAP.TAM.MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS) ;*MANUAL R22 CODE CONVERSION
    POS.L.AC.ALPH.AC.NO = LOC.REF.POS<1,1>
    POS.L.TT.NO.OF.CHQ  = LOC.REF.POS<2,1>
    POS.L.NCF.NUMBER    = LOC.REF.POS<2,2>

    Y.TRANSACTION.CODE = R.NEW(TFS.TRANSACTION)


RETURN
*-------------------------------------------------------------
PROCESS:
*-------------------------------------------------------------
    Y.FIELD.NAME = Y.INP.DEAL

    IF Y.FIELD.NAME EQ 'Y.CUR.TIME' THEN

        GET.DATE.TIME = R.NEW(TFS.DATE.TIME)
        F1 = GET.DATE.TIME[1,2]
        F2 = GET.DATE.TIME[3,2]
        F3 = GET.DATE.TIME[5,2]
        F4 = GET.DATE.TIME[7,2]
        F5 = GET.DATE.TIME[9,2]
        Y.CUR.TIME = F3:'/':F2:'/':F1:'-':F4:':':F5
        Y.INP.DEAL = FMT(Y.CUR.TIME,'R#30')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.TELLER.ID' THEN
        GOSUB GET.TELLER.ID
        Y.INP.DEAL = FMT(Y.TELLER.ID,'R#30')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.MONTO.NCF' THEN
        Y.INP.DEAL = '' ;* NCF Amount not calculated as of now.
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.TOTAL.AMT' THEN
        GOSUB CALC.AMOUNT
        Y.INP.DEAL = FMT(Y.TOTAL.AMT,"R2,#24")        ;* Total  Txn amount.
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.CASH.AMT' THEN
        GOSUB CALC.AMOUNT
        Y.INP.DEAL = FMT(Y.CASH.AMT,"R2,#24")
        RETURN

    END
    IF Y.FIELD.NAME EQ 'Y.CHEQUE.AMT' THEN
        GOSUB CALC.AMOUNT
        Y.INP.DEAL = FMT(Y.CHEQUE.AMT,"R2,#24")
        RETURN

    END
    IF Y.FIELD.NAME EQ 'Y.TRANS.DESC' THEN
        GOSUB CALC.AMOUNT
        Y.INP.DEAL = FMT(Y.TRANS.DESC,'R#30')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.CURRENCY' THEN
        GOSUB GET.CURRENCY
        Y.INP.DEAL = FMT(Y.CURRENCY,'R#30')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.TRANS.REF' THEN
        Y.TRANS.REF = FIELD(ID.NEW,'-',1)
        Y.INP.DEAL  = FMT(Y.TRANS.REF,'R#24')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.ACCOUNT' THEN
        Y.PRIMARY.ACCOUNT = R.NEW(TFS.PRIMARY.ACCOUNT)
        Y.INP.DEAL        = FMT(Y.PRIMARY.ACCOUNT,'R#24')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.AC.ALPH.ACC' THEN
        GOSUB GET.AC.ALPH.ACC
        Y.INP.DEAL = FMT(Y.L.AC.ALPH.AC.NO,'R#30')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'Y.NAME1' THEN
        GOSUB GET.AC.ALPH.ACC
        Y.INP.DEAL = FMT(Y.NAME.1,'R#30')
        RETURN
    END
    IF Y.FIELD.NAME EQ 'Y.NAME2' THEN
        GOSUB GET.AC.ALPH.ACC
        Y.INP.DEAL = FMT(Y.NAME.2,'R#30')
        RETURN
    END
    IF Y.FIELD.NAME EQ 'CHEQUE.NO' THEN
        Y.L.TT.NO.OF.CHQ = R.NEW(TFS.LOCAL.REF)<1,POS.L.TT.NO.OF.CHQ>
        Y.INP.DEAL = FMT(Y.L.TT.NO.OF.CHQ,'R#24')
        RETURN
    END

    IF Y.FIELD.NAME EQ 'NCF.NUMBER' THEN
        Y.L.NCF.NUMBER   = R.NEW(TFS.LOCAL.REF)<1,POS.L.NCF.NUMBER>
        Y.INP.DEAL = Y.L.NCF.NUMBER
        RETURN
    END
RETURN
*-----------------------------------------------------------
GET.TELLER.ID:
*-----------------------------------------------------------
* Teller Id Info.

    FN.TT = 'F.TELLER'
    F.TT  = ''
    CALL OPF(FN.TT,F.TT)

    FN.TT.NAU = 'F.TELLER$NAU'
    F.TT.NAU  = ''
    CALL OPF(FN.TT.NAU,F.TT.NAU)

    FN.TELLER.HIS = 'F.TELLER$HIS'
    F.TELLER.HIS = ''
    CALL OPF(FN.TELLER.HIS, F.TELLER.HIS)

    IF R.COMPANY(EB.COM.COMPANY.NAME)<1,LNGG> EQ '' THEN
        Y.COMP.DESB = R.COMPANY(EB.COM.COMPANY.NAME)<1,1>
    END ELSE
        Y.COMP.DESB = R.COMPANY(EB.COM.COMPANY.NAME)<1,LNGG>
    END
******
* Here we get the Teller Id from the underlying teller record.
******
    Y.UNDER.TT = R.NEW(TFS.UNDERLYING)<1,1>
    CALL F.READ(FN.TT,Y.UNDER.TT,R.TT,F.TT,TT.ERR)

    IF R.TT THEN
        Y.TELLER.ID = Y.COMP.DESB :' - ': R.TT<TT.TE.TELLER.ID.1>
        RETURN
    END

* Fix for PACS00333423
    ELSE
        Y.UNDER.TT.HIS.ID = Y.UNDER.TT
        R.TT.HIS = ''
        Y.READ.ERR = ''
        CALL EB.READ.HISTORY.REC(F.TELLER.HIS, Y.UNDER.TT.HIS.ID, R.TT.HIS, Y.READ.ERR)
        Y.TELLER.ID =   Y.COMP.DESB :' - ': R.TT.HIS<TT.TE.TELLER.ID.1>
        RETURN
    END
* End of fix for PACS00333423
    CALL F.READ(F.TT.NAU,Y.UNDER.TT,R.TT.NAU,F.TT.NAU,TT.NAU.ERR)
    IF R.TT.NAU THEN
        Y.TELLER.ID = Y.COMP.DESB :' - ': R.TT.NAU<TT.TE.TELLER.ID.1>
        RETURN
    END

    Y.TELLER.ID = Y.COMP.DESB :' - ': FIELD(R.NEW(TFS.INPUTTER),'_',2)

RETURN
*----------------------------------------------
CALC.AMOUNT:
*----------------------------------------------
* Here we calculate the total amount, cash & cheque amount

    Y.CASH.TRANS   = ''
    Y.CHEQUE.TRANS = ''
    Y.CASH.AMT     = 0
    Y.CHEQUE.AMT   = 0
    Y.VAR1 = 1
    Y.TRANSACTION.CNT = DCOUNT(Y.TRANSACTION.CODE,@VM)
    LOOP
    WHILE Y.VAR1 LE Y.TRANSACTION.CNT
        Y.TRANS = Y.TRANSACTION.CODE<1,Y.VAR1>
        IF Y.TRANS EQ 'CASHDEP' OR Y.TRANS EQ 'FCASHDEP' OR Y.TRANS EQ 'CASHDEPD' THEN
            Y.CASH.TRANS = 'YES'
            Y.CASH.AMT += R.NEW(TFS.AMOUNT)<1,Y.VAR1>
        END
        IF Y.TRANS EQ 'CHQDEP' OR Y.TRANS EQ 'FCHQDEP' THEN
            Y.CHEQUE.TRANS = 'YES'
            Y.CHEQUE.AMT += R.NEW(TFS.AMOUNT)<1,Y.VAR1>
        END

        Y.VAR1++
    REPEAT
    BEGIN CASE
        CASE Y.CASH.TRANS EQ 'YES' AND Y.CHEQUE.TRANS EQ 'YES'
            Y.TRANS.DESC = 'DEPOSITO EN CHEQUE - MIXTO'
        CASE Y.CASH.TRANS EQ '' AND Y.CHEQUE.TRANS EQ 'YES'
            Y.TRANS.DESC = 'DEPOSITO EN CHEQUE'
        CASE Y.CASH.TRANS EQ 'YES' AND Y.CHEQUE.TRANS EQ ''
            Y.TRANS.DESC = 'DEPOSITO EN EFFECTIVO'
    END CASE

    Y.TOTAL.AMT = Y.CASH.AMT + Y.CHEQUE.AMT

RETURN
*-------------------------------------------------------
GET.CURRENCY:
*-------------------------------------------------------
* Get the Currency Information.
    FN.CURRENCY = 'F.CURRENCY'
    F.CURRENCY  = ''
    CALL OPF(FN.CURRENCY,F.CURRENCY)

    Y.CURRENCY = R.NEW(TFS.CURRENCY)<1,1>
    CALL CACHE.READ(FN.CURRENCY,Y.CURRENCY,R.CURRENCY,CUR.ERR)
    IF R.CURRENCY<EB.CUR.CCY.NAME,LNGG> EQ '' THEN
        Y.CURRENCY = R.CURRENCY<EB.CUR.CCY.NAME,1>
    END ELSE
        Y.CURRENCY = R.CURRENCY<EB.CUR.CCY.NAME,LNGG>
    END

RETURN
*-------------------------------------------------------
GET.AC.ALPH.ACC:
*-------------------------------------------------------
* Here get the Customer and Account details

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)


    Y.PRIMARY.ACCOUNT = R.NEW(TFS.PRIMARY.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,Y.PRIMARY.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.L.AC.ALPH.AC.NO = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.ALPH.AC.NO>
    Y.CUS.ID          = R.ACCOUNT<AC.CUSTOMER>
    CALL APAP.REDORETAIL.REDO.CUST.IDENTITY.REF(Y.CUS.ID,Y.ALT.ID,Y.CUS.NAME) ;*MANUAL R22 CODE CONVERSION
    Y.NAME.1         = Y.CUS.NAME[1,35]
    Y.NAME.2         = Y.CUS.NAME[36,LEN(Y.CUS.NAME)]

RETURN
END
