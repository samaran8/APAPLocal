* @ValidationCode : MjotNzI5OTI5MzkyOkNwMTI1MjoxNjgyNjgyNjUxNTE2OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 17:20:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM

*----------------------------------------------------------------------------
* <Rating>641</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.NOF.GEN.CHQ.AGENCY(Y.FIN.ARR)
*-----------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : TAM
* Program Name  : REDO.NOF.GEN.CHQ.AGENCY
* ODR NUMBER    : ODR-2010-03-0131
*-----------------------------------------------------------------------------
* Description   : This is nofile routine, will fetch the values to pass to enquiry
* In parameter  : none
* out parameter : Y.FIN.ARR
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*   DATE             WHO             REFERENCE         DESCRIPTION
* 10-01-2011      MARIMUTHU s     ODR-2010-03-0131   Initial Creation
* 19-08-2011      Pradeep S       PACS00101170       Credit card masking
* 08-09-2011      Pradeep S       PACS00101170/KB    Account no validation for CHEQUE.DEPOSIT
* 20-09-2011      Pradeep S       PACS00130535       Sorting of records
* 20-09-2011      JEEVA T         PACS00130535       Sorting of records
* 28.04.2023    Conversion Tool       R22            Auto Conversion     - No changes
* 28.04.2023    Shanmugapriya M       R22            Manual Conversion   - FM TO @FM, VM TO @VM, SM TO @SM, Add call routine prefix
*----------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.MULTI.TRANSACTION.SERVICE
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.RELATION
    $INSERT I_F.REDO.TRANS.TYPES.FT.PARAM
    $INSERT I_F.REDO.TRANS.TYPES.TT.PARAM
    $INSERT I_F.COMPANY
    $INSERT I_REDO.OPEN.FILES.R33.COMMON
    
*-----------------------------------------------------------------------------
MAIN:
*-----------------------------------------------------------------------------
*CALL REDO.OPEN.FILES.R33
** R22 Manual conversion
    CALL APAP.TAM.redoOpenFilesR33()
    GOSUB GET.LOCAL.FIELD.POS
    GOSUB GET.PARAM.VALUE

    GOSUB PROCESS
    GOSUB SUB.MTS.SELECT
    IF Y.SET.AGENT NE 'Y' AND Y.DATE.SET NE 'Y' AND Y.SET.CASH NE 'Y' THEN
        GOSUB SUB.PROCESS.ONE
    END ELSE
        GOSUB SUB.PROCESS
    END
    GOSUB SORT.ARRAY
    GOSUB  PROGRAM.END
RETURN

*------------
SORT.ARRAY:
*------------
*PACS00130535 - New Section added for sorting

    IF Y.FIN.ARR THEN
*CALL REDO.SORT.FINAL.ARRAY(Y.FIN.ARR,Y.OUT.ARRAY)
** R22 Manual conversion
        CALL APAP.TAM.redoSortFinalArray(Y.FIN.ARR,Y.OUT.ARRAY)
    END
    Y.FIN.ARR = Y.OUT.ARRAY
RETURN

*-----------------------------------------------------------------------------
GET.LOCAL.FIELD.POS:
*-----------------------------------------------------------------------------
    APPLN = 'FUNDS.TRANSFER':@FM:'TELLER':@FM:'T24.FUND.SERVICES':@FM:'CUSTOMER'
    FIELD.NAMES = 'L.FT.CR.CARD.NO':@VM:'L.LOAN.ARR.ID':@VM:'T24.FS.REF':@VM:'CERT.CHEQUE.NO':@FM:'L.TT.CR.CARD.NO':@VM:'L.TT.CMPNY.ID':@VM:'L.TT.CMPNY.NAME':@VM:'L.TT.ARRANGE.ID':@VM:'T24.FS.REF':@VM:'L.TT.NO.OF.CHQ':@VM:'L.INITIAL.ID':@FM:'L.TT.NO.OF.CHQ':@VM:'L.TT.CR.CARD.NO':@VM:'L.TT.CMPNY.ID':@VM:'L.TT.CMPNY.NAME':@FM:'L.CU.TIPO.CL'
    CALL MULTI.GET.LOC.REF(APPLN,FIELD.NAMES,FIELD.POS)
    Y.POS.FT.CR = FIELD.POS<1,1>
    Y.POS.FT.ARR = FIELD.POS<1,2>
    Y.POS.FT.REF = FIELD.POS<1,3>
    Y.POS.CHQ.NUM = FIELD.POS<1,4>
    Y.POS.TT.CR = FIELD.POS<2,1>
    Y.POS.TT.CMPNY = FIELD.POS<2,2>
    Y.POS.TT.NAME = FIELD.POS<2,3>
    Y.POS.ARR.ID = FIELD.POS<2,4>
    Y.POS.TT.REF = FIELD.POS<2,5>
    Y.POS.TT.REF.TT = FIELD.POS<2,6>
    Y.POS.TT.FT.ID = FIELD.POS<2,7>
    Y.POS.TFS.CQ = FIELD.POS<3,1>
    Y.POS.TFS.CR = FIELD.POS<3,2>
    Y.POS.TFS.CMP = FIELD.POS<3,3>
    Y.POS.TFS.NAME = FIELD.POS<3,4>
    Y.POS.CUS = FIELD.POS<4,1>
RETURN
*-----------------------------------------------------------------------------
GET.PARAM.VALUE:
*-----------------------------------------------------------------------------
    CALL CACHE.READ(FN.REDO.TRANS.TYPES.FT.PARAM,'SYSTEM-FT',R.REDO.TRANS.TYPES.FT.PARAM,REDO.FT.PAR.ERR)
    Y.TXNS.FT = R.REDO.TRANS.TYPES.FT.PARAM<REDO.TXN.FT.TXN.TYPE>
    Y.TXN.PAY.TY.FT = R.REDO.TRANS.TYPES.FT.PARAM<REDO.TXN.FT.TYPE.PAYMENT>
    CALL CACHE.READ(FN.REDO.TRANS.TYPES.TT.PARAM,'SYSTEM-TT',R.REDO.TRANS.TYPES.TT.PARAM,REDO.TT.PAR.ERR)
    Y.TXNS.TT = R.REDO.TRANS.TYPES.TT.PARAM<REDO.TXN.TT.TXN.TYPE>
    Y.TXN.PAY.TY.TT = R.REDO.TRANS.TYPES.TT.PARAM<REDO.TXN.TT.TYPE.PAYMENT>
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    LOCATE 'DATE' IN D.FIELDS SETTING POS THEN
        Y.OPER = D.LOGICAL.OPERANDS<POS>
        Y.DATE = D.RANGE.AND.VALUE<POS>
        Y.CK.DATE = Y.DATE
        Y.DATE.SET = 'Y'
        BEGIN CASE
            CASE Y.OPER EQ 1
                GOSUB OPERAND.EQ
            CASE Y.OPER EQ 3
                GOSUB OPERAND.LT
            CASE Y.OPER EQ 4
                GOSUB OPERAND.GT
            CASE Y.OPER EQ 8
                GOSUB OPERAND.LE
            CASE Y.OPER EQ 9
                GOSUB OPERAND.GE
            CASE Y.OPER EQ 2
                GOSUB OPERAND.RG
        END CASE
    END
    LOCATE 'CASHIER' IN D.FIELDS SETTING POS.CAS THEN
        Y.CASHIER = D.RANGE.AND.VALUE<POS.CAS>
        Y.CASHIER.OPER = D.LOGICAL.OPERANDS<POS.CAS>
        GOSUB CASHIER.SEL
    END
    LOCATE 'AGENCY' IN D.FIELDS SETTING POS.AG THEN
        Y.AGENCY = D.RANGE.AND.VALUE<POS.AG>
        Y.SET.AGENT = 'Y'
        Y.AGENCY.OPER = D.LOGICAL.OPERANDS<POS.AG>
        GOSUB CASE.AGENCY.SEC
    END
    GOSUB GET.ENQ.VALUES
RETURN
*-----------------------------------------------------------------------------
CASHIER.SEL:
*-----------------------------------------------------------------------------
    SEL.FT.LV = ''
    SEL.FT = ''
    Y.SET.CASH = 'Y'
    IF Y.DATE.SET EQ 'Y' THEN
        SEL.TT := ' AND (TELLER.ID.1 EQ ':Y.CASHIER:' OR TELLER.ID.2 EQ ':Y.CASHIER:')'
        IF SET.LIVE EQ 'Y' THEN
            SEL.TT.LV := ' WITH (TELLER.ID.1 EQ ':Y.CASHIER:' OR TELLER.ID.2 EQ ':Y.CASHIER:')'
        END
    END ELSE
        SEL.TT.LV = 'SELECT ':FN.TELLER:' WITH TELLER.ID.1 EQ ':Y.CASHIER:' OR TELLER.ID.2 EQ ':Y.CASHIER
        SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH TELLER.ID.1 EQ ':Y.CASHIER:' OR TELLER.ID.2 EQ ':Y.CASHIER
    END
RETURN
*-----------------------------------------------------------------------------
CASE.AGENCY.SEC:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.SET.CASH EQ 'Y' AND Y.DATE.SET EQ 'Y'
            SEL.TT := ' AND CO.CODE EQ ':Y.AGENCY
            SEL.TT.LV := ' AND CO.CODE EQ ':Y.AGENCY
        CASE Y.DATE.SET EQ 'Y' AND Y.SET.CASH NE 'Y'
            SEL.TT := ' AND CO.CODE EQ ':Y.AGENCY
            IF SET.LIVE EQ 'Y' THEN
                SEL.TT.LV := ' WITH CO.CODE EQ ':Y.AGENCY
                SEL.FT.LV := ' WITH CO.CODE EQ ':Y.AGENCY
            END
            SEL.FT := ' AND CO.CODE EQ ':Y.AGENCY
        CASE Y.SET.CASH EQ 'Y' AND Y.DATE.SET NE 'Y'
            SEL.TT := ' AND CO.CODE EQ ':Y.AGENCY
            SEL.TT.LV := ' AND CO.CODE EQ ':Y.AGENCY
        CASE Y.DATE.SET NE 'Y' AND Y.SET.CASH NE 'Y'
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER:' WITH CO.CODE EQ ':Y.AGENCY
            SEL.TT.LV = 'SELECT ':FN.TELLER:' WITH CO.CODE EQ ':Y.AGENCY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH CO.CODE EQ ':Y.AGENCY
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH CO.CODE EQ ':Y.AGENCY
    END CASE
RETURN
*-----------------------------------------------------------------------------
GET.ENQ.VALUES:
*-----------------------------------------------------------------------------


    BEGIN CASE
        CASE Y.DATE.SET EQ 'Y' AND Y.SET.CASH EQ 'Y' AND Y.SET.AGENT EQ 'Y'
            GOSUB GET.HEAD.1
        CASE Y.DATE.SET EQ 'Y' AND Y.SET.CASH EQ 'Y' AND Y.SET.AGENT NE 'Y'
            GOSUB GET.HEAD.2
        CASE Y.DATE.SET EQ 'Y' AND Y.SET.CASH NE 'Y' AND Y.SET.AGENT EQ 'Y'
            GOSUB GET.HEAD.3
        CASE Y.DATE.SET NE 'Y' AND Y.SET.CASH EQ 'Y' AND Y.SET.AGENT EQ 'Y'
*CALL F.READ(FN.COMPANY,Y.AGENCY,R.COMP,F.COMPANY,COMP.ERR)
*Y.AG.NAME = R.COMP<EB.COM.COMPANY.NAME>
            Y.ENQ.DATAS = 'CAJERO ':OPERAND.LIST<Y.CASHIER.OPER>:' ':Y.CASHIER:', ':'AGENCIA ':OPERAND.LIST<Y.AGENCY.OPER>:' ':Y.AGENCY
        CASE Y.DATE.SET EQ 'Y' AND Y.SET.CASH NE 'Y' AND Y.SET.AGENT NE 'Y'
            GOSUB GET.HEAD.4
        CASE Y.DATE.SET NE 'Y' AND Y.SET.CASH EQ 'Y' AND Y.SET.AGENT NE 'Y'
            Y.ENQ.DATAS = 'CAJERO: ':Y.CASHIER
        CASE Y.DATE.SET NE 'Y' AND Y.SET.CASH NE 'Y' AND Y.SET.AGENT EQ 'Y'
*CALL F.READ(FN.COMPANY,Y.AGENCY,R.COMP,F.COMPANY,COMP.ERR)
*Y.AG.NAME = R.COMP<EB.COM.COMPANY.NAME>
            Y.ENQ.DATAS = 'AGENCIA ':OPERAND.LIST<Y.AGENCY.OPER>:' ':Y.AGENCY
        CASE Y.DATE.SET NE 'Y' AND Y.SET.CASH NE 'Y' AND Y.SET.AGENT NE 'Y'
            Y.ENQ.DATAS = 'ALL'
    END CASE
RETURN
*-----------------------------------------------------------------------------
GET.HEAD.1:
*-----------------------------------------------------------------------------
    IF Y.OPER EQ 2 THEN
        CALL EB.DATE.FORMAT.DISPLAY(Y.S.DATE, Y.DUP.SRT.DATE, '', '')
        CALL EB.DATE.FORMAT.DISPLAY(Y.E.DATE, Y.DUP.END.DATE, '', '')
*CALL F.READ(FN.COMPANY,Y.AGENCY,R.COMP,F.COMPANY,COMP.ERR)
*Y.AG.NAME = R.COMP<EB.COM.COMPANY.NAME>
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.SRT.DATE:' ':Y.DUP.END.DATE:', ':'CAJERO ':OPERAND.LIST<Y.CASHIER.OPER>:' ':Y.CASHIER:', ':'AGENCIA ':OPERAND.LIST<Y.AGENCY.OPER>:' ':Y.AGENCY
    END ELSE
        Y.DATE.CNT = FIELD(Y.CK.DATE,@SM,2)
        IF Y.DATE.CNT NE '' THEN
            Y.CK.DATE = Y.DATE.CNT
        END

        CALL EB.DATE.FORMAT.DISPLAY(Y.CK.DATE, Y.DUP.CHK.DATE, '', '')
*CALL F.READ(FN.COMPANY,Y.AGENCY,R.COMP,F.COMPANY,COMP.ERR)
*Y.AG.NAME = R.COMP<EB.COM.COMPANY.NAME>
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.CHK.DATE:', ':'CAJERO ':OPERAND.LIST<Y.CASHIER.OPER>:' ':Y.CASHIER:', ':'AGENCIA ':OPERAND.LIST<Y.AGENCY.OPER>:' ':Y.AGENCY
    END
RETURN
*-----------------------------------------------------------------------------
GET.HEAD.2:
*-----------------------------------------------------------------------------
    IF Y.OPER EQ 2 THEN
        CALL EB.DATE.FORMAT.DISPLAY(Y.S.DATE, Y.DUP.SRT.DATE, '', '')
        CALL EB.DATE.FORMAT.DISPLAY(Y.E.DATE, Y.DUP.END.DATE, '', '')
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.SRT.DATE:' ':Y.DUP.END.DATE:', ':'CAJERO ':OPERAND.LIST<Y.CASHIER.OPER>:' ':Y.CASHIER
    END ELSE
        Y.DATE.CNT = FIELD(Y.CK.DATE,@SM,2)
        IF Y.DATE.CNT NE '' THEN
            Y.CK.DATE = Y.DATE.CNT
        END
        CALL EB.DATE.FORMAT.DISPLAY(Y.CK.DATE, Y.DUP.CHK.DATE, '', '')
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.CHK.DATE:', ':'CAJERO ':OPERAND.LIST<Y.CASHIER.OPER>:' ':Y.CASHIER
    END
RETURN
*-----------------------------------------------------------------------------
GET.HEAD.3:
*-----------------------------------------------------------------------------
    IF Y.OPER EQ 2 THEN
        CALL EB.DATE.FORMAT.DISPLAY(Y.S.DATE, Y.DUP.SRT.DATE, '', '')
        CALL EB.DATE.FORMAT.DISPLAY(Y.E.DATE, Y.DUP.END.DATE, '', '')
*CALL F.READ(FN.COMPANY,Y.AGENCY,R.COMP,F.COMPANY,COMP.ERR)
*Y.AG.NAME = R.COMP<EB.COM.COMPANY.NAME>
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.SRT.DATE:' ':Y.DUP.END.DATE:', ':'AGENCIA ':OPERAND.LIST<Y.AGENCY.OPER>:' ':Y.AGENCY
    END ELSE
        Y.DATE.CNT = FIELD(Y.CK.DATE,@SM,2)
        IF Y.DATE.CNT NE '' THEN
            Y.CK.DATE = Y.DATE.CNT
        END
        CALL EB.DATE.FORMAT.DISPLAY(Y.CK.DATE, Y.DUP.CHK.DATE, '', '')
*CALL F.READ(FN.COMPANY,Y.AGENCY,R.COMP,F.COMPANY,COMP.ERR)
*Y.AG.NAME = R.COMP<EB.COM.COMPANY.NAME>
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.CHK.DATE:', ':'AGENCIA '::OPERAND.LIST<Y.AGENCY.OPER>:' ':Y.AGENCY
    END
RETURN
*-----------------------------------------------------------------------------
GET.HEAD.4:
*-----------------------------------------------------------------------------
    IF Y.OPER EQ 2 THEN
        CALL EB.DATE.FORMAT.DISPLAY(Y.S.DATE, Y.DUP.SRT.DATE, '', '')
        CALL EB.DATE.FORMAT.DISPLAY(Y.E.DATE, Y.DUP.END.DATE, '', '')
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.DUP.SRT.DATE:' ':Y.DUP.END.DATE
    END ELSE
        Y.DATE.CNT = FIELD(Y.CK.DATE,@SM,2)
        IF Y.DATE.CNT NE '' THEN
            Y.CK.DATE = Y.DATE.CNT
        END
        CALL EB.DATE.FORMAT.DISPLAY(Y.CK.DATE, Y.CHK.DATE, '', '')
        Y.ENQ.DATAS = 'FECHA ':OPERAND.LIST<Y.OPER>:' ':Y.CHK.DATE
    END
RETURN
*-----------------------------------------------------------------------------
SUB.MTS.SELECT:
*-----------------------------------------------------------------------------
    IF Y.SET.CASH EQ 'Y' THEN
        SEL.MTS = 'SELECT ':FN.MULTI.TRANSACTION.SERVICE:' WITH TELLER.ID EQ ':Y.CASHIER
        IF Y.SET.AGENT EQ 'Y' THEN
            SEL.MTS := ' AND CO.CODE EQ ':Y.AGENCY
        END
        CALL EB.READLIST(SEL.MTS,SEL.LIST.MTS,'',NO.MTS,MTS.REE)
        IF Y.DATE.SET EQ 'Y' THEN
            GOSUB READ.MTS.VALUES
        END
    END
RETURN
*-----------------------------------------------------------------------------
READ.MTS.VALUES:
*-----------------------------------------------------------------------------
    LOOP
        REMOVE Y.MTS.ID FROM SEL.LIST.MTS SETTING POS.MTS.ID
    WHILE Y.MTS.ID:POS.MTS.ID
        CALL F.READ(FN.MULTI.TRANSACTION.SERVICE,Y.MTS.ID,R.MTS.SEL,F.MULTI.TRANSACTION.SERVICE,MST.RE.ERR)
        Y.TEST.DATE= R.MTS.SEL<REDO.MTS.TRANSACTION.DATE>
        GOSUB CHECK.CASE.MTS
    REPEAT
RETURN
*-----------------------------------------------------------------------------
CHECK.CASE.MTS:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.OPER EQ 1
            IF Y.TEST.DATE EQ Y.DATE THEN
                Y.TXNS.MTS.IDS = R.MTS.SEL<REDO.MTS.TRANSACTION.ID>
                Y.TXNS.MTS.IDS = CHANGE(Y.TXNS.MTS.IDS,@VM,@FM)
                Y.MTS.LIST.FIN := @FM:Y.TXNS.MTS.IDS
            END
        CASE Y.OPER EQ 3
            IF Y.TEST.DATE LT Y.DATE THEN
                Y.TXNS.MTS.IDS = R.MTS.SEL<REDO.MTS.TRANSACTION.ID>
                Y.TXNS.MTS.IDS = CHANGE(Y.TXNS.MTS.IDS,@VM,@FM)
                Y.MTS.LIST.FIN := @FM:Y.TXNS.MTS.IDS
            END
        CASE Y.OPER EQ 4
            IF Y.TEST.DATE GT Y.DATE THEN
                Y.TXNS.MTS.IDS = R.MTS.SEL<REDO.MTS.TRANSACTION.ID>
                Y.TXNS.MTS.IDS = CHANGE(Y.TXNS.MTS.IDS,@VM,@FM)
                Y.MTS.LIST.FIN := @FM:Y.TXNS.MTS.IDS
            END
        CASE Y.OPER EQ 8
            IF Y.TEST.DATE LE Y.DATE THEN
                Y.TXNS.MTS.IDS = R.MTS.SEL<REDO.MTS.TRANSACTION.ID>
                Y.TXNS.MTS.IDS = CHANGE(Y.TXNS.MTS.IDS,@VM,@FM)
                Y.MTS.LIST.FIN := @FM:Y.TXNS.MTS.IDS
            END
        CASE Y.OPER EQ 9
            IF Y.TEST.DATE GE Y.DATE THEN
                Y.TXNS.MTS.IDS = R.MTS.SEL<REDO.MTS.TRANSACTION.ID>
                Y.TXNS.MTS.IDS = CHANGE(Y.TXNS.MTS.IDS,@VM,@FM)
                Y.MTS.LIST.FIN := @FM:Y.TXNS.MTS.IDS
            END
        CASE Y.OPER EQ 2
            IF (Y.TEST.DATE GE Y.S.DATE) AND (Y.TEST.DATE LE Y.E.DATE) THEN
                Y.TXNS.MTS.IDS = R.MTS.SEL<REDO.MTS.TRANSACTION.ID>
                Y.TXNS.MTS.IDS = CHANGE(Y.TXNS.MTS.IDS,@VM,@FM)
                Y.MTS.LIST.FIN := @FM:Y.TXNS.MTS.IDS
            END
    END CASE

RETURN
*-----------------------------------------------------------------------------
SUB.PROCESS.ONE:
*-----------------------------------------------------------------------------
    SEL.TT.LV = 'SELECT ':FN.TELLER:' BY TELLER.ID.1 BY TRANSACTION.CODE BY CURRENCY.1 BY CO.CODE'
    SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER:' BY @ID BY TRANSACTION.TYPE BY DEBIT.CURRENCY BY CO.CODE'

    SEL.TT = 'SELECT ':FN.TELLER.HIS:' BY TELLER.ID.1 BY TRANSACTION.CODE BY CURRENCY.1 BY CO.CODE'
    SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' BY @ID BY TRANSACTION.TYPE BY DEBIT.CURRENCY BY CO.CODE'

    CALL EB.READLIST(SEL.TT.LV,SEL.TT.LV.LIST,'',NO.OF.REC,TT.LV.ER)
    CALL EB.READLIST(SEL.FT.LV,SEL.FT.LV.LIST,'',NO.OF.REC.FT,FT.ERRR)

    CALL EB.READLIST(SEL.TT,SEL.TT.LIST,'',NO.OF.REC.H,TT.LH.ER)
    CALL EB.READLIST(SEL.FT,SEL.FT.LIST,'',NO.OF.REC.FT.H,FTH.ERRR)

    FIN.LIST = SEL.TT.LV.LIST:@FM:SEL.FT.LV.LIST:@FM:SEL.TT.LIST:@FM:SEL.FT.LIST
    GOSUB PROCESS.LISTS
RETURN
*-----------------------------------------------------------------------------
SUB.PROCESS:
*-----------------------------------------------------------------------------
    IF Y.SET.CASH NE 'Y' THEN
        SEL.FT := ' BY @ID BY TRANSACTION.TYPE BY DEBIT.CURRENCY BY CO.CODE'
        SEL.FT.LV := ' BY @ID BY TRANSACTION.TYPE BY DEBIT.CURRENCY BY CO.CODE'
        CALL EB.READLIST(SEL.FT,SEL.FT.LIST,'',NO.OF.REC,FT.ERR)
        CALL EB.READLIST(SEL.FT.LV,SEL.FT.LIST.LV,'',NO.OF.REC.LV,FT.LV.ERR)
    END ELSE
        SEL.FT.LIST = '' ; SEL.FT.LIST.LV = ''
    END
    SEL.TT := ' BY TELLER.ID.1 BY TRANSACTION.CODE BY CURRENCY.1 BY CO.CODE'
    SEL.TT.LV := ' BY TELLER.ID.1 BY TRANSACTION.CODE BY CURRENCY.1 BY CO.CODE'
    CALL EB.READLIST(SEL.TT,SEL.TT.LIST,'',NO.OF.RECS,TT.ERR)
    CALL EB.READLIST(SEL.TT.LV,SEL.TT.LIST.LV,'',NO.OF.RECS.LV,TT.LV.ERR)
    IF SEL.FT.LIST NE '' OR SEL.FT.LIST.LV NE '' OR SEL.TT.LIST NE '' OR SEL.TT.LIST.LV NE '' OR SEL.LIST.MTS NE '' THEN
        FIN.LIST = SEL.FT.LIST:@FM:SEL.FT.LIST.LV:@FM:SEL.TT.LIST:@FM:SEL.TT.LIST.LV:@FM:Y.MTS.LIST.FIN
    END
    GOSUB PROCESS.LISTS

RETURN
*-----------------------------------------------------------------------------
PROCESS.LISTS:
*-----------------------------------------------------------------------------


    LOOP
        REMOVE Y.ID FROM FIN.LIST SETTING POS.LIST
    WHILE Y.ID:POS.LIST

        Y.ID.1 = Y.ID
        Y.ID = FIELD(Y.ID,';',1)

        IF Y.ID[1,2] EQ 'FT' THEN
            IF Y.ID.DUP NE Y.ID THEN
                GOSUB CHECK.PRE.IDS.PROCESS
            END
        END ELSE
            IF Y.ID.DUP.TT NE Y.ID THEN
                GOSUB CHECK.PRE.IDS.PROCESS.TT
            END
        END
    REPEAT
RETURN
*-----------------------------------------------------------------------------
CHECK.PRE.IDS.PROCESS:
*-----------------------------------------------------------------------------
    Y.ID.DUP = Y.ID
    CALL F.READ(FN.FUNDS.TRANSFER,Y.ID,R.FT,F.FUNDS.TRANSFER,FT.ER)
    IF NOT(R.FT) THEN
        CALL F.READ(FN.FUNDS.TRANSFER.HIS,Y.ID.1,R.FT,F.FUNDS.TRANSFER.HIS,FT.ER.HIS)
        IF R.FT EQ '' THEN
            CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIS,Y.ID,R.FT,FT.ER.HIS)
        END
    END
    IF R.FT THEN
        Y.TXN.TYPE.FT.P = R.FT<FT.TRANSACTION.TYPE>
        LOCATE Y.TXN.TYPE.FT.P IN Y.TXNS.FT<1,1> SETTING POS.FT THEN
            GOSUB GET.FT.VALUES
        END
    END
RETURN
*-----------------------------------------------------------------------------
CHECK.PRE.IDS.PROCESS.TT:
*-----------------------------------------------------------------------------
    Y.ID.DUP.TT = Y.ID
    CALL F.READ(FN.TELLER,Y.ID,R.TT,F.TELLER,TT.ER)
    IF NOT(R.TT) THEN
        CALL F.READ(FN.TELLER.HIS,Y.ID.1,R.TT,F.TELLER.HIS,TT.ER.HIS)
        IF R.TT EQ '' THEN
            CALL EB.READ.HISTORY.REC(F.TELLER.HIS,Y.ID,R.TT,TT.ER.HIS)
        END
    END
    IF R.TT THEN
        Y.TXN.TYPE.TT.P = R.TT<TT.TE.TRANSACTION.CODE>
        LOCATE Y.TXN.TYPE.TT.P IN Y.TXNS.TT<1,1> SETTING POS.TT THEN
            GOSUB GET.TT.VALUES
        END
    END
RETURN
*-----------------------------------------------------------------------------
GET.FT.VALUES:
*-----------------------------------------------------------------------------
    Y.DATE = R.FT<FT.AUTH.DATE>
    Y.REQ.DATE = ICONV(Y.DATE,'DJ')
    Y.REQ.DATE = OCONV(Y.REQ.DATE,'D4E/')
    Y.FT.ID = ''
    Y.USER.NAME = R.FT<FT.INPUTTER>
    Y.USER.NAME = FIELD(Y.USER.NAME,'_',2)
    Y.AGENCY = R.FT<FT.CO.CODE>
    Y.TXN.TYPE = R.FT<FT.TRANSACTION.TYPE>
    CALL F.READ(FN.FT.TXN.TYPE.CONDITION,Y.TXN.TYPE,R.FTTC,F.FT.TXN.TYPE.CONDITION,FTTC.ERR)
    Y.FT.DESC = R.FTTC<FT6.DESCRIPTION,LNGG>        ;* PACS00101170 - S/E
    Y.TXN.DESC = Y.TXN.TYPE:'-':Y.FT.DESC

    Y.FT.T24.REF = R.FT<FT.LOCAL.REF,Y.POS.FT.REF>

    LOCATE Y.TXN.TYPE IN Y.TXNS.FT<1,1> SETTING POS.TXN.FT THEN
        Y.COR.PAY.TYPE.FT = Y.TXN.PAY.TY.FT<1,POS.TXN.FT>
    END

    IF Y.FT.T24.REF EQ '' THEN
        Y.TRANS.AMT = R.FT<FT.AMOUNT.CREDITED>
        Y.TRANS.AMT = Y.TRANS.AMT[4,-1]
        IF Y.COR.PAY.TYPE.FT EQ 'LOAN.PAYMENT' THEN
            Y.ACC.NO.DIS = R.FT<FT.CREDIT.ACCT.NO>
        END
        IF Y.COR.PAY.TYPE.FT EQ 'CHEQUE.DEPOSIT' THEN
            Y.ACC.NO.DIS = R.FT<FT.CREDIT.ACCT.NO>
        END
    END ELSE
        CALL F.READ(FN.MULTI.TRANSACTION.SERVICE,Y.FT.T24.REF,R.MTS,F.MULTI.TRANSACTION.SERVICE,MTS.ERR)
        Y.TELLER.ID.FT = R.MTS<REDO.MTS.TELLER.ID>
        Y.REF.IDSS = R.MTS<REDO.MTS.TRANSACTION.ID>
        Y.PAYMNT.MODE = R.MTS<REDO.MTS.PAYMENT.MODE>
        LOCATE Y.ID IN Y.REF.IDSS<1,1> SETTING POS.MD THEN
            IF Y.PAYMNT.MODE<1,POS.MD> EQ 'CHEQUE' THEN
                Y.ACC.NO.DIS = R.FT<FT.CREDIT.ACCT.NO>
            END
        END

    END

    IF Y.ACC.NO.DIS[1,2] EQ 'AA' THEN
        CALL F.READ(FN.AA.ARRANGEMENT,Y.ACC.NO.DIS,R.AA.ARR,F.AA.ARRANGEMENT,AA.ARR.ERR)
        Y.AC.ID = R.AA.ARR<AA.ARR.LINKED.APPL.ID>
        CALL F.READ(FN.ACCOUNT,Y.AC.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.ALT.ID = R.ACCOUNT<AC.ALT.ACCT.ID>
        Y.ACC.ID.T = Y.AC.ID
        GOSUB GET.CUS.DIS.NAME
    END ELSE
        CALL F.READ(FN.ACCOUNT,Y.ACC.NO.DIS,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.ALT.ID = R.ACCOUNT<AC.ALT.ACCT.ID>
        Y.ACC.ID.T = Y.ACC.NO.DIS
        GOSUB GET.CUS.DIS.NAME
    END
    Y.CURRENCY = R.FT<FT.DEBIT.CURRENCY>

    Y.FT.T24.REF = R.FT<FT.LOCAL.REF,Y.POS.FT.REF>
    IF Y.FT.T24.REF[1,3] EQ 'MTS' THEN
        CALL F.READ(FN.MULTI.TRANSACTION.SERVICE,Y.FT.T24.REF,R.MTS,F.MULTI.TRANSACTION.SERVICE,MTS.ERR)
        Y.NO.OF.CHQ = R.MTS<REDO.MTS.NO.OF.CHEQUES>
        Y.TRANS.AMT = R.MTS<REDO.MTS.TOTAL.AMT>
    END
    IF Y.FT.T24.REF[1,3] EQ 'T24' THEN
        GOSUB CHEK.T24.FS.REF
    END

    Y.DEBIT.AMT = R.FT<FT.AMOUNT.DEBITED>
    Y.DEBIT.AMT = Y.DEBIT.AMT[4,-1]
    Y.CREDIT.AMT = R.FT<FT.AMOUNT.CREDITED>
    Y.CREDIT.AMT = Y.CREDIT.AMT[4,-1]
    Y.CHQ.NUMBER = R.FT<FT.CHEQUE.NUMBER>

    Y.ID = FIELD(Y.ID,';',1)
    CHANGE "*" TO "|" IN Y.ACC.NO.DIS     ;* PACS00101170 - S/E
    Y.SORT.VALUE = Y.CURRENCY:"#":Y.TELLER.ID.FT:"#":Y.AGENCY:"#":Y.TXN.DESC:"#":""         ;*PACS00130535 - S/E

    GOSUB FINAL.ARRAY.PARA
RETURN
******************
FINAL.ARRAY.PARA:
******************

*                                       1                2                3             4             5               6            7                8                   9              10               11             12          13            14               15         16               17                 18                                          19

*    Y.FIN.ARR<-1> = Y.SORT.VALUE:'*':Y.REQ.DATE:'*':Y.TELLER.ID.FT:'*':Y.USER.NAME:'*':Y.AGENCY:'*':Y.TXN.DESC:'*':Y.ACC.NO.DIS:'*':Y.ALT.ID:'*':Y.JOINT.HOLDERS:'*':Y.CURRENCY:'*':Y.NO.OF.CHQ:'*':Y.DEBIT.AMT:'*':Y.TRANS.AMT:'*':Y.ID:'*':Y.CHQ.NUMBER:'*':Y.DEBIT.AMT:'*':'':'*':OCONV(TIME(),'MTS'):'*':Y.ENQ.DATAS:'*':'REPORTE GENERAL DE CHEQUES RECIBIDOS DESDE LAS AGENCIAS'
    IF Y.FIN.ARR THEN
        Y.FIN.ARR<-1> = Y.REQ.DATE:'*':Y.TELLER.ID.FT:"*":Y.USER.NAME:"*":Y.AGENCY:"*":Y.TXN.DESC:"*":Y.ACC.NO.DIS:"*":Y.ALT.ID:"*":Y.JOINT.HOLDERS:"*":Y.CURRENCY:"*":Y.NO.OF.CHQ:"*":Y.DEBIT.AMT:"*":Y.TRANS.AMT:"*":Y.ID:"*":Y.CHQ.NUMBER:"*":Y.DEBIT.AMT:"*":"*":OCONV(TIME(),'MTS'):"*":Y.ENQ.DATAS:"*":'REPORTE GENERAL DE CHEQUES RECIBIDOS DESDE LAS AGENCIAS'
    END ELSE
        Y.FIN.ARR = Y.REQ.DATE:'*':Y.TELLER.ID.FT:"*":Y.USER.NAME:"*":Y.AGENCY:"*":Y.TXN.DESC:"*":Y.ACC.NO.DIS:"*":Y.ALT.ID:"*":Y.JOINT.HOLDERS:"*":Y.CURRENCY:"*":Y.NO.OF.CHQ:"*":Y.DEBIT.AMT:"*":Y.TRANS.AMT:"*":Y.ID:"*":Y.CHQ.NUMBER:"*":Y.DEBIT.AMT:"*":"*":OCONV(TIME(),'MTS'):"*":Y.ENQ.DATAS:"*":'REPORTE GENERAL DE CHEQUES RECIBIDOS DESDE LAS AGENCIAS'
    END
    Y.ACC.NO.DIS = ''; Y.ALT.ID = '';Y.JOINT.HOLDERS = '';Y.NO.OF.CHQ = ''; Y.DEBIT.AMT = ''; Y.TRANS.AMT = '';Y.CHQ.NUMBER = '';Y.CREDIT.AMT = ''; Y.TELLER.ID.FT = ''

RETURN
*-----------------------------------------------------------------------------
CHEK.T24.FS.REF:
*-----------------------------------------------------------------------------
    CALL F.READ(FN.T24.FUND.SERVICES,Y.FT.T24.REF,R.T24,F.T24.FUND.SERVICES,T24.ERR)
    Y.NO.OF.CHQ = R.T24<TFS.LOCAL.REF,Y.POS.TFS.CQ>
    Y.DUP.TRANS.AMT = R.T24<TFS.AMOUNT>
    Y.CNT.MN = DCOUNT(Y.TRANS.AMT,@VM)
    FLG.2 = ''
    IF Y.CNT.MN EQ 1 THEN
        Y.TRANS.AMT = Y.DUP.TRANS.AMT
    END ELSE
        LOOP
        WHILE Y.CNT.MN GT 0 DO
            FLG.2 += 1
            Y.TRANS.AMT = Y.TRANS.AMT + Y.DUP.TRANS.AMT<1,FLG.2>
            Y.CNT.MN -= 1
        REPEAT
    END

RETURN
*-----------------------------------------------------------------------------
GET.TT.VALUES:
*-----------------------------------------------------------------------------
    Y.TT.DATE = R.TT<TT.TE.AUTH.DATE>
    Y.TT.REQ.DATE = ICONV(Y.TT.DATE,'DJ')
    Y.TT.REQ.DATE = OCONV(Y.TT.REQ.DATE,'D4E/')

    Y.TT.ID = R.TT<TT.TE.TELLER.ID.1>
    Y.TT.USER = R.TT<TT.TE.INPUTTER>
    Y.TT.USER = FIELD(Y.TT.USER,'_',2)
    Y.TT.AGENCY = R.TT<TT.TE.CO.CODE>

    Y.TT.TXN = R.TT<TT.TE.TRANSACTION.CODE>

    CALL F.READ(FN.TELLER.TRANSACTION,Y.TT.TXN,R.TT.TXN,F.TELLER.TRANSACTION,TT.TXN.ERR)
    Y.TT.DESC = R.TT.TXN<TT.TR.DESC,1,LNGG>         ;*PACS00101170 - S/E
*Y.TT.DESC = R.TT.TXN<TT.TR.DESC,1,2>
*IF Y.TT.DESC EQ '' THEN
*Y.TT.DESC = R.TT.TXN<TT.TR.DESC,1,1>
*END
    Y.TT.TXN.DESC = Y.TT.TXN:'-':Y.TT.DESC

    Y.TT.T24.REF = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.REF>
    LOCATE Y.TT.TXN IN Y.TXNS.TT<1,1> SETTING POS.TXN.TT THEN
        Y.COR.PAY.TYPE = Y.TXN.PAY.TY.TT<1,POS.TXN.TT>
    END

    IF Y.TT.T24.REF EQ '' THEN
        Y.TRANS.AMT.TT = R.TT<TT.TE.AMOUNT.LOCAL.2>
        BEGIN CASE
            CASE Y.COR.PAY.TYPE EQ 'CREDIT.CARD.PAYMENT'
                Y.AC.TT.DIS = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.CR>
                IF Y.AC.TT.DIS EQ '' THEN
                    Y.AC.TT.DIS = R.TT<TT.TE.ACCOUNT.2>
                END
            CASE Y.COR.PAY.TYPE EQ 'LOAN.PAYMENT'
                Y.AC.TT.DIS = R.TT<TT.TE.ACCOUNT.2>
            CASE Y.COR.PAY.TYPE EQ 'BILL.PAYMENT'
                Y.AC.TT.DIS = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.CMPNY>
                GOSUB AC.TT.DIS
            CASE Y.COR.PAY.TYPE EQ 'CHEQUE.DEPOSIT'
                Y.AC.TT.DIS = R.TT<TT.TE.ACCOUNT.2>

        END CASE
    END ELSE
        GOSUB GET.ELSE.T24
    END

*PACS00101170 - S
*IF Y.AC.TT.DIS EQ '' AND Y.COR.PAY.TYPE EQ 'CHEQUE.DEPOSIT' THEN
*Y.AC.TT.DIS = R.TT<TT.TE.ACCOUNT.2>
*END
*PACS00101170 - E

    GOSUB GET.TT.VALUES.SUB

RETURN

*-----------
AC.TT.DIS:
*-----------

    IF R.TT<TT.TE.LOCAL.REF,Y.POS.TT.NAME> NE '' AND Y.AC.TT.DIS NE '' THEN
        Y.AC.TT.DIS := ",":R.TT<TT.TE.LOCAL.REF,Y.POS.TT.NAME>
    END
    IF R.TT<TT.TE.LOCAL.REF,Y.POS.TT.NAME> NE '' AND Y.AC.TT.DIS EQ '' THEN
        Y.AC.TT.DIS = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.NAME>
    END

RETURN

*-----------------------------------------------------------------------------
GET.TT.VALUES.SUB:
*-----------------------------------------------------------------------------
    IF Y.AC.TT.DIS[1,2] EQ 'AA' THEN
        CALL F.READ(FN.AA.ARRANGEMENT,Y.ACC.NO.DIS,R.AA.ARR,F.AA.ARRANGEMENT,AA.ARR.ERR)
        Y.AC.ID = R.AA.ARR<AA.ARR.LINKED.APPL.ID>
        CALL F.READ(FN.ACCOUNT,Y.AC.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.ALT.TT.ID = R.ACCOUNT<AC.ALT.ACCT.ID>
        Y.ACC.ID.T = Y.AC.ID
        GOSUB GET.CUS.DIS.NAME
    END ELSE
        CALL F.READ(FN.ACCOUNT,Y.AC.TT.DIS,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.ALT.TT.ID = R.ACCOUNT<AC.ALT.ACCT.ID>
        Y.ACC.ID.T = Y.AC.TT.DIS
        GOSUB GET.CUS.DIS.NAME
    END
    Y.CUR.TT = R.TT<TT.TE.CURRENCY.1>

* Y.TT.T24.REF = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.REF>


    IF Y.TT.T24.REF[1,3] EQ 'T24' THEN
        CALL F.READ(FN.T24.FUND.SERVICES,Y.TT.T24.REF,R.T24,F.T24.FUND.SERVICES,T24.ERR)
        Y.NO.OF.CHQ.TT = R.T24<TFS.LOCAL.REF,Y.POS.TFS.CQ>
*Y.DUP.TRANS.AMT.TT = R.T24<TFS.AMOUNT> ;* PACS00101170 - S/E
        Y.DUP.TRANS.AMT.TT = R.T24<TFS.NET.TXN.AMT>
        Y.CNT.MN.T = DCOUNT(Y.DUP.TRANS.AMT.TT,@VM)
        FLG.3 = '1'
        IF Y.CNT.MN.T EQ 1 THEN
            Y.TRANS.AMT.TT = Y.DUP.TRANS.AMT.TT
        END ELSE
            GOSUB SUM.TFS.AMT
        END
    END
    IF Y.TT.T24.REF[1,3] EQ 'MTS' THEN
        CALL F.READ(FN.MULTI.TRANSACTION.SERVICE,Y.TT.T24.REF,R.MTS,F.MULTI.TRANSACTION.SERVICE,MTS.ERR)
        Y.NO.OF.CHQ.TT = R.MTS<REDO.MTS.NO.OF.CHEQUES>
        Y.TRANS.AMT.TT = R.MTS<REDO.MTS.TOTAL.AMT>
    END
*PACS00130535 - S
    IF Y.CUR.TT EQ LCCY THEN
        Y.DEBIT.AMT.TT = R.TT<TT.TE.AMOUNT.LOCAL.1>
    END ELSE
        Y.DEBIT.AMT.TT = R.TT<TT.TE.AMOUNT.FCY.1>
    END
*PACS00130535 - E
    Y.CHQ.NUM.TT = R.TT<TT.TE.CHEQUE.NUMBER>
    Y.CHQ.BNK.CODE = R.TT<TT.TE.CHEQUE.BANK.CDE>
*Y.AMT.TT = R.TT<TT.TE.AMOUNT.LOCAL.2>
    IF Y.CUR.TT EQ LCCY THEN
        Y.AMT.TT = R.TT<TT.TE.AMOUNT.LOCAL.1>
    END ELSE
        Y.AMT.TT = R.TT<TT.TE.AMOUNT.FCY.1>
    END

    Y.ID = FIELD(Y.ID,';',1)
    CHANGE "*" TO "|" IN Y.AC.TT.DIS      ;* PACS00101170 - S/E
    Y.SORT.VALUE = Y.CUR.TT:"#":Y.TT.ID:"#":Y.TT.AGENCY:"#":Y.TT.TXN.DESC:"#":Y.CHQ.BNK.CODE          ;*PACS00130535 - S/E
*                                          1             2            3              4                5                6               7                 8                9             10                  11                  12           13           14             15               16                  17                   18                          19
*    Y.FIN.ARR<-1> = Y.SORT.VALUE:'*':Y.TT.REQ.DATE:'*':Y.TT.ID:'*':Y.TT.USER:'*':Y.TT.AGENCY:'*':Y.TT.TXN.DESC:'*':Y.AC.TT.DIS:'*':Y.ALT.TT.ID:'*':Y.JOINT.HOLDERS:'*':Y.CUR.TT:'*':Y.NO.OF.CHQ.TT:'*':Y.DEBIT.AMT.TT:'*':Y.TRANS.AMT.TT:'*':Y.ID:'*':Y.CHQ.NUM.TT:'*':Y.AMT.TT:'*':Y.CHQ.BNK.CODE:'*':OCONV(TIME(),'MTS'):'*':Y.ENQ.DATAS:'*':'REPORTE GENERAL DE CHEQUES RECIBIDOS DESDE LAS AGENCIAS'

*IF Y.ID EQ 'TT1630945943' THEN DEBUG

    IF Y.NO.OF.CHQ.TT EQ '' THEN
        GOSUB FETCH.CHQ.QNTY.AND.NUM
    END


    IF Y.FIN.ARR THEN
        Y.FIN.ARR<-1> = Y.TT.REQ.DATE:'*':Y.TT.ID:"*":Y.TT.USER:"*":Y.TT.AGENCY:"*":Y.TT.TXN.DESC:"*":Y.AC.TT.DIS:"*":Y.ALT.TT.ID:"*":Y.JOINT.HOLDERS:"*":Y.CUR.TT:"*":Y.NO.OF.CHQ.TT:'*':Y.DEBIT.AMT.TT:'*':Y.TRANS.AMT.TT:'*':Y.ID:'*':Y.CHQ.NUM.TT:"*":Y.AMT.TT:"*":Y.CHQ.BNK.CODE:"*":OCONV(TIME(),'MTS'):'*':Y.ENQ.DATAS:'*':'REPORTE GENERAL DE CHEQUES RECIBIDOS DESDE LAS AGENCIAS':'*':Y.FT.CHEQ.NUMBER
    END ELSE
        Y.FIN.ARR = Y.TT.REQ.DATE:'*':Y.TT.ID:"*":Y.TT.USER:"*":Y.TT.AGENCY:"*":Y.TT.TXN.DESC:"*":Y.AC.TT.DIS:"*":Y.ALT.TT.ID:"*":Y.JOINT.HOLDERS:"*":Y.CUR.TT:"*":Y.NO.OF.CHQ.TT:'*':Y.DEBIT.AMT.TT:'*':Y.TRANS.AMT.TT:'*':Y.ID:'*':Y.CHQ.NUM.TT:"*":Y.AMT.TT:"*":Y.CHQ.BNK.CODE:"*":OCONV(TIME(),'MTS'):'*':Y.ENQ.DATAS:'*':'REPORTE GENERAL DE CHEQUES RECIBIDOS DESDE LAS AGENCIAS':'*':Y.FT.CHEQ.NUMBER
    END
    Y.TT.REQ.DATE = ''; Y.TT.ID = ''; Y.TT.USER = ''; Y.TT.AGENCY = ''; Y.TT.TXN.DESC = ''; Y.AC.TT.DIS = ''; Y.ALT.TT.ID = '';Y.JOINT.HOLDERS = ''; Y.CUR.TT = '';Y.NO.OF.CHQ.TT = ''; Y.DEBIT.AMT.TT = ''; Y.TRANS.AMT.TT = '';Y.CHQ.NUM.TT = ''; Y.AMT.TT = '';Y.CHQ.BNK.CODE = ''

RETURN

*------------
SUM.TFS.AMT:
*------------
*PACS00101170 -
    LOOP
    WHILE FLG.3 LE Y.CNT.MN.T
        IF Y.DUP.TRANS.AMT.TT<1,FLG.3> GT 0 THEN
            Y.TRANS.AMT.TT = Y.TRANS.AMT.TT + Y.DUP.TRANS.AMT.TT<1,FLG.3>
        END
        FLG.3 += 1
    REPEAT

RETURN

*-----------------------------------------------------------------------------
GET.ELSE.T24:
*-----------------------------------------------------------------------------
    IF Y.TT.T24.REF[1,3] EQ 'T24' THEN
        CALL F.READ(FN.T24.FUND.SERVICES,Y.TT.T24.REF,R.T24,F.T24.FUND.SERVICES,T24.ERR)
        BEGIN CASE
            CASE Y.COR.PAY.TYPE EQ 'CREDIT.CARD.PAYMENT'
                Y.AC.TT.DIS = R.T24<TFS.LOCAL.REF,Y.POS.TFS.CR>
            CASE Y.COR.PAY.TYPE EQ 'BILL.PAYMENT'
                Y.AC.TT.DIS = R.T24<TFS.LOCAL.REF,Y.POS.TFS.CMP>
                Y.AC.NAME = R.T24<TFS.LOCAL.REF,Y.POS.TFS.NAME>
                Y.AC.TT.DIS = Y.AC.TT.DIS:' ':Y.AC.NAME
*PACS00101170/KB - S
            CASE Y.COR.PAY.TYPE EQ 'CHEQUE.DEPOSIT'
                Y.AC.TT.DIS = R.T24<TFS.PRIMARY.ACCOUNT>
*PACS00101170/KB - E
        END CASE
    END

RETURN

*-----------------------------------------------------------------------------
GET.CUS.DIS.NAME:
*-----------------------------------------------------------------------------
    CALL F.READ(FN.ACCOUNT,Y.ACC.ID.T,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.CUS.ID = R.ACCOUNT<AC.CUSTOMER>

    CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUS,F.CUSTOMER,CUS.ERR)
    Y.LOC.FIL.CUS = R.CUS<EB.CUS.LOCAL.REF,Y.POS.CUS>
    IF Y.LOC.FIL.CUS EQ 'PERSONA FISICA' OR Y.LOC.FIL.CUS EQ 'CLIENTE MENOR' THEN
        Y.REQ.NAME = R.CUS<EB.CUS.GIVEN.NAMES>:' ':R.CUS<EB.CUS.FAMILY.NAME>
    END ELSE
        IF Y.LOC.FIL.CUS EQ 'PERSONA JURIDICA' THEN
            Y.REQ.NAME = R.CUS<EB.CUS.NAME.1,1>:' ':R.CUS<EB.CUS.NAME.2,1>
        END
    END

    Y.REL.CODE.ARR = R.ACCOUNT<AC.RELATION.CODE>
    Y.REL.COD.CNT = DCOUNT(Y.REL.CODE.ARR,@VM)
    Y.REL.CNTR = 1
    GOSUB GET.CUS.VALS.12
    Y.JOINT.HOLDERS = Y.REQ.NAME:' ':CHANGE(Y.JOINT.HOLDERS,@FM,' ')
    Y.REQ.NAME = ''

RETURN
*-----------------------------------------------------------------------------
GET.CUS.VALS.12:
*-----------------------------------------------------------------------------
    LOOP
    WHILE Y.REL.CNTR LE Y.REL.COD.CNT
        Y.REL.CODE = R.ACCOUNT<AC.RELATION.CODE><1,Y.REL.CNTR>
*
* Read the relation table using Y.REL.CODE as ID
* Get the relation description
*
        R.RELATION = ''
        CALL F.READ(FN.RELATION,Y.REL.CODE,R.RELATION,F.RELATION,Y.REL.ERR)
        IF R.RELATION NE '' THEN
            Y.REL.NAME = R.RELATION<EB.REL.DESCRIPTION>
*
            IF Y.REL.CODE GE '500' AND Y.REL.CODE LE '529' THEN
                Y.JOINT.HOLD = R.ACCOUNT<AC.JOINT.HOLDER><1,Y.REL.CNTR>
*
* Read the customer record using Y.JOINT.HOLD as a ID
* Get the Short name
*
                GOSUB GET.CUS
*
                Y.JOINT.HOLDERS<-1> = Y.REL.NAME:" ":Y.SHORT.NAME
            END
        END
        Y.REL.CNTR++
    REPEAT

RETURN

*-----------------------------------------------------------------------------
GET.CUS:
*-----------------------------------------------------------------------------
    R.CUSTOMER = ''
    Y.SHORT.NAME = ''
    CALL F.READ(FN.CUSTOMER,Y.JOINT.HOLD,R.CUSTOMER,F.CUSTOMER,Y.CUS.ERR)
    Y.LOC.FIL.CUS = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.POS.CUS>
    IF Y.LOC.FIL.CUS EQ 'PERSONA FISICA' OR Y.LOC.FIL.CUS EQ 'CLIENTE MENOR' THEN
        Y.SHORT.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
    END ELSE
        IF Y.LOC.FIL.CUS EQ 'PERSONA JURIDICA' THEN
            Y.SHORT.NAME = R.CUSTOMER<EB.CUS.NAME.1,1>:' ':R.CUSTOMER<EB.CUS.NAME.2,1>
        END
    END
RETURN
*-----------------------------------------------------------------------------
OPERAND.EQ:
*-----------------------------------------------------------------------------
    BEGIN CASE

        CASE Y.DATE GT TODAY
            ENQ.ERROR = 'Date cannot be greater then TODAY'
        CASE Y.DATE LT TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE EQ ':Y.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE EQ ':Y.DATE
        CASE Y.DATE EQ TODAY
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
            SET.LIVE = 'Y'
    END CASE
RETURN
*-----------------------------------------------------------------------------
OPERAND.LT:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.DATE LT TODAY OR Y.DATE EQ TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE LT ':Y.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE LT ':Y.DATE
        CASE Y.DATE GT TODAY
            ENQ.ERROR = 'Date cannot be greater then TODAY'
    END CASE
RETURN
*-----------------------------------------------------------------------------
OPERAND.GT:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.DATE EQ TODAY
            ENQ.ERROR ='Date cannot be greater then TODAY'
        CASE Y.DATE LT TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE GT ':Y.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE GT ':Y.DATE
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
            SET.LIVE = 'Y'
        CASE Y.DATE GT TODAY
            ENQ.ERROR = 'Date cannot be greater then TODAY'
    END CASE
RETURN
*-----------------------------------------------------------------------------
OPERAND.LE:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.DATE EQ TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE LT ':Y.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE LT ':Y.DATE
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
            SET.LIVE = 'Y'
        CASE Y.DATE LT TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE LE ':Y.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE LE ':Y.DATE
        CASE Y.DATE GT TODAY
            ENQ.ERROR = 'Date cannot be greater then TODAY'
    END CASE
RETURN
*-----------------------------------------------------------------------------
OPERAND.GE:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.DATE GT TODAY
            ENQ.ERROR = 'Date cannot be greater then TODAY'
        CASE Y.DATE EQ TODAY
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
        CASE Y.DATE LT TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE GT ':Y.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE GT ':Y.DATE
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
            SET.LIVE = 'Y'
    END CASE
RETURN
*-----------------------------------------------------------------------------
OPERAND.RG:
*-----------------------------------------------------------------------------
    Y.S.DATE = FIELD(Y.DATE,@SM,1)
    Y.E.DATE = FIELD(Y.DATE,@SM,2)
    BEGIN CASE
        CASE Y.S.DATE LT TODAY AND Y.E.DATE GT TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE GE ':Y.S.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE GE ':Y.S.DATE
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
            SET.LIVE = 'Y'
        CASE Y.S.DATE LT TODAY AND Y.E.DATE EQ TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE GE ':Y.S.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE GE ':Y.S.DATE
            SEL.FT.LV = 'SELECT ':FN.FUNDS.TRANSFER
            SEL.TT.LV = 'SELECT ':FN.TELLER
            SET.LIVE = 'Y'
        CASE Y.S.DATE LT TODAY AND Y.E.DATE LT TODAY
            SEL.FT = 'SELECT ':FN.FUNDS.TRANSFER.HIS:' WITH AUTH.DATE GE ':Y.S.DATE:' AND AUTH.DATE LE ':Y.E.DATE
            SEL.TT = 'SELECT ':FN.TELLER.HIS:' WITH AUTH.DATE GE ':Y.S.DATE:' AND AUTH.DATE LE ':Y.E.DATE
    END CASE
RETURN
*-----------------------------------------------------------------------------
FETCH.CHQ.QNTY.AND.NUM:

    IF Y.TXN.TYPE.TT.P EQ '96' THEN
        Y.NO.OF.CHQ.TT = R.TT<TT.TE.CHEQUE.NUMBER>

        Y.TT.FT.ID = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.FT.ID>

        CALL F.READ(FN.FUNDS.TRANSFER,Y.TT.FT.ID,R.FT,F.FUNDS.TRANSFER,FT.ER)
        IF NOT(R.FT) THEN
            CALL F.READ(FN.FUNDS.TRANSFER.HIS,Y.TT.FT.ID,R.FT,F.FUNDS.TRANSFER.HIS,FT.ER.HIS)
            IF R.FT EQ '' THEN
                CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIS,Y.TT.FT.ID,R.FT,FT.ER.HIS)
            END
        END
        Y.FT.CHEQ.NUMBER = R.FT<FT.LOCAL.REF,Y.POS.CHQ.NUM>

    END
    ELSE
        Y.NO.OF.CHQ.TT = R.TT<TT.TE.LOCAL.REF,Y.POS.TT.REF.TT>
    END

RETURN

PROGRAM.END:

END
