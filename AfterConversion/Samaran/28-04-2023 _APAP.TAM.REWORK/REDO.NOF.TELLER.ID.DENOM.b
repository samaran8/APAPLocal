* @ValidationCode : Mjo1NzMwMTUwODA6Q3AxMjUyOjE2ODI2OTAxMDUzNjk6c2FtYXI6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 19:25:05
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*-------------------------------------------------------------------------------------------------------------
* <Rating>405</Rating>
*--------------------------------------------------------------------------------------------------------
SUBROUTINE REDO.NOF.TELLER.ID.DENOM(Y.OUT.ARRAY)
*
* Date           Who                Reference              Description
* 03-Mar-2010   A.SabariKumar     ODR-2010-03-0148       Initial Creation
* 05-SEP-2014   Egambaram A        PACS00346225           select value has changed
* 19 SEP 2014   Egambaram A        PACS00346225           FT reference has been added
**-------------------------------------------------------------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*18-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*18-04-2023       Samaran T               R22 Manual Code Conversion       FM TO @FM,VM TO @VM,SM TO @SM
*-----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.CURRENCY
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER.ID
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.TRANSACTION
    $INSERT I_F.REDO.TRANSACTION.CHAIN

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS
    GOSUB SELECT.PROCESS
    GOSUB FORM.OUT.ARRAY
RETURN
*---------------------------------------------------------------------------
INITIALISE:
*------------
    Y.PROC.TFS = ""; SEL.CMD = ''; SEL.LIST = ''; Y.TELLER.ID = ''; Y.CURRENCY = ''; Y.TELLER.TRANSACTION = ''
    Y.TELLER = ''; Y.CR.DR.MARKER = ''; Y.CCY1 = ''; Y.CCY2 = ''; Y.ACCOUNT1 = ''; Y.ACCOUNT2 = ''
    Y.AMOUNT1 = ''; Y.AMOUNT2 = ''; Y.SORT.VAL = ''; Y.FINAL.SORT.VAL = ''; Y.SORT.ARR = ''; DUP.HIS.ID = ''
    Y.PROCESS.ACTUAL = ''; Y.DATE.ALONE = ''; Y.TIME.ALONE = ''; Y.CONV.DATE = ''; Y.ICONV.DATE = ''; Y.MONTH = ''

    APPL.NAME = 'TELLER' ; FLD.POS = ''
    FLD.NAME = 'L.TT.CR.CARD.NO':@VM:'L.DEBIT.AMOUNT':@VM:'L.CREDIT.AMOUNT':@VM:'T24.FS.REF':@VM:'L.INITIAL.ID':@VM:"L.TT.AZ.ACC.REF"
    CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)
    Y.CARD.POS = FLD.POS<1,1>; Y.DEB.AMTPOS = FLD.POS<1,2>; Y.CREAMTPOS = FLD.POS<1,3>; Y.TFSPOS = FLD.POS<1,4>; L.INITAL.POS = FLD.POS<1,5>; L.AZ.REFPOS = FLD.POS<1,6>
RETURN
*--------------------------------------------------------------------------------------------------------------------------------------
OPEN.FILES:
*----------
    FN.TELLER = 'F.TELLER' ; F.TELLER = ''; CALL OPF(FN.TELLER,F.TELLER)

    FN.TELLER.HIS = 'F.TELLER$HIS'; F.TELLER.HIS = ''; CALL OPF(FN.TELLER.HIS,F.TELLER.HIS)

    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'; F.TELLER.TRANSACTION = ''; CALL OPF(FN.TELLER.TRANSACTION,F.TELLER.TRANSACTION); R.TELLER.TRANSACTION = ''

    FN.ACCOUNT = 'F.ACCOUNT'; F.ACCOUNT = ''; CALL OPF(FN.ACCOUNT,F.ACCOUNT); R.ACCOUNT = ''

    FN.REDO.TRANSACTION.CHAIN = "F.REDO.TRANSACTION.CHAIN"; F.REDO.TRANSACTION.CHAIN = ""; CALL OPF(FN.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN); R.FN.REDO.TRANSACTION.CHAIN = ""

    FN.T24.FUND.SERVICES = "F.T24.FUND.SERVICES"; F.T24.FUND.SERVICES = ""; CALL OPF(FN.T24.FUND.SERVICES,F.T24.FUND.SERVICES); R.T24.FUND.SERVICES = ""

    FN.TFS.TRANSACTION = "F.TFS.TRANSACTION"; F.TFS.TRANSACTION = ""; CALL OPF(FN.TFS.TRANSACTION,F.TFS.TRANSACTION); R.TFS.TRANSACTION = ""

    FN.TELLER.ID.HIS = "F.TELLER.ID$HIS"; F.TELLER.ID.HIS = ""; CALL OPF(FN.TELLER.ID.HIS,F.TELLER.ID.HIS)
RETURN
*---------------------------------------------------------------------------
PROCESS:
*--------
    LOCATE "DATE.RANGE" IN D.FIELDS<1> SETTING DATE.POS THEN
        Y.DATE = D.RANGE.AND.VALUE<DATE.POS>
        Y.START.RG = Y.DATE<1,1,1>; Y.END.RG = Y.DATE<1,1,2>; Y.START.RG = Y.START.RG[3,6]:'0000'; Y.END.RG = Y.END.RG[3,6]:'2359'
        SEL.CMD = "SELECT ":FN.TELLER:" WITH DATE.TIME GE ":Y.START.RG:" AND DATE.TIME LE ":Y.END.RG
        SEL.CMD1 = "SELECT ":FN.TELLER.HIS:" WITH DATE.TIME GE ":Y.START.RG:" AND DATE.TIME LE ":Y.END.RG
    END

    LOCATE "TELLER.REF" IN D.FIELDS<1> SETTING TEL.POS THEN
        Y.TELLER = D.RANGE.AND.VALUE<TEL.POS>
        Y.ID.OPERAND = D.LOGICAL.OPERANDS<TEL.POS>
        IF Y.ID.OPERAND EQ 1 THEN
            Y.ID.OPERAND = 'EQ'
        END
        IF Y.ID.OPERAND EQ 6 THEN
            Y.ID.OPERAND = 'LIKE'
            Y.TELLER = '...':Y.TELLER:'...'
        END
        SEL.CMD := " AND WITH @ID ":Y.ID.OPERAND:" ":Y.TELLER
        SEL.CMD1 := " AND WITH  @ID ":Y.ID.OPERAND:" ":Y.TELLER
    END

    LOCATE "TELLER.ID.REF" IN D.FIELDS<1> SETTING TEL.ID.POS THEN
        Y.TELLER.ID = D.RANGE.AND.VALUE<TEL.ID.POS>
        SEL.CMD := " AND WITH TELLER.ID.1 EQ ":Y.TELLER.ID:" OR TELLER.ID.2 EQ ":Y.TELLER.ID
        SEL.CMD1 := " AND WITH TELLER.ID.1 EQ ":Y.TELLER.ID:" OR TELLER.ID.2 EQ ":Y.TELLER.ID
    END

    LOCATE "CURRENCY.REF" IN D.FIELDS<1> SETTING CCY.POS THEN
        Y.CURRENCY = D.RANGE.AND.VALUE<CCY.POS>
        SEL.CMD := " AND WITH CURRENCY.1 EQ ":Y.CURRENCY
        SEL.CMD1 := " AND WITH CURRENCY.1 EQ ":Y.CURRENCY
    END

    LOCATE "TELLER.TXN.REF" IN D.FIELDS<1> SETTING TXN.POS THEN
        Y.TELLER.TRANSACTION = D.RANGE.AND.VALUE<TXN.POS>
        SEL.CMD := " AND WITH TRANSACTION.CODE EQ ":Y.TELLER.TRANSACTION
        SEL.CMD1 := " AND WITH TRANSACTION.CODE EQ ":Y.TELLER.TRANSACTION
    END
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SELECT.PROCESS:
*==============
    SEL.CMD1 := ' BY.DSND @ID'

    CALL EB.READLIST(SEL.CMD,SEL.LIST,"",NO.OF.REC,RET.CODE)
    CALL EB.READLIST(SEL.CMD1,SEL.LIST1,"",NO.OF.REC.HIS,RET.CODE1)

    Y.ALL.IDS = SEL.LIST:@FM:SEL.LIST1

    LOOP
        REMOVE ORG.TELL.ID FROM Y.ALL.IDS SETTING TELLER.POS
    WHILE ORG.TELL.ID:TELLER.POS
        LOCATE FIELD(ORG.TELL.ID,';',1) IN Y.PROCESSED.LIST<1> SETTING Y.PRO.POS THEN
            CONTINUE
        END ELSE
            Y.PROCESSED.LIST<-1> = FIELD(ORG.TELL.ID,';',1)
        END
        FINDSTR ';' IN ORG.TELL.ID SETTING COLON.POS THEN
            GOSUB HISTORY.PROCESS
        END ELSE
            GOSUB TELLER.PROCESS
        END
    REPEAT
RETURN
*---------------------------------------------------------------------------
TELLER.PROCESS:
*---------------
    R.TELLER = ''
    TELL.ERR = ''
    Y.DATE.TIME = ''
    CALL F.READ(FN.TELLER,ORG.TELL.ID,R.TELLER,F.TELLER,TELL.ERR)
    Y.CR.DR.MARKER = R.TELLER<TT.TE.DR.CR.MARKER>; Y.ACCOUNT1 = R.TELLER<TT.TE.ACCOUNT.1>; Y.ACCOUNT2 = R.TELLER<TT.TE.ACCOUNT.2>; Y.CCY1  = R.TELLER<TT.TE.CURRENCY.1>
    Y.CCY2  = R.TELLER<TT.TE.CURRENCY.2>; Y.CASHIER.NO = R.TELLER<TT.TE.TELLER.ID.1>; Y.CASHIER.NO2 = R.TELLER<TT.TE.TELLER.ID.2>; Y.TRANSACTION.CODE = R.TELLER<TT.TE.TRANSACTION.CODE>
    CALL F.READ(FN.TELLER.TRANSACTION,Y.TRANSACTION.CODE,R.TELLER.TXN,F.TELLER.TRANSACTION,TRANS.ERR)
    Y.TRANSACTION.TYPE = R.TELLER.TXN<TT.TR.DESC,1,1>; Y.DATE.TIME = R.TELLER<TT.TE.DATE.TIME>
    Y.CARD.ACCOUNT = R.TELLER<TT.TE.LOCAL.REF,Y.CARD.POS>; Y.TFS.VALUE = R.TELLER<TT.TE.LOCAL.REF,Y.TFSPOS>; Y.INITIAL.ID = R.TELLER<TT.TE.LOCAL.REF,L.INITAL.POS>
    Y.AZ.REF       = R.TELLER<TT.TE.LOCAL.REF,L.AZ.REFPOS>; Y.CRE.AMT  = R.TELLER<TT.TE.LOCAL.REF,Y.CREAMTPOS>; Y.DEB.AMT = R.TELLER<TT.TE.LOCAL.REF,Y.DEB.AMTPOS>

    Y.DENOM = ''; Y.UNIT = ''; Y.DENOM1 = R.TELLER<TT.TE.DR.DENOM>; Y.UNIT1 =  R.TELLER<TT.TE.DR.UNIT>; Y.DENOM2 = R.TELLER<TT.TE.DENOMINATION>; Y.UNIT2 = R.TELLER<TT.TE.UNIT>
*
    IF Y.DENOM1 EQ "" AND Y.DENOM2 EQ "" THEN
        RETURN
    END ELSE
        GOSUB CASH.DETAIL
        GOSUB TELLER.CCY
    END
RETURN
*---------------------------------------------------------------------------
HISTORY.PROCESS:
*---------------
    TELL.ERR = ''; R.TELLER.HIS = ''; Y.DATE.TIME = ''
    CALL F.READ(FN.TELLER.HIS,ORG.TELL.ID,R.TELLER.HIS,F.TELLER.HIS,TELL.ERR)
    Y.CR.DR.MARKER = R.TELLER.HIS<TT.TE.DR.CR.MARKER>; Y.ACCOUNT1 = R.TELLER.HIS<TT.TE.ACCOUNT.1>
    Y.ACCOUNT2 = R.TELLER.HIS<TT.TE.ACCOUNT.2>; Y.CCY1 = R.TELLER.HIS<TT.TE.CURRENCY.1>; Y.CCY2 = R.TELLER.HIS<TT.TE.CURRENCY.2>
    Y.CASHIER.NO = R.TELLER.HIS<TT.TE.TELLER.ID.1>; Y.CASHIER.NO2 = R.TELLER.HIS<TT.TE.TELLER.ID.2>; Y.TRANSACTION.CODE = R.TELLER.HIS<TT.TE.TRANSACTION.CODE>
    CALL F.READ(FN.TELLER.TRANSACTION,Y.TRANSACTION.CODE,R.TELLER.TXN,F.TELLER.TRANSACTION,TRANS.ERR)
    Y.TRANSACTION.TYPE = R.TELLER.TXN<TT.TR.DESC,1,1>; Y.DATE.TIME = R.TELLER.HIS<TT.TE.DATE.TIME>
    Y.CARD.ACCOUNT = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.CARD.POS>; Y.TFS.VALUE = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.TFSPOS>; Y.INITIAL.ID = R.TELLER.HIS<TT.TE.LOCAL.REF,L.INITAL.POS>
    Y.AZ.REF = R.TELLER.HIS<TT.TE.LOCAL.REF,L.AZ.REFPOS>; Y.CRE.AMT = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.CREAMTPOS>; Y.DEB.AMT = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.DEB.AMTPOS>

    Y.DENOM = ''; Y.UNIT = ''; Y.DENOM1 = R.TELLER.HIS<TT.TE.DR.DENOM>; Y.UNIT1 =  R.TELLER.HIS<TT.TE.DR.UNIT>; Y.DENOM2 = R.TELLER.HIS<TT.TE.DENOMINATION>; Y.UNIT2 = R.TELLER.HIS<TT.TE.UNIT>
*
    IF Y.DENOM1 EQ "" AND Y.DENOM2 EQ "" THEN
        RETURN
    END ELSE
        GOSUB CASH.DETAIL.HIS
        GOSUB TELLER.CCY
    END
RETURN
*---------------------------------------------------------------------------
ACCT.CREDIT.LOAN:
*-----------------
    IF Y.CARD.ACCOUNT EQ '' THEN
        GOSUB FIND.CUSTOMER.ACCOUNT
    END
RETURN
*---------------------------------------------------------------------------
LEGACY.SYSTEM:
*---------------
    Y.ALT.ACCT.ID = ''; Y.LEGACY.ACCOUNT = ''; R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,Y.CARD.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.ALT.ACCT.TYPE = R.ACCOUNT<AC.ALT.ACCT.TYPE>
    LOCATE 'ALTERNO1' IN Y.ALT.ACCT.TYPE<1,1> SETTING LEGA.POS THEN
        Y.ALT.ACCT.ID = R.ACCOUNT<AC.ALT.ACCT.ID,LEGA.POS>
    END
    Y.LEGACY.ACCOUNT = Y.ALT.ACCT.ID
RETURN
*---------------------------------------------------------------------------
CASH.DETAIL:
*------------
    Y.CASH.FLAG = ''; Y.CASH.DETAIL = ''
    IF Y.CCY1 EQ 'DOP' AND Y.CCY2 EQ 'DOP' THEN
        IF Y.DENOM1 NE '' THEN
            Y.CASH.DETAIL = R.TELLER<TT.TE.LOCAL.REF,Y.DEB.AMTPOS>
            IF Y.CASH.DETAIL EQ "" THEN
                Y.CASH.DETAIL = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
            END
        END
        IF Y.DENOM2 NE '' THEN
            Y.CASH.DETAIL = R.TELLER<TT.TE.LOCAL.REF,Y.CREAMTPOS>
            IF Y.CASH.DETAIL EQ "" THEN
                Y.CASH.DETAIL = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
            END
        END
        Y.CASH.DETAIL = Y.CASH.DETAIL<1,1>
        Y.CASH.FLAG = 1
    END
*----
    IF Y.CCY1 EQ 'DOP' AND Y.CCY2 NE 'DOP' AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER<TT.TE.AMOUNT.LOCAL.1>; Y.AMOUNT2 = R.TELLER<TT.TE.AMOUNT.FCY.2>
        GOSUB COMMON.CASH.PROCESS
        Y.CASH.FLAG = 1
    END
*---
    GOSUB CASH.DETAIL.2
RETURN
*--------------------------------------------------------------------------------------------------------------
CASH.DETAIL.2:
*-------------
    IF Y.CCY1 NE 'DOP' AND Y.CCY2 EQ 'DOP' AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER<TT.TE.AMOUNT.FCY.1>; Y.AMOUNT2 = R.TELLER<TT.TE.AMOUNT.LOCAL.2>
*GOSUB COMMON.CASH.PROCESS
        Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.AMOUNT2
        Y.CASH.FLAG = 1
    END
*---
    IF Y.CCY1 NE 'DOP' AND Y.CCY2 NE 'DOP' AND Y.CCY1 NE Y.CCY2 AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER<TT.TE.AMOUNT.FCY.1>; Y.AMOUNT2 = R.TELLER<TT.TE.AMOUNT.FCY.2>
        GOSUB COMMON.CASH.PROCESS
        Y.CASH.FLAG = 1
    END
*---
    IF Y.CCY1 NE 'DOP' AND Y.CCY2 NE 'DOP' AND Y.CCY1 EQ Y.CCY2 AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER<TT.TE.AMOUNT.FCY.1>; Y.AMOUNT2 = R.TELLER<TT.TE.AMOUNT.FCY.2>
        GOSUB COMMON.CASH.PROCESS
        Y.CASH.DETAIL = Y.CASH.DETAIL<1,1>
        Y.CASH.FLAG = 1
    END
RETURN
*-------------------------------------------------------------------------------------------------------------------------------------------------------------
COMMON.CASH.PROCESS:
*===================
    IF Y.DENOM1 NE '' THEN
        Y.DEB.AMT = R.TELLER<TT.TE.LOCAL.REF,Y.DEB.AMTPOS>
        Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.DEB.AMT
        IF Y.DEB.AMT EQ "" THEN
            Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.AMOUNT2
        END
    END
    IF Y.DENOM1 EQ "" AND Y.DENOM2 NE '' THEN
        Y.CRE.AMT = R.TELLER<TT.TE.LOCAL.REF,Y.CREAMTPOS>
        Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.CRE.AMT
        IF Y.CRE.AMT EQ "" THEN
            Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.AMOUNT2
        END
    END
RETURN
*---------------------------------------------------------------------------
CASH.DETAIL.HIS:
*---------------
    Y.CASH.FLAG = ''; Y.CASH.DETAIL = ''

    IF Y.CCY1 EQ 'DOP' AND Y.CCY2 EQ 'DOP' THEN
        IF Y.DENOM1 NE '' THEN
            Y.CASH.DETAIL = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.DEB.AMTPOS>
            IF Y.CASH.DETAIL EQ "" THEN
                Y.CASH.DETAIL = R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>
            END
        END
        IF Y.DENOM2 NE '' THEN
            Y.CASH.DETAIL = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.CREAMTPOS>
            IF Y.CASH.DETAIL EQ "" THEN
                Y.CASH.DETAIL = R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>
            END
        END
        Y.CASH.DETAIL = Y.CASH.DETAIL<1,1>
        Y.CASH.FLAG = 1
    END
*---
    IF Y.CCY1 EQ 'DOP' AND Y.CCY2 NE 'DOP' AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.1>; Y.AMOUNT2 = R.TELLER.HIS<TT.TE.AMOUNT.FCY.2>
        GOSUB COMMON.CASH.PROCESS.HISTORY
        Y.CASH.FLAG = 1
    END
*---
    GOSUB CASH.DETAIL.HIS.2
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------
CASH.DETAIL.HIS.2:
*------------------
    IF Y.CCY1 NE 'DOP' AND Y.CCY2 EQ 'DOP' AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER.HIS<TT.TE.AMOUNT.FCY.1>; Y.AMOUNT2 = R.TELLER.HIS<TT.TE.AMOUNT.LOCAL.2>
        Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.AMOUNT2
        Y.CASH.FLAG = 1
    END
*---
    IF Y.CCY1 NE 'DOP' AND Y.CCY2 NE 'DOP' AND Y.CCY1 NE Y.CCY2 AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER.HIS<TT.TE.AMOUNT.FCY.1>; Y.AMOUNT2 = R.TELLER.HIS<TT.TE.AMOUNT.FCY.2>
        GOSUB COMMON.CASH.PROCESS.HISTORY
        Y.CASH.FLAG = 1
    END
*---
    IF Y.CCY1 NE 'DOP' AND Y.CCY2 NE 'DOP' AND Y.CCY1 EQ Y.CCY2 AND Y.CASH.FLAG EQ '' THEN
        Y.AMOUNT1 = R.TELLER.HIS<TT.TE.AMOUNT.FCY.1>; Y.AMOUNT2 = R.TELLER.HIS<TT.TE.AMOUNT.FCY.2>
        GOSUB COMMON.CASH.PROCESS.HISTORY
        Y.CASH.DETAIL = Y.CASH.DETAIL<1,1>
        Y.CASH.FLAG = 1
    END
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------
COMMON.CASH.PROCESS.HISTORY:
*===========================
    IF Y.DENOM1 NE '' THEN
        Y.DEB.AMT = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.DEB.AMTPOS>
        Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.DEB.AMT
        IF Y.DEB.AMT EQ "" THEN
            Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.AMOUNT2
        END
    END

    IF Y.DENOM1 EQ "" AND Y.DENOM2 NE '' THEN
        Y.CRE.AMT = R.TELLER.HIS<TT.TE.LOCAL.REF,Y.CREAMTPOS>
        Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.CRE.AMT
        IF Y.CRE.AMT EQ "" THEN
            Y.CASH.DETAIL = Y.AMOUNT1:@VM:Y.AMOUNT2
        END
    END
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------
DENOM.DETAIL:
*--------------
    IF (Y.DENOM1 NE '' AND Y.DENOM2 NE '') AND (Y.DENOM1 EQ Y.DENOM2) THEN
        Y.DENOM = Y.DENOM1; Y.UNIT = Y.UNIT1
        GOSUB TWICE.DENOM
        GOSUB FORM.ARRAY
    END

    IF (Y.DENOM1 NE '' AND Y.DENOM2 NE '') AND (Y.DENOM1 NE Y.DENOM2) THEN
        Y.DENOM = Y.DENOM1; Y.UNIT = Y.UNIT1
        GOSUB TWICE.DENOM
        GOSUB FORM.ARRAY
        Y.DENOM = Y.DENOM2; Y.UNIT = Y.UNIT2
        GOSUB TWICE.DENOM
        GOSUB FORM.ARRAY
    END

    IF Y.DENOM1 NE '' AND Y.DENOM2 EQ '' THEN
        Y.DENOM = Y.DENOM1; Y.UNIT = Y.UNIT1
        GOSUB TWICE.DENOM
        GOSUB FORM.ARRAY
    END

    IF Y.DENOM1 EQ "" AND Y.DENOM2 NE "" THEN
        Y.DENOM = Y.DENOM2; Y.UNIT = Y.UNIT2
        GOSUB TWICE.DENOM
        GOSUB FORM.ARRAY
    END
RETURN
*---------------------------------------------------------------------------
TWICE.DENOM:
*************
    CHANGE @VM TO '*' IN Y.DENOM; CHANGE @VM TO '*' IN Y.UNIT

    INIT = '1'; Y.DENOMINATION.VALUE = ''
    Y.DENOM.COUNT = DCOUNT(Y.DENOM,'*')
    LOOP
    WHILE INIT LE Y.DENOM.COUNT
        Y.DENOM.VALUE = FIELD(Y.DENOM,'*',INIT)
        Y.DENOM.UNIT = FIELD(Y.UNIT,'*',INIT)
        IF Y.DENOM.VALUE AND Y.DENOM.UNIT THEN
            Y.DENOMINATION.VALUE<-1> = Y.DENOM.VALUE:'*':Y.DENOM.UNIT
        END
        INIT++
    REPEAT
    CHANGE @FM TO @VM IN Y.DENOMINATION.VALUE
RETURN
*---------------------------------------------------------------------------
FIND.CUSTOMER.ACCOUNT:
*------------------------
    Y.ACCOUNT.ID = Y.ACCOUNT1
    GOSUB READ.ACCOUNT
    IF Y.CARD.ACCOUNT EQ '' THEN
        Y.ACCOUNT.ID = Y.ACCOUNT2
        GOSUB READ.ACCOUNT
    END
RETURN
*---------------------------------------------------------------------------
READ.ACCOUNT:
*---------------
    R.ACCOUNT = ''; ACC.ERR = ''; Y.AC.CATEGORY = ''
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.AC.CATEGORY =  R.ACCOUNT<AC.CATEGORY>
    IF Y.AC.CATEGORY GE 1000 AND Y.AC.CATEGORY LE 9999 THEN
        Y.CARD.ACCOUNT = Y.ACCOUNT.ID
    END
RETURN
*---------------------------------------------------------------------------
TELLER.CCY:
*-----------
    IF Y.DATE.TIME NE '' THEN
        Y.DATE.ALONE1 = Y.DATE.TIME[1,6]; Y.DATE.ALONE = "20":Y.DATE.ALONE1; Y.TIME.ALONE = Y.DATE.TIME[4]; Y.ICONV.DATE = ICONV(Y.DATE.ALONE,'D')
        Y.CONV.DATE = OCONV(Y.ICONV.DATE,'D4 '); Y.MONTH =  OCONV(Y.ICONV.DATE,'DMA'); Y.PROCESS.ACTUAL = Y.DATE.ALONE; Y.TIME.ACTUAL    = Y.TIME.ALONE[1,2]:':':Y.TIME.ALONE[3,2]
    END
*
    GOSUB ACCT.CREDIT.LOAN
    GOSUB LEGACY.SYSTEM

    Y.CASH.DETAIL.COUNT = DCOUNT(Y.CASH.DETAIL,@VM); Y.FIRST.AMOUNT = FIELD(Y.CASH.DETAIL,@VM,1); Y.SECOND.AMOUNT = FIELD(Y.CASH.DETAIL,@VM,2)
    IF Y.CCY1 EQ Y.CCY2 THEN
        Y.DISPLAY.CURRENCY = Y.CCY1; Y.CCY2 = Y.CCY1
        Y.DISPLAY.TXN.REF = ORG.TELL.ID
        IF Y.TELLER.ID THEN
            Y.DISPLAY.CASHIER = Y.TELLER.ID
        END ELSE
            Y.DISPLAY.CASHIER = Y.CASHIER.NO
        END
        Y.DISPLAY.TXN.TYPE = Y.TRANSACTION.TYPE; Y.DISPLAY.TIME = Y.PROCESS.ACTUAL; Y.DISPLAY.CARD = Y.CARD.ACCOUNT; Y.DISPLAY.LEGACY = Y.LEGACY.ACCOUNT; Y.DISPLAY.CASH = Y.CASH.DETAIL
        GOSUB SAME.CCY.2
    END

    IF (Y.CCY1 NE Y.CCY2) AND (Y.CASH.DETAIL.COUNT GT 1) THEN
        Y.DISPLAY.CURRENCY = Y.CCY1
        Y.DISPLAY.TXN.REF = ORG.TELL.ID
        IF Y.TELLER.ID THEN
            Y.DISPLAY.CASHIER = Y.TELLER.ID
        END ELSE
            Y.DISPLAY.CASHIER = Y.CASHIER.NO
        END
        Y.DISPLAY.TXN.TYPE = Y.TRANSACTION.TYPE; Y.DISPLAY.TIME = Y.PROCESS.ACTUAL; Y.DISPLAY.CARD = Y.CARD.ACCOUNT; Y.DISPLAY.LEGACY = Y.LEGACY.ACCOUNT; Y.DISPLAY.CASH = Y.FIRST.AMOUNT
        Y.CHK.CCY = Y.CCY1
        GOSUB DIFF.CCY.2
    END
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------
SAME.CCY.2:
*==========
    IF Y.TFS.VALUE NE '' THEN
        GOSUB TFS.PROCESS
        RETURN
    END
*
    IF Y.AZ.REF THEN
        Y.DISPLAY.CARD = Y.AZ.REF
    END
*
    Y.CAT.AC.1 = Y.ACCOUNT1[4,5]; Y.CAT.AC.2 = Y.ACCOUNT2[4,5]
    IF Y.CAT.AC.1 NE "10001" THEN
        Y.DISPLAY.CARD = Y.ACCOUNT1
    END
    IF Y.CAT.AC.2 NE "10001" THEN
        Y.DISPLAY.CARD = Y.ACCOUNT2
    END

    IF Y.CARD.ACCOUNT THEN
        Y.DISPLAY.CARD = Y.CARD.ACCOUNT
    END

    IF (Y.INITIAL.ID NE '' AND Y.INITIAL.ID[1,2] EQ "FT") THEN
        Y.DISPLAY.CARD = Y.INITIAL.ID
    END
*---
    GOSUB DENOM.DETAIL

*--->Based on condition teller record is displaying twice

    IF (Y.TRANSACTION.CODE EQ "1" AND Y.TELLER.ID EQ "") THEN
        Y.DISPLAY.CASHIER = Y.CASHIER.NO2
        GOSUB FORM.ARRAY
        RETURN
    END
RETURN
*------------------------------------------------------------------------------------------------------------------------------------
DIFF.CCY.2:
*==========
    IF Y.TFS.VALUE NE '' THEN
        GOSUB TFS.PROCESS
        RETURN
    END
*
    IF Y.AZ.REF THEN
        Y.DISPLAY.CARD = Y.AZ.REF
    END
*
    Y.CAT.AC.1 = Y.ACCOUNT1[4,5]; Y.CAT.AC.2 = Y.ACCOUNT2[4,5]
    IF Y.CAT.AC.1 NE "10001" THEN
        Y.DISPLAY.CARD = Y.ACCOUNT1
    END
    IF Y.CAT.AC.2 NE "10001" THEN
        Y.DISPLAY.CARD = Y.ACCOUNT2
    END

    IF Y.CARD.ACCOUNT THEN
        Y.DISPLAY.CARD = Y.CARD.ACCOUNT
    END
*
    IF (Y.INITIAL.ID NE '' AND Y.INITIAL.ID[1,2] EQ "FT") THEN
        Y.DISPLAY.CARD = Y.INITIAL.ID
    END
*---
    GOSUB DENOM.DETAIL.DIFFCCY
*---
    IF (Y.TRANSACTION.CODE EQ "1" AND Y.TELLER.ID EQ "") THEN
        Y.DISPLAY.CASHIER = Y.CASHIER.NO2
        GOSUB FORM.ARRAY
    END
RETURN
*---------------------------------------------------------------------------
FORM.ARRAY:
*------------
    Y.DUP.ARRAY = Y.DISPLAY.CASHIER:"$":Y.DISPLAY.TXN.TYPE:"$":Y.DISPLAY.TXN.REF:"$": Y.DISPLAY.TIME:"$":Y.DISPLAY.CARD:"$":Y.DISPLAY.LEGACY:"$":Y.DISPLAY.CURRENCY:"$":Y.DISPLAY.CASH:"$":Y.DENOMINATION.VALUE:"$":Y.TIME.ACTUAL
    Y.SORT.VAL  = Y.DISPLAY.CURRENCY:Y.DATE.TIME
    Y.FINAL.SORT.VAL<-1> = Y.DUP.ARRAY:@FM:Y.SORT.VAL
    Y.SORT.ARR<-1> = Y.SORT.VAL
RETURN
*---------------------------------------------------------------------------
FORM.OUT.ARRAY:
*---------------
    Y.SORT.ARR = SORT(Y.SORT.ARR)
    LOOP
        REMOVE Y.ARR.ID FROM Y.SORT.ARR SETTING Y.ARR.POS
    WHILE Y.ARR.ID : Y.ARR.POS
        LOCATE Y.ARR.ID IN Y.FINAL.SORT.VAL SETTING Y.FM.POS THEN
            Y.OUT.ARRAY<-1> = Y.FINAL.SORT.VAL<Y.FM.POS-1>
            DEL Y.FINAL.SORT.VAL<Y.FM.POS>
            DEL Y.FINAL.SORT.VAL<Y.FM.POS-1>
        END
    REPEAT
*
    GOSUB LAST.PROCESS
RETURN
*---------------------------------------------------------------------------
DENOM.DETAIL.DIFFCCY:
*=====================
    BEGIN CASE
        CASE Y.DENOM1 NE "" AND Y.DENOM2 NE ""
            Y.DENOM = Y.DENOM1:@VM:Y.DENOM2; Y.UNIT = Y.UNIT1:@VM:Y.UNIT2
            GOSUB TWICE.DENOM.DIFFCCY
            GOSUB FORM.ARRAY
            Y.CHK.CCY = Y.CCY2; Y.DISPLAY.CASH = Y.SECOND.AMOUNT; Y.DISPLAY.CURRENCY = Y.CCY2; Y.DENOM = Y.DENOM1:@VM:Y.DENOM2; Y.UNIT = Y.UNIT1:@VM:Y.UNIT2
            GOSUB TWICE.DENOM.DIFFCCY
            GOSUB FORM.ARRAY
        CASE Y.DENOM1 NE "" AND Y.DENOM2 EQ ""
            Y.DENOM = Y.DENOM1; Y.UNIT = Y.UNIT1; Y.CHK = Y.DENOM1<1,1>; Y.CHK.CCY = Y.CHK[1,3]; Y.DISPLAY.CURRENCY = Y.CHK.CCY; Y.DISPLAY.CASH = Y.DEB.AMT
            GOSUB TWICE.DENOM.DIFFCCY
            GOSUB FORM.ARRAY
        CASE Y.DENOM1 EQ "" AND Y.DENOM2 NE ""
            Y.DENOM = Y.DENOM2; Y.UNIT = Y.UNIT2; Y.CHK = Y.DENOM2<1,1>; Y.CHK.CCY = Y.CHK[1,3]; Y.DISPLAY.CURRENCY = Y.CHK.CCY; Y.DISPLAY.CASH = Y.CRE.AMT
            GOSUB TWICE.DENOM.DIFFCCY
            GOSUB FORM.ARRAY
    END CASE
RETURN
*------------------------------------------------------------------------
TWICE.DENOM.DIFFCCY:
*******************
    CHANGE @VM TO '*' IN Y.DENOM
    CHANGE @VM TO '*' IN Y.UNIT
    INIT = 1
    Y.DENOMINATION.VALUE = ''
    Y.DENOM.COUNT = DCOUNT(Y.DENOM,'*')
    LOOP
    WHILE INIT LE Y.DENOM.COUNT
        Y.DENOM.VALUE = FIELD(Y.DENOM,'*',INIT); Y.DENOM.UNIT = FIELD(Y.UNIT,'*',INIT); Y.DENOM.CCY = Y.DENOM.VALUE[1,3]
        IF Y.DENOM.CCY EQ Y.CHK.CCY THEN
            IF Y.DENOM.VALUE AND Y.DENOM.UNIT THEN
                Y.DENOMINATION.VALUE<-1> = Y.DENOM.VALUE:'*':Y.DENOM.UNIT
            END
        END
        INIT++
    REPEAT
    CHANGE @FM TO @VM IN Y.DENOMINATION.VALUE
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------
TFS.PROCESS:
*===========
    CALL F.READ(FN.T24.FUND.SERVICES,Y.TFS.VALUE,R.T24.FUND.SERVICES,F.T24.FUND.SERVICES,TFS.ERR)
    Y.TT.TFS = R.T24.FUND.SERVICES<TFS.UNDERLYING>
*
    LOCATE Y.TFS.VALUE IN Y.PROC.TFS<1> SETTING Y.TFS.VALUE.POS THEN
        RETURN
    END
*
    Y.PROC.TFS<-1> = Y.TFS.VALUE
    LOOP
        REMOVE Y.TFS.TT.ID FROM Y.TT.TFS SETTING Y.TFS.TT.ID.POS
    WHILE Y.TFS.TT.ID : Y.TFS.TT.ID.POS
        LOCATE Y.TFS.TT.ID IN Y.TT.TFS<1,1> SETTING LOC.POS THEN
            Y.CR.DENOM = R.T24.FUND.SERVICES<TFS.CR.DENOM,LOC.POS>; Y.CR.UNIT  = R.T24.FUND.SERVICES<TFS.CR.DEN.UNIT,LOC.POS>
            Y.DR.DENOM = R.T24.FUND.SERVICES<TFS.DR.DENOM,LOC.POS>; Y.DR.UNIT  = R.T24.FUND.SERVICES<TFS.DR.DEN.UNIT,LOC.POS>
            Y.TRAN.TFS = R.T24.FUND.SERVICES<TFS.TRANSACTION,LOC.POS>; Y.DISPLAY.CARD = R.T24.FUND.SERVICES<TFS.SURROGATE.AC,LOC.POS>
            Y.DISPLAY.CASH = R.T24.FUND.SERVICES<TFS.AMOUNT,LOC.POS>
            GOSUB TFS.ANOTHER.PROCESS
        END
    REPEAT
RETURN
*-----------------------------------------------------------------------------------------------
TFS.ANOTHER.PROCESS:
*===================
    IF Y.CR.DENOM EQ "" AND Y.DR.DENOM EQ "" THEN
        RETURN
    END

    BEGIN CASE
        CASE Y.CR.DENOM NE "" AND Y.DR.DENOM EQ ""
            Y.DENOM = Y.CR.DENOM; Y.UNIT = Y.CR.UNIT
        CASE Y.CR.DENOM EQ "" AND Y.DR.DENOM NE ""
            Y.DENOM = Y.DR.DENOM; Y.UNIT = Y.DR.UNIT
        CASE Y.CR.DENOM NE "" AND Y.DR.DENOM NE ""
            Y.DENOM = Y.CR.DENOM:@VM:Y.DR.DENOM; Y.UNIT = Y.CR.UNIT:@VM:Y.DR.UNIT
    END CASE

    CHANGE @SM TO @VM IN Y.DENOM
    CHANGE @SM TO @VM IN Y.UNIT

    Y.DISPLAY.TXN.REF = Y.TFS.TT.ID
    CALL F.READ(FN.TFS.TRANSACTION,Y.TRAN.TFS,R.TFS.TRANSACTION,F.TFS.TRANSACTION,TFS.TRANSACTION.ERR)
    Y.DISPLAY.TXN.TYPE = R.TFS.TRANSACTION<TFS.TXN.DESCRIPTION,2>

    IF Y.CCY1 EQ Y.CCY2 THEN
        GOSUB TWICE.DENOM
        GOSUB FORM.ARRAY
    END ELSE
        GOSUB TWICE.DENOM.DIFFCCY
        GOSUB FORM.ARRAY
    END
RETURN
*-----------------------------------------------------------------------------------------------------------------
LAST.PROCESS:
*============
    IF Y.TELLER.ID THEN
        SEL.TT.CMD = "SELECT " : FN.TELLER.ID.HIS : " WITH DIFFERENCE NE " : '0':" AND @ID LIKE ":Y.TELLER.ID:"..."
    END ELSE
        SEL.TT.CMD = "SELECT " : FN.TELLER.ID.HIS : " WITH DIFFERENCE NE " : '0'
    END
    CALL EB.READLIST(SEL.TT.CMD,SEL.TT.LIST,"",NO.OF.REC.TT,RET.CODE.TT)

    LOOP
        REMOVE Y.TT.ID FROM SEL.TT.LIST SETTING Y.TT.ID.POS
    WHILE Y.TT.ID:Y.TT.ID.POS
        CALL F.READ(FN.TELLER.ID.HIS,Y.TT.ID,R.TELLER.ID.HIS,F.TELLER.ID.HIS,TELLER.ID.ERR)
        Y.DISPLAY.CASHIER = Y.TT.ID
        Y.OVERRIDE = R.TELLER.ID.HIS<TT.TID.OVERRIDE>
        FINDSTR 'SOBRANTE' OR 'FALTANTE' IN Y.OVERRIDE<1,1> SETTING OVER.POS THEN
            Y.DISPLAY.TXN.TYPE = R.TELLER.ID.HIS<TT.TID.OVERRIDE,OVER.POS>
            Y.DISPLAY.TXN.TYPE.1 = FIELD(Y.DISPLAY.TXN.TYPE,'}',2,3)
            CHANGE '&' TO "" IN Y.DISPLAY.TXN.TYPE.1
            CHANGE '{' TO "" IN Y.DISPLAY.TXN.TYPE.1
            CHANGE '}' TO " " IN Y.DISPLAY.TXN.TYPE.1
            Y.DISPLAY.TXN.REF = R.TELLER.ID.HIS<TT.TID.STMT.NO,1>; Y.DISPLAY.TIME = R.TELLER.ID.HIS<TT.TID.DATE.OF.CLOSE>
            Y.TIME.ACTUAL = R.TELLER.ID.HIS<TT.TID.TIME.OF.CLOSE>
            GOSUB GET.CCY.CASH
            Y.DISPLAY.CARD = ""; Y.DISPLAY.LEGACY = ""; Y.DENOMINATION.VALUE = ""
            Y.DISPLAY.TXN.TYPE = Y.DISPLAY.TXN.TYPE.1
            Y.OUT.ARRAY<-1> = Y.DISPLAY.CASHIER:"$":Y.DISPLAY.TXN.TYPE:"$":Y.DISPLAY.TXN.REF:"$": Y.DISPLAY.TIME:"$":Y.DISPLAY.CARD:"$":Y.DISPLAY.LEGACY:"$":Y.DISPLAY.CURRENCY:"$":Y.DISPLAY.CASH:"$":Y.DENOMINATION.VALUE:"$":Y.TIME.ACTUAL
        END

        Y.DISPLAY.TXN.TYPE = ""
        Y.DISPLAY.TXN.TYPE.1 = ""
        Y.DISPLAY.TXN.TYPE.2 = ""
    REPEAT
***
RETURN
*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
GET.CCY.CASH:
*************
    TT.FLAG = ""; Y.DIFFERENCE = R.TELLER.ID.HIS<TT.TID.DIFFERENCE>; Y.CURRENCY   = R.TELLER.ID.HIS<TT.TID.CURRENCY>

    CNT = "1"
    DCNT.DIF = DCOUNT(Y.DIFFERENCE,@VM)
    LOOP
    WHILE CNT LE DCNT.DIF
        Y.DIF = Y.DIFFERENCE<1,CNT>
        IF (Y.DIF NE "0") AND (TT.FLAG NE "1") THEN
            Y.DISPLAY.CURRENCY = Y.CURRENCY<1,CNT>; Y.DISPLAY.CASH = ABS(Y.DIF); TT.FLAG = "1"
        END
        CNT = CNT + 1
    REPEAT
RETURN
*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
END
