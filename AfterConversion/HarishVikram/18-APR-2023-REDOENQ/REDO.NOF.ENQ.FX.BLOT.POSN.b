* @ValidationCode : MjozNzc0MDI2MDA6Q3AxMjUyOjE2ODE4MDU5OTE3MzQ6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Apr 2023 13:49:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOF.ENQ.FX.BLOT.POSN(Y.ARR)
*********************************************************************************************************
*Company   Name    : APAP Bank
*Developed By      : Temenos Application Management
*Program   Name    : REDO.NOF.FX.BLOT.POSN
*--------------------------------------------------------------------------------------------------------
*Description       : This is a nofile routine attached to the STANDARD.SELECTION:NOFILE.REDO.ENQ.FX.BLOT.POSN
*                    which is for ENQUIRY : REDO.ENQ.FX.BLOT.POSN.This to display position of given currency
*                    with their transaction details
*In Parameter      :N/A

*Out Parameter     :Y.ARR
*Files  Used       : REDO.NOF.FX.BLOT.POSN
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                  Reference                      Description
*   ------         ------               -------------                    -------------
*  11/08/2010      S.REKHA          ODR-2010-07-0073 FX-002            Initial Creation
*  31/10/2010      SHANKAR RAJU     ODR-2010-07-0073                   Modifications for PQC Issues
*  14/04/2011      Pradeep S        PACS00051758                       Mapping changed for Y.INIT.DOP.POSN
*  21/10/2011      Pradeep S        PACS00148243                       Selection criteria field changed for FX
*                                                                      Spanish description changed for BUY and SELL
*  22/11/2011      Pradeep S        PACS00157021                       Mapping changed for previous days balances
*
* 18-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM, ++ to +=, -- to -=
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FOREX
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.FX.CCY.POSN

    GOSUB INIT
    GOSUB PROCESS

RETURN
*--------------------------------------------------------------------------------------------------------
INIT:
*****
    FN.FOREX = 'F.FOREX'
    F.FOREX = ''
    CALL OPF(FN.FOREX,F.FOREX)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.FOREX.HIS = 'F.FOREX$HIS'
    F.FOREX.HIS = ''
    CALL OPF(FN.FOREX.HIS,F.FOREX.HIS)

    FN.FUNDS.TRANSFER.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER.HIS = ''
    CALL OPF(FN.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS)

    FN.TELLER.HIS = 'F.TELLER$HIS'
    F.TELLER.HIS = ''
    CALL OPF(FN.TELLER.HIS,F.TELLER.HIS)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.REDO.FX.CCY.POSN = 'F.REDO.FX.CCY.POSN'
    F.REDO.FX.CCY.POSN = ''
    CALL OPF(FN.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN)
    Y.INIT=1
    Y.TOT.SELL.FCY=''
    Y.TOT.SELL.DOP=''
    Y.TOT.BUY.FCY=''
    Y.TOT.BUY.DOP=''
    Y.USER=''
    Y.FINAL.DATE=''
    Y.INIT.FCY.POSN=''
    Y.INIT.DOP.POSN=''
    Y.FCY.POSN = ''
    Y.HIS.NEW.ID = ''
    Y.HIS.PREV.ID=''
    R.REDO.FX.CCY.POSN.LOC=''
    Y.CHK.DAYS = '100'
    Y.HIS.DATE=TODAY
    Y.FREQ = '-':'1':'C'
    CALL CDT('',Y.HIS.DATE,Y.FREQ)
RETURN

*--------------------------------------------------------------------------------------------------------
PROCESS:
********
    GOSUB SELECT.APPLN

    GOSUB SEL.FLDS.LOCATE

    GOSUB SEL.FLDS.PROCESS

    GOSUB GET.TXN.IDS

    GOSUB TXN.ID.PROCESS

RETURN

SELECT.APPLN:
*************

    Y.SEL.FX.CMD = 'SELECT ':FN.FOREX: ' WITH L.FX.FXSN.NUM NE "" '

    Y.SEL.FT.CMD = 'SELECT ':FN.FUNDS.TRANSFER: ' WITH L.FT.FXSN.NUM NE "" '

    Y.SEL.TT.CMD = 'SELECT ':FN.TELLER: ' WITH L.TT.FXSN.NUM  NE "" '

*Y.SEL.FX.HIS.CMD = 'SELECT ':FN.FOREX.HIS: ' WITH RECORD.STATUS NE "" AND WITH L.FX.FXSN.NUM NE "" '
    Y.SEL.FX.HIS.CMD = 'SELECT ':FN.FOREX.HIS: ' WITH STATUS EQ MAT AND WITH L.FX.FXSN.NUM NE "" '

*Y.SEL.FT.HIS.CMD = 'SELECT ':FN.FUNDS.TRANSFER.HIS: ' WITH RECORD.STATUS NE "" AND WITH L.FT.FXSN.NUM NE "" '
    Y.SEL.FT.HIS.CMD = 'SELECT ':FN.FUNDS.TRANSFER.HIS: ' WITH RECORD.STATUS EQ MAT AND WITH L.FT.FXSN.NUM NE "" '

*Y.SEL.TT.HIS.CMD = 'SELECT ':FN.TELLER.HIS: ' WITH RECORD.STATUS NE "" AND WITH L.TT.FXSN.NUM NE "" '
    Y.SEL.TT.HIS.CMD = 'SELECT ':FN.TELLER.HIS: ' WITH RECORD.STATUS EQ MAT AND WITH L.TT.FXSN.NUM NE "" '

RETURN

SEL.FLDS.LOCATE:
****************
    LOCATE 'INITIAL.DATE' IN D.FIELDS<1> SETTING DATE1.POS THEN
        Y.INITIAL.DATE = D.RANGE.AND.VALUE<DATE1.POS>
    END

    LOCATE 'FINAL.DATE' IN D.FIELDS<1> SETTING DATE2.POS THEN
        Y.FINAL.DATE = D.RANGE.AND.VALUE<DATE2.POS>
    END

    LOCATE 'CURRENCY' IN D.FIELDS<1> SETTING CURR.POS THEN
        Y.CURRENCY = D.RANGE.AND.VALUE<CURR.POS>
*PACS00051758 - S
        IF Y.CURRENCY EQ LCCY THEN
            ENQ.ERROR = 'EB-CCY.LOCAL.ERR'
            GOSUB PGM.END
        END
*PACS00051758 - E
    END

    LOCATE 'USER.NAME' IN D.FIELDS<1> SETTING USER.POS THEN
        Y.USER = D.RANGE.AND.VALUE<USER.POS>
    END

RETURN
*---------------------------------------------------------------------------------------------------------------
SEL.FLDS.PROCESS:
****************

    BEGIN CASE

        CASE Y.INITIAL.DATE NE '' AND Y.FINAL.DATE EQ ''
*Y.SEL.FX.CMD:=" AND WITH VALUE.DATE.BUY GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.BUY LE ':TODAY
            Y.SEL.FX.CMD:=" AND WITH DEAL.DATE GE ":Y.INITIAL.DATE:' AND WITH DEAL.DATE LE ':TODAY
            Y.SEL.FT.CMD:=" AND WITH DEBIT.VALUE.DATE GE ":Y.INITIAL.DATE:' AND WITH DEBIT.VALUE.DATE LE ':TODAY
            Y.SEL.TT.CMD:=" AND WITH VALUE.DATE.1 GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.1 LE ':TODAY
            Y.DATE1 = Y.INITIAL.DATE
            Y.DATE2 = TODAY
            NO.OF.DAYS = 'C'
            CALL CDD('',Y.DATE1,Y.DATE2,NO.OF.DAYS)
            Y.DAYS.CNT = NO.OF.DAYS

            IF Y.DAYS.CNT THEN
*Y.SEL.FX.HIS.CMD:=" AND WITH VALUE.DATE.BUY GE ":Y.INITIAL.DATE: ' AND WITH VALUE.DATE.BUY LE ':TODAY: ' AND DEAL.DATE LE ':TODAY
                Y.SEL.FX.HIS.CMD:=" AND WITH DEAL.DATE GE ":Y.INITIAL.DATE: ' AND WITH DEAL.DATE LE ':TODAY
                Y.SEL.FT.HIS.CMD:=" AND WITH DEBIT.VALUE.DATE GE ":Y.INITIAL.DATE: ' AND WITH DEBIT.VALUE.DATE LE ':TODAY
                Y.SEL.TT.HIS.CMD:=" AND WITH VALUE.DATE.1 GE ":Y.INITIAL.DATE: ' AND WITH VALUE.DATE.1 LE ': TODAY
            END

            LOOP
            WHILE Y.INIT LE Y.CHK.DAYS
                Y.FREQ = '-1C'
                CALL CDT('',Y.DATE1,Y.FREQ)
                Y.ID = Y.CURRENCY:Y.DATE1
                CALL F.READ(FN.REDO.FX.CCY.POSN,Y.ID,R.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN,Y.CCY.ERR)
                IF R.REDO.FX.CCY.POSN NE '' AND R.REDO.FX.CCY.POSN<REDO.FX.TOTAL.POSN> THEN
                    Y.INIT.FCY.POSN=R.REDO.FX.CCY.POSN<REDO.FX.TOTAL.POSN>
                    Y.PREV.WGT.BUY.RATE = R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG>
                    GOSUB GET.PREV.WGT.BUY.RATE1
                    Y.INIT.DOP.POSN = Y.INIT.FCY.POSN * Y.PREV.WGT.BUY.RATE       ;* PACS00157021 - S/E
                    EXIT
                END
                Y.INIT += 1
            REPEAT

        CASE Y.INITIAL.DATE NE '' AND Y.FINAL.DATE NE ''
*Y.SEL.FX.CMD:=" AND WITH VALUE.DATE.BUY GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.BUY LE ':Y.FINAL.DATE
            Y.SEL.FX.CMD:=" AND WITH DEAL.DATE GE ":Y.INITIAL.DATE:' AND WITH DEAL.DATE LE ':Y.FINAL.DATE
            Y.SEL.FT.CMD:=" AND WITH DEBIT.VALUE.DATE GE ":Y.INITIAL.DATE:' AND WITH DEBIT.VALUE.DATE LE ':Y.FINAL.DATE
            Y.SEL.TT.CMD:=" AND WITH VALUE.DATE.1 GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.1 LE ':Y.FINAL.DATE

            GOSUB CURR.POSN.TABL

    END CASE

    IF Y.CURRENCY THEN
        Y.SEL.FX.CMD:=" AND WITH (CURRENCY.BOUGHT EQ ":Y.CURRENCY:" OR CURRENCY.SOLD EQ ":Y.CURRENCY:')'
        Y.SEL.FT.CMD:=" AND WITH (DEBIT.CURRENCY EQ ":Y.CURRENCY:" OR CREDIT.CURRENCY EQ ":Y.CURRENCY:')'
        Y.SEL.TT.CMD:=" AND WITH (CURRENCY.1 EQ ":Y.CURRENCY:" OR CURRENCY.2 EQ ":Y.CURRENCY:')'
    END

    IF Y.DAYS.CNT AND Y.CURRENCY THEN
        Y.SEL.FX.HIS.CMD:=" AND  WITH (CURRENCY.BOUGHT EQ ":Y.CURRENCY:" OR CURRENCY.SOLD EQ ":Y.CURRENCY:')'
        Y.SEL.FT.HIS.CMD:=" AND  WITH (DEBIT.CURRENCY EQ ":Y.CURRENCY:" OR CREDIT.CURRENCY EQ ":Y.CURRENCY:')'
        Y.SEL.TT.HIS.CMD:=" AND  WITH (CURRENCY.1 EQ ":Y.CURRENCY:" OR CURRENCY.2 EQ ":Y.CURRENCY:')'
    END

    IF Y.USER THEN
        Y.SEL.FX.CMD:=" AND WITH INPUTTER LIKE ...":Y.USER:"..."
        Y.SEL.FT.CMD:=" AND WITH INPUTTER LIKE ...":Y.USER:"..."
        Y.SEL.TT.CMD:=" AND WITH INPUTTER LIKE ...":Y.USER:"..."
    END

    IF Y.DAYS.CNT AND Y.USER THEN
        Y.SEL.FX.HIS.CMD:=" AND WITH INPUTTER LIKE ...":Y.USER:"..."
        Y.SEL.FT.HIS.CMD:=" AND WITH INPUTTER LIKE ...":Y.USER:"..."
        Y.SEL.TT.HIS.CMD:=" AND WITH INPUTTER LIKE ...":Y.USER:"..."
    END

RETURN
*---------------------------------------------------------------------------------------------------------------

GET.PREV.WGT.BUY.RATE1:
************************
    IF NOT(Y.PREV.WGT.BUY.RATE) THEN
        LOOP
        WHILE Y.INIT LE Y.CHK.DAYS
            Y.FREQ = '-1C'
            CALL CDT('',Y.DATE1,Y.FREQ)
            Y.ID = Y.CURRENCY:Y.DATE1
            CALL F.READ(FN.REDO.FX.CCY.POSN,Y.ID,R.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN,Y.CCY.ERR)
            IF R.REDO.FX.CCY.POSN NE '' AND R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG> THEN
                Y.PREV.WGT.BUY.RATE = R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG>
                Y.INIT += Y.CHK.DAYS
            END
            Y.INIT += 1
        REPEAT
    END
RETURN
*---------------------------------------------------------------------------------------------------------------

CURR.POSN.TABL:
***************
    IF Y.INITIAL.DATE EQ Y.FINAL.DATE THEN
        Y.SEL.FX.HIS.CMD:=" AND WITH VALUE.DATE.BUY GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.BUY LE ':Y.FINAL.DATE
        Y.SEL.FT.HIS.CMD:=" AND WITH DEBIT.VALUE.DATE GE ":Y.INITIAL.DATE:' AND WITH DEBIT.VALUE.DATE LE ':Y.FINAL.DATE
        Y.SEL.TT.HIS.CMD:=" AND WITH VALUE.DATE.1 GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.1 LE ':Y.FINAL.DATE
        Y.DAYS.CNT = '1'
        Y.DATE.INITIAL = Y.INITIAL.DATE
        LOOP
        WHILE Y.INIT LE Y.CHK.DAYS
            Y.FREQ = '-1C'
            CALL CDT('',Y.DATE.INITIAL,Y.FREQ)
            Y.ID = Y.CURRENCY:Y.DATE.INITIAL
            CALL F.READ(FN.REDO.FX.CCY.POSN,Y.ID,R.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN,Y.CCY.ERR)
            IF R.REDO.FX.CCY.POSN NE '' AND R.REDO.FX.CCY.POSN<REDO.FX.TOTAL.POSN> THEN
                Y.INIT.FCY.POSN=R.REDO.FX.CCY.POSN<REDO.FX.TOTAL.POSN>
                Y.PREV.WGT.BUY.RATE = R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG>
                IF NOT(Y.PREV.WGT.BUY.RATE) THEN
                    LOOP
                    WHILE Y.INIT LE Y.CHK.DAYS
                        Y.FREQ = '-1C'
                        CALL CDT('',Y.DATE.INITIAL,Y.FREQ)
                        Y.ID = Y.CURRENCY:Y.DATE.INITIAL
                        CALL F.READ(FN.REDO.FX.CCY.POSN,Y.ID,R.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN,Y.CCY.ERR)
                        IF R.REDO.FX.CCY.POSN NE '' AND R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG> THEN
                            Y.PREV.WGT.BUY.RATE = R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG>
                            Y.INIT += Y.CHK.DAYS
                        END
                        Y.INIT += 1
                    REPEAT
                END

                Y.INIT.DOP.POSN = Y.INIT.FCY.POSN * Y.PREV.WGT.BUY.RATE       ;* PACS00157021 - S/E
                EXIT
            END
            Y.INIT += 1
        REPEAT
    END ELSE
        Y.DATE1 = Y.INITIAL.DATE
        Y.DATE2 = Y.FINAL.DATE
        NO.OF.DAYS ='C'
        CALL CDD('',Y.DATE1,Y.DATE2,NO.OF.DAYS)
        Y.DAYS.CNT = NO.OF.DAYS

        IF Y.DAYS.CNT THEN
            Y.SEL.FX.HIS.CMD:=" AND WITH VALUE.DATE.BUY GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.BUY LE ':Y.FINAL.DATE
            Y.SEL.FT.HIS.CMD:=" AND WITH DEBIT.VALUE.DATE GE ":Y.INITIAL.DATE:' AND WITH DEBIT.VALUE.DATE LE ':Y.FINAL.DATE
            Y.SEL.TT.HIS.CMD:=" AND WITH VALUE.DATE.1 GE ":Y.INITIAL.DATE:' AND WITH VALUE.DATE.1 LE ':Y.FINAL.DATE
        END

        LOOP
        WHILE Y.INIT LE Y.CHK.DAYS
            Y.FREQ = '-':'1':'C'
            CALL CDT('',Y.DATE1,Y.FREQ)
            Y.ID = Y.CURRENCY:Y.DATE1
            CALL F.READ(FN.REDO.FX.CCY.POSN,Y.ID,R.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN,Y.CCY.ERR)
            IF R.REDO.FX.CCY.POSN NE '' THEN
                Y.INIT.FCY.POSN=R.REDO.FX.CCY.POSN<REDO.FX.TOTAL.POSN>
                Y.PREV.WGT.BUY.RATE = R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG>
                IF NOT(Y.PREV.WGT.BUY.RATE) THEN
                    LOOP
                    WHILE Y.INIT LE Y.CHK.DAYS
                        Y.FREQ = '-1C'
                        CALL CDT('',Y.DATE1,Y.FREQ)
                        Y.ID = Y.CURRENCY:Y.DATE1
                        CALL F.READ(FN.REDO.FX.CCY.POSN,Y.ID,R.REDO.FX.CCY.POSN,F.REDO.FX.CCY.POSN,Y.CCY.ERR)
                        IF R.REDO.FX.CCY.POSN NE '' AND R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG> THEN
                            Y.PREV.WGT.BUY.RATE = R.REDO.FX.CCY.POSN<REDO.FX.WGT.BUY.AVG>
                            Y.INIT += Y.CHK.DAYS
                        END
                        Y.INIT += 1
                    REPEAT
                END
                Y.INIT.DOP.POSN = Y.INIT.FCY.POSN * Y.PREV.WGT.BUY.RATE       ;* PACS00157021 -S/E
                EXIT
            END
            Y.INIT += 1
        REPEAT
    END
RETURN
*---------------------------------------------------------------------------------------------------------------
GET.TXN.IDS:
************

    CALL EB.READLIST(Y.SEL.FX.CMD,Y.SEL.FX.LIST,'',NO.OF.REC.FX,Y.RET.FX.CODE)
    CALL EB.READLIST(Y.SEL.FT.CMD,Y.SEL.FT.LIST,'',NO.OF.REC.FT,Y.RET.FT.CODE)
    CALL EB.READLIST(Y.SEL.TT.CMD,Y.SEL.TT.LIST,'',NO.OF.REC.TT,Y.RET.TT.CODE)

    Y.TXN.IDS = Y.SEL.FX.LIST:@FM:Y.SEL.FT.LIST:@FM:Y.SEL.TT.LIST

    IF Y.DAYS.CNT THEN
        CALL EB.READLIST(Y.SEL.FX.HIS.CMD,Y.SEL.LIST.FX.HIS,'',NO.OF.REC.HIS.FX,Y.RET.FX.CODE)
        CALL EB.READLIST(Y.SEL.FT.HIS.CMD,Y.SEL.LIST.FT.HIS,'',NO.OF.REC.HIS.FT,Y.RET.FT.CODE)
        CALL EB.READLIST(Y.SEL.TT.HIS.CMD,Y.SEL.LIST.TT.HIS,'',NO.OF.REC.HIS.TT,Y.RET.TT.CODE)

        Y.TXN.IDS<-1> =Y.SEL.LIST.FX.HIS:@FM:Y.SEL.LIST.FT.HIS:@FM:Y.SEL.LIST.TT.HIS
    END

RETURN
*---------------------------------------------------------------------------------------------------------------
TXN.ID.PROCESS:
***************
    Y.FIRST.FLAG = 1
    GOSUB GET.LOC.FLD.POS
    LOOP
        REMOVE Y.TXN FROM Y.TXN.IDS SETTING Y.TXN.POS
    WHILE Y.TXN:Y.TXN.POS
        IF Y.TXN NE '' THEN
            Y.TXN.PREFIX = Y.TXN[1,2]
            BEGIN CASE
                CASE Y.TXN.PREFIX EQ 'FX'
                    GOSUB FX.PROCESS
                CASE Y.TXN.PREFIX EQ 'FT'
                    GOSUB FT.PROCESS
                CASE Y.TXN.PREFIX EQ 'TT'
                    GOSUB TT.PROCESS
                CASE 1
                    GOSUB ASSIGN.VAR
            END CASE
        END
    REPEAT

    GOSUB COMMON.DETAILS

RETURN
*--------------------------------------------------------------------------------------------------------------
COMMON.DETAILS:
*--------------
    IF Y.ARR NE '' THEN
        Y.I = 1
        Y.CNT = DCOUNT(Y.ARR,@FM)
        LOOP
            REMOVE Y.ARR.VALUE FROM Y.ARR SETTING Y.ARR.POS
        WHILE Y.ARR.VALUE:Y.ARR.POS

            IF FIELD(Y.ARR.VALUE,'*',4,1) EQ 'COMPRA' THEN
                Y.TOT.BUY.FCY = Y.TOT.BUY.FCY + FIELD(Y.ARR.VALUE,'*', 5, 1)
                Y.TOT.BUY.DOP = Y.TOT.BUY.DOP + FIELD(Y.ARR.VALUE,'*', 7, 1)
            END
            IF FIELD(Y.ARR.VALUE,'*',4,1) EQ 'VENTA' THEN
                Y.TOT.SELL.FCY = Y.TOT.SELL.FCY + FIELD(Y.ARR.VALUE,'*', 5, 1)
                Y.TOT.SELL.DOP = Y.TOT.SELL.DOP + FIELD(Y.ARR.VALUE,'*', 7, 1)
            END
        REPEAT

        Y.STR.APP = Y.TOT.BUY.FCY:'*':Y.TOT.BUY.DOP:'*':Y.TOT.SELL.FCY:'*':Y.TOT.SELL.DOP:'*':Y.INIT.FCY.POSN:'*':Y.INIT.DOP.POSN
        Y.LAST.ARR = Y.ARR<Y.CNT>
        DEL Y.ARR<Y.CNT>
        Y.TOT.ARR = Y.LAST.ARR:'*':Y.STR.APP

        Y.ARR<-1> = Y.TOT.ARR
    END
RETURN
*--------------------------------------------------------------------------------------------------------------
GET.LOC.FLD.POS:
****************
    APPL.ARRAY = 'FOREX':@FM:'FUNDS.TRANSFER':@FM:'TELLER'
    FLD.ARRAY  = 'L.FX.FXSN.NUM':@FM:'L.FT.FXSN.NUM':@VM:'L.FT.LEGAL.ID':@FM:'L.TT.FXSN.NUM':@VM:'L.TT.LEGAL.ID'
    FLD.POS    = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)
    LOC.L.FX.FXSN.NUM = FLD.POS<1,1>
    LOC.L.FT.FXSN.NUM = FLD.POS<2,1>
    LOC.L.FT.LEGAL.ID = FLD.POS<2,2>
    LOC.L.TT.FXSN.NUM = FLD.POS<3,1>
    LOC.L.TT.LEGAL.ID = FLD.POS<3,2>
RETURN
*--------------------------------------------------------------------------------------------------------------
FX.PROCESS:
***********
    CALL F.READ(FN.FOREX,Y.TXN,R.FOREX,F.FOREX,Y.FOREX.ERR)
    IF R.FOREX NE '' THEN
        Y.TXN.DATE = R.FOREX<FX.VALUE.DATE.BUY>
        Y.CT.PARTY = R.FOREX<FX.COUNTERPARTY>
        Y.L.FX.NUM  = R.FOREX<FX.LOCAL.REF><1,LOC.L.FX.FXSN.NUM>
    END ELSE
        CALL F.READ(FN.FOREX.HIS,Y.TXN,R.FOREX,F.FOREX.HIS,Y.FOREXHIS.ERR)
        Y.TXN.DATE = R.FOREX<FX.VALUE.DATE.BUY>
        Y.CT.PARTY = R.FOREX<FX.COUNTERPARTY>
        Y.L.FX.NUM = R.FOREX<FX.LOCAL.REF><1,LOC.L.FX.FXSN.NUM>
    END

*PACS00157021 - S
    IF Y.CT.PARTY THEN
        R.CUS = ''
        CALL F.READ(FN.CUSTOMER,Y.CT.PARTY,R.CUS,F.CUSTOMER,CUS.ERR)
        Y.CT.PARTY = R.CUS<EB.CUS.NAME.1>
    END
*PACS00157021 - E

    Y.FX.LOC = Y.L.FX.NUM[1,1]
    Y.FX.VAL = '1'
    IF Y.FX.LOC EQ Y.FX.VAL THEN
*Y.TXN.TYPE = 'BUY'
        Y.TXN.TYPE = 'COMPRA'
    END ELSE
*Y.TXN.TYPE = 'SELL'
        Y.TXN.TYPE = 'VENTA'
    END

    IF Y.TXN.TYPE EQ 'COMPRA' THEN
        Y.FCY.AMT = R.FOREX<FX.AMOUNT.BOUGHT>
    END ELSE
        Y.FCY.AMT = R.FOREX<FX.AMOUNT.SOLD>
    END

    IF R.FOREX<FX.DEAL.TYPE> EQ 'SP' THEN
        Y.SPOT.RATE = R.FOREX<FX.SPOT.RATE>
    END ELSE
        Y.SPOT.RATE = R.FOREX<FX.FORWARD.RATE>
    END

    Y.DOP.AMT = Y.FCY.AMT * Y.SPOT.RATE

    IF Y.TXN.TYPE EQ 'COMPRA' AND Y.FCY.AMT GT 0 THEN
        IF Y.FIRST.FLAG EQ 1 THEN
            Y.FCY.POSN = Y.INIT.FCY.POSN + Y.FCY.AMT
            Y.FIRST.FLAG += 1
        END ELSE
            Y.FCY.POSN = Y.FCY.AMT + Y.FCY.POSN
        END
    END

    IF Y.TXN.TYPE EQ 'VENTA' AND Y.FCY.AMT GT 0 THEN
        IF Y.FIRST.FLAG EQ 1 THEN
            Y.FCY.POSN = Y.INIT.FCY.POSN - Y.FCY.AMT
            Y.FIRST.FLAG += 1
        END ELSE
            Y.FCY.POSN -= Y.FCY.AMT
        END
    END

    Y.USER = FIELD(R.FOREX<FX.INPUTTER>,'_',2,1)

    Y.DEALER.DESK = R.FOREX<FX.DEALER.DESK>

    GOSUB ASSIGN.VAR

RETURN
*-----------------------------------------------------------------------------------------------------------------------------------------------------------------
ASSIGN.VAR:
***********

    Y.TXN = FIELD(Y.TXN,";",1)

    IF Y.ARR EQ '' THEN
        Y.ARR = Y.TXN:'*':Y.TXN.DATE:'*': Y.CT.PARTY:'*':Y.TXN.TYPE:'*':Y.FCY.AMT:'*':Y.SPOT.RATE:'*':Y.DOP.AMT:'*':Y.FCY.POSN:'*':Y.USER:'*':Y.DEALER.DESK

    END ELSE
        Y.ARR<-1> = Y.TXN:'*':Y.TXN.DATE:'*':Y.CT.PARTY:'*':Y.TXN.TYPE:'*':Y.FCY.AMT:'*':Y.SPOT.RATE:'*':Y.DOP.AMT:'*':Y.FCY.POSN:'*':Y.USER:'*':Y.DEALER.DESK
    END

RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------------------
FT.PROCESS:
***********

    CALL F.READ(FN.FUNDS.TRANSFER,Y.TXN,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,Y.FT.ERR)
    IF R.FUNDS.TRANSFER NE '' THEN
        Y.TXN.DATE = R.FUNDS.TRANSFER<FT.DEBIT.VALUE.DATE>
        Y.L.FT.NUM  = R.FUNDS.TRANSFER<FT.LOCAL.REF><1,LOC.L.FT.FXSN.NUM>
    END ELSE
        CALL F.READ(FN.FUNDS.TRANSFER.HIS,Y.TXN,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER.HIS,Y.FT.ERR)
        Y.TXN.DATE = R.FUNDS.TRANSFER<FT.DEBIT.VALUE.DATE>
        Y.L.FT.NUM  = R.FUNDS.TRANSFER<FT.LOCAL.REF><1,LOC.L.FT.FXSN.NUM>
    END

    Y.FT.LOC = Y.L.FT.NUM[1,1]
    Y.FT.VAL = '1'

    IF Y.FT.LOC EQ Y.FT.VAL THEN
        Y.TXN.TYPE = 'COMPRA'
    END ELSE
        Y.TXN.TYPE = 'VENTA'
    END

    IF Y.TXN.TYPE EQ 'COMPRA' THEN
        Y.FCY.AMT = R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT>
    END ELSE
        Y.FCY.AMT = R.FUNDS.TRANSFER<FT.CREDIT.AMOUNT>
    END

    Y.SPOT.RATE = R.FUNDS.TRANSFER<FT.TREASURY.RATE>
    Y.DOP.AMT = Y.FCY.AMT * Y.SPOT.RATE
    Y.CT.PARTY = R.FUNDS.TRANSFER<FT.LOCAL.REF,LOC.L.FT.LEGAL.ID>       ;*PACS00157021 - S/E
    Y.CT.PARTY = FIELD(Y.CT.PARTY,'.',3)

    IF Y.TXN.TYPE EQ 'COMPRA' AND Y.FCY.AMT GT 0 THEN
        IF Y.FIRST.FLAG EQ 1 THEN
            Y.FCY.POSN = Y.INIT.FCY.POSN + Y.FCY.AMT
            Y.FIRST.FLAG += 1
        END ELSE
            Y.FCY.POSN = Y.FCY.AMT + Y.FCY.POSN
        END
    END

    IF Y.TXN.TYPE EQ 'VENTA'  AND Y.FCY.AMT GT 0 THEN
        IF Y.FIRST.FLAG EQ 1 THEN
            Y.FCY.POSN = Y.INIT.FCY.POSN - Y.FCY.AMT
            Y.FIRST.FLAG += 1
        END ELSE
            Y.FCY.POSN -= Y.FCY.AMT
        END
    END

    Y.USER = FIELD(R.FUNDS.TRANSFER<FT.INPUTTER>,'_',2,1)
    Y.DEALER.DESK = R.FUNDS.TRANSFER<FT.DEALER.DESK>
    GOSUB ASSIGN.VAR
RETURN
*---------------------------------------------------------------------------------------------------------------------------------------------------------------
TT.PROCESS:
***********
    CALL F.READ(FN.TELLER,Y.TXN,R.TELLER,F.TELLER,Y.TEL.ERR)

    IF R.TELLER NE '' THEN
        IF R.TELLER<TT.TE.DR.CR.MARKER> EQ 'DEBIT' OR R.TELLER<TT.TE.DR.CR.MARKER> EQ 'CREDIT' THEN
            Y.TXN.DATE = R.TELLER<TT.TE.VALUE.DATE.1>
        END ELSE
            Y.TXN.DATE = R.TELLER<TT.TE.VALUE.DATE.2>
        END
        Y.L.TT.NUM  = R.TELLER<TT.TE.LOCAL.REF><1,LOC.L.TT.FXSN.NUM>
    END

    IF R.TELLER EQ '' THEN
        CALL F.READ(FN.TELLER.HIS,Y.TXN,R.TELLER,F.TELLER.HIS,Y.TEL.ERR)
        IF R.TELLER<TT.TE.DR.CR.MARKER> EQ 'DEBIT' OR R.TELLER<TT.TE.DR.CR.MARKER> EQ 'CREDIT' THEN
            Y.TXN.DATE = R.TELLER<TT.TE.VALUE.DATE.1>
        END ELSE
            Y.TXN.DATE = R.TELLER<TT.TE.VALUE.DATE.2>
        END
        Y.L.TT.NUM  = R.TELLER<TT.TE.LOCAL.REF><1,LOC.L.TT.FXSN.NUM>
    END

* Y.L.TT.NUM  = R.TELLER<TT.TE.LOCAL.REF><1,LOC.L.TT.FXSN.NUM>

    IF Y.L.TT.NUM[1,1] EQ '1' THEN
*Y.TXN.TYPE = 'BUY'
        Y.TXN.TYPE = 'COMPRA'
    END ELSE
*Y.TXN.TYPE = 'SELL'
        Y.TXN.TYPE = 'VENTA'
    END

    IF Y.TXN.TYPE EQ 'COMPRA' AND R.TELLER<TT.TE.CURRENCY.1> EQ 'DOP' THEN
        Y.FCY.AMT = R.TELLER<TT.TE.AMOUNT.FCY.2>
        Y.BACKUP.FCY.AMT = R.TELLER<TT.TE.AMOUNT.FCY.1>
    END ELSE
* Y.FCY.AMT = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
        Y.FCY.AMT = R.TELLER<TT.TE.AMOUNT.FCY.1>
        Y.BACKUP.FCY.AMT = R.TELLER<TT.TE.AMOUNT.FCY.2>
    END

    IF Y.FCY.AMT EQ '' THEN
        Y.FCY.AMT = Y.BACKUP.FCY.AMT
    END

    Y.SPOT.RATE = R.TELLER<TT.TE.DEAL.RATE>
    Y.DOP.AMT = Y.FCY.AMT * Y.SPOT.RATE
    Y.CT.PARTY = R.TELLER<TT.TE.LOCAL.REF><1,LOC.L.TT.LEGAL.ID>         ;* PACS00157021 - S/E
    Y.CT.PARTY = FIELD(Y.CT.PARTY,'.',3)

    IF Y.TXN.TYPE EQ 'COMPRA'  AND Y.FCY.AMT GT 0 THEN
        IF Y.FIRST.FLAG EQ 1 THEN
            Y.FCY.POSN = Y.INIT.FCY.POSN + Y.FCY.AMT
            Y.FIRST.FLAG += 1
        END ELSE
            Y.FCY.POSN = Y.FCY.AMT + Y.FCY.POSN
        END
    END

    IF Y.TXN.TYPE EQ 'VENTA'  AND Y.FCY.AMT GT 0 THEN
        IF Y.FIRST.FLAG EQ 1 THEN
            Y.FCY.POSN = Y.INIT.FCY.POSN - Y.FCY.AMT
            Y.FIRST.FLAG += 1
        END ELSE
            Y.FCY.POSN -= Y.FCY.AMT
        END
    END

    Y.USER = FIELD(R.TELLER<TT.TE.INPUTTER>,'_',2,1)
    Y.DEALER.DESK = R.TELLER<TT.TE.DEALER.DESK>
    GOSUB ASSIGN.VAR
RETURN

PGM.END:
*********

END
