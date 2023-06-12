* @ValidationCode : MjoxNTg2MjMzMzU6Q3AxMjUyOjE2ODA2OTA0NjA3MTQ6SVRTUzotMTotMTo1MTA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:57:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 510
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.SC.REV.DISC.ENTRY(Y.ID)
*-----------------------------------------------------------------------------
* DESCRIPTION : This BATCH routine will look for Spec entries that raised on the bussiness day from RE.SPEC.ENT.TODAY to reverse and re-calculate interest accrual based on
*               effective interest rate method and raise accounting entries
*-----------------------------------------------------------------------------
* * Input / Output
* -----------------------------------------------------------------------------
* IN Parameter    : NA
* OUT Parameter   : NA
*-----------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : PRADEEP S
* PROGRAM NAME : REDO.B.SC.REV.DISC.ENTRY
*-----------------------------------------------------------------------------
* Modification History :
* Date             Author             Reference           Description
* 06 Jul 2011      Pradeep S          PACS00080124        Initial Creation
* 04-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.DATES
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.SECURITY.MASTER
    $INSERT I_F.RE.CONSOL.SPEC.ENTRY
    $INSERT I_F.SC.TRADING.POSITION
    $INSERT I_F.SC.TRADE.POS.HISTORY
    $INSERT I_F.SEC.TRADE
    $INSERT I_F.REDO.APAP.L.CONTRACT.BALANCES
    $INSERT I_REDO.B.SC.REV.DISC.ENTRY.COMMON
    $INSERT I_F.SEC.ACC.MASTER
    $INSERT I_F.REDO.APAP.L.SC.DISC.AMORT


    GOSUB PROCESS

RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-------

    Y.STP.ID = Y.ID
    Y.STP.HIS.ID = Y.STP.ID:".":TODAY[1,6]
    R.STP.HIS = ''
    CALL F.READ(FN.SC.TRADE.POS.HISTORY,Y.STP.HIS.ID,R.STP.HIS,F.SC.TRADE.POS.HISTORY,ERR.STP)



    Y.PROCESS.GOHEAD = @TRUE
    Y.COMMON.ARRAY = ''
    GOSUB CALC.NEW.ACCRUAL
    IF Y.PROCESS.GOHEAD THEN
        GOSUB STMT.ENTRY
        Y.COMMON.ARRAY = LOWER(R.SPEC.ENT)
        GOSUB CATEG.ENTRY
        Y.COMMON.ARRAY<-1> = LOWER(R.SPEC.ENT)
        GOSUB CALL.EB.ACCOUNTING
    END

RETURN

CALC.NEW.ACCRUAL:
******************

    Y.SUB.ASSEST.TYPE = ''
    Y.SEC.TRADE.ID = Y.ID
    IF INDEX(Y.SEC.TRADE.ID,".",1) ELSE
        Y.PROCESS.GOHEAD = @FALSE
        RETURN
    END

    Y.SAM.ID = FIELD(Y.SEC.TRADE.ID,".",1)
    Y.SEC.CODE = FIELD(Y.SEC.TRADE.ID,".",2)

    R.SECURITY.MASTER = ''
    CALL F.READ(FN.SECURITY.MASTER,Y.SEC.CODE,R.SECURITY.MASTER,F.SECURITY.MASTER,E.SECURITY.MASTER)

    IF R.SECURITY.MASTER<SC.SCM.BOND.OR.SHARE> EQ 'S'  THEN
        Y.PROCESS.GOHEAD = @FALSE
        RETURN
    END

    IF R.SECURITY.MASTER THEN
        Y.SM.PAR.VALUE = R.SECURITY.MASTER<SC.SCM.PAR.VALUE>
        Y.SEC.CCY = R.SECURITY.MASTER<SC.SCM.SECURITY.CURRENCY>
*VALUE.DATE = R.SECURITY.MASTER<SC.SCM.ISSUE.DATE>
        MAT.DATE = R.SECURITY.MASTER<SC.SCM.MATURITY.DATE>
        GOSUB CALCULATIONS
    END

RETURN

CALCULATIONS:
**************

    R.SAM = ''
    SAM.ERR = ''
    CALL F.READ(FN.SEC.ACC.MASTER,Y.SAM.ID,R.SAM,F.SEC.ACC.MASTER,SAM.ERR)
    CUSTOMER.NO = R.SAM<SC.SAM.CUSTOMER.NUMBER>

    DISC.ADJ.CAT = R.SAM<SC.SAM.LOCAL.REF,POS.L.DISC.ADJ.CAT>
    DISC.ADJ.ACC = Y.SEC.CCY:DISC.ADJ.CAT:'0001'
    DISC.PL.CAT = R.SAM<SC.SAM.DISCOUNT.CAT>

    DISC.AMORT.ID = Y.SEC.TRADE.ID
    R.DISC.AMORT = ''
    DISC.AMORT.ERR = ''
    CALL F.READ(FN.REDO.APAP.L.SC.DISC.AMORT,DISC.AMORT.ID,R.DISC.AMORT,F.REDO.APAP.L.SC.DISC.AMORT,DISC.AMORT.ERR)
    Y.DTM = R.DISC.AMORT<DISC.AMRT.DAYS.TO.MAT>
    Y.PD.AMT    = R.DISC.AMORT<DISC.AMRT.LIN.ACCR.AMT>
    GOSUB CALC.VALUE.DATE

    R.SC.TRADING.POSITION = ''
    CALL F.READ(FN.SC.TRADING.POSITION,Y.SEC.TRADE.ID,R.SC.TRADING.POSITION,F.SC.TRADING.POSITION,E.SC.TRADING.POSITION)
    IF R.SC.TRADING.POSITION THEN
        FACE.VALUE    = R.SC.TRADING.POSITION<SC.TRP.CURRENT.POSITION>
        AVG.PRICE = R.SC.TRADING.POSITION<SC.TRP.CUR.AVG.PRICE>
        SC.PD = Y.SM.PAR.VALUE - AVG.PRICE
        Y.ACCR.POSTED = R.SC.TRADING.POSITION<SC.TRP.CPN.ACCR.POSTED>
*VALUE.DATE = R.SC.TRADING.POSITION<SC.TRP.DATE.LAST.TRADED>
        SC.NOMINAL = R.SC.TRADING.POSITION<SC.TRP.CURRENT.POSITION>
        TRANS.TYPE = R.SC.TRADING.POSITION<SC.TRP.TRD.TRANS.TYPE,1>

        INTEREST.RATE      = R.SECURITY.MASTER<SC.SCM.INTEREST.RATE>
        BASE.1             = R.SECURITY.MASTER<SC.SCM.INTEREST.DAY.BASIS>
        BASE               = FIELD(BASE.1,'/',2)
        ACCRUAL.START.DATE = R.SECURITY.MASTER<SC.SCM.ACCRUAL.START.DATE>
        INT.PAYMENT.DATE   = R.SECURITY.MASTER<SC.SCM.INT.PAYMENT.DATE>
        REGION.CODE        = ""

        Y.STP.DAY = R.STP.HIS<SC.TPH.DAY>
        CHANGE @VM TO @FM IN Y.STP.DAY
        LOCATE TODAY[7,2] IN Y.STP.DAY SETTING DAY.POS THEN
            Y.SPEC.AMT = R.STP.HIS<SC.TPH.V.D.DISC.ACCR,DAY.POS>
        END

        COUPON.DAYS = "C"
        DAYS = "C"

        GOSUB PRE.CALC

        IF DAYS GT COUPON.DAYS THEN
            RETURN
        END

        CALC1          = Y.PD.AMT / FACE.VALUE
        CALC2          = (1 + CALC1)

        CALC.EXP1      = (DAYS/Y.DTM)

        CALC3          = PWR(CALC2,CALC.EXP1)
        EFF.RATE       = (CALC3 - 1)
        EFFECTIVE.RATE = EFF.RATE

        DISC.ACCR.VAL  = (FACE.VALUE*EFFECTIVE.RATE)
*DISC.ACCR.VAL = DROUND(DISC.ACCR.VAL,2)
        GOSUB UPD.APAP.DISCOUNT.AMORT
        GOSUB LCCY.FCCY.CONV

    END

RETURN

**********
PRE.CALC:
**********

    IF VALUE.DATE AND MAT.DATE THEN
        CALL CDD(REGION.CODE,VALUE.DATE,MAT.DATE,COUPON.DAYS)

        Y.NXT.CAL.DAY = TODAY
        CALL CDT('', Y.NXT.CAL.DAY, '+1C')

        YDAY.TYPE = ''
        CALL AWD('',Y.NXT.CAL.DAY,YDAY.TYPE)
        IF YDAY.TYPE EQ 'W' THEN
            CALL CDD(REGION.CODE,VALUE.DATE,TODAY,DAYS)
        END ELSE
            Y.CALC.DAY = R.DATES(EB.DAT.NEXT.WORKING.DAY)
            GOSUB CALC.LAST.DAY.OF.MONTH
            DAYS = 'C'
            CALL CDD(REGION.CODE,VALUE.DATE,Y.CALC.DAY,DAYS)
        END
        DAYS += 1
    END
RETURN

************************
CALC.LAST.DAY.OF.MONTH:
************************

    Y.V.DATE.POSN = ''
    Y.NXT.MONTH = Y.CALC.DAY[5,2]
    Y.CUR.MONTH = TODAY[5,2]
    IF Y.CUR.MONTH NE Y.NXT.MONTH THEN
        COMI = TODAY
        CALL LAST.DAY.OF.THIS.MONTH
        Y.CALC.DAY = COMI
        Y.DAYS.DATE = R.DATES(EB.DAT.NEXT.WORKING.DAY)
        CALL CDT('',Y.DAYS.DATE,'-1C')
        GOSUB CALC.NEXT.DAY.ACCR
        Y.V.DATE.POSN = DISC.ACCR.VAL
    END ELSE
        CALL CDT('', Y.CALC.DAY, '-1C')
    END

RETURN

********************
CALC.NEXT.DAY.ACCR:
********************

    DAYS = 'C'
    CALL CDD(REGION.CODE,VALUE.DATE,Y.DAYS.DATE,DAYS)
    DAYS += 1

    CALC1          = SC.PD / FACE.VALUE
    CALC2          = (1 + CALC1)

    CALC.EXP1      = (DAYS/Y.DTM)

    CALC3          = PWR(CALC2,CALC.EXP1)

    EFF.RATE       = (CALC3 - 1)
    EFFECTIVE.RATE = EFF.RATE

    DISC.ACCR.VAL  = (FACE.VALUE*EFFECTIVE.RATE)
    DISC.ACCR.VAL  = DROUND(DISC.ACCR.VAL,2)

RETURN


********************
CALC.AS.OF.STERDAY:
********************

    Y.DAY.BEFORE = TODAY
    CALL CDT('', Y.DAY.BEFORE, '-1C')

    COUPON.DAYS1 = "C"
    DAYS1 = "C"

    CALL CDD(REGION.CODE,VALUE.DATE,MAT.DATE,COUPON.DAYS1)

    CALL CDD(REGION.CODE,VALUE.DATE,Y.DAY.BEFORE,DAYS1)
    DAYS1 += 1

    CALC1          = Y.PD.AMT / FACE.VALUE
    CALC2          = (1 + CALC1)

    CALC.EXP1      = (DAYS/Y.DTM)

    CALC3          = PWR(CALC2,CALC.EXP1)
    EFF.RATE       = (CALC3 - 1)
    EFFECTIVE.RATE1 = EFF.RATE

    DISC.ACCR.VAL.STR  = (FACE.VALUE*EFFECTIVE.RATE1)
*DISC.ACCR.VAL.STR  = DROUND(DISC.ACCR.VAL.STR,2)

RETURN


STMT.ENTRY:
***************

    DISC.ACCR.VAL.LCY = DROUND(DISC.ACCR.VAL.LCY,2)
    DISC.ACCR.VAL.FCY = DROUND(DISC.ACCR.VAL.FCY,2)
    R.SPEC.ENT = ''
    R.SPEC.ENT<AC.STE.ACCOUNT.NUMBER>   = DISC.ADJ.ACC
    R.SPEC.ENT<AC.STE.COMPANY.CODE>     = ID.COMPANY
    R.SPEC.ENT<AC.STE.AMOUNT.LCY>       = DISC.ACCR.VAL.LCY
    IF R.SPEC.ENT<AC.STE.AMOUNT.LCY> GT 0 THEN
        R.SPEC.ENT<AC.STE.TRANSACTION.CODE> = Y.SC.CR.TXN.CODE
    END ELSE
        R.SPEC.ENT<AC.STE.TRANSACTION.CODE> = Y.SC.DR.TXN.CODE
    END
    R.SPEC.ENT<AC.STE.PL.CATEGORY>      = ''
    R.SPEC.ENT<AC.STE.NARRATIVE>        = "DAILY.ACCR-DISC"
    R.SPEC.ENT<AC.STE.CUSTOMER.ID>      = CUSTOMER.NO
    R.SPEC.ENT<AC.STE.PRODUCT.CATEGORY> = Y.DEFAULT.CATEG
    R.SPEC.ENT<AC.STE.VALUE.DATE>       = TODAY
    R.SPEC.ENT<AC.STE.CURRENCY>         = Y.SEC.CCY
    IF DISC.ACCR.VAL.FCY THEN
        R.SPEC.ENT<AC.STE.AMOUNT.FCY>       = DISC.ACCR.VAL.FCY
        R.SPEC.ENT<AC.STE.EXCHANGE.RATE >   = EX.RATE
    END
    R.SPEC.ENT<AC.STE.POSITION.TYPE>    = 'TR'
    R.SPEC.ENT<AC.STE.OUR.REFERENCE>    = Y.ID
    R.SPEC.ENT<AC.STE.CURRENCY.MARKET>  = Y.CCY.MARKET
    R.SPEC.ENT<AC.STE.TRANS.REFERENCE>  = Y.ID
    R.SPEC.ENT<AC.STE.SYSTEM.ID>        = 'SC'
    R.SPEC.ENT<AC.STE.BOOKING.DATE>     = TODAY

RETURN

CATEG.ENTRY:
***********

    DISC.ACCR.VAL.LCY = DROUND(DISC.ACCR.VAL.LCY,2)
    DISC.ACCR.VAL.FCY = DROUND(DISC.ACCR.VAL.FCY,2)
    R.SPEC.ENT = ''
    R.SPEC.ENT<AC.CAT.COMPANY.CODE>     = ID.COMPANY
    R.SPEC.ENT<AC.CAT.AMOUNT.LCY>       = DISC.ACCR.VAL.LCY * -1
    IF R.SPEC.ENT<AC.CAT.AMOUNT.LCY>  GT 0 THEN
        R.SPEC.ENT<AC.CAT.TRANSACTION.CODE> = Y.SC.CR.TXN.CODE
    END ELSE
        R.SPEC.ENT<AC.CAT.TRANSACTION.CODE> = Y.SC.DR.TXN.CODE
    END
    R.SPEC.ENT<AC.CAT.NARRATIVE,1>      = "DAILY.ACCR-DISC"
    R.SPEC.ENT<AC.CAT.PL.CATEGORY>      = DISC.PL.CAT
    R.SPEC.ENT<AC.CAT.CUSTOMER.ID>      = CUSTOMER.NO
    R.SPEC.ENT<AC.CAT.PRODUCT.CATEGORY> = Y.DEFAULT.CATEG
    R.SPEC.ENT<AC.CAT.VALUE.DATE>       = TODAY
    R.SPEC.ENT<AC.CAT.CURRENCY>         = Y.SEC.CCY
    IF DISC.ACCR.VAL.FCY THEN
        R.SPEC.ENT<AC.CAT.AMOUNT.FCY>       = DISC.ACCR.VAL.FCY
        R.SPEC.ENT<AC.CAT.EXCHANGE.RATE>    = EX.RATE
    END
    R.SPEC.ENT<AC.CAT.OUR.REFERENCE>    = Y.ID
    R.SPEC.ENT<AC.CAT.CURRENCY.MARKET>  = Y.CCY.MARKET
    R.SPEC.ENT<AC.CAT.TRANS.REFERENCE>  = Y.ID
    R.SPEC.ENT<AC.CAT.SYSTEM.ID>        = "SC"
    R.SPEC.ENT<AC.CAT.BOOKING.DATE>     = TODAY

RETURN

CALL.EB.ACCOUNTING:
********************

    V = SC.TRP.LAST.COB.TXNS
    ID.NEW = Y.ID
    CALL EB.ACCOUNTING("SC","SAO",Y.COMMON.ARRAY,'')

RETURN

****************************
UPD.APAP.DISCOUNT.AMORT:
*****************************

    IF R.DISC.AMORT THEN
        DISC.ADJ.TOT = R.DISC.AMORT<DISC.AMRT.EFF.ADJ.DISC.TOT>
        R.DISC.AMORT<DISC.AMRT.EFF.DISC.TODATE> = DISC.ACCR.VAL
        R.DISC.AMORT<DISC.AMRT.DISC.ACC.DATE,-1> = TODAY
        R.DISC.AMORT<DISC.AMRT.EFF.DISC.RATE,-1> = DROUND(EFFECTIVE.RATE*100,4)
        DISC.ACCR.VAL = Y.SPEC.AMT - DISC.ACCR.VAL - DISC.ADJ.TOT
        R.DISC.AMORT<DISC.AMRT.EFF.DISC.AMT,-1> = DROUND(DISC.ACCR.VAL,2)
        R.DISC.AMORT<DISC.AMRT.EFF.ADJ.DISC.TOT> += DISC.ACCR.VAL

        CALL F.WRITE(FN.REDO.APAP.L.SC.DISC.AMORT,DISC.AMORT.ID,R.DISC.AMORT)
    END

    IF DISC.ACCR.VAL EQ 0 THEN
        Y.PROCESS.GOHEAD = @FALSE
    END
RETURN

*****************
LCCY.FCCY.CONV:
*****************

    DISC.ACCR.VAL.FCY = ''
    DISC.ACCR.VAL.LCY = ''

    IF Y.SEC.CCY EQ LCCY THEN
        DISC.ACCR.VAL.LCY = DISC.ACCR.VAL
        DISC.ACCR.VAL.FCY = ''
    END

    IF Y.SEC.CCY NE LCCY THEN
        DISC.ACCR.VAL.FCY = DISC.ACCR.VAL
        CCY.MARKET = Y.CCY.MARKET
        FCY.TXN.CCY = Y.SEC.CCY
        FCY.TXN.AMOUNT = DISC.ACCR.VAL
        AML.CCY = LCCY
        SELL.AMT = ""
        DIFF.AMT = ''
        LCCY.AMT = ''
        RET.ERR = ''
        EX.RATE = ''
        CALL EXCHRATE(CCY.MARKET,FCY.TXN.CCY,FCY.TXN.AMOUNT,AML.CCY,SELL.AMT,'',EX.RATE,DIFF.AMT,LCCY.AMT,RET.ERR)
        DISC.ACCR.VAL.LCY = LCCY.AMT
    END

RETURN

*****************
CALC.VALUE.DATE:
*****************

    Y.CALC.DAYS = '-':Y.DTM:'C'
    VALUE.DATE = MAT.DATE
    CALL CDT('',VALUE.DATE,Y.CALC.DAYS)

RETURN

END