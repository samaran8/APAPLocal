* @ValidationCode : MjotMTcxMDkyMTYzNzpDcDEyNTI6MTY4MTM3ODI0MDUxMTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:00:40
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
SUBROUTINE REDO.E.NOF.FXSN(RETURN.ARR)
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This is a no file enquiry routine for the enquiry REDO.ENQ.FXSN.DETAILS
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* In Parameter    : NA
* Out Parameter   : RETURN.ARR
*-----------------------------------------------------------------------------------------------------
* Company Name : APAP
* Developed By : Chandra Prakash T
* Program Name : REDO.E.NOF.FXSN
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Who                   Reference           Description
* 11-Jun-2010      Chandra Prakash T     ODR-2010-01-0213    Initial Creation
* 31-Mar-2011      Shankar Raju          PACS00033472        GET THE DESCRIPTION OF THE LOCAL FIELD
*                                                            L.TT.PAY.METHOD & L.TT.RCEP.MTHD FROM EB.LOOKUP TABLE
* 25-Apr-2011      Pradeep S             PACS00054288        Transactions moved to history are not displayed

* 13-APR-2023     Conversion tool   R22 Auto conversion     FM TO @FM, VM to @VM, SM to @SM, = to EQ
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*----------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FOREX
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.FOREX.SEQ.NUM

    GOSUB OPEN.FILES
    GOSUB GET.LOCAL.REF.POSITIONS
    GOSUB FETCH.INFORMATION
    GOSUB SELECT.REQUEST
    GOSUB PROCESS

RETURN

*-----------------------------------------------------------------------------------------------------
OPEN.FILES:
*-----------------------------------------------------------------------------------------------------

    FN.REDO.FOREX.SEQ.NUM = 'F.REDO.FOREX.SEQ.NUM'
    F.REDO.FOREX.SEQ.NUM = ''
    CALL OPF(FN.REDO.FOREX.SEQ.NUM,F.REDO.FOREX.SEQ.NUM)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FOREX = 'F.FOREX'
    F.FOREX = ''
    CALL OPF(FN.FOREX,F.FOREX)

*PACS00054288 - S
    FN.TELLER.HIS = 'F.TELLER$HIS'
    F.TELLER.HIS = ''
    CALL OPF(FN.TELLER.HIS,F.TELLER.HIS)

    FN.FUNDS.TRANSFER.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER.HIS = ''
    CALL OPF(FN.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS)

    FN.FOREX.HIS = 'F.FOREX$HIS'
    F.FOREX.HIS = ''
    CALL OPF(FN.FOREX.HIS,F.FOREX.HIS)
*PACS00054288 - E

    RETURN.ARR = ''

RETURN

*-----------------------------------------------------------------------------------------------------
GET.LOCAL.REF.POSITIONS:
*-----------------------------------------------------------------------------------------------------

    APPL.ARR = "FUNDS.TRANSFER":@FM:"TELLER":@FM:"FOREX"

    FT.LOC.FIELDS = "L.FT.FX.BUY.SRC":@VM:"L.FT.FX.SEL.DST":@VM:"L.FT.FXSN.NUM":@VM:"L.FT.LEGAL.ID":@VM:"L.FT.PAY.METHOD":@VM:"L.FT.RCEP.MTHD":@VM:"L.FT.CLNT.TYPE"
    TT.LOC.FIELDS = "L.TT.FX.BUY.SRC":@VM:"L.TT.FX.SEL.DST":@VM:"L.TT.FXSN.NUM":@VM:"L.TT.LEGAL.ID":@VM:"L.TT.PAY.METHOD":@VM:"L.TT.RCEP.MTHD":@VM:"L.TT.CLNT.TYPE"
    FX.LOC.FIELDS = "L.FX.FX.BUY.SRC":@VM:"L.FX.FX.SEL.DST":@VM:"L.FX.FXSN.NUM":@VM:"L.FX.LEGAL.ID":@VM:"L.FX.PAY.METHOD":@VM:"L.FX.RCEP.MTHD":@VM:"L.FX.CLNT.TYPE"

    FIELDS.NAME.ARR = FT.LOC.FIELDS:@FM:TT.LOC.FIELDS:@FM:FX.LOC.FIELDS
    FIELD.POS.ARR = ''

    CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDS.NAME.ARR,FIELD.POS.ARR)

    L.FT.FX.BUY.SRC.POS = FIELD.POS.ARR<1,1>
    L.FT.FX.SEL.DST.POS = FIELD.POS.ARR<1,2>
    L.FT.FXSN.NUM.POS   = FIELD.POS.ARR<1,3>
    L.FT.LEGAL.ID.POS   = FIELD.POS.ARR<1,4>
    L.FT.PAY.METHOD.POS = FIELD.POS.ARR<1,5>
    L.FT.RCEP.MTHD.POS  = FIELD.POS.ARR<1,6>
    L.FT.CLNT.TYPE.POS  = FIELD.POS.ARR<1,7>

    L.TT.FX.BUY.SRC.POS = FIELD.POS.ARR<2,1>
    L.TT.FX.SEL.DST.POS = FIELD.POS.ARR<2,2>
    L.TT.FXSN.NUM.POS   = FIELD.POS.ARR<2,3>
    L.TT.LEGAL.ID.POS   = FIELD.POS.ARR<2,4>
    L.TT.PAY.METHOD.POS = FIELD.POS.ARR<2,5>
    L.TT.RCEP.MTHD.POS  = FIELD.POS.ARR<2,6>
    L.TT.CLNT.TYPE.POS  = FIELD.POS.ARR<2,7>

    L.FX.FX.BUY.SRC.POS = FIELD.POS.ARR<3,1>
    L.FX.FX.SEL.DST.POS = FIELD.POS.ARR<3,2>
    L.FX.FXSN.NUM.POS   = FIELD.POS.ARR<3,3>
    L.FX.LEGAL.ID.POS   = FIELD.POS.ARR<3,4>
    L.FX.PAY.METHOD.POS = FIELD.POS.ARR<3,5>
    L.FX.RCEP.MTHD.POS  = FIELD.POS.ARR<3,6>
    L.FX.CLNT.TYPE.POS  = FIELD.POS.ARR<3,7>

RETURN

*-----------------------------------------------------------------------------------------------------
FETCH.INFORMATION:
*-----------------------------------------------------------------------------------------------------

    SELECT.COMMAND.1 = "SELECT ":FN.REDO.FOREX.SEQ.NUM:" WITH @ID NE '' "
    TEMP.SELECT.COMMAND.1 = SELECT.COMMAND.1

    LOCATE "SEQUENCE.NUMBER" IN D.FIELDS<1> SETTING SEQ.NO.POS THEN
        SEQUENCE.NUMBER.VAL = D.RANGE.AND.VALUE<SEQ.NO.POS>
        IF SEQUENCE.NUMBER.VAL EQ "" ELSE
            SELECT.COMMAND.1:= " AND @ID EQ ":SEQUENCE.NUMBER.VAL
        END
    END ELSE
        SEQUENCE.NUMBER.VAL = ""
    END

    LOCATE "VALUE.DATE" IN D.FIELDS<1> SETTING V.DATE.POS THEN
        VALUE.DATE.VAL = D.RANGE.AND.VALUE<V.DATE.POS>
        VALUE.DATE.OPRD.POS = D.LOGICAL.OPERANDS<V.DATE.POS>
        VALUE.DATE.OPRD = OPERAND.LIST<VALUE.DATE.OPRD.POS>

        IF VALUE.DATE.OPRD EQ 'RG' THEN
            SELECT.COMMAND.1:=" AND TXN.VALUE.DATE GE ":VALUE.DATE.VAL<1,1,1>:" AND TXN.VALUE.DATE LE ":VALUE.DATE.VAL<1,1,2>
        END ELSE
            SELECT.COMMAND.1:=" AND TXN.VALUE.DATE ":VALUE.DATE.OPRD:" ":VALUE.DATE.VAL
        END
    END ELSE
        VALUE.DATE.VAL = ""
    END

    LOCATE "LEGAL.ID" IN D.FIELDS<1> SETTING LEGAL.ID.POS THEN
        LEGAL.ID.VAL = D.RANGE.AND.VALUE<LEGAL.ID.POS>
        SELECT.COMMAND.1:=" AND LEGAL.ID EQ ":LEGAL.ID.VAL
    END ELSE
        LEGAL.ID.VAL = ""
    END

    LOCATE "PASSPORT" IN D.FIELDS<1> SETTING PASS.POS THEN
        PASSPORT.VAL = D.RANGE.AND.VALUE<PASS.POS>
        SELECT.COMMAND.1:=" AND LEGAL.ID EQ ":PASSPORT.VAL
    END ELSE
        PASSPORT.VAL = ""
    END

    LOCATE "CUSTOMER.NAME" IN D.FIELDS<1> SETTING CUS.NAME.POS THEN
        CUS.NAME.VAL = D.RANGE.AND.VALUE<CUS.NAME.POS>
        CUS.NAME.VAL = SQUOTE(CUS.NAME.VAL)
        CHANGE @SM TO " " IN CUS.NAME.VAL
        SELECT.COMMAND.1:=" AND CUSTOMER.NAME EQ ":CUS.NAME.VAL
    END ELSE
        CUS.NAME.VAL = ""
    END

RETURN

*-----------------------------------------------------------------------------------------------------
SELECT.REQUEST:
*-----------------------------------------------------------------------------------------------------

    IF TEMP.SELECT.COMMAND.1 EQ SELECT.COMMAND.1 THEN
        SELECT.COMMAND.1 = "SELECT ":FN.REDO.FOREX.SEQ.NUM
    END

    SELECT.LIST = ""
    SELECT.NR = ""
    SELECT.ERR = ""
    CALL EB.READLIST(SELECT.COMMAND.1,SELECT.LIST,'',SELECT.NR,SELECT.ERR)

RETURN

*-----------------------------------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------------------------------

    LOOP
        REMOVE FXSN.ID FROM SELECT.LIST SETTING FXSN.POS
    WHILE FXSN.ID:FXSN.POS
        R.REDO.FOREX.SEQ.NUM = ''
        REDO.FOREX.SEQ.NUM.ERR = ''
        CALL F.READ(FN.REDO.FOREX.SEQ.NUM,FXSN.ID,R.REDO.FOREX.SEQ.NUM,F.REDO.FOREX.SEQ.NUM,REDO.FOREX.SEQ.NUM.ERR)
        IF R.REDO.FOREX.SEQ.NUM NE "" ELSE
            RETURN
        END
        TXN.CUSTOMER.NO = R.REDO.FOREX.SEQ.NUM<REDO.FXSN.CUSTOMER.NO>
        FX.TXN.ID = R.REDO.FOREX.SEQ.NUM<REDO.FXSN.FX.TXN.ID>

        BEGIN CASE
            CASE FX.TXN.ID[1,2] EQ "FT"
                TXN.APPLICAITON = "FUNDS.TRANSFER"
                GOSUB GET.FT.TXN.DETAILS
            CASE FX.TXN.ID[1,2] EQ "TT"
                TXN.APPLICAITON = "TELLER"
                GOSUB GET.TT.TXN.DETAILS
            CASE FX.TXN.ID[1,2] EQ "FX"
                TXN.APPLICAITON = "FOREX"
                GOSUB GET.FX.TXN.DETAILS
        END CASE

    REPEAT

RETURN

*-----------------------------------------------------------------------------------------------------
GET.FT.TXN.DETAILS:
*-----------------------------------------------------------------------------------------------------

    R.FUNDS.TRANSFER = ''
    FUNDS.TRANSFER.ERR = ''
    CALL F.READ(FN.FUNDS.TRANSFER,FX.TXN.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FUNDS.TRANSFER.ERR)

*PACS00054288 - S
    IF FUNDS.TRANSFER.ERR THEN
        FUNDS.TRANSFER.ERR = ''
        CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIS,FX.TXN.ID,R.FUNDS.TRANSFER,FUNDS.TRANSFER.ERR)
        FX.TXN.ID = FIELD(FX.TXN.ID,";",1)
    END
*PACS00054288 - E

    IF R.FUNDS.TRANSFER THEN
        TXN.BASE.CURRENCY = R.FUNDS.TRANSFER<FT.BASE.CURRENCY>
        TXN.EXCHANGE.RATE = R.FUNDS.TRANSFER<FT.CUSTOMER.RATE>
        TXN.AMT.DEBITED = R.FUNDS.TRANSFER<FT.AMOUNT.DEBITED>
        TXN.AMT.CREDITED = R.FUNDS.TRANSFER<FT.AMOUNT.CREDITED>
        IF TXN.AMT.DEBITED[1,3] EQ LCCY THEN
            TXN.AMOUNT.LCCY = TXN.AMT.DEBITED[4,99]
            TXN.AMOUNT.FCCY = TXN.AMT.CREDITED[4,99]
        END ELSE
            TXN.AMOUNT.LCCY = TXN.AMT.CREDITED[4,99]
            TXN.AMOUNT.FCCY = TXN.AMT.DEBITED[4,99]
        END
        TXN.FX.REF.NUM = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.FXSN.NUM.POS>
        TXN.FX.CCY.BUY.SRC = ""
        TXN.FX.CCY.SELL.DST = ""
        TXN.LEGAL.ID = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.LEGAL.ID.POS>
        CHANGE '.' TO @FM IN TXN.LEGAL.ID
        TXN.CUSTOMER.NAME = TXN.LEGAL.ID<3>
        TXN.ID.CARD.NO = TXN.LEGAL.ID<1>:" ":TXN.LEGAL.ID<2>
        TXN.FX.CCY.BUY.SRC = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.FX.BUY.SRC.POS>
        TXN.FX.CCY.SELL.DST = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.FX.SEL.DST.POS>
        TXN.FX.RCEP.METHOD = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.RCEP.MTHD.POS>
        TXN.FX.PAY.METHOD = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.PAY.METHOD.POS>
        TXN.FX.CLNT.TYPE  = R.FUNDS.TRANSFER<FT.LOCAL.REF,L.FT.CLNT.TYPE.POS>
        RETURN.ARR<-1> = TXN.FX.REF.NUM:"*":TXN.CUSTOMER.NAME:"*":FX.TXN.ID:"*":TXN.BASE.CURRENCY:"*":TXN.EXCHANGE.RATE:"*":TXN.AMOUNT.LCCY:"*":TXN.AMOUNT.FCCY:"*":TXN.FX.CCY.BUY.SRC:"*":TXN.FX.CCY.SELL.DST:"*":TXN.FX.RCEP.METHOD:"*":TXN.FX.PAY.METHOD:"*":TXN.FX.CLNT.TYPE:"*":TXN.ID.CARD.NO:"*":TXN.CUSTOMER.NO
    END
RETURN

*-----------------------------------------------------------------------------------------------------
GET.TT.TXN.DETAILS:
*-----------------------------------------------------------------------------------------------------
* Teller
    R.TELLER = ''
    TELLER.ERR = ''
    CALL F.READ(FN.TELLER,FX.TXN.ID,R.TELLER,F.TELLER,TELLER.ERR)

*PACS00054288 - S
    IF TELLER.ERR THEN
        TELLER.ERR = ''
        CALL EB.READ.HISTORY.REC(F.TELLER.HIS,FX.TXN.ID,R.TELLER,TELLER.ERR)
        FX.TXN.ID = FIELD(FX.TXN.ID,";",1)
    END
*PACS00054288 - E

    IF R.TELLER THEN
        IF R.TELLER<TT.TE.CURRENCY.1> NE LCCY THEN
            TXN.BASE.CURRENCY = R.TELLER<TT.TE.CURRENCY.1>
        END ELSE
            TXN.BASE.CURRENCY = R.TELLER<TT.TE.CURRENCY.2>
        END
        TXN.EXCHANGE.RATE = R.TELLER<TT.TE.RATE.1>
        TXN.AMOUNT.LCCY = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
        TXN.AMOUNT.FCCY = R.TELLER<TT.TE.AMOUNT.FCY.1>
        TXN.FX.REF.NUM = R.TELLER<TT.TE.LOCAL.REF,L.TT.FXSN.NUM.POS>
        TXN.FX.CCY.BUY.SRC = ""
        TXN.FX.CCY.SELL.DST = ""
        TXN.LEGAL.ID = R.TELLER<TT.TE.LOCAL.REF,L.TT.LEGAL.ID.POS>
        CHANGE '.' TO @FM IN TXN.LEGAL.ID

        TXN.ID.CARD.NO = TXN.LEGAL.ID<1>:" ":TXN.LEGAL.ID<2>
        TXN.CUSTOMER.NAME = TXN.LEGAL.ID<3>
        TXN.FX.CCY.BUY.SRC = R.TELLER<TT.TE.LOCAL.REF,L.TT.FX.BUY.SRC.POS>
        TXN.FX.CCY.SELL.DST = R.TELLER<TT.TE.LOCAL.REF,L.TT.FX.SEL.DST.POS>
        TXN.FX.RCEP.METHOD = R.TELLER<TT.TE.LOCAL.REF,L.TT.RCEP.MTHD.POS>

*>>>>>>>>PACS00033472 - START
*TO GET THE DESCRIPTION OF THE LOCAL FIELD L.TT.RCEP.MTHD FROM EB.LOOKUP TABLE
        VAR.VIRTUAL.TABLE = 'L.TT.RCEP.MTHD'
        CALL EB.LOOKUP.LIST(VAR.VIRTUAL.TABLE)
        CNT.VTABLE= DCOUNT(VAR.VIRTUAL.TABLE,@FM)
        VIRTUAL.TABLE.IDS = VAR.VIRTUAL.TABLE<2>      ;*2nd Part of @ID
        VIRTUAL.TABLE.VALUES = VAR.VIRTUAL.TABLE<CNT.VTABLE>    ;*Description field values
        CHANGE '_' TO @FM IN VIRTUAL.TABLE.VALUES
        CHANGE '_' TO @FM IN VIRTUAL.TABLE.IDS

        LOCATE TXN.FX.RCEP.METHOD IN VIRTUAL.TABLE.IDS SETTING POS THEN
            TXN.FX.RCEP.METHOD = VIRTUAL.TABLE.VALUES<POS>
        END
*>>>>>>>>PACS00033472 - END

        TXN.FX.PAY.METHOD = R.TELLER<TT.TE.LOCAL.REF,L.TT.PAY.METHOD.POS>

*>>>>>>>>PACS00033472 - START
*TO GET THE DESCRIPTION OF THE LOCAL FIELD L.TT.PAY.METHOD FROM EB.LOOKUP TABLE
        VAR.VIRTUAL.TABLE = 'L.TT.PAY.METHOD'
        CALL EB.LOOKUP.LIST(VAR.VIRTUAL.TABLE)
        CNT.VTABLE= DCOUNT(VAR.VIRTUAL.TABLE,@FM)
        VIRTUAL.TABLE.IDS = VAR.VIRTUAL.TABLE<2>      ;*2nd Part of @ID
        VIRTUAL.TABLE.VALUES = VAR.VIRTUAL.TABLE<CNT.VTABLE>    ;*Description field values
        CHANGE '_' TO @FM IN VIRTUAL.TABLE.VALUES
        CHANGE '_' TO @FM IN VIRTUAL.TABLE.IDS

        LOCATE TXN.FX.PAY.METHOD IN VIRTUAL.TABLE.IDS SETTING POS THEN
            TXN.FX.PAY.METHOD = VIRTUAL.TABLE.VALUES<POS>
        END
*>>>>>>>>PACS00033472 - END

        TXN.FX.CLNT.TYPE  = R.TELLER<TT.TE.LOCAL.REF,L.TT.CLNT.TYPE.POS>
        RETURN.ARR<-1> = TXN.FX.REF.NUM:"*":TXN.CUSTOMER.NAME:"*":FX.TXN.ID:"*":TXN.BASE.CURRENCY:"*":TXN.EXCHANGE.RATE:"*":TXN.AMOUNT.LCCY:"*":TXN.AMOUNT.FCCY:"*":TXN.FX.CCY.BUY.SRC:"*":TXN.FX.CCY.SELL.DST:"*":TXN.FX.RCEP.METHOD:"*":TXN.FX.PAY.METHOD:"*":TXN.FX.CLNT.TYPE:"*":TXN.ID.CARD.NO:"*":TXN.CUSTOMER.NO
    END
RETURN

*-----------------------------------------------------------------------------------------------------
GET.FX.TXN.DETAILS:
*-----------------------------------------------------------------------------------------------------
* Forex

    R.FOREX = ''
    FOREX.ERR = ''
    CALL F.READ(FN.FOREX,FX.TXN.ID,R.FOREX,F.FOREX,FOREX.ERR)

*PACS00054288 - S
    IF FOREX.ERR THEN
        FOREX.ERR = ''
        CALL EB.READ.HISTORY.REC(F.FOREX.HIS,FX.TXN.ID,R.FOREX,FOREX.ERR)
        FX.TXN.ID = FIELD(FX.TXN.ID,";",1)
    END
*PACS00054288 - E

    IF R.FOREX THEN
        TXN.BASE.CURRENCY = R.FOREX<FX.BASE.CCY>

        BEGIN CASE
            CASE R.FOREX<FX.DEAL.TYPE> EQ 'SP'
                TXN.EXCHANGE.RATE =  R.FOREX<FX.SPOT.RATE>
            CASE R.FOREX<FX.DEAL.TYPE> EQ 'FW'
                TXN.EXCHANGE.RATE = R.FOREX<FX.FORWARD.RATE>
            CASE OTHERWISE
                TXN.EXCHANGE.RATE = R.FOREX<FX.TREASURY.RATE>
        END CASE

        TXN.AMOUNT.LCCY = R.FOREX<FX.SEL.LCY.EQUIV>
        IF R.FOREX<FX.CURRENCY.BOUGHT> EQ TXN.BASE.CURRENCY THEN
            TXN.AMOUNT.FCCY = R.FOREX<FX.AMOUNT.BOUGHT>
        END ELSE
            TXN.AMOUNT.FCCY = R.FOREX<FX.AMOUNT.SOLD>
        END
        TXN.FX.REF.NUM = R.FOREX<FX.LOCAL.REF,L.FX.FXSN.NUM.POS>
        TXN.FX.CCY.BUY.SRC = ""
        TXN.FX.CCY.SELL.DST = ""

        TXN.LEGAL.ID = R.FOREX<FX.LOCAL.REF,L.FX.LEGAL.ID.POS>
        CHANGE '.' TO @FM IN TXN.LEGAL.ID

        TXN.ID.CARD.NO = TXN.LEGAL.ID<1>:" ":TXN.LEGAL.ID<2>
        TXN.CUSTOMER.NAME = TXN.LEGAL.ID<3>
        TXN.FX.CCY.BUY.SRC = R.FOREX<FX.LOCAL.REF,L.FX.FX.BUY.SRC.POS>
        TXN.FX.CCY.SELL.DST = R.FOREX<FX.LOCAL.REF,L.FX.FX.SEL.DST.POS>
        TXN.FX.RCEP.METHOD = R.FOREX<FX.LOCAL.REF,L.FX.RCEP.MTHD.POS>
        TXN.FX.PAY.METHOD = R.FOREX<FX.LOCAL.REF,L.FX.PAY.METHOD.POS>
        TXN.FX.CLNT.TYPE  = R.FOREX<FX.LOCAL.REF,L.FX.CLNT.TYPE.POS>
        RETURN.ARR<-1> = TXN.FX.REF.NUM:"*":TXN.CUSTOMER.NAME:"*":FX.TXN.ID:"*":TXN.BASE.CURRENCY:"*":TXN.EXCHANGE.RATE:"*":TXN.AMOUNT.LCCY:"*":TXN.AMOUNT.FCCY:"*":TXN.FX.CCY.BUY.SRC:"*":TXN.FX.CCY.SELL.DST:"*":TXN.FX.RCEP.METHOD:"*":TXN.FX.PAY.METHOD:"*":TXN.FX.CLNT.TYPE:"*":TXN.ID.CARD.NO:"*":TXN.CUSTOMER.NO

    END
RETURN

END
