* @ValidationCode : MjotMTE4Njc5ODU3MzpDcDEyNTI6MTY4MTE5MzkzNDEwMzpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 11:48:54
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
SUBROUTINE REDO.VP.E.CONSOLIDATED(Y.ARRAY)
    
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 11.04.2023       Conversion Tool       R22            Auto Conversion     - FM TO @FM, VM TO @VM, = TO EQ
* 11.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------------------
    

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER

*****check for actual CHANNELS values (e.g. is TELLER or Code or ...)
*****create subtotals
*****FTTC AC43 AC09

    GOSUB OPENFILES
    GOSUB LOCATE.VALUES
    GOSUB SELECT.ARRANGEMENT
RETURN

OPENFILES:
    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    SELECT.CMD.TT = 'SELECT ' : FN.TELLER ;* : ' WITH TRANSACTION.CODE EQ 432 161 160'

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    SELECT.CMD.FT = 'SELECT ' : FN.FUNDS.TRANSFER : ' WITH TRANSACTION.TYPE EQ "FTTC" "AC43" "AC09"'

RETURN

LOCATE.VALUES:
    LOCATE 'CLIENT NO' IN D.FIELDS<1> SETTING Y.CLI.POS THEN
        Y.CLI.OP      = D.LOGICAL.OPERANDS<Y.CLI.POS>
        Y.CLI.NO     = D.RANGE.AND.VALUE<Y.CLI.POS>
    END

    LOCATE 'DEBIT ACCOUNT NO' IN D.FIELDS<1> SETTING Y.DR.ACCT.POS THEN
        Y.DR.ACCT.OP  = D.LOGICAL.OPERANDS<Y.DR.ACCT.POS>
        Y.DR.ACCT.NO    = D.RANGE.AND.VALUE<Y.DR.ACCT.POS>
    END

    LOCATE 'CREDIT CARD NO' IN D.FIELDS<1> SETTING Y.CR.ACCT.POS THEN
        Y.CR.CARD.OP = D.LOGICAL.OPERANDS<Y.CR.ACCT.POS>
        Y.CR.CARD.NO    = D.RANGE.AND.VALUE<Y.CR.ACCT.POS>[1,6] : '******' : D.RANGE.AND.VALUE<Y.CR.ACCT.POS>[-4,4]
    END

    LOCATE 'CHANNEL' IN D.FIELDS<1> SETTING Y.CHANNEL.POS THEN
        Y.CHANNEL.OP = D.LOGICAL.OPERANDS<Y.CHANNEL.POS>
        Y.CHANNEL = D.RANGE.AND.VALUE<Y.CHANNEL.POS>
    END

    LOCATE 'TRANSACTION DATE' IN D.FIELDS<1> SETTING Y.TRAN.DATE.POS THEN
        Y.TRAN.DATE.OP = D.LOGICAL.OPERANDS<Y.TRAN.DATE.POS>
        Y.TRAN.DATE = D.RANGE.AND.VALUE<Y.TRAN.DATE.POS>
    END

    LOCATE 'CURRENCY' IN D.FIELDS<1> SETTING Y.CURR.POS THEN
        Y.CURR.OP = D.LOGICAL.OPERANDS<Y.CURR.POS>
        Y.CURR  = D.RANGE.AND.VALUE<Y.CURR.POS>
    END

    LOC.REF.APPL     =  'TELLER':@FM:'FUNDS.TRANSFER'
    LOC.REF.FIELDS   =  'L.TT.CLIENT.COD':@VM:'L.TT.CR.CARD.NO':@VM:'L.TT.CLIENT.NME':@VM:'L.TT.MSG.CODE':@FM:'L.FT.CLIENT.COD':@VM:'L.FT.CR.CARD.NO':@VM:'L.FT.CLIENT.NME':@VM:'L.FT.MSG.CODE'
    LOC.REF.POS      = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    TT.CLI.COD.POS = LOC.REF.POS<1,1>
    TT.CAR.NO.POS = LOC.REF.POS<1,2>
    TT.CLI.NME.POS = LOC.REF.POS<1,3>
    TT.MSG.COD.POS = LOC.REF.POS<1,4>
    FT.CLI.COD.POS = LOC.REF.POS<2,1>
    FT.CAR.NO.POS = LOC.REF.POS<2,2>
    FT.CLI.NME.POS = LOC.REF.POS<2,3>
    FT.MSG.COD.POS = LOC.REF.POS<2,4>

    QRY.FLD = 'L.TT.CLIENT.COD/L.FT.CLIENT.COD'; QRY.OP = Y.CLI.OP ; QRY.VAL = Y.CLI.NO
    GOSUB SET.SELECT.CMD
    QRY.FLD = 'ACCOUNT.1/DEBIT.ACCT.NO' ; QRY.OP = Y.DR.ACCT.OP ; QRY.VAL = Y.DR.ACCT.NO
    GOSUB SET.SELECT.CMD
    QRY.FLD = 'L.TT.CR.CARD.NO/L.FT.CR.CARD.NO' ; QRY.OP = Y.CR.CARD.OP ; QRY.VAL = Y.CR.CARD.NO
    GOSUB SET.SELECT.CMD
    QRY.FLD = 'DATE.TIME/DATE.TIME' ; QRY.OP = Y.TRAN.DATE.OP ; QRY.VAL = Y.TRAN.DATE
    GOSUB SET.SELECT.CMD
    QRY.FLD = 'CURRENCY.1/DEBIT.CURRENCY' ; QRY.OP = Y.CURR.OP ; QRY.VAL = Y.CURR
    GOSUB SET.SELECT.CMD

RETURN

SELECT.ARRANGEMENT:
    F.SEP = '&'

    Y.SEL.LIST = ''
    TT.ID = ''
    CALL EB.READLIST(SELECT.CMD.TT,Y.SEL.LIST,'',Y.SEL.CNT,Y.ERR.SELL)
    LOOP
        REMOVE TT.ID FROM Y.SEL.LIST SETTING POS
    WHILE TT.ID:POS
        CALL F.READ(FN.TELLER,TT.ID,R.T,F.TELLER,Y.TT.ERR)
        IF R.T THEN
            TT.LINE = R.T<TT.TE.TRANSACTION.CODE>:F.SEP
            TT.LINE := TT.ID:F.SEP
            TT.LINE := FIELD(R.T<TT.TE.LOCAL.REF,TT.CLI.COD.POS>,'/',1) : F.SEP
            TT.LINE := R.T<TT.TE.ACCOUNT.1> : F.SEP
            TT.LINE := R.T<TT.TE.LOCAL.REF,TT.CAR.NO.POS> : F.SEP
            TT.LINE := R.T<TT.TE.AMOUNT.LOCAL.1> : F.SEP
            TT.LINE := R.T<TT.TE.CURRENCY.1> : F.SEP
            TT.LINE := '20':R.T<TT.TE.DATE.TIME>[1,2]:' ':R.T<TT.TE.DATE.TIME>[3,2]:' ':R.T<TT.TE.DATE.TIME>[5,2] : F.SEP
            TT.LINE := R.T<TT.TE.LOCAL.REF,TT.CLI.NME.POS> : F.SEP
            TT.LINE := R.T<TT.TE.LOCAL.REF,TT.MSG.COD.POS>
            Y.ARRAY<-1>= TT.LINE
        END
    REPEAT

    Y.SEL.LIST = ''
    FT.ID = ''
    CALL EB.READLIST(SELECT.CMD.FT,Y.SEL.LIST,'',Y.SEL.CNT,Y.ERR.SELL)
    LOOP
        REMOVE FT.ID FROM Y.SEL.LIST SETTING POS
    WHILE FT.ID:POS
        CALL F.READ(FN.FUNDS.TRANSFER,Y.FT.ID,R.FT,F.FUNDS.TRANSFER,Y.FT.ERR)
        IF R.FT THEN
            Y.TRAN.TYPE = R.FT<FT.TRANSACTION.TYPE>
            BEGIN CASE
                CASE Y.TRAN.TYPE EQ 'AC09'
                    Y.CANAL = 'IVR'
                CASE Y.TRAN.TYPE EQ 'AC43'
                    Y.CANAL = 'IB'
                CASE Y.TRAN.TYPE EQ 'FTTC'
                    Y.CANAL = 'FTC'
            END CASE
            IF Y.CHANNEL MATCHES '' : @VM : Y.CANAL THEN
                FT.LINE = Y.CANAL : F.SEP
                FT.LINE := FT.ID : F.SEP
                FT.LINE := FIELD(R.FT<FT.LOCAL.REF,FT.CLI.COD.POS>,'/',1) : F.SEP
                FT.LINE := R.FT<FT.DEBIT.ACCT.NO> : F.SEP
                FT.LINE := R.FT<FT.LOCAL.REF,FT.CAR.NO.POS> : F.SEP
                FT.LINE := R.FT<FT.DEBIT.AMOUNT> : F.SEP
                FT.LINE := R.FT<FT.DEBIT.CURRENCY> : F.SEP
                FT.LINE := '20':R.FT<FT.DATE.TIME>[1,2]:' ':R.FT<FT.DATE.TIME>[3,2]:' ':R.FT<FT.DATE.TIME>[5,2] : F.SEP
                FT.LINE := R.FT<FT.LOCAL.REF,FT.CLI.NME.POS> : F.SEP
                FT.LINE := R.FT<FT.LOCAL.REF,FT.MSG.COD.POS>
                Y.ARRAY<-1>= FT.LINE
            END
        END
    REPEAT

RETURN

SET.SELECT.CMD:
    IF UNASSIGNED(QRY.OP) THEN
        QRY.OP = ''
    END
    IF UNASSIGNED(QRY.VAL) THEN
        QRY.VAL = ''
    END

    IF QRY.OP NE '' AND QRY.VAL NE '' THEN
        OP = ''
        BEGIN CASE
            CASE QRY.OP EQ 1              ;** R22 Auto conversion - = TO EQ
                OP = 'EQ'
            CASE QRY.OP EQ 2                 ;** R22 Auto conversion - = TO EQ
                OP = 'RG'
            CASE QRY.OP EQ 3           ;** R22 Auto conversion - = TO EQ
                OP = 'LT'
            CASE QRY.OP EQ 4           ;** R22 Auto conversion - = TO EQ
                OP = 'GT'
            CASE QRY.OP EQ 5           ;** R22 Auto conversion - = TO EQ
                OP = 'NE'
            CASE QRY.OP EQ 6             ;** R22 Auto conversion - = TO EQ
                OP = 'LK'
            CASE QRY.OP EQ 7             ;** R22 Auto conversion - = TO EQ
                OP = 'UL'
            CASE QRY.OP EQ 8             ;** R22 Auto conversion - = TO EQ
                OP = 'LE'
            CASE QRY.OP EQ 9             ;** R22 Auto conversion - = TO EQ
                OP = 'GE'
            CASE QRY.OP EQ 10               ;** R22 Auto conversion - = TO EQ
                OP = 'NR'
        END CASE
        SELECT.CMD.TT := ' WITH ' : FIELD(QRY.FLD,'/',1) : ' ' : OP : ' ' : QRY.VAL
        SELECT.CMD.FT := ' WITH ' : FIELD(QRY.FLD,'/',2) : ' ' : OP : ' ' : QRY.VAL
    END

RETURN
