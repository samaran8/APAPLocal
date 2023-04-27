* @ValidationCode : MjoxNDY0MzkyMzAzOkNwMTI1MjoxNjgxMjgzOTM2NzQwOklUU1M6LTE6LTE6NjIxOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:48:56
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 621
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.AMORT.SEC.TRADE.UPDATE
*-------------------------------------------------------------------
*  This is an authorisation routine to update APAP.AMORT.SEC.TRADE
*  with @id, nominals for Authorisation & Reversal
*-------------------------------------------------------------------
* Modification History:
*
* 03/12/2010 - ODR-2010-07-0081 - Shankar Raju - Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*10-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                VM TO @VM, FM TO @fM
*10-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SEC.TRADE
    $INSERT I_F.SEC.ACC.MASTER
    $INSERT I_F.REDO.AMORT.SEC.TRADE
    $INSERT I_F.REDO.CALL.LIST.CATEG
    $INSERT I_F.REDO.ST.TXN.CODE.PARAM

    GOSUB INITIAL.CHECK

RETURN

*-------------------------------------------------------------------
INITIAL.CHECK:
*-------------

    FN.SEC.ACC.MASTER = 'F.SEC.ACC.MASTER'   ; R.SEC.ACC.MASTER = ''
    F.SEC.ACC.MASTER = ''                    ; ERR.SAM = ''
    CALL OPF(FN.SEC.ACC.MASTER,F.SEC.ACC.MASTER)

    Y.PORT.ID = R.NEW(SC.SBS.CUST.SEC.ACC)

    CALL F.READ(FN.SEC.ACC.MASTER,Y.PORT.ID,R.SEC.ACC.MASTER,F.SEC.ACC.MASTER,ERR.SAM)

    IF R.SEC.ACC.MASTER<SC.SAM.DEALER.BOOK> THEN
        GOSUB INITIALISE
        GOSUB UPDATE.APAP.AMORT.SEC.TRADE
    END

RETURN
*-------------------------------------------------------------------
INITIALISE:
*----------

    FN.REDO.CALL.LIST.CATEG = 'F.REDO.CALL.LIST.CATEG'                         ; R.REDO.CALL.LIST.CATEG = ""
    F.REDO.CALL.LIST.CATEG = ''                                                ; ERR.RCLC = ""
    CALL OPF(FN.REDO.CALL.LIST.CATEG, F.REDO.CALL.LIST.CATEG)

    R.APAP.AMORT.SEC.TRADE = ''

    FN.REDO.ST.TXN.CODE.PARAM = 'F.REDO.ST.TXN.CODE.PARAM'                     ; R.REDO.ST.TXN.CODE.PARAM = ""
    F.REDO.ST.TXN.CODE.PARAM = ''                                              ; ERR.RSTCP = ""

    CALL CACHE.READ(FN.REDO.ST.TXN.CODE.PARAM,'BUY',R.REDO.ST.TXN.CODE.BUY,ERR.RSTCP)

    CALL CACHE.READ(FN.REDO.ST.TXN.CODE.PARAM,'SEL',R.REDO.ST.TXN.CODE.SEL,ERR.RSTCP)

    Y.APAP.AMORT.SEC.TRADE  = R.NEW(SC.SBS.CUST.SEC.ACC):".":R.NEW(SC.SBS.SECURITY.CODE)
    Y.TXN.REF               = ID.NEW
    Y.VALUE.DT              = R.NEW(SC.SBS.VALUE.DATE)
    Y.NOMINAL               = R.NEW(SC.SBS.CUST.NO.NOM)
    Y.BUY.SELL.MARKER       = R.NEW(SC.SBS.CUST.TRANS.CODE)

    LOCATE Y.BUY.SELL.MARKER IN R.REDO.ST.TXN.CODE.BUY<1> SETTING POS.BUY THEN
        Y.STAGE.MARKER      = 'CREDIT'
    END ELSE
        LOCATE Y.BUY.SELL.MARKER IN R.REDO.ST.TXN.CODE.SEL<1> SETTING POS.SEL THEN
            Y.STAGE.MARKER      = 'DEBIT'
        END
    END

    FN.REDO.AMORT.SEC.TRADE = 'F.REDO.AMORT.SEC.TRADE'
    F.REDO.AMORT.SEC.TRADE  = ''
    CALL OPF(FN.REDO.AMORT.SEC.TRADE,F.REDO.AMORT.SEC.TRADE)

    GOSUB READ.APAP.AMORT.SEC.TRADE

RETURN
*-------------------------------------------------------------------
READ.APAP.AMORT.SEC.TRADE:
*-------------------------

    R.APAP.AMORT.SEC.TRADE = '' ; READ.ERR = '' ; RETRY = ''
    CALL F.READU(FN.REDO.AMORT.SEC.TRADE, Y.APAP.AMORT.SEC.TRADE, R.APAP.AMORT.SEC.TRADE, F.REDO.AMORT.SEC.TRADE, READ.ERR, RETRY)

RETURN
*-------------------------------------------------------------------
UPDATE.APAP.AMORT.SEC.TRADE:
*---------------------------
    Y.RECORD.STATUS = R.NEW(SC.SBS.RECORD.STATUS)

    BEGIN CASE

        CASE V$FUNCTION EQ 'I' OR (V$FUNCTION EQ 'A' AND Y.RECORD.STATUS EQ 'INAU')
            GOSUB AUTH.PROCESS
        CASE V$FUNCTION EQ 'R' OR (V$FUNCTION EQ 'A' AND Y.RECORD.STATUS EQ 'RNAU')
            GOSUB REVERSAL.PROCESS

    END CASE

    GOSUB WRITE.APAP.AMORT.SEC.TRADE

RETURN
*-------------------------------------------------------------------
AUTH.PROCESS:
*------------
    BEGIN CASE
        CASE Y.STAGE.MARKER EQ "CREDIT"
            GOSUB UPDATE.CR.TXN
        CASE Y.STAGE.MARKER EQ "DEBIT"
            GOSUB UPDATE.DR.TXN
    END CASE

RETURN
*-------------------------------------------------------------------
REVERSAL.PROCESS:
*----------------
    BEGIN CASE
        CASE Y.STAGE.MARKER EQ "CREDIT"
            GOSUB REVERSE.CR
        CASE Y.STAGE.MARKER EQ "DEBIT"
            GOSUB REVERSE.DR
    END CASE

RETURN
*-------------------------------------------------------------------
UPDATE.CR.TXN:
*-------------
* Update MF.TXN.LINK with the credit txn detail

    BUY.VAL.DAT.POSN = 1

    LOOP.COUNT = DCOUNT(R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.VALUE.DT>,@VM)

    COUNT.NOW = 1

    LOOP
    WHILE COUNT.NOW LE LOOP.COUNT
        IF Y.VALUE.DT GE R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.VALUE.DT,COUNT.NOW> THEN
            BUY.VAL.DAT.POSN += 1
        END
        COUNT.NOW += 1
    REPEAT

    INS ID.NEW BEFORE R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.TXN.REF,BUY.VAL.DAT.POSN>
    INS Y.VALUE.DT BEFORE R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.VALUE.DT,BUY.VAL.DAT.POSN>
    INS Y.NOMINAL BEFORE R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.NOMINAL,BUY.VAL.DAT.POSN>
    INS Y.NOMINAL BEFORE R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BALANCE.NOMINAL,BUY.VAL.DAT.POSN>

RETURN
*-------------------------------------------------------------------
UPDATE.DR.TXN:
*-------------

    Y.SELL.UPD.NUM          = ''
    Y.ALL.BUY               = R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.TXN.REF>
    Y.TOT.BUY               = DCOUNT(Y.ALL.BUY,@VM)

    Y.NEXT.VAL              = 1

    LOOP
    WHILE Y.NEXT.VAL LE Y.TOT.BUY

        Y.BAL.NOMINAL       = ''
        Y.CURR.NOMINAL      = ''
        Y.SELL.NOMINAL      = ''

        Y.BAL.NOMINAL = R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BALANCE.NOMINAL,Y.NEXT.VAL>

        IF Y.BAL.NOMINAL LE 0 THEN
            Y.NEXT.VAL += 1
            CONTINUE
        END
        Y.REMAINING.NOM     = Y.NOMINAL - Y.BAL.NOMINAL
        Y.CURR.NOMINAL      = Y.BAL.NOMINAL - Y.NOMINAL

        IF Y.CURR.NOMINAL LT 0 THEN
            Y.CURR.NOMINAL  = 0
            Y.SELL.NOMINAL  = Y.BAL.NOMINAL
        END ELSE
            Y.SELL.NOMINAL  = Y.BAL.NOMINAL - Y.CURR.NOMINAL
        END

        R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BALANCE.NOMINAL,Y.NEXT.VAL> = Y.CURR.NOMINAL

        Y.SELL.UPD.NUM      = DCOUNT(R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.TXN.REF,Y.NEXT.VAL>,@SM)
        Y.SELL.UPD.NUM +=1

        R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.TXN.REF,Y.NEXT.VAL,Y.SELL.UPD.NUM> = Y.TXN.REF
        R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.VALUE.DT,Y.NEXT.VAL,Y.SELL.UPD.NUM> = Y.VALUE.DT
        R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.NOMINAL,Y.NEXT.VAL,Y.SELL.UPD.NUM> = Y.SELL.NOMINAL

        IF Y.REMAINING.NOM LE 0 THEN
            RETURN
        END

        Y.NOMINAL = Y.REMAINING.NOM
        Y.REMAINING.NOM = ''
        Y.NEXT.VAL += 1
    REPEAT

RETURN
*-------------------------------------------------------------------
REVERSE.DR:
*----------

    Y.ALL.SELL = DCOUNT(R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.TXN.REF>,@VM)

    Y.THIS.SELL = 1

    LOOP
    WHILE Y.THIS.SELL LE Y.ALL.SELL

        Y.THIS.IN.SELL = 1

        LOCATE ID.NEW IN R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.TXN.REF,Y.THIS.SELL,1> SETTING Y.SELL.ID.POS THEN

            ADJ.NOM = R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.NOMINAL,Y.THIS.SELL,Y.SELL.ID.POS>
            R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BALANCE.NOMINAL,Y.THIS.SELL> += ADJ.NOM

            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.TXN.REF,Y.THIS.SELL,Y.SELL.ID.POS>
            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.VALUE.DT,Y.THIS.SELL,Y.SELL.ID.POS>
            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.SELL.NOMINAL,Y.THIS.SELL,Y.SELL.ID.POS>
        END

        Y.THIS.SELL += 1
    REPEAT

RETURN
*-------------------------------------------------------------------
REVERSE.CR:
*----------

    LOCATE ID.NEW IN R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.TXN.REF,1> SETTING Y.BUY.ID.POS THEN

        Y.NOMINAL.CALC = R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.NOMINAL,Y.BUY.ID.POS> - R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BALANCE.NOMINAL,Y.BUY.ID.POS>

        IF Y.NOMINAL.CALC EQ 0 THEN
            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.TXN.REF,Y.BUY.ID.POS>
            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.VALUE.DT,Y.BUY.ID.POS>
            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BUY.NOMINAL,Y.BUY.ID.POS>
            DEL R.APAP.AMORT.SEC.TRADE<APAP.AMORT.ST.BALANCE.NOMINAL,Y.BUY.ID.POS>
            IF NOT(R.APAP.AMORT.SEC.TRADE) THEN
                CALL F.DELETE(FN.REDO.AMORT.SEC.TRADE,Y.APAP.AMORT.SEC.TRADE)
            END
        END
    END

RETURN

*-------------------------------------------------------------------
WRITE.APAP.AMORT.SEC.TRADE:
*--------------------------

    CALL F.WRITE(FN.REDO.AMORT.SEC.TRADE, Y.APAP.AMORT.SEC.TRADE, R.APAP.AMORT.SEC.TRADE)

RETURN
*-------------------------------------------------------------------
END
