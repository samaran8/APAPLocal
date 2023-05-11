* @ValidationCode : MjotMzgyODU2Mjg0OkNwMTI1MjoxNjgxMzc2MDk4NDg0OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:24:58
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
SUBROUTINE REDO.MULTI.TRAN.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: JEEVA T
* PROGRAM NAME: REDO.MULTI.TRAN.AUTORTN
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.MULTI.TXN.PROCESS to
* default the value for the  MULTI.TRANSACTION.SERVICE application from REDO.MULTI.TXN.PROCESS
* It is AUTOM NEW CONTENT routine

*IN PARAMETER : NA
*OUT PARAMETER: NA
*LINKED WITH  : REDO.MULTI.TXN.PROCESS
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
* DATE           WHO           REFERENCE         DESCRIPTION
*16-11-2010   JEEVA T           B.12           INITIAL CREATION
*13.04.2023   Conversion Tool   R22            Auto Conversion     - FM TO @FM, VM TO @VM, ++ TO += 1
*13.04.2023   Shanmugapriya M   R22            Manual Conversion   - No changes
*
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.ALTERNATE.ACCOUNT
    $INSERT I_F.REDO.MULTI.TXN.PROCESS
    $INSERT I_F.MULTI.TRANSACTION.SERVICE



    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------


    FN.REDO.MULTI.TXN.PROCESS = 'F.REDO.MULTI.TXN.PROCESS'
    F.REDO.MULTI.TXN.PROCESS = ''
    CALL OPF(FN.REDO.MULTI.TXN.PROCESS,F.REDO.MULTI.TXN.PROCESS)

    FN.ALTERNATE.ACCOUNT = 'F.ALTERNATE.ACCOUNT'
    F.ALTERNATE.ACCOUNT = ''
    CALL OPF(FN.ALTERNATE.ACCOUNT,F.ALTERNATE.ACCOUNT)

    FN.MULTI.TRANSACTION.SERVICE = 'F.MULTI.TRANSACTION.SERVICE'
    F.MULTI.TRANSACTION.SERVICE = ''
    CALL OPF(FN.MULTI.TRANSACTION.SERVICE,F.MULTI.TRANSACTION.SERVICE)

RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    Y.DATA = "" ; Y.CASH.FLAG = '' ; Y.CHEQUE.FLAG = '' ; Y.SETTLE.TYPE = ''
    CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.REDO.MULTI.PROCESS.ID=FIELD(Y.DATA,"*",2)

    CALL F.READ(FN.REDO.MULTI.TXN.PROCESS,Y.REDO.MULTI.PROCESS.ID,R.REDO.MULTI.TXN.PROCESS,F.REDO.MULTI.TXN.PROCESS,PRO.ERR)
    Y.AA.ID      = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.ARRANGEMENT.ID>
    Y.TRANS.DATE = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.VALUE.DATE>

    R.NEW(REDO.MTS.ARRANGEMENT.ID)     = Y.AA.ID
    R.NEW(REDO.MTS.TRANSACTION.DATE)   = Y.TRANS.DATE
    Y.AMOUNT                           = SUM(R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.AMOUNT>)

*    R.NEW(REDO.MTS.TOTAL.AMT)          = Y.AMOUNT

    Y.CNT=DCOUNT(R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.CURRENCY>,@VM)

    FOR Y.COUNT=1 TO Y.CNT
        Y.TRANS.TYPE = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.TRAN.TYPE,Y.COUNT>
        Y.CURRENCY   = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.CURRENCY,Y.COUNT>
        Y.AMT        = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.AMOUNT,Y.COUNT>
        Y.ARR.ID     = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.ARRANGEMENT.ID>
        Y.OVER       = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.TOT.AMT.OVRDUE>
        R.NEW(REDO.MTS.PAYMENT.MODE)<1,Y.COUNT>    = Y.TRANS.TYPE
        R.NEW(REDO.MTS.CURRENCY)<1,Y.COUNT>        = Y.CURRENCY
        R.NEW(REDO.MTS.TRANSACTION.AMT)<1,Y.COUNT> = Y.AMT
        R.NEW(REDO.MTS.ARR.ID)<1,Y.COUNT>          = Y.ARR.ID
        R.NEW(REDO.MTS.OD.AMT)<1,Y.COUNT>          = Y.OVER
        R.NEW(REDO.MTS.OD.BILLS)<1,Y.COUNT>        = R.REDO.MULTI.TXN.PROCESS<MUL.TXN.PRO.NO.OF.OD.BILLS>
        Y.SETTLE.TYPE<-1> = Y.TRANS.TYPE
    NEXT Y.COUNT
    R.NEW(REDO.MTS.RESERVED.1) = Y.REDO.MULTI.PROCESS.ID
    Y.CHECK = R.NEW(REDO.MTS.RESERVED.1)
    Y.COUNT.SET = DCOUNT(Y.SETTLE.TYPE,@FM)
    Y.COUNT = 1
    LOOP
    WHILE Y.COUNT LE Y.COUNT.SET
        IF Y.SETTLE.TYPE<Y.COUNT> EQ 'CASH' THEN
            Y.CASH.FLAG = '1'
        END
        IF Y.SETTLE.TYPE<Y.COUNT> EQ 'CHEQUE' THEN
            Y.CHEQUE.FLAG = '1'
        END
        Y.COUNT += 1                              ;** R22 Auto conversion - ++ TO += 1
    REPEAT
    IF Y.CASH.FLAG AND NOT(Y.CHEQUE.FLAG) THEN
        R.NEW(REDO.MTS.SETTLEMENT.MODE) = 'CASH'
    END
    IF Y.CHEQUE.FLAG AND NOT(Y.CASH.FLAG) THEN
        R.NEW(REDO.MTS.SETTLEMENT.MODE)  = 'CHEQUE'
    END
    IF Y.CHEQUE.FLAG AND Y.CASH.FLAG THEN
        R.NEW(REDO.MTS.SETTLEMENT.MODE) = 'CASH-CHEQUE'
    END
RETURN
*--------------------------------------------------------------------------------------------------
END
