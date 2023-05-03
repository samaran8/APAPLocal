* @ValidationCode : MjoxMjU3NzY3NTQ6Q3AxMjUyOjE2ODMwNTY1MjUxNzc6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 03 May 2023 01:12:05
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
*-----------------------------------------------------------------------------
* <Rating>-65</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.V.INP.CHQ.MAND
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : GANESH R
* Program Name  : REDO.V.INP.CHQ.MAND
*-------------------------------------------------------------------------
* Description: This routine is a Auth routine
*
*----------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE              ODR                             DESCRIPTION
* 25-11-10          ODR-2010-09-0251              Initial Creation

*
* Date             Who                   Reference      Description
* 03.05.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 03.04.2023       Shanmugapriya M       R22            Manual Conversion   - FM TO @FM, VM TO @VM
*
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.PARAMETER
    $INSERT I_T24.FS.COMMON

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB PROCESS

RETURN

INIT:
    LOC.APPLICATION  = 'T24.FUND.SERVICES'
    LOC.FIELDS       = 'L.TT.NO.OF.CHQ':@VM:'L.TT.CHQ.TYPE'
    LOC.POS          = ''
RETURN

*********
OPEN.FILE:
*********

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN

********
PROCESS:
********
    CALL MULTI.GET.LOC.REF(LOC.APPLICATION,LOC.FIELDS,LOC.POS)
    LOC.CHQ.POS  = LOC.POS<1,1>
    LOC.CHQ.TYPE = LOC.POS<1,2>
    IF PGM.VERSION EQ ',LCY.COLLECT' THEN
        VAR.TRANS.AC = R.NEW(TFS.PRIMARY.ACCOUNT)
        CALL F.READ(FN.ACCOUNT,VAR.TRANS.AC,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
        VAR.TRANC.CCY = R.ACCOUNT<AC.CURRENCY>
        IF VAR.TRANC.CCY NE LCCY THEN
            AF = TFS.PRIMARY.ACCOUNT
            ETEXT = "AC-INVALID.AC.NO"
            CALL STORE.END.ERROR
        END
    END
    Y.TOT.TXNS     = R.NEW(TFS.TRANSACTION)
    Y.TOT.TXNS.CNT = DCOUNT(Y.TOT.TXNS,@VM)
    IF R.NEW(TFS.DO.CASH.BACK) EQ 'YES' THEN
        GOSUB PROCESS.CASHBACK
    END

    LOCATE 'NET.ENTRY' IN Y.TOT.TXNS<1,1> SETTING POS.TXN THEN
        GOSUB PROCESS.NET.ENTRY
    END

RETURN
*--------------------------------------------------------
PROCESS.CASHBACK:
*--------------------------------------------------------
* Here we will check the sum of all cashback transaction amount is null. i.e no difference in amount.
* we are not using the TFS application fields TFS.CASH.BACK.DIR & TFS.CASH.BACK.TXN cos it is not updating properly if we alter the amount.

    Y.CASHBACK.DIRECTION = 'OUT':@FM:'IN':@FM:'OUT'   ;* Even in core routine T24.FS.CHECK.FIELDS, this is the order!!!
    Y.CASH.BACK.TXN      = TFS$R.TFS.PAR<TFS.PAR.CASH.BACK.TXN.1>:@FM:TFS$R.TFS.PAR<TFS.PAR.CASH.BACK.TXN.2>:@FM:TFS$R.TFS.PAR<TFS.PAR.CASH.BACK.TXN.3>

    Y.FINAL.AMT = 0


    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.TOT.TXNS.CNT

        Y.TXN = Y.TOT.TXNS<1,Y.CNT>
        Y.AMT           = R.NEW(TFS.AMOUNT)<1,Y.CNT>
        LOCATE Y.TXN IN Y.CASH.BACK.TXN SETTING POS2 THEN
            IF Y.CASHBACK.DIRECTION<POS2> EQ 'IN' THEN
                Y.FINAL.AMT += Y.AMT
                AV = Y.CNT
            END
            IF Y.CASHBACK.DIRECTION<POS2> EQ 'OUT' THEN
                Y.FINAL.AMT -= Y.AMT
                AV = Y.CNT
            END
        END
        Y.CNT++
    REPEAT

    IF Y.FINAL.AMT THEN
        AF = TFS.AMOUNT
        AV = AV
        ETEXT = 'EB-REDO.CASHBACK.MISMATCH'
        CALL STORE.END.ERROR
    END

RETURN
*---------------------------------------------------------------
PROCESS.NET.ENTRY:
*---------------------------------------------------------------
* Here we will check whether all the amount entered matches with the net-entry amount.

    NET.ENTRY.WTHRU.CATEG = TFS$R.TFS.PAR<TFS.PAR.NET.ENTRY.WASHTHRU>
    ENTRY.AMT = 0
    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.TOT.TXNS.CNT

        BEGIN CASE
            CASE R.NEW(TFS.ACCOUNT.DR)<1,Y.CNT>[4,5] EQ NET.ENTRY.WTHRU.CATEG
                ENTRY.AMT -= R.NEW(TFS.AMOUNT)<1,Y.CNT>
            CASE R.NEW(TFS.ACCOUNT.CR)<1,Y.CNT>[4,5] EQ NET.ENTRY.WTHRU.CATEG
                ENTRY.AMT += R.NEW(TFS.AMOUNT)<1,Y.CNT>
        END CASE
        Y.CNT++
    REPEAT
    IF ENTRY.AMT THEN
        AF = TFS.AMOUNT
        AV = POS.TXN
        ETEXT = 'EB-REDO.NET.ENTRY.NOT.MATCH'
        CALL STORE.END.ERROR
    END

RETURN
END
*End of program
