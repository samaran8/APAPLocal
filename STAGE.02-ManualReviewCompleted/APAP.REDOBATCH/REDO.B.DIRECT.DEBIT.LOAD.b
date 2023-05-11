* @ValidationCode : MjotMTM3Njg1NDA4MjpDcDEyNTI6MTY4MTE5MDMyNDQyMTpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 10:48:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.DIRECT.DEBIT.LOAD

*------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : JEEVA T
* PROGRAM NAME : REDO.B.DIRECT.DEBIT.LOAD
*------------------------------------------------------------------
* Description : This is the load rotuine to initialise all the variables
* to be used in this batch routine
*------------------------------------------------------------------

*Modification Details:
*=====================
*   Date            Who           Reference            Description
* 31-10-2011       JEEVA T        B.9-Direct Debit
* Date                   who                   Reference              
* 11-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM 
* 11-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_REDO.B.DIRECT.DEBIT.COMMON
    $INSERT I_F.REDO.W.DIRECT.DEBIT

    GOSUB INITIALISE
    GOSUB GET.LRF.POS

RETURN

INITIALISE:


    APP.NAME = 'FUNDS.TRANSFER'
    OFSFUNCT = 'I'
    PROCESS  = 'PROCESS'
    OFSVERSION = 'FUNDS.TRANSFER,REDO'
    OFS.SOURCE.ID = 'REDO.OFS.STATUS.UPD'

    FN.FTTC = 'F.FT.TXN.TYPE.CONDITION'
    F.FTTC = ''
    CALL OPF(FN.FTTC,F.FTTC)

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    FN.REDO.W.DIRECT.DEBIT = 'F.REDO.W.DIRECT.DEBIT'
    F.REDO.W.DIRECT.DEBIT = ''
    CALL OPF(FN.REDO.W.DIRECT.DEBIT,F.REDO.W.DIRECT.DEBIT)

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)


    FN.AA.BILL.DETAILS = 'F.AA.BILL.DETAILS'
    F.AA.BILL.DETAILS = ''
    CALL OPF(FN.AA.BILL.DETAILS,F.AA.BILL.DETAILS)

    FN.AA.ARRANGEMENT  = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
    R.REDO.W.DIRECT.DEBIT.BK = ''
    Y.ID = 'TODAY'
    CALL F.READ(FN.REDO.W.DIRECT.DEBIT,Y.ID,R.REDO.W.DIRECT.DEBIT,F.REDO.W.DIRECT.DEBIT,Y.ERR)
    CALL CACHE.READ(FN.REDO.W.DIRECT.DEBIT,'SYSTEM',R.REDO.W.DIRECT.DEBIT.BK,Y.ERR.DR)

RETURN

*-----------
GET.LRF.POS:
*-----------
*----------------------------------------------------------------------
* This section gets the position of the local reference field positions
*----------------------------------------------------------------------

    LR.APP = 'AA.PRD.DES.OVERDUE':@FM:'ACCOUNT':@FM:'AA.PRD.DES.PAYMENT.SCHEDULE'
    LR.FLDS = 'L.LOAN.STATUS.1':@VM:'L.LOAN.COND':@FM
    LR.FLDS := 'L.AC.STATUS2':@VM:'L.AC.AV.BAL':@VM:'L.AC.TRAN.AVAIL':@FM
    LR.FLDS := 'L.AA.PAY.METHD':@VM:'L.AA.DEBT.AC'
    LR.POS = ''
    CALL MULTI.GET.LOC.REF(LR.APP,LR.FLDS,LR.POS)

    OD.LOAN.STATUS.POS = LR.POS<1,1>
    OD.LOAN.COND.POS   = LR.POS<1,2>
    POS.STATUS.2       = LR.POS<2,1>
    POS.AVL.BAL        = LR.POS<2,2>
    POS.TRANS.AMT      = LR.POS<2,3>
    PAYMT.METHOD.POS   = LR.POS<3,1>
    DEBIT.ACCT.POS     = LR.POS<3,2>


RETURN
END
