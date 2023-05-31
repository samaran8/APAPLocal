* @ValidationCode : MjotMzYzMjg0ODIzOkNwMTI1MjoxNjg0ODU0Mzg5OTcyOklUU1M6LTE6LTE6OTc4OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:29
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 978
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.LOAN.STATUS.ELIMINATION.LOAD
*-------------------------------------------------------------------------------------------------
*DESCRIPTION:
* This routine is the load routine of the batch job REDO.B.LOAN.STATUS.ELIMINATION
*  which updates the local reference fields LOAN.STATUS and LOAN.CONDITIN based on the conditions
* This routine Opens the necessary files and gets the position for the local reference fields
*
* ------------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who               Reference                   Description
* 03-JUN-2010   N.Satheesh Kumar     TAM-ODR-2009-10-0331          Initial Creation
* 09-Dec-2010   Krishna Murthy T.S   TAM-ODR-2009-10-1678(B.10)    Modified. Opening the file
*                                                                  REDO.LOAN.CHQ.RETURN
* Date                   who                   Reference              
* 12-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM
* 12-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.OVERDUE
    $INSERT I_LOAN.STATUS.ELIMINATION.COMMON
    $INSERT I_F.REDO.LOAN.CHQ.RETURN
    $INSERT I_F.REDO.APAP.CLEAR.PARAM

    GOSUB OPEN.FILES
    GOSUB GET.LRF.POS

RETURN

*----------
OPEN.FILES:
*----------
*---------------------------------------
* This section opens the necessary files
*---------------------------------------

    FN.AA.ARR.OVERDUE = 'F.AA.ARR.OVERDUE'
    F.AA.ARR.OVERDUE = ''
    CALL OPF(FN.AA.ARR.OVERDUE,F.AA.ARR.OVERDUE)

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    FN.AA.BILL.DETAILS = 'F.AA.BILL.DETAILS'
    F.AA.BILL.DETAILS = ''
    CALL OPF(FN.AA.BILL.DETAILS,F.AA.BILL.DETAILS)

    FN.REDO.APAP.LOAN.CHEQUE.DETAILS = 'F.REDO.APAP.LOAN.CHEQUE.DETAILS'
    F.REDO.APAP.LOAN.CHEQUE.DETAILS = ''
    CALL OPF(FN.REDO.APAP.LOAN.CHEQUE.DETAILS,F.REDO.APAP.LOAN.CHEQUE.DETAILS)

    FN.REDO.OFS.PARAM='F.REDO.OFS.PARAM'
    F.REDO.OFS.PARAM = ''
    CALL OPF(FN.REDO.OFS.PARAM,F.REDO.OFS.PARAM)

    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT=''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

*ODR2009101678-START.1

    FN.REDO.LOAN.CHQ.RETURN = 'F.REDO.LOAN.CHQ.RETURN'
    F.REDO.LOAN.CHQ.RETURN = ''
    CALL OPF(FN.REDO.LOAN.CHQ.RETURN,F.REDO.LOAN.CHQ.RETURN)

    FN.REDO.APAP.CLEAR.PARAM = 'F.REDO.APAP.CLEAR.PARAM'
    F.REDO.APAP.CLEAR.PARAM = ''

    FN.REDO.AA.LOAN.UPD.STATUS = 'F.REDO.AA.LOAN.UPD.STATUS'
    F.REDO.AA.LOAN.UPD.STATUS = ''


    FN.REDO.H.AA.DIS.CHG = 'F.REDO.H.AA.DIS.CHG'
    F.REDO.H.AA.DIS.CHG = ''
    CALL OPF(FN.REDO.H.AA.DIS.CHG,F.REDO.H.AA.DIS.CHG)

    CALL OPF(FN.REDO.AA.LOAN.UPD.STATUS,F.REDO.AA.LOAN.UPD.STATUS)

    FN.OFS = 'F.OFS.MESSAGE.QUEUE'
    F.OFS = ''
    CALL OPF(FN.OFS,F.OFS)

*ODR2009101678-END.1

RETURN

*-----------
GET.LRF.POS:
*-----------
*----------------------------------------------------------------------
* This section gets the position of the local reference field positions
*----------------------------------------------------------------------

    AA.OD.LRF.POS = ''
    AA.LOAN.STATUS.POS = ''
    AA.LOAN.COND.POS = ''
    AA.OD.LRF = 'L.LOAN.STATUS.1':@VM:'L.STATUS.CHG.DT':@VM:'L.LOAN.COMMENT':@VM
    AA.OD.LRF := 'L.LOAN.COND':@VM:'L.LOAN.COMMENT1':@FM:'L.MIGRATED.LN'

    Y.APPLN = 'AA.PRD.DES.OVERDUE':@FM:'AA.PRD.DES.PAYMENT.SCHEDULE'
    CALL MULTI.GET.LOC.REF(Y.APPLN,AA.OD.LRF,AA.OD.LRF.POS)
    AA.LOAN.STATUS.POS = AA.OD.LRF.POS<1,1>
    AA.LS.CHG.DTE.POS = AA.OD.LRF.POS<1,2>
    AA.LS.COMMENT.POS = AA.OD.LRF.POS<1,3>
    AA.LOAN.COND.POS = AA.OD.LRF.POS<1,4>
    AA.LC.COMMENT.POS = AA.OD.LRF.POS<1,5>
    AA.MIG.POS = AA.OD.LRF.POS<2,1>

    CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,'SYSTEM',R.REDO.APAP.CLEAR.PARAM,PARAM.ERR)
    CALL CACHE.READ(FN.REDO.H.AA.DIS.CHG,'SYSTEM',R.REDO.H.AA.DIS.CHG,PRE.ERR)
RETURN
END
