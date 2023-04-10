* @ValidationCode : Mjo4ODgyNzUzMTM6Q3AxMjUyOjE2ODA3NjM2OTUwNjM6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Apr 2023 12:18:15
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.NAB.ACCT.HOL.LOAD
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 04-APR-2023     Conversion tool   R22 Auto conversion         VM to @VM
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_REDO.B.NAB.ACCT.HOL.COMMON


    FN.AA.BILL = 'F.AA.BILL.DETAILS'
    F.AA.BILL = ''
    CALL OPF(FN.AA.BILL,F.AA.BILL)

    FN.AA.SCH = 'F.AA.SCHEDULED.ACTIVITY'
    F.AA.SCH = ''
    CALL OPF(FN.AA.SCH,F.AA.SCH)

    FN.REDO.AA.NAB.HISTORY = 'F.REDO.AA.NAB.HISTORY'
    F.REDO.AA.NAB.HISTORY = ''
    CALL OPF(FN.REDO.AA.NAB.HISTORY,F.REDO.AA.NAB.HISTORY)

    FN.AA.AC = 'F.AA.ACCOUNT.DETAILS'
    F.AA.AC = ''
    CALL OPF(FN.AA.AC,F.AA.AC)

    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    CALL OPF(FN.AC,F.AC)

    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
    CALL OPF(FN.AA.ARR,F.AA.ARR)

    POS.ST = ''
    APPL = 'ACCOUNT'
    AL.FIELDS = 'L.OD.STATUS':@VM:'L.OD.STATUS.2':@VM:'L.LOAN.STATUS.POS'
    CALL MULTI.GET.LOC.REF(APPL,AL.FIELDS,POS.ST)
    Y.OD1.ST = POS.ST<1,1>
    Y.OD2.ST = POS.ST<1,2>
    Y.LN.ST = POS.ST<1,3>

RETURN

END
