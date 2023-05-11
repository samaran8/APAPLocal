* @ValidationCode : MjoxMTU4MDMyMzc0OkNwMTI1MjoxNjgxMjk2OTg4MTAzOjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 16:26:28
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.PRE.PRIN.INT.PENAL.INT.LOAD
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This is COB routine for B16 development
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*---------------
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who              Reference            Description
* 02-JUL-2010    Kishore.SP       ODR-2009-10-0325      Initial Creation
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*12/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION           FM TO @FM, VM TO @VM
*12/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.COMPANY
    $INSERT I_F.REDO.RATE.CHANGE.CRIT
    $INSERT I_F.REDO.SUCESS.RATE.CHANGE.COB
    $INSERT I_REDO.PRE.PRIN.INT.PENAL.INT.COMMON
*-----------------------------------------------------------------------------
    GOSUB INITIALISE
RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*----------
* Open All the needed files
*

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
*
    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)
*
    FN.AA.ARRANGEMENT.ACTIVITY = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AA.ARRANGEMENT.ACTIVITY = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY,F.AA.ARRANGEMENT.ACTIVITY)
*
    FN.REDO.RATE.CHANGE.CRIT = 'F.REDO.RATE.CHANGE.CRIT'
    F.REDO.RATE.CHANGE.CRIT  = ''
    CALL OPF(FN.REDO.RATE.CHANGE.CRIT,F.REDO.RATE.CHANGE.CRIT)
*
    FN.AA.INTEREST.ACCRUALS = 'F.AA.INTEREST.ACCRUALS'
    F.AA.INTEREST.ACCRUALS = ''
    CALL OPF(FN.AA.INTEREST.ACCRUALS,F.AA.INTEREST.ACCRUALS)
*
    FN.AA.ARR.INTEREST = 'F.AA.ARR.INTEREST'
    F.AA.ARR.INTEREST = ''
    CALL OPF(FN.AA.ARR.INTEREST,F.AA.ARR.INTEREST)
*
    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS  = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)
*
    FN.REDO.SUCESS.RATE.CHANGE.COB = 'F.REDO.SUCESS.RATE.CHANGE.COB'
    F.REDO.SUCESS.RATE.CHANGE.COB   = ''
    CALL OPF(FN.REDO.SUCESS.RATE.CHANGE.COB,F.REDO.SUCESS.RATE.CHANGE.COB)

*
    FN.REDO.LOAN.MARGIN.RATE='F.REDO.LOAN.MARGIN.RATE'
    F.REDO.LOAN.MARGIN.RATE=''
    CALL OPF(FN.REDO.LOAN.MARGIN.RATE,F.REDO.LOAN.MARGIN.RATE)
*
    FN.REDO.MASSIVE.RATE.CHANGE = 'F.REDO.MASSIVE.RATE.CHANGE'
    F.REDO.MASSIVE.RATE.CHANGE = ''
    CALL OPF(FN.REDO.MASSIVE.RATE.CHANGE,F.REDO.MASSIVE.RATE.CHANGE)


    LOC.REF.APPL="AA.PRD.DES.INTEREST":@FM:"AA.PRD.DES.OVERDUE":@FM:"AA.PRD.DES.CUSTOMER"
    LOC.REF.FIELDS="L.AA.REV.FORM":@VM:"L.AA.REV.RT.TY":@VM:"L.AA.FIR.REV.DT":@VM:"L.AA.NXT.REV.DT":@VM:"L.AA.RT.RV.FREQ":@VM:"L.AA.POOL.RATE":@VM:"L.AA.LST.REV.DT":@FM:"L.LOAN.COND":@VM:"L.LOAN.STATUS.1":@FM:"L.AA.CAMP.TY":@VM:"L.AA.AFF.COM"
    LOC.REF.POS=""
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    Y.REV.FORM.POS       =  LOC.REF.POS<1,1>
    Y.REV.RT.TY.POS      =  LOC.REF.POS<1,2>
    Y.FST.REV.DT.POS     =  LOC.REF.POS<1,3>
    Y.SND.REV.DT.POS     =  LOC.REF.POS<1,4>
    Y.RATE.REV.FQU.POS   =  LOC.REF.POS<1,5>
    Y.POOL.RATE.POS      =  LOC.REF.POS<1,6>
    POS.L.AA.LST.REV.DT  =  LOC.REF.POS<1,7>
    Y.LOAN.COND.POS      =  LOC.REF.POS<2,1>
    Y.LOAN.STATUS.1.POS  =  LOC.REF.POS<2,2>
    Y.CAMP.TYPE.POS      =  LOC.REF.POS<3,1>
    Y.AFFLI.COMP.POS     =  LOC.REF.POS<3,2>
*
RETURN
END
