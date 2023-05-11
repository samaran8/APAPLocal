* @ValidationCode : MjotNjQ1MTIyMDk3OkNwMTI1MjoxNjgwNjk3Nzg2MjA4OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 17:59:46
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
SUBROUTINE REDO.CHEQUE.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: JEEVA T
* PROGRAM NAME: REDO.CHEQUE.AUTORTN
* ODR NO      : ODR-2009-10-0322
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.CHEQUE.PROCESS to
* default the value for the TELLER application from REDO.CHEQUE.PROCESS
* It is AUTOM NEW CONTENT routine

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.CHEQUE.PROCESS
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*23-AUG-2011 JEEVA T         N.11            INITIAL CREATION

* Date             Who                   Reference      Description
* 05.04.2023       Conversion Tool       R22            Auto Conversion     - VM TO @VM, SM TO @SM, FM TO @FM, New condition added
* 05.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.CHEQUE.PROCESS
    $INSERT I_F.TELLER
    $INSERT I_System
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT

    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------


    FN.REDO.CHEQUE.PROCESS = 'F.REDO.CHEQUE.PROCESS'
    F.REDO.CHEQUE.PROCESS = ''
    CALL OPF(FN.REDO.CHEQUE.PROCESS,F.REDO.CHEQUE.PROCESS)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    LOC.REF.APPLICATION="TELLER":@FM:'AZ.ACCOUNT'
    LOC.REF.FIELDS='L.TT.AZ.ACC.REF':@VM:'L.TT.REINV.AMT':@VM:'L.TT.ORG.DEPST':@VM:'L.1ST.NAME':@VM:'L.2ND.NAME':@VM:'L.LAST.NAME':@VM:'L.2ND.LAST.NAME':@VM:'L.TT.BENEFICIAR':@VM:'L.COMMENTS':@VM:'L.CREDIT.AMOUNT':@FM:'BENEFIC.NAME'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.TT.AZ.ACC.REF =LOC.REF.POS<1,1>
    Y.L.REIVSD.INT.POS     = LOC.REF.POS<1,2>
    Y.L.ORG.DP.AMT.POS     = LOC.REF.POS<1,3>
    Y.L.1ST.NAME.POS       = LOC.REF.POS<1,4>
    Y.L.2ND.NAME.POS       = LOC.REF.POS<1,5>
    Y.L.LAST.NAME.POS      = LOC.REF.POS<1,6>
    Y.L.2ND.LAST.NAME.POS  = LOC.REF.POS<1,7>
    Y.L.TT.BENEFICIAR.POS  = LOC.REF.POS<1,8>
    LOC.COMMENTS.POS       = LOC.REF.POS<1,9>
    LOC.CREDIT.AMOUNT      = LOC.REF.POS<1,10>
    AZ.LOCAL.REF.POS       = LOC.REF.POS<2,1>
RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    R.REDO.CHEQUE.PROCESS = ''
    Y.DATA = ""
*CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.DATA = System.getVariable("CURRENT.ID")
  
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN   ;* R22 Auto Conversion - Start
        Y.DATA = ""
    END                                  ;* R22 Auto Conversion - Start
    Y.REDO.CHEQUE.PROCESS.ID=FIELD(Y.DATA,"*",1)
    CALL F.READ(FN.REDO.CHEQUE.PROCESS,Y.REDO.CHEQUE.PROCESS.ID,R.REDO.CHEQUE.PROCESS,F.REDO.CHEQUE.PROCESS,Y.ERR)
    IF R.REDO.CHEQUE.PROCESS THEN
        Y.ACCOUNT = R.REDO.CHEQUE.PROCESS<CHQ.PRO.AZ.ACCOUNT>
        Y.AMOUNT = R.REDO.CHEQUE.PROCESS<CHQ.PRO.DEBIT.AMOUNT>
        Y.CURR = R.REDO.CHEQUE.PROCESS<CHQ.PRO.DEBIT.CUR>
        R.NEW(TT.TE.AMOUNT.LOCAL.1) = Y.AMOUNT
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,Y.ERR.ACC)
        Y.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
        CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
        Y.NAME1 = R.CUSTOMER<EB.CUS.NAME.1>
        Y.NAME2 = R.CUSTOMER<EB.CUS.NAME.2>
        CALL F.READ(FN.AZ.ACCOUNT,Y.ACCOUNT,R.AZ.ACCOUNT,F.AZ.ACCOUNT,Y.AZ.ERR.ACC)
        Y.BEN.NAME1 = R.AZ.ACCOUNT<AZ.LOCAL.REF,AZ.LOCAL.REF.POS>
        CHANGE @SM TO '' IN Y.BEN.NAME1
        CHANGE @VM TO '' IN Y.BEN.NAME1
        R.NEW(TT.TE.CURRENCY.2) = Y.CURR
        R.NEW(TT.TE.LOCAL.REF)<1,Y.L.TT.BENEFICIAR.POS,1> = Y.BEN.NAME1[1,65]
        R.NEW(TT.TE.LOCAL.REF)<1,Y.L.TT.BENEFICIAR.POS,2> = Y.BEN.NAME1[66,65]
        R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.AZ.ACC.REF> = Y.ACCOUNT
        R.NEW(TT.TE.LOCAL.REF)<1,Y.L.REIVSD.INT.POS>  = R.REDO.CHEQUE.PROCESS<CHQ.PRO.INT.AMT>
        R.NEW(TT.TE.LOCAL.REF)<1,Y.L.ORG.DP.AMT.POS> = R.REDO.CHEQUE.PROCESS<CHQ.PRO.ORG.DEPST>
        R.NEW(TT.TE.LOCAL.REF)<1,LOC.COMMENTS.POS>   = Y.REDO.CHEQUE.PROCESS.ID:'TT'
        R.NEW(TT.TE.LOCAL.REF)<1,LOC.CREDIT.AMOUNT>  = Y.AMOUNT
    END
RETURN
END
