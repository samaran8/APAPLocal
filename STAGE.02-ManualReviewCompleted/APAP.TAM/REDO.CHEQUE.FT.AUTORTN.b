* @ValidationCode : Mjo0OTczMjkzOTpDcDEyNTI6MTY4MDY4OTEyMzU2NTozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:35:23
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
SUBROUTINE REDO.CHEQUE.FT.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.CHEQUE.FT.AUTORTN
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.TELLER.PROCESS to
* default the value for the TELLER application from REDO.TELLER.PROCESS
* It is AUTOM NEW CONTENT routine

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.CHEQUE.FT.AUTORTN
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*28.07.2011  JEEVA T         N.11             INITIAL CREATION
*14-SEP-2011 JEEVA T         N.11             PACS00126456
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*05/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION         FM TO @FM, VM TO @VM, SM TO @SM
*05/04/2023         SURESH           MANUAL R22 CODE CONVERSION        NOCHANGE
*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.CHEQUE.PROCESS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_System
    $INSERT I_F.CUSTOMER
    $INSERT I_F.RELATION
    $INSERT I_REDO.TELLER.PROCESS.COMMON

    GOSUB INIT
    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------

    FN.REDO.CHEQUE.PROCESS = 'F.REDO.CHEQUE.PROCESS'
    F.REDO.CHEQUE.PROCESS = ''
    CALL OPF(FN.REDO.CHEQUE.PROCESS,F.REDO.CHEQUE.PROCESS)

    FN.ACCOUNT= 'F.ACCOUNT'
    F.ACCOUNT= ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.ACCOUNT= 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT= ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.RELATION = 'F.RELATION'
    F.RELATION = ''
    CALL OPF(FN.RELATION,F.RELATION)

    LOC.REF.APPLICATION="FUNDS.TRANSFER":@FM:'CUSTOMER':@FM:'AZ.ACCOUNT'
    LOC.REF.FIELDS='L.FT.AZ.ACC.REF':@VM:'L.FT.REINV.AMT':@VM:'L.FT.ORG.DEPST':@VM:'BENEFIC.NAME':@VM:'L.1ST.NAME':@VM:'L.COMMENTS':@VM:'INTEREST.AMOUNT':@FM:'L.CU.TIPO.CL':@FM:'BENEFIC.NAME'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    L.FT.AZ.ACC.REF.POS = LOC.REF.POS<1,1>
    L.FT.REINV.AMT.POS = LOC.REF.POS<1,2>
    L.FT.ORG.DEPST.POS = LOC.REF.POS<1,3>
    POS.BENEFIC.NAME   = LOC.REF.POS<1,4>
    POS.L.1ST.NAME     = LOC.REF.POS<1,5>
    LOC.COMMENTS       = LOC.REF.POS<1,6>
    POS.INT.AMT        = LOC.REF.POS<1,7>
    POS.TIPO.CUS       = LOC.REF.POS<2,1>
    AZ.LOCAL.REF.POS   = LOC.REF.POS<3,1>

RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    Y.REDO.CHEQUE.PROCESS.ID = VAR.PROCESS.ID
    CALL F.READ(FN.REDO.CHEQUE.PROCESS,Y.REDO.CHEQUE.PROCESS.ID,R.REDO.CHEQUE.PROCESS,F.REDO.CHEQUE.PROCESS,Y.ERR)
    Y.ACCOUNT = R.REDO.CHEQUE.PROCESS<CHQ.PRO.AZ.ACCOUNT>
    Y.AMOUNT = R.REDO.CHEQUE.PROCESS<CHQ.PRO.DEBIT.AMOUNT>
    Y.CURR =   R.REDO.CHEQUE.PROCESS<CHQ.PRO.DEBIT.CUR>
    Y.ORG.DES = R.REDO.CHEQUE.PROCESS<CHQ.PRO.ORG.DEPST>
    Y.INT.AMT = R.REDO.CHEQUE.PROCESS<CHQ.PRO.INT.AMT>
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,Y.ERR.ACC)
    Y.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
    CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
    Y.NAME1 = R.CUSTOMER<EB.CUS.NAME.1>
    Y.NAME2 = R.CUSTOMER<EB.CUS.NAME.2>
    Y.JOINT.NAME = Y.NAME1:' ':Y.NAME2
    CALL F.READ(FN.AZ.ACCOUNT,Y.ACCOUNT,R.AZ.ACCOUNT,F.AZ.ACCOUNT,Y.AZ.ERR.ACC)
    Y.BEN.NAME1 = R.AZ.ACCOUNT<AZ.LOCAL.REF,AZ.LOCAL.REF.POS>
    CHANGE @SM TO '' IN Y.BEN.NAME1
    CHANGE @VM TO '' IN Y.BEN.NAME1

*-------------------------PACS00126456 Starts---------------------------------------------
    R.NEW(FT.LOCAL.REF)<1,POS.BENEFIC.NAME,1> = Y.BEN.NAME1[1,65]
    R.NEW(FT.LOCAL.REF)<1,POS.BENEFIC.NAME,2> = Y.BEN.NAME1[66,65]
*-------------------------PACS00126456 Ends---------------------------------------------
    R.NEW(FT.LOCAL.REF)<1,L.FT.AZ.ACC.REF.POS> = Y.ACCOUNT
*R.NEW(FT.DEBIT.AMOUNT) = Y.AMOUNT
    R.NEW(FT.DEBIT.ACCT.NO) = R.ACCOUNT<AC.INTEREST.LIQU.ACCT>
    R.NEW(FT.DEBIT.CURRENCY) = Y.CURR
    R.NEW(FT.CREDIT.CURRENCY) = Y.CURR
    R.NEW(FT.LOCAL.REF)<1,L.FT.REINV.AMT.POS>= Y.INT.AMT
    R.NEW(FT.LOCAL.REF)<1,L.FT.ORG.DEPST.POS>= Y.ORG.DES
    BEGIN CASE
        CASE PGM.VERSION EQ ',REINV.WDL'
            R.NEW(FT.DEBIT.AMOUNT) = Y.AMOUNT
            R.NEW(FT.LOCAL.REF)<1,LOC.COMMENTS> = VAR.PROCESS.ID:'FT'
        CASE PGM.VERSION EQ ',CHQ.OTHERS.DEPOSIT'
            R.NEW(FT.LOCAL.REF)<1,POS.INT.AMT> = Y.AMOUNT
            R.NEW(FT.LOCAL.REF)<1,LOC.COMMENTS> = VAR.PROCESS.ID:'NG'
        CASE PGM.VERSION EQ ',CHQ.NO.TAX.DEPOSIT'
            R.NEW(FT.DEBIT.AMOUNT) = Y.AMOUNT
            R.NEW(FT.LOCAL.REF)<1,LOC.COMMENTS> = VAR.PROCESS.ID:'GO'
        CASE PGM.VERSION EQ ',CHQ.GOVT.WITH.TAX'
            R.NEW(FT.LOCAL.REF)<1,POS.INT.AMT> =  Y.AMOUNT
            R.NEW(FT.LOCAL.REF)<1,LOC.COMMENTS> = VAR.PROCESS.ID:'GT'
    END CASE

RETURN
END
