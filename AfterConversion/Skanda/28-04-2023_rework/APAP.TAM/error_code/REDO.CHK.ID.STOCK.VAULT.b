* @ValidationCode : MjoyMDM3MDg1MDU0OkNwMTI1MjoxNjgwNjcxNzU3ODIwOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 10:45:57
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
SUBROUTINE REDO.CHK.ID.STOCK.VAULT
***********************************************************
*----------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : KAVITHA
* PROGRAM NAME : REDO.CHK.ID.STOCK.VAULT
*----------------------------------------------------------


* DESCRIPTION : This routine is to default stock values for Vault
*------------------------------------------------------------

*    IN PARAMETER: NONE
*    OUT PARAMETER: NONE

*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                    REFERENCE         DESCRIPTION
*20-1-2011       KAVITHA                 ODR-2010-03-0400  INITIAL CREATION
** 05-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 05-04-2023 Skanda R22 Manual Conversion - line no 68
*----------------------------------------------------------------------


*-------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STOCK.ENTRY
    $INSERT I_F.REDO.CARD.REQUEST
    $INSERT I_F.REDO.CARD.SERIES.PARAM
    $INSERT I_GTS.COMMON
    $INSERT I_System
    $INSERT I_F.COMPANY

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*-------------------------------------------------------------
INIT:
*Initialising
*-------------------------------------------------------------
    AGENCY.POS= ''
    R.STOCK.ENTRY = ''
    LOC.REF.APPLICATION = ''
    LOC.REF.FIELDS=''
    BATCH.POS = ''

    LOC.REF.APPLICATION='STOCK.ENTRY'
    LOC.REF.FIELDS='L.SE.AGENCY':@VM:'L.SE.BATCH.NO'
    LOC.REF.POS=''

    CALL APAP.TAM.MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS) ;* R22 Manual Conversion
    AGENCY.POS = LOC.REF.POS<1,1>
    BATCH.POS = LOC.REF.POS<1,2>

RETURN

*-------------------------------------------------------------
OPENFILES:
*Opening File

    FN.STOCK.ENTRY = 'F.STOCK.ENTRY'
    F.STOCK.ENTRY = ''
    CALL OPF(FN.STOCK.ENTRY,F.STOCK.ENTRY)

    FN.REDO.CARD.REQ = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQ = ''
    CALL OPF(FN.REDO.CARD.REQ,F.REDO.CARD.REQ)

    FN.REDO.SER.PARAM = 'F.REDO.CARD.SERIES.PARAM'
    F.REDO.SER.PARAM = ''
    CALL OPF(FN.REDO.SER.PARAM,F.REDO.SER.PARAM)

    FN.COMP = 'F.COMPANY'
    F.COMP= ''
    CALL OPF(FN.COMP,F.COMP)

RETURN
*-------------------------------------------------------------
PROCESS:

*  CALL F.READ(FN.REDO.SER.PARAM,'SYSTEM',R.REDO.SER.PARAM,F.REDO.SER.PARAM,SER.ERR) ;*Tus Start
    CALL CACHE.READ(FN.REDO.SER.PARAM,'SYSTEM',R.REDO.SER.PARAM,SER.ERR) ; * Tus End
    IF R.REDO.SER.PARAM THEN
        EMBOSS.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.EMBOSS.DEPT.CODE>
        RECEIVE.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.RECEIVE.DEPT.CODE>
        VAULT.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.VAULT.DEPT.CODE>
    END

    Y.DEFAULT.BATCH = System.getVariable('CURRENT.BATCH')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        Y.DEFAULT.BATCH = ""
    END
    Y.DEFAULT.START.NO =System.getVariable('CURRENT.START.NO')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        Y.DEFAULT.START.NO = ""
    END
    Y.DEFAULT.SERIES = System.getVariable('CURRENT.SERIES')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        Y.DEFAULT.SERIES = ""
    END
    Y.DEFAULT.BRANCH = System.getVariable('CURRENT.BRANCH')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        Y.DEFAULT.BRANCH = ""
    END

    CALL CACHE.READ(FN.COMP, Y.DEFAULT.BRANCH, R.COMP, Y.COMP.ERR) ;* R22 Auto conversion

    FINAL.COMP = R.COMP<EB.COM.FINANCIAL.COM>
    Y.SERIES.COUNT = DCOUNT(Y.DEFAULT.SERIES,@VM)
    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.SERIES.COUNT
        R.NEW(STO.ENT.FROM.REGISTER)<1,Y.CNT> = 'CARD.':Y.DEFAULT.BRANCH:'-':RECEIVE.DEPT.CODE
        R.NEW(STO.ENT.TO.REGISTER)<1,Y.CNT> = 'CARD.':FINAL.COMP:'-':VAULT.DEPT.CODE
        R.NEW(STO.ENT.LOCAL.REF)<1,BATCH.POS> = Y.DEFAULT.BATCH
        R.NEW(STO.ENT.STOCK.SERIES)<1,Y.CNT> = Y.DEFAULT.SERIES<1,Y.CNT>
        R.NEW(STO.ENT.STOCK.START.NO)<1,Y.CNT> = Y.DEFAULT.START.NO<1,Y.CNT>
        R.NEW(STO.ENT.NOTES)<1,Y.CNT,1> = "RETURNED TO VAULT"
        Y.CNT += 1 ;* R22 Auto conversion
    REPEAT
RETURN
END
