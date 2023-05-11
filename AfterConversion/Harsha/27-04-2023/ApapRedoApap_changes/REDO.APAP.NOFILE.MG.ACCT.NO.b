* @ValidationCode : MjotMTExNDY4MTE4MTpDcDEyNTI6MTY4MjU3MjQ1MjUwNDpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 27 Apr 2023 10:44:12
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
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.NOFILE.MG.ACCT.NO(Y.ENQ.OUT)
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.APAP.NOFILE.MG.ACCT.NO
* ODR NO      : ODR-2009-10-0346
*----------------------------------------------------------------------
*DESCRIPTION: REDO.APAP.NOFILE.MG.ACCT.NO is a no-file enquiry routine used
* to fetch and return details from local template REDO.APAP.MORTGAGES.DETAIL
* whose maturity date is at least N  times the tenor of deposit; where N  is
* defined in REDO.APAP.CPH.PARAMETER and whose loan status is of allowed status
* in REDO.APAP.CPH.PARAMETER.  It also returns OUTSTANDING AMOUNT of loan as on
* deposit maturity date and deposit MATURITY AMOUNT




*IN PARAMETER: NA
*OUT PARAMETER: Y.ENQ.OUT.OUT
*LINKED WITH: REDO.APAP.ENQ.MG.ACCT.NO

*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*27.07.2010  H GANESH        ODR-2009-10-0346  INITIAL CREATION
*21-07-2011   JEEVAT         PACS00038165
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*18-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  ++ to += , VM to @VM , IF STATEMENT ADDED
*18-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------




*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.APAP.CPH.PARAMETER
    $INSERT I_F.REDO.APAP.MORTGAGES.DETAIL
	$USING APAP.TAM


    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------
    Y.ENQ.OUT=''
    Y.MATURE.AMT =0
RETURN
*----------------------------------------------------------------------
OPENFILES:
*----------------------------------------------------------------------
    FN.REDO.APAP.CPH.PARAMETER='F.REDO.APAP.CPH.PARAMETER'

    FN.REDO.APAP.MORTGAGES.DETAIL='F.REDO.APAP.MORTGAGES.DETAIL'
    F.REDO.APAP.MORTGAGES.DETAIL=''

    CALL OPF(FN.REDO.APAP.MORTGAGES.DETAIL,F.REDO.APAP.MORTGAGES.DETAIL)

    FN.ACCOUNT ='F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    LOCATE 'LOAN.NUM' IN D.FIELDS SETTING POS1 THEN
        Y.ID =D.RANGE.AND.VALUE<POS1>
    END

    GOSUB GET.SEL.REC
RETURN

*----------------------------------------------------------------------
GET.SEL.REC:
*----------------------------------------------------------------------


    CALL F.READ(FN.REDO.APAP.MORTGAGES.DETAIL,Y.ID,R.REDO.APAP.MORTGAGES.DETAIL,F.REDO.APAP.MORTGAGES.DETAIL,MRTG.ERR)
    Y.LOAN.STATUS = R.REDO.APAP.MORTGAGES.DETAIL<MG.DET.STATUS>
    Y.OUT.PRINC= R.REDO.APAP.MORTGAGES.DETAIL<MG.DET.OUTS.PRINCIPLE>
    Y.BAL.AVAIL =R.REDO.APAP.MORTGAGES.DETAIL<MG.DET.BAL.PRINCIPAL>
    GOSUB GET.LOAN.OUT.AMT
    GOSUB GET.LOAN.MAT.AMT
    GOSUB GET.DIFF.AMT
    CALL F.READ(FN.ACCOUNT,Y.ID,R.ACCOUNT,F.ACCOUNT,Y.AC.ERR)
    Y.ACCOUNT.NAME = R.ACCOUNT<AC.ACCOUNT.TITLE.1>
    Y.ENQ.OUT=Y.ID:"*":Y.ACCOUNT.NAME:'*':Y.LOAN.STATUS:'*':Y.OUT.PRINC:'*':Y.BAL.AVAIL:'*':LOAN.OUT.AMT:'*':Y.MATURE.AMT:'*':Y.DIFF.PREC


RETURN
*----------------------------------------------------------------------
GET.LOAN.OUT.AMT:
*----------------------------------------------------------------------

    IN.ACC.ID=Y.ID
    IN.ARR.ID=''
    CALL APAP.TAM.redoConvertAccount(IN.ACC.ID,IN.ARR.ID,OUT.ID,ERR.TEXT)
    Y.ARR.ID=OUT.ID
    Y.MAT.DATE = System.getVariable("CURRENT.MAT.DATE")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;* R22 AUTO CODE CONVERSION-START
        Y.MAT.DATE = ""
    END ;*R22 AUTO CODE CONVERSION - END
    Y.TEMP.ENQ.SELECTION = ENQ.SELECTION
    CALL APAP.REDOAPAP.REDO.APAP.GET.OUTSTANDING.AMT(Y.MAT.DATE,Y.ARR.ID,Y.AMT)
    ENQ.SELECTION = Y.TEMP.ENQ.SELECTION

    LOAN.OUT.AMT =Y.AMT

RETURN
*----------------------------------------------------------------------
GET.LOAN.MAT.AMT:
*----------------------------------------------------------------------

    Y.DEP.NO=R.REDO.APAP.MORTGAGES.DETAIL<MG.DET.DEP.ACT.NO>
    Y.DEP.NO.CNT=DCOUNT(Y.DEP.NO,@VM)
    Y.MATURE.AMT=0
    VAR3=1
    LOOP
    WHILE VAR3 LE Y.DEP.NO.CNT
        CALL APAP.REDOAPAP.REDO.APAP.GET.MATURITY.AMT(Y.DEP.NO<1,VAR3>,MATURE.AMT)
        Y.MATURE.AMT+=MATURE.AMT
        VAR3 += 1 ;*R22 AUTO CODE CONVERSION
    REPEAT

RETURN
*----------------------------------------------------------------------
GET.DIFF.AMT:
*----------------------------------------------------------------------

    IF Y.MATURE.AMT EQ 0 THEN
        Y.DIFF.PREC=0
        RETURN
    END
    Y.DIFFERENCE.AMOUNT=(LOAN.OUT.AMT-Y.MATURE.AMT)/Y.MATURE.AMT
    Y.DIFF.PREC =Y.DIFFERENCE.AMOUNT*100
RETURN

END
