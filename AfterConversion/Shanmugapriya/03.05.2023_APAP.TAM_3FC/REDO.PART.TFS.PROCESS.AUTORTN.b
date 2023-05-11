* @ValidationCode : MjotMTUzNDkzMjcxNTpDcDEyNTI6MTY4MzA4MTcwMjY3MTpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 03 May 2023 08:11:42
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

SUBROUTINE REDO.PART.TFS.PROCESS.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.PART.TFS.PROCESS.AUTORTN
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.TFS.REJECT to
* default the value for the REDO.TFS.PROCESS application from REDO.TFS.REJECT
* It is AUTOM NEW CONTENT routine

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.TFS.REJECT
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*08.07.2010  S SUDHARSANAN   ODR-2010-08-0017   INITIAL CREATION
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*12/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             VM TO @VM
*12/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.PART.TFS.REJECT
    $INSERT I_F.REDO.PART.TFS.PROCESS
    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------


    FN.REDO.PART.TFS.REJECT = 'F.REDO.PART.TFS.REJECT'
    F.REDO.PART.TFS.REJECT = ''
    CALL OPF(FN.REDO.PART.TFS.REJECT,F.REDO.PART.TFS.REJECT)
RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    Y.DATA = ""
    CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.REDO.PART.TFS.REJECT.ID=FIELD(Y.DATA,"*",2)
    CALL F.READ(FN.REDO.PART.TFS.REJECT,Y.REDO.PART.TFS.REJECT.ID,R.REDO.PART.TFS.REJECT,F.REDO.PART.TFS.REJECT,REJ.ERR)
    R.NEW(PAY.PART.TFS.ARRANGEMENT.ID) = R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.ARRANGEMENT.ID>

    Y.CNT=DCOUNT(R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.CURRENCY>,@VM)
    FOR Y.COUNT=1 TO Y.CNT
        R.NEW(PAY.PART.TFS.TRAN.TYPE)<1,Y.COUNT>=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.TRAN.TYPE,Y.COUNT>
        R.NEW(PAY.PART.TFS.CURRENCY)<1,Y.COUNT>=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.CURRENCY,Y.COUNT>
        R.NEW(PAY.PART.TFS.AMOUNT)<1,Y.COUNT>=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.AMOUNT,Y.COUNT>
        R.NEW(PAY.PART.TFS.ACCOUNT.NUMBER)<1,Y.COUNT>=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.ACCOUNT.NUMBER,Y.COUNT>
    NEXT Y.COUNT

    R.NEW(PAY.PART.TFS.VALUE.DATE)=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.VALUE.DATE>
    R.NEW(PAY.PART.TFS.NO.OF.OD.BILLS)=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.NO.OF.OD.BILLS>
    R.NEW(PAY.PART.TFS.TOT.AMT.OVRDUE)=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.TOT.AMT.OVRDUE>
    R.NEW(PAY.PART.TFS.REMARKS)=R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.REMARKS>
    Y.LOAN.STAT = R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.LOAN.STATUS.1>
    Y.LOAN.STAT.CNT = DCOUNT(Y.LOAN.STAT,@VM)
    FOR STAT = 1 TO Y.LOAN.STAT.CNT
        R.NEW(PAY.PART.TFS.LOAN.STATUS.1)<1,STAT> = Y.LOAN.STAT<1,STAT>
    NEXT STAT
    Y.LOAN.COND = R.REDO.PART.TFS.REJECT<PAY.PART.TFS.REJ.LOAN.CONDITION>
    Y.LOAN.COND.CNT = DCOUNT(Y.LOAN.COND,@VM)
    FOR COND = 1 TO Y.LOAN.COND.CNT
        R.NEW(PAY.PART.TFS.LOAN.CONDITION) = Y.LOAN.COND<1,COND>
    NEXT COND
RETURN
*------------------------------------------------------------------------------
END
