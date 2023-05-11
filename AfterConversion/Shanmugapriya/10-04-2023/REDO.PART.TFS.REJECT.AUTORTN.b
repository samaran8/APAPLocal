* @ValidationCode : MjotMjM0OTYzNDIzOkNwMTI1MjoxNjgwODg4MzAxNjA0OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 07 Apr 2023 22:55:01
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
SUBROUTINE REDO.PART.TFS.REJECT.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.PART.TFS.REJECT.AUTORTN
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.PART.TFS.REJECT to
* default the value for the REDO.PART.TFS.REJECT application from REDO.PART.TFS.PROCESS
* It is AUTOM NEW CONTENT routine

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.PART.TFS.PROCESS
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*08.07.2010  S SUDHARSANAN     B.12           INITIAL CREATION
*10.04.2023  Conversion Tool   R22            Auto Conversion     - VM TO @VM
*10.04.2023  Shanmugapriya M   R22            Manual Conversion   - No changes
*
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


    FN.REDO.PART.TFS.PROCESS = 'F.REDO.PART.TFS.PROCESS'
    F.REDO.PART.TFS.PROCESS = ''
    CALL OPF(FN.REDO.PART.TFS.PROCESS,F.REDO.PART.TFS.PROCESS)
RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    Y.DATA = ""
    CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.REDO.PART.TFS.PROCESS.ID=FIELD(Y.DATA,"*",2)
    CALL F.READ(FN.REDO.PART.TFS.PROCESS,Y.REDO.PART.TFS.PROCESS.ID,R.REDO.PART.TFS.PROCESS,F.REDO.PART.TFS.PROCESS,PRO.ERR)
    R.NEW(PAY.PART.TFS.REJ.ARRANGEMENT.ID) = R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.ARRANGEMENT.ID>

    Y.CNT=DCOUNT(R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.CURRENCY>,@VM)
    FOR Y.COUNT=1 TO Y.CNT
        R.NEW(PAY.PART.TFS.REJ.TRAN.TYPE)<1,Y.COUNT>=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.TRAN.TYPE,Y.COUNT>
        R.NEW(PAY.PART.TFS.REJ.CURRENCY)<1,Y.COUNT>=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.CURRENCY,Y.COUNT>
        R.NEW(PAY.PART.TFS.REJ.AMOUNT)<1,Y.COUNT>=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.AMOUNT,Y.COUNT>
        R.NEW(PAY.PART.TFS.REJ.ACCOUNT.NUMBER)<1,Y.COUNT>=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.ACCOUNT.NUMBER,Y.COUNT>
    NEXT Y.COUNT

    R.NEW(PAY.PART.TFS.REJ.VALUE.DATE)=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.VALUE.DATE>
    R.NEW(PAY.PART.TFS.REJ.NO.OF.OD.BILLS)=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.NO.OF.OD.BILLS>
    R.NEW(PAY.PART.TFS.REJ.TOT.AMT.OVRDUE)=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.TOT.AMT.OVRDUE>
    R.NEW(PAY.PART.TFS.REJ.REMARKS)=R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.REMARKS>

    Y.LOAN.STAT = R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.LOAN.STATUS.1>
    Y.LOAN.STAT.CNT = DCOUNT(Y.LOAN.STAT,@VM)
    FOR STAT = 1 TO Y.LOAN.STAT.CNT
        R.NEW(PAY.PART.TFS.REJ.LOAN.STATUS.1)<1,STAT> = Y.LOAN.STAT<1,STAT>
    NEXT STAT

    Y.LOAN.COND = R.REDO.PART.TFS.PROCESS<PAY.PART.TFS.LOAN.CONDITION>
    Y.LOAN.COND.CNT = DCOUNT(Y.LOAN.COND,@VM)
    FOR COND = 1 TO Y.LOAN.COND.CNT
        R.NEW(PAY.PART.TFS.REJ.LOAN.CONDITION)<1,COND> = Y.LOAN.COND<1,COND>
    NEXT COND
RETURN
*------------------------------------------------------------------------------
END
