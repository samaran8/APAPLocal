* @ValidationCode : MjoxMDAzNjk5NTY5OkNwMTI1MjoxNjgzMDgxNzAyNzE1OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
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

SUBROUTINE REDO.PART.TT.PROCESS.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.PART.TT.PROCESS.AUTORTN
* ODR NO      : ODR-2009-11-0157
*----------------------------------------------------------------------
*DESCRIPTION: This is the  Routine for REDO.TELLER.REJECT to
* default the value for the REDO.TELLER.PROCESS application from REDO.TELLER.REJECT
* It is AUTOM NEW CONTENT routine

  

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.PART.TT.PROCESS
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
* DATE          WHO                REFERENCE       DESCRIPTION
* 30.06.2010    S SUDHARSANAN      B.12            INITIAL CREATION
* 10.08.2011    MARIMUTHU S        PACS00094144
* 01/03/2013    Vignesh Kumaar R   PACS00251031    Update the Principal and penality INT/CHG AMT
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*12/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             VM TO @VM
*12/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.PART.TT.PROCESS
    $INSERT I_F.REDO.PART.TT.REJECT

    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------

    FN.REDO.PART.TT.REJECT = 'F.REDO.PART.TT.REJECT'
    F.REDO.PART.TT.REJECT = ''
    CALL OPF(FN.REDO.PART.TT.REJECT,F.REDO.PART.TT.REJECT)

RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

    Y.DATA = ""
    CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.REDO.PART.TT.REJECT.ID=FIELD(Y.DATA,"*",2)
    CALL F.READ(FN.REDO.PART.TT.REJECT,Y.REDO.PART.TT.REJECT.ID,R.REDO.PART.TT.REJECT,F.REDO.PART.TT.REJECT,REJ.ERR)

    Y.AA.ID = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.ARRANGEMENT.ID>
    Y.CURRENCY= R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.CURRENCY>
    Y.TRAN.TYPE = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.TRAN.TYPE>
    Y.AMOUNT = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.AMOUNT>
    Y.VALUE.DATE = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.VALUE.DATE>
    Y.NOF.BILLS = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.NO.OF.OD.BILLS>
    Y.TOT.AMT.OVRDUE = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.TOT.AMT.OVRDUE>
    Y.REMARKS = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.REMARKS>
    Y.LOAN.STAT = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.LOAN.STATUS.1>
    Y.LOAN.STAT.CNT = DCOUNT(Y.LOAN.STAT,@VM)
    Y.LOAN.COND = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.LOAN.CONDITION>
    Y.LOAN.COND.CNT =  DCOUNT(Y.LOAN.COND,@VM)
    Y.NO.OF.INSTALL = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.NO.OF.INSTALLMENT>

    R.NEW(PAY.PART.TT.ARRANGEMENT.ID) = Y.AA.ID
    R.NEW(PAY.PART.TT.CURRENCY) = Y.CURRENCY
    R.NEW(PAY.PART.TT.TRAN.TYPE)=Y.TRAN.TYPE
    R.NEW(PAY.PART.TT.AMOUNT) = Y.AMOUNT
    R.NEW(PAY.PART.TT.VALUE.DATE) = Y.VALUE.DATE
    R.NEW(PAY.PART.TT.NO.OF.OD.BILLS) = Y.NOF.BILLS
    R.NEW(PAY.PART.TT.TOT.AMT.OVRDUE) = Y.TOT.AMT.OVRDUE
    R.NEW(PAY.PART.TT.REMARKS) = Y.REMARKS
    FOR STAT = 1 TO Y.LOAN.STAT.CNT
        R.NEW(PAY.PART.TT.LOAN.STATUS.1)<1,STAT> = Y.LOAN.STAT<1,STAT>
    NEXT STAT
    FOR COND = 1 TO Y.LOAN.COND.CNT
        R.NEW(PAY.PART.TT.LOAN.CONDITION)<1,COND> = Y.LOAN.COND<1,COND>
    NEXT COND
    R.NEW(PAY.PART.TT.NO.OF.INSTALLMENT) = Y.NO.OF.INSTALL

* Fix for PACS00251031 [Update the Principal and penality INT/CHG AMT]

    R.NEW(PAY.PART.TT.PRINCIPAL.INT) = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.PRINCIPAL.INT>
    R.NEW(PAY.PART.TT.CHARGE.AMT) = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.CHARGE.AMT>
    R.NEW(PAY.PART.TT.PENALTY.INT) = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.PENALTY.INT>
    R.NEW(PAY.PART.TT.PENALTY.CHG) = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.PENALTY.CHG>
    R.NEW(PAY.PART.TT.PRINCIPAL.AMT) = R.REDO.PART.TT.REJECT<PAY.PART.TT.REJ.PRINCIPAL.AMT>

* End of Fix

RETURN
*--------------------------------------------------------------------------
END
