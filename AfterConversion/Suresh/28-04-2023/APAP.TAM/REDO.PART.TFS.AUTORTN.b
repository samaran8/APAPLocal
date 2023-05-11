* @ValidationCode : MjoxMjU0MTI3OTM5OkNwMTI1MjoxNjgyNjYxNzQ3MDc3OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 11:32:27
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
*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.PART.TFS.AUTORTN
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: REDO.PART.TFS.AUTORTN
*----------------------------------------------------------------------
*DESCRIPTION: This is the Routine for REDO.TFS.PROCESS to
* default the value for the T24.FUND.SERVICES application from REDO.TFS.PROCESS
* It is AUTOM NEW CONTENT routine

*IN PARAMETER: NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.PART.TFS.PROCESS
*----------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------
*DATE WHO REFERENCE DESCRIPTION
*09.07.2010 S SUDHARSANAN B.12 INITIAL CREATION
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*12/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*12/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.PART.TFS.PROCESS
    $INSERT I_F.ALTERNATE.ACCOUNT
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.TRANSACTION
    GOSUB INIT
    GOSUB PROCESS
RETURN

*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------


    FN.REDO.PART.TFS.PROCESS = 'F.REDO.PART.TFS.PROCESS'
    F.REDO.PART.TFS.PROCESS = ''
    CALL OPF(FN.REDO.PART.TFS.PROCESS,F.REDO.PART.TFS.PROCESS)

    FN.ALTERNATE.ACCOUNT = 'F.ALTERNATE.ACCOUNT'
    F.ALTERNATE.ACCOUNT = ''
    CALL OPF(FN.ALTERNATE.ACCOUNT,F.ALTERNATE.ACCOUNT)

    FN.TFS.TRANSACTION = 'F.TFS.TRANSACTION'
    F.TFS.TRANSACTION = ''
    CALL OPF(FN.TFS.TRANSACTION,F.TFS.TRANSACTION)

    LOC.REF.APPLICATION="T24.FUND.SERVICES"
    LOC.REF.FIELDS='L.TFS.DUE.PRS'
    LOC.REF.POS=''
    CALL GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.TFS.DUE.PRS=LOC.REF.POS<1,1>
RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    Y.DATA = ""
    CALL BUILD.USER.VARIABLES(Y.DATA)
    Y.REDO.PART.TFS.PROCESS.ID=FIELD(Y.DATA,"*",2)
    CALL F.READ(FN.REDO.PART.TFS.PROCESS,Y.REDO.PART.TFS.PROCESS.ID,R.REDO.TFS.PROCESS,F.REDO.PART.TFS.PROCESS,PRO.ERR)
    Y.AA.ID = R.REDO.TFS.PROCESS<PAY.PART.TFS.ARRANGEMENT.ID>
    CALL F.READ(FN.ALTERNATE.ACCOUNT,Y.AA.ID,R.ALT.ACC,F.ALTERNATE.ACCOUNT,ALT.ERR)
    ACC.ID = R.ALT.ACC<AAC.GLOBUS.ACCT.NUMBER>
    R.NEW(TFS.PRIMARY.ACCOUNT)<1,1> = ACC.ID
    Y.CNT=DCOUNT(R.REDO.TFS.PROCESS<PAY.PART.TFS.CURRENCY>,@VM)
    FOR Y.COUNT=1 TO Y.CNT
        R.NEW(TFS.TRANSACTION)<1,Y.COUNT>=R.REDO.TFS.PROCESS<PAY.PART.TFS.TRAN.TYPE,Y.COUNT>
        R.NEW(TFS.CURRENCY)<1,Y.COUNT>=R.REDO.TFS.PROCESS<PAY.PART.TFS.CURRENCY,Y.COUNT>
        Y.TXN = R.REDO.TFS.PROCESS<PAY.PART.TFS.TRAN.TYPE,Y.COUNT>
        CALL F.READ(FN.TFS.TRANSACTION,Y.TXN,R.TFS.TRAN,F.TFS.TRANSACTION,TFS.TRAN.ERR)
        Y.SURR.ACC = R.TFS.TRAN<TFS.TXN.SURROGATE.AC>
        IF Y.SURR.ACC NE '' THEN
            R.NEW(TFS.SURROGATE.AC)<1,Y.COUNT> = R.REDO.TFS.PROCESS<PAY.PART.TFS.ACCOUNT.NUMBER,Y.COUNT>
        END
        R.NEW(TFS.AMOUNT.LCY)<1,Y.COUNT>=R.REDO.TFS.PROCESS<PAY.PART.TFS.AMOUNT,Y.COUNT>
    NEXT Y.COUNT
    R.NEW(TFS.LOCAL.REF)<1,POS.L.TFS.DUE.PRS>=Y.REDO.PART.TFS.PROCESS.ID
RETURN
*--------------------------------------------------------------------------------------------------
END
