* @ValidationCode : MjotMTc2MzUwMjQ2ODpDcDEyNTI6MTY4MjY1ODc1NDQ3NjpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 10:42:34
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
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-25</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.VAL.CCY.CHECK
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Ganesh R
* Program Name  : REDO.VAL.CCY.CHECK
*-------------------------------------------------------------------------
* Description: This routine is a Validation routine to CURRENCY field
*
*----------------------------------------------------------
* Linked with:  T24.FUNDS.SERVICES,FCY.COLLECT
* In parameter :
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE              ODR                             DESCRIPTION
* 21-09-10          ODR-2010-09-0251              Initial Creation
** 18-04-2023 R22 Auto Conversion FM, VM, SM TO @FM, @VM, @SM
** 18-04-2023 Skanda R22 Manual Conversion - No changes
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.T24.FUND.SERVICES


    GOSUB OPEN.FILE
    GOSUB PROCESS
RETURN

OPEN.FILE:
*Opening Files

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN

PROCESS:

*Get the Count of Transaction Field

    VAR.MULTI.TXN = R.NEW(TFS.TRANSACTION)
    VAR.TRANS.COUNT = DCOUNT(VAR.MULTI.TXN,@VM)

*Get the values of Account and Currency field
    CCY.FLAG = ''
    VAR.COUNT = 1
    LOOP
        REMOVE TXN FROM VAR.MULTI.TXN SETTING TXN.POS
    WHILE VAR.COUNT LE VAR.TRANS.COUNT
        VAR.TRANS.AC = R.NEW(TFS.SURROGATE.AC)<1,VAR.COUNT>
        VAR.TRANC.CCY = COMI
*Get the Currency of Surrogate Account

        CALL F.READ(FN.ACCOUNT,VAR.TRANS.AC,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
        VAR.SUR.CCY = R.ACCOUNT<AC.CURRENCY>

*Check the Currency of Surrogate Account with Currency value in the Record

        IF VAR.TRANC.CCY EQ VAR.SUR.CCY THEN
            CCY.FLAG = 1
        END
        VAR.COUNT++
    REPEAT

    IF CCY.FLAG EQ '' THEN
        ETEXT = "EB-INVALID.CURRENCY"
        CALL STORE.END.ERROR
    END
RETURN
END
