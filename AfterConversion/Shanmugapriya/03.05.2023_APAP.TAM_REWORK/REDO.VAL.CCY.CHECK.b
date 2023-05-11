* @ValidationCode : MjoxNDg5MDkxMDU4OkNwMTI1MjoxNjgzMDU2NTI1NTM1OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 03 May 2023 01:12:05
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
*
* Date             Who                   Reference      Description
* 03.05.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 03.05.2023       Shanmugapriya M       R22            Manual Conversion   - VM TO @VM
*
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
