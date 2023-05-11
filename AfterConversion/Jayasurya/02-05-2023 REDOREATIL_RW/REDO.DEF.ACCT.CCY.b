* @ValidationCode : Mjo0NTA0NDcxNTc6Q3AxMjUyOjE2ODMwMTkyMDA4NTY6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 02 May 2023 14:50:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
*-----------------------------------------------------------------------------
* <Rating>-24</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.DEF.ACCT.CCY
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Ganesh R
* Program Name  : REDO.DEF.ACCT.CCY
*-------------------------------------------------------------------------
* Description: This routine is a Inout routine to default currency and Account.1
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
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            VM TO @VM
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
    VAR.TRANS.COUNT = DCOUNT(VAR.MULTI.TXN,@VM) ;* MANUAL R22 CODE CONVERSION

*Get the values of Account and Currency field
    CCY.FLAG = ''
    VAR.COUNT = 1

    VAR.TRANS.AC = R.NEW(TFS.PRIMARY.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,VAR.TRANS.AC,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
    VAR.TRANC.CCY = R.ACCOUNT<AC.CURRENCY>

    LOOP
        REMOVE TXN FROM VAR.MULTI.TXN SETTING TXN.POS
    WHILE VAR.COUNT LE VAR.TRANS.COUNT
*Assign the Currency and Surrogate Account of Primary Account
        R.NEW(TFS.SURROGATE.AC)<1,VAR.COUNT> = VAR.TRANS.AC
        R.NEW(TFS.CURRENCY)<1,VAR.COUNT> = VAR.TRANC.CCY
        VAR.COUNT++
    REPEAT

RETURN
END
