* @ValidationCode : MjoxODM0MzU2ODQ1OkNwMTI1MjoxNjgwNzY3OTg2MDYzOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 13:29:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.INP.CHEQUE.PROCESS
*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.CHEQUE.PROCESS.AUTHORISE
*--------------------------------------------------------------------------------
* Description:
*--------------------------------------------------------------------------------
*            This validation routine is be attached to the VERSION FUNDS.TRANSFER,REINV.WDL
* to populate DEBIT.ACCOUNT number and currency
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO               REFERENCE         DESCRIPTION
* 22-AUG-2011     JEEVA T              N.11               intial creation
*06-04-2023       Conversion Tool      R22 Auto Code conversion      FM TO @FM,VM TO @VM
*06-04-2023       Samaran T            Manual R22 Code Conversion    No Changes
*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.VERSION
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.CHEQUE.PROCESS
    $INSERT I_REDO.TELLER.PROCESS.COMMON
    $INSERT I_F.REDO.APAP.REINV.CHQ.AMT
*   $INSERT I_F.REDO.CHEQUE.PROCESS ;*R22 AUTO CODE CONVERSION
    $INSERT I_F.REDO.H.PAY.VERSION
    $INSERT I_System

    FN.REDO.APAP.REINV.CHQ.AMT='F.REDO.APAP.REINV.CHQ.AMT'
    F.REDO.APAP.REINV.CHQ.AMT= ''
    R.REDO.APAP.REINV.CHQ.AMT=''
    CALL OPF(FN.REDO.APAP.REINV.CHQ.AMT,F.REDO.APAP.REINV.CHQ.AMT)

    FN.REDO.H.PAY.VERSION = 'F.REDO.H.PAY.VERSION'
    F.REDO.H.PAY.VERSION = ''
    CALL OPF(FN.REDO.H.PAY.VERSION,F.REDO.H.PAY.VERSION)
    R.REDO.H.PAY.VERSION = ''

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    R.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
    GOSUB PROCESS.FILE

RETURN

*--------------------------------------------------------------------------------
PROCESS.FILE:
*--------------------------------------------------------------------------------

    CALL CACHE.READ(FN.REDO.H.PAY.VERSION,'SYSTEM',R.REDO.H.PAY.VERSION,Y.ERR.PAR)
    Y.PAYMENT.LIST = R.REDO.H.PAY.VERSION<REDO.H.VER.PAYMENT.TYPE>
    Y.VERSION.LIST = R.REDO.H.PAY.VERSION<REDO.H.VER.VERSIONS>

    Y.PAYMENT.TYPE = R.NEW(CHQ.PRO.PAYMENT.TYPE)
    LOCATE Y.PAYMENT.TYPE IN Y.PAYMENT.LIST<1,1> SETTING POS.VER THEN
        VAR.PROCESS.ID = ID.NEW
        R.VERSION(EB.VER.NEXT.VERSION) = Y.VERSION.LIST<1,POS.VER>:' I NEW '
    END

    IF Y.PAYMENT.TYPE EQ 'CASH' THEN
        R.VERSION(EB.VER.NEXT.VERSION) = ''
    END


    Y.AZ.ID = R.NEW(CHQ.PRO.AZ.ACCOUNT)
    CALL F.READ(FN.AZ.ACCOUNT,Y.AZ.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,Y.ERR)
    Y.INT.LIQ.ACC = R.AZ.ACCOUNT<AZ.INTEREST.LIQU.ACCT>
    CALL F.READ(FN.REDO.APAP.REINV.CHQ.AMT,Y.INT.LIQ.ACC,R.REDO.APAP.REINV.CHQ.AMT,F.REDO.APAP.REINV.CHQ.AMT,ACC.ERR)
    Y.DEBIT.AMT = R.NEW(CHQ.PRO.DEBIT.AMOUNT)
    R.REDO.APAP.REINV.CHQ.AMT<REDO.CHQ.PROCESS.ID,-1> = ID.NEW
    R.REDO.APAP.REINV.CHQ.AMT<REDO.CHQ.AMOUNT,-1> = Y.DEBIT.AMT
    IF V$FUNCTION EQ 'I' THEN
        IF Y.PAYMENT.TYPE EQ 'CASH' THEN
            CALL F.WRITE(FN.REDO.APAP.REINV.CHQ.AMT,Y.INT.LIQ.ACC,R.REDO.APAP.REINV.CHQ.AMT)
        END
    END

    IF V$FUNCTION EQ 'R' THEN
        Y.PROCESS.ID = R.REDO.APAP.REINV.CHQ.AMT<REDO.CHQ.PROCESS.ID>
        CHANGE @VM TO @FM IN Y.PROCESS.ID
        LOCATE ID.NEW IN Y.PROCESS.ID SETTING Y.POS THEN
            CALL F.READ(FN.REDO.APAP.REINV.CHQ.AMT,Y.INT.LIQ.ACC,R.REDO.APAP.REINV.CHQ.AMT,F.REDO.APAP.REINV.CHQ.AMT,ACC.ERR)
            DEL R.REDO.APAP.REINV.CHQ.AMT<REDO.CHQ.PROCESS.ID,Y.POS>
            DEL R.REDO.APAP.REINV.CHQ.AMT<REDO.CHQ.AMOUNT,Y.POS>
            DEL R.REDO.APAP.REINV.CHQ.AMT<REDO.CHQ.STATUS,Y.POS>
            CALL F.WRITE(FN.REDO.APAP.REINV.CHQ.AMT,Y.INT.LIQ.ACC,R.REDO.APAP.REINV.CHQ.AMT)
        END
    END
RETURN
END
