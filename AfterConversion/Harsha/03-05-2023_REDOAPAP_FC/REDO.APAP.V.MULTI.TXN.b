* @ValidationCode : MjozMDk4OTUwMjc6Q3AxMjUyOjE2ODI1MDI2NzUxMjQ6SVRTUzotMTotMTo5Njc6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:21:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 967
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.V.MULTI.TXN
*----------------------------------------------------------------------------
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: JEYACHANDRAN S
* PROGRAM NAME:
* ODR NO      :
*----------------------------------------------------------------------
* DESCRIPTION  :This routine is used to display the field descriptions based upon
*               the selection in Payment Mode field
* IN PARAMETER :NA
* OUT PARAMETER:NA
* LINKED WITH  :
* LINKED FILE  :
*----------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                 REFERENCE           DESCRIPTION
* 28.09.2010   Jeyachandran S                           INITIAL CREATION
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM, SM to @SM, ++ to +=
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*-------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.MULTI.TRANSACTION.SERVICE
    $INSERT I_F.TELLER.ID
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.MULTI.TRANSACTION.PARAMETER
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.ARRANGEMENT

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB GOEND
RETURN
*---------
INIT:
    Y.PAY.MODE = ''
RETURN
*--------------
OPENFILES:

    FN.MULTI.TRANSACTION.SERVICE = 'F.MULTI.TRANSACTION.SERVICE'
    CALL OPF(FN.MULTI.TRANSACTION.SERVICE,F.MULTI.TRANSACTION.SERVICE)

    FN.TELLER.USER = 'F.TELLER.USER'
    F.TELLER.USER = ''
    CALL OPF(FN.TELLER.USER,F.TELLER.USER)

    FN.TELLER.ID = 'F.TELLER.ID'
    F.TELLER.ID = ''
    CALL OPF(FN.TELLER.ID,F.TELLER.ID)

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT=''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.BILL.DETAILS = 'F.AA.BILL.DETAILS'
    F.AA.BILL.DETAILS = ''
    CALL OPF(FN.AA.BILL.DETAILS,F.AA.BILL.DETAILS)



    FN.MULTI.TRANSACTION.PARAMETER = 'F.MULTI.TRANSACTION.PARAMETER'
    F.MULTI.TRANSACTION.PARAMETER = ''
    CALL OPF(FN.MULTI.TRANSACTION.PARAMETER,F.MULTI.TRANSACTION.PARAMETER)

    FN.ACCT.ACTIVITY = 'F.ACCT.ACTIVITY'
    F.ACCT.ACTIVITY = ''
    CALL OPF(FN.ACCT.ACTIVITY,F.ACCT.ACTIVITY)
RETURN
*-------------
PROCESS:

    Y.TYPE = R.NEW(REDO.MTS.SETTLEMENT.TYPE)
    Y.ARR.ID1 = R.NEW(REDO.MTS.ARRANGEMENT.ID)
    Y.OPERATION = R.NEW(REDO.MTS.OPERATION)

    Y.PAY.MODE = COMI

    IF Y.TYPE EQ 'SINGLE' AND Y.PAY.MODE NE '' THEN
        R.NEW(REDO.MTS.ARR.ID)<1,AV>=Y.ARR.ID1
        Y.ID3 = R.NEW(REDO.MTS.ARR.ID)<1,AV>

        CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.ARR.ID1,R.AA.ACCOUNT.DETAILS ,F.AA.ACCOUNT.DETAILS,ERR)
        VAR.BILL.LIST        =R.AA.ACCOUNT.DETAILS<AA.AD.BILL.ID>
        VAR.BILL.STATUS.LIST = R.AA.ACCOUNT.DETAILS<AA.AD.BILL.STATUS>
        VAR.SET.STATUS.LIST  = R.AA.ACCOUNT.DETAILS<AA.AD.SET.STATUS>
        CHANGE @SM TO @FM IN VAR.BILL.LIST
        CHANGE @VM TO @FM IN VAR.BILL.LIST
        CHANGE @SM TO @FM IN VAR.BILL.STATUS.LIST
        CHANGE @VM TO @FM IN VAR.BILL.STATUS.LIST
        CHANGE @SM TO @FM IN VAR.SET.STATUS.LIST
        CHANGE @VM TO @FM IN VAR.SET.STATUS.LIST
        VAR.BILL.LIST.SIZE=DCOUNT(VAR.BILL.LIST,@FM)
        BILL.CNT=1
        VAR.BILL.CNT=0
        VAR.OS.AMOUNT= 0
        LOOP
            VAR.BILL.LIST.ID = VAR.BILL.LIST<BILL.CNT>
            VAR.SET.STATUS   = VAR.SET.STATUS.LIST<BILL.CNT>
            VAR.BILL.STATUS  = VAR.BILL.STATUS.LIST<BILL.CNT>
            IF VAR.SET.STATUS EQ 'UNPAID' THEN
                CALL F.READ(FN.AA.BILL.DETAILS,VAR.BILL.LIST.ID,R.AA.BILL.DETAILS,F.AA.BILL.DETAILS,ERR)
                VAR.BILL.CNT += 1 ;*R22 AUTO CODE CONVERSION
                VAR.OS.AMOUNT+= SUM(R.AA.BILL.DETAILS<AA.BD.OS.PROP.AMOUNT>)
            END
        WHILE BILL.CNT LE VAR.BILL.LIST.SIZE
            BILL.CNT += 1 ;*R22 AUTO CODE CONVERSION
        REPEAT
        R.NEW(REDO.MTS.OD.AMT)<1,AV> = VAR.OS.AMOUNT
        R.NEW(REDO.MTS.OD.BILLS)<1,AV> = VAR.BILL.CNT

    END


*        CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.ARR.ID1,R.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS,F.ERR)
*        Y.OD.BILL = R.AA.ACCOUNT.DETAILS<AA.AD.BILL.ID>
*        Y.OD.BILL = CHANGE(Y.OD.BILL,VM,FM)
*        Y.CNT = DCOUNT(Y.OD.BILL,FM)
*        Y.TOT.AMT = R.NEW(REDO.MTS.TOTAL.AMT)
*        R.NEW(REDO.MTS.TRANSACTION.AMT) = Y.TOT.AMT
*        Y.TOT.AMT1 = R.NEW(REDO.MTS.TRANSACTION.AMT)


    IF Y.PAY.MODE EQ 'Cash' THEN
        T(REDO.MTS.ACCOUNT.SETTLEMENT)<3> = 'NOINPUT'
    END

    Y.PAY.MODE1 = R.NEW(REDO.MTS.PAYMENT.MODE)
    Y.PAY.MODE2 = CHANGE(Y.PAY.MODE1,@VM,@FM)
    Y.PAY.CNT = DCOUNT(Y.PAY.MODE2,@FM)
    Y.CNTR = 1
    LOOP
    WHILE Y.CNTR LE Y.PAY.CNT
        Y.CURR.VAL = Y.PAY.MODE2<Y.CNTR>
        IF Y.CURR.VAL EQ 'Cash' THEN
            T(REDO.MTS.ACCOUNT.SETTLEMENT)<3> = 'NOINPUT'
        END
        Y.CNTR + = 1
    REPEAT

    IF Y.OPERATION EQ 'REPAYMENT' THEN
        Y.RESIDUAL.VAL = R.NEW(REDO.MTS.RESIDUAL)
        IF Y.RESIDUAL.VAL EQ 'NO' THEN
            T(REDO.MTS.RESIDUAL.MODE)<3> = 'NOINPUT'
        END

        Y.TYPE = R.NEW(REDO.MTS.SETTLEMENT.TYPE)
        Y.OPERATION = R.NEW(REDO.MTS.OPERATION)
        IF Y.TYPE EQ 'MULTIPLE' THEN
            T(REDO.MTS.ARRANGEMENT.ID)<3> = 'NOINPUT'
        END
    END


    IF Y.OPERATION NE 'REPAYMENT' THEN
        R.NEW(REDO.MTS.OD.AMT) = ''
        R.NEW(REDO.MTS.OD.BILLS) = ''
    END
RETURN
*--------------
GOEND:
END
