* @ValidationCode : MjotMjgyNDk1MTQ5OkNwMTI1MjoxNjgxNzM0NzU4Nzg2OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 18:02:38
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
SUBROUTINE REDO.V.VAL.TYPE.PAYMENT.COLL
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Marimuthu S
* Program Name : REDO.V.VAL.TYPE.PAYMENT.COLL
*-----------------------------------------------------------------------------
* Description : This is attached as validation routine for the field TRANS.TYPE
*
**DATE           ODR                   DEVELOPER               VERSION
*
*15/10/11       PACS00126000            Marimuthu S
* 18/07/12      PACS00208258            Marimuthu S
*-----------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                     VM TO @VM,SM TO @SM
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-----------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.PART.TT.PROCESS
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.OVERDUE

MAIN:

    IF MESSAGE EQ 'VAL' THEN
        RETURN
    END

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AA.BILL.DETAILS = 'F.AA.BILL.DETAILS'
    F.AA.BILL.DETAILS = ''
    CALL OPF(FN.AA.BILL.DETAILS,F.AA.BILL.DETAILS)

    FN.AA.ACCOUNT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCOUNT.DETAILS = ''
    CALL OPF(FN.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS)

    LR.APP  = 'AA.PRD.DES.OVERDUE'
    LR.FLDS = 'L.LOAN.STATUS.1'
    LR.POS  = ''
    CALL MULTI.GET.LOC.REF(LR.APP,LR.FLDS,LR.POS)
    LOAN.ST.POS = LR.POS<1,1>

    GOSUB PROCESS
    GOSUB PGM.END

PROCESS:

    Y.TYPE.PAY = COMI
    Y.AC.AMT = R.NEW(PAY.PART.TT.PRINCIPAL.AMT)
    Y.INT.AMT = R.NEW(PAY.PART.TT.PRINCIPAL.INT)
    Y.CHG.AMT = R.NEW(PAY.PART.TT.CHARGE.AMT)
    Y.PN.AMT = R.NEW(PAY.PART.TT.PENALTY.INT)
    Y.PN.CG = R.NEW(PAY.PART.TT.PENALTY.CHG)

    R.NEW(PAY.PART.TT.AMOUNT) = ''

    Y.AA.ID = R.NEW(PAY.PART.TT.ARRANGEMENT.ID)
    IF Y.AA.ID[1,2] NE 'AA' THEN
        CALL F.READ(FN.ACCOUNT,Y.AA.ID,R.ACCOUNT,F.ACCOUNT,AC.ERR)
        Y.AA.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
    END

    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,'OVERDUE','','',RET.PROP,RET.COND,RET.ERR)
    RET.COND = RAISE(RET.COND)
    Y.LOAN.COND = RET.COND<AA.OD.LOCAL.REF,LOAN.ST.POS>
    Y.LOAN.COND = CHANGE(Y.LOAN.COND,@SM,@VM)
    IF 'Write-off' MATCHES Y.LOAN.COND THEN
        Y.SET.WOF = 'Y'
    END

    GOSUB SET.PAY.TYPE.1
    IF Y.S.V NE 'Y' THEN
        GOSUB SET.PAY.TYPE.2
    END

RETURN

SET.PAY.TYPE.1:

    BEGIN CASE

        CASE Y.TYPE.PAY EQ 'ACCOUNT'
            R.NEW(PAY.PART.TT.AMOUNT) = Y.AC.AMT
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'INTEREST'
            R.NEW(PAY.PART.TT.AMOUNT) = Y.INT.AMT + Y.PN.AMT
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'PENALTY.INTEREST'
            R.NEW(PAY.PART.TT.AMOUNT) = Y.PN.CG
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'CHARGE'
            R.NEW(PAY.PART.TT.AMOUNT) = Y.CHG.AMT
            Y.S.V = 'Y'

    END CASE

RETURN

SET.PAY.TYPE.2:

    BEGIN CASE

        CASE Y.TYPE.PAY EQ 'ACCOUNT.INTEREST' OR Y.TYPE.PAY EQ 'INTEREST.ACCOUNT'
            R.NEW(PAY.PART.TT.AMOUNT) = Y.AC.AMT + Y.INT.AMT + Y.PN.AMT
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'PAYOFF'
            GOSUB GET.PAYOFF.AMT
            R.NEW(PAY.PART.TT.AMOUNT)  = Y.OR.PAYOFF.AMT
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'NORMAL.PAYMENT'
            R.NEW(PAY.PART.TT.AMOUNT) = ''
            IF Y.SET.WOF EQ 'Y' THEN
                GOSUB RAISE.WOF.ERR
            END
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'OVERPAYMENT'
            R.NEW(PAY.PART.TT.AMOUNT) = ''
            IF Y.SET.WOF EQ 'Y' THEN
                GOSUB RAISE.WOF.ERR
            END
            Y.S.V = 'Y'

        CASE Y.TYPE.PAY EQ 'WOF.INSTALL.PAY'
            R.NEW(PAY.PART.TT.AMOUNT) = ''
            IF Y.SET.WOF NE 'Y' THEN
                AF = PAY.PART.TT.TRAN.TYPE
                ETEXT = 'EB-REDO.NT.LOAN.WOF'
                CALL STORE.END.ERROR
            END

    END CASE

RETURN

GET.PAYOFF.AMT:

    CALL F.READ(FN.AA.ACCOUNT.DETAILS,Y.AA.ID,R.AA.ACCOUNT.DETAILS,F.AA.ACCOUNT.DETAILS,AA.ACC.ERR)
    Y.BILLS = R.AA.ACCOUNT.DETAILS<AA.AD.BILL.ID>
    Y.BILLS = CHANGE(Y.BILLS,@SM,@VM)
    Y.CNT = DCOUNT(Y.BILLS,@VM)
    FLG = ''
    LOOP
    WHILE Y.CNT GT 0 DO
        FLG += 1
        Y.BIL.ID = Y.BILLS<1,FLG>
        CALL F.READ(FN.AA.BILL.DETAILS,Y.BIL.ID,R.BILL.DETAILS,F.AA.BILL.DETAILS,BIL.ERR)
        Y.PAY.TYPE = R.BILL.DETAILS<AA.BD.PAYMENT.METHOD>
        LOCATE 'INFO' IN Y.PAY.TYPE<1,1> SETTING POS.PY THEN
            Y.OR.PAYOFF.AMT = R.BILL.DETAILS<AA.BD.OR.TOTAL.AMOUNT>
            Y.SET.PYOF = 'Y'
            RETURN
        END
        Y.CNT -= 1
    REPEAT

    IF Y.SET.PYOF NE 'Y' THEN
        AF = PAY.PART.TT.TRAN.TYPE
        ETEXT = 'EB-PAYOFF.NT.REQ'
        CALL STORE.END.ERROR
    END

RETURN

RAISE.WOF.ERR:

    AF = PAY.PART.TT.TRAN.TYPE
    ETEXT = 'EB-REDO.LOAN.WOF'
    CALL STORE.END.ERROR

RETURN

PGM.END:

END
