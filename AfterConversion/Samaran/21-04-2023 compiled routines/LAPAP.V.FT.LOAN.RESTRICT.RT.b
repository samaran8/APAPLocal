* @ValidationCode : MjotMTMzMjI2ODU5OTpDcDEyNTI6MTY4MjA3MzY5MzI0NTpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:11:33
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
$PACKAGE APAP.LAPAP
SUBROUTINE LAPAP.V.FT.LOAN.RESTRICT.RT
*------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*21-04-2023            Conversion Tool             R22 Auto Code conversion                FM TO @FM,VM TO @VM,SM TO @SM,INSERT FILE MODIFIED
*21-04-2023              Samaran T                R22 Manual Code conversion                     CALL ROUTINE FORMAT MODIFIED
*-----------------------------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON   ;*R22 AUTO CODE CONVERSION.START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AI.REDO.ACCT.RESTRICT.PARAMETER
    $INSERT I_F.AA.OVERDUE    ;*R22 AUTO CODE CONVERSION.END

    GOSUB INIT
    GOSUB VALIDA.ACCT

RETURN

*----
INIT:
*----
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''

    FN.AI.REDO.ACCT.RESTRICT.PARAMETER = 'F.AI.REDO.ACCT.RESTRICT.PARAMETER'
    F.AI.REDO.ACCT.RESTRICT.PARAMETER  = ''

    Y.CONTINUE = 'Y'
    Y.ACCT.ID = ''
    Y.CONDICION = ''

    LREF.LOAN.STATUS.1 = 'L.LOAN.STATUS.1'
    LREF.LOAN.COND = 'L.LOAN.COND'
    LREF.APP = 'AA.PRD.DES.OVERDUE'

    CALL GET.LOC.REF(LREF.APP,LREF.LOAN.STATUS.1,STATUS1.POS)
    CALL GET.LOC.REF(LREF.APP,LREF.LOAN.COND,COND.POS)

    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.AI.REDO.ACCT.RESTRICT.PARAMETER,F.AI.REDO.ACCT.RESTRICT.PARAMETER)

RETURN

*--------------
VALIDA.ACCT:
*--------------

    Y.ACCT.ID=R.NEW(FT.CREDIT.ACCT.NO)

    CALL F.READ(FN.ACCOUNT,Y.ACCT.ID,R.ACCOUNT,F.ACCOUNT,R.ERR)
    Y.ARRANGEMENT.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>

    IF Y.ARRANGEMENT.ID EQ '' THEN
        RETURN
    END

    CALL CACHE.READ(FN.AI.REDO.ACCT.RESTRICT.PARAMETER,'SYSTEM',R.AI.REDO.ACCT.RESTRICT.PARAMETER,RES.ERR)

    Y.PARAM.LOAN.STATUS = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.LOAN.ACCT.STATUS>
    CHANGE @VM TO @FM IN Y.PARAM.LOAN.STATUS

    PROP.CLASS = 'OVERDUE'
    PROPERTY = ''
    R.CONDITION = ''
    ERR.MSG = ''
    EFF.DATE = ''
    CALL APAP.REDOFCFI.REDO.CRR.GET.CONDITIONS(Y.ARRANGEMENT.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.CONDITION,ERR.MSG)     ;*R22 MANUAL CODE CONVERSION
    LOAN.COND = R.CONDITION<AA.OD.LOCAL.REF,COND.POS>
    LOAN.STATUS = R.CONDITION<AA.OD.LOCAL.REF,STATUS1.POS>

    CHANGE @SM TO @FM IN LOAN.COND
    CHANGE @SM TO @FM IN LOAN.STATUS

    Y.COUNT = 0
    Y.COUNT = DCOUNT(Y.PARAM.LOAN.STATUS,@FM)

    FOR A = 1 TO Y.COUNT STEP 1
        Y.PARAM.LOAN.VAL = Y.PARAM.LOAN.STATUS<A>

        Y.COUNT.STATUS = 0
        Y.COUNT.STATUS = DCOUNT(LOAN.STATUS,@FM)
        Y.COUNT.COND = 0
        Y.COUNT.COND = DCOUNT(LOAN.COND,@FM)

        FOR ASTATUS = 1 TO Y.COUNT.STATUS STEP 1
            Y.STATUS.VAL = LOAN.STATUS<ASTATUS>

            LOCATE Y.STATUS.VAL IN Y.PARAM.LOAN.VAL SETTING SUB.POS THEN
                ETEXT='PRESTAMO CON CONDICIONES BLOQUEANTES - ' : Y.STATUS.VAL
                CALL STORE.END.ERROR
                RETURN
            END

        NEXT ASTATUS

        FOR ACOND = 1 TO Y.COUNT.COND STEP 1
            Y.COND.VAL = LOAN.COND<ACOND>

            LOCATE Y.COND.VAL IN Y.PARAM.LOAN.VAL SETTING SUB.POS THEN
                ETEXT='PRESTAMO CON CONDICIONES BLOQUEANTES - ' : Y.COND.VAL
                CALL STORE.END.ERROR
                RETURN
            END

        NEXT ACOND

    NEXT A

RETURN

END
